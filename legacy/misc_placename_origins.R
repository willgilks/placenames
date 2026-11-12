## Packages 
library("tidyverse") # data object manipulation
library("sf") # geo assignment
library("rnaturalearth") # geo functions
library("leaflet") # plotting maps

# Place-name data downloaded from https://hub.arcgis.com/datasets/208d9884575647c29f0dd5a1184e711a/about

## some places being assigned to two different counties ??
## e.g. barmby on the marsh...
# Because there are two different lat and longs from two diff place types!



# CONSTANTS ####

DATA_PATH="~/Downloads/IPN_GB_2024/IPN_GB_2024.csv"

# define area types to use
# by using all these place types, some redundancy/overlap is expected.
PLACE_TYPES = c("BUA",
                # "LOC",
                "LONB","MD","PAR")


# placename origin search strings
PLACENAME_ORIGIN_SEARCH_STRINGS=list(
  "Prefix"=list(
    "Celtic"=c("aber","bal","bally","bryn","caer","car","cwm","dum","dun","glen","inver",
               "kil","kirk","llan","nant","pen","pont","tre"),
    "Norman"=c("beau","bel","mal","rich"),
    "Roman"=c("fosse","strat","stret","street"),
    "Saxon"=c("ast","brad","cheap","chipping","eas","nor","sud","sut","wes")),
  "Suffix"=list(
    "Celtic"=c("combe","coombe","more","tor"),
    "Old Norse"=c("beck","by","dale","force","foss","gate","holm","holme","kirk","ness","tarn",
                  "thorp","thorpe","thwaite","toft"),
    "Roman"=c("caister","caster","cester","chester","coln","xeter"),
    "Saxon"=c("borough","bourne","brough","burgh","burn","bury","den","dene","ea","ey",
              "field","ford","ham","hurst","ing","ingas","lea","ley","minster",
              "stead","stoke","stow","ton","wic","wich","worth")),
  "Middle"=list(
    "Norman" = c(" de la ", " en le ", " in ", " le ", " on ", " sub ", " super ", " upon ")))



# function to concatenate lookup list to make lookup table
make_placename_pattern_lookup = function(lookup_list=PLACENAME_ORIGIN_SEARCH_STRINGS){
  regex_lookup_output=imap_dfr(lookup_list, function(origin_list, position) {
    imap_dfr(origin_list, function(elements, origin) {
      
      # Native if-else block checks the scalar position once upfront, 
      # which is fast, clean, and completely removes the dplyr warning.
      processed_elements <- if (position == "Prefix") {
        paste0("^", elements)
      } else if (position == "Suffix") {
        paste0(elements, "$")
      } else {
        elements
      }
      
      combined_pattern <- paste(processed_elements, collapse = "|")
      tibble(origin = origin, pattern = combined_pattern)
    })}) %>%
    group_by(origin) %>%
    summarise(pattern = paste0("(", pattern, ")", collapse = "|"), .groups = "drop")
  return(regex_lookup_output)
}

PLACENAME_PATTERN_LOOKUP = make_placename_pattern_lookup(lookup_list=PLACENAME_ORIGIN_SEARCH_STRINGS)

# for making searchable text, remove any placename text after these strings.
# these were used for removing any placenames which contained them ..
# .. because they were often duplicates of simpler placenames at roughly the same location.
# but some of the strings are actually useful.
# Deal with this after the other technical issues have been sorted out.
# REMOVE_AFTER_STRINGS = c("[,]"," by "," of "," on "," under "," upon ")
# REMOVE_AFTER_STRINGS_CONCAT = paste0(paste0(REMOVE_AFTER_STRINGS,collapse=".*|"),".*")



# Vector of common words to remove sensibly from place names.
# this helps remove duplicate place names records which are effectively the same location.
# it also helps prevent generic descriptors from being queried by the tribal origins scan.
COMPASS_POINTS = c('north','south','east','west')

COMMON_WORDS = c(
  COMPASS_POINTS,
  "abbey","abbots","bank","bay","bottom","bridge","brook","brow",
  "central","city","common","corner","cross","crossing","cum","down","edge","end","estate",
  "farm","field","fields","fold","gardens","gate","grange","great","green","grove",
  "hall","head","heath","high","higher","hill","hills","house","houses","in","island","lane","little","low","lower",
  "marsh","mary","mill","moor","moss","new","of","old","on",
  "park","park farm","parks","place","priory",
  "road","row","side","st","st[.]","street","the","top","town","upper","vale","valley","village",
  "wharf","without","wood")


# removal conditions:
# 1. if these words are at the start, followed by a space
# 2. if the words are at the end, preceded by a space.
# 3. if the words are in the middle, flanked by spaces.

# define concatenation strings for joining common words in prep for removal
COMMON_WORDS_CONCAT_STRINGS = c(" |^","$| "," | ")

# concatenate words to replace when making searchable place names.
COMMON_WORDS_CONCAT = lapply(COMMON_WORDS_CONCAT_STRINGS, function(z){
  z_prefix = gsub("*.[|]","",z)
  z_suffix = gsub("[|].*","",z)
  paste0(z_prefix,paste0(COMMON_WORDS,collapse=z),z_suffix)
})

# uk county map base
# for plotting the percentage of place names of each origin across the GB counties.
# Question : in st_simplfy, how does changing the variables up or down, change the result ?
# Issue: This isn't really the best map. I really want a map for just the counties, not with individual
# polygons for cities, although London would be good to divide up into boroughs.
# Also, having some kind of facetted multi-plot would still be good, but something in leaflet is making this crash,
# possibly due to memory overload. We've tried to fix this and it hasn't worked, some i'm still wondering ..
# .. if plotly is a viable alternative.
GB_BOUNDARIES <- st_transform(
  ne_states(country = "united kingdom", returnclass = "sf") |> 
    filter(region != "Northern Ireland"), crs = 4326) |> 
  st_simplify(preserveTopology = TRUE, dTolerance = 0.005)



## FUNCTIONS ####

# do_initial_cleanup
# remove duplicates
# this initially limits the placename types (LOC, BUA etc), and removes geographic outliers.
# it makes a repaired placename column by converting to ascii to remove special charcters
# it then attempts to remove obvious sources of duplication by removing placenames with ' and ' in them.
# e.g. 'Brighton and Hove' is also separately reported as 'Brighton' and 'Hove'.
# also any placenames containing arbitary names like 'area' and 'housing estate' are removed.
# placenames where cty61nm=="Cornwall and Isles of Scilly"&descnm=="LOC" are removed because..
# .. this county has an massive excess of arbitary LOC-type placename records.
do_initial_cleanup = function(
    input_df = placenames_dat_raw,
    place_types = PLACE_TYPES){
  
  input_df_copy = input_df
  output_df = input_df_copy |> 
    filter(descnm %in% place_types) |>
    filter(lat!=0,long!=0,lat>35,long>(-8)) |> 
    select(tempcode,placeid,place23nm,
           descnm,lat,long,ctry23nm,cty61nm) |> 
    mutate(
      place23nm_repaired = iconv(place23nm, to = "ASCII", sub = "\u0020"),
      pname_length = nchar(place23nm_repaired)) |> 
    filter(
      !grepl(" \\& ", place23nm_repaired,ignore.case = T),
      !grepl(" and ", place23nm_repaired,ignore.case = T),
      !grepl(" with ", place23nm_repaired,ignore.case = T),
      !grepl("^area | area | area$", place23nm_repaired,ignore.case = T),
      !grepl("\\(", place23nm_repaired),
      !grepl("\\/", place23nm_repaired)) |>
    filter(!(cty61nm=="Cornwall and Isles of Scilly"&descnm=="LOC")) |>
    mutate(lat = if_else(lat == 0,as.numeric(NA),lat),
           long = if_else(long == 0,as.numeric(NA),long)) |>
    ungroup()
  
  return(output_df)
} 


# Assign counties to place names
# Although county information is technically available for the placenames, as several different columns...
# .. they do not match up to the county names in the map object used in the leaflet plot.
# therefore, the solution is to instead use the provided lat and long values for each placename..
# to compare with co-ordinates of each county name in the the map object in order to assign county.
# where this fails, do a second estimate based on proximity to town in already assigned county.
assign_countries_from_sf = function(
    input_df = placenames_dat_filtered,
    gb_boundaries = GB_BOUNDARIES) {
  
  input_df_copied = input_df
  
  # lat and long for each place
  geo_dat_input = input_df_copied |> 
    select(tempcode,placeid,place23nm_repaired,lat,long) |> 
    distinct()
  
  # Convert to a spatial point object
  geo_dat_sf <- st_as_sf(geo_dat_input, coords = c("long", "lat"), crs = 4326, remove = FALSE)
  
  # Perform the spatial lookup
  geo_lookup_result <- st_join(geo_dat_sf, gb_boundaries, join = st_intersects)
  
  # make into tibble
  place_county_reference=geo_lookup_result |> 
    st_drop_geometry() |> 
    select(tempcode,placeid,place23nm_repaired,lat,long,
           county_name = name)
  
  ## for missing counties, assign from closest known.
  known_towns=place_county_reference |> 
    filter(!is.na(county_name) & county_name != "")
  
  missing_towns=place_county_reference |>
    filter(is.na(county_name) | county_name == "")
  
  # Convert both tables to sf objects using GPS coordinates
  known_sf <- st_as_sf(known_towns, coords = c("long", "lat"), crs = 4326)
  missing_sf <- st_as_sf(missing_towns, coords = c("long", "lat"), crs = 4326)
  
  # Find the index of nearest known town for each missing town
  nearest_index <- st_nearest_feature(missing_sf, known_sf)
  
  # Assign the county_name from nearest known town
  missing_towns$county_name <- known_towns$county_name[nearest_index]
  output_df <- bind_rows(known_towns, missing_towns)
  output_df
}




# Function to make searchable place names
# make lowercase and replace dash with whitespace
# previously, it removed text after specified strings but i've commented out this line.
# removes common words, provided in constants. not really sure if this is legit in the overall logic.
# remove duplicates by prioritising shortest original name
# so there is only one searchable_placename per county. not really sure if this is legit int the overall logic either.
# maybe answer this question after clearing up any other technical issues.
secondary_prep_placenames = function(
    main_input_df = placenames_dat_filtered,
    reference_df = place_county_reference,
    common_words_concat = COMMON_WORDS_CONCAT) {
  
  placenames_with_searchable= main_input_df |> 
    left_join(reference_df) |> 
    select(tempcode,placeid,place23nm,place23nm_repaired,
           county_name,ctry23nm,long,lat,descnm,pname_length) |> 
    distinct() |> 
    mutate(searchable_placename = tolower(place23nm_repaired)) |> 
    mutate(searchable_placename = gsub("-"," ",searchable_placename)) |> 
    # following rows:
    # trying to remove common words which contain origin strings, but not sure if this is warranted.
    # what the next chunk does is group by searchable_placename, and then only take the shortest corresponding place23nm_repaired.
    # .. which i think makes sense.
    mutate(searchable_placename = gsub(remove_after_strings_concat," ",searchable_placename)) |>
    mutate(searchable_placename = gsub(common_words_concat[[1]],"",searchable_placename)) |>
    mutate(searchable_placename = gsub(common_words_concat[[2]],"",searchable_placename)) |>
    mutate(searchable_placename = gsub(common_words_concat[[3]],"",searchable_placename)) |>
    mutate(searchable_placename = gsub(common_words_concat[[1]],"",searchable_placename)) |>
    mutate(searchable_placename = gsub(common_words_concat[[2]],"",searchable_placename)) |>
    mutate(searchable_placename = gsub(common_words_concat[[3]],"",searchable_placename)) |>
    mutate(searchable_placename = if_else(is.na(searchable_placename)|searchable_placename=="",tolower(place23nm_repaired),
                                          searchable_placename))
  
  # Make unique
  # filter so there's only one of each searchable placename per county
  # not entirely sure about the logic/reasoning for doing this but i think it's roughly right.
  # initially filter based on which original placename was the shortest, i.e. the root.
  # then filter consistently just on lowest tempcode, placeid, lat and long.
  output_df = placenames_with_searchable|> 
    group_by(searchable_placename,county_name) |> 
    mutate(grp_min_len = min(pname_length,na.rm=T),grp_n = n()) |> 
    ungroup() |> 
    filter(pname_length==grp_min_len) |>
    select(-c(grp_min_len,grp_n)) |>
    group_by(searchable_placename,county_name) |>
    arrange(tempcode,placeid,lat,long,by_group = T) |>
    mutate(rn = row_number()) |>
    ungroup() |>
    filter(rn==1) |>
    select(-rn) |>
    arrange(searchable_placename,county_name) |>
    mutate(rn = row_number())
  
  return(output_df)
  
}



# # Resolves duplicate locality ('LOC') points sitting inside Parishes ('PAR') or Built-Up Areas ('BUA')
## I'm not really sure where to implement this.
# deduplicate_spatial_clusters <- function(df, distance_threshold_meters = 1500) {
#   message("Converting to spatial layer for cluster evaluation...")
#   
#   # transform to meters
#   sf_df <- st_as_sf(df, coords = c("long", "lat"), crs = 4326, remove = FALSE) |> 
#     st_transform(crs = 27700)
#   
#   # Calculate spatial distance matrix matching matching names
#   message("Clustering identical place-names within proximity threshold...")
#   sf_df <- sf_df |> 
#     group_by(place23nm_repaired, county_name) |> 
#     mutate(
#       geometry_json = as.character(geometry),
#       # Create distance-based sub-clusters for matching spellings in the same county
#       cluster_id = {
#         if(n() > 1) {
#           dist_matrix <- st_distance(geometry)
#           hc <- hclust(as.dist(dist_matrix), method = "complete")
#           cutree(hc, h = distance_threshold_meters)
#         } else {
#           1
#         }}) |> 
#     ungroup() |> 
#     # Prioritise administrative boundaries (PAR, BUA) over loose context (LOC) inside the cluster
#     group_by(place23nm_repaired, county_name, cluster_id) |> 
#     mutate(
#       type_priority = case_when(
#         descnm == "PAR" ~ 1,
#         descnm == "BUA" ~ 2,
#         descnm == "MD"  ~ 3,
#         TRUE ~ 4)) |> 
#     arrange(type_priority, tempcode) |> 
#     filter(row_number() == 1) |> 
#     ungroup() |> 
#     select(-type_priority, -geometry_json)
#   
#   return(st_transform(sf_df, crs = 4326))
# }


# Function to assign origin to searchable place names
# Iterate through rows, and match against the pre-compiled regex
match_origin_to_placename = function(
    input_df = placenames_prepped,
    lookup_list = PLACENAME_LOOKUP_LIST){
  
  result_of_origin_assignment = bind_rows(lapply(split(input_df, input_df$rn), function(base_row){
    
    target <- base_row$place23nm_repaired[1]
    base_row_rn = base_row$rn[1]
    base_row_searchable_placename = base_row$searchable_placename[1]
    
    if(base_row_rn %% 10000 == 0)
      print(paste(base_row_rn, base_row_searchable_placename))
    
    # Loop through Prefix, Suffix, Middle
    matches_info <- lapply(names(lookup_list), function(position) {
      sub_list <- lookup_list[[position]]
      
      # Loop through the origins inside each position
      lapply(names(sub_list), function(origin) {
        elements <- sub_list[[origin]]
        
        # Apply regex anchors
        patterns <- if (position == "Prefix") {
          paste0("^", elements)
        } else if (position == "Suffix") {
          paste0(elements, "$")
        } else {
          elements
        }
        
        # Identify which sub-elements match target string
        matching_elements <- elements[sapply(patterns, function(pat) grepl(pat, target, ignore.case = TRUE))]
        
        if (length(matching_elements) > 0) {
          return(list(origin = origin, fragments = matching_elements))
        } else {
          return(NULL)
        }
      })
    })
    
    # Flatten
    flattened_matches <- unlist(matches_info, recursive = FALSE)
    flattened_matches <- flattened_matches[!sapply(flattened_matches, is.null)]
    
    # Extract matched origins and text fragments
    matched_origins <- unique(sapply(flattened_matches, function(x) x$origin))
    matched_frags   <- unique(unlist(sapply(flattened_matches, function(x) x$fragments)))
    
    # Collapse vectors
    origins_collapsed <- if (length(matched_origins) == 0) "Unknown" else paste0(sort(matched_origins), collapse = ", ")
    frags_collapsed   <- if (length(matched_frags) == 0) "None" else paste0(sort(matched_frags), collapse = ", ")
    
    list(
      place23nm_repaired = target,
      searchable_placename = base_row_searchable_placename,
      comb_origins = origins_collapsed,
      matched_fragments = frags_collapsed)
  }))
  
  # Group and finalise counts
  output_df = result_of_origin_assignment |> 
    distinct() |> 
    mutate(n_origins = if_else(comb_origins == "Unknown", 0L, as.integer(stringr::str_count(comb_origins, ",") + 1)))
  
  return(output_df)
}




# Function to combine the origin-assignment result with the pre-prepped place names data
combine_results_with_info = function(
    origin_assignment_result_df = origin_assignment_result,
    placenames_prepped_df = placenames_prepped){
  
  output_df = placenames_prepped_df |>
    left_join(origin_assignment_result_df) |>
    mutate(county_name=if_else(is.na(county_name),"Unknown",county_name)) |>
    filter(!is.na(searchable_placename)) |>
    select(placeid,place23nm,place23nm_repaired,searchable_placename,
           county_name,ctry23nm,lat,long,descnm,comb_origins,n_origins,
           matched_fragments)
  
  return(output_df)
}




# Function to calculate the percentage of placenames of each origin, in each county.
# Because there are often a lot of placenames that cannot be assigned an origin at all, ...
# .. then these are assigned 'Unknown'.
# And then the percentages are calculated either from a total that includes Unknown (overall_pc), and ..
# .. one that omits Unknown (origin_pc).
# Function to calculate the percentage of placenames of each origin, in each county.
make_county_level_results = function(input_df){
  
  input_df_copy = input_df
  
  # FIX: Calculate the strict total count per county upfront 
  # before modifying groups or splitting rows. This ensures the baseline matches.
  county_baselines <- input_df_copy |>
    group_by(county_name) |>
    summarise(
      county_total = n(),
      county_total_known = sum(comb_origins != "Unknown"),
      .groups = "drop"
    )
  
  origins_per_county = input_df_copy |> 
    mutate(comb_origins = if_else(grepl("[,]",comb_origins),"Mixed",comb_origins)) |> 
    group_by(county_name,comb_origins) |> 
    summarise(n = n(),place23nm_vals = paste0(sort(unique(place23nm_repaired)),collapse = ";"),.groups='drop') |>
    ungroup()
  
  origins_per_county_summarised = origins_per_county |> 
    select(county_name,origin = comb_origins,n,place23nm_vals) |> 
    group_by(county_name,origin) |> 
    summarise(county_origin_n = sum(n),
              place_23nm_vals2=paste0(sort(unique(place23nm_vals)),collapse=" ; "),.groups = 'drop') |> 
    ungroup() |>
    # Join the true unmutated baselines back in
    left_join(county_baselines, by = "county_name") |>
    select(county_name,origin,county_total,county_total_known,county_origin_n,place_23nm_vals2) |> 
    distinct() |> 
    mutate(county_pc_known = round(100*(county_total_known/county_total),2),
           origin_pc = round(100*(county_origin_n/county_total_known),1),
           overall_pc = round(100*(county_origin_n/county_total),1)) |> 
    mutate(origin_pc = if_else(origin == "Unknown",as.numeric(NA),origin_pc))
  
  output_df = crossing(
    county_name = sort(unique(origins_per_county_summarised$county_name)),
    origin = sort(unique(origins_per_county_summarised$origin))) |> 
    left_join(origins_per_county_summarised) |> 
    group_by(county_name) |> 
    fill(all_of(c("county_total","county_total_known","county_pc_known")),.direction = 'updown') |> 
    ungroup() |> 
    mutate(county_origin_n = if_else(!is.finite(county_origin_n),0,county_origin_n),
           origin_pc = if_else(!is.finite(origin_pc),0,origin_pc),
           overall_pc = if_else(!is.finite(overall_pc),0,overall_pc),
           place_23nm_vals2 = if_else(is.na(place_23nm_vals2),"",place_23nm_vals2))
  
  output_df
}

# Plotting functions ##

## make leaflet county map
# plots GB counties, coloured by a column specified in the wrapper.

## make leaflet county map
# Plots GB counties, coloured by a column specified in the wrapper.
make_leaflet_map = function(
    leaflet_county_data = df,
    ptitle = "",
    gb_boundaries = GB_BOUNDARIES,
    zoom_level = 6,
    is_percentage = TRUE,
    fix_percent_scale = FALSE,
    zoom_restrict = FALSE,
    starting_zoom = 6) {
  
  # Clean up titles for presentation layout
  display_title <- case_when(
    ptitle == "county_pc_known" ~ "County % known",
    TRUE ~ ptitle)
  
  print(display_title)
  
  # Bind to shape file 
  leaflet_map_data_input = gb_boundaries |> 
    left_join(leaflet_county_data, by = c("name" = "county_name")) |> 
    mutate(my_value = replace_na(my_value, 0))
  
  # transform CRS
  leaflet_map_data_tf = st_transform(leaflet_map_data_input, crs = 4326)
  
  # Set colour scale ranges
  domain_vals = leaflet_map_data_tf$my_value
  max_domain_vals = max(domain_vals, na.rm = TRUE)
  min_domain_vals = min(domain_vals, na.rm = TRUE)
  
  if (is.infinite(max_domain_vals) || is.na(max_domain_vals)) {
    max_domain_vals <- 100
    min_domain_vals <- 0
  }
  
  if (fix_percent_scale) {
    max_upper <- max(100, max_domain_vals)
    map_bins <- seq(0, max_upper, length.out = 6) 
  } else {
    map_bins <- pretty(c(min_domain_vals, max_domain_vals), n = 5)
    if (min_domain_vals >= 0 && length(map_bins[map_bins < 0]) > 0) {
      map_bins <- map_bins[map_bins >= 0]
    }
  }
  
  # make base palette
  base_pal <- colorBin(
    palette = "YlOrRd",
    domain = domain_vals, 
    bins = map_bins,
    na.color = "white")
  
  # set zero as white
  my_color_pal <- function(x) {
    ifelse(!is.na(x) & x == 0, "#FFFFFF", base_pal(x))
  }
  
  # Make hover text
  hover_labels <- lapply(1:nrow(leaflet_map_data_tf), function(i) {
    
    row <- leaflet_map_data_tf[i,]
    
    county_name_text <- paste0("<span>", row$name, "</span><br>")
    
    # FIX: Added display_title before the value per your comment request
    percent_text=paste0(" ", row$county_origin_n, "/", row$county_total_known)
    origin_and_percent_text=paste0("<span>", display_title, " ", row$my_value, if (is_percentage) "%" else "", percent_text, "</span>")
    
    example_place_names_text <- {
      raw_val <- ifelse(is.na(row$place_23nm_vals2), "", row$place_23nm_vals2)
      raw_elements <- unlist(strsplit(raw_val, ";"))
      raw_elements <- raw_elements[raw_elements != ""]
      
      if (length(raw_elements) == 0) {
        ""
      } else {
        sampled_elements <- sample(raw_elements, min(length(raw_elements), 10))
        paired_lines <- sapply(seq(1, length(sampled_elements), by = 2), function(idx) {
          pair <- sampled_elements[idx:min(idx + 1, length(sampled_elements))]
          paste(pair, collapse = ", ")
        })
        paste0("<br><span style='font-size: 10px;'>",paste(paired_lines, collapse = ",<br>"),"</span>")
      }
    }
    
    htmltools::HTML(paste0(county_name_text, origin_and_percent_text, example_place_names_text))
  })
  
  # Make leaflet map proper ####
  leaflet(data=leaflet_map_data_tf,
          options=leafletOptions(
            minZoom = if(zoom_restrict) starting_zoom else 5,
            maxZoom = if(zoom_restrict) starting_zoom else 10,
            zoomControl = !zoom_restrict,
            dragging = !zoom_restrict,
            preferCanvas=TRUE,
            zoomAnimation=FALSE, fadeAnimation=FALSE)) |>
    setMaxBounds(lng1=-10, lat1=49, lng2=2, lat2=61) |>
    setView(lng=-2.75, lat=55.25, zoom=starting_zoom) |> 
    addPolygons(
      fillColor=~my_color_pal(my_value), fillOpacity=1,
      weight=.5, color='black',
      popup=hover_labels,
      popupOptions = popupOptions(
        autoPan = FALSE,keepInView = TRUE,maxHeight = 250)) |>
    addLegend(
      pal=base_pal,
      values=~my_value,
      opacity=1,
      title=display_title,
      position="topright")
}




## EXECUTE ####

# Base data prepartion ####

# load data
placenames_dat_raw = read_csv(DATA_PATH,show_col_types = F)

# remove obviously undesired rows
placenames_dat_filtered = do_initial_cleanup(
  input_df = placenames_dat_raw,
  place_types = PLACE_TYPES)

# Assign counties to place names, based on lat and long,
# compared to gb_boundaries map object.
place_county_reference = assign_countries_from_sf(
  input_df = placenames_dat_filtered,
  gb_boundaries = GB_BOUNDARIES)

# do a second level of filtering
placenames_prepped=secondary_prep_placenames(
  main_input_df = placenames_dat_filtered,
  reference_df = place_county_reference)


## Start of variable code ####

## Assign origins to searchable place names
## Here, the 'default' lookup_list is PLACENAME_ORIGIN_SEARCH_STRINGS
## It would be good if:
## 1. The user could add, modify or remove which prefix/suffix search patterns were included in the origin patterns.
## 2. Rather than using the 'origin' search patterns, the user could input their own arbitrary (alphabetical) search pattern.
# this slightly affects downstream logic, but adds functional power to the app.
# one way in which it might affect downstream logic is that when using the common_words constant, which are used to simplfy the input data,
# in doing so, this might also remove the user's search strings from the target..so every place name and county would be zero.

origin_assignment_result = match_origin_to_placename(
  input_df = placenames_prepped,
  lookup_list = PLACENAME_ORIGIN_SEARCH_STRINGS)

## Combine the placename-origin result with the more detailed place name information
placenames_with_origins = combine_results_with_info(
  origin_assignment_result_df = origin_assignment_result,
  placenames_prepped_df = placenames_prepped)

# Count the number of placenames of each origin in each county,
# then express as a percentage of the total.
# Then expand the data set, so each county has a row for each origin, even in the count is zero.
placenames_fin_sum_exploded = make_county_level_results(
  input_df = placenames_with_origins)


# MAKE PLOTS ####
# make leaflet plots
# if e.g. origin_pc, is chosen as the plot_column,
# then the list loops through each origin (norman, old norse etc),
# making a plot for each.
# if a generic plot_column is used, e.g. county_pc_known, then only a single plot is needed.
# 
# leaflet_plots = leaflet_multi_wrapper(
#   input_df = placenames_fin_sum_exploded,
#   plot_column = "origin_pc")
# plot_column = "county_pc_known")
# leaflet_plots

## plot execution ####
# Ideally, any of the variables could be plotted, and on single or multiple graphs, on a front end.
# Also I probably need some other analyses and plots.
# basic plots could be a horizontal bar chart, for each origin, ranking the county_name by the percentage of place names from that Origin.
# It would probably be interesting for the user to see how much the actual origin search strings are in each county.
# Finally, it would be good to plot all the place names individually by lat and long, coloured by Origin.
pig = placenames_fin_sum_exploded |> 
  select(county_name,
         origin,
         county_total_known,county_origin_n,place_23nm_vals2,origin_pc,overall_pc) |>
  filter(origin == 'Celtic') |>
  mutate(my_value = origin_pc) |> 
  distinct()
pig

make_leaflet_map(
    leaflet_county_data = pig,
    ptitle = "",
    gb_boundaries = GB_BOUNDARIES,
    zoom_level = 6,
    is_percentage = TRUE,
    fix_percent_scale = FALSE,
    zoom_restrict = FALSE,
    starting_zoom = 6)
  
  
  




# # leaflet multi-plot wrapper function
# # wrapper is useful for making multiple map plots, each for a different 'origin'.
# # e.g. columns "county_origin_n","origin_pc","overall_pc"
# leaflet_multi_wrapper = function(
#     input_df = placenames_fin_sum_exploded,
#     plot_column = "origin_pc"){
# 
#   if (plot_column %in% c("county_origin_n","origin_pc","overall_pc")){
# 
#     input_df_filtered = input_df %>%
#       { if ( plot_column == "origin_pc") filter(.,origin!="Unknown") else . } %>%
#       ungroup()
# 
#     origin_names = sort(unique(input_df_filtered$origin))
# 
#     output_plot = lapply(origin_names,function(z){
# 
#       df = input_df_filtered |>
#         filter(origin == z)|>
#         select(
#           county_name,origin,my_value := {{plot_column}},
#           place_23nm_vals2,
#           county_total,county_total_known, county_origin_n) |>
#         filter(is.finite(my_value)) |>
#         distinct() |>
#         as.data.frame()
# 
#       make_leaflet_map(
#         leaflet_county_data = df,
#         ptitle = df$origin[1],
#         gb_boundaries = GB_BOUNDARIES,
#         fix_percent_scale = FALSE,
#         zoom_restrict = TRUE,
#         starting_zoom = 5)
#     })
#   }
# 
#   if (!plot_column %in% c("county_origin_n","origin_pc","overall_pc")){
# 
#     df = input_df |>
#       select(county_name,my_value := {{plot_column}}) |>
#       filter(is.finite(my_value)) |>
#       distinct() |>
#       as.data.frame()
# 
#     is_pc = ifelse(grepl("_pc",plot_column),TRUE,FALSE)
# 
#     output_plot = make_leaflet_map(
#       leaflet_county_data = df,
#       ptitle = paste(as.character({{plot_column}})),
#       gb_boundaries = GB_BOUNDARIES,
#       is_percentage = is_pc,
#       fix_percent_scale = FALSE,
#       zoom_restrict = TRUE,
#       starting_zoom = 5)
#   }
# 
#   return(output_plot)
# }
# 
# 
# leaflet_plots = leaflet_multi_wrapper(
#     input_df = placenames_fin_sum_exploded,
#     plot_column = "origin_pc")
#   
#   
# # memory intensive multiple plots.
# # htmltools::browsable(
# #   htmltools::tagList(
# #     htmltools::tags$head(
# #       htmltools::tags$style(htmltools::HTML(".leaflet-container { background: #add8e6; }"))),
# #     do.call(leafsync::sync, c(leaflet_plots, list(sync.cursor = FALSE)))))
# 
# htmltools::browsable(
#   htmltools::tagList(
#     htmltools::tags$head(
#       htmltools::tags$style(htmltools::HTML("
#         .leaflet-container { background: #add8e6; }
#         /* Add minimal outer padding around the maps if you want breathing room */
#         body { margin: 10px; } 
#       "))
#     ),
#     do.call(leafsync::sync, c(leaflet_plots, list(ncol = 3, full.height = FALSE, sync.cursor = FALSE)))
#   )
# )


# leaflet multi-plot wrapper function
leaflet_multi_wrapper = function(
    input_df = placenames_fin_sum_exploded,
    plot_column = "origin_pc"){
  
  # USE IF-ELSE to prevent code execution bleeding or variable overwriting
  if (plot_column %in% c("county_origin_n", "origin_pc", "overall_pc")){
    
    input_df_filtered = input_df %>%
      { if ( plot_column == "origin_pc") filter(., origin != "Unknown") else . } %>%
      ungroup()
    
    origin_names = sort(unique(input_df_filtered$origin))
    
    output_plot = lapply(origin_names, function(z){
      
      df = input_df_filtered |>
        filter(origin == z)|>
        select(
          county_name, origin, my_value := {{plot_column}},
          place_23nm_vals2,
          county_total, county_total_known, county_origin_n) |>
        filter(is.finite(my_value)) |>
        distinct() |>
        as.data.frame()
      
      make_leaflet_map(
        leaflet_county_data = df,
        ptitle = df$origin[1],
        gb_boundaries = GB_BOUNDARIES,
        fix_percent_scale = FALSE,
        zoom_restrict = TRUE,
        starting_zoom = 5)
    })
    
  } else { # This block only runs if the first one doesn't!
    
    df = input_df |>
      select(county_name, my_value := {{plot_column}}) |>
      filter(is.finite(my_value)) |>
      distinct() |>
      as.data.frame()
    
    is_pc = ifelse(grepl("_pc", plot_column), TRUE, FALSE)
    
    output_plot = make_leaflet_map(
      leaflet_county_data = df,
      ptitle = paste(as.character({{plot_column}})),
      gb_boundaries = GB_BOUNDARIES,
      is_percentage = is_pc,
      fix_percent_scale = FALSE,
      zoom_restrict = TRUE,
      starting_zoom = 5)
  }
  
  return(output_plot)
}

# Run the wrapper
leaflet_plots = leaflet_multi_wrapper(
  input_df = placenames_fin_sum_exploded,
  plot_column = "origin_pc")

# Remove slot names to ensure leafsync reads the items purely as an indexed list
clean_plots <- unname(leaflet_plots)

# Final grid output rendering (3 wide, 2 high proportions)
htmltools::browsable(
  htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML("
        .leaflet-container { background: #add8e6; }
        body { margin: 10px; } 
        /* Controls the aspect ratio layout cleanly */
        .leafsync-container, .sync-container {
          display: grid !important;
          grid-template-columns: repeat(3, 1fr) !important; 
          gap: 10px !important;
          width: 100% !important;
          height: auto !important;
        }
        .leafsync-container > div, .sync-container > div {
          height: 380px !important; /* Makes each plot wider than it is high */
          width: 100% !important;
        }
      "))
    ),
    do.call(leafsync::sync, c(clean_plots, list(ncol = 3, sync.cursor = FALSE)))
  )
)

