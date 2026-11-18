
#  FUNCTIONS ####

# pulls string and origin info from wiki url
get_wiki_placenames_lookup=function(wiki_url=WIKI_URL){
  loaded_wiki<-wiki_url%>%
    read_html()%>%
    html_nodes(table)
  output=bind_rows(html_table(loaded_wiki))
  output
}




# Cleaning code for wiki data
# rename ?

## Positions of terms are not always given, which makes pattern matching badly non-specific.
## As a solution, infer the position using the provided examples.

# Expand terms in brackets e.g. clo(u)(gh) -> c(clo, clou, clogh, clough)
# expand_optional_parens <- function(terms) {
#   vapply(terms, function(t) {
#     if (!stri_detect_fixed(t, ()) return(t)
#     # Split into literal tokens optional tokens
#     parts <- stri_extract_all_regex(t, \\([^)]+\\)|[^()]+)[[1]]
#     opts <- lapply(parts, function(p) {
#       if (stri_startswith_fixed(p, ()) {
#         c(, stri_sub(p, 2, -2))
#       } else {
#         p
#       }
#     })
#     expanded <- do.call(paste0, expand.grid(opts, stringsAsFactors = FALSE))
#     paste(unique(expanded), collapse = ,)
#   }, character(1), USE.NAMES = FALSE)
# }


expand_optional_parens <- function(terms) {
  vapply(terms, function(cell) {
    if (!stri_detect_fixed(cell, "(")) return(cell)
    
    # Split comma-separated terms and extract character vector with [[1]]
    sub_terms <- stri_split_fixed(cell, ",")[[1]]
    
    expanded_all <- unlist(lapply(sub_terms, function(t) {
      if (!stri_detect_fixed(t, "(")) return(t)
      
      # Extract optional "(...)" blocks and literal chunks
      parts <- stri_extract_all_regex(t, "\\([^)]+\\)|[^()]+")[[1]]
      
      opts <- lapply(parts, function(p) {
        if (stri_startswith_fixed(p, "(")) {
          inner <- stri_sub(p, 2, -2)
          if (stri_detect_fixed(inner, "/")) {
            stri_split_fixed(inner, "/")[[1]]
          } else {
            c("", inner)
          }
        } else {
          p
        }
      })
      
      grid <- expand.grid(opts, stringsAsFactors = FALSE)
      do.call(paste0, grid)
    }))
    
    paste(unique(expanded_all[expanded_all != ""]), collapse = ",")
  }, character(1), USE.NAMES = FALSE)
}




prepare_wiki_data <- function(
    input_df = wiki_table_raw,
    key_to_languages = KEY_TO_LANGUAGES,
    allowed_string_positions = ALLOWED_STRING_POSITIONS) {
  
  valid_langs <- names(key_to_languages)
  
  # initial standardisation
  clean_base <- input_df |>
    mutate(source_row_number = row_number()) |>
    select(
      term = Term, origin = Origin, position = Position,
      example = Example, meaning = Meaning, source_row_number) |>
    distinct() |>
    mutate(
      across(c(term, position, example), tolower),
      # remove characters in brackets
      across(c(term, origin, position, example), ~ stri_replace_all_regex(.x, "\\s*\\[[^]]*\\]", "")),
      across(c(origin, position, example), ~ stri_replace_all_regex(.x, "\\s*\\([^)]*\\)", "")),
      # deal with term column
      # standardise weird strings like ; also and and by replacing them with a comma...
      term = stri_replace_all_regex(term, ";\\s*also\\s+|\\s+and\\s+", ","),
      term = stri_replace_all_regex(term, "[-/\\s]", ""),
      # deal with origin column. discard invalid origins.
      origin = vapply(stri_split_regex(origin, "[,/\\s]+"), function(x) {
        x <- sort(unique(x[x %in% valid_langs]))
        paste(x, collapse = ",")
      }, character(1)),
      # deal with postions column
      position = if_else(term == "upon,on,in", "interfix", position),
      position = vapply(stri_split_regex(position, "[,\\s]+"), function(x) {
        x <- sort(unique(x[x %in% allowed_string_positions]))
        paste(x, collapse = ",")
      }, character(1))) |>
    filter(origin != "", term != "") |>
    # Explode terms with brackets
    mutate(term = expand_optional_parens(term))
  
  
  # Explode term and example terms
  term_example_pairs <- clean_base |>
    separate_longer_delim(term, delim = ",") |>
    separate_longer_delim(example, delim = ",") |>
    mutate(
      term = stri_trim_both(term),
      example = stri_trim_both(example)) |>
    filter(
      term != "",
      example != "",
      example != "all surrounding the city of exeter",
      !stri_detect_fixed(example, "people of")) |>
    mutate(
      # Clean noise words but keep dash for interfix matching
      example_hyphen = example |>
        stri_replace_all_regex("\\.|\\b(river|country|park|county|high|king's|the)\\b", " ") |>
        stri_trim_both() |>
        stri_replace_all_regex("\\s+", " "),
      # Space-only version for prefix/suffix/standalone matching
      example_clean = stri_trim_both(stri_replace_all_regex(stri_replace_all_fixed(example_hyphen, "-", " "), "\\s+", " "))) |>
    filter(example_clean != "")
  
  
  # placename origins lookup table
  source_placenames_origins <- term_example_pairs |>
    distinct(place_name = example_clean, origin) |>
    summarise(
      origins = paste(sort(unique(unlist(stri_split_fixed(origin, ",")))), collapse = ","),
      .by = place_name)
  
  
  # position inference when missing position
  inferred_positions <- term_example_pairs |>
    filter(position == "", stri_detect_fixed(example_clean, term)) |>
    distinct(term, origin, example_clean, example_hyphen) |>
    mutate(
      is_standalone = if ("standalone" %in% allowed_string_positions)
        stri_detect_regex(example_clean, paste0("(^|\\s)", term, "($|\\s)")) else FALSE,
      is_prefix = if ("prefix" %in% allowed_string_positions)
        stri_detect_regex(example_clean, paste0("(^|\\s)", term)) else FALSE,
      is_suffix = if ("suffix" %in% allowed_string_positions)
        stri_detect_regex(example_clean, paste0(term, "($|\\s)")) else FALSE,
      is_interfix = if ("interfix" %in% allowed_string_positions)
        stri_detect_regex(example_hyphen, paste0("(-|\\s)", term, "(-|\\s)")) else FALSE) |>
    pivot_longer(
      cols = starts_with("is_"),
      names_to = "inferred_pos",
      names_prefix = "is_",
      values_to = "matched") |>
    filter(matched) |>
    summarise(
      inferred_position = paste(sort(unique(inferred_pos)), collapse = ","),
      .by = c(term, origin))
  
  # Combine inferred positions with source-defined positions
  term_origin_position_dat <- clean_base |>
    separate_longer_delim(term, delim = ",") |>
    mutate(term = stri_trim_both(term)) |>
    filter(term != "") |>
    left_join(inferred_positions, by = c("term", "origin")) |>
    mutate(position = if_else(position == "", coalesce(inferred_position, ""), position)) |>
    filter(position != "") |>
    separate_longer_delim(position, delim = ",") |>
    summarise(
      origin = paste(sort(unique(unlist(stri_split_fixed(origin, ",")))), collapse = ","),
      .by = c(term, position, meaning)) |>
    mutate(term_len = nchar(term)) |>
    select(term, origin, position, meaning, term_len) |>
    arrange(term, origin, position)
  
  list(source_placenames_origins = source_placenames_origins,
       term_origin_position_dat = term_origin_position_dat)
} # eof




# clean place names data
clean_placenames_data <- function(
    input_df,
    place_types = PLACE_TYPES,
    lead_remove_strings = LEAD_REMOVE_STRINGS,
    terminal_remove_strings = TERMINAL_REMOVE_STRINGS) {
  
  
  all_lead_and_termin_strings <- unique(c(lead_remove_strings, terminal_remove_strings))
  
  # Pre-compile compact grouped regex patterns
  lead_pattern <- paste0("^(", paste(lead_remove_strings, collapse = "|"), ")\\s+")
  term_pattern <- paste0("\\s+(", paste(terminal_remove_strings, collapse = "|"), ")$")
  bad_suffix_pattern <- "\\s+(church|closes?|copses?|lands?|parks?|shop|square)$"
  
  input_df |>
    # 1. Filter early on un-mutated columns before running expensive string ops
    filter(
      descnm %in% place_types,
      !(cty61nm == "Cornwall and Isles of Scilly" & descnm == "LOC"),
      lat != 0, long != 0, lat > 49, long > (-8)
    ) |>
    select(tempcode, placeid, place23nm, descnm, lat, long, ctry23nm, cty61nm) |>
    # 2. Fast C++ ASCII transliteration (replaces iconv)
    mutate(place23nm_repaired = stri_trans_general(place23nm, "Latin-ASCII")) |>
    filter(
      place23nm_repaired != "Area not comprised",
      !stri_detect_regex(place23nm_repaired, bad_suffix_pattern, case_insensitive = TRUE)
    ) |>
    mutate(
      place23nm_repaired_length = stri_length(place23nm_repaired),
      
      # 3. Single-pass cleanup for place23nm_mod
      place23nm_mod = place23nm_repaired |>
        stri_replace_all_regex("\\s*\\([^)]*\\)|\\s+\\b(by|in|and)\\b.*", "") |>
        stri_replace_all_regex("\\s+", " ") |>
        stri_trim_both(),
      
      # 4. Vectorised normalisation for searchable_placename
      searchable_placename = stri_trans_tolower(place23nm_mod) |>
        stri_replace_all_fixed("-", " ") |>
        stri_replace_all_fixed(".", "") |>
        stri_replace_all_regex("(,|\\s+&|\\s+with\\s+|\\s+urban|\\s+on\\s+).*", "") |>
        stri_trim_both() |>
        # Two passes of leading & terminal word stripping (for compound modifiers like "New High ...")
        stri_replace_first_regex(lead_pattern, "") |>
        stri_replace_first_regex(term_pattern, "") |>
        stri_replace_first_regex(lead_pattern, "") |>
        stri_replace_first_regex(term_pattern, "") |>
        stri_trim_both(),
      
      searchable_placename_length = stri_length(searchable_placename),
      searchable_placename = if_else(
        searchable_placename == "" | searchable_placename %in% all_lead_and_termin_strings,
        NA_character_,
        searchable_placename
      )
    )
} # eof




# Assign counties to place names based on their lat/long and a reference map
# rename ? variable names
assign_counties_from_sf=function(
    input_df = placenames_initial_clean,
    gb_boundaries=GB_BOUNDARIES){
  
  geo_dat_input=input_df|>
    select(tempcode,placeid,place23nm_mod,lat,long)|>
    distinct()
  
  geo_dat_sf=st_as_sf(geo_dat_input,coords=c(long,lat),crs=4326,remove=FALSE)
  geo_lookup_result=st_join(geo_dat_sf,gb_boundaries,join=st_intersects)
  
  place_county_reference=geo_lookup_result|>
    st_drop_geometry()|>
    select(tempcode,placeid,
           place23nm_mod,
           lat,long,county_name=name)
  
  known_towns=place_county_reference|>filter(!is.na(county_name)&county_name!="")
  missing_towns=place_county_reference|>filter(is.na(county_name)|county_name=="")
  
  if(nrow(missing_towns)>0){
    known_sf=st_as_sf(known_towns,coords=c(long,lat),crs=4326)
    missing_sf=st_as_sf(missing_towns,coords=c(long,lat),crs=4326)
    nearest_index=st_nearest_feature(missing_sf,known_sf)
    missing_towns$county_name=known_towns$county_name[nearest_index]
  }
  
  input_df |> left_join(bind_rows(known_towns,missing_towns))
}



# Match origin search patterns to place names
# regex processor core function.
# used in the match_origin_to_placename function
# too many non-specific matches. review logic.
make_placename_pattern_lookup=function(lookup_list=NULL){
  
  imap_dfr(lookup_list,function(origin_list,position){
    imap_dfr(origin_list,function(elements,origin){
      
      processed_elements <- if (position==prefix){
        paste0("^",elements)
        
      } else if (position==suffix) {
        paste0(elements,"$")
        
      } else {
        # elements
        paste0(" ",elements," ")
      }
      tibble(origin=origin,pattern=paste(processed_elements,collapse="|"))
    })})|>
    group_by(origin)|>
    summarise(pattern=paste0(" ",pattern," ",collapse="|"),.groups="drop")
} # eof




# Make search terms into list structure
# makes a nested list, using three columns of the input table
# also filters-out terms less than a specified length
make_search_terms_list = function(
    input_df = wiki_prepped$term_origin_position_dat,
    min_term_length = 2){
  output_list <- input_df |>
    filter(term_len>=min_term_length) |> 
    select(position,origin,term) %>%
    split(.$position) %>%
    map(~ split(.x$term, .x$origin))
  output_list
} # eof



## Wrapper to match origin to place name ##
match_origin_to_placename <- function(
    origin_to_placename_input_df = placenames_matched_with_counties,
    search_terms_df = wiki_prepped$term_origin_position_dat,
    min_term_length = 2) {
  
  
  # make nested search terms reference list
  search_terms_list = make_search_terms_list(
    input_df = search_terms_df,
    min_term_length = min_term_length ## is the repeat use of the variable name ok ??
  )
  
  
  all_terms_concat = paste0(sort(unique(search_terms_df$term)),collapse="|")
  
  # Filter place names data so that only those with any pattern matches are considered.
  # This is in order to reduce down the search space.
  filtered_searchable_placenames_df = origin_to_placename_input_df |>
    select(searchable_placename)|>
    filter(grepl(all_terms_concat,searchable_placename))  |>
    distinct() |>
    arrange(searchable_placename)
  
  # too many non-specific matches. review logic.
  compiled_lookup <- make_placename_pattern_lookup(lookup_list=search_terms_list)
  
  # Extract the exact matching substrings from the place names for each pattern
  # Returns a list of character vectors (one per pattern, length = nrow(input_df))
  match_matrix <- map(compiled_lookup$pattern, \(pat) {
    stringr::str_extract(filtered_searchable_placenames_df$searchable_placename,
                         stringr::regex(pat, ignore_case = TRUE))
  })
  
  # Collapse the matches for each row into comma-separated strings
  match_results <- map_dfr(1:nrow(filtered_searchable_placenames_df), \(row_idx) {
    
    # Get the extracted substrings across all patterns for this specific row
    row_matches <- map_chr(match_matrix, ~ .x[row_idx])
    valid_indices <- !is.na(row_matches)
    
    if (!any(valid_indices)) {
      return(tibble(comb_origins = "Unknown", exact_match_text = NA_character_))
    }
    
    # Extract unique origins and the actual text segments that matched
    unique_origins <- sort(unique(compiled_lookup$origin[valid_indices]))
    unique_substrings <- sort(unique(row_matches[valid_indices]))
    
    tibble(
      comb_origins = paste0(unique_origins, collapse = ","),
      exact_match_text = paste0(unique_substrings, collapse = ","))
    
  })
  
  # Append results back to data frame
  origin_to_placename_output_df = filtered_searchable_placenames_df |>
    mutate(
      comb_origins = match_results$comb_origins,
      exact_match_text = match_results$exact_match_text)
  
  origin_to_placename_output_df
} ## eof




## summarise the assignment of origins to place names
## outputs a list of relevant tables
post_prep_matched_names <- function(
    assignment_result_df = res,
    placenames_matched_with_counties_df = placenames_matched_with_counties) {
  
  print("running post_prep_matched_names")
  
  ## summarise the counts
  intial_sum_df <- assignment_result_df |>
    filter(!is.na(comb_origins), comb_origins != "Unknown") |>
    group_by(searchable_placename) |>
    summarise(
      comb_origins = paste0(sort(unique(comb_origins)), collapse = ","),
      text_matches = paste0(unique(exact_match_text), collapse = ","),
      .groups = "drop")
  
  ## join place names with origins to main data
  origin_placename_matched <- placenames_matched_with_counties_df |>
    left_join(intial_sum_df, by = "searchable_placename") |>
    mutate(comb_origins = if_else(is.na(comb_origins), "Unknown", comb_origins))
  
  ## filter joined data to reduce duplication
  matched_and_filtered_df <- origin_placename_matched |>
    mutate(placename_length = nchar(place23nm_repaired)) |>
    filter(!grepl(",", place23nm_repaired, fixed = TRUE)) |>
    group_by(placeid) |>
    filter(placename_length == min(placename_length)) |>
    arrange(lat, long, .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    group_by(lat, long) |>
    filter(placename_length == min(placename_length)) |>
    ungroup() |>
    group_by(county_name, searchable_placename) |>
    arrange(lat, long, .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    filter(comb_origins != "Unknown") |>
    select(county_name, comb_origins, place23nm_mod, text_matches) |>
    mutate(rn = row_number())
  
  ## vectorised explosion: one row per unique origin, place, and text match
  matched_exploded_df <- matched_and_filtered_df |>
    separate_longer_delim(comb_origins, delim = ",") |>
    mutate(comb_origins = trimws(comb_origins)) |>
    filter(comb_origins != "") |>
    distinct(rn, county_name, comb_origins, place23nm_mod, text_matches)
  
  ## summary count the number of each origin in each county, weighted by number of origins
  county_summary_df <- matched_exploded_df |>
    dplyr::rename(origin = comb_origins, text_match = text_matches) |>
    group_by(county_name, place23nm_mod, text_match) |>
    mutate(n_origins = n()) |>
    ungroup() |>
    mutate(place_wt = 1 / n_origins) |>
    group_by(county_name, origin, text_match) |>
    mutate(disp_places = paste0(head(sort(unique(place23nm_mod)), 5), collapse = ", ")) |>
    ungroup() |>
    mutate(disp_str = paste0(text_match, ": ", disp_places)) |>
    group_by(county_name, origin) |>
    summarise(
      n_places = sum(place_wt, na.rm = TRUE),
      strings_and_matches = paste0(sort(unique(disp_str)), collapse = ". "),
      .groups = "drop"
    ) |>
    mutate(strings_and_matches = gsub(" [,]", "", strings_and_matches)) |>
    group_by(county_name) |>
    mutate(n_county_places = sum(n_places)) |>
    ungroup() |>
    mutate(pc = round(100 * (n_places / n_county_places), 2))
  
  ## expand grid for mapping so missing county-origin pairs show as zero
  county_plot_df <- crossing(
    county_name = sort(unique(county_summary_df$county_name)),
    origin = sort(unique(county_summary_df$origin))
  ) |>
    left_join(county_summary_df, by = c("county_name", "origin")) |>
    group_by(county_name) |>
    fill(all_of(c("n_county_places")), .direction = "updown") |>
    ungroup() |>
    mutate(across(where(is.numeric), ~ coalesce(., 0))) |>
    left_join(key_to_languages_table, by = "origin") |>
    mutate(origin = full_origin) |>
    select(-full_origin)
  
  outlist <- list(
    matched_and_filtered_dat = matched_and_filtered_df,
    county_plot_dat = county_plot_df
  )
  
  print("completed post_prep_matched_names")
  outlist
}



# County map coloured by percentage ##
# made with leaflet
# Plots GB counties, coloured by a column specified in the wrapper.
make_leaflet_map=function(
    leaflet_county_data=df,
    ptitle="",
    gb_boundaries=GB_BOUNDARIES,
    zoom_level=6,
    is_percentage=TRUE,
    fix_percent_scale=FALSE,
    zoom_restrict=FALSE,
    starting_zoom=6){
  
  # Bind to shape file
  boundaries_with_variables_data=gb_boundaries|>
    left_join(leaflet_county_data,by=c("name"="county_name"))|>
    mutate(pc=replace_na(pc,0))
  
  # transform CRS
  map_data_tf=st_transform(boundaries_with_variables_data,crs=4326)
  
  # Set colour scale ranges
  domain_vals=map_data_tf$pc
  max_domain_vals=max(domain_vals,na.rm=TRUE)
  min_domain_vals=min(domain_vals,na.rm=TRUE)
  
  if (fix_percent_scale){
    max_upper<-max(c(100,max_domain_vals),na.rm=T)
    map_bins<-seq(0,max_upper,length.out=6)
  } else {
    map_bins<-pretty(c(min_domain_vals,max_domain_vals),n=5)
    if (min_domain_vals>=0&&length(map_bins[map_bins<0])>0) {
      map_bins<-map_bins[map_bins>=0]
    }
  }
  
  # make base palette
  base_pal=colorBin(
    palette=YlOrRd,
    domain=domain_vals,
    bins=map_bins,
    na.color="white")
  
  # set zero as white
  my_color_pal=function(x) {
    ifelse(!is.na(x) & x == 0,"#FFFFFF",base_pal(x))
  }
  
  
  # Make hover text
  hover_labels=lapply(1:nrow(map_data_tf),function(i) {
    
    row<-map_data_tf[i,]
    
    county_name_text=paste0("<span>",row$name,"</span><br>")
    
    origin_and_percent_text = paste0(
      "<span>", round(row$pc), "%" , ptitle,
       # ("," round(row$n_places), "/", row$n_county_places, )"</span>"
      " (", round(row$n_places), "/", row$n_county_places, ")</span>"
    )
    
    example_place_names_text <- as.character("")
    
    
    if (!is.na(row$strings_and_matches[1])) {
      
      hover_subtext = gsub("[.]","<br>",row$strings_and_matches[1])
      example_place_names_text <- paste0("<br><span style='font-size:12px;'>",hover_subtext,"</span>")
    }
    
    htmltools::HTML(paste0(county_name_text, origin_and_percent_text, example_place_names_text))
  })
  
  # Make leaflet map proper
  leaflet(
    data=map_data_tf,
    options=leafletOptions(
      minZoom=if(zoom_restrict)starting_zoom else 5,
      maxZoom=if(zoom_restrict)starting_zoom else 10,
      zoomControl=!zoom_restrict,
      dragging=!zoom_restrict,
      preferCanvas=TRUE,
      zoomAnimation=FALSE,fadeAnimation=FALSE))|>
    setMaxBounds(lng1=-10,lat1=49,lng2=2,lat2=61)|>
    setView(lng=-2.75,lat=55.25,zoom=starting_zoom)|>
    addPolygons(
      fillColor=~my_color_pal(pc),fillOpacity=1,
      weight=.5,color='black',
      popup=hover_labels,
      popupOptions=popupOptions(
        autoPan=FALSE,keepInView=TRUE,maxHeight=150,maxWidth = 100))|>
    addLegend(
      pal=base_pal,
      values=~pc,
      opacity=1,
      title=ptitle,
      position="topright")
} # eof



# Leaflet multi-plot wrapper function
leaflet_multi_wrapper = function(
  input_df = post_prep_result_list$county_plot_dat){
  
  origin_names = sort(unique(input_df$origin))
  
  output_plot = lapply(origin_names, function(z){
    
    df = input_df |>
      filter(origin == z)|>
      select(
        county_name, origin, pc,
        strings_and_matches,
        n_county_places, n_places) |>
      filter(is.finite(pc)) |>
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
  
  clean_plots <- unname(output_plot)
  
  
  # grid output
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
        }"
      ))
      ),
      do.call(leafsync::sync, c(clean_plots, list(ncol = 3, sync.cursor = FALSE)))
    )
  )
  
} # eof

