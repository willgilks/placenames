
#  FUNCTIONS ####

# pulls string and origin info from wiki url
get_wiki_placenames_lookup=function(wiki_url=WIKI_URL){
  loaded_wiki<-wiki_url%>%
    read_html()%>%
    html_nodes("table")
  output=bind_rows(html_table(loaded_wiki))
  output
}




# Cleaning code for wiki data
# rename ?

## Positions of terms are not always given, which makes pattern matching badly non-specific.
## As a solution, infer the position using the provided examples.
prepare_wiki_data = function(
    input_df = wiki_table_raw,
    key_to_languages=KEY_TO_LANGUAGES,
    allow_string_positions=ALLOWED_STRING_POSITIONS){
  
  input_df_copy = input_df
  
  # remove text in brackets
  # from term, remove dash and slash and whitespace.
  # from origin replace slash or whitespace with comma, then replace double comma with single
  # add row number for splitting
  initial_clean_df = input_df_copy |> 
    mutate(source_row_number = row_number()) |>
    select(term=Term, origin = Origin, position=Position, example = Example, meaning = Meaning,source_row_number) |> 
    distinct() |> 
    mutate(across(c("term","position","example"), tolower)) |> 
    mutate(across(all_of(c("term","origin","position","example")),~gsub("\\s*\\[[^]]*\\]","",.x)))|>
    mutate(across(all_of(c("term","origin","position","example")),~gsub("\\s*\\([^)]*\\)","",.x)))|>
    mutate(term=gsub("[-]|[/]|\\s","",term)) |> 
    mutate(term=gsub("[-]|[/]|\\s","",term)) |> 
    mutate(origin=gsub("[/]|\\s",",",origin)) |> 
    mutate(origin=gsub(",,",",",origin)) |>
    mutate(position = gsub("\\s",",",position)) |> 
    mutate(position = gsub(",,",",",position)) |> 
    mutate(position = gsub(",,",",",position)) |> 
    mutate(position = gsub(",$","",position)) |> 
    mutate(position=if_else(term=="upon[,]on[,]in","interfix",position)) |> 
    filter(origin!="") |> 
    mutate(rn = row_number())
  
  
  # De-concatenate, so that each origin, term, position and example has its own row
  # replace standalone position with interfix
  deconcatenated_df = bind_rows(
    lapply(split(initial_clean_df,initial_clean_df$rn),
           function(z){
             crossing(
               source_row_number = z$source_row_number[1],
               origin=unlist(str_split(z$origin[1],",")),
               term = unlist(str_split(z$term[1],",")),
               position = unlist(str_split(z$position[1],",")),
               example = unlist(str_split(z$example[1],","))) |> 
               mutate(meaning=z$meaning[1])
           }))
  
  # remove rows with bad origins or positions text.
  # group by term and example, then concatenate origin and position.
  df_filtered = deconcatenated_df|>
    mutate(example=trimws(example)) |> 
    filter(!example %in% c("all surrounding the city of exeter")) |> 
    filter(origin %in% names(key_to_languages),term!="",position %in% c("",allow_string_positions)) |>
    filter(!grepl("people of",example)) |>
    mutate_if(is.character, str_trim)|> 
    mutate(example=gsub("[-]|[.]|river |country | park|park |county |high |king[']s |the "," ",example)) |> 
    mutate(example=gsub("  "," ",example)) |> 
    mutate(example=trimws(example))
  
  
  # make table of pre-assigned placename origins according to the source
  source_placenames_origins=df_filtered |> 
    filter(example!="",!is.na(example)) |> 
    mutate(example = trimws(example)) |> 
    distinct() |> 
    group_by(example) |> 
    summarise(origins = paste0(sort(unique(paste0(origin))),collapse=','),.groups='drop') |> 
    dplyr::rename(place_name = example)
  
  
  ## where position data are missing
  ## test the example placenames against the term that they are associated with
  ## create search patterns for grepl to whether the string is a prefix, suffix etc.
  ## if the search pattern is found, then that indicates that particular position.
  ## then extract where the positions
  inferred_string_position_data = bind_rows(lapply(allow_string_positions, function(z){
    
    z_base = df_filtered|>
      # remove instances where the term is not found in the example.
      rowwise() |> 
      filter(grepl(term,example))|> 
      ungroup() |> 
      select(source_row_number,origin,term,example) |> 
      distinct()
    
    if (z=="prefix"){
      z_dat = z_base |> 
        mutate(search_expr = paste0( paste0("^",term) ,"|",paste0(" ",term)))
    }
    
    if (z=="suffix"){
      z_dat = z_base |> 
        mutate(search_expr = paste0( paste0(term,"$") ,"|",paste0(term," ")))
    }
    
    if (z=="standalone"){
      z_dat = z_base |> 
        mutate(search_expr = paste0( paste0("^",term,"$") ,"|",paste0(" ",term," ")))
    }
    
    if (z=="interfix"){
      z_dat = z_base |> 
        mutate(search_expr = paste0( paste0("-",term,"-") ,"|",paste0(" ",term," ")))
    }
    
    z_out = z_dat |> 
      rowwise() |> 
      mutate(position = if_else(grepl(search_expr,example),z,as.character(""))) |> 
      ungroup() |> 
      filter(position!="")
    z_out
  }))
  
  
  # collapse the origin column, grouping by term and position
  # then collapse the position column, grouping by term and origin
  # this prevents merging where for strings the exact origin is dependent on the position in the placename, e.g. don
  redux_inferred_string_position_data = inferred_string_position_data |> 
    group_by(term,position) |> 
    summarise(
      origin = paste0(sort(unique(unlist(str_split(origin,",")))),collapse=","),
      .groups='drop') |> 
    group_by(term,origin) |>
    summarise(
      position = paste0(sort(unique(unlist(str_split(position,",")))),collapse=","),
      .groups='drop') |> 
    dplyr::rename(inferred_position = position)
  
  
  ## collapse the filtered df by term and position
  # then join the inferred position data, and complete where missing
  # then re-collapse, grouping again by term and position
  collapsed_df = df_filtered |> 
    group_by(term,position,meaning) |> 
    summarise(
      source_row_number = paste0(sort(unique(source_row_number)),collapse=","),
      origin = paste0(sort(unique(unlist(str_split(origin,",")))),collapse=","),
      example = paste0(sort(unique(unlist(str_split(example,",")))),collapse=","),
      .groups = 'drop') |> 
    left_join(redux_inferred_string_position_data) |> 
    mutate(position = if_else(is.na(position)|position=="",inferred_position,position)) |> 
    group_by(term,position,meaning) |> 
    summarise(
      source_row_number = paste0(sort(unique(source_row_number)),collapse=","),
      origin = paste0(sort(unique(unlist(str_split(origin,",")))),collapse=","),
      example = paste0(sort(unique(unlist(str_split(example,",")))),collapse=","),
      .groups = 'drop') |> 
    filter(!is.na(position),position!="") |> 
    mutate(rn = row_number())
  
  
  
  # expand/de-concatenate the position column
  df_with_exploded_position_column = bind_rows(
    lapply(split(collapsed_df,collapsed_df$rn), function(z){
      z |> 
        select(-position) |> 
        crossing(position = unlist(str_split(z$position[1],",")))
    })) |> 
    select(term,origin,position,meaning) |> 
    mutate(term_len = nchar(term))
  
  
  outlist = list(
    source_placename_origin_dat = source_placenames_origins,
    term_origin_position_dat = df_with_exploded_position_column)
  outlist
  
} ## EOF





# clean place names data
# rename ?
do_initial_cleanup=function(input_df,place_types=PLACE_TYPES){
  
  # these are used with a trailing white space in order to remove full word only.
  lead_remove_strings = c(
    "bank",
    "castle","chapel","east","easter","ferry",
    "garrison","garden","gardeners","great","gate",
    "high","higher",
    "low","lower","little",
    "marsh","middle",
    "new","north","old","port",
    "south","the","upper","west","windmill")
  
  lead_remove_strings_concat = gsub("[|]$","",paste0("^",lead_remove_strings," |",collapse = ""));
  
  terminal_remove_strings = c(
    "bank",
    "camp","castle","city","colliery","common","corner","cottages","creek","crossing",
    "east","easter","end","edge","estate",
    "docks","down","drive",
    "farm","ferry","field","fields","foot","forest",
    "garden","gardens","garrison","gate","granch","grange","green","grove",
    "house","junction","lane","lodge","lower",
    "marsh","mead","meadow","meadows","mill","mills","moor","old",
    "park","parks","passage","plain","place","port","priors",
    "quay","rd","side","south","st","street",
    "vale","valley","view","village")
  
  terminal_remove_strings_concat = gsub("[|]$","",paste0(" ",terminal_remove_strings,"$|",collapse = ""))
  
  
  all_lead_and_termin_strings = sort(unique(c(lead_remove_strings,terminal_remove_strings)))
  
  
  output_df = input_df|>
    select(tempcode,placeid,place23nm,descnm,lat,long,ctry23nm,cty61nm)|>
    # convert to ascii to remove nonstandard characters
    mutate(place23nm_repaired=iconv(place23nm,to="ASCII",sub="\x20")) |>
    # remove unusable place types
    filter(descnm%in%place_types)|>
    filter(!place23nm_repaired %in% c("Area not comprised")) |> 
    filter(!(cty61nm=="Cornwall and Isles of Scilly"&descnm=="LOC"))|> 
    filter(lat!=0,long!=0,lat>49,long>(-8))|>
    filter(!grepl(" church$| close$| closes$| copse$| land$| lands$| park$| parks$| shop$| square$",place23nm_repaired)) |> 
    mutate(place23nm_repaired_length=nchar(place23nm_repaired)) |> 
    # remove text in bracks and after by,in.
    mutate(place23nm_mod=place23nm_repaired) |> 
    mutate(place23nm_mod=gsub("\\s*\\([^)]*\\)","",place23nm_mod)) |> 
    mutate(place23nm_mod = gsub("\\s+\\b(by|in|and)\\b.*", "", place23nm_mod)) |>
    mutate(place23nm_mod=gsub("  "," ",place23nm_mod))|>
    mutate(place23nm_mod = trimws(place23nm_mod)) |> 
    mutate(searchable_placename=place23nm_mod) |> 
    # make lowercase, replace dash with space to standardise, remove anything after comma
    mutate(searchable_placename = tolower(searchable_placename)) |> 
    mutate(searchable_placename=gsub("-","\x20",searchable_placename))|>
    mutate(searchable_placename=gsub("[,].*","",searchable_placename))|>
    mutate(searchable_placename=gsub("[.]","",searchable_placename))|>
    mutate(searchable_placename=gsub(" [&].*","",searchable_placename))|>
    mutate(searchable_placename=gsub(" with .*| urban.*| on .*","",searchable_placename))|>
    mutate(searchable_placename=gsub(lead_remove_strings_concat,"",searchable_placename))|>
    mutate(searchable_placename=gsub(terminal_remove_strings_concat,"",searchable_placename))|>
    mutate(searchable_placename=gsub(lead_remove_strings_concat,"",searchable_placename))|>
    mutate(searchable_placename=gsub(terminal_remove_strings_concat,"",searchable_placename))|>
    mutate(searchable_placename_length=nchar(searchable_placename)) |> 
    mutate(searchable_placename=if_else(searchable_placename %in% all_lead_and_termin_strings,
                                        "",searchable_placename)) |> 
    mutate(searchable_placename=if_else(searchable_placename=="",as.character(NA),searchable_placename))
  return(output_df)
} # eof



# Assign counties to place names based on their lat/long and a reference map
# rename ? variable names
assign_counties_from_sf=function(
    input_df = placenames_initial_clean,
    gb_boundaries=GB_BOUNDARIES){
  
  geo_dat_input=input_df|>
    select(tempcode,placeid,place23nm_mod,lat,long)|>
    distinct()
  
  geo_dat_sf=st_as_sf(geo_dat_input,coords=c("long","lat"),crs=4326,remove=FALSE)
  geo_lookup_result=st_join(geo_dat_sf,gb_boundaries,join=st_intersects)
  
  place_county_reference=geo_lookup_result|>
    st_drop_geometry()|>
    select(tempcode,placeid,
           place23nm_mod,
           lat,long,county_name=name)
  
  known_towns=place_county_reference|>filter(!is.na(county_name)&county_name!="")
  missing_towns=place_county_reference|>filter(is.na(county_name)|county_name=="")
  
  if(nrow(missing_towns)>0){
    known_sf=st_as_sf(known_towns,coords=c("long","lat"),crs=4326)
    missing_sf=st_as_sf(missing_towns,coords=c("long","lat"),crs=4326)
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
      
      processed_elements <- if (position=="prefix"){
        paste0("^",elements)
        
      } else if (position=="suffix") {
        paste0(elements,"$")
        
      } else {
        # elements
        paste0(" ",elements," ")
      }
      tibble(origin=origin,pattern=paste(processed_elements,collapse="|"))
    })})|>
    group_by(origin)|>
    summarise(pattern=paste0("(",pattern,")",collapse="|"),.groups="drop")
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
    stringr::str_extract(filtered_searchable_placenames_df$searchable_placename, stringr::regex(pat, ignore_case = TRUE))
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
post_prep_matched_names=function(
    assignment_result_df=res,
    placenames_matched_with_counties_df=placenames_matched_with_counties){
  
  ## summarise the counts
  intial_sum_df=assignment_result_df|>
    filter(!is.na(comb_origins))|>
    filter(comb_origins!="Unknown")|>
    group_by(searchable_placename)|>
    summarise(comb_origins=paste0(sort(unique(comb_origins)),collapse=","),
              text_matches=paste0(unique(exact_match_text),collapse=","),.groups="drop")
  
  
  ## join place names with origins to main data
  origin_placename_matched=placenames_matched_with_counties_df|>
    left_join(intial_sum_df)|>
    mutate(comb_origins=if_else(is.na(comb_origins),"Unknown",comb_origins))
  
  
  ## filter joined data to reduce duplication
  matched_and_filtered_df=origin_placename_matched|>
    mutate(placename_length=nchar(place23nm_repaired))|>
    filter(!grepl(",",place23nm_repaired))|>
    group_by(placeid)|>
    filter(placename_length==min(placename_length))|>
    ungroup()|>
    group_by(placeid)|>
    arrange(lat,long,.by_group=T)|>
    filter(row_number()==1)|>
    ungroup()|>
    group_by(lat,long)|>
    filter(placename_length==min(placename_length))|>
    ungroup()|>
    group_by(county_name,searchable_placename)|>
    arrange(lat,long,.by_group=T)|>
    filter(row_number()==1)|>
    ungroup()|>
    filter(comb_origins!="Unknown")|>
    select(county_name,comb_origins,place23nm_mod,text_matches)|>
    mutate(rn=row_number())
  
  
  ## explode so there's one row per origin, place and text match
  # takes too long.
  max_rn = nrow(matched_and_filtered_df)
  
  matched_exploded_df = bind_rows(lapply(split(matched_and_filtered_df,matched_and_filtered_df$rn),function(z){
    
    if (z$rn[1] %% 1000 == 0){
      print(paste(z$rn[1],"/",max_rn))
    }
    z_origins = sort(unique(unlist(str_split(z$comb_origins,pattern = ","))))
    crossing(comb_origins = z_origins,
             z |> select(county_name,place23nm_mod,text_matches))
  }))
  
  
  ## summary count the number of each origin in each county, having first weighted the place name by how many origins are assigned to it.
  # this process ensures that the total number of places in that county is constant.
  # and provides a finer-level estimate of the name origin that county.
  # then calculate what percentage of the county names are of each origin (pc column)
  county_summary_df = matched_exploded_df |>
    dplyr::rename(origin = comb_origins,text_match = text_matches) |> 
    group_by(county_name,place23nm_mod,text_match) |> 
    mutate(n_origins = n()) |> 
    ungroup() |> 
    mutate(place_wt = 1/n_origins) |> 
    group_by(county_name, origin, text_match) |> 
    mutate(disp_places = paste0(sort(unique(place23nm_mod)[1:5]),collapse=", ")) |> 
    ungroup() |> 
    mutate(disp_str = paste0(text_match,": ",disp_places))|>
    group_by(county_name,origin) |> 
    summarise(n_places = sum(place_wt,na.rm=T),
              strings_and_matches = paste0(sort(unique(disp_str)),collapse=". "),.groups = 'drop') |> 
    mutate(strings_and_matches = gsub(" [,]",",",strings_and_matches)) |> 
    group_by(county_name) |> 
    mutate(n_county_places = sum(n_places)) |> 
    ungroup() |> 
    mutate(pc = round ( 100 * (n_places / n_county_places), 2))
  
  
  # this is for the map, so that there are still county values of zero even if missing for that origin.
  # then left join the percentages
  # and fill any missing with zeros
  # convert origin shortened names to full names.
  county_plot_df=crossing(
    county_name = sort(unique(county_summary_df$county_name)),
    origin = sort(unique(county_summary_df$origin))) |> 
    left_join(county_summary_df) |> 
    group_by(county_name) |> 
    fill(all_of(c("n_county_places")),.direction='updown') |>
    ungroup() |>
    mutate(across(where(is.numeric), ~ coalesce(., 0))) |>
    ungroup() |> 
    left_join(key_to_languages_table) |> 
    mutate(origin = full_origin) |> select(-full_origin)
  
  outlist = list(
    matched_and_filtered_dat = matched_and_filtered_df,
    county_plot_dat = county_plot_df)
  
  print("completed post_prep_matched_names")
  outlist
} ## eof




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
    palette="YlOrRd",
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
      "<span>", round(row$pc), "% ", ptitle, 
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
        }
      "))
      ),
      do.call(leafsync::sync, c(clean_plots, list(ncol = 3, sync.cursor = FALSE)))
    )
  )
  
} # eof

