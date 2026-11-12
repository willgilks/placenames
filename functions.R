
#  FUNCTIONS ####

# pulls string and origin info from wiki url
get_wiki_placenames_lookup=function(wiki_url=WIKI_URL){
  loaded_wiki<-wiki_url%>%
    read_html()%>%
    html_nodes("table")
  output=bind_rows(html_table(loaded_wiki))
  output
}



# prepares wiki string and origin info for use.
# from character columns, standardises the text strings.
prepare_wiki_data = function(input_df = wiki_table_raw){
  
  input_df_copy = input_df
  
  # standardise column contents
  clean_df=input_df_copy|>
    rename_with(tolower)|>
    mutate(origin2=origin,term2=term,example2=example,position2=position)|>
    select(origin2,example2,term2,position2)|>
    mutate(across(all_of(contains("2")),~gsub("\\s*\\[[^]]*\\]","",.x)))|>
    mutate(across(all_of(contains("2")),~gsub("\\s*\\([^)]*\\)","",.x)))|>
    mutate(origin2 = gsub("[/]",",",origin2)) |> 
    mutate(origin2 = gsub(", ",",",origin2)) |> 
    mutate(origin2 = gsub("\\s.*","",origin2)) |> 
    mutate(term2 = tolower(term2)) |> 
    mutate(term2 = gsub("-","",term2)) |> 
    mutate(term2 = gsub("[/]","",term2)) |> 
    mutate(term2 = gsub(", ",",",term2)) |> 
    mutate(position2 = gsub(", ", ",", position2)) |>
    mutate(position2 = gsub(",$","",position2)) |>
    mutate(position2 = gsub(" meaning|,occasionally| but .*","",position2)) |>
    mutate(position2 = if_else(term2 == "law,low","prefix,suffix",position2)) |>
    mutate(position2 = gsub(" or ", ",", position2)) |>
    mutate(position2 = gsub(" .*","",position2)) |>
    mutate(position2 = gsub("\\\n","",position2)) |>
    mutate(position2 = if_else(position2 %in% c("suffix prefix","suffix prefix"),"prefix,suffix",position2)) |>
    mutate(position2=if_else(position2=="standalone,suffix","suffix",position2))|>
    filter(origin2!="")|>
    filter(!grepl("possibly",origin2,ignore.case=T))|>
    mutate(across(where(is.character),~trimws(.x)))|>
    select(origin2,example2,term2,position2)|>
    mutate(position2=if_else(term2=="upon,on,in","interfix",position2))
  
  
  # make long-format so there is one place name per row.
  long_df = bind_rows(lapply(split(clean_df,clean_df$example2), function(z){
    z_origin = z$origin2[1]
    z_term = z$term2[1]
    z_position = z$position2[1]
    z_out = tibble(place = unlist(strsplit(z$example2[1],", "))) |> 
      mutate(origin = z_origin,term = z_term,position = z_position)
    z_out
  }))
  
  
  # standardise the order of multi-origin terms. (e.g. W,C is set to C,W to prevent ambiguity)
  origin_repaired_df = bind_rows(
    lapply(
      split(long_df,long_df$origin), function(z){
        z_new_origin = paste0( sort(unique( unlist(strsplit(x = z$origin[1],split = ",")))),collapse = ",")
        zout = z |> mutate(origin = z_new_origin)
        zout
      }))
  
  
  # standardise terms so that they're in alphabetical order
  # and also remove some ambiguous terms
  term_repaired_df = bind_rows(
    lapply(
      split(origin_repaired_df,origin_repaired_df$term), function(z){
        unfiltered_terms = sort(unique( unlist(strsplit(x = z$term[1],split = ","))))
        # filtered_terms = unfiltered_terms[!unfiltered_terms %in% c("a", "at","y")]
        filtered_terms = unfiltered_terms
        z_new_term = paste0( filtered_terms,collapse = ",")
        zout = z |> 
          mutate(term = z_new_term)
      }))
  
  
  # make distinct. set missing positions to prefix and suffix
  # important due to text inconsistencies
  repaired_distinct_df = term_repaired_df |> 
    distinct() |> 
    mutate(position = if_else(position =="","prefix,suffix",position)) |> 
    ungroup()
  
  
  
  ## Get the source-defined place name origins.
  ## This serves as origin as assigned by experts, for later, independent of the main algorithm.
  source_placename_origin_lookup_df=repaired_distinct_df|>
    mutate(searchable_placename=tolower(place))|>
    select(searchable_placename,origin)|>
    distinct()|>
    group_by(searchable_placename)|>
    summarise(source_origins=paste0(sort(unique(origin)),collapse=";"),.groups='drop')
  
  
  
  # get term, origin and position columns.
  term_origin_lookup_df = repaired_distinct_df |> 
    select(term,origin,position) |> 
    distinct() |> 
    mutate(rn=row_number())
  
  # expand position into columns, then make long
  max_positions = 1+max(str_count(term_origin_lookup_df$position,","))
  
  term_origin_exploded = term_origin_lookup_df |> 
    separate(position,into = paste0("p",1:max_positions),sep = ",",remove = F) |>
    select(-rn) |> 
    gather(plevel,pgrp,-c(term,origin,position)) |> 
    filter(!is.na(pgrp)) |> 
    select(-plevel,-position) |> 
    dplyr::rename(position = pgrp) |> 
    mutate(rn=row_number())
  
  
  # simple vector of all possible search terms
  # used for pre-filter to remove place names with no string matches at all
  all_terms = sort(unique(trimws(unlist(lapply(split(term_origin_exploded,term_origin_exploded$rn),function(z){
    unlist(str_split(z$term[1],pattern = ","))
  })))))
  
  all_terms_concat_str = paste0(all_terms,collapse="|")
  
  
  
  ## explode the concatentated search terms, so there's one per row
  term_origin_fin_df = bind_rows(
    lapply(split(term_origin_exploded,term_origin_exploded$rn), function(z){
      new_terms = sort(unique(unlist(str_split(z$term[1],pattern = ","))))
      tibble(term = new_terms) |>
        mutate(origin = z$origin[1],position=z$position[1]) |>
        mutate(term = if_else(position=="interfix",paste0(" ",term," "),term)) |>
        mutate(nchar_term=nchar(term))
    })) 
  
  
  # make search terms list structure
  new_search_terms <- term_origin_fin_df |> 
    select(position,origin,term) %>% 
    split(.$position) %>% 
    map(~ split(.x$term, .x$origin))
  
  outlist = list(
    source_placename_origin_lookup = source_placename_origin_lookup_df,
    all_terms_concat = all_terms_concat_str,
    term_origin_fin = term_origin_fin_df,
    new_search_terms_list=new_search_terms)
  outlist
} # eof prep wiki








do_initial_cleanup=function(input_df,place_types=PLACE_TYPES){
  
  # these are used with a trailing white space in order to remove full word only.
  lead_remove_strings = c(
    "castle","chapel","east","easter","ferry",
    "garrison","garden","gardeners","great","gate",
    "high","higher",
    "low","lower","little",
    "marsh","middle",
    "new","north","old","port",
    "south","the","upper","west","windmill")
  
  lead_remove_strings_concat = gsub("[|]$","",paste0("^",lead_remove_strings," |",collapse = ""));
  
  
  terminal_remove_strings = c(
    "camp","castle","city","colliery","common","corner","cottages","creek","crossing",
    "east","easter","end","edge","estate",
    "docks","down","drive",
    "farm","ferry","field","fields","foot","forest",
    "garden","gardens","garrison","gate","granch","grange","green","grove",
    "junction","lane","lodge","lower",
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
    filter(!(cty61nm=="Cornwall and Isles of Scilly"&descnm=="LOC"))|> 
    filter(lat!=0,long!=0,lat>49,long>(-8))|>
    filter(!grepl(" church$| close$| closes$|  copse$| land$| lands$| park$| parks$| shop$| square$",place23nm_repaired)) |> 
    mutate(place23nm_repaired_length=nchar(place23nm_repaired)) |> 
    # remove text in bracks and after by,in.
    mutate(place23nm_mod=place23nm_repaired) |> 
    mutate(place23nm_mod=gsub("\\s*\\([^)]*\\)","",place23nm_mod)) |> 
    # mutate(place23nm_mod = gsub("\\s+\\b(by|in|and|upon)\\b.*", "", place23nm_mod)) |> 
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
}



# Assign counties to locations based on lat/long and reference map
assign_counties_from_sf=function(
    input_df = placenames_initial_clean,
    gb_boundaries=GB_BOUNDARIES){
  
  geo_dat_input=input_df|>
    select(tempcode,placeid,place23nm_mod,
           # place23nm_repaired,
           lat,long)|>
    distinct()
  
  geo_dat_sf=st_as_sf(geo_dat_input,coords=c("long","lat"),crs=4326,remove=FALSE)
  geo_lookup_result=st_join(geo_dat_sf,gb_boundaries,join=st_intersects)
  
  place_county_reference=geo_lookup_result|>
    st_drop_geometry()|>
    select(tempcode,placeid,
           place23nm_mod,
           # place23nm_repaired,
           lat,long,county_name=name)
  
  known_towns=place_county_reference|>filter(!is.na(county_name)&county_name!="")
  missing_towns=place_county_reference|>filter(is.na(county_name)|county_name=="")
  
  if(nrow(missing_towns)>0){
    known_sf=st_as_sf(known_towns,coords=c("long","lat"),crs=4326)
    missing_sf=st_as_sf(missing_towns,coords=c("long","lat"),crs=4326)
    nearest_index=st_nearest_feature(missing_sf,known_sf)
    missing_towns$county_name=known_towns$county_name[nearest_index]
  }
  
  input_df |> 
    left_join(
      bind_rows(known_towns,missing_towns))
}






# regex processor supporting custom user strings
make_placename_pattern_lookup=function(lookup_list=NULL){
  imap_dfr(lookup_list,function(origin_list,position){
    imap_dfr(origin_list,function(elements,origin){
      processed_elements<-if(position=="prefix"){paste0("^",elements)} else if(position=="suffix") {
        paste0(elements,"$")
      } else {
        elements
      }
      tibble(origin=origin,pattern=paste(processed_elements,collapse="|"))
    })})|>
    group_by(origin)|>
    summarise(pattern=paste0("(",pattern,")",collapse="|"),.groups="drop")
}





## function, match origin to place name ##
match_origin_to_placename <- function(
    input_df = filtered_searchable_placenames, 
    lookup_list = wiki_prepped$new_search_terms_list) {
  
  compiled_lookup <- make_placename_pattern_lookup(lookup_list=lookup_list)
  
  # Extract the exact matching substrings from the place names for each pattern
  # Returns a list of character vectors (one per pattern, length = nrow(input_df))
  match_matrix <- map(compiled_lookup$pattern, \(pat) {
    stringr::str_extract(input_df$searchable_placename, stringr::regex(pat, ignore_case = TRUE))
  })
  
  # Collapse the matches for each row into comma-separated strings
  match_results <- map_dfr(1:nrow(input_df), \(row_idx) {
    
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
  output_df = input_df |>
    mutate(
      comb_origins = match_results$comb_origins,
      exact_match_text = match_results$exact_match_text)
  
  output_df
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
  matched_exploded_df = bind_rows(lapply(split(matched_and_filtered_df,matched_and_filtered_df$rn),function(z){
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
    matched_exploded_dat = matched_exploded_df,
    county_summary_dat = county_summary_df,
    county_plot_dat = county_plot_df)
  
  outlist
} ## eof




# make leaflet county map ##
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
  
  # Clean up titles for presentation layout
  # display_title<-case_when(ptitle=="county_pc_known"~"County % known",TRUE~ptitle)
  # display_title<-case_when(ptitle=="county_pc_known"~"County % known",TRUE~ptitle)
  
  # print(display_title)
  
  # Bind to shape file
  leaflet_map_data_input=gb_boundaries|>
    left_join(leaflet_county_data,by=c("name"="county_name"))|>
    mutate(pc=replace_na(pc,0))
  
  # transform CRS
  leaflet_map_data_tf=st_transform(leaflet_map_data_input,crs=4326)
  
  #Set colour scale ranges
  domain_vals=leaflet_map_data_tf$pc
  max_domain_vals=max(domain_vals,na.rm=TRUE)
  min_domain_vals=min(domain_vals,na.rm=TRUE)
  
  if(is.infinite(max_domain_vals)||is.na(max_domain_vals)){
    max_domain_vals<-100
    min_domain_vals<-0
  }
  
  if (fix_percent_scale){
    max_upper<-max(100,max_domain_vals)
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
  hover_labels=lapply(1:nrow(leaflet_map_data_tf),function(i) {
    
    row<-leaflet_map_data_tf[i,]
    
    county_name_text=paste0("<span>",row$name,"</span><br>")
    
    origin_and_percent_text = paste0(
      "<span>", round(row$pc), "% ", ptitle, 
      " (", round(row$n_places), "/", row$n_county_places, ")</span>"
    )
    
    example_place_names_text <- as.character("")
    
    # origin_and_percent_text = paste(
    #   "<span>", paste0(round(row$pc), "%", ptitle,
    #                    paste0("(", round(row$n_places), "/", row$n_county_places, ")", "</span>")
                       
                       
    # origin_and_percent_text=paste(
      # "<span>",paste0(round(row$pc),"%", ptitle,
#                       
                      # paste0("(",round(row$n_places),"/",row$n_county_places,")", "</span>")
      
      # example_place_names_text<-as.character("")
      
      if (!is.na(row$strings_and_matches[1])) {
        
        hover_subtext = gsub("[.]","<br>",row$strings_and_matches[1])
        example_place_names_text <- paste0("<br><span style='font-size:8px;'>",hover_subtext,"</span>")
      }
      
      htmltools::HTML(paste0(county_name_text, origin_and_percent_text, example_place_names_text))
      })
    
    # Make leaflet map proper
    leaflet(data=leaflet_map_data_tf,
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



# leaflet multi-plot wrapper function
leaflet_multi_wrapper = function(
    # input_df = placenames_fin_sum_exploded,
  # plot_column = "pc"){
  input_df = post_prep_result_list$county_plot_dat
){
  
  # if (plot_column %in% c("n_places", "pc")){
  
  # input_df_filtered = input_df %>%
  # { if ( plot_column == "pc") filter(., origin != "Unknown") else . } %>%
  # ungroup()
  
  origin_names = sort(unique(
    # input_df_filtered$origin
    input_df$origin
  ))
  
  output_plot = lapply(origin_names, function(z){
    
    df = input_df |>
      filter(origin == z)|>
      select(
        # county_name, origin, my_value := {{plot_column}},
        county_name, origin, pc,
        # place_23nm_vals2,
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
  
  # } else {
  #   
  #   df = input_df |>
  #     select(county_name, my_value := {{plot_column}}) |>
  #     filter(is.finite(my_value)) |>
  #     distinct() |>
  #     as.data.frame()
  #   
  #   is_pc = ifelse(grepl("pc$", plot_column), TRUE, FALSE)
  #   
  #   output_plot = make_leaflet_map(
  #     leaflet_county_data = df,
  #     ptitle = paste(as.character({{plot_column}})),
  #     gb_boundaries = GB_BOUNDARIES,
  #     is_percentage = is_pc,
  #     fix_percent_scale = FALSE,
  #     zoom_restrict = TRUE,
  #     starting_zoom = 5)
  # }
  
  # return(output_plot)
  
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
  
}

