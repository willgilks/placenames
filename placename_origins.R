## Packages ####
library("rvest") ## masks readr guess_encoding() though possibly not an issue
library("tidyverse")
library("sf")
library("rnaturalearth")
library("leaflet")
library("stringi")
# library("htmltools") ## used for rendering multiple leaflet plots. unsure if should be loaded here.

# source custom constants and functions.
source("./constants.R")
source("./functions.R")


# Load reference table with search strips and origins
wiki_table_raw=get_wiki_placenames_lookup(wiki_url = WIKI_URL)

{ 
  write_csv(
    wiki_table_raw,
    file = paste0("./","wiki_table_raw.csv"),progress = T)
  print(paste(nrow(wiki_table_raw),"rows"))
}



# Prep wiki reference data for string-origin matching
wiki_prepped = prepare_wiki_data(
  input_df = wiki_table_raw,
  key_to_languages = KEY_TO_LANGUAGES,
  allowed_string_positions = ALLOWED_STRING_POSITIONS)



{ 
  write_csv(
    wiki_prepped$term_origin_position_dat,
    file = paste0("./","placename_strings_reference.csv"),progress = T)
  print(paste(nrow(wiki_prepped$term_origin_position_dat),"rows"))
}


# Load place names data
placenames_dat_raw=readr::read_csv(paste0(Sys.getenv("DATA_PATH_ROOT"),"/",DATA_PATH), show_col_types = FALSE)


# Basic filter and standardisation
placenames_initial_clean=clean_placenames_data(
  input_df = placenames_dat_raw,
  place_types = PLACE_TYPES,
  lead_remove_strings = LEAD_REMOVE_STRINGS,
  terminal_remove_strings = TERMINAL_REMOVE_STRINGS)


# Assign counties to places, based on source lat,long.
placenames_matched_with_counties=assign_counties_from_sf(
  input_df = placenames_initial_clean,
  gb_boundaries=GB_BOUNDARIES)


## Assign origins to place names
# base on partial string matching
origin_placename_result = match_origin_to_placename(
  origin_to_placename_input_df = placenames_matched_with_counties,
  search_terms_df = wiki_prepped$term_origin_position_dat,
  min_term_length = 2)


## Summarise the assignment of origins to place names
# parts of this function take too long are are generally inefficient.
# outputs a list object.
post_prep_result_list = post_prep_matched_names(
  assignment_result_df=origin_placename_result,
  placenames_matched_with_counties_df=placenames_matched_with_counties)



## Make multiple single-origin leaflet plots.
origins_vec=sort(unique(post_prep_result_list$county_plot_dat$origin))
plist = lapply(origins_vec, function(z){
  z_pdat=post_prep_result_list$county_plot_dat |>
    filter(origin == z) |>
    distinct()
  make_leaflet_map(leaflet_county_data = z_pdat, ptitle = z)
})
plist


# # Make the fecet wrap leaflet plotm
# # where each plot is for a different origin
# leaflet_plots = leaflet_multi_wrapper(
#   input_df = post_prep_result_list$county_plot_dat)