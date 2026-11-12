## Packages ####
library("rvest") ## masks readr guess_encoding() though possibly not an issue
library("tidyverse")
library("sf")
library("rnaturalearth")
library("leaflet")
# library("htmltools") ## used for rendering multiple leaflet plots. unsure if should be loaded here.

# source custon constants and functions.
source("./constants.R")
source("./functions.R")


# Load reference table with search strips and origins
wiki_table_raw=get_wiki_placenames_lookup(wiki_url = WIKI_URL)

# Load place names data
placenames_dat_raw=readr::read_csv(DATA_PATH, show_col_types = FALSE)


# Prep wiki reference data for string-origin matching
wiki_prepped = prepare_wiki_data(input_df = wiki_table_raw)


# Initial clean-up of place names data
# Remove troublesome places like parks and castles and shops
# Standardise character values.
placenames_initial_clean=do_initial_cleanup(placenames_dat_raw)

# Assign counties to places, based on reported lat,long values.
placenames_matched_with_counties=assign_counties_from_sf(placenames_initial_clean)


# Filter place names data so that only those with any pattern matches are considered.
# This is in order to reduce down the search space.
filtered_searchable_placenames = placenames_matched_with_counties |> 
  select(searchable_placename)|> 
  filter(grepl(wiki_prepped$all_terms_concat,searchable_placename))  |> 
  distinct() |> 
  arrange(searchable_placename)



## Assign origins to place names
# base on partial string matching
origin_placename_result = match_origin_to_placename(
  input_df = filtered_searchable_placenames, 
  lookup_list = wiki_prepped$new_search_terms_list)


## Summarise the assignment of origins to place names
# parts of this function take too long are are generally inefficient.
# outputs a list object.
post_prep_result_list = post_prep_matched_names(
  assignment_result_df=origin_placename_result,
  placenames_matched_with_counties_df=placenames_matched_with_counties)


## Make multiple single-origin leaflet plots.
origins_vec=sort(unique(post_prep_result_list$county_plot_dat$origin))
# here the text in the hover box all needs to be bigger because it's a single plot.
plist = lapply(origins_vec, function(z){
  z_pdat=post_prep_result_list$county_plot_dat |>
    filter(origin == z) |>
    distinct()
  make_leaflet_map(leaflet_county_data = z_pdat, ptitle = z)
})
# plist


# Make the fecet wrap leaflet plotm
# where each plot is for a different origin
leaflet_plots = leaflet_multi_wrapper(
  input_df = post_prep_result_list$county_plot_dat)