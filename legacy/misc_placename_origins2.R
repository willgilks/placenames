## Packages ####
# library("rvest") ## masks readr guess encoding !
# library("tidyverse")
# library("sf")
# library("rnaturalearth")
# library("leaflet")
# library("htmltools")

source("./constants.R")
source("./functions.R")


WIKI_URL=paste0(
  "https://en.wikipedia.org/wiki/",
  "List_of_elements_in_place_names_in_the_British_Isles")

wiki_temp <- WIKI_URL %>%
  read_html() %>%
  html_nodes("table")

wiki_table_raw = bind_rows(html_table(wiki_temp))


key_to_languages=c(
  "Bry"="Brythonic",
  "C"="Cumbric",
  "K"="Cornish",
  "I"="Irish",
  "L"="Latin",
  "ME"="Middle English",
  "NF"="Norman French",
  "OE"="Old English",
  "ON"="Old Norse",
  "P"="Pictish",
  "S"="Scots",
  "SG"="Scots Gaelic",
  "W"="Welsh")
wiki_table_raw

## completely remove places containing these strings
# BOGEY_STRINGS = c("school")


## PRE-PREP ####

# Load Data
placenames_dat_raw=readr::read_csv(DATA_PATH, show_col_types = FALSE)

# Initial clean-up
placenames_initial_clean=do_initial_cleanup(placenames_dat_raw)



# Assign counties to places
placenames_matched_with_counties=assign_counties_from_sf(placenames_initial_clean)

# Assign origins to place names
origin_placename_matched = match_origin_to_placename(input_df = placenames_matched_with_counties,lookup_list = PLACENAME_ORIGIN_SEARCH_STRINGS)


# filter(
#   !grepl("\\sor\\s|\\s\\&\\s|\\sand\\s|\\swith\\s",place23nm_repaired,ignore.case=TRUE),
#   !grepl("^area\\s|\\sarea\\s|\\sarea$",place23nm_repaired,ignore.case=TRUE),
#   !grepl("\\(|\\/",place23nm_repaired))|>


origin_placename_matched_with_counties_filtered = origin_placename_matched |> 
  # mutate()
  filter(!grepl(",",place23nm_repaired)) |> 
  group_by(placeid) |> 
  filter(placename_length==min(placename_length)) |> 
  ungroup() |> 
  group_by(placeid) |> 
  arrange(lat,long,.by_group = T) |> 
  filter(row_number()==1) |> 
  ungroup() |> 
  group_by(lat,long) |> 
  filter(placename_length==min(placename_length)) |> 
  ungroup() |>
  group_by(county_name,searchable_placename) |> 
  arrange(lat,long,.by_group = T) |> 
  filter(row_number()==1) |> 
  ungroup()

# pdat=origin_placename_matched_with_counties_filtered |> 
#   # filter(comb_origins=='Unknown') |> 
#   # filter(grepl(",",comb_origins)) |> 
#   ungroup()
# 
# ggplot(pdat,aes(long,lat))+
#   geom_text(size = 2.5, aes(label = place23nm_repaired,colour = comb_origins),
#             alpha = 1,show.legend = F)+
#   # geom_point(size = .5, aes(colour = comb_origins),alpha = .5)+
#   facet_wrap(comb_origins~.,scales='free',ncol=5)+
#   theme()


## USER PREP ####
## options to filter place names to prevent duplication
origins_per_county_prep = origin_placename_matched_with_counties_filtered |>
  filter(comb_origins!="Unknown") |> 
  mutate(comb_origins = if_else(grepl(",",comb_origins),"Mixed",comb_origins)) |> 
  group_by(county_name,comb_origins) |> 
  summarise(n_places = n(),place_23nm_vals2 = paste0(sort(place23nm_repaired),collapse = ";"),.groups='drop') |> 
  group_by(county_name) |> 
  mutate(county_places_n = sum(n_places)) |> 
  ungroup() |> 
  mutate(county_places_pc = 100*(n_places / county_places_n)) |> 
  group_by(comb_origins) |>
  mutate(origin_n = sum(n_places)) |>
  ungroup()

origins_per_county_dat=crossing(
  county_name = sort(unique(origins_per_county_prep$county_name)),
  comb_origins = sort(unique(origins_per_county_prep$comb_origins))) |> 
  left_join(origins_per_county_prep) |> 
  group_by(county_name) |> 
  fill(all_of(c("county_places_n")),.direction='updown') |>
  ungroup() |> 
  group_by(comb_origins) |> 
  fill(all_of(c("origin_n")),.direction='updown') |>
  ungroup() |> 
  mutate(across(where(is.numeric), ~ coalesce(., 0))) |>
  ungroup()

origins_vec=sort(unique(origins_per_county_dat$comb_origins))

plist = lapply(origins_vec, function(z){
  z_pdat=origins_per_county_dat |>
    filter(comb_origins == z) |>
    # mutate(my_value = overall_pc) |>
    mutate(my_value = county_places_pc) |>
    distinct()
  make_leaflet_map(leaflet_county_data = z_pdat, ptitle = z)
})
plist[[1]]


{
  leaflet_plots = leaflet_multi_wrapper(
    input_df = origins_per_county_dat,
    plot_column = "county_places_pc")
  
  # Remove slot names to ensure leafsync reads the items purely as an indexed list
  clean_plots <- unname(leaflet_plots)
  clean_plots
  
  
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
}


## Corwall, 'Bude-Stratton' is assigned Saxon ??
## Also in searchind, 'West Town' comes up as saxon. But these aren't true prefixes/suffixes
## Also where is 'dean' or 'deen'

# plist = lapply(
#   split(origins_per_county_dat,origins_per_county_dat$comb_origins),
#   function(z){
#     
#     z_title = z$comb_origins
#     z_pdat = z |> 
#       mutate(
#         county_name = fct_reorder(county_name,county_places_pc))
#     
#     ggplot(z_pdat,aes(county_places_pc,county_name))+
#       geom_col()+
#       coord_cartesian(expand = 0)+
#       labs(x="",y="",title = z_title)
#   })
# cowplot::plot_grid(
#   plotlist = plist,ncol = 3)



## PCA ####
pca_prep = origins_per_county_dat |> 
  select(county_name,comb_origins,county_places_pc) |> 
  spread(comb_origins,county_places_pc) |> 
  mutate(across(where(is.numeric), ~ coalesce(., 0))) |> 
  arrange(county_name)

# Geographic deduplication (takes time)
# placenames_dat_filtered=deduplicate_spatial_clusters(placenames_initial_clean,distance_threshold_meters =  1e4)


# Extract numeric matrix for analysis (ignoring county names)
data_matrix <- pca_prep %>%
  select(-county_name) %>%
  scale() 

# Set row names to county names so they carry over to your plots/results
rownames(data_matrix) <- pca_prep$county_name


# Principal Component Analysis

pca_result <- prcomp(data_matrix, scale. = FALSE)

# Extract PC scores to use for visualization or clustering
pca_scores <- as.data.frame(pca_result$x) %>%
  mutate(county_name = rownames(.))

# View variance explained by each PC
summary(pca_result)


# K-Means Clustering

set.seed(42) # For reproducibility

# Choose your number of clusters (e.g., k = 3)
k_cores <- 6
kmeans_result <- kmeans(data_matrix, centers = k_cores, nstart = 25)

# Add cluster assignments back to your original dataset
final_df <- pca_prep %>%
  mutate(cluster = as.factor(kmeans_result$cluster))



# Merge cluster IDs with PCA scores
plot_data <- pca_scores %>%
  left_join(final_df %>% select(county_name, cluster), by = "county_name")

ggplot(plot_data, aes(x = PC1, y = PC2, color = cluster, label = county_name)) +
  # geom_point(size = 3) +
  geom_text(size = 3) +
  labs(
    title = "County Clusters Visualized via PCA",
    x = paste0("PC1 (", round(100 * pca_result$sdev[1]^2 / sum(pca_result$sdev^2), 1), "%)"),
    y = paste0("PC2 (", round(100 * pca_result$sdev[2]^2 / sum(pca_result$sdev^2), 1), "%)")
  ) +
  theme_minimal()


# Text normalization
# filters to match root short-names to prevent multiple county occurrences
# placenames_prepped=secondary_prep_placenames(placenames_dat_filtered, place_county_reference)


# Scan place names for strings indicating origin.

user_input_string = 'ford'

placenames_with_origins=match_origin_to_placename(placenames_prepped, PLACENAME_ORIGIN_SEARCH_STRINGS) ## this works downstream

# Make county-level results
placenames_fin_sum_exploded=make_county_level_results(
  input_df = placenames_with_origins, 
  multi_origin_method = "mixed",
  convert_to_single_pat_mode = 'yes')


# Make plots ####
# sort(unique(placenames_fin_sum_exploded$origin))

target_origin <- user_input_string

pig <- placenames_fin_sum_exploded |>
  filter(origin == target_origin) |>
  # mutate(my_value = overall_pc) |>
  mutate(my_value = origin_pc) |>
  distinct()


make_leaflet_map(leaflet_county_data = pig, ptitle = target_origin)
