
# local path where place names data are stored
DATA_PATH="~/Downloads/IPN_GB_2024/IPN_GB_2024.csv"

# url where the reference data for origins and strings are stored
WIKI_URL=paste0(
  "https://en.wikipedia.org/wiki/",
  "List_of_elements_in_place_names_in_the_British_Isles")

# lookup for matching abbreviated origin name with full origin name
KEY_TO_LANGUAGES=c(
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


# used when prepairing data for the leaflet map, and general output formatting
key_to_languages_table = stack(KEY_TO_LANGUAGES) |> 
  dplyr::rename(origin = ind, full_origin = values)


# place types to keep from the place names source data
PLACE_TYPES=c("BUA","LOC","LONB", "MD", "PAR")

# Geometric Base
# for assigning county bondaries, possibly using in the leaflet map ??
GB_BOUNDARIES <- st_transform(
  ne_states(country = "united kingdom", returnclass = "sf") |> 
    filter(region != "Northern Ireland") |>
    ungroup(), crs = 4326) |> 
  st_simplify(preserveTopology = TRUE, dTolerance = 0.001)
