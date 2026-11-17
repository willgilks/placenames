
# local path where place names data are stored
DATA_PATH="IPN_GB_2024/IPN_GB_2024.csv"

# url where the reference data for origins and strings are stored
WIKI_URL=as.character("https://en.wikipedia.org/wiki/List_of_elements_in_place_names_in_the_British_Isles")

ALLOWED_STRING_POSITIONS=c("interfix","prefix","standalone","suffix")

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



LEAD_REMOVE_STRINGS <- c(
  "bank", "castle", "chapel", "east", "easter", "ferry",
  "garrison", "garden", "gardeners", "great", "gate",
  "high", "higher", "low", "lower", "little",
  "marsh", "middle", "new", "north", "old", "port",
  "south", 
  "temple","the", "upper", "west", "windmill")


TERMINAL_REMOVE_STRINGS <- c(
  "bank", "camp", "castle", "city", "colliery", "common", "corner", "cottages", "creek", "crossing",
  "east", "easter", "end", "edge", "estate",
  "docks", "down", "drive",
  "farm", "ferry", "field", "fields", "foot", "forest",
  "garden", "gardens", "garrison", "gate", "granch", "grange", "green", "grove",
  "heath","house", "junction", "lane", "lodge", "lower",
  "marsh", "mead", "meadow", "meadows", "mill", "mills", "moor", "old",
  "park", "parks", "passage", "plain", "place", "port", "priors",
  "quay", "rd", "side", "south", "st", "street",
  "vale", "valley", "view", "village")

