# IMPORTS ----
library("tidyverse")
library("leaflet")
library("geojsonio")


# LOAD DATA ----
data <- read.csv("data/StudyAbrd_Data_2226-2234.csv") 


# PULL RELEVANT DATA ----
search_term = 2228
data <- data |> 
  filter(Term == search_term) |> 
  select(ID, Country.Short.Desc)

# GET MAP DATA ----
map_datafile = "data/world_data.json"
map_data <- geojson_read(map_datafile, what="sp")

# INITIALIZE LEAFLET
map <- leaflet(map_data) |> 
  setView(0, 20, 2) |> 
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS-TOKEN')
  )) |> addProviderTiles(providers$Stadia.Outdoors)

# SET UP BINS ----
bins <- ...

# SET UP PALETTE AND LABELS
pal <- colorBin("Reds", domain = ..., bins = bins)


map
