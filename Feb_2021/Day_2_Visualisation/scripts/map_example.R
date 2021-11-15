library(readr)
library(sf)
library(ggplot2)

burglary_df <- read_csv("data/burglary_records.csv")

burglary_sf <- st_as_sf(x = burglary_df, coords = c(x = "Longitude", y = "Latitude"), crs = 4326)

ggplot(data = burglary_sf) +
  geom_sf() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

burglary_bng_sf <- burglary_sf %>% 
  st_transform(crs = 27700)

ggplot(data = burglary_bng_sf) +
  geom_sf() +
  coord_sf(datum = st_crs(27700))
