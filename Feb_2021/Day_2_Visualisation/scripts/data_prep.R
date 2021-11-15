# This is data prep for the course - runs locally, but output is saved for course material

# =============================================================
library(tidyverse)
library(sf)
setwd("C:/Users/langt/Documents/OneDrive - MMU/Lenovo/Teaching/Teaching/UofM/UKDataService/data/original")
# =============================================================


# =============================================================
# Greater Manchester crime data for Jan-Dec 2017, downloaded from data.police.uk
# Load and merge crime data, subset for burglary, aggregate to LSOA

dfs <- list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))

full.crime.raw <- bind_rows(dfs)

full.crime <- full.crime.raw %>% 
  mutate(crime_id = 1:nrow(full.crime.raw)) %>% 
  select(crime_id, Month, `LSOA code`, `LSOA name`, `Crime type`) %>% 
  rename(LSOAcode = `LSOA code`,
         LSOAname = `LSOA name`,
         crime_type = `Crime type`) %>% 
  drop_na()

lsoa.crime <- full.crime %>%
  filter(crime_type == "Burglary") %>% 
  group_by(LSOAcode) %>% 
  tally(name = "burglary_count")

# Load in latest IMD data
imd <- read_csv("imd/eng_imd19_lsoa.csv")

# subset
imd.sub <- imd %>% 
  select(`LSOA code (2011)`,
         `Local Authority District name (2019)`,
         `Index of Multiple Deprivation (IMD) Score`,
         `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
         `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`,
         `Income Score (rate)`) %>% 
  rename(LSOAcode = `LSOA code (2011)`,
         LAname   = `Local Authority District name (2019)`,
         IMDscore = `Index of Multiple Deprivation (IMD) Score`,
         IMDrank  = `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
         IMDdeci  = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`,
         incscore = `Income Score (rate)`)


# Merge with GM crime data, so removing all others
lsoa.crime.imd <- left_join(lsoa.crime, imd.sub)

# Keep only GM
gm.names <- c("Bolton", "Bury", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford",
              "Oldham", "Wigan", "Manchester")

lsoa.crime.imd <- lsoa.crime.imd %>% 
  filter(LAname %in% gm.names)

# =============================================================
# Visuals
# =============================================================

# scatter
ggplot(data = lsoa.crime.imd, 
       mapping = aes(x = incscore, y = burglary_count, colour = LAname)) +
  geom_point()

# scatter with trimmings
ggplot(data = lsoa.crime.imd, 
       mapping = aes(x = incscore, y = burglary_count, colour = LAname)) +
  geom_point(size = 3, alpha = 0.5) +
  scale_color_brewer(palette = "Spectral") +
  theme_bw() +
  labs(title = "Relationship between burglary victimisation and income",
       y = "count", x = "IMD income score", colour = NULL,
       caption = "IMD data from 2019, police recorded crime data covers 2017")

# histogram basic
ggplot(data = lsoa.crime.imd) +
  geom_histogram(mapping = aes(x = burglary_count), bins = 60)

# bar basic
ggplot(data = lsoa.crime.imd) +
  geom_bar(mapping = aes(x = as.factor(IMDdeci), fill = as.factor(IMDdeci))) 

# density basic
ggplot(data = lsoa.crime.imd) +
  geom_density(mapping = aes(x = IMDscore)) +
  facet_wrap(~LAname)

# add population for r textbook ==============================

pop.df <- read_csv("C:/Users/langt/Documents/OneDrive - MMU/Lenovo/PhD_project/PhD_project/data/pop_estimates/all_lsoa_pop_est.csv")

lsoa.crime.imd <- left_join(lsoa.crime.imd, pop.df)

# write_csv(x = lsoa.crime.imd, path = "C:/Users/langt/Documents/GitHub/r_textbook/Datasets/gmp_2017.csv")
# 
# for QGIS mapping
lsoa.crime.imd.qgis <- lsoa.crime.imd %>% 
  select(-burglary_count)

#write_csv(x = lsoa.crime.imd, path = "C:/Users/langt/Documents/GitHub/qgis_intro/data/gm_demographics.csv")

england.lsoa.sf <- st_read("C:/Users/langt/Documents/OneDrive - MMU/Lenovo/PhD_project/PhD_project/data/uk_shapefiles/england_lsoa_2011_clipped/england_lsoa_2011_clipped.shp")

list.lsoa <- lsoa.crime.imd.qgis$LSOAcode

gm.lsoa.sf <- england.lsoa.sf %>% 
  filter(code %in% list.lsoa) %>% 
  rename(LSOAcode = code)

gm.lsoa.sf <- left_join(gm.lsoa.sf, lsoa.crime.imd.qgis)

st_write(obj = gm.lsoa.sf, dsn = "C:/Users/langt/Documents/GitHub/qgis_intro/data/gm_lsoa.shp",
         driver = "ESRI Shapefile")

# ============================================================


# =============================================================
# Creating a longitudinal example which uses group aesthetic 

month.crime <- full.crime %>% 
  group_by(Month, crime_type) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(n > 1000,
         crime_type != "Shoplifting",
         crime_type != "Other theft") %>% 
  mutate(Month = as.factor(recode(Month,
    `2017-01` = "1",
    `2017-02` = "2",
    `2017-03` = "3",
    `2017-04` = "4",
    `2017-05` = "5",
    `2017-06` = "6",
    `2017-07` = "7",
    `2017-08` = "8",
    `2017-09` = "9",
    `2017-10` = "10",
    `2017-11` = "11",
    `2017-12` = "12")
  ))

table(month.crime$crime_type)

month.crime$Month <- factor(month.crime$Month, levels = c("1",
                                               "2",
                                               "3",
                                               "4",
                                               "5",
                                               "6",
                                               "7",
                                               "8",
                                               "9",
                                               "10",
                                               "11",
                                               "12"))
# Line graph
ggplot(data = month.crime, aes(x = Month, y = n,
                               colour = crime_type,
                               group = crime_type)) +
  geom_line()
# =============================================================

# =============================================================
# Clean the data and make spatial for the afternoon maps

lsoa.la.names <- lsoa.crime.imd %>% 
  select(LSOAcode, LAname)

full.crime.df <- full.crime.raw %>% 
  filter(Month == "2017-01",
         `Crime type` == "Burglary") %>% 
  mutate(crime_id = 1:nrow(.)) %>% 
  select(crime_id, Month, `LSOA code`, `Crime type`, `Last outcome category`, Longitude, Latitude)  %>% 
  rename(LSOAcode = `LSOA code`,
         crime_type = `Crime type`,
         outcome = `Last outcome category`) %>% 
  left_join(lsoa.la.names) %>% 
  filter(LAname == "Manchester")

  # st_as_sf(coords = c(x = "Longitude", y = "Latitude"), crs = 4326)

ggplot(data = full.crime.sf) +
  geom_sf(mapping = aes(fill = outcome), pch = 21)

# Raw LSOA
manc.sf <- st_read("C:/Users/langt/Documents/GitHub/data_viz_R_workshop/data/england_lsoa_2011.shp")
manc.sf <- st_transform(manc.sf, 4326)

# Give them plot of two sf layers
ggplot() +
  geom_sf(data = manc.sf) +
  geom_sf(data = full.crime.sf, mapping = aes(fill = outcome), pch = 21)

# But the LSOA data they load in should have something to visualise area-based, so aggregate these
# point for them to use.

full.crime.agg.df <- full.crime.sf %>% 
  st_set_geometry(NULL) %>% 
  group_by(LSOAcode) %>% 
  tally(name = "burglary_counts") 

imd.only.df <- imd.sub %>% 
  select(LSOAcode, IMDscore, IMDrank, IMDdeci, incscore)

manc.crime.sf <- manc.sf %>% 
  rename(LSOAcode = code) %>% 
  left_join(full.crime.agg.df) %>% 
  replace_na(list(burglary_counts = 0)) %>% 
  select(-label) %>% 
  left_join(imd.only.df) %>% 
  st_transform(27700)

# area-based plot
ggplot(data = manc.crime.sf) +
  geom_sf(mapping = aes(fill = burglary_counts))

# =============================================================
# Save data 

# LSOA IMD df
write_csv(x = lsoa.crime.imd, path = "C:/Users/langt/Documents/GitHub/data_viz_R_workshop/data/gmp_2017.csv")

# Monthly data
write_csv(x = month.crime, path = "C:/Users/langt/Documents/GitHub/data_viz_R_workshop/data/gmp_monthly_2017.csv")

# point df data
write_csv(x = full.crime.df, path = "C:/Users/langt/Documents/GitHub/data_viz_R_workshop/data/burglary_records.csv")

# Point sf data
st_write(obj = full.crime.sf, dsn = "C:/Users/langt/Documents/GitHub/data_viz_R_workshop/data/burglary_incidents.shp")

# LSOA sf data
st_write(obj = manc.crime.sf, dsn = "C:/Users/langt/Documents/GitHub/data_viz_R_workshop/data/burglary_lsoa.shp")
