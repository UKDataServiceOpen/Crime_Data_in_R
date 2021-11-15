# Load packages.
library(kml)
library(cowplot)
library(lorenzgini)
library(spdep)
library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)
library(ggplot2)
library(sf)

# Useful function.
`%nin%` <- Negate(`%in%`)

# Archive data downloaded January 2012, covering the 3-year period up to and including November 2020.
# download.file(url = "https://data.police.uk/data/archive/2020-11.zip", destfile = "data/archive2020-11.zip")

# Unzip.
# unzip(zipfile = "data/archive2020-11.zip", exdir = "data")

# List all those 'street' files (rather than outcomes or stop and search).
list_2020 <- paste("data/", list.files("data", pattern = glob2rx("2020*street.csv"),  recursive=TRUE), sep = "")
list_2019 <- paste("data/", list.files("data", pattern = glob2rx("2019*street.csv"),  recursive=TRUE), sep = "")

# Read in .csv files for each year.
data_2020 <- lapply(list_2020, read_csv)
data_2019 <- lapply(list_2019, read_csv)

# Bind each in to data frames. For now, we keep the years separate.
full_data_2020 <- data_2020 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2020")

full_data_2019 <- data_2019 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2019")

# Create year ID, then remove Greater Manchester Police and Police Service of NI.
sub_data_2020 <- full_data_2020 %>% 
  filter(reported_by != "Greater Manchester Police", 
         reported_by != "Police Service of Northern Ireland")

sub_data_2019 <- full_data_2019 %>% 
  filter(reported_by != "Greater Manchester Police", 
         reported_by != "Police Service of Northern Ireland")

# Remove existing data objects to free up memory if needed.
rm(data_2019, data_2020, full_data_2019, full_data_2020)

# Check months.
unique(sub_data_2020$month)
unique(sub_data_2019$month)

# Check crime types.
unique(sub_data_2020$crime_type)
unique(sub_data_2019$crime_type)

# Check missings.
sum(is.na(sub_data_2020$month))
sum(is.na(sub_data_2019$month))

sum(is.na(sub_data_2020$crime_type))
sum(is.na(sub_data_2019$crime_type))

sum(is.na(sub_data_2020$lsoa_code)) # ~98k crimes have no LSOA.
sum(is.na(sub_data_2019$lsoa_code)) # ~161k crimes have no LSOA.

# Drop crimes with missing LSOA (!!!).
sub_data_2020 <- drop_na(sub_data_2020, lsoa_code)
sub_data_2019 <- drop_na(sub_data_2019, lsoa_code)

# Aggregate by month (N = 11), crime type (N = 14), and LSOA (N = ~33,000). Keep 'Year' in for later use.
sub_data_agg_2020 <- sub_data_2020 %>% 
  group_by(crime_type, month, lsoa_code, year) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(crime_type, month, lsoa_code, year, fill = list(crime_count = 0))

sub_data_agg_2019 <- sub_data_2019 %>% 
  group_by(crime_type, month, lsoa_code, year) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(crime_type, month, lsoa_code, year, fill = list(crime_count = 0))

# Check number of each LSOAs appearing in each year. There is a difference.
# This will be because some LSOAs had no recorded crime, and thus never even appeared in individual records.
# Note that we DO have zeros in this data for some LSOAs in some months, but that will be because there was
# a crime recorded in those LSOA in another month. It's possible there is a lack of snap poits in some LSOA, too.
length(unique(sub_data_agg_2020$lsoa_code))
length(unique(sub_data_agg_2019$lsoa_code))

# To create zeros for the no-show LSOAs, first load in a complete set of LSOA codes. 

# Download LSOA from UK Data Service.
# download.file(url = "https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/infuse_lsoa_lyr_2011_clipped.zip",
#               destfile = "data/ukds_infuse_lsoa.zip")

# Unzip.
# unzip(zipfile = "data/ukds_infuse_lsoa.zip", exdir = "data/ukds_infuse_lsoa")

# Load.
lsoa_sf <- st_read("data/ukds_infuse_lsoa/infuse_lsoa_lyr_2011_clipped.shp")

# Subset LSOAs for E&W and then remove Greater Manchester.
lsoa_ew_sf <- lsoa_sf %>% 
  mutate(country_cd = str_extract(geo_code, "^.{1}")) %>% 
  filter(country_cd == "E" | country_cd == "W",
         !str_detect(geo_label, "Manchester|Bolton|Oldham|Rochdale|Stockport|Tameside|Salford|Bury|Trafford|Wigan"))

# Check validity of the remaining LSOAs. QGIS exploration indicated that there were four.
validity_check <- st_is_valid(lsoa_ew_sf)

# This confirms the four.
table(validity_check)

# Identify them
lsoa_ew_sf <- lsoa_ew_sf %>% 
  mutate(valid = validity_check)

filter(lsoa_ew_sf, valid == "FALSE")

# Try to resolve invalid geometries.
lsoa_ew_valid_sf <- st_make_valid(lsoa_ew_sf)

# Check. It worked.
table(st_is_valid(lsoa_ew_valid_sf))

# Remove objects to save space.
rm(sub_data_2019, sub_data_2020)

# There are multiple islands in the dataset, but most are not considered a problem, since they are LSOAs
# which share areas of the mainland or part of major islands (e.g. Isle of Wight). Of course,
# crimes on the islands cannot be considered 'neighbours' with those on the shore, but so few crimes
# actually occurr on these islands that it was deemed less of a problem than arbitarily removing them.
# That said, one island (Scilly) has no corresponding LSOA on the mainland, and thus would have no 
# neighbours in a continuity matrix. This becomes apparent later - just making a note for now.

# First, we identify LSOA within Northern Ireland and Greater Manchester. We have removed those crimes reported by
# these forces, but for consistency we also ensure that any crimes reported by other forces falling within
# their boundaries are removed.

gm_lsoa <- lsoa_sf %>% 
  filter(str_detect(geo_label, "Manchester|Bolton|Oldham|Rochdale|Stockport|Tameside|Salford|Bury|Trafford|Wigan")) %>% 
  select(geo_code) %>% 
  pluck(1)

ni_lsoa <- lsoa_sf %>%
  mutate(country_cd = str_extract(geo_code, "^.{1}")) %>% 
  filter(country_cd == "9") %>% 
  select(geo_code) %>% 
  pluck(1)

# We can now remove lsoa_sf to save space.
rm(lsoa_sf)

# Remove these LSOA from the crime data.
sub_data_agg_2020 <- sub_data_agg_2020 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

sub_data_agg_2019 <- sub_data_agg_2019 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

# Check that the crime data also doesn't include LSOAs from outside of England and Wales.
sub_data_agg_2020 <- sub_data_agg_2020 %>% 
  mutate(country_cd = str_extract(lsoa_code, "^.{1}"))

table(sub_data_agg_2020$country_cd) # Confirmed.

sub_data_agg_2019 <- sub_data_agg_2019 %>% 
  mutate(country_cd = str_extract(lsoa_code, "^.{1}"))

table(sub_data_agg_2019$country_cd) # Confirmed.

# Check LSOA in each. Confirms that police data has slightly less (N = 4) LSOA than the total data.
length(unique(lsoa_ew_valid_sf$geo_code))
length(unique(sub_data_agg_2019$lsoa_code))
length(unique(sub_data_agg_2020$lsoa_code))

# Pull out these 4 LSOA (for 2019 and 2020).
# All four are in London, so it's highly likely this is a snap point quirk, rather than due to zero crimes.
missing_lsoa <- lsoa_ew_sf %>% 
  filter(lsoa_ew_sf$geo_code %nin% sub_data_agg_2020$lsoa_code) # Could also be sub_data_agg_2019

# E01033493 is a football stadium. No roads therefore no snap points.
# E01032775 only covers ends of streets near river - may have missed the crime snap points.
# E01004711 has some construction work, and a break in Google Street View availability (on 11 January 2012).
#           This indicates another snap point street quirk.
# E01003179  appears to not contain any car-worthy roads are could therefore miss snap points.

# Conclusion: it would be misleading to say these had 'zero' crimes, so we will simply not include them
# in any analysis. The polygons are removed from the LSOA data too.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  filter(geo_code %nin% missing_lsoa$geo_code)

# Check number of LSOA in each dataset again. Now identical.
length(unique(lsoa_ew_valid_sf$geo_code))    # 33076
length(unique(sub_data_agg_2019$lsoa_code))  # 33076
length(unique(sub_data_agg_2020$lsoa_code))  # 33076

# Load in urban-rural classification.
urban_df <- read_csv("data/Rural_Urban_Classification__2011__of_Lower_Layer_Super_Output_Areas_in_England_and_Wales.csv")

# Clean names
urban_df <- urban_df %>% 
  clean_names()

# Check classications.
table(urban_df$ruc11)
table(urban_df$ruc11cd)

# Create a broader urban and rural distinction.
urban_df <- urban_df %>% 
  mutate(urban_rural = if_else(condition = str_detect(string = ruc11, pattern = "Rural"), true = "Rural", false = "Urban"))

# Check classications.
table(urban_df$urban_rural)

# Append to the crime data.
sub_data_agg_2020 <- left_join(sub_data_agg_2020, urban_df, by = c("lsoa_code" = "lsoa11cd"))
sub_data_agg_2019 <- left_join(sub_data_agg_2019, urban_df, by = c("lsoa_code" = "lsoa11cd"))

# Check missings.
sum(is.na(sub_data_agg_2020$urban_rural))
sum(is.na(sub_data_agg_2019$urban_rural))

# Append to spatial data.
lsoa_ew_valid_sf <- left_join(lsoa_ew_valid_sf, urban_df, by = c("geo_code" = "lsoa11cd"))

# Create notifiable offences (minus Drugs) df for each April.
notif_off_lsoa_df <- sub_data_agg_1920_df %>% 
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs",
         month == "2020-04" | month == "2019-04") %>% 
  group_by(lsoa_code, month) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 
  mutate(crime_type = "Total crime") %>% 
  pivot_wider(id_cols = lsoa_code, names_from = month, values_from = ew_crime_count) %>% 
  rename(april19 = `2019-04`,
         april20 = `2020-04`)
  
# Append to sf object.
lsoa_ew_valid_not_off_sf <- left_join(lsoa_ew_valid_sf, notif_off_lsoa_df, by = c("geo_code" = "lsoa_code"))

# Save for use in GeoDa.
st_write(obj = lsoa_ew_valid_not_off_sf, dsn = "data/")

# Combine 2019 and 2029 datasets for next steps.
sub_data_agg_1920_df <- bind_rows(sub_data_agg_2019, sub_data_agg_2020)

# Remove objects to retain memory if needed.
rm(lsoa_ew_sf, sub_data_agg_2019, sub_data_agg_2020)

# Raw counts plot, comparing trends across years, similar to first Crime Science paper.

# First, create a small, identical data frame with total notifiable offences as a 'crime type' (excluding drugs).
total_crime_agg_df <- sub_data_agg_1920_df %>%
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs") %>% 
  group_by(month, year) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 
  mutate(crime_type = "Total crime (excl. drugs)") %>% 
  select(crime_type, year, month, ew_crime_count) %>% 
  separate(month, into = c("year", "month"), sep = "-") %>% 
  filter(month != "01", month != "09", month != "10", month != "11", month != "12") 
  
# Then calculate these counts by crime type.
raw_counts_gg <- sub_data_agg_1920_df %>% 
  group_by(crime_type, month, year) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 
  separate(month, into = c("year", "month"), sep = "-") %>% 
  filter(month != "01", month != "09", month != "10", month != "11", month != "12") %>% 
  bind_rows(total_crime_agg_df) %>% 
  mutate(year = recode_factor(year, "2019" = "2019", "2020" = "2020")) %>% 
  ggplot() +
  geom_line(mapping = aes(x = month, y = ew_crime_count, group = year, colour = year), size = 0.8) +
  geom_vline(xintercept = 1.7, linetype = "dotted") +
  facet_wrap(~ crime_type, ncol = 3, scales = "free_y") +
  scale_x_discrete(labels = str_extract(month.name[2:9], "^.{3}")) +
  scale_color_manual(values = rev(c("black", "darkgrey"))) +
  labs(x = NULL, y = NULL, colour = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6), #, hjust = -0.4
        axis.ticks = element_line(size = 0.3, lineend = "round"),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "transparent"),
        legend.position = "bottom")

# Save.
ggsave(plot = raw_counts_gg, filename = "visuals/raw_counts_gg.png", width = 16, height = 20, units = "cm", dpi = 600)

# Calculate E&W-wide Generalized Gini coefficient for notifiable offences, for 2019 and 2020.
gini_tot_1920_df <- sub_data_agg_1920_df %>%
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs") %>% 
  group_by(year, month) %>% 
  summarise(gini_coef = gini(crime_count, generalized = TRUE, unbiased = TRUE)) %>% 
  ungroup() %>% 
  separate(month, into = c("year","month"), sep = "-") %>% 
  mutate(crime_type = "Total crime (excl. drugs)") %>% 
  select(crime_type, gini_coef, year,  month)

# Do the same but by crime type.
gini_ct_1920_df <- sub_data_agg_1920_df %>%
  group_by(year, month, crime_type) %>% 
  summarise(gini_coef = gini(crime_count, generalized = TRUE, unbiased = TRUE)) %>% 
  ungroup() %>% 
  separate(month, into = c("year","month"), sep = "-") %>%
  bind_rows(gini_tot_1920_df) %>% 
  filter(month != "01", month != "09", month != "10", month != "11", month != "12") %>% 
  mutate(year = recode_factor(year, "2019" = "2019", "2020" = "2020")) 

# Facet plot.
gini_gg <- ggplot(data = gini_ct_1920_df) +
  geom_line(mapping = aes(x = month, y = gini_coef, group = year, colour = year), size = 0.8) +
  geom_vline(xintercept = 1.7, linetype = "dotted") +
  facet_wrap(~ crime_type, ncol = 3) +
  ylim(0, 1) +
  labs(x = NULL, y = "Generalized Gini Coefficient", colour = NULL) +
  scale_x_discrete(labels = str_extract(month.name[2:9], "^.{3}")) +
  scale_color_manual(values = rev(c("black", "darkgrey"))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6), #, hjust = -0.4
        axis.ticks = element_line(size = 0.3, lineend = "round"),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "transparent"),
        legend.position = "bottom")

# Save.
ggsave(plot = gini_gg, filename = "visuals/gini_gg.png", width = 14, height = 20, units = "cm", dpi = 600)

# Use longitudinal k-means to unpick the total noficiable crimes (excl. drugs trend) at LSOA level.
tc_kmeans_df <- sub_data_agg_1920_df %>%
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs",
         month != "2019-12") %>% 
  group_by(lsoa_code, month) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() 

# For now, only keep the lockdown period in the first visual: March to August.
tc_kmeans_sub_df <- tc_kmeans_df %>% 
  filter(month == "2020-02" | month == "2020-03" | month == "2020-04" | month == "2020-05" |
         month == "2020-06" | month == "2020-07" | month == "2020-08")

# Check for those LSOAs which were essentially crime-free during the study period.
# We keep in wide because that's what kml likes.
tc_kmeans_sub_clean_df <- tc_kmeans_sub_df %>% 
  pivot_wider(id_cols = lsoa_code, names_from = "month", values_from = "ew_crime_count") %>%
  mutate(total_months = rowSums(.[2:8])) %>% 
  filter(total_months != 0) %>% # one march-aug, none feb-aug.
  select(-total_months) %>% 
  rename(february = `2020-02`,
         march = `2020-03`, april = `2020-04`, may = `2020-05`, june = `2020-06`,
         july = `2020-07` , august = `2020-08`)

length(unique(tc_kmeans_sub_df$lsoa_code))
length(unique(tc_kmeans_sub_clean_df$lsoa_code))


# Perform kmeans
n <- 3:6 # max 6 for simplicity.

tc_kml_mat <- as.matrix(tc_kmeans_sub_clean_df[2:8])
tc_traj    <- clusterLongData(traj = tc_kml_mat) 
kml(tc_traj, nbClusters = n, toPlot = "criterion", nbRedrawing = 20)

# Apppend clusters back with data frame.
tc_clusters <- cbind.data.frame(lsoa_code   = tc_kmeans_sub_clean_df$lsoa_code,
                                 traj       = getClusters(tc_traj, 6)) # We select 6 due to CH value.

tc_clusters_df <- tc_kmeans_sub_clean_df %>% 
  left_join(tc_clusters) %>% 
  pivot_longer(cols = c(-lsoa_code, -traj), names_to = "month", values_to = "ew_crime_counts") %>% 
  mutate(month_fac = fct_relevel(month, "february", "march", "april", "may", "june", "july", "august"))

# Check cluster sizes.
as.data.frame(table(tc_clusters_df$traj)) %>% 
  mutate(traj_count = Freq/7,
         traj_prop  = 100*(traj_count/sum(traj_count)))

# Create new label for each cluster.
tc_clusters_df <- tc_clusters_df %>% 
  mutate(traj_titles  = ifelse(test = traj == "A", yes = "[A] N = 19,162 (58%)" , no = traj),
         traj_titles  = ifelse(test = traj == "B", yes = "[B] N = 10,162 (31%)" , no = traj_titles),
         traj_titles  = ifelse(test = traj == "C", yes = "[C] N = 3,184 (10%)"  , no = traj_titles),
         traj_titles  = ifelse(test = traj == "D", yes = "[D] N = 533 (1.6%)"   , no = traj_titles),
         traj_titles  = ifelse(test = traj == "E", yes = "[E] N = 101 (0.3%)"   , no = traj_titles),
         traj_titles  = ifelse(test = traj == "F", yes = "[F] N = 9 (0.03%)"    , no = traj_titles),
         traj_titles  = fct_relevel(traj_titles, "[A] N = 19,162 (58%)",
                                                 "[B] N = 10,162 (31%)",
                                                 "[C] N = 3,184 (10%)",
                                                 "[D] N = 533 (1.6%)",
                                                 "[E] N = 101 (0.3%)",
                                                 "[F] N = 9 (0.03%)"))

# Create new label for each cluster.
# tc_clusters_df <- tc_clusters_df %>% 
#   mutate(traj_titles  = ifelse(test = traj == "A", yes = "N = 23,924 (72%)", no = traj),
#          traj_titles  = ifelse(test = traj == "B", yes = "N = 8,124 (25%)" , no = traj_titles),
#          traj_titles  = ifelse(test = traj == "C", yes = "N = 944 (3%)"  , no = traj_titles),
#          traj_titles  = ifelse(test = traj == "D", yes = "N = 83 (0.25%)"  , no = traj_titles),
#          traj_titles  = fct_relevel(traj_titles, "N = 23,924 (72%)",
#                                                  "N = 8,124 (25%)",
#                                                  "N = 944 (3%)",
#                                                  "N = 83 (0.25%)"))


# Save for Anthony
# tc_clusters_df %>% 
#   distinct(lsoa_code, traj, traj_titles) %>% 
#   write_csv(path = "results/tc_clusters_df.csv")

# Add 2019 years to it.
traj_names_df <- tc_clusters_df %>% 
  distinct(lsoa_code, traj, traj_titles)
  
tc_clusters_2019_df <- sub_data_agg_1920_df %>%
  filter(year == "2019",
         crime_type != "Anti-social behaviour" & crime_type != "Drugs") %>% 
  group_by(month, lsoa_code) %>% 
  summarise(ew_crime_counts = sum(crime_count)) %>% 
  ungroup() %>% 
  left_join(traj_names_df) %>% 
  drop_na(traj_titles) %>% # the LSOA dropped for clustering
  filter(month != "2019-01", month != "2019-09", month != "2019-10", month != "2019-11", month != "2019-12") %>%
  mutate(month = as_factor(month),
         month_fac = fct_recode(month, february = "2019-02",
                                       march    = "2019-03",
                                       april    = "2019-04",
                                       may      = "2019-05",
                                       june     = "2019-06",
                                       july     = "2019-07",
                                       august   = "2019-08"))

tc_clusters_2019_df <- tc_clusters_2019_df %>%
  mutate(traj_titles  = ifelse(test = traj == "A", yes = "[A] N = 19,162 (58%)" , no = traj),
         traj_titles  = ifelse(test = traj == "B" , yes = "[B] N = 10,162 (31%)" , no = traj_titles),
         traj_titles  = ifelse(test = traj == "C" , yes = "[C] N = 3,184 (10%)"  , no = traj_titles),
         traj_titles  = ifelse(test = traj == "D" , yes = "[D] N = 533 (1.6%)"   , no = traj_titles),
         traj_titles  = ifelse(test = traj == "E" , yes = "[E] N = 101 (0.3%)"   , no = traj_titles),
         traj_titles  = ifelse(test = traj == "F" , yes = "[F] N = 9 (0.03%)"    , no = traj_titles),
         traj_titles  = fct_relevel(traj_titles, "[A] N = 19,162 (58%)",
                                                 "[B] N = 10,162 (31%)",
                                                 "[C] N = 3,184 (10%)",
                                                 "[D] N = 533 (1.6%)",
                                                 "[E] N = 101 (0.3%)",
                                                 "[F] N = 9 (0.03%)"))



# Expand violin plot with 2018 data.
list_2018 <- paste("data/", list.files("data", pattern = glob2rx("2018*street.csv"),  recursive=TRUE), sep = "")

# Read in .csv files for each year.
data_2018 <- lapply(list_2018, read_csv)

# Bind each in to data frames. For now, we keep the years separate.
full_data_2018 <- data_2018 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2018")

# Create year ID, then remove Greater Manchester Police and Police Service of NI.
sub_data_2018 <- full_data_2018 %>% 
  filter(reported_by != "Greater Manchester Police", 
         reported_by != "Police Service of Northern Ireland")

# Remove existing data objects to free up memory if needed.
rm(data_2018, full_data_2018)

# Check months.
unique(sub_data_2018$month)

# Check missings.
sum(is.na(sub_data_2018$month))
sum(is.na(sub_data_2018$crime_type))
sum(is.na(sub_data_2018$lsoa_code)) # ~123k crimes have no LSOA.

# Drop crimes with missing LSOA (!!!).
sub_data_2018 <- drop_na(sub_data_2018, lsoa_code)

# Remove crimes GM or NI.
sub_ew_data_2018 <- sub_data_2018 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

# Check that the crime data also doesn't include LSOAs from outside of England and Wales.
sub_ew_data_2018 <- sub_ew_data_2018 %>% 
  mutate(country_cd = str_extract(lsoa_code, "^.{1}"))

table(sub_ew_data_2018$country_cd) # Confirmed.

# Retain only those LSOA used in the k-means for 2020 (a bit repetitive but it checks).
sub_ew_data_2018 <- sub_ew_data_2018 %>% 
  filter(lsoa_code %in% tc_clusters$lsoa_code)

# Check.
length(unique(sub_ew_data_2018$lsoa_code))

# Aggregate. Remove ASB and drugs, and unwanted months, then tally.
sub_ew_agg_2018 <- sub_ew_data_2018 %>% 
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs",
         month == "2018-02" | month == "2018-03" | month == "2018-04" | month == "2018-05" |
           month == "2018-06" | month == "2018-07" | month == "2018-08") %>% 
  group_by(month, lsoa_code) %>% 
  summarise(ew_crime_counts = n()) %>% 
  ungroup() 

# Get df with cluster classes (letter only due to memory) and LSOA names.
lsoa_clusters_df <- tc_clusters_df %>% 
  distinct(lsoa_code, traj, traj_titles)

# Join with 2018 data.
tc_clusters_2018_df <- sub_ew_agg_2018 %>% 
  left_join(lsoa_clusters_df) %>% 
  mutate(month_fac   = fct_recode(month, february = "2018-02",
                                  march    = "2018-03",
                                  april    = "2018-04",
                                  may      = "2018-05",
                                  june     = "2018-06",
                                  july     = "2018-07",
                                  august   = "2018-08"),
         traj_titles  = ifelse(test = traj == "A", yes = "[A] N = 19,162 (58%)" , no = traj),
         traj_titles  = ifelse(test = traj == "B", yes = "[B] N = 10,162 (31%)" , no = traj_titles),
         traj_titles  = ifelse(test = traj == "C", yes = "[C] N = 3,184 (10%)"  , no = traj_titles),
         traj_titles  = ifelse(test = traj == "D", yes = "[D] N = 533 (1.6%)"   , no = traj_titles),
         traj_titles  = ifelse(test = traj == "E", yes = "[E] N = 101 (0.3%)"   , no = traj_titles),
         traj_titles  = ifelse(test = traj == "F", yes = "[F] N = 9 (0.03%)"    , no = traj_titles),
         traj_titles  = fct_relevel(traj_titles, "[A] N = 19,162 (58%)",
                                                 "[B] N = 10,162 (31%)",
                                                 "[C] N = 3,184 (10%)",
                                                 "[D] N = 533 (1.6%)",
                                                 "[E] N = 101 (0.3%)",
                                                 "[F] N = 9 (0.03%)"))



# Violin plot with 2018 added.
kmeans_violin_1819_gg <- ggplot(data = tc_clusters_df,
                                mapping = aes(x = month_fac, y = ew_crime_counts,#
                                              fill = traj_titles)) +
  geom_violin(alpha = 0.3, colour = "transparent", adjust = 2) + # to match asb - makes no difference.
  facet_wrap(~traj_titles, ncol = 2, scales = "free_y") +
  stat_summary(data = tc_clusters_2019_df,
               mapping = aes(x = month_fac, y = ew_crime_counts, group = traj_titles),
               fun = "median", colour = "red", size = 0.5, geom = "line") +
  stat_summary(data = tc_clusters_2019_df,
               mapping = aes(x = month_fac, y = ew_crime_counts, group = traj_titles),
               fun = "mean", colour = "red", linetype = "dotted", size = 0.5, geom = "line") +
  stat_summary(data = tc_clusters_2018_df,
               mapping = aes(x = month_fac, y = ew_crime_counts, group = traj_titles),
               fun = "median", colour = "blue", size = 0.5, geom = "line") +
  stat_summary(data = tc_clusters_2018_df,
               mapping = aes(x = month_fac, y = ew_crime_counts, group = traj_titles),
               fun = "mean", colour = "blue", linetype = "dotted", size = 0.5, geom = "line") +
  stat_summary(aes(group = traj_titles), fun = "median", colour = "black", size = 0.8, geom = "line") +
  stat_summary(aes(group = traj_titles), fun = "mean", colour = "black", linetype = "dotted", size = 0.8, geom = "line") +
  scale_x_discrete(labels = c(str_extract(month.name[2:8], "^.{3}"), character(1))) +
  labs(x = NULL, y = "crime count") +
  theme_bw() +
  theme(legend.position = "none")

# Save full plot: 2020 clusters, 2018 and 2019.
ggsave(plot = kmeans_violin_1819_gg, filename = "visuals/kmeans_violin_k6_1819_gg.png",
       height = 24, width = 20, unit = "cm", dpi = 200)
# ggsave(plot = kmeans_violin_1819_gg, filename = "visuals/kmeans_violin_1819_gg.png",
#        height = 20, width = 20, unit = "cm", dpi = 200)

# Save and load workspace as appropriate.
# save.image(file = "data_handling_6kmean.RData")
load(file = "data_handling_6kmean.RData")


# Create proportion contribution to total crime in each month.
tc_clusters_props_df <- tc_clusters_df %>%
  group_by(month_fac) %>% 
  mutate(monthly_counts = sum(ew_crime_counts)) %>% 
  ungroup() %>% 
  mutate(monthly_props = 100*(ew_crime_counts/monthly_counts)) %>% 
  group_by(month_fac, traj_titles) %>% 
  summarise(sum_monthly_props = sum(monthly_props)) %>% 
  ungroup()

# Plot stacks
tc_props_gg <- ggplot(data = tc_clusters_props_df) +
  geom_bar(mapping = aes(x = month_fac, y = sum_monthly_props, group = traj_titles, fill = traj_titles),
            alpha = 0.8, stat = "identity") +
  scale_x_discrete(labels = c(str_extract(month.name[3:9], "^.{3}"), character(1))) +
  labs(x = NULL, y = "% total crime", fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

# Save.
ggsave(plot = tc_props_gg, filename = "visuals/tc_props_gg.png",
       height = 14, width = 16, unit = "cm", dpi = 200)

# Calculate the % of month-on-month change each cluster contributed to.
change_df <- tc_clusters_df %>% 
  select(lsoa_code, month, traj_titles, ew_crime_counts) %>% 
  group_by(month, traj_titles) %>%
  summarise(traj_crimes = sum(ew_crime_counts)) %>%
  ungroup() %>%
  group_by(month) %>%
  mutate(total_crimes = sum(traj_crimes)) %>% # sum crimes across all trajectories (i.e. count per month in e&w)
  pivot_wider(id_cols = traj_titles, names_from = month, values_from = c(traj_crimes, total_crimes)) %>% 
  select(traj_titles,
         traj_crimes_february,
         traj_crimes_march, traj_crimes_april, traj_crimes_may, 
         traj_crimes_june, traj_crimes_july, traj_crimes_august,
         total_crimes_february,
         total_crimes_march, total_crimes_april, total_crimes_may, 
         total_crimes_june, total_crimes_july, total_crimes_august) %>% 
  mutate(traj_feb_mar = traj_crimes_march-traj_crimes_february,
         traj_mar_apr = traj_crimes_april-traj_crimes_march,
         traj_apr_may = traj_crimes_may-traj_crimes_april,
         traj_may_jun = traj_crimes_june-traj_crimes_may,
         traj_jun_jul = traj_crimes_july-traj_crimes_june,
         traj_jul_aug = traj_crimes_august-traj_crimes_july,
         tot_feb_mar  = sum(abs(traj_feb_mar)),
         tot_mar_apr  = sum(abs(traj_mar_apr)),
         tot_apr_may  = sum(abs(traj_apr_may)),
         tot_may_jun  = sum(abs(traj_may_jun)),
         tot_jun_jul  = sum(abs(traj_jun_jul)),
         tot_jul_aug  = sum(abs(traj_jul_aug)),
         prop_feb_mar = 100*round(traj_feb_mar/tot_feb_mar, 2),
         prop_mar_apr = 100*round(traj_mar_apr/tot_mar_apr, 2),
         prop_apr_may = 100*round(traj_apr_may/tot_apr_may, 2),
         prop_may_jun = 100*round(traj_may_jun/tot_may_jun, 2),
         prop_jun_jul = 100*round(traj_jun_jul/tot_jun_jul, 2),
         prop_jul_aug = 100*round(traj_jul_aug/tot_jul_aug, 2)) %>% 
  select(traj_titles, traj_feb_mar:traj_jul_aug, prop_feb_mar:prop_jul_aug) %>% 
  pivot_longer(cols = -traj_titles, names_to = "month_change", values_to = "change") %>% 
  mutate(stat = if_else(condition = str_detect(month_change, "prop"), true = "prop_change", false = "count_change"),
         month = if_else(condition = str_detect(month_change, "feb_mar"), true = "feb_mar", false = month_change),
         month = if_else(condition = str_detect(month_change, "mar_apr"), true = "mar_apr", false = month),
         month = if_else(condition = str_detect(month_change, "apr_may"), true = "apr_may", false = month),
         month = if_else(condition = str_detect(month_change, "may_jun"), true = "may_jun", false = month),
         month = if_else(condition = str_detect(month_change, "jun_jul"), true = "jun_jul", false = month),
         month = if_else(condition = str_detect(month_change, "jul_aug"), true = "jul_aug", false = month)) %>%
  pivot_wider(id_cols = c(month, traj_titles), names_from = stat, values_from = change) %>% 
  mutate(month = fct_relevel(month,
                             "feb_mar",
                             "mar_apr",
                             "apr_may",
                             "may_jun",
                             "jun_jul",
                             "jul_aug"),
         prop_change = paste(abs(prop_change), "%", sep = ""))


change_gg <- ggplot(data = change_df) +
  geom_bar(mapping = aes(x = month, y = count_change,
                         group = traj_titles, fill = traj_titles), stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.3, alpha = 1) +
  geom_text(mapping = aes(x = month, y = count_change, group = traj_titles, label = prop_change),
            position = position_dodge(width = 0.9), size = 1.7,  vjust = -1.5) +
  labs(y = "Count change attributable to cluster", x = NULL, fill = NULL) +
  scale_x_discrete(labels = c("February to March", "March to April", "April to May", "May to June", "June to July", "July to August")) +
  theme_bw() +
  theme(legend.position = "bottom")

change_ann_gg <- change_gg +
  annotate(geom = "text", x = 1.5, y = 14000, size = 3, label = "Percentage of nationwide \n absolute monthly change \n attributable to cluster") +
  annotate(geom = "curve", x = 2.15, xend = 2.65, y = 14500, yend = 15000, curvature = -0.1, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = 3.5, y = -11000, size = 3,
           label = "15% of the lockdown drop  \n between March and April \n attributable to just 110 LSOA") +
  annotate(geom = "curve", x = 3.5, xend = 2.52, y = -9000, yend = -3000, curvature = 0.2, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "curve", x = 3.5, xend = 2.32, y = -9000, yend = -6000, curvature = 0.2, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = 5, y = -8000, size = 3, label = "Further nationwide increase \n partially offset") +
  annotate(geom = "curve", x = 5.2, xend = 5.6, y = -6500, yend = -1300, curvature = -0.3, arrow = arrow(length = unit(1, "mm")))

# change_ann_gg <- change_gg +
#   annotate(geom = "text", x = 2.5, y = -15000, size = 3, 
#            label = "Cluster accounted for 13% \n of the lockdown crime drop \n between March and April") +
#   annotate(geom = "curve", x = 2, xend = 1.4, y = -12500, yend = -9000, curvature = 0.2, arrow = arrow(length = unit(1, "mm"))) +
#   annotate(geom = "text", x = 4, y = -8000, size = 3, label = "Further nationwide increase \n partially offset") +
#   annotate(geom = "curve", x = 4.65, xend = 4.8, y = -7200, yend = -2500, curvature = 0.3, arrow = arrow(length = unit(1, "mm"))) 

# Save.
ggsave(plot = change_ann_gg, filename = "visuals/change_k6_gg.png", width = 18, height = 16, unit = "cm")
# ggsave(plot = change_ann_gg, filename = "visuals/change_gg.png", width = 18, height = 16, unit = "cm")

# Crime composition of each cluster.
# Only keep what's needed.
tc_clusters_sub_df <- tc_clusters_df %>% 
  select(lsoa_code, traj) %>% 
  distinct(lsoa_code, traj)
  
# Join back with crime data.
traj_crime_types_df <- sub_data_agg_1920_df %>% 
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs",
         month == "2020-02" | month == "2020-03" | month == "2020-04" | month == "2020-05" |
         month == "2020-06" | month == "2020-07" | month == "2020-08") %>% 
  left_join(tc_clusters_sub_df, by = "lsoa_code")

# Calculate
traj_crime_types_chars_df <- traj_crime_types_df %>% 
  group_by(traj, month) %>% 
  mutate(traj_crimes = sum(crime_count)) %>%
  ungroup() %>% 
  group_by(crime_type, traj, month) %>% 
  mutate(traj_ct_crime = sum(crime_count)) %>% 
  ungroup() %>% 
  mutate(traj_ct_crime_prop = traj_ct_crime/traj_crimes) %>% 
  distinct(crime_type, month, traj, traj_ct_crime_prop) %>% 
  drop_na(traj) # These all belong the LSOA that we dropped from the clustering.

# Check.
traj_crime_types_chars_df %>% 
  group_by(traj, month) %>% 
  summarise(sum_prop = sum(traj_ct_crime_prop))

# Retrieve cluster names - they were too big for join earlier.
traj_names_df <- tc_clusters_df %>% 
  distinct(traj, traj_titles)

traj_crime_types_chars_df <- traj_crime_types_chars_df %>% 
  left_join(traj_names_df)

# Plot.
traj_crimes_gg <- ggplot(data = traj_crime_types_chars_df) +
  geom_bar(mapping = aes(x = month, y = traj_ct_crime_prop, group = crime_type, fill = crime_type),
            stat = "identity", colour = "black", size = 0.2) +
  facet_wrap(~traj_titles, nrow = 6) +
  labs(y = "Proportion comprising total crime", x = NULL, fill = NULL) +
  scale_x_discrete(labels = c(str_extract(month.name[3:9], "^.{3}"), character(1))) +
  guides(fill = guide_legend(ncol = 1)) +
  theme_bw() +
  theme(legend.position = "right", strip.background = element_rect(fill = "transparent"))

# Save.
ggsave(plot = traj_crimes_gg, filename = "visuals/traj_crimes_monthly_k6_gg.png", width = 14, height = 26, unit = "cm")
# ggsave(plot = traj_crimes_gg, filename = "visuals/traj_crimes_monthly_gg.png", width = 15, height = 21, unit = "cm")
# ggsave(plot = traj_crimes_gg, filename = "visuals/traj_crimes_gg.png", width = 16, height = 16, unit = "cm")


# # Spatial plot of clusters (rather pointless at the national level)
# traj_names_df <- tc_clusters_df %>% 
#   distinct(lsoa_code, traj, traj_titles)
# 
# # Join with valid sf object
# lsoa_ew_valid_sf <- left_join(lsoa_ew_valid_sf, traj_names_df, by = c("geo_code" = "lsoa_code"))
# 
# # Plot nationwide map.
# cluster_map_gg <- ggplot(data = lsoa_ew_valid_sf) +
#   geom_sf(mapping = aes(fill = traj_titles), alpha = 0.7, colour = "transparent")
# 
# # Save.
# ggsave(plot = cluster_map_gg, filename = "visuals/cluster_map_gg.png", width = 20, height = 30, unit = "cm")

# OSM characteristics of the clusters (see osm_handling.r)
osm_df <- read_csv("data/osm_full.csv")

# Create bus and railway station total.
osm_df <- osm_df %>% 
  mutate(transport_total = trains + bus,
         shops_total     = shops_total + conveniences)
         # parking_total = car_spaces + car_parkings + moto_parkings) # this measure is not reliable due to low
                                                                      # counts, so we don't calculate stats later.

# Get LSOA and cluster only.
traj_names_df <- tc_clusters_df %>% 
  distinct(lsoa_code, traj, traj_titles) 

# Join OSM data.
traj_names_osm_df <- left_join(traj_names_df, osm_df, by = c("lsoa_code" = "geo_code"))

# Overall descriptives.

osm_total_stats_df <- traj_names_osm_df %>%
  summarise(Median_nightlife = median(nightlife_total),
            Mean_nightlife   = mean(nightlife_total),
            SD_nightlife     = sd(nightlife_total),
            Min_nightlife    = min(nightlife_total),
            Max_nightlife    = max(nightlife_total),
            Median_shops     = median(shops_total),
            Mean_shops       = mean(shops_total),
            SD_shops         = sd(shops_total),
            Min_shops        = min(shops_total),
            Max_shops        = max(shops_total),
            Median_transport     = median(transport_total),
            Mean_transport       = mean(transport_total),
            SD_transport         = sd(transport_total),
            Min_transport        = min(transport_total),
            Max_transport        = max(transport_total),
            Median_bikes     = median(bikes),
            Mean_bikes       = mean(bikes),
            SD_bikes         = sd(bikes),
            Min_bikes        = min(bikes),
            Max_bikes        = max(bikes)) %>% 
  mutate_if(is.numeric, round, 2) %>%
  mutate(id = 1) %>% 
  pivot_longer(cols = -id, names_to = "Facility", values_to = "value") %>% 
  separate(col = Facility, sep = "_", into = c("statistic", "Facility")) %>% 
  pivot_wider(id_cols = Facility, names_from = statistic, values_from = value) %>% 
  mutate_if(is.character, str_to_title)

# Cluster descriptives.
osm_stats_df <- traj_names_osm_df %>%
  group_by(traj_titles) %>% 
  summarise(median_nightlife = median(nightlife_total),
            mean_nightlife   = mean(nightlife_total),
            sd_nightlife     = sd(nightlife_total),
            median_shops     = median(shops_total),
            mean_shops       = mean(shops_total),
            sd_shops         = sd(shops_total),
            median_trans     = median(transport_total),
            mean_trans       = mean(transport_total),
            sd_trans         = sd(transport_total),
            median_bikes     = median(bikes),
            mean_bikes       = mean(bikes),
            sd_bikes         = sd(bikes)) %>% 
  mutate_if(is.numeric, round, 2)

# # Stats visual.
# osm_stats_gg <- traj_names_osm_df %>% 
#   select(traj_titles, nightlife_total, shops_total, transport_total, bikes) %>% 
#   pivot_longer(cols = -traj_titles, names_to = "type", values_to = "count") %>% 
#   ggplot(.) +
#   geom_histogram(mapping = aes(x = count), bins = 30) +
#   facet_wrap(~type + traj_titles, scales = "free_y") +
#   theme_bw() +
#   theme(axis.text = element_text(size = 6))
# 
# # Save.
# ggsave(plot = osm_stats_gg, filename = "visuals/osm_plots_k6.png", height = 10, width = 12)

# Save stats.
write_csv(x = osm_stats_df, path = "data/osm_stats_rounded_k6.csv")


# What are the demographic/urban characteristics of these clusters?

# Urban-rural classification.
tc_clusters_df <- tc_clusters_df %>%
  left_join(urban_df, by = c("lsoa_code" = "lsoa11cd"))

urban_gg <- tc_clusters_df %>%
  group_by(traj_titles) %>% 
  mutate(n_traj = n()) %>% 
  ungroup() %>% 
  group_by(traj_titles, ruc11) %>% 
  summarise(n_ruc11 = n(),
            prop_ruc11 = n_ruc11/n_traj) %>% 
  ungroup() %>% 
  distinct(traj_titles, ruc11, prop_ruc11) %>%
  ggplot() +
  geom_bar(mapping = aes(x = traj_titles, y = prop_ruc11, fill = ruc11), stat = "identity", alpha = 0.7) +
  guides(fill = guide_legend(nrow = 3)) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(x = NULL, y = "Proportion of cluster", fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(plot = urban_gg, filename = "visuals/urban_gg.png", height = 20, width = 20, unit = "cm")

# Deprivation

# Download 2019 IMD.
# download.file(url = "https://opendata.arcgis.com/datasets/d4b79be994ac4820ad44e10ded313df3_0.csv",
#               destfile = "data/e_imd_lsoa.csv")
# 
# download.file(url = "http://geoportal1-ons.opendata.arcgis.com/datasets/b4fddfae9a0a4259ade22cab349208f7_0.csv",
#               destfile = "data/w_imd_lsoa.csv")

# Load data.
e_imd_df <- read_csv("data/e_imd_lsoa.csv")
w_imd_df <- read_csv("data/w_imd_lsoa.csv")

# Check zeros.
table(e_imd_df$IMDDecil) # 1909 LSOA in England do not have a decile - same number as Welsh LSOA.

zeros_df <- e_imd_df %>% 
  filter(IMDDecil == 0) # The zeros are just the Welsh ones.

# Make comparable
e_imd_df <- e_imd_df %>% 
  select(lsoa11cd, IMDRank, IMDDecil) %>% 
  filter(IMDDecil !=0) # Remove the Welsh (zero) LSOA from the England data.

w_imd_df <- w_imd_df %>% 
  select(lsoa11cd, wimd_2019) %>% 
  rename(IMDRank = wimd_2019) %>% 
  mutate(IMDDecil = as.numeric(ntile(IMDRank, 10))) # Create deciles for Wales.

table(w_imd_df$IMDDecil)

# Check names
names(e_imd_df)
names(w_imd_df)

# Combine
ew_imd_df <- bind_rows(e_imd_df, w_imd_df)

# Still equal number of deciles, but note that the deciles are for E&W each.
table(ew_imd_df$IMDDecil)

# Check LSOA codes.
length(unique(tc_clusters_df$lsoa_code))
length(unique(ew_imd_df$lsoa11cd))

# How many match? All of them. So we have deciles for each LSOA in the cluster data.
matches_df <- ew_imd_df %>% 
  filter(lsoa11cd %in% unique(tc_clusters_df$lsoa_code))

nrow(matches_df) # N = 33075
sum(is.na(matches_df)) # no missings.

# Join.
tc_clusters_imd_df <- tc_clusters_df %>% 
  left_join(ew_imd_df, by = c("lsoa_code" = "lsoa11cd"))

# Check. 
table(tc_clusters_imd_df$month)
length(unique(tc_clusters_imd_df$lsoa_code))
table(tc_clusters_imd_df$IMDDecil) # Note unbalanced groupings.

test_df <- tc_clusters_imd_df %>% 
  group_by(lsoa_code, IMDDecil) %>% 
  tally()

nrow(test_df) # N = 33075
length(unique(test_df$lsoa_code)) # 33075
table(test_df$n)  # Each one has 6 time measurement points

# Plot.
decile_gg <- tc_clusters_imd_df %>%
  group_by(traj_titles) %>% 
  mutate(n_traj = n(),
         IMDDecil = as.factor(IMDDecil)) %>% 
  ungroup() %>% 
  group_by(traj_titles, IMDDecil) %>% 
  summarise(n_decile = n(),
            prop_decile = n_decile/n_traj) %>% 
  ungroup() %>% 
  distinct(traj_titles, IMDDecil, prop_decile) %>%
  ggplot() +
  geom_bar(mapping = aes(x = traj_titles, y = prop_decile, fill = IMDDecil), stat = "identity", alpha = 0.9) +
  scale_fill_brewer(palette = "Spectral") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = NULL, y = "Proportion of cluster", fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

# Save.
ggsave(plot = decile_gg, filename = "visuals/decile_gg.png", height = 20, width = 20, unit = "cm")

# We know that the most change occurred in April, across all crimes types.
# But what areas were driving this change? Was it previously criminal areas?
# Here, we calculate which LSOAs are driving the change observed in April.

# Note that we create a count+1 variable for the Poisson test.
aprils_lsoa_df <- sub_data_agg_1920_df %>%
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs",
         month == "2019-04" | month == "2020-04") %>%
  group_by(lsoa_code, month) %>% 
  summarise(cc_total   = sum(crime_count)) %>% 
  mutate(cc_total_plus = cc_total+1) %>% 
  group_by(month) %>%
  mutate(decile = ntile(cc_total_plus, 10)) %>%
  ungroup() %>%
  arrange(decile)

# Calculate mean counts occuring in each decile, in each month.
decile_means_df <- aprils_lsoa_df %>% 
  group_by(decile, month) %>% 
  summarise(mean_dec_cc_plus = mean(cc_total_plus)) %>% 
  mutate(mean_dec_cc_plus = round(mean_dec_cc_plus)) %>% 
  arrange(month, decile)

# Check visually.
ggplot(data = decile_means_df) +
  geom_bar(mapping = aes(x = decile, y = mean_dec_cc_plus), stat = "identity") +
  facet_wrap(~ month)

# Now we need to link the LSOAs in each decile from 2019, to the counts for 2020.
april19_df <- aprils_lsoa_df %>% 
  filter(month == "2019-04")

april20_df <- aprils_lsoa_df %>% 
  filter(month == "2020-04") %>% 
  select(lsoa_code, month, cc_total, cc_total_plus) %>% 
  rename(cc_total_20      = cc_total,
         cc_total_plus_20 = cc_total_plus,
         month_20         = month)

# Join back with the 2019 deciles.
april19_joined_df <- left_join(april19_df, april20_df)

# Check distirbutions.
ggplot(data = filter(april19_joined_df, cc_total_plus < 125)) +
  geom_histogram(mapping = aes(x = cc_total_plus), bins = 100) 

ggplot(data = filter(april19_joined_df, cc_total_plus_20 < 125)) +
  geom_histogram(mapping = aes(x = cc_total_plus_20), bins = 100) 

# Create mean counts for each April (2019 and 2020) by the 2019 deciles.
# Removing outliers >125 does not change results.
decile_means_19 <- april19_joined_df %>%
  group_by(decile) %>% 
  summarise(mean_dec_count19 = round(mean(cc_total_plus)))

decile_means_20 <- april19_joined_df %>% 
  group_by(decile) %>% 
  summarise(mean_dec_count20 = round(mean(cc_total_plus_20))) %>% 
  select(-decile)

# Bind together, then into list, for poisson test.
decile_means_df <- bind_cols(decile_means_19, decile_means_20)
decile_means_list <- group_split(decile_means_df, decile)

perc_changes <- paste(round(100*(decile_means_df$mean_dec_count20-decile_means_df$mean_dec_count19)/decile_means_df$mean_dec_count19),
                      "%", sep = "")

# Run poisson text through list.
lapply(decile_means_list, function(x){poisson.test(c(x$mean_dec_count19, x$mean_dec_count20))})

# Visualise the change descriptively. 
deciles_tot_gg <- april19_joined_df %>%
  select(-month, -cc_total, -cc_total_20, -month_20) %>%
  pivot_longer(cols = c(-lsoa_code, -decile), values_to = "counts", names_to = "year") %>% 
  group_by(year, decile) %>% 
  summarise(mean_counts = round(mean(counts))) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = as.factor(decile), y = mean_counts, group = year, fill = year),
           stat = "identity", position = "dodge") +
  scale_fill_discrete(labels = c("April 2019", "April 2020"), direction = -1) +
  labs(fill = NULL,  y = "Mean recorded crimes", x = "LSOA crime decile in April 2019",
       title = "Notifiable offences in England and Wales", caption = "Drug offences excluded") +
  theme_bw()

deciles_tot_an_gg <- deciles_tot_gg +
  annotate(geom = "curve", x = 0.5, xend = 9.5, y = 25, yend = 25, curvature = 0) +
  annotate(geom = "curve", x = 0.5, xend = 0.5, y = 25, yend = 23, curvature = 0) +
  annotate(geom = "curve", x = 9.5, xend = 9.5, y = 25, yend = 23, curvature = 0) +
  annotate(geom = "text" , x = 4, y = 27, label = "No stat. sig. change", size = 3) +
  annotate(geom = "curve", x = 8.5, xend = 10, y = 35, yend = 23, curvature = -0.2, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text" , x = 7.4, y = 36, label = "Stat. sig. change", size = 3) +
  annotate(geom = "text" , x = 1 , y = 6, label = paste("+", perc_changes[1], sep = ""), size = 3) +
  annotate(geom = "text" , x = 2 , y = 7, label = paste("+", perc_changes[2], sep = ""), size = 3) +
  annotate(geom = "text" , x = 3 , y = 8, label = paste("+", perc_changes[3], sep = ""), size = 3) +
  annotate(geom = "text" , x = 4 , y = 9, label = perc_changes[4] , size = 3) +
  annotate(geom = "text" , x = 5 , y = 10, label = perc_changes[5] , size = 3) +
  annotate(geom = "text" , x = 6 , y = 12, label = perc_changes[6] , size = 3) +
  annotate(geom = "text" , x = 7 , y = 14, label = perc_changes[7] , size = 3) +
  annotate(geom = "text" , x = 8 , y = 17, label = perc_changes[8] , size = 3) +
  annotate(geom = "text" , x = 9 , y = 22, label = perc_changes[9] , size = 3) +
  annotate(geom = "text" , x = 10, y = 46, label = perc_changes[10], size = 3) 
  
ggsave(plot = deciles_tot_an_gg, filename = "visuals/deciles_tot_an_gg.png", height = 12, width = 18, unit = "cm")

# Create this crime distinciton in the main df.
crime_cats_1920_df <- sub_data_agg_1920_df %>% 
  filter(crime_type != "Drugs") %>% 
  mutate(crime_cat = if_else(condition = crime_type != "Anti-social behaviour", "Notifiable", crime_type))

# Spearman's rank between deciles.
cor_prep_list <- sub_data_agg_1920_df %>% 
  filter(month == "2019-04" | month == "2020-04") %>% 
  select(crime_type, month, lsoa_code, crime_count) %>% 
  group_split(crime_type) 







