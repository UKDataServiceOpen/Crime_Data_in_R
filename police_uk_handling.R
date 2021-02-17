# Load packages.
library(readr)
library(dplyr)
library(janitor)
library(tidyr)

# Archive data covering the 3-year period up to and including November 2020. Note that you may want to create
# or edit your folders/working directory accordingly to ensure there is a folder called 'data'.
download.file(url = "https://data.police.uk/data/archive/2020-11.zip", destfile = "data/archive2020-11.zip")

# Unzip.
unzip(zipfile = "data/archive2020-11.zip", exdir = "data")

# List all those 'street' files (rather than outcomes or stop and search) for 2020 and 2019.
list_2020 <- paste("data/", list.files("data", pattern = glob2rx("2020*street.csv"),  recursive=TRUE), sep = "")
list_2019 <- paste("data/", list.files("data", pattern = glob2rx("2019*street.csv"),  recursive=TRUE), sep = "")

# Read in .csv files for each year.
data_2020 <- lapply(list_2020, read_csv)
data_2019 <- lapply(list_2019, read_csv)

# Bind each in to data frames. For now, we keep the years separate, but the structure is the same, so
# you could combine if needed.

full_data_2020 <- data_2020 %>% 
  bind_rows() %>% 
  clean_names() %>% # useful function to make column names uniform
  mutate(year = "2020")

full_data_2019 <- data_2019 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2019")

# Example of aggregating by crime type, by lsoa, by month, and by year. The month column does tell us
# the year but we group by year too just to keep the info. This example is for 2019 only.
sub_data_agg_2019 <- full_data_2019 %>% 
  drop_na(lsoa_code) %>%                             # Not necessarily a good idea!
  group_by(crime_type, month, lsoa_code, year) %>%   # Grouping.
  summarise(crime_count = n()) %>%                   # Each row is a crime, so we counts rows by groups.
  ungroup() %>%                                      # Ungroup everything.
  complete(crime_type, month, lsoa_code, year,
           fill = list(crime_count = 0))             # Counts with 0 get filled in, rather than removed.

# You now have 5837832 rows because it's 12 months, 14 crime types, and 34749 LSOA.
34749*12*14 == 5837832
