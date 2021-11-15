# Load packages
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load data
burglary_df <- read_csv("data/gmp_2017.csv")

# Scatter plot 1
ggplot(data = burglary_df, mapping = aes(x = incscore, y = burglary_count)) +
  geom_point() +
  geom_smooth()

# Scatter plot 2
ggplot(data = burglary_df) +
  geom_point(mapping = aes(x = incscore, y = burglary_count)) +
  geom_smooth(mapping = aes(x = incscore, y = burglary_count))

# Scatter plot 3
ggplot() +
  geom_point(data = burglary_df, mapping = aes(x = incscore, y = burglary_count)) +
  geom_line(data = monthly_df, mapping = aes(x = Month, y = n, linetype = crime_type))
  
# Bar plot 1


