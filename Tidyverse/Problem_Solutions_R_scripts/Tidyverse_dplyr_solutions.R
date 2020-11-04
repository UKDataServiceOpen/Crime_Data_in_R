#### Exercises: tidyr ####
require(readxl)
require(tidyverse)

#### Ensure you have the data sets loaded into R as tidy tibbles ####
Suicides <- as_tibble(read_excel("Suicides.xlsx"))
Suicides_tidy <- Suicides %>% gather(GenderAge, Count, 4:ncol(Suicides)) %>% spread(Type, Count) %>% separate(GenderAge, c("Gender", "Age"), ":")

#### Exercises: dplyr ####
# using the tidy suicides data
#    calculate the number of suicides per year

Suicides_tidy %>% group_by(year) %>% summarise(Count=sum(suicides_no))

#    add a new column to the data which contains the rate of suicide per 100,000 population

Suicides_tidy <- Suicides_tidy %>% mutate(Rate=suicides_no*100000/population)

# read in the file "AMT_new.xlsx" and use the dplyr functions to calculate 
# the number of applications made in each region (column AdaptedNUTS)

AMT <- read_excel("AMT_new.xlsx")

AMT_counts <- AMT %>% group_by(AdaptedNUTS) %>% summarise(Count=n())