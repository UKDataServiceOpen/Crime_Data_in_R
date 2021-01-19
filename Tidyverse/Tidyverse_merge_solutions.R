#### Exercises: tidyr ####
require(readxl)
require(tidyverse)

#### Ensure you have the data sets loaded into R as tidy tibbles ####
Suicides_tb <- as_tibble(read_excel("Suicides.xlsx"))
Suicides_tidy <- Suicides_tb %>% gather(GenderAge, Count, 4:ncol(Suicides)) %>% spread(Type, Count) %>% separate(GenderAge, c("Gender", "Age"), ":")

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

#### Exercises: Merging data sets ####

# load the workspace "joining.RData" then using the join functions in dplyr:

load("joining.RData")

#    Find all of the rows in Expenditure which have a match in Production

semi_join(Expenditure, Production)

#    merge Employees and Production, keeping all rows in Production

left_join(Production, Employees)

# left_join and right_join essentially do the same thing 
# so this would give a similar result (with different column orderings)

right_join(Employees, Production)

#    merge Employees and Expenditure, keeping all rows

full_join(Employees, Expenditure)

#    Find the rows in Production which do not have a match in Employees

anti_join(Production, Employees)

# load the workspace "mapping.RData"
#    The object "mapping" contains shape information on all of the regions in the object "AMT_new". The object "link" links the two objects.
#    Use the join functions to merge the "mapping" object with the object of counts created during the last set of exercises

load("mapping.RData")

# the object of counts is AMT_counts
# first need to join the link file with either mapping or AMT_counts then join this table with the unlinked table
# this can be done all in one line of code:

data_plot <- mapdata %>% left_join(link, by="region") %>% left_join(AMT_counts, by="AdaptedNUTS") 

# I chose to link mapdata and link first as these both have more regions than the count data 550 compared to 355
