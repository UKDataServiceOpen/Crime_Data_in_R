#### Exercises: tidyr ####
require(readxl)
require(tidyverse)

#### Ensure you have the 3 data sets loaded into R as tibbles ####
Mammals <- as_tibble(read.csv("Mammals.csv"))
population_data <- as_tibble(read.table("Population_data.txt", header=TRUE))
Suicides <- as_tibble(read_excel("Suicides.xlsx"))

#### Make each tibble tidy as needed ####

# The mammals data have multiple observations per cell in the orderGenus column
# Need to use separate()

Mammals_tidy <- Mammals %>% separate(orderGenus, c("Order","Genus"), sep=',')

# The population data have multiple observations in one row
# need to use gather()

population_data_tidy <- population_data %>% gather(Year, Population, Y2001:Y2013)

# If you wanted to get rid of the "Y" in the year column you could additionally use mutate()

population_data_tidy <- population_data %>% gather(Year, Population, Y2001:Y2013) %>% mutate(Year=as.numeric(substr(Year, 2, nchar(Year))))


# The Suicides data have all of the tidy issues. 
# Population and suicides_no should be columns
# The gender and age values which are currently column headings should be rows
# The gender and age values should each have their own cell

Suicides_tidy <- Suicides %>% gather(GenderAge, Count, 4:ncol(Suicides)) %>% spread(Type, Count) %>% separate(GenderAge, c("Gender", "Age"), ":")
