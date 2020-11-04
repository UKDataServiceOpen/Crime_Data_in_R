#### Exercises: tidyr ####
require(readxl)
require(tidyverse)

#### Load the three different data sets into R using appropriate read in functions ####
Mammals <- read.csv("Mammals.csv")
population_data <- read.table("Population_data.txt", header=TRUE)
Suicides <- read_excel("Suicides.xlsx")

#### Print one of them ####
print(Mammals)
typeof(Mammals)

#### Convert all three to tibbles ####
Mammals <- as_tibble(Mammals)
population_data <- as_tibble(population_data)
Suicides <- as_tibble(Suicides)


## Hint = you can do both of these in one step by using this format
Mammals <- as_tibble(read.csv("Mammals.csv"))

#### Extract a column as a value in 2 different ways ####
Mammal_name <-Mammals$name
Mammal_name <- Mammals[['name']]
Mammal_name <- Mammals[[1]]
Mammal_name <- Mammals[,1]

#### Extract a column as a variable, extract a row as a variable ####
Mammal_name <- Mammals[1]
Cheetah <- Mammals[1,]
