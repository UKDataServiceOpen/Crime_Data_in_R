#### Exercises: tidyr ####
require(readxl)
require(tidyverse)

#### Load the three different data sets into R using appropriate read in functions ####

drugs_siezures <- read.csv("drugs_siezures.csv")
drugs_volume <- read.csv("drugs_volume.csv")

crime_survey <- read.table("bcs_2007_8_teaching_data.tab", sep="\t", header=TRUE)
sexual_offences <- read_excel("SexualOffences1918_70_Data.xlsx")

#### Print one of them ####
print(drugs_siezures)
typeof(drugs_siezures)

#### Convert all three to tibbles ####
drugs_siezures <- as_tibble(drugs_siezures)
drugs_volume <- as_tibble(drugs_volume)

crime_survey <- as_tibble(crime_survey)
sexual_offences <- as_tibble(sexual_offences)

## Hint = you can do both of these in one step by using this format

drugs_siezures <- as_tibble(read.csv("drugs_siezures.csv"))
drugs_volume <- as_tibble(read.csv("drugs_volume.csv"))

crime_survey <- as_tibble(read.table("bcs_2007_8_teaching_data.tab", sep="\t", header=TRUE))
sexual_offences <- as_tibble(read_excel("SexualOffences1918_70_Data.xlsx"))

#### Extract a column as a value in 2 different ways ####
Mammal_name <-Mammals$name
Mammal_name <- Mammals[['name']]
Mammal_name <- Mammals[[1]]
Mammal_name <- Mammals[,1]

#### Extract a column as a variable, extract a row as a variable ####
Mammal_name <- Mammals[1]
Cheetah <- Mammals[1,]
