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

#### Exercises: ggplot2 ####

## Create a scatterplot of rem sleep against total sleep in the mammals data set

p1 <- ggplot(Mammals_tidy) + geom_point(aes(sleep_total, sleep_rem))
p1

##   add the fitted regression line to this plot

# get fitted regression line first
lm1 <- lm(sleep_rem~sleep_total, data=Mammals_tidy)

p1+geom_abline(aes(intercept=lm1$coef[1], slope=lm1$coef[2]))

##    colour the points by the "vore" variable

ggplot(Mammals_tidy) + geom_point(aes(sleep_total, sleep_rem, colour=vore))

## Create a histogram of the rate variable that you created in the suicides data

ggplot(Suicides_tidy) + geom_histogram(aes(Rate)) # get a warning about bins - this is fine but you can pick a value

ggplot(Suicides_tidy) + geom_histogram(aes(Rate), bins=20) # no more warning

ggplot(Suicides_tidy) + geom_histogram(aes(Rate), bins=20, colour="white") # changes outline so you can see individual bars


## Plot the regions in the "mapdata" data

ggplot(mapdata) + geom_polygon(aes(long, lat, group=group)) # This looks a bit squashed
ggplot(mapdata) + geom_polygon(aes(long, lat, group=group)) + coord_fixed(1.3) # coord_fixed is the ratio of the y axis to the xaxis

##    colour these by the number of applications made

ggplot(data_plot) + geom_polygon(aes(long, lat, group=group, fill=Count)) + coord_fixed(1.3) # coord_fixed is the ratio of the y axis to the xaxis
# grey areas are those which have missing counts (i.e. no applications) you can change this if you want to!

data_plot$Count[is.na(data_plot$Count)] <- 0
ggplot(data_plot) + geom_polygon(aes(long, lat, group=group, fill=Count)) + coord_fixed(1.3) # coord_fixed is the ratio of the y axis to the xaxis

# you can also plot the locations from AMT_new on a map
ggplot(mapdata) + geom_polygon(aes(long, lat, group=group)) + coord_fixed(1.3) + geom_point(aes(x=Long, y=Lat), data=AMT, colour="red", size=0.5)

# additional argument size is used to change the size of the points. This can be attributed to some other variable to make a bubble plot
AMT_loc <- AMT %>% group_by(Long, Lat) %>% summarise(Count=n())

ggplot(mapdata) + geom_polygon(aes(long, lat, group=group)) + coord_fixed(1.3) + geom_point(aes(x=Long, y=Lat, size=Count, colour=Count), data=AMT_loc)

