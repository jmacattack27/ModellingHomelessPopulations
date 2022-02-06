#### Preamble ####
# Purpose: Clean the survey data downloaded from Open Data Toronto
# Author: Jack McKay
# Data: 4 February 2022
# Contact: jack.mckay@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: None

#### Workspace Set-Up ####
#install.packages("opendatatoronto")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("knitr")
#install.packages("janitor")
#install.packages("lubridate")
#install.packages("tibble")
#install.packages("dplyr")
#install.packages("scales")
#install.packages("data.table")
#install.packages("float")
library(haven)
library(opendatatoronto)
library(tidyverse)
library(tidyr)
library(knitr)
library(janitor)
library(lubridate)
library(tibble)
library(dplyr)
library(scales)
library(data.table)
library(float)


### Finding the dataset from Open Data Toronto ### 
populations <-
  list_package_resources("ac77f532-f18b-427c-905c-4ae87ce69c93") %>% 
  filter(name == "toronto-shelter-system-flow")  %>%  
  get_resource()
view(populations)


### Saving the Dataset ###

write_csv(populations, "inputs/data/raw_data.csv")
populations <- read.csv("inputs/data/raw_data.csv")

### Changing Dates to date objects ###

populations$date.mmm.yy. <- Map(paste, populations$date.mmm.yy., "-01", sep="")
populations$date.mmm.yy. <- strptime(populations$date.mmm.yy., "%b-%y-%d")
populations$date.mmm.yy. <- as.Date(populations$date.mmm.yy., format = "%Y-%m-%d")
names(populations)[2] <- "Date"

### Creating dataset for housing success rates ###

homeless_housing <-
  populations %>% 
  
  filter(population_group == 'All Population') %>%   # Filter data so that we only look at the full population for each month
  
  select(Date, returned_from_housing, moved_to_housing) %>% 
  
  mutate(Attrition = percent(1 - returned_from_housing / moved_to_housing)) %>% 
  
  rename("Returned from Housing" = returned_from_housing,   # Rename columns so that table reads nicer
         "Moved to Housing" = moved_to_housing)


### Creating dataset for population group and age groups ###

homeless_ages <-
  raw_data %>% 
  select(population_group, Month, ageunder16, age16.24, age25.44, age45.64, age65over) %>% 
  tidyr::gather("Age Group", "Population", 3:7)


### Creating dataset for the composition of homeless people (ie how much of the actively homeless population is comprised ###
### of each population group and graphing with line chart                                                                 ###

homeless_comp <-
  populations %>% 
  filter(population_group != "All Population") %>% 
  select(population_group, Date, actively_homeless, population_group_percentage)

homeless_comp %>% 
  ggplot(aes(x = Date, y = population_group_percentage, color = population_group, group = 2)) +
  geom_point() +
  geom_line(aes(group = population_group)) +
  labs(title = "Total Population Composition Over Since 2020") +
  ylab("Percentage of Total Population") +
  scale_y_continuous(breaks = pretty(homeless_comp$population_group_percentage, n = 10))


### Graphing homeless population based on age group and population group ###

homeless_ages <- 
  homeless_ages %>% 
  filter(Date == as.Date("2021-01-01")) %>% 
  filter(population_group != "All Population")

homeless_ages %>% 
  ggplot( aes(x = `Age Group`, y = Population, fill = population_group, label = Population)) +
  geom_col(stat="identity") +
  geom_text(stat = "identity", size = 2, position = position_stack(vjust = 0.5), check_overlap = TRUE)


### Graphing how homeless populations, grouped by age, fluctuate throughout the year ###

total_homeless <-
  homeless_ages %>% 
  filter(population_group == 'All Population')

total_homeless %>% 
  ggplot(aes(x = Date, y = Population, color = `Age Group`, group = 2)) +
  scale_x_date(date_labels = "%Y-%m") + 
  geom_point() +
  geom_line(aes(group = `Age Group`))


### Graphing the proportion of homeless people that were moved to housing against those who have returned from housing ###

graph_housing <-
  homeless_housing %>% 
  tidyr::gather("Moved to Housing vs Returned from Housing", "Population", 2:3, na.rm = FALSE, convert = FALSE) 

graph_housing %>% 
  ggplot(aes(x = Date, y = Population, color = `Moved to Housing vs Returned from Housing`, group = 2)) +
  geom_point() +
  geom_line(aes(group = `Moved to Housing vs Returned from Housing`))



         