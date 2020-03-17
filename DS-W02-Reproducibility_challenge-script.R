library(tidyverse)
BOM_data <- read_csv("data/BOM_data.csv")
BOM_Stations <- read_csv("data/BOM_stations.csv")

# Question 1:
# For each station, how many days have a minimum temperature, 
# a maximum temperature and a rainfall measurement recorded?

BOM_data_sep <- BOM_data %>% 
  separate(Temp_min_max, into = c('min_temp', 'max_temp'), sep = "/")
BOM_data_sep

# filtering out the variables in the selected columns with a '-'.

BOM_filtered <- BOM_data_sep %>% 
  filter(min_temp != "-" & max_temp != "-", Rainfall != "-")

BOM_data_grouped_by_station <-  group_by(BOM_filtered, Station_number)

summarise(BOM_data_grouped_by_station, n_days = n())

# alternative solution to Q1 using pipe

Q1_ans <- filter(BOM_data_sep, min_temp != "-", max_temp != "-", Rainfall != "-") %>% 
  group_by(Station_number) %>% summarise(n_days = n())

Q1_ans

###################################