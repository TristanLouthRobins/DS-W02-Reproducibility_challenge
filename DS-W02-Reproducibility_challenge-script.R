library(tidyverse)
BOM_data <- read_csv("data/BOM_data.csv")
BOM_Stations <- read_csv("data/BOM_stations.csv")

# Q1:
# For each station, how many days have a minimum temperature, 
# a maximum temperature and a rainfall measurement recorded?

BOM_data_sep <- BOM_data %>% 
  separate(Temp_min_max, into = c('min_temp', 'max_temp'), sep = "/")
view(BOM_data_sep)

# filtering out the variables in the selected columns with a '-'.

BOM_filtered <- BOM_data_sep %>% 
  filter(min_temp != "-" & max_temp != "-", Rainfall != "-")

BOM_data_grouped_by_station <-  group_by(BOM_filtered, Station_number)

summarise(BOM_data_grouped_by_station, n_days = n())

# alternative solution to Q1 using pipe

BOM_data_sep <- BOM_data %>% 
  separate(Temp_min_max, into = c('min_temp', 'max_temp'), sep = "/")

  Q1_ans <- filter(BOM_data_sep, min_temp != "-", max_temp != "-", Rainfall != "-") %>% 
  group_by(Station_number) %>% summarise(n_days = n())

Q1_ans

###################################

# Q2: Which month saw the lowest average daily temperature difference?

BOM_separated <- BOM_data %>% 
  separate(Temp_min_max, into = c('min_temp', 'max_temp'), sep = "/")

BOM_temp_diff <- BOM_separated %>% 
  mutate(temp_var = as.numeric(max_temp) - as.numeric(min_temp))  %>% 
  group_by(Month) %>% 
  summarise(temp_var = mean(temp_var, na.rm = TRUE)) %>% 
  arrange(temp_var)

view(BOM_temp_diff)

####################################

# Q3: Which state saw the lowest average daily temperature difference?

BOM_station_data <- BOM_Stations %>% 
  gather(Station_ID, Misc, -info) %>% 
  spread(info, Misc) 

BOM_station_data <- mutate(BOM_station_data, Station_ID = as.numeric(Station_ID))
# Since the format of the Station_ID column data is formatted as characters, this mutate 
# and as.numeric function change the column data to a numeric format. 

BOM_station_rename <- rename(BOM_filtered, Station_ID = Station_number) 
# BOM_filtered is a filtered version of the BOM_data tibble, the rename function is 
# used to ensure the Station column names and format in both tibbles are the same.

BOM_merge_data <- full_join(BOM_station_rename, BOM_station_data)
# Using full join to merge both tibbles.

BOM_state_avg_temp <- BOM_merge_data %>%
  mutate(temp_var = as.numeric(max_temp) - as.numeric(min_temp))  %>% 
  group_by(state) %>% 
  summarise(temp_var = mean(temp_var, na.rm = TRUE)) %>% 
  arrange(temp_var)
# Using the same mutate function as Q2 to create a temperature difference column, 
# called 'temp_var' then using group_by to group the tibble by 'state' and then 
# summarising a mean to determine the average temp for each state. 
# Lastly, the arrange funtion is used to display the state averages in ascending order.




