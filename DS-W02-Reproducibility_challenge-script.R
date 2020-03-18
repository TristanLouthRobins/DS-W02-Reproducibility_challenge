library(tidyverse)
BOM_data <- read_csv("data/BOM_data.csv")
BOM_Stations <- read_csv("data/BOM_stations.csv")

# Q1:
# For each station, how many days have a minimum temperature, 
# a maximum temperature and a rainfall measurement recorded?

BOM_data_sep <- BOM_data %>% 
  separate(Temp_min_max, into = c('min_temp', 'max_temp'), sep = "/")
view(BOM_data_sep)
# define a new variable which separates the Temp_min_max value into
# separate columns.

BOM_filtered <- BOM_data_sep %>% 
  filter(min_temp != "-" & max_temp != "-", Rainfall != "-")
# a new variable defined to filter out the values in the min_temp 
# and max_temp columns which contain a '-'.

BOM_data_grouped_by_station <-  group_by(BOM_filtered, Station_number)
# a new variable defined to group the dataframe by the Station_number.

summarise(BOM_data_grouped_by_station, n_days = n())
# summarise the variable by number of days.

BOM_data_grouped_by_station

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
# define a new variable which separates the Temp_min_max value into
# separate columns.

Q2_ans <- BOM_separated %>% 
  mutate(temp_var = as.numeric(max_temp) - as.numeric(min_temp))  %>% 
  group_by(Month) %>% 
  summarise(temp_var = mean(temp_var, na.rm = TRUE)) %>% 
  arrange(temp_var)
# define a new variable which converts the data type for min_temp and max_temp
# to numeric data; grouping the dataframe by Month, summarising the dataframe
# by the avg temp_var; arranging this summary in ascending order.

Q2_ans

####################################

# Q3: Which state saw the lowest average daily temperature difference?

BOM_station_data <- BOM_Stations %>% 
  gather(Station_ID, Misc, -info) %>% 
  spread(info, Misc) 

BOM_station_data <- mutate(BOM_station_data, Station_ID = as.numeric(Station_ID))
# As the format of the Station_ID column data is formatted as characters, 
# the mutate and as.numeric function change the column data to a numeric format. 

BOM_station_rename <- rename(BOM_filtered, Station_ID = Station_number) 
# BOM_filtered is a filtered version of the BOM_data dataframe, 
# the rename function is used to ensure the Station column names 
# and format in both dataframes are the same.

BOM_merge_data <- full_join(BOM_station_rename, BOM_station_data)
# Using full join to merge both dataframes.

Q3_ans <- BOM_merge_data %>%
  mutate(temp_var = as.numeric(max_temp) - as.numeric(min_temp))  %>% 
  group_by(state) %>% 
  summarise(temp_var = mean(temp_var, na.rm = TRUE)) %>% 
  arrange(temp_var)

Q3_ans

# Using the same mutate function as Q2 to create a temperature difference column, 
# called 'temp_var' then using group_by to group the dataframe by 'state' 
# and then summarising a mean to determine the average temp for each state. 
# Lastly, the arrange funtion is used to display the state averages 
# in ascending order.

# Answer to Q3: QLD saw the lowest average daily temp difference.

######################################

# Q4: Does the westmost (lowest longitude) or eastmost (highest longitude) 
# weather station in our dataset have a higher average solar exposure?

Q4_ans <- BOM_merge_data %>% 
  filter(Solar_exposure != '-') %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>% 
  group_by(state, Station_ID, lon) %>% 
  summarise(Solar_exposure = mean(Solar_exposure)) %>% 
  arrange(lon) %>% 
  ungroup() %>% 
  slice(-2: -(n()-1)) 

Q4_ans

# Define a new variable, convert the character value for 
# Solar_exposure to numeric; group_by state, Station_ID and lon;
# summarise Solar_exposure by the mean Solar_exposure;
# arrange in ascending order.

# We now have an ascending summary of the mean Solar_exposure listed by 
# lowest to highest longitude. In order to remove everything but the lowest 
# to highest values, we will need to un_group and slice the tibble.
#
# ungroup() is used to remove the grouping function, because after summarising
# it is no longer required and will confuse the 'slice' function.
#
# slice omits everything from the second row to second to last row. 
# 'n' tells us the total number of rows in the table based upon the grouping function:
# e.g -2 removes second line through to second-to-last line: -(n()-1))

# Answer to Q4: Station 9194 (westmost) has a Solar exposure of 19.1; 
# Station 40043 (eastmost) has a Solar exposure of 19.6

# Alternative solution could use a second filter instead of the slice function.

Q4_ans_alt <- BOM_merge_data %>% 
  filter(Solar_exposure != '-') %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>% 
  group_by(state, Station_ID, lon) %>% 
  summarise(Solar_exposure = mean(Solar_exposure)) %>% 
  arrange(lon) %>% 
  ungroup() %>% 
  filter(lon == max(lon) | lon == min(lon))

Q4_ans_alt
