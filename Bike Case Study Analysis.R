install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)


Divvy_Trips_2019_Q1 <- read.csv("C:/Users/HP/Downloads/Divvy_Trips_2019_Q1.csv")
View(Divvy_Trips_2019_Q1)
Divvy_Trips_2020_Q1 <- read_csv("C:/Users/HP/Downloads/Divvy_Trips_2020_Q1.csv")
View(Divvy_Trips_2020_Q1)

# Inspecting columns' names.

colnames(Divvy_Trips_2019_Q1)
colnames(Divvy_Trips_2020_Q1)

# changing column names for consistency. 

library(dplyr)
Divvy_Trips_2020_Q1 <- data_renamed <- rename(Divvy_Trips_2020_Q1, 
                                              trip_id = ride_id, bikeid = rideable_type, 
                                              start_time = started_at, end_time = ended_at,
                                              from_station_name = start_station_name,
                                              from_station_id = start_station_id,
                                              to_station_name = end_station_name, 
                                              to_station_id = end_station_id, 
                                              usertype = member_casual)

# Inspecting for inconsistencies

colnames(Divvy_Trips_2019_Q1)
colnames(Divvy_Trips_2020_Q1)

# Data type consistency for data manipulation

glimpse(Divvy_Trips_2019_Q1)
glimpse(Divvy_Trips_2020_Q1)

rider_id_chr <-Divvy_Trips_2019_Q1 <- Divvy_Trips_2019_Q1 %>% mutate(Divvy_Trips_2019_Q1, 
                                                                     trip_id = as.character(trip_id))
# Dropping columns I won't use was a standing idea, but I changed my mind here :p
# We need to change data in the user type column content-wise.
Divvy_Trips_2019_Q1 <- Divvy_Trips_2019_Q1 %>% mutate(usertype = recode(usertype,
                                                                        "Subscriber" = "member",
                                                                        "Customer" = "casual"))

#Calculating trip duration for 2020 Q1 and converting data from chr to dttm.
glimpse(Divvy_Trips_2020_Q1)
Divvy_Trips_2020_Q1 <- Divvy_Trips_2020_Q1 %>% 
  mutate(across(c(start_time, end_time),
                ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%S")))
glimpse(Divvy_Trips_2020_Q1) 

Divvy_Trips_2020_Q1 <- Divvy_Trips_2020_Q1 %>% mutate (tripduration = 
                                                         as.numeric(difftime
                                                                    (end_time,start_time, units = "mins")))

#Converting 2019 data from chr to dttm

Divvy_Trips_2019_Q1 <- Divvy_Trips_2019_Q1 %>% 
  mutate(across(c(start_time, end_time),
                ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%S")))
Divvy_Trips_2019_Q1 <- Divvy_Trips_2019_Q1 %>% mutate(tripduration = as.numeric(tripduration))

class(Divvy_Trips_2019_Q1$tripduration)                                                 
class(Divvy_Trips_2020_Q1$tripduration)

#Significant difference in trip duration appears in the two datasets, so I ran a few functions to validate

Divvy_Trips_2019_Q1 %>% select(start_time, end_time, tripduration) %>% slice(50:55)
Divvy_Trips_2020_Q1 %>% select(start_time, end_time, tripduration) %>% slice(50:55)

#Converting time from seconds to minutes because I want to use correlation and visualization for analysis

Divvy_Trips_2019_Q1 <-Divvy_Trips_2019_Q1 %>% mutate(tripduration = tripduration/60)

#I ran initial analysis and it showed negative outputs, so I filtered them

#Results showed negative value for min
Divvy_Trips_2020_Q1 <- Divvy_Trips_2020_Q1 %>%
  mutate(tripduration = as.numeric(difftime(end_time, start_time, units = "mins"))) %>%
  filter(tripduration >= 0)

#Descriptive Analysis

summary_2019 <-Divvy_Trips_2019_Q1 %>% summarise(min_duration = 
                                                 min(tripduration, na.rm = TRUE), max_duration = max(tripduration, na.rm = TRUE),
                                                 mean_duration = mean(tripduration, na.rm = TRUE), 
                                                 median_duration = median(tripduration, na.rm = TRUE))
print(summary_2019)                                            

summary_2020 <- Divvy_Trips_2020_Q1 %>% summarise(min_duration = 
                                                  min(tripduration, na.rm = TRUE), max_duration = max(tripduration, na.rm = TRUE),
                                                  mean_duration = mean(tripduration, na.rm = TRUE),
                                                  median_duration = median(tripduration, na.rm = TRUE))
print(summary_2020)

#Finding Correlations - At first, I chose correlations over aggregation, but then I realized the I have to use aggregation as the foundation (more on this in the Notion documentation)

#Average trip duration/user type

aggregate(tripduration ~ usertype, data = Divvy_Trips_2019_Q1, FUN = mean)
aggregate(tripduration ~ usertype, data = Divvy_Trips_2020_Q1, FUN = mean)

#Found a big increase in average riding minutes for casual riders, so I had to run additional functions

str(Divvy_Trips_2020_Q1$tripduration)
Divvy_Trips_2020_Q1$tripduration <- as.numeric(difftime(Divvy_Trips_2020_Q1$end_time, 
                                                        Divvy_Trips_2020_Q1$start_time, 
                                                        units = "mins"))
summary(Divvy_Trips_2020_Q1)
Divvy_Trips_2020_Q1 <- Divvy_Trips_2020_Q1 %>% filter(tripduration > 0 & tripduration < 300)
library(ggplot2)
ggplot(Divvy_Trips_2020_Q1, aes(x = tripduration)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "purple") +
  facet_wrap(~ usertype) +
  labs(title = "Trip Duration Distribution by User Type (in minutes)",
       x = "Trip Duration (minutes)",
       y = "Count") +
  xlim(0, 200) 