library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(conflicted)

conflict_prefer("filter", "dplyr")#selecting prefernces for package conflcts
conflict_prefer("lag", "dplyr")

q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")#uploading csv files and renaming
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

colnames(q1_2019)#checking column names for differences
colnames(q1_2020)

(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))#renaming 2019 columns to match 2020 column names

colnames(q1_2019)#verifying changes
colnames(q1_2020)

str(q1_2019)#checking datatypes of columns against each other
str(q1_2020)

q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))#changing datatype to match for matching columns

all_trips <- bind_rows(q1_2019, q1_2020)#combining 2019 & 2020 dataframes

all_trips_v2 <- all_trips %>%
  select(-c(tripduration, gender, birthyear, start_lat, start_lng, end_lat, end_lng))#created updated dataframe for the removal of unnecessary columns

colnames(all_trips_v2)

dim(all_trips_v2)
head(all_trips_v2)
tail(all_trips_v2)
str(all_trips_v2)
summary(all_trips_v2)#checking dimensions and values of new dataframe, as well as datatypes and summary overview of dataframe

table(all_trips_v2$member_casual)#get sum of unique values of member, casual, Customer, and Subscriber

all_trips_v3 <- all_trips_v2 %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))#created updated dataframe, unified names for types of customers in member_casual column

table(all_trips_v3$member_casual)#verifying change to member_casual

all_trips_v3$date <- as.Date(all_trips_v3$started_at)#convert started_at to date format and created new date column
all_trips_v3$month <- format(as.Date(all_trips_v3$date), "%m")
all_trips_v3$day <- format(as.Date(all_trips_v3$date), "%d")
all_trips_v3$year <- format(as.Date(all_trips_v3$date), "%Y")
all_trips_v3$day_of_week <- format(as.Date(all_trips_v3$date), "%A")#extracted month, day, year, and day of the week from date column

all_trips_v3$ride_length <- difftime(all_trips_v3$ended_at, all_trips_v3$started_at)#calculated ride length using difftime function, output is in seconds

str(all_trips_v3)#verify all changes

is.factor(all_trips_v3$ride_length)
all_trips_v3$ride_length <- as.numeric(as.character(all_trips_v3$ride_length))
is.numeric(all_trips_v3$ride_length)#verify and convert datatype of ride_length to numeric for data manipulation

all_trips_v4 <- all_trips_v3[!(all_trips_v3$start_station_name == "HQ QR" | all_trips_v3$ride_length<0),]#excluded certain starting stations, and negative values of ride_length

summary(all_trips_v4$ride_length)#summary overview of ride_length

aggregate(all_trips_v4$ride_length ~ all_trips_v4$member_casual, FUN = mean)
aggregate(all_trips_v4$ride_length ~ all_trips_v4$member_casual, FUN = median)
aggregate(all_trips_v4$ride_length ~ all_trips_v4$member_casual, FUN = max)
aggregate(all_trips_v4$ride_length ~ all_trips_v4$member_casual, FUN = min)#aggregate ride_length with respect to member type
aggregate(all_trips_v4$ride_length ~ all_trips_v4$member_casual + all_trips_v4$day_of_week, FUN = mean)#aggregate mean of ride_length with respect to member type and day_of_week

all_trips_v4$day_of_week <- ordered(all_trips_v4$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))#ordered day_of_week

all_trips_v5 <- all_trips_v4 %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)#grouped new dataframe by member type and day of week, added number of rides and avg ride duration

head(all_trips_v5)#verify new dataframe

write.csv(all_trips_v5, "2019_2020combined.csv", row.names = FALSE)#convert dataframe to csv for further use



