#used library#
library(tidyverse)
library(lubridate)
library(dplyr)
library(rmarkdown)
library(knitr)
library(skimr)
library(ggplot2)
library(naniar)

#import dataset#
aug22 <- read.csv("D:/BELAJAR/Coursera/Case Study 1/USE/202208-divvy-tripdata.csv", header = TRUE, sep = ",", na.strings = c(""))
sep22 <- read.csv("D:/BELAJAR/Coursera/Case Study 1/USE/202209-divvy-publictripdata.csv", header = TRUE, sep = ",", na.strings = c(""))
okt22 <- read.csv("D:/BELAJAR/Coursera/Case Study 1/USE/202210-divvy-tripdata.csv", header = TRUE, sep = ",", na.strings = c(""))
nov22 <- read.csv("D:/BELAJAR/Coursera/Case Study 1/USE/202211-divvy-tripdata.csv", header = TRUE, sep = ",", na.strings = c(""))
des22 <- read.csv("D:/BELAJAR/Coursera/Case Study 1/USE/202212-divvy-tripdata.csv", header = TRUE, sep = ",", na.strings = c(""))
jan23 <- read.csv("D:/BELAJAR/Coursera/Case Study 1/USE/202301-divvy-tripdata.csv", header = TRUE, sep = ",", na.strings = c(""))
feb23 <- read.csv("D:/BELAJAR/Coursera/Case Study 1/USE/202302-divvy-tripdata.csv", header = TRUE, sep = ",", na.strings = c(""))
mar23 <- read.csv("D:/BELAJAR/Coursera/Case Study 1/USE/202303-divvy-tripdata.csv", header = TRUE, sep = ",", na.strings = c(""))
apr23 <- read.csv("D:/BELAJAR/Coursera/Case Study 1/USE/202304-divvy-tripdata.csv", header = TRUE, sep = ",", na.strings = c(""))
may23 <- read.csv("D:/BELAJAR/Coursera/Case Study 1/USE/202305-divvy-tripdata.csv", header = TRUE, sep = ",", na.strings = c(""))
jun23 <- read.csv("D:/BELAJAR/Coursera/Case Study 1/USE/202306-divvy-tripdata.csv", header = TRUE, sep = ",", na.strings = c(""))
jul23 <- read.csv("D:/BELAJAR/Coursera/Case Study 1/USE/202307-divvy-tripdata.csv", header = TRUE, sep = ",", na.strings = c(""))

#combine datset#
cyclistics <- rbind(aug22, sep22, okt22, nov22, des22, jan23, feb23, mar23, apr23, may23, jun23, jul23)
View(cyclistics)

#Checking Data#
glimpse(cyclistics)
summary(cyclistics)

#Cleaning Data#
#1. Detecting Duplicate Data#
cyclistics_cleaned <- cyclistics %>%
  distinct()
nrow(cyclistics_cleaned)

#2. Detecting Missing Value#
miss_var_summary(cyclistics_cleaned)
#remove#
cyclistics_cleaned <- na.omit(cyclistics_cleaned)
#checking#
miss_var_summary(cyclistics_cleaned)

glimpse(cyclistics_cleaned)
head(cyclistics_cleaned, 2)
skim_without_charts(cyclistics_cleaned)

##Analyze##
#1. Rename column to make analyze easier#
cyclistics_cleaned <- rename(cyclistics_cleaned, "ride_type" = "rideable_type", "customer_type" = "member_casual")
head(cyclistics_cleaned, 2)

#2. Adding new column >> ride_length and day_of_week#
cyclistics_cleaned$ride_length <- round(as.numeric(difftime(cyclistics_cleaned$ended_at, cyclistics_cleaned$started_at, units = "mins")), 2)
cyclistics_cleaned$day_of_week <- wday(cyclistics_cleaned$started_at)
head(cyclistics_cleaned, 2)
##ride_length must be >0, so check it out##
any(cyclistics_cleaned$ride_length < 0)
which(cyclistics_cleaned$ride_length < 0)
#remove it#
cyclistics_cleaned <- subset(cyclistics_cleaned, ride_length > 0)

#3. Adding new column about time especially#
#but before that ensure that the data type is consistent#
cyclistics_cleaned$started_at <- ymd_hms(cyclistics_cleaned$started_at)
cyclistics_cleaned$ended_at <- ymd_hms(cyclistics_cleaned$ended_at)
cyclistics_cleaned$day_of_week <- format(as.Date(cyclistics_cleaned$day_of_week, origin = "2022-08-01"), "%A")

cyclistics_cleaned$month <- format(as.Date(cyclistics_cleaned$started_at), "%B %Y")
cyclistics_cleaned$day <- format(as.Date(cyclistics_cleaned$started_at), "%d")
cyclistics_cleaned$year <- format(as.Date(cyclistics_cleaned$started_at), "%Y")
cyclistics_cleaned$hour <- lubridate::hour(cyclistics_cleaned$started_at)
head(cyclistics_cleaned, 2)

##ANALYZE##
#1. Descriptive Analysis of Data#
summary(cyclistics_cleaned)

#2. Descriptive Analysis of Ride Length#
DA <- cyclistics_cleaned %>% 
  group_by(customer_type) %>% summarise("Average Ride Length" = mean(ride_length), "Median Ride Length" = median(ride_length), "Max Ride Length"= max(ride_length), "Min Ride Length" = min(ride_length))
View(DA)

#3. Calculating Total Rides (ride_id) by Customer Type#
numberofride <- cyclistics_cleaned %>% 
  group_by(customer_type) %>% 
  summarise("Total Rides" = length(ride_id), "Percentage of Total Rides" = ((length(ride_id) / nrow(cyclistics_cleaned)) * 100))
numberofride

ggplot(numberofride, aes(x = "", y = `Percentage of Total Rides`, fill = customer_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Pie Chart Total Rides by Customer Type", fill = "Customer Type") +
  geom_text(aes(label = paste0(round(`Percentage of Total Rides`, 2), "%")),
            position = position_stack(vjust = 0.5)) + 
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

#Total Rides by Customer Type and Bike Type#
cyclistics_cleaned %>% 
  group_by(customer_type, ride_type) %>% 
  summarise(total_rides = n()) %>% 
  ggplot(aes(x = customer_type, y = total_rides, fill = ride_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_rides), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Stacked Bar Chart of Rideable Type by Customer Type", x = "Customer Type", y = "Total Rides") +
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

#average#
cyclistics_cleaned %>%
  group_by(customer_type) %>%
  summarise(avg_ride_length = mean(ride_length)) %>%
  ggplot(aes(x = customer_type, y = avg_ride_length, fill = customer_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", avg_ride_length)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Average Ride Length by Customer Type", x = "Customer Type", y = "Average Ride Length") +
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

#4. Total Rides by Customer Type per Month#(Bulan Blm Urut)
cyclistics_cleaned %>%
  group_by(month, customer_type) %>%
  mutate(month = my(month)) %>% 
  summarise(total_rides = n()) %>%
  arrange(month) %>%
  ggplot(aes(x = month, y = total_rides, fill = customer_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = total_rides), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Total Rides by Customer Type per Month", x = "Month", y = "Total Rides") +
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

#line chart#blm urut bulannya
cyclistics_cleaned %>%
  group_by(month, customer_type) %>%
  mutate(month = my(month)) %>% 
  summarise(total_rides = n()) %>%
  arrange(month) %>%
  ggplot(aes(x = month, y = total_rides, color = customer_type, group = customer_type)) +
  geom_line() +
  geom_point() +  # Menambahkan titik untuk setiap data point
  geom_text(aes(label = total_rides), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Total Rides by Customer Type per Month", x = "Month", y = "Total Rides") +
  scale_color_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#average#
cyclistics_cleaned %>%
  group_by(month, customer_type) %>%
  mutate(month = my(month)) %>% 
  summarise(total_rides = n(), avg_ride_length = mean(ride_length)) %>%
  arrange(month) %>%
  ggplot(aes(x = month, y = avg_ride_length, fill = customer_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", avg_ride_length)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Average Ride Length by Customer Type per Month", x = "Month", y = "Average Ride Length") +
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#5. Total Rides by Customer Type per Day of Week#
cyclistics_cleaned %>%
  group_by(day_of_week, customer_type) %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  summarise(total_rides = n()) %>%
  ggplot(aes(x = day_of_week, y = total_rides, fill = customer_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = total_rides), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Total Rides by Customer Type per Day of Week", x = "Day of Week", y = "Total Rides") +
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

#line chart#
cyclistics_cleaned %>%
  group_by(day_of_week, customer_type) %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  summarise(total_rides = n()) %>%
  ggplot(aes(x = day_of_week, y = total_rides, color = customer_type, group = customer_type)) +
  geom_line() +
  geom_text(aes(label = total_rides), position = position_dodge(width = 0.9), vjust = -0.5) +
  geom_point() +  # Menambahkan titik untuk setiap data point
  labs(title = "Total Rides by Customer Type per Day of week", x = "Day of Week", y = "Total Rides") +
  scale_color_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#average#
cyclistics_cleaned %>%
  group_by(day_of_week, customer_type) %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  summarise(total_rides = n(), avg_ride_length = mean(ride_length)) %>%
  ggplot(aes(x = day_of_week, y = avg_ride_length, fill = customer_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", avg_ride_length)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Average Ride Length by Customer Type per Day of Week", x = "Day of Week", y = "Average Ride Length") +
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#6. Total Rides by Customer Type per Hour#
total_rides_per_hour <- cyclistics_cleaned %>%
  group_by(hour, customer_type) %>%
  summarise(total_rides = n()) 

View(total_rides_per_hour)

ggplot(total_rides_per_hour, aes(x = hour, y = total_rides, fill = customer_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Rides by Customer Type per Hour", x = "Hour", y = "Total Rides") +
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

#7. Top 10 Station#
#Start Station#
popular_start_station <- cyclistics_cleaned %>% 
  group_by(customer_type, start_station_name) %>% 
  summarize(total_rides = n()) %>% 
  arrange(desc(total_rides)) %>% 
  head(10)
View(popular_start_station)

popular_start_station %>%
  ggplot(aes(x = reorder(start_station_name, total_rides), y = total_rides, fill = customer_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_rides), position = position_dodge(width = 0.9), vjust = -0.5) +
  coord_flip() +
  labs(title = "Top 10 The Most Popular Start Station",
       x = "Station Name", 
       y = "Total Rides") +
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
       plot.subtitle = element_text(hjust = 0.5))

#most popular start station for member#
popular_start_station_member <- cyclistics_cleaned %>% 
  filter(customer_type == "member") %>% 
  group_by(start_station_name) %>%
  summarize(total_rides = n()) %>%
  arrange(desc(total_rides)) %>%
  head(10)
View(popular_start_station_member)

popular_start_station_member %>%
  ggplot(aes(x = reorder(start_station_name, total_rides), y = total_rides)) +
  geom_bar(stat = "identity", fill = "#FFA07A") +
  geom_text(aes(label = total_rides), position = position_dodge(width = 0.9), vjust = -0.5) +
  coord_flip() +
  labs(title = "Top 10 The Most Popolar Start Station by Customer Type ",
       subtitle = "Customer Type: Member",
       x = "Station Name",
       y = "Total Rides") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

#most popular end station for member#
popular_end_station_member <- cyclistics_cleaned %>% 
  filter(customer_type == "member") %>% 
  group_by(end_station_name) %>%
  summarize(total_rides = n()) %>%
  arrange(desc(total_rides)) %>%
  head(10)
View(popular_end_station_member)

popular_end_station_member %>%
  ggplot(aes(x = reorder(end_station_name, total_rides), y = total_rides)) +
  geom_bar(stat = "identity", fill = "#FFA07A") +
  geom_text(aes(label = total_rides), position = position_dodge(width = 0.9), vjust = -0.5) +
  coord_flip() +
  labs(title = "Top 10 The Most Popolar End Station by Customer Type ",
       subtitle = "Customer Type: Member",
       x = "Station Name",
       y = "Total Rides") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

#most popular start station by casual#
popular_start_station_casual <- cyclistics_cleaned %>% 
  filter(customer_type == "casual") %>% 
  group_by(start_station_name) %>%
  summarize(total_rides = n()) %>%
  arrange(desc(total_rides)) %>%
  head(10)
View(popular_start_station_casual)

popular_start_station_casual %>%
  ggplot(aes(x = reorder(start_station_name, total_rides), y = total_rides)) +
  geom_bar(stat = "identity", fill = "#FFA07A") +
  geom_text(aes(label = total_rides), position = position_dodge(width = 0.9), vjust = -0.5) +
  coord_flip() +
  labs(title = "Top 10 The Most Popolar Start Station by Customer Type ",
       subtitle = "Customer Type: Casual",
       x = "Station Name",
       y = "Total Rides") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

#most popular end station for casual#
popular_end_station_casual <- cyclistics_cleaned %>% 
  filter(customer_type == "casual") %>% 
  group_by(end_station_name) %>%
  summarize(total_rides = n()) %>%
  arrange(desc(total_rides)) %>%
  head(10)
View(popular_end_station_casual)

popular_end_station_casual %>%
  ggplot(aes(x = reorder(end_station_name, total_rides), y = total_rides)) +
  geom_bar(stat = "identity", fill = "#FFA07A") +
  geom_text(aes(label = total_rides), position = position_dodge(width = 0.9), vjust = -0.5) +
  coord_flip() +
  labs(title = "Top 10 The Most Popolar End Station by Customer Type ",
       subtitle = "Customer Type: Casual",
       x = "Station Name",
       y = "Total Rides") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

write.csv(cyclistics_cleaned, file = "D:/BELAJAR/Coursera/Case Study 1/USE/cyclistics.csv", row.names = FALSE)
