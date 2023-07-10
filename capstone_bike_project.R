setwd("C:/Kesava/R Files")
# Load all packages required
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(fasttime)
library(geosphere)

# get the files from the directory
directory <- "data_files/bike"
file_names <- list.files(path = directory, full.names = TRUE)

bike_data <- data.frame() #empty dataframe
for (file in file_names) {
  file.path <- file.path(directory, file)
  file_data <- read.csv(file.path)
  bike_data <- rbind(bike_data, file_data)
}

# check the data
str(bike_data) 

# Data Cleaning
# Convert start_dt and end_dt to datetime format using fasttime
bike_data$started_at <- as.POSIXct(bike_data$started_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
bike_data$ended_at <- as.POSIXct(bike_data$ended_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Create a new dataframe with selected columns (or removing columns)
trips_df <- bike_data
trips_df <- subset(trips_df, select = -c(ride_id,start_station_id,end_station_id))

# Check for null values column-wise in the bike_data dataframe
null_counts <- colSums(is.na(trips_df))
print(null_counts) # found start_station_id and end_station_id, to be dealt later

# check station name and lat, lag are the same, take any station name
subset_df <- subset(trips_df, end_station_name == "Kosciuszko Park") #found that its same as per data

# Find and replace missing end_lat,end_lag values in df
trips_df$end_lat[is.na(trips_df$end_lat)] <- na.omit(trips_df$end_lat)[match(trips_df$end_station_name[is.na(trips_df$end_lat)], trips_df$end_station_name)]
trips_df$end_lng[is.na(trips_df$end_lng)] <- na.omit(trips_df$end_lng)[match(trips_df$end_station_name[is.na(trips_df$end_lng)], trips_df$end_station_name)]


# Create trip_duration from started_at and ended_at and add to trips_df
trips_df$trip_duration <- trips_df$ended_at - trips_df$started_at
trips_df$trip_duration <- as.numeric(as.character(trips_df$trip_duration))

# Remove trip_duration where -ve or zero
trips_df <- trips_df %>% filter(trip_duration > 0)

# Create trip_distance from start_lat, start_lng & end_lat, end_lng
trips_df$trip_distance <- distHaversine(trips_df[,6:7], trips_df[,8:9])

# Extract Date, Month, Year, Hour & Weekday components from started_at
trips_df$Day <- as.numeric(format(trips_df$started_at, "%d"))
trips_df$Month <- month.abb[month(trips_df$started_at)]
trips_df$Hour <- hour(trips_df$started_at)
trips_df$Weekday <- weekdays(trips_df$started_at)

# Remove unnecessary columns and move the subset to new dataframe
data_df <- subset(trips_df, select = -c(started_at, ended_at, start_lat, start_lng, end_lat, end_lng))

# check data_df (the cleaned data for Analysis)
str(data_df)

# Analysis
summary(data_df)

# Analyze Customer types
customer_type <- table(data_df$member_casual)
print(customer_type)
customer_type_prop <- prop.table(customer_type)

# Create the pie chart
pie(customer_type_prop, labels = paste(names(customer_type), "(", format(customer_type_prop * 100, digits = 2), "%)", sep = ""), main = "Distribution of Customers Types", col = rainbow(length(customer_type)))
# Observation - 41% of the customers are Causal

# Calculate mean trip durations and ride counts by member type
mean_values <- aggregate(trip_duration ~ member_casual, data = data_df, FUN = mean)$trip_duration
ride_counts <- aggregate(trip_duration ~ member_casual, data = data_df, FUN = length)$trip_duration

# Set up the plotting layout with two plots side by side
par(mfrow = c(1, 2))

# Convert member_type to factor
data_df$member_casual <- factor(data_df$member_casual)

# Create the bar plot for mean trip durations
barplot(mean_values, col = "blue", ylim = c(0, max(mean_values) * 1.1),
        main = "Mean Trip Duration",
        xlab = "Member Type", ylab = "Trip Duration (Mean)",
        names.arg = levels(data_df$member_casual), las = 1)

barplot(ride_counts, col = "red", ylim = c(0, max(ride_counts) * 1.1),
        main = "Ride Counts",
        xlab = "Member Type", ylab = "Ride Count", las = 1,
        names.arg = levels(data_df$member_casual))

# Observation - Casual customers rides less, but trip duration is high, where is members trip duration is less and ride counts are higher

data_df %>%
  mutate(Weekday = ifelse(Weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday"), "Weekday", "Weekend")) %>%
  group_by(member_casual, Weekday) %>%
  summarize(rides = n(), mean_duration = mean(trip_duration)) %>%
  arrange(member_casual, Weekday) %>%
  ggplot(mapping = aes(x = Weekday, y = rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Ride Counts by Member Type and Day Type",
       x = "Day Type",
       y = "Ride Count",
       fill = "Member Type") +
  scale_x_discrete(labels = c("Weekday" = "Weekday", "Weekend" = "Weekend")) +
  theme_minimal()

# Observation - Casual customer rides less on weekdays, & members on weekdays

# Calculate ride counts by hour, day type, and member type
ride_counts <- data_df %>%
  mutate(DayType = ifelse(Weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")) %>%
  group_by(Hour, DayType, member_casual) %>%
  summarize(rides = n())

# Create the heatmap
ggplot(ride_counts, aes(x = Hour, y = DayType, fill = rides)) +
  geom_tile() +
  facet_grid(. ~ member_casual) +
  labs(title = "Ride Counts by Hour, Day Type, and Member Type",
       x = "Hour",
       y = "Day Type",
       fill = "Ride Count") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal()

# Observation - there is no pattern on Casual, looks like members are using on weekdays for commute to work & after work exercise?

ride_counts_month <- data_df %>%
  group_by(Month, member_casual) %>%
  summarize(total_rides = n())

# Create the line chart
ggplot(ride_counts_month, aes(x = Month, y = total_rides, color = member_casual, group = member_casual)) +
  geom_line() +
  labs(title = "Total Ride Counts by Month and Member Type",
       x = "Month",
       y = "Total Ride Count",
       color = "Member Type") +
  theme_minimal()

# Observation - looks like bike riding is seasonal element


# Calculate the number of rides for each starting station and calculate the percentage
ride_counts <- data_df %>%
  filter(start_station_name != "") %>%
  group_by(start_station_name) %>%
  summarise(ride_count = n()) %>%
  ungroup() %>%
  mutate(percentage = (ride_count / sum(ride_count)) * 100) %>%
  filter(percentage > 0.5) %>% 
  arrange(desc(percentage)) 


# Create a bar chart of the top N starting stations
ggplot(ride_counts, aes(x = start_station_name, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Starting Station") +
  ylab("Percentage of Bike Rides") +
  ggtitle(paste("Top", "Starting Stations with Highest Percentage of Bike Rides")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels for better readability
  annotate("text", x = 8, y = mean(ride_counts$percentage), label = paste("Top Stations =", nrow(ride_counts)),
           color = "red", angle = 45, hjust = 0)

# Observation - 37 out of 704 stations, identified more bike hiring, suggest run marketing in these stations



