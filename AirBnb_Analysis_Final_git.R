# Clearing the environment
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session


# Loading the pacman and tidyverse libraries
library(pacman)
p_load(tidyverse)
library(ggplot2)
library(dplyr)
library(leaflet)
library(sf)
library(shiny)
library(leaflet.extras)
library(here)
library(corrplot)
library(ggpubr)


listings <- read_csv("listings.csv")


# Selecting the necessary columns
listings_df <- select(listings, c(name, latitude, longitude, room_type, price,
                                  minimum_nights, availability_365, neighbourhood))


# Descriptive summary of the whole dataset
summary(listings_df)


# Splitting the name column into different columns
listings_df$name <- strsplit(listings_df$name, " . ")
listings_df

listings_df <- unnest_wider(listings_df, name, names_sep="_")
listings_df


# Renaming the new columns
names(listings_df)[c(1,2,3,4,5)] <- c("listing","rating","bedrooms","beds","baths")


listings_df <- select(listings_df, -c(name_6))


# Checking for null values
sapply(listings_df, anyNA)


# Dropping the null values in these three columns
listings_df <- drop_na(listings_df, bedrooms,beds,baths)


# Checking the unique values in bedrooms, beds and baths
unique(listings_df$bedrooms)
unique(listings_df$beds)
unique(listings_df$baths)


# Removing records with ratings as number of bedrooms
listings_df <- filter(listings_df, bedrooms!= "★4.88")


# Function to extract numbers from string for bedrooms, beds and bath columns
cleaning <- function(value) {
  if(value=="Studio") {
    return(0) }
  else if(grepl("half-bath", value, ignore.case = TRUE)) {    # checks if the string has "half-bath" as substring
    return(0.5)
  }
  else {
    return(as.numeric(str_extract(value, "\\d+")))
  }
}

listings_df$bedrooms <- sapply(listings_df$bedrooms, cleaning)
listings_df$beds <- sapply(listings_df$beds, cleaning)
listings_df$baths <- sapply(listings_df$baths, cleaning)

listings_df$rating <- replace(listings_df$rating, TRUE, gsub("★", "", listings_df$rating))


# checking data types
str(listings_df)


# Changing data type of rating from charater to numeric
unique(listings_df$rating)


# After finding the unique values in rating column some cells consist of rating value "New".
# Thus filtering the data accordingly and changing the data type of rating
listings_df <- filter(listings_df, rating != "New")
listings_df$rating <- as.numeric(listings_df$rating)


# Bar plot to show distribution of room types 
ggplot(listings_df, aes(x = room_type, fill = room_type)) +
  geom_bar() +
  labs(x = "Room type", 
       y = "Total number of listings", 
       title = "Listings distribution of different room types in Toronto") +
  theme(legend.position = "right")


# Subsetting the data frame to have relevant columns for showing correlation
selected_columns <- select(listings_df, c("bedrooms", "baths", "beds", "price", "rating"))


# Computing the correlation matrix
cor_matrix <- cor(selected_columns)

# Plot the correlation matrix
corrplot(cor_matrix, method = "number")


# Creating a boxplot for price column per room type 
ggplot(listings_df, aes(x=room_type, y=price, fill=room_type)) +
  geom_boxplot() +
  labs(title = "Boxplot of Prices for each Room Type",
       x="Room Type",
       y="Price",
       fill = "Room Type") +  
  scale_fill_brewer(palette="Set1")  


# Since the price column has large outliers, we use  Interquartile Range (IQR)
# method to remove them. The IQR method defines an outlier as a value that falls
# below the 1st Quartile minus 1.5 times the IQR or above the 3rd Quartile plus
# 1.5 times the IQR.
Q1 <- quantile(listings_df$price, 0.25)
Q3 <- quantile(listings_df$price, 0.75)
IQR <- Q3 - Q1


# Define the lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR


# Filter out outliers
listings_df <- listings_df[listings_df$price >= lower_bound & listings_df$price <= upper_bound, ]

ggplot(listings_df, aes(x=room_type, y=price, fill=room_type)) +
  geom_boxplot() +
  labs(title = "New Boxplot of Prices for each Room Type",
       y = "Room Type", x = "Price", fill = "Room Type") +  
  scale_fill_brewer(palette="Set1") +
  theme_minimal() +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="blue") +
  stat_summary(
    fun=median,
    geom="text",
    aes(label=..y..),
    position=position_dodge(width=0.75),
    vjust=-0.5,
    hjust= 1.2
  )


# Create the leaflet map 
# Selecting the necessary columns
geo <- listings_df[c("longitude", "latitude", "neighbourhood", "price")]

# Creating the label
label <- paste("Name: ", geo$neighbourhood, "<br>", "Price: $", geo$price)
label <- lapply(label, HTML)

# Building the leaflet map
map <- leaflet(data = geo)
map <- addTiles(map)
map <- addMarkers(map, clusterOptions = markerClusterOptions(), data = geo, label = label)

map

#----------------------------------Milestone2-----------------------------------

listings<-read.csv("listings.csv")


listings1<- select(listings, name, latitude,longitude,room_type,price,minimum_nights,availability_365,neighbourhood,calculated_host_listings_count)

listings1$name <- strsplit(listings1$name, " . ")
listings1

listings_df <- unnest_wider(listings1, name, names_sep="_")
listings_df

names(listings_df)[c(1,2,3,4,5)] <- c("listing","rating","bedrooms","beds","baths")


listings_df <- select(listings_df, -c(name_6))


# Checking for null values
sapply(listings_df, anyNA)


# Dropping the null values in these three columns
listings_df <- drop_na(listings_df, bedrooms,beds,baths)


cleaning <- function(value) {
  if(value=="Studio") {
    return(0) }
  else if(grepl("half-bath", value, ignore.case = TRUE)) {    # checks if the string has "half-bath" as substring
    return(0.5)
  }
  else {
    return(as.numeric(str_extract(value, "\\d+")))
  }
}

listings_df$bedrooms <- sapply(listings_df$bedrooms, cleaning)
listings_df$beds <- sapply(listings_df$beds, cleaning)
listings_df$baths <- sapply(listings_df$baths, cleaning)

listings_df$rating <- replace(listings_df$rating, TRUE, gsub("★", "", listings_df$rating))

unique(listings_df$rating)

listings_df <- drop_na(listings_df)

listings_df <- filter(listings_df, rating != "New")
listings_df$rating <- as.numeric(listings_df$rating)



vanlistings<-read.csv("van-listings.csv")
listings<-read.csv("listings.csv")

vanlistings<- select(vanlistings, name, latitude,longitude,room_type,price,minimum_nights,availability_365,neighbourhood,calculated_host_listings_count)


vanlistings$name <- strsplit(vanlistings$name, " . ")
vanlistings

listings_df1 <- unnest_wider(vanlistings, name, names_sep="_")
listings_df1

names(listings_df1)[c(1,2,3,4,5)] <- c("listing","rating","bedrooms","beds","baths")



# Checking for null values
sapply(listings_df1, anyNA)


# Dropping the null values in these three columns
listings_df1 <-  drop_na(listings_df1, bedrooms,beds,baths)

cleaning <- function(value) {
  if(value=="Studio") {
    return(0) }
  else if(grepl("half-bath", value, ignore.case = TRUE)) {    # checks if the string has "half-bath" as substring
    return(0.5)
  }
  else {
    return(as.numeric(str_extract(value, "\\d+")))
  }
}

listings_df1$bedrooms <- sapply(listings_df1$bedrooms, cleaning)
listings_df1$beds <- sapply(listings_df1$beds, cleaning)
listings_df1$baths <- sapply(listings_df1$baths, cleaning)

listings_df1$rating <- replace(listings_df1$rating, TRUE, gsub("★", "", listings_df1$rating))

unique(listings_df1$rating)

listings_df1 <-  drop_na(listings_df1)

listings_df1 <-  filter(listings_df1, rating != "New")
listings_df1$rating <- as.numeric(listings_df1$rating)

# Define longitude and latitude ranges for downtown Toronto
min_longitude <- -79.3872
max_longitude <- -79.3832
min_latitude <- 43.6426
max_latitude <- 43.6511

# Filter the data
downtown_listings <-
  filter(listings_df,
         longitude >= min_longitude,
         longitude <= max_longitude,
         latitude >= min_latitude,
         latitude <= max_latitude
  )

downtown_listings <- filter(listings_df, longitude >= min_longitude,
                            longitude <= max_longitude,
                            latitude >= min_latitude,
                            latitude <= max_latitude)
downtown_listings

min_longitude_vancouver <- -123.1246
max_longitude_vancouver <- -123.1170
min_latitude_vancouver <- 49.2806
max_latitude_vancouver <- 49.2906

dt_van_listings <-
  filter(listings_df1,
         longitude >= min_longitude_vancouver,
         longitude <= max_longitude_vancouver,
         latitude >= min_latitude_vancouver,
         latitude <= max_latitude_vancouver
  )


# ---------------------------DM--------------------------
## Q1
# Null Hypothesis: The average price of listings in Toronto downtown and vancouver downtown has no significant difference.
# Alternate Hypothesis:  The average price of listings in Toronto downtown and vancouver downtown are different.

price_vancouver <- pull(dt_van_listings, price)
price_toronto <- pull(downtown_listings, price)


test_price <- t.test(price_vancouver, price_toronto)
test_price

price_data <- rbind(transform(dt_van_listings, city = "Vancouver"),
                    transform(downtown_listings, city = "Toronto"))

ggboxplot(price_data, x = "city", y = "price", 
          color = "city", palette = c("#00AFBB", "#E7B800"),
          ylab = "Price", xlab = "City") +
  stat_compare_means(aes(label = ..p.format..), label.y = max(price_data$price)) +
  coord_cartesian(ylim = c(0, 400))

# ---------------------------------QS------------------------------------
## Q2

# Null Hypothesis (H0): There is no significant difference in rating between downtown Toronto and downtown Vancouver Airbnb.
# Alternative Hypothesis (H1): There is a significant difference in rating between the two groups.

t_test_result <- t.test(dt_van_listings$rating, downtown_listings$rating, alternative = "two.sided")
t_test_result

combined_data <- rbind(transform(dt_van_listings, city = "Vancouver"),
                       transform(downtown_listings, city = "Toronto"))

ggboxplot(combined_data, x = "city", y = "rating", 
          color = "city", palette = c("#00AFBB", "#E7B800"),
          ylab = "Rating", xlab = "City") +
  stat_compare_means(aes(label = ..p.format..), label.y = max(combined_data$rating))

# -------------------------------RG----------------------------------
## Q3

listings <- read_csv("listings.csv")


# Selecting the necessary columns
listings_df <- select(listings, c(name, latitude, longitude, room_type, price,
                                  minimum_nights, availability_365, neighbourhood))

# Splitting the name column into different columns
listings_df$name <- strsplit(listings_df$name, " . ")
listings_df

listings_df <- unnest_wider(listings_df, name, names_sep="_")
listings_df


# Renaming the new columns
names(listings_df)[c(1,2,3,4,5)] <- c("listing","rating","bedrooms","beds","baths")


listings_df <- select(listings_df, -c(name_6))


# Checking for null values
sapply(listings_df, anyNA)


# Dropping the null values in these three columns
listings_df <- drop_na(listings_df, bedrooms,beds,baths)


# Checking the unique values in bedrooms, beds and baths
unique(listings_df$bedrooms)
unique(listings_df$beds)
unique(listings_df$baths)


# Removing records with ratings as number of bedrooms
listings_df <- filter(listings_df, bedrooms!= "★4.88")


# Function to extract numbers from string for bedrooms, beds and bath columns
cleaning <- function(value) {
  if(value=="Studio") {
    return(0) }
  else if(grepl("half-bath", value, ignore.case = TRUE)) {    # checks if the string has "half-bath" as substring
    return(0.5)
  }
  else {
    return(as.numeric(str_extract(value, "\\d+")))
  }
}

listings_df$bedrooms <- sapply(listings_df$bedrooms, cleaning)
listings_df$beds <- sapply(listings_df$beds, cleaning)
listings_df$baths <- sapply(listings_df$baths, cleaning)

listings_df$rating <- replace(listings_df$rating, TRUE, gsub("★", "", listings_df$rating))

# Changing data type of rating from charater to numeric
unique(listings_df$rating)


# After finding the unique values in rating column some cells consist of rating value "New".
# Thus filtering the data accordingly and changing the data type of rating
listings_df <- filter(listings_df, rating != "New")
listings_df$rating <- as.numeric(listings_df$rating)

# Removing outliers
Q1 <- quantile(listings_df$price, 0.25)
Q3 <- quantile(listings_df$price, 0.75)
IQR <- Q3 - Q1

# Define the lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter out outliers
listings_df2 <- listings_df[listings_df$price >= lower_bound & listings_df$price <= upper_bound, ]

# Function to calculate distance using Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Earth radius in kilometers
  R <- 6371
  
  # Convert degrees to radians
  lat1 <- lat1 * (pi/180)
  lon1 <- lon1 * (pi/180)
  lat2 <- lat2 * (pi/180)
  lon2 <- lon2 * (pi/180)
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- R * c
  
  return(distance)
}
# Latitude and longitude of city center of Toronto
lat_center <- 43.6532
lon_center <- -79.3832

listings_df2 <-  mutate(listings_df2, 
                        distance_from_center = haversine_distance(latitude, 
                                                                  longitude, 
                                                                  lat_center, 
                                                                  lon_center))

# median of the distnace from city center
median_split_point <- median(listings_df2$distance_from_center)

# Perform two-sample t-test on the 'price' column

data <- mutate(listings_df2, group = ifelse(distance_from_center > median_split_point,
                                            "more than 4.7", "less or equal 4.7"))

t_test_result <- t.test(price ~ group, data)

t_test_df1 <- data.frame(
  t_statistic = t_test_result$statistic,
  df = t_test_result$parameter,
  p_value = t_test_result$p.value,
  confidence_interval_lower = t_test_result$conf.int[1],
  confidence_interval_upper = t_test_result$conf.int[2],
  mean_difference = t_test_result$estimate[1] - t_test_result$estimate[2],
  estimate_group1 = t_test_result$estimate[1],
  estimate_group2 = t_test_result$estimate[2])

# Print the results of the t-test
print(t_test_df1)

#Visualizing the t test
data$group <- as.factor(data$group)

# Creating a boxplot
ggboxplot(data, x = "group", y = "price", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Price", xlab = "Distance from Center Group") +
  stat_compare_means(aes(label = ..p.format..), label.y = max(data$price))

#########################
# CN Tower as landmark and one sample t test on 1 km walking distance from it

lat_CN_tower <- 43.6426
lon_CN_tower <- -79.3871

listings_df2 <-  mutate(listings_df2, 
                        distance_from_CN = haversine_distance(latitude, 
                                                              longitude, 
                                                              lat_CN_tower, 
                                                              lon_CN_tower))

# Calculate the population mean price from the original dataset
population_mean_price <- mean(listings_df2$price, na.rm = TRUE)

# Filter the data for less than or equal 1 km from CN Tower
group_less_than_1 <- filter(listings_df2, distance_from_CN <= 1)

# Perform one-sample t-test
t_test_result_group <- t.test(group_less_than_1$price, mu = population_mean_price)

# Create a dataframe from the t-test results
t_test_df2 <- data.frame(
  t_statistic = t_test_result_group$statistic,
  df = t_test_result_group$parameter,
  p_value = t_test_result_group$p.value,
  confidence_interval_lower = t_test_result_group$conf.int[1],
  confidence_interval_upper = t_test_result_group$conf.int[2],
  estimate_mean = mean(group_less_than_1$price, na.rm = TRUE)
)

# Print the results dataframe
print(t_test_df2)


#-------------------------------Milestone3-------------------------------------


# ---------------------------DM---------------------------
# Scatterplot of Availability and price
ggplot(listings_df, aes(x = availability_365, y = price)) +
  geom_point() +                           # Scatterplot
  geom_smooth(method = "lm", se = FALSE) +  # Regression line without confidence interval
  labs(title = "Scatterplot of Availability vs. Price",
       x = "Availability",
       y = "Price") +
  theme_bw()

regression <- lm(listings_df$price ~ listings_df$availability_365)
summary(regression)


# ---------------------------------QS------------------------------------
cor_test_result <- cor.test(downtown_listings$calculated_host_listings_count, downtown_listings$rating)
cor_test_result

# Scatterplot
plot(downtown_listings$calculated_host_listings_count, downtown_listings$rating,
     xlab = "Calculated Host Listings Count",
     ylab = "Rating",
     main = "Scatterplot of Calculated Host Listings Count vs Rating")

# Add regression line
abline(lm(downtown_listings$rating ~ downtown_listings$calculated_host_listings_count), col = "red")

# Add correlation coefficient
text(5, 10, paste("Correlation = ", round(cor_test_result$estimate, 3)), col = "blue")



#---------------------------------RG---------------------------------
listings_df <- read_csv("listings2.csv")

#Adding a column that shows the distance from city center

# Function to calculate distance using Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Earth radius in kilometers
  R <- 6371
  
  # Convert degrees to radians
  lat1 <- lat1 * (pi/180)
  lon1 <- lon1 * (pi/180)
  lat2 <- lat2 * (pi/180)
  lon2 <- lon2 * (pi/180)
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- R * c
  
  return(distance)
}
# Latitude and longitude of city center of Toronto
lat_center <- 43.6532
lon_center <- -79.3832

listings_df <- mutate(listings_df,distance_from_center = 
                        haversine_distance(latitude, longitude, lat_center, 
                                           lon_center))

#Sub-setting data based on last review to be only in 2023 and not before. This
#is because we want to have updated price (if a host left the platform 5 years
#ago, most probably the updated price is for 5 years ago and it cannot be 
#considered as comparison with others because of inflation factors)
filtered_data <- filter(listings_df, year(last_review) == 2023)

#Number of days between first and last review
calculate_days_difference <- function(date1, date2) {
  diff_days <- as.numeric(difftime(date2, date1, units = "days"))
  return(diff_days)}

filtered_data$host_experience <- calculate_days_difference(filtered_data$first_review,
                                                           filtered_data$last_review)

# Histogram of Distance from city center
hist(filtered_data$distance_from_center,main = "Number of listings based on Distance",
     breaks = 25, xaxt = "n", ylim = c(0,2000), xlab = "Distance from City Center",
     col = "skyblue", 
     border = "black")
axis(1, at = seq(0, 26, by = 1))

#Considering only between 1 to 2 km distance from City center which has more data
filtered_data <- filter(filtered_data,distance_from_center>=1 & distance_from_center<=2)

#Histogram of room types
ggplot(filtered_data, aes(x = room_type, fill = room_type)) +
  geom_histogram(stat = "count") +
  labs(title = "Histogram of Listings by Room Type",
       x = "Room Type",
       y = "Number of Listings") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

#Choosing Entire home/apt
filtered_data <- filter(filtered_data, room_type == "Entire home/apt")

#Scatter plot and linear regression
ggplot(filtered_data, aes(x = host_experience, y = price)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Scatter Plot of Price vs Host Experience",
       x = "Host Experience (days)",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

model <- lm(price ~ host_experience, data = filtered_data)
model_summary <- summary(model)
coefficients <- model_summary$coefficients  # Extract coefficients
residuals <- model_summary$residuals        # Extract residuals
r_squared <- model_summary$r.squared        # Extract R-squared
adj_r_squared <- model_summary$adj.r.squared # Extract Adjusted R-squared
results_df <- data.frame(Coefficients = coefficients,
                         RSquared = r_squared,
                         AdjustedRSquared = adj_r_squared)