# Load necessary packages
if (!require("SPEI")) install.packages("SPEI", dependencies = TRUE)
if (!require("lubridate")) install.packages("lubridate", dependencies = TRUE)
if (!require("zoo")) install.packages("zoo", dependencies = TRUE)

library(SPEI)
library(lubridate)
library(zoo)

# Load the precipitation data
data <- read.csv("SPI_Index.csv")

# Inspect the data structure
str(data)
head(data)

# Attempt to parse the date column correctly
data$DATE <- as.Date(data$DATE, format = "%m/%d/%Y")

# Check if date parsing was successful
if (all(is.na(data$DATE))) {
  stop("Date parsing failed. Please check the date format in your CSV file.")
}

# Extract month and year from the date
data$Month <- month(data$DATE)
data$Year <- year(data$DATE)

# Filter data from 1993 to 2024
data <- subset(data, Year >= 1993 & Year <= 2024)

# Adjust the year for the growing season (Oct-Dec of previous year and Jan-Apr of current year)
data$SeasonYear <- ifelse(data$Month %in% c(10, 11, 12), data$Year + 1, data$Year)

# Filter data for the crop growing season (October to April)
crop_season <- data[data$Month %in% c(10, 11, 12, 1, 2, 3, 4), ]

# Aggregate daily precipitation to monthly totals
monthly_precip <- aggregate(crop_season$PRCP, 
                            by = list(SeasonYear = crop_season$SeasonYear, Month = crop_season$Month), 
                            FUN = sum, na.rm = TRUE)

# Rename columns
colnames(monthly_precip) <- c("Year", "Month", "Precipitation")

# Handle NA values (use interpolation for missing data)
monthly_precip$Precipitation <- na.approx(monthly_precip$Precipitation, na.rm = FALSE)

# Re-check for NA values after interpolation
if (any(is.na(monthly_precip$Precipitation))) {
  # Replace remaining NAs with the mean precipitation value
  monthly_precip$Precipitation[is.na(monthly_precip$Precipitation)] <- mean(monthly_precip$Precipitation, na.rm = TRUE)
}

# Create a time series object (monthly)
ts_precip <- ts(monthly_precip$Precipitation, 
                start = c(min(monthly_precip$Year), min(monthly_precip$Month)), 
                frequency = 12)

# Calculate SPI for a 1-month timescale
spi_1 <- spi(ts_precip, scale = 1)

# Extract the SPI values and add them to the data frame
monthly_precip$SPI <- coredata(spi_1$fitted)

# Save the results to a CSV file
write.csv(monthly_precip, "SPI_1993_2024.csv", row.names = FALSE)

# Print the first few rows of the output
print(head(monthly_precip))

# Plot the SPI
plot(spi_1, main = "SPI (Standardized Precipitation Index) for Crop Growing Season (Oct-Apr, 1993-2024)", 
     xlab = "Time", ylab = "SPI", col = "blue")

cat("SPI values saved to 'SPI_1993_2024.csv'")


# Classifications 

# Load the SPI data
spi_data <- read.csv("SPI_1993_2024.csv")

# Inspect the data structure
str(spi_data)
head(spi_data)

# Calculate the mean SPI for each year
annual_spi <- aggregate(SPI ~ Year, data = spi_data, FUN = mean)

# Add a new column to classify the years based on the mean SPI
annual_spi$Classification <- ifelse(annual_spi$SPI < -1, "Drought", 
                                    ifelse(annual_spi$SPI > 1, "Wet", "Normal"))

# Print the classified data
print(annual_spi)

# Save the classified data to a new CSV file
write.csv(annual_spi, "SPI_Classification_1993_2024.csv", row.names = FALSE)

# Optional: Print a summary of the classifications
cat("\nSummary of Classifications:\n")
table(annual_spi$Classification)


## NASA Classification 

# Calculate the mean SPI for each year
annual_spi <- aggregate(SPI ~ Year, data = spi_data, FUN = mean)

# Add a new column to classify the years based on the NASA classification
annual_spi$Classification <- ifelse(annual_spi$SPI <= -2, "Extremely Dry", 
                                    ifelse(annual_spi$SPI > -2 & annual_spi$SPI <= -1.5, "Severely Dry", 
                                           ifelse(annual_spi$SPI > -1.5 & annual_spi$SPI <= -1, "Moderately Dry", 
                                                  ifelse(annual_spi$SPI > -1 & annual_spi$SPI <= 1, "Near Normal", 
                                                         ifelse(annual_spi$SPI > 1 & annual_spi$SPI <= 1.5, "Moderately Wet", 
                                                                ifelse(annual_spi$SPI > 1.5 & annual_spi$SPI <= 2, "Very Wet", 
                                                                       "Extremely Wet"))))))

# Print the classified data
print(annual_spi)

# Save the classified data to a new CSV file
write.csv(annual_spi, "SPI_Classification_NASA_1993_2024.csv", row.names = FALSE)

# Optional: Print a summary of the classifications
cat("\nSummary of NASA Classifications:\n")
table(annual_spi$Classification)











### Updated Code with detailed protocol

# Load necessary packages
library(SPEI)
library(lubridate)
library(zoo)

# Load the raw precipitation data
data <- read.csv("SPI_Index.csv")

# Inspect the data structure
str(data)
head(data)

# Parse the date column correctly
data$DATE <- as.Date(data$DATE, format = "%m/%d/%Y")

# Check if date parsing was successful
if (all(is.na(data$DATE))) {
  stop("Date parsing failed. Please check the date format in your CSV file.")
}

# Extract month and year from the date
data$Month <- month(data$DATE)
data$Year <- year(data$DATE)

# Filter data from 1993 to 2024
data <- subset(data, Year >= 1993 & Year <= 2024)

# Adjust the year for the growing season (Oct-Dec of previous year and Jan-Apr of current year)
data$SeasonYear <- ifelse(data$Month %in% c(10, 11, 12), data$Year + 1, data$Year)

# Filter data for the crop growing season (October to April)
crop_season <- data[data$Month %in% c(10, 11, 12, 1, 2, 3, 4), ]

# Aggregate daily precipitation to monthly totals
monthly_precip <- aggregate(crop_season$PRCP, 
                            by = list(SeasonYear = crop_season$SeasonYear, Month = crop_season$Month), 
                            FUN = sum, na.rm = TRUE)

# Rename columns
colnames(monthly_precip) <- c("Year", "Month", "Precipitation")

# Handle missing values (use interpolation for missing data)
monthly_precip$Precipitation <- na.approx(monthly_precip$Precipitation, na.rm = FALSE)

# Replace remaining NAs with the mean precipitation value
monthly_precip$Precipitation[is.na(monthly_precip$Precipitation)] <- mean(monthly_precip$Precipitation, na.rm = TRUE)

# Create a time series object (monthly)
ts_precip <- ts(monthly_precip$Precipitation, 
                start = c(min(monthly_precip$Year), min(monthly_precip$Month)), 
                frequency = 12)

# Calculating SPI using the correct method (Gamma distribution fitting)
spi_1 <- spi(ts_precip, scale = 1, distribution = "Gamma")

# Extract SPI values and add them to the data frame
monthly_precip$SPI <- coredata(spi_1$fitted)

# Save the results to a CSV file
write.csv(monthly_precip, "SPI_1993_2024_Correct.csv", row.names = FALSE)

# Print the first few rows of the output
print(head(monthly_precip))

# Plot the SPI
plot(spi_1, main = "SPI (Standardized Precipitation Index) for Crop Growing Season (Oct-Apr, 1993-2024)", 
     xlab = "Time", ylab = "SPI", col = "blue")

cat("SPI values calculated and saved to 'SPI_1993_2024_Correct.csv'")



# Classifications
library(dplyr)
library(lubridate)
library(anytime)

# Load the SPI data
spi_data <- read.csv("SPI_1993_2024_Correct.csv")

# Inspect the data structure
str(spi_data)
head(spi_data)

# Step 1: Correct Date Parsing
# Convert the DATE column to Date format using anytime for flexibility
spi_data$DATE <- anytime(spi_data$DATE)

# Check if the dates were parsed correctly
if (any(is.na(spi_data$DATE))) {
  stop("Date parsing failed. Please check the format of the DATE column.")
}

# Extract month and year from the corrected DATE column
spi_data$Month <- month(spi_data$DATE)
spi_data$Year <- year(spi_data$DATE)

# Step 2: Filter the data for the crop growing season (Oct to Apr)
crop_season_spi <- spi_data %>%
  filter(Month %in% c(10, 11, 12, 1, 2, 3, 4))

# Step 3: Calculate the growing season year (Oct-Dec of previous year, Jan-Apr of current year)
crop_season_spi <- crop_season_spi %>%
  mutate(SeasonYear = ifelse(Month %in% c(10, 11, 12), Year + 1, Year)) %>%
  group_by(SeasonYear) %>%
  summarize(Mean_SPI = mean(SPI, na.rm = TRUE))

# Step 4: SPI Classification as per NASA Protocol
crop_season_spi <- crop_season_spi %>%
  mutate(Classification = case_when(
    Mean_SPI <= -2                     ~ "Extremely Dry",
    Mean_SPI > -2 & Mean_SPI <= -1.5    ~ "Severely Dry",
    Mean_SPI > -1.5 & Mean_SPI <= -1    ~ "Moderately Dry",
    Mean_SPI > -1 & Mean_SPI <= 1       ~ "Near Normal",
    Mean_SPI > 1 & Mean_SPI <= 1.5      ~ "Moderately Wet",
    Mean_SPI > 1.5 & Mean_SPI <= 2      ~ "Very Wet",
    Mean_SPI > 2                        ~ "Extremely Wet"
  ))

# Print the classified data
print(crop_season_spi)

# Save the classified data to a new CSV file
write.csv(crop_season_spi, "SPI_Classification_Growing_Season_NASA_1993_2024.csv", row.names = FALSE)

# Summary of the classifications
cat("\nSummary of NASA Classifications (Crop Growing Season):\n")
print(table(crop_season_spi$Classification))

# Step 5: Plotting the classified SPI data
barplot(table(crop_season_spi$Classification), 
        main = "SPI Classification (NASA) for Crop Growing Season (1993-2024)",
        col = "lightgreen", 
        xlab = "Classification", 
        ylab = "Frequency")

crop_season_spi <- crop_season_spi %>%
  mutate(SeasonYear = ifelse(Month %in% c(10, 11, 12), Year + 1, Year)) %>%
  group_by(SeasonYear) %>%
  summarize(Mean_SPI = ifelse(all(is.na(SPI)), NA, mean(SPI, na.rm = TRUE)))









library(dplyr)
library(lubridate)

# Load the SPI data
spi_data <- read.csv("SPI_1993_2024_Correct.csv")

# Inspect the data structure
str(spi_data)
head(spi_data)

# Check the DATE column format and convert to Date if necessary
if (!inherits(spi_data$DATE, "Date")) {
  spi_data$DATE <- as.Date(spi_data$DATE, format = "%Y-%m-%d")
}

# Extract Year and Month from the DATE column
spi_data$Year <- year(spi_data$DATE)
spi_data$Month <- month(spi_data$DATE)

# Filter the data for the crop growing season (Oct to Apr)
crop_season_spi <- spi_data %>%
  filter(Month %in% c(10, 11, 12, 1, 2, 3, 4))

# Create a SeasonYear column, handling cross-year seasons (Oct-Dec in previous year)
crop_season_spi <- crop_season_spi %>%
  mutate(SeasonYear = ifelse(Month %in% c(10, 11, 12), Year + 1, Year))

# Calculate the mean SPI for each growing season year (Oct-Apr)
crop_season_spi <- crop_season_spi %>%
  group_by(SeasonYear) %>%
  summarize(Mean_SPI = ifelse(all(is.na(SPI)), NA, mean(SPI, na.rm = TRUE)))

# SPI Classification as per NASA Protocol
crop_season_spi <- crop_season_spi %>%
  mutate(Classification = case_when(
    Mean_SPI <= -2                     ~ "Extremely Dry",
    Mean_SPI > -2 & Mean_SPI <= -1.5    ~ "Severely Dry",
    Mean_SPI > -1.5 & Mean_SPI <= -1    ~ "Moderately Dry",
    Mean_SPI > -1 & Mean_SPI <= 1       ~ "Near Normal",
    Mean_SPI > 1 & Mean_SPI <= 1.5      ~ "Moderately Wet",
    Mean_SPI > 1.5 & Mean_SPI <= 2      ~ "Very Wet",
    Mean_SPI > 2                        ~ "Extremely Wet",
    is.na(Mean_SPI)                     ~ "No Data"
  ))

# Print the classified data
print(crop_season_spi)

# Save the classified data to a new CSV file
write.csv(crop_season_spi, "SPI_Classification_Growing_Season_NASA_1993_2024.csv", row.names = FALSE)

# Summary of the classifications
cat("\nSummary of NASA Classifications (Crop Growing Season):\n")
print(table(crop_season_spi$Classification))

# Plotting the classified SPI data
barplot(table(crop_season_spi$Classification), 
        main = "SPI Classification (NASA) for Crop Growing Season (1993-2024)",
        col = "lightgreen", 
        xlab = "Classification", 
        ylab = "Frequency")









## Recompute

install.packages("SPEI")
install.packages("dplyr")
install.packages("lubridate")


# Load necessary libraries
library(tidyverse)
library(lubridate)
library(MASS)

# Load the precipitation data
data <- read.csv("SPI_Index.csv")

# Convert the DATE column to a date object
data$DATE <- mdy(data$DATE)

# Function to calculate SPI for a given timescale and data
calculate_spi <- function(precipitation, timescale) {
  spi <- rep(NA, length(precipitation))
  for (i in timescale:length(precipitation)) {
    window <- precipitation[(i - timescale + 1):i]
    if (any(is.na(window))) {
      spi[i] <- NA
    } else {
      # Remove NA and small values before fitting gamma distribution
      window_filtered <- window[window > 0 & !is.na(window)]
      if (length(window_filtered) < 2) {
        spi[i] <- NA # Not enough data for gamma fitting
      } else {
        gamma_fit <- tryCatch(
          fitdistr(window_filtered, "gamma", lower = c(0, 0))$estimate,
          error = function(e) return(NULL)
        )
        if (is.null(gamma_fit)) {
          spi[i] <- NA # Gamma fit failed
        } else {
          shape <- gamma_fit["shape"]
          rate <- gamma_fit["rate"]
          
          # Calculate cumulative probability
          cumulative_prob <- pgamma(precipitation[i], shape = shape, rate = rate)
          
          # Adjust for zero precipitation
          if (precipitation[i] == 0) {
            zero_prob <- sum(window == 0) / timescale
            cumulative_prob <- zero_prob + (1 - zero_prob) * cumulative_prob
          }
          
          # Convert to standard normal distribution
          spi[i] <- qnorm(cumulative_prob)
        }
      }
    }
  }
  return(spi)
}

# Function to classify SPI values
classify_spi <- function(spi_values) {
  classification <- case_when(
    spi_values < -2.0 ~ "extremely dry",
    spi_values >= -2.0 & spi_values < -1.5 ~ "severely dry",
    spi_values >= -1.5 & spi_values < -1.0 ~ "moderately dry",
    spi_values >= -1.0 & spi_values < 1.0 ~ "near normal",
    spi_values >= 1.0 & spi_values < 1.5 ~ "moderately wet",
    spi_values >= 1.5 & spi_values < 2.0 ~ "very wet",
    spi_values >= 2.0 ~ "extremely wet",
    is.na(spi_values) ~ NA_character_
  )
  return(classification)
}

# Process data for winter wheat growing season (Oct-Apr)
data$Year <- year(data$DATE)
data$Month <- month(data$DATE)

# Filter for relevant months
winter_wheat_data <- data %>%
  filter(Month %in% c(10, 11, 12, 1, 2, 3, 4)) %>%
  mutate(
    SeasonYear = ifelse(Month %in% c(10, 11, 12), Year + 1, Year)
  )

# Group by SeasonYear and calculate SPI for 6-month timescale
spi_results <- winter_wheat_data %>%
  group_by(SeasonYear) %>%
  mutate(
    SPI_6month = calculate_spi(PRCP, 6),
    SPI_Classification = classify_spi(SPI_6month)
  ) %>%
  ungroup()

# Calculate Season-wide SPI.
season_spi <- spi_results %>%
  group_by(SeasonYear) %>%
  summarize(
    Season_Mean_SPI = mean(SPI_6month, na.rm = TRUE),
    Season_SPI_Classification = classify_spi(Season_Mean_SPI)
  )

# Select relevant columns and save to CSV
write.csv(season_spi, "SPI_WinterWheat_Season_Results.csv", row.names = FALSE)

print("SPI calculation and classification completed. Results saved to SPI_WinterWheat_Season_Results.csv")
