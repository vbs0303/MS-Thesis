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
