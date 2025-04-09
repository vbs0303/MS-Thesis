# Load necessary libraries
library(dplyr)
library(lubridate)
library(readr)

# Read the CSV file
weatherdata <- read.csv("WeatherData/RawFiles/Posey_CIG_1-1-21_12-00_AM_1_Year_1744119785_v2.csv")

# Rename columns for easier access
colnames(weatherdata) <- c("DateTimeRaw", "SolarRadiation_W_m2")

# Clean extra spaces (if any)
weatherdata$DateTimeRaw <- trimws(weatherdata$DateTimeRaw)

# Parse the datetime properly using correct format
weatherdata$DateTime <- as.POSIXct(weatherdata$DateTimeRaw, format = "%m/%d/%Y %H:%M", tz = "UTC")

# Check parsing success
failed_rows <- sum(is.na(weatherdata$DateTime))
cat("⚠️ Failed to parse rows:", failed_rows, "\n")

# Filter out unparsed rows
weatherdata <- weatherdata %>% filter(!is.na(DateTime))

# Extract date
weatherdata$Date <- as.Date(weatherdata$DateTime)

# Convert 15-min W/m² to MJ/m²
weatherdata <- weatherdata %>%
  mutate(Solar_MJ_m2 = SolarRadiation_W_m2 * 900 * 1e-6)

# Daily aggregation
daily_radiation <- weatherdata %>%
  group_by(Date) %>%
  summarise(Daily_Solar_MJ_m2 = sum(Solar_MJ_m2, na.rm = TRUE))

# Save output
write_csv(daily_radiation, "2021_Daily_Solar_Radiation.csv")

# Show result
print(head(daily_radiation))

# Plotting the daily solar radiation
library(ggplot2)
ggplot(daily_radiation, aes(x = Date, y = Daily_Solar_MJ_m2)) +
  geom_line(color = "blue") +
  labs(title = "Daily Solar Radiation (MJ/m²/day)",
       x = "Date",
       y = "Solar Radiation (MJ/m²/day)") +
  theme_minimal()

