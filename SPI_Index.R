# Traditional formula calculation of SPI

# Load necessary libraries
library(dplyr)
library(lubridate)

# Load the SPI data
spi_data <- read.csv("SPI_Index.csv")

# Convert the DATE column to Date format (MM/DD/YYYY)
spi_data$DATE <- as.Date(spi_data$DATE, format = "%m/%d/%Y")

# Check if the date conversion was successful
if (all(is.na(spi_data$DATE))) {
  stop("Error: DATE column could not be converted to Date format. Please check the data.")
}

# Extract the year and month from the DATE column
spi_data$Year <- year(spi_data$DATE)
spi_data$Month <- month(spi_data$DATE)

# Check the structure and first few rows of the data
str(spi_data)
head(spi_data)

# Filter the data for the crop growing season (October to April)
crop_season_spi <- spi_data %>%
  filter(Month %in% c(10, 11, 12, 1, 2, 3, 4, 5)) %>%
  mutate(SeasonYear = ifelse(Month %in% c(10, 11, 12), Year + 1, Year)) %>%
  group_by(SeasonYear) %>%
  summarize(
    Mean_PRCP = mean(PRCP, na.rm = TRUE),
    SD_PRCP = sd(PRCP, na.rm = TRUE)
  )

# Calculate SPI using the given formula:
# SPI = (P - P*) / σp
crop_season_spi <- crop_season_spi %>%
  mutate(SPI = (Mean_PRCP - mean(Mean_PRCP)) / sd(Mean_PRCP))

# SPI Classification:
# SPI ≤ -1.0: Drought
# -1.0 < SPI ≤ 1.0: Normal
# SPI > 1.0: Wet
crop_season_spi <- crop_season_spi %>%
  mutate(Classification = case_when(
    SPI <= -1.0 ~ "Drought",
    SPI > -1.0 & SPI <= 1.0 ~ "Normal",
    SPI > 1.0 ~ "Wet"
  ))

# Print the classified data
print(crop_season_spi)

# Save the classified data to a new CSV file
write.csv(crop_season_spi, "SPI_Classification_Growing_Season_from_1993-2024.csv", row.names = FALSE)

# Summary of the classifications
cat("\nSummary of SPI Classifications (Crop Growing Season):\n")
print(table(crop_season_spi$Classification))

# Plotting the classified SPI data
barplot(
  table(crop_season_spi$Classification),
  main = "SPI Classification for Crop Growing Season (1993-2024)",
  col = "lightblue",
  xlab = "Classification",
  ylab = "Frequency"
)




# By performing the Gamma Fiting

# Load necessary libraries
library(dplyr)
library(lubridate)
library(fitdistrplus)
library(SPEI)

# Load the SPI data
spi_data <- read.csv("SPI_Index.csv")

# Convert the DATE column to Date format (MM/DD/YYYY)
spi_data$DATE <- as.Date(spi_data$DATE, format = "%m/%d/%Y")

# Check if the date conversion was successful
if (all(is.na(spi_data$DATE))) {
  stop("Error: DATE column could not be converted to Date format. Please check the data.")
}

# Extract the year and month from the DATE column
spi_data$Year <- year(spi_data$DATE)
spi_data$Month <- month(spi_data$DATE)

# Check the structure and first few rows of the data
str(spi_data)
head(spi_data)

# Filter the data for the crop growing season (October to April)
crop_season_spi <- spi_data %>%
  filter(Month %in% c(10, 11, 12, 1, 2, 3, 4)) %>%
  mutate(SeasonYear = ifelse(Month %in% c(10, 11, 12), Year + 1, Year))

# Group data by SeasonYear
seasonal_data <- crop_season_spi %>%
  group_by(SeasonYear) %>%
  summarize(Total_PRCP = sum(PRCP, na.rm = TRUE))

# Fit the Gamma distribution to the seasonal precipitation data
gamma_fit <- fitdist(seasonal_data$Total_PRCP, "gamma")

# Calculate cumulative distribution (CDF) values using the fitted Gamma distribution
cdf_values <- pgamma(seasonal_data$Total_PRCP, 
                     shape = gamma_fit$estimate["shape"], 
                     rate = gamma_fit$estimate["rate"])

# Transform CDF values to Z-scores (standard normal distribution)
spi_values <- qnorm(cdf_values)

# Add SPI values to the data frame
seasonal_data <- seasonal_data %>%
  mutate(SPI = spi_values)

# SPI Classification:
# SPI ≤ -1.0: Drought
# -1.0 < SPI ≤ 1.0: Normal
# SPI > 1.0: Wet
seasonal_data <- seasonal_data %>%
  mutate(Classification = case_when(
    SPI <= -1.0 ~ "Drought",
    SPI > -1.0 & SPI <= 1.0 ~ "Normal",
    SPI > 1.0 ~ "Wet"
  ))

# Print the classified data
print(seasonal_data)

# Save the classified data to a new CSV file
write.csv(seasonal_data, "SPI_Gamma_Classification.csv", row.names = FALSE)

# Summary of the classifications
cat("\nSummary of SPI Classifications (Gamma Distribution):\n")
print(table(seasonal_data$Classification))

# Plotting the SPI values with classification
barplot(
  table(seasonal_data$Classification),
  main = "SPI Classification (Gamma Fitting) for Crop Growing Season",
  col = "lightgreen",
  xlab = "Classification",
  ylab = "Frequency"
)


