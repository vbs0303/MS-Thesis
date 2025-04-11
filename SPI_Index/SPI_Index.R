# SPI Calculation and Classification

# Gamma Fitting method

# Required libraries
library(dplyr)
library(lubridate)
library(fitdistrplus)  # For fitting gamma distribution
library(stats)          # For qnorm

# Load your SPI data
spi_data <- read.csv("SPI_Index.csv")
spi_data$DATE <- as.Date(spi_data$DATE, format = "%m/%d/%Y")

# Extract year and month
spi_data$Year <- year(spi_data$DATE)
spi_data$Month <- month(spi_data$DATE)

# Filter out post-Sep 2024 data
spi_data <- spi_data %>%
  filter(!(Year == 2024 & Month %in% c(10, 11, 12)))

# Remove Jan-May 1993
spi_data <- spi_data %>%
  filter(!(Year == 1993 & Month %in% c(1, 2, 3, 4, 5)))

# Filter for crop season months
season_data <- spi_data %>%
  filter(Month %in% c(10, 11, 12, 1, 2, 3, 4, 5)) %>%
  mutate(SeasonYear = ifelse(Month %in% c(10, 11, 12), Year + 1, Year)) %>%
  filter(SeasonYear <= 2024)

# Group by season year and compute total precipitation
season_agg <- season_data %>%
  group_by(SeasonYear) %>%
  summarize(Total_PRCP = sum(PRCP, na.rm = TRUE)) %>%
  ungroup()

# Fit gamma distribution
fit <- fitdist(season_agg$Total_PRCP, "gamma", method = "mme")
alpha <- fit$estimate["shape"]
beta <- fit$estimate["rate"]

# Compute cumulative probability G(x)
season_agg <- season_agg %>%
  mutate(
    CDF_Gamma = pgamma(Total_PRCP, shape = alpha, rate = beta),
    CDF_Gamma = ifelse(CDF_Gamma == 1, 0.9999, ifelse(CDF_Gamma == 0, 0.0001, CDF_Gamma)),
    
    # Transform cumulative probability to SPI using standard normal quantile function
    SPI = qnorm(CDF_Gamma)
  )

# SPI Classification
season_agg <- season_agg %>%
  mutate(Classification = case_when(
    SPI <= -1.0 ~ "Drought",
    SPI > -1.0 & SPI <= 1.0 ~ "Normal",
    SPI > 1.0 ~ "Wet"
  ))

# Save to CSV
write.csv(season_agg, "SPI_Classification_Gamma_distribution_method.csv", row.names = FALSE)

# Summary
cat("\nSummary of SPI Classifications (Literature-based SPI):\n")
print(table(season_agg$Classification))




# Sandard deviation method  

# Load required libraries
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

# Read your CSV file
data <- read_csv("SPI_Index.csv")

# Convert DATE column to Date format
data$DATE <- as.Date(data$DATE, format = "%m/%d/%Y")

# Extract year and month
data$Year <- year(data$DATE)
data$Month <- month(data$DATE)

# Assign growing season year (Oct-Dec belongs to next year's season)
data <- data %>%
  mutate(Growing_Season_Year = ifelse(Month >= 10, Year + 1, Year))

# Filter only growing season months (Oct to May)
data_growing <- data %>%
  filter(Month %in% c(10, 11, 12, 1, 2, 3, 4, 5))

# Exclude records where Growing_Season_Year is 1993 (removes Oct-Dec 1992 and Jan-May 1993)
data_growing <- data_growing %>%
  filter(Growing_Season_Year != 1993)

# Calculate total precipitation for each growing season
seasonal_precip <- data_growing %>%
  group_by(Growing_Season_Year) %>%
  summarise(Total_PRCP = sum(PRCP, na.rm = TRUE)) %>%
  ungroup()

# Calculate mean and standard deviation
mean_p <- mean(seasonal_precip$Total_PRCP)
sd_p <- sd(seasonal_precip$Total_PRCP)

# Calculate SPI and classify
seasonal_precip <- seasonal_precip %>%
  mutate(SPI = (Total_PRCP - mean_p) / sd_p,
         Classification = case_when(
           SPI <= -1 ~ "Drought",
           SPI > -1 & SPI <= 1 ~ "Normal",
           SPI > 1 ~ "Wet"
         ))

# Save the result as CSV
write_csv(seasonal_precip, "SPI_Classification_Standard_deviation_method.csv")

# Plot the SPI classification
library(ggplot2)
library(scales)
library(grid)

ggplot(seasonal_precip, aes(x = factor(Growing_Season_Year), y = SPI, fill = Classification)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Drought" = "red", "Normal" = "grey", "Wet" = "blue")) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "green") +
  geom_hline(yintercept = -1.0, linetype = "dashed", color = "green") +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Standardized Precipitation Index (SPI)",
       x = "Water years",
       y = "Z-score") +
  theme_classic() +
  theme(
    axis.line = element_line(color = "black", size = 0.8),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(color = "black", size = 0.8),
    axis.ticks.y.left = element_line(color = "black", size = 0.8),
    axis.ticks.x.bottom = element_line(color = "black", size = 0.8),
    axis.minor.ticks.length = unit(2.5, "pt"),
    axis.minor.ticks.y.left = element_line(color = "black"),
    axis.minor.ticks.x.bottom = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_text(aes(label = round(Total_PRCP),
                y = ifelse(SPI < 0, SPI - 0.2, SPI + 0.2)),
            size = 3) +
  scale_y_continuous(
    limits = c(-4, 4),
    breaks = seq(-4, 4, by = 1),
    minor_breaks = seq(-4, 4, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    breaks = seasonal_precip$Growing_Season_Year[seq(1, nrow(seasonal_precip), by = 1)],
    expand = c(0, 0)
  ) +
  guides(
    x = guide_axis(minor.ticks = TRUE),
    y = guide_axis(minor.ticks = TRUE)
  )


# Save the plot
ggsave("SPI_Classification_Standard_deviation_method.jpg",
       width = 10, height = 6, dpi = 300)


