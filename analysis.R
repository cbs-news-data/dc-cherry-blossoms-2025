# Libraries
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(janitor)
library(lubridate)
library(ggplot2)
library(ggtext)
library(ggimage)
library(svglite)

# Lood data
cherry_blossoms <- read.csv("data/cherry-blossoms.csv") %>% clean_names() %>% select(year, yoshino_peak_bloom_date)

# Convert day of year to month and date
cherry_blossoms <- cherry_blossoms %>%
  mutate(
    bloom_date = as.Date(yoshino_peak_bloom_date, origin = paste0(year, "-01-01")),
    formatted_date = format(bloom_date, "%b. %d")  # Format as "Mar. 20"
  )

Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Plot with cherry blossom emojis as points
cherry_blossoms <- cherry_blossoms %>%
  mutate(emoji = "https://abs.twimg.com/emoji/v2/72x72/1f338.png")  # Direct emoji URL

# Define the start and end dates for peak bloom (March 28-31)
peak_bloom_start <- as.Date("2025-03-28")
peak_bloom_end <- as.Date("2025-03-31")

# Convert peak bloom dates to day of year for correct plotting
peak_bloom_start_doy <- as.numeric(format(peak_bloom_start, "%j"))
peak_bloom_end_doy <- as.numeric(format(peak_bloom_end, "%j"))

# Generate sequence of dates for more y-axis labels
y_breaks <- seq(60, 110, by = 5)  # Bloom dates range; adjust if necessary

ggplot(cherry_blossoms, aes(x = year, y = yoshino_peak_bloom_date)) +
  geom_image(aes(image = emoji), size = 0.04) +  # Emoji as points
  geom_smooth(span = 0.2, size = 1.5, se = FALSE, color = "blue") +
  # Add shaded area for March 28-31, 2025
  geom_rect(aes(xmin = 2025 - 0.5, xmax = 2025 + 0.5, ymin = peak_bloom_start_doy, ymax = peak_bloom_end_doy),
            fill = "red", alpha = 0.3) +  # Shaded area for March 28-31
  scale_y_continuous(
    breaks = y_breaks,  # More frequent tick marks
    labels = function(x) format(as.Date(x, origin = "2024-01-01"), "%d-%b")  # Format as day-month
  ) +
  labs(
    title = "Peak Bloom Dates of Yoshino Cherry Blossoms",
    x = "Year",
    y = "Peak Bloom Date"
  ) +
  theme_minimal()

# Save the ggplot as an SVG file
ggsave("output/cherry_blossoms.svg", width = 374 / 96, height = 6, device = "svg")  