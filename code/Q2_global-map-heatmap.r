# --------- Group project for data science ------------
# --- Measuring progress towards the UN Sustainable ---
# ---- Development Goal 8:work and economic growth ----
# ----------------------- Q2 --------------------------


# ----------- Install necessary packages --------------
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("rnaturalearth")                 #doing this to produce the map heatmap accurately
#install.packages("rnaturalearthdata")
#install.packages("sf")
# ------------- Load necessary packages ----------------
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# ---------------- Load all data sets ------------------
continents <- read.csv("data/continents-according-to-our-world-in-data.csv") %>%
  select(-Year)                                                                             # Remove Year as unneccessary
gdp_per_capita <- read.csv("data/gdp-per-capita-worldbank.csv")
NEET <- read.csv("data/youth-not-in-education-employment-training.csv")
cleaned_NEET_country <- read.csv("output/cleaned_country_level.csv") %>%
  select(-gdp_pc_ppp_2017)                                                                  # Remove GDP column to avoid confusion
additional_data_set <- read.csv("output/additional_df.csv")

# --------Filter data for the two time periods ---------
# selecting 2012 - 2015 average data 
data_2015 <- cleaned_NEET_country %>%
  filter(year >= 2012 & year <= 2015, !is.na(neet_youth_pct)) %>%
  group_by(country, continent, code) %>%                                                    # Group by code to prepare for joining with map data
  summarise(neet_youth_pct = mean(neet_youth_pct, na.rm = TRUE), .groups = "drop") %>%      # Average over recent years to not skew the data
  arrange(continent, country)                                                               # Sort data for easier visual comparison
head(data_2015, 20)

# selecting 2019 - 2022 average data
data_recent <- cleaned_NEET_country %>%
  filter(year >= 2019 & year <= 2022, !is.na(neet_youth_pct)) %>%
  group_by(country, continent, code) %>%                                                    # Group by code to prepare for joining with map data
  summarise(neet_youth_pct = mean(neet_youth_pct, na.rm = TRUE), .groups = "drop") %>%      # Average over recent years to not skew the data
  arrange(continent, country)                                                               # Sort data for easier visual comparison
head(data_recent, 30)

# find median to use for midpoint in heatmap
median_2015 <- median(data_2015$neet_youth_pct, na.rm = TRUE)
median_recent <- median(data_recent$neet_youth_pct, na.rm = TRUE)
print(median_2015)
print(median_recent)


# --------- Create world map heatmap for 2015 ----------
world <- ne_countries(scale = "medium", returnclass = "sf")                                  # Get world map country data
continents <- ne_coastline(scale = "medium", returnclass = "sf")                             # Get continent data

head(world, 20)
names(world)                                                                                 # Check column names for joining

# ----- Merge map data with NEET data for 2015 ---------
world_neet_2015 <- world %>%
  left_join(data_2015, by = c("iso_a3" = "code"))                                            # Join map data with NEET data using country codes whi h matched
head(world_neet_2015, 20)

# ----- Merge map data with NEET data for recent ---------
world_neet_recent <- world %>%
  left_join(data_recent, by = c("iso_a3" = "code"))                                          # Join map data with NEET data using country codes whi h matched
head(world_neet_recent, 15)                                                                  # this is really long and confusing, ended up starting chatGBT as this point


# ------- Plot world map heatmap for 2015 -------------
p1 <- ggplot(data = world_neet_2015, aes(fill = neet_youth_pct))
p1 + geom_sf(color = "white", size = 0.2) +
  scale_fill_viridis_c(
  option = "C",
  direction = -1,                                                                      # reverse the color scale so higher values are darker
  na.value = "#dceeff",                                                              # light blue color for NA values (contrast with color scale)    
  limits = c(0, 40),
  name = "Avg. NEET %",
  breaks = seq(0, 40, 10)                                                              # set breaks for legend          
  ) +
  labs(title = "Youth NEET Rates (2012 - 2015)",
       subtitle = "Darker coolours show a larger share of young people Not in Employment, Education, or Training") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.45, size = 22),
    plot.subtitle = element_text(hjust = 0.9, size = 16, colour = "gray15", margin = margin(b = 15)),
    plot.background = element_rect(fill = "#f5f5f5", colour = NA),                     # warm light gray background for better contrast
    legend.background = element_rect(fill = "#f5f5f5", colour = NA),                   # match legend background to plot background
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),           # white background for better contrast
    panel.border = element_rect(colour = "gray70", linewidth = 0.8),                     # border around plot area
    plot.margin = margin(20, 20, 20, 20)                                                 # add margin around the plot
  )
print(p1)
ggsave("Q2_world_map_heatmap_2015.png", plot = p1, width = 12, height = 8, dpi = 300)

# ------- Plot world map heatmap for recent -------------
p2 <- ggplot(data = world_neet_recent, aes(fill = neet_youth_pct)) +
p2 + geom_sf(color = "white", size = 0.2) +
  scale_fill_viridis_c(
  option = "C",
  direction = -1,                                                                      # reverse the color scale so higher values are darker
  na.value = "#dceeff",                                                              # light blue color for NA values (contrast with color scale)
  limits = c(0, 40),
  name = "Avg. NEET %",
  breaks = seq(0, 40, 10)                                                              # set breaks for legend          
  ) +
  labs(title = "Youth NEET Rates (2019 - 2022)",
       subtitle = "Darker coolours show a larger share of young people Not in Employment, Education, or Training") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.45, size = 22),
    plot.subtitle = element_text(hjust = 0.9, size = 16, colour = "gray15", margin = margin(b = 15)),
    plot.background = element_rect(fill = "#f5f5f5", colour = NA),                     # warm light gray background for better contrast
    legend.background = element_rect(fill = "#f5f5f5", colour = NA),                   # match legend background to plot background
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),           # white background for better contrast
    panel.border = element_rect(colour = "gray70", linewidth = 0.8),                     # border around plot area
    plot.margin = margin(20, 20, 20, 20)                                                 # add margin around the plot
  )
print(p2)
ggsave("Q2_world_map_heatmap_recent.png", plot = p2, width = 12, height = 8, dpi = 300)





