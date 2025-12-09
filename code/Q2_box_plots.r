# --------- Group project for data science ------------
# --- Measuring progress towards the UN Sustainable ---
# ---- Development Goal 8:work and economic growth ----
# ----------------------- Q2 --------------------------


# ----------- Install necessary packages --------------
#install.packages("tidyverse")
#install.packages("ggplot2")

# ------------- Load necessary packages ----------------
library(tidyverse)
library(ggplot2)

# --------------- Load all data sets ------------------
continents <- read.csv("data/continents-according-to-our-world-in-data.csv")
gdp_per_capita <- read.csv("data/gdp-per-capita-worldbank.csv")
NEET <- read.csv("data/youth-not-in-education-employment-training.csv")
cleaned_NEET_country <- read.csv("output/cleaned_country_level.csv") %>%
  select(-gdp_pc_ppp_2017)                                                                  # Remove GDP column to avoid confusion
additional_data_set <- read.csv("output/additional_df.csv")

# ------------- Clean additional data set -------------  
additional_data_cleaned <- additional_data_set %>% 
  select(-Investment_Share) %>%                                                             # Remove Investment Share as not reauiremed for q2
  filter(!is.na(Secondary_School_Enrollment))                                               # Remove rows to quicken data cleaning as df is so large

# ---- Join continent data to additional data set ------
education_share_continents <- additional_data_cleaned %>%
  left_join(continents, by = "Code") %>%                                                       # Join continent data
  select(-Entity)
head(education_share_continents, 20)


# --------Filter data for the two time periods ---------
# selecting 2012 - 2015 average data 
data_2015 <- cleaned_NEET_country %>%
  filter(year >= 2012 & year <= 2015, !is.na(neet_youth_pct), !is.na(continent)) %>%
  group_by(country, continent) %>%
  summarise(neet_youth_pct = mean(neet_youth_pct, na.rm = TRUE), .groups = "drop") %>%      # Average over recent years to not skew the data
  arrange(continent, country)                                                               # Sort data for easier visual comparison
head(data_2015, 30)

# same for education data
data_2015_education <- education_share_continents %>%
  filter(year >= 2012 & year <= 2015, !is.na(neet_youth_pct), !is.na(continent)) %>%
  group_by(country, Continent) %>%
  summarise(education_pct = mean(Secondary_School_Enrollment, na.rm = TRUE), .groups = "drop") %>%      # Average over recent years to not skew the data
  arrange(Continent, country)                                                                           # Sort data for easier visual comparison
head(data_2015_education, 30)


# selecting 2019 - 2022 average data
data_recent <- cleaned_NEET_country %>%
  filter(year >= 2019 & year <= 2022, !is.na(neet_youth_pct), !is.na(continent)) %>%
  group_by(country, continent) %>%
  summarise(neet_youth_pct = mean(neet_youth_pct, na.rm = TRUE), .groups = "drop") %>%      # Average over recent years to not skew the data
  arrange(continent, country)                                                               # Sort data for easier visual comparison
head(data_recent, 30)

# same for education data
data_recent_education <- education_share_continents %>%
  filter(year >= 2019 & year <= 2022) %>%
  group_by(country, Continent) %>%
  summarise(education_pct = mean(Secondary_School_Enrollment, na.rm = TRUE), .groups = "drop") %>%      # Average over recent years to not skew the data
  arrange(Continent, country)                                                                           # Sort data for easier visual comparison
head(data_recent_education, 30)


# ----------- Create boxplots for both time periods ------------
# ------------ and both educaiton and NEET data ----------------
# Boxplot for 2015 data
p1 <- ggplot(data_2015, aes(x = continent, y = neet_youth_pct, fill = continent, colour = continent)) +
  geom_boxplot(size = 1.3, alpha = 0.6, outlier.shape = 4, outlier.size = 5, outlier.stroke = 2.2) +     # give outlyers a distinct shape and size
  stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 1.3, alpha = 0.8) +            # add error bars to the boxplot
  labs(title = "Box Plots of Youth NEET % by Continent (2012-2015)",
  y = "youth NEET %",
  subtitle = "Averaged across available years per country (2012 - 2015)") +
  scale_colour_manual(values = c("Africa" = "seaGreen", "Asia" = "Red", "Europe" = "dodgerBlue", "North America" = "#9C27B0", "Oceania" = "Orange", "South America" = "Brown")) +
  scale_fill_manual(values = c("Africa" = "seaGreen", "Asia" = "Red", "Europe" = "dodgerBlue", "North America" = "#9C27B0", "Oceania" = "Orange", "South America" = "Brown")) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
    plot.subtitle = element_text(hjust = 0.5, size = 16, colour = "gray15", margin = margin(b = 15)),
    plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
    legend.position = "none",
    panel.grid.major = element_line(colour = "gray85", linewidth = 0.4),     # Major grid every 
    panel.grid.minor = element_line(colour = "gray90", linewidth = 0.3),     # Faint grid every year
    panel.background = element_rect(fill = "white"),                         # white background for better contrast
    panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
    axis.title = element_text(face = "bold", size = 17.5, colour = "gray20"),
    axis.text = element_text(size = 15, colour = "gray20"),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks = element_line(colour = "gray50"),
    axis.line = element_line(colour = "gray40"),
    plot.margin = margin(15, 15, 15, 15)                                     # add margin around the plot
  ) +
  scale_y_continuous(breaks = seq(0, 50, by = 10)) +
  coord_cartesian(ylim = c(2, 50.5), clip = "off")                                           # limit y-axis to focus on relevant data range

print(p1)
ggsave("Q2_box_plot_2012-2015.png", width = 11, height = 14, dpi = 300)

# education 2012 - 2015
# (not requested but will be useful for further analysis)
p1.e <- ggplot(data_2015_education, aes(x = Continent, y = education_pct, fill = Continent, colour = Continent)) +
  geom_boxplot(size = 1.3, alpha = 0.6, outlier.shape = 4, outlier.size = 5, outlier.stroke = 2.2) +     # give outlyers a distinct shape and size
  stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 1.3, alpha = 0.8) +            # add error bars to the boxplot
  labs(title = "Box Plots of % Youths Enrolled in Education by Continent (2012-2015)",
  y = "% Youths Enrolled in Education",
  subtitle = "Averaged across available years per country (2012 - 2015)") +
  scale_colour_manual(values = c("Africa" = "seaGreen", "Asia" = "Red", "Europe" = "dodgerBlue", "North America" = "#9C27B0", "Oceania" = "Orange", "South America" = "Brown")) +
  scale_fill_manual(values = c("Africa" = "seaGreen", "Asia" = "Red", "Europe" = "dodgerBlue", "North America" = "#9C27B0", "Oceania" = "Orange", "South America" = "Brown")) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 16, colour = "gray15", margin = margin(b = 15)),
    plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
    legend.position = "none",
    panel.grid.major = element_line(colour = "gray85", linewidth = 0.4),     # Major grid every 
    panel.grid.minor = element_line(colour = "gray90", linewidth = 0.3),     # Faint grid every year
    panel.background = element_rect(fill = "white"),                         # white background for better contrast
    panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
    axis.title = element_text(face = "bold", size = 16, colour = "gray20"),
    axis.text = element_text(size = 15, colour = "gray20"),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks = element_line(colour = "gray50"),
    axis.line = element_line(colour = "gray40"),
    plot.margin = margin(15, 15, 15, 15)                                     # add margin around the plot
  ) +
  scale_y_continuous(breaks = seq(0, 170, by = 20)) +
  coord_cartesian(ylim = c(7, 163), clip = "off")

print(p1.e)
ggsave("Q2_box_plot_education_2012-2015.png", width = 11, height = 14, dpi = 300)


# Boxplot for recent data (2019 - 2022)
# jsut copy and pasted but changed some key points
p2 <- ggplot(data_recent, aes(x = continent, y = neet_youth_pct, fill = continent, colour = continent)) +
  geom_boxplot(size = 1.3, alpha = 0.6, outlier.shape = 4, outlier.size = 5, outlier.stroke = 2.2) +     # give outlyers a distinct shape and size
  stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 1.3, alpha = 0.8) +            # add error bars to the boxplot
  labs(title = "Box Plots of Youth NEET % by Continent (2019-2022)",
  y = "youth NEET %",
  subtitle = "Averaged across available years per country (2019-2022)") +
  scale_colour_manual(values = c("Africa" = "seaGreen", "Asia" = "Red", "Europe" = "dodgerBlue", "North America" = "#9C27B0", "Oceania" = "Orange", "South America" = "Brown")) +
  scale_fill_manual(values = c("Africa" = "seaGreen", "Asia" = "Red", "Europe" = "dodgerBlue", "North America" = "#9C27B0", "Oceania" = "Orange", "South America" = "Brown")) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
    plot.subtitle = element_text(hjust = 0.5, size = 16, colour = "gray15", margin = margin(b = 15)),
    plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
    legend.position = "none",
    panel.grid.major = element_line(colour = "gray85", linewidth = 0.4),     # Major grid every 
    panel.grid.minor = element_line(colour = "gray90", linewidth = 0.3),     # Faint grid every year
    panel.background = element_rect(fill = "white"),                         # white background for better contrast
    panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
    axis.title = element_text(face = "bold", size = 17.5, colour = "gray20"),
    axis.text = element_text(size = 15, colour = "gray20"),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks = element_line(colour = "gray50"),
    axis.line = element_line(colour = "gray40"),
    plot.margin = margin(15, 15, 15, 15)                                     # add margin around the plot
  ) +
  scale_y_continuous(breaks = seq(0, 50, by = 10)) +
  coord_cartesian(ylim = c(2, 50.5), clip = "off")                                           # limit y-axis to focus on relevant data range

print(p2)
ggsave("Q2_box_plot_2019-2022.png", width = 11, height = 14, dpi = 300)

# education 2019 - 2022
# (not requested but will be useful for further analysis)
p2.e <- ggplot(data_recent_education, aes(x = Continent, y = education_pct, fill = Continent, colour = Continent)) +
  geom_boxplot(size = 1.3, alpha = 0.6, outlier.shape = 4, outlier.size = 5, outlier.stroke = 2.2) +     # give outlyers a distinct shape and size
  stat_boxplot(geom = "errorbar", width = 0.3, linewidth = 1.3, alpha = 0.8) +            # add error bars to the boxplot
  labs(title = "Box Plots of % Youths Enrolled in Education by Continent (2019-2022)",
  y = "% Youths Enrolled in Education",
  subtitle = "Averaged across available years per country (2019 - 2022)") +
  scale_colour_manual(values = c("Africa" = "seaGreen", "Asia" = "Red", "Europe" = "dodgerBlue", "North America" = "#9C27B0", "Oceania" = "Orange", "South America" = "Brown")) +
  scale_fill_manual(values = c("Africa" = "seaGreen", "Asia" = "Red", "Europe" = "dodgerBlue", "North America" = "#9C27B0", "Oceania" = "Orange", "South America" = "Brown")) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 16, colour = "gray15", margin = margin(b = 15)),
    plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
    legend.position = "none",
    panel.grid.major = element_line(colour = "gray85", linewidth = 0.4),     # Major grid every 
    panel.grid.minor = element_line(colour = "gray90", linewidth = 0.3),     # Faint grid every year
    panel.background = element_rect(fill = "white"),                         # white background for better contrast
    panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
    axis.title = element_text(face = "bold", size = 16, colour = "gray20"),
    axis.text = element_text(size = 15, colour = "gray20"),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.ticks = element_line(colour = "gray50"),
    axis.line = element_line(colour = "gray40"),
    plot.margin = margin(15, 15, 15, 15)                                     # add margin around the plot
  )  +
  scale_y_continuous(breaks = seq(0, 170, by = 20)) +
  coord_cartesian(ylim = c(6, 145), clip = "off")

print(p2.e)
ggsave("Q2_box_plot_education_2019-2022.png", width = 11, height = 14, dpi = 300)
