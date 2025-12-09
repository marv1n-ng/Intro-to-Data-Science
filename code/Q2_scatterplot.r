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
  select(-gdp_pc_ppp_2017) %>%                                                              # Remove GDP column to avoid confusion
  filter(!is.na(neet_youth_pct))                                                            # Remove rows with NA in NEET to speed up processing
additional_data_set <- read.csv("output/additional_df.csv")


# ------------- Clean additional data set -------------  
additional_data_cleaned <- additional_data_set %>% 
  select(-Investment_Share) %>%                                                             # Remove Investment Share as not reauiremed for q2
  filter(!is.na(Secondary_School_Enrollment))                                               # Remove rows to quicken data cleaning as df is so large

# ---- Join continent data to additional data set -----
education_share_continents <- additional_data_cleaned %>%
  left_join(continents, by = "Code") %>%                                                    # Join continent data
  select(-Entity, - Year)                                                                   # Removing "Year" as ther are two, one is incorrect
head(education_share_continents, 20)

# ------------ check cleaned NEET data ----------------
head(cleaned_NEET_country, 20)


# ------ Merge NEET and education data for 2012-2015 ------------
merged_data <- left_join(cleaned_NEET_country, education_share_continents, by = c("country" = "country", "year" = "year")) %>%
    select(-Code, -code, -Continent) %>%
    filter(!is.na(Secondary_School_Enrollment)) %>%                                         # Remove rows with NA in education data
    group_by(continent)
head(merged_data, 30)

# -------------- Scatterplot + regression lines per continent ---------------
# -------------------------- Africa --------------------------------
merged_data_Africa <- merged_data %>%
  filter(continent == "Africa")

# Linear model for Africa + r value
model_africa <- lm(neet_youth_pct ~ Secondary_School_Enrollment, data = merged_data_Africa)
summary(model_africa)

# Extract R-squared value
r_squared <- summary(model_africa)$r.squared
r_squared
# Extract slope and intercept
intercept <- coef(model_africa)[1]
intercept
slope <- coef(model_africa)[2]
slope

# Scatterplot for Africa
p1 <- ggplot(merged_data_Africa, aes(x = Secondary_School_Enrollment, y = neet_youth_pct))
p1 + geom_point(size = 2, colour = "seaGreen", alpha = 0.7) + 
    geom_smooth(method = "lm", se = TRUE, colour = "seaGreen", fill = "seaGreen", alpha = 0.3) +                                                  # Add linear regression lines
    labs(title = "Scatterplot of NEET youth percentage vs Secondary School Enrollment",
        subtitle = "Africa",
        x = "Secondary School Enrollment (%)",
        y = "NEET Youth Percentage (%)") +
    scale_x_continuous(breaks = seq(0, 120, by = 10)) +
    scale_y_continuous(breaks = seq(0, 50, by = 10)) +
    annotate("text", x = 75, y = 5,                                              # Adjust position to fit in gap
        label = "y = 0.045x + 22.83\nR² = 0.017",
        size = 5, fontface = "bold", color = "seaGreen") +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "gray30", margin = margin(b = 15)),
        plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
        panel.grid.major = element_line(colour = "gray90", linewidth = 0.5),     # Major grid every 5 years
        panel.grid.minor = element_line(colour = "gray95", linewidth = 0.3),     # Faint grid every year
        panel.background = element_rect(fill = "white"),                         # white background for better contrast
        panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 10, colour = "gray20"),
        axis.ticks = element_line(colour = "gray50"),
        axis.line = element_line(colour = "gray40"),
        plot.margin = margin(15, 15, 15, 15)                                    # add margin around the plot
    ) +
    coord_cartesian(ylim = c(2, 48), clip = "off")

ggsave("NEET_v_Enrollment/scatterplot_Africa.png", width = 8, height = 6)


# -------------------------- Asia --------------------------------
# merge data for Asia
merged_data_Asia <- merged_data %>%
  filter(continent == "Asia")

# Linear model for Asia + r value
model_asia <- lm(neet_youth_pct ~ Secondary_School_Enrollment, data = merged_data_Asia)
summary(model_asia)

# Extract R-squared value
r_squared <- summary(model_asia)$r.squared
r_squared
# Extract slope and intercept
intercept <- coef(model_asia)[1]
intercept
slope <- coef(model_asia)[2]
slope

# Scatterplot for Asia
p2 <- ggplot(merged_data_Asia, aes(x = Secondary_School_Enrollment, y = neet_youth_pct))
p2 + geom_point(size = 2, colour = "red", alpha = 0.7) + 
    geom_smooth(method = "lm", se = TRUE, colour = "red", fill = "red", alpha = 0.3) +               # Add linear regression lines
    labs(title = "Scatterplot of NEET youth percentage vs Secondary School Enrollment",
        subtitle = "Asia",
        x = "Secondary School Enrollment (%)",
        y = "NEET Youth Percentage (%)") +
    scale_x_continuous(breaks = seq(0, 130, by = 10)) +
    scale_y_continuous(breaks = seq(0, 50, by = 10)) +
    annotate("text", x = 72, y = 5,                                              # Adjust position to fit in gap
        label = "y = -0.14x + 32.85\nR² = 0.108",
        size = 5, fontface = "bold", color = "red") +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "gray30", margin = margin(b = 15)),
        plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
        panel.grid.major = element_line(colour = "gray90", linewidth = 0.5),     # Major grid every 5 years
        panel.grid.minor = element_line(colour = "gray95", linewidth = 0.3),     # Faint grid every year
        panel.background = element_rect(fill = "white"),                         # white background for better contrast
        panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 10, colour = "gray20"),
        axis.ticks = element_line(colour = "gray50"),
        axis.line = element_line(colour = "gray40"),
        plot.margin = margin(15, 15, 15, 15)                                    # add margin around the plot
    ) +
    coord_cartesian(ylim = c(2, 48), xlim = c(15,125), clip = "off")

ggsave("NEET_v_Enrollment/scatterplot_Asia.png", width = 8, height = 6)


# -------------------------- Europe --------------------------------
# merge data for Europe
merged_data_Europe <- merged_data %>%
  filter(continent == "Europe")

# Linear model for Europe + r^2 value
model_Europe <- lm(neet_youth_pct ~ Secondary_School_Enrollment, data = merged_data_Europe)
summary(model_Europe)

# Extract R-squared value
r_squared <- summary(model_Europe)$r.squared
r_squared
# Extract slope and intercept
intercept <- coef(model_Europe)[1]
intercept
slope <- coef(model_Europe)[2]
slope

# Scatterplot for Europe
p3 <- ggplot(merged_data_Europe, aes(x = Secondary_School_Enrollment, y = neet_youth_pct))
p3 + geom_point(size = 2, colour = "dodgerBlue", alpha = 0.7) + 
    geom_smooth(method = "lm", se = TRUE, colour = "dodgerBlue", fill = "dodgerBlue", alpha = 0.3) +               # Add linear regression lines
    labs(title = "Scatterplot of NEET youth percentage vs Secondary School Enrollment",
        subtitle = "Europe",
        x = "Secondary School Enrollment (%)",
        y = "NEET Youth Percentage (%)") +
    scale_x_continuous(breaks = seq(0, 180, by = 10)) +
    scale_y_continuous(breaks = seq(0, 45, by = 10)) +
    annotate("text", x = 130, y = 28,                                              # Adjust position to fit in gap
        label = "y = -0.18x + 31.50\nR² = 0.188",
        size = 5, fontface = "bold", color = "dodgerBlue") +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "gray30", margin = margin(b = 15)),
        plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
        panel.grid.major = element_line(colour = "gray90", linewidth = 0.5),     # Major grid every 5 years
        panel.grid.minor = element_line(colour = "gray95", linewidth = 0.3),     # Faint grid every year
        panel.background = element_rect(fill = "white"),                         # white background for better contrast
        panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 10, colour = "gray20"),
        axis.ticks = element_line(colour = "gray50"),
        axis.line = element_line(colour = "gray40"),
        plot.margin = margin(15, 15, 15, 15)                                    # add margin around the plot
    ) +
    coord_cartesian(ylim = c(2, 45), clip = "off")

ggsave("NEET_v_Enrollment/scatterplot_Europe.png", width = 8, height = 6)


# -------------------------- North America --------------------------------
# merge data for North America
merged_data_NA <- merged_data %>%
  filter(continent == "North America")

# Linear model for North America + r^2 value
model_NA <- lm(neet_youth_pct ~ Secondary_School_Enrollment, data = merged_data_NA)
summary(model_NA)

# Extract R-squared value
r_squared <- summary(model_NA)$r.squared
r_squared
# Extract slope and intercept
intercept <- coef(model_NA)[1]
intercept
slope <- coef(model_NA)[2]
slope

# Scatterplot for North America
p4 <- ggplot(merged_data_NA, aes(x = Secondary_School_Enrollment, y = neet_youth_pct))
p4 + geom_point(size = 2, colour = "#9C27B0", alpha = 0.7) + 
    geom_smooth(method = "lm", se = TRUE, colour = "#9C27B0", fill = "#9C27B0", alpha = 0.3) +           # Add linear regression lines
    labs(title = "Scatterplot of NEET youth percentage vs Secondary School Enrollment",
        subtitle = "North America",
        x = "Secondary School Enrollment (%)",
        y = "NEET Youth Percentage (%)") +
    scale_x_continuous(breaks = seq(0, 160, by = 10)) +
    scale_y_continuous(breaks = seq(0, 45, by = 10)) +
    annotate("text", x = 60, y = 10,                                              # Adjust position to fit in gap
        label = "y = -0.17x + 36.3\nR² = 0.351",
        size = 5, fontface = "bold", color = "#9C27B0") +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "gray30", margin = margin(b = 15)),
        plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
        panel.grid.major = element_line(colour = "gray90", linewidth = 0.5),     # Major grid every 5 years
        panel.grid.minor = element_line(colour = "gray95", linewidth = 0.3),     # Faint grid every year
        panel.background = element_rect(fill = "white"),                         # white background for better contrast
        panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 10, colour = "gray20"),
        axis.ticks = element_line(colour = "gray50"),
        axis.line = element_line(colour = "gray40"),
        plot.margin = margin(15, 15, 15, 15)                                    # add margin around the plot
    ) +
    coord_cartesian(ylim = c(6, 39), clip = "off")

ggsave("NEET_v_Enrollment/scatterplot_NA.png", width = 8, height = 6)


# -------------------------- Oceania --------------------------------
# merge data for Oceania
merged_data_Oceania <- merged_data %>%
  filter(continent == "Oceania")

# Linear model for Oceania + r^2 value
model_Oceania <- lm(neet_youth_pct ~ Secondary_School_Enrollment, data = merged_data_Oceania)
summary(model_Oceania)

# Extract R-squared value
r_squared <- summary(model_Oceania)$r.squared
r_squared
# Extract slope and intercept
intercept <- coef(model_Oceania)[1]
intercept
slope <- coef(model_Oceania)[2]
slope

# Scatterplot for Oceania
p5 <- ggplot(merged_data_Oceania, aes(x = Secondary_School_Enrollment, y = neet_youth_pct))
p5 + geom_point(size = 2, colour = "Orange", alpha = 0.7) + 
    geom_smooth(method = "lm", se = TRUE, colour = "Orange", fill = "Orange", alpha = 0.3) +           # Add linear regression lines
    labs(title = "Scatterplot of NEET youth percentage vs Secondary School Enrollment",
        subtitle = "Oceania",
        x = "Secondary School Enrollment (%)",
        y = "NEET Youth Percentage (%)") +
    scale_x_continuous(breaks = seq(0, 160, by = 10)) +
    scale_y_continuous(breaks = seq(0, 45, by = 10)) +
    annotate("text", x = 80, y = 10,                                              # Adjust position to fit in gap
        label = "y = -0.30x + 51.3\nR² = 0.540",
        size = 5, fontface = "bold", color = "Orange") +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "gray30", margin = margin(b = 15)),
        plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
        panel.grid.major = element_line(colour = "gray90", linewidth = 0.5),     # Major grid every 5 years
        panel.grid.minor = element_line(colour = "gray95", linewidth = 0.3),     # Faint grid every year
        panel.background = element_rect(fill = "white"),                         # white background for better contrast
        panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 10, colour = "gray20"),
        axis.ticks = element_line(colour = "gray50"),
        axis.line = element_line(colour = "gray40"),
        plot.margin = margin(15, 15, 15, 15)                                    # add margin around the plot
    ) +
    coord_cartesian(ylim = c(-4, 49), clip = "off")

ggsave("NEET_v_Enrollment/scatterplot_Oceania.png", width = 8, height = 6)


# -------------------------- South America --------------------------------
# merge data for South America
merged_data_SA <- merged_data %>%
  filter(continent == "South America")

# Linear model for South America + r^2 value
model_SA <- lm(neet_youth_pct ~ Secondary_School_Enrollment, data = merged_data_SA)
summary(model_SA)

# Extract R-squared value
r_squared <- summary(model_SA)$r.squared
r_squared
# Extract slope and intercept
intercept <- coef(model_SA)[1]
intercept
slope <- coef(model_SA)[2]
slope

# Scatterplot for South America
p6 <- ggplot(merged_data_SA, aes(x = Secondary_School_Enrollment, y = neet_youth_pct))
p6 + geom_point(size = 2, colour = "Brown", alpha = 0.7) + 
    geom_smooth(method = "lm", se = TRUE, colour = "Brown", fill = "Brown", alpha = 0.3) +           # Add linear regression lines
    labs(title = "Scatterplot of NEET youth percentage vs Secondary School Enrollment",
        subtitle = "South America",
        x = "Secondary School Enrollment (%)",
        y = "NEET Youth Percentage (%)") +
    scale_x_continuous(breaks = seq(0, 160, by = 10)) +
    scale_y_continuous(breaks = seq(0, 45, by = 10)) +
    annotate("text", x = 65, y = 12,                                              # Adjust position to fit in gap
        label = "y = 0.013x + 17.8\nR² = 0.002",
        size = 5, fontface = "bold", color = "Brown") +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "gray30", margin = margin(b = 15)),
        plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
        panel.grid.major = element_line(colour = "gray90", linewidth = 0.5),     # Major grid every 5 years
        panel.grid.minor = element_line(colour = "gray95", linewidth = 0.3),     # Faint grid every year
        panel.background = element_rect(fill = "white"),                         # white background for better contrast
        panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 10, colour = "gray20"),
        axis.ticks = element_line(colour = "gray50"),
        axis.line = element_line(colour = "gray40"),
        plot.margin = margin(15, 15, 15, 15)                                    # add margin around the plot
    ) +
    coord_cartesian(ylim = c(9, 30), clip = "off")

ggsave("NEET_v_Enrollment/scatterplot_SA.png", width = 8, height = 6)


# -------------------------- Global --------------------------------
# Linear model for Global + r^2 value
model_Global <- lm(neet_youth_pct ~ Secondary_School_Enrollment, data = merged_data)
summary(model_Global)

# Extract R-squared value
r_squared <- summary(model_Global)$r.squared
r_squared
# Extract slope and intercept
intercept <- coef(model_Global)[1]
intercept
slope <- coef(model_Global)[2]
slope

# Scatterplot for Global
p7 <- ggplot(merged_data, aes(x = Secondary_School_Enrollment, y = neet_youth_pct))
p7 + geom_point(size = 2, colour = "darkslategray", alpha = 0.7) + 
    geom_smooth(method = "lm", se = TRUE, colour = "darkslategray", fill = "darkslategray", alpha = 0.3) +           # Add linear regression lines
    labs(title = "Scatterplot of NEET youth percentage vs Secondary School Enrollment",
        subtitle = "-- Global --",
        x = "Secondary School Enrollment (%)",
        y = "NEET Youth Percentage (%)") +
    scale_x_continuous(breaks = seq(0, 160, by = 10)) +
    scale_y_continuous(breaks = seq(0, 50, by = 10)) +
    annotate("text", x = 130, y = 35,                                              # Adjust position to fit in gap
        label = "y = -0.20x + 35.4\nR² = 0.294",
        size = 5, fontface = "bold", color = "darkslategray") +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "gray30", margin = margin(b = 15)),
        plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
        panel.grid.major = element_line(colour = "gray90", linewidth = 0.5),     # Major grid every 5 years
        panel.grid.minor = element_line(colour = "gray95", linewidth = 0.3),     # Faint grid every year
        panel.background = element_rect(fill = "white"),                         # white background for better contrast
        panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(size = 10, colour = "gray20"),
        axis.ticks = element_line(colour = "gray50"),
        axis.line = element_line(colour = "gray40"),
        plot.margin = margin(15, 15, 15, 15),                                    # add margin around the plot
        aspect.ratio = 6/8                                                       # set aspect ratio for better appearance using RMD
    ) +
    coord_cartesian(ylim = c(1, 49), clip = "on")

ggsave("NEET_v_Enrollment/scatterplot_Global.png", width = 8, height = 6)

# ------------------------- end of scatterplots ------------------------------