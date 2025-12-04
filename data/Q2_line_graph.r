
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
cleaned_NEET <- read.csv("output/cleaned_continent_panel.csv") %>%
  select(-mean_gdp_pc_ppp_2017, -gdp_pc_growth_pct)
additional_data_set <- read.csv("output/additional_df.csv")

# ------------- Clean additional data set -------------  
additional_data_cleaned <- additional_data_set %>% 
  select(-Investment_Share) %>%                                                             # Remove Investment Share as not reauiremed for q2
  filter(!is.na(Secondary_School_Enrollment))                                               # Remove rows to quicken data cleaning as df is so large

# ---- Join continent data to additional data set ------
education_share_continents <- additional_data_cleaned %>%
  left_join(continents, by = "Code") %>%                                                       # Join continent data
  select(-Entity, -Year)

# ----------------- View datasets ---------------------
head(continents)
head(gdp_per_capita)
head(NEET)
head(cleaned_NEET)                  # this is far better than the original NEET dataset as all combined + alread pre combined continent info
head(education_share_continents)
# LIMTATIONS as some countires been removed due to missing continent values + not all years have data for every country




# ------------- remove rows with NA youth NEET values -----------------
cleaned_NEET <- cleaned_NEET %>%
  filter(!is.na(mean_neet_youth_pct))
head(cleaned_NEET)

# -------------------------- line plot -------------------------

p <- ggplot(cleaned_NEET, aes(x = year, y = mean_neet_youth_pct, color = continent, group = continent))
p + geom_line(linewidth = 1.1, alpha = 0.7) +       # opaque lines to show trends when lines overlap
  geom_point(size = 2.5, alpha = 0.8) +
  scale_colour_manual(values = c("Africa" = "seaGreen", "Asia" = "Red", "Europe" = "dodgerBlue", "North America" = "#9C27B0", "Oceania" = "Orange", "South America" = "Brown")) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5),                      # Show years every 5 years
    minor_breaks = seq(1990, 2025, by = 1),                                 # Grid lines every year
    expand = expansion(mult = 0.025)) +                                     # 2.5% padding on x-axis
  scale_y_continuous(expand = expansion(mult = 0.1)) +                      # 10% padding on y-axis
  labs(title = "Visualisation of Average Youth NEET Percentage by Continent over Time",
       subtitle = "Helping to analyse progress towards UN SDG 8",
       x = "Year",
       y = "Average Youth NEET Percentage") +                               # improved labels
  theme(
    plot.title = element_text(face = "bold", hjust = 0.3, size = 14),
    plot.subtitle = element_text(hjust = 0.4, size = 11, colour = "gray30", margin = margin(b = 15)),
    plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "#f5f5f5", colour = NA),
    legend.key = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(colour = "gray90", linewidth = 0.5),     # Major grid every 5 years
    panel.grid.minor = element_line(colour = "gray95", linewidth = 0.3),     # Faint grid every year
    panel.background = element_rect(fill = "white"),                        # white background for better contrast
    panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10, colour = "gray20"),
    axis.ticks = element_line(colour = "gray50"),
    axis.line = element_line(colour = "gray40"),
    plot.margin = margin(15, 15, 15, 15)                                    # add margin around the plot
  ) +
  guides(colour = guide_legend(title = "Continent")) +
  theme(legend.position = "bottom", legend.title = element_blank()) 

# --------------------- Save the plot ---------------------
ggsave("Q2_line_graph.png", width = 10, height = 6, dpi = 300)


# --- now calculate a year average per continent for enrolement----
enrollment_per_year_continent <- education_share_continents %>%
  group_by(Continent, year) %>%
  summarise(
    mean_enrollement = mean(Secondary_School_Enrollment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Continent, year)

head(enrollment_per_year_continent)

 
# ---- plot average youth enrollment in education per continent over time ----
# -------------------------- line plot -------------------------
p <- ggplot(enrollment_per_year_continent, aes(x = year, y = mean_enrollement, color = Continent, group = Continent))
p + geom_line(linewidth = 1.1, alpha = 0.7) +       # opaque lines to show trends when lines overlap
  geom_point(size = 2.5, alpha = 0.8) +
  scale_colour_manual(values = c("Africa" = "seaGreen", "Asia" = "Red", "Europe" = "dodgerBlue", "North America" = "#9C27B0", "Oceania" = "Orange", "South America" = "Brown")) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5),                      # Show years every 5 years
    minor_breaks = seq(1990, 2025, by = 1),                                 # Grid lines every year
    expand = expansion(mult = 0.025)) +                                     # 2.5% padding on x-axis
  scale_y_continuous(expand = expansion(mult = 0.1)) +                      # 10% padding on y-axis
  labs(title = "Visualisation of Average Youths in education by Continent over Time",
       subtitle = "Helping to analyse progress towards UN SDG 8",
       x = "Year",
       y = "Average Youth Enrollment in Education Percentage") +                               # improved labels
  theme(
    plot.title = element_text(face = "bold", hjust = 0.3, size = 14),
    plot.subtitle = element_text(hjust = 0.4, size = 11, colour = "gray30", margin = margin(b = 15)),
    plot.background = element_rect(fill = "#f5f5f5", colour = NA),         # warm light gray background for better contrast
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "#f5f5f5", colour = NA),
    legend.key = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(colour = "gray90", linewidth = 0.5),     # Major grid every 5 years
    panel.grid.minor = element_line(colour = "gray95", linewidth = 0.3),     # Faint grid every year
    panel.background = element_rect(fill = "white"),                        # white background for better contrast
    panel.border = element_rect(colour = "gray70", linewidth = 0.8),         # border around plot area
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10, colour = "gray20"),
    axis.ticks = element_line(colour = "gray50"),
    axis.line = element_line(colour = "gray40"),
    plot.margin = margin(15, 15, 15, 15)                                    # add margin around the plot
  ) +
  guides(colour = guide_legend(title = "Continent")) +
  theme(legend.position = "bottom", legend.title = element_blank()) 

# --------------------- Save the plot ---------------------
ggsave("Q2_line_graph_education.png", width = 10, height = 6, dpi = 300)


# --------------------- End of Q2_p1.r ---------------------