###############################################
# 01_data_cleaning.R
# Full cleaning + merging pipeline for SDG 8
###############################################

library(tidyverse)
library(janitor)
library(here)

options(dplyr.summarise.inform = FALSE)

# ---- Import raw data ----
continents_raw <- read_csv(here("data", "continents-according-to-our-world-in-data.csv"))
gdp_raw <- read_csv(here("data", "gdp-per-capita-worldbank.csv"))
neet_raw <- read_csv(here("data", "youth-not-in-education-employment-training.csv"))

# ---- Clean data ----
continents <- continents_raw %>%
  clean_names() %>%
  transmute(
    country = entity,
    code = code,
    year = year,
    continent = continent
  ) %>%
  filter(continent != "Antarctica")

gdp <- gdp_raw %>%
  clean_names() %>%
  transmute(
    country = entity,
    code = code,
    year = year,
    gdp_pc_ppp_2017 = gdp_per_capita_ppp_constant_2017_international
  ) %>%
  drop_na(code, year)

neet <- neet_raw %>%
  clean_names() %>%
  transmute(
    country = entity,
    code = code,
    year = year,
    neet_youth_pct = share_of_youth_not_in_education_employment_or_training_total_of_youth_population
  ) %>%
  drop_na(code, year)

# ---- Merge datasets ----
data_country <- continents %>%
  inner_join(gdp, by = c("country", "code", "year")) %>%
  inner_join(neet, by = c("country", "code", "year")) %>%
  arrange(continent, country, year)

# ---- Continent-year averages ----
data_continent <- data_country %>%
  group_by(continent, year) %>%
  summarise(
    mean_gdp_pc_ppp_2017 = mean(gdp_pc_ppp_2017, na.rm = TRUE),
    mean_neet_youth_pct = mean(neet_youth_pct, na.rm = TRUE),
    n_countries = n()
  ) %>%
  ungroup() %>%
  arrange(continent, year)

# ---- GDP per capita growth ----
data_continent <- data_continent %>%
  group_by(continent) %>%
  mutate(
    gdp_pc_growth_pct =
      (mean_gdp_pc_ppp_2017 / lag(mean_gdp_pc_ppp_2017) - 1) * 100
  ) %>%
  ungroup()

# ---- Save outputs ----
if (!dir.exists(here("output"))) dir.create(here("output"))

write_csv(data_country, here("output", "cleaned_country_level.csv"))
write_csv(data_continent, here("output", "cleaned_continent_panel.csv"))

cat("Data cleaned and saved to /output.\n")
