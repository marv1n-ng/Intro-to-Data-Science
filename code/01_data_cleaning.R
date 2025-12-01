###############################################
# 01_data_cleaning.R
# Cleaning + merging pipeline for SDG 8
###############################################

library(tidyverse)
library(janitor)
library(here)

options(dplyr.summarise.inform = FALSE)

# ---- 1. Import raw data ----

continents_raw <- read_csv(
  here("data", "continents-according-to-our-world-in-data.csv"),
  show_col_types = FALSE
)

gdp_raw <- read_csv(
  here("data", "gdp-per-capita-worldbank.csv"),
  show_col_types = FALSE
)

neet_raw <- read_csv(
  here("data", "youth-not-in-education-employment-training.csv"),
  show_col_types = FALSE
)


# ---- 2. Clean individual datasets ----

# 2.1 Continents: time-invariant mapping (no year)
continents <- continents_raw %>%
  clean_names() %>%
  distinct(entity, code, continent) %>%        # drop duplicate rows if any
  transmute(
    country   = entity,
    code      = code,
    continent = continent
  ) %>%
  filter(continent != "Antarctica")            # exclude Antarctica


# 2.2 GDP per capita (PPP, 2017 USD)
gdp <- gdp_raw %>%
  clean_names() %>%
  transmute(
    country         = entity,
    code            = code,
    year            = year,
    gdp_pc_ppp_2017 =
      gdp_per_capita_ppp_constant_2017_international
  ) %>%
  drop_na(code, year)                          # keep only valid country-year rows


# 2.3 Youth NEET (% of youth population)
neet <- neet_raw %>%
  clean_names() %>%
  transmute(
    country       = entity,
    code          = code,
    year          = year,
    neet_youth_pct =
      share_of_youth_not_in_education_employment_or_training_total_percent_of_youth_population
  ) %>%
  drop_na(code, year)


# ---- 3. Merge to country-year panel ----

data_country <- gdp %>%
  # add continent (time-invariant)
  left_join(continents, by = c("country", "code")) %>%
  # add NEET where available
  left_join(
    neet %>% select(code, year, neet_youth_pct),
    by = c("code", "year")
  ) %>%
  # keep only real continents (drop aggregates like World, High income, etc.)
  filter(!is.na(continent)) %>%
  relocate(continent, .after = country) %>%
  arrange(continent, country, year)


# ---- 4. Continent-year averages ----

data_continent <- data_country %>%
  group_by(continent, year) %>%
  summarise(
    mean_gdp_pc_ppp_2017 = mean(gdp_pc_ppp_2017, na.rm = TRUE),
    mean_neet_youth_pct  = mean(neet_youth_pct,  na.rm = TRUE),
    n_countries          = n(),
    .groups = "drop"
  ) %>%
  arrange(continent, year)


# ---- 5. GDP per capita growth (% YoY) ----

data_continent <- data_continent %>%
  group_by(continent) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    gdp_pc_growth_pct =
      (mean_gdp_pc_ppp_2017 / lag(mean_gdp_pc_ppp_2017) - 1) * 100
  ) %>%
  ungroup()


# ---- 6. Export cleaned datasets ----

if (!dir.exists(here("output"))) dir.create(here("output"))

write_csv(data_country,   here("output", "cleaned_country_level.csv"))
write_csv(data_continent, here("output", "cleaned_continent_panel.csv"))


# ---- View data ----
country_data <- read_csv("output/cleaned_country_level.csv")
continent_data <- read_csv("output/cleaned_continent_panel.csv")

View(country_data)
View(continent_data)
