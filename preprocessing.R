##### UPDATE DATA INPUTS #####

library(readr)
library(dplyr)
library(janitor)
library(highcharter)
library(tidyr)

# Join tables, restructure and export data

fbs_raw <- read_csv("fbs.csv") %>%
  clean_names() %>%
  pivot_longer(cols = 7:last_col(), names_to = "year", values_to = "value") %>%
  mutate(year = as.integer(parse_number(year))) %>%
  filter(element_name == "Total food supply") %>%
  select(-c("unit")) %>%
  rename(total_food_supply = value)

# Add "China, " in front of the name for Taiwan as per OCC request

fbs_raw$country_name[fbs_raw$country_code_un_m49 == "158"] <- "China, Taiwan Province of China"

pop_raw <- read_csv("population.csv") %>%
  clean_names() %>%
  pivot_longer(cols = 5:last_col(), names_to = "year", values_to = "value") %>%
  mutate(year = as.integer(parse_number(year)),
         value = value * 1000,
         unit = "Number") %>%
  select(-c("unit_name")) %>%
  rename(population = value)

# Load geographical coordinates

cou_coordinates <- read_csv("https://raw.githubusercontent.com/pamdx/country_coordinates/refs/heads/main/country_coordinates.csv") %>%
  select(un_code, lat, lon)

# Aggregate data at species group level

fbs_group <- fbs_raw %>%
  mutate(conc = paste(country_code_un_m49, "|", year)) %>%
  left_join(pop_raw %>% mutate(conc = paste(country_code_un_m49, "|", year)) %>% select(conc, population), by = "conc", relationship = "many-to-one") %>%
  select(-conc) %>%
  mutate(supply_capita_kg = total_food_supply/population*1000) %>%
  left_join(y = cou_coordinates, by = c("country_code_un_m49" = "un_code"))

saveRDS(fbs_group, "fbs_group.RDS")

# Aggregated data

fbs_total <- fbs_raw %>%
  group_by(across(c(-faostat_group_name, -total_food_supply))) %>%
  summarise(total_food_supply = sum(total_food_supply)) %>%
  ungroup() %>%
  mutate(conc = paste(country_code_un_m49, "|", year)) %>%
  left_join(pop_raw %>% mutate(conc = paste(country_code_un_m49, "|", year)) %>% select(conc, population), by = "conc") %>%
  select(-conc) %>%
  mutate(supply_capita_kg = total_food_supply/population*1000) %>%
  left_join(y = cou_coordinates, by = c("country_code_un_m49" = "un_code")) %>%
  mutate(faostat_group_name = "All species") %>%
  select(country_name, country_code_un_m49, faostat_group_name, element_name, unit_name, year, total_food_supply, population, supply_capita_kg, lat, lon)

saveRDS(fbs_total, "fbs_total.RDS")

# Download map for HC viz

# map <- download_map_data(url = "custom/world-continents.js", showinfo = FALSE, quiet = FALSE)
# 
# saveRDS(map, "map.RDS")