##### UPDATE DATA INPUTS #####

library(readr)
library(dplyr)
library(janitor)
library(highcharter)
library(tidyr)

# Join tables, restructure and export data

fbs_raw <- read_csv("fbs.csv", na = "") %>% # adding na = "" so that Namibia's ISO2 code isn't interpreted as a missing value
  clean_names() %>%
  pivot_longer(cols = 7:last_col(), names_to = "year", values_to = "value") %>%
  mutate(year = as.integer(parse_number(year))) %>%
  filter(element_name == "Total food supply") %>%
  select(-c("unit")) %>%
  rename(total_food_supply = value)

fbs_raw$country_iso2_code[fbs_raw$country_name == "Belgium-Luxembourg"] <- "BX" # Fix lack of ISO2 for Belgium-Luxembourg
fbs_raw$country_iso2_code[fbs_raw$country_name == "Czechoslovakia"] <- "CE" # Fix lack of ISO2 for Czechoslovakia
fbs_raw$country_iso2_code[fbs_raw$country_name == "Ethiopia PDR"] <- "EX" # Fix lack of ISO2 for Ethiopia PDR
fbs_raw$country_iso2_code[fbs_raw$country_name == "Other nei"] <- "XX" # Fix lack of ISO2 for Other nei
fbs_raw$country_iso2_code[fbs_raw$country_name == "Sudan (former)"] <- "SF" # Fix lack of ISO2 for Sudan (former)

# Add "China, " in front of the name for Taiwan as per OCC request

fbs_raw$country_name[fbs_raw$country_name == "Taiwan Province of China"] <- "China, Taiwan Province of China"

pop_raw <- read_csv("population.csv", na = "") %>% # adding na = "" so that Namibia's ISO2 code isn't interpreted as a missing value
  clean_names() %>%
  pivot_longer(cols = 5:last_col(), names_to = "year", values_to = "value") %>%
  mutate(year = as.integer(parse_number(year)),
         value = value * 1000,
         unit = "Number") %>%
  select(-c("unit_name")) %>%
  rename(population = value)

pop_raw$country_iso2_code[pop_raw$country_name == "Belgium-Luxembourg"] <- "BX" # Fix lack of ISO2 for Belgium-Luxembourg
pop_raw$country_iso2_code[pop_raw$country_name == "Czechoslovakia"] <- "CE" # Fix lack of ISO2 for Czechoslovakia
pop_raw$country_iso2_code[pop_raw$country_name == "Ethiopia PDR"] <- "EX" # Fix lack of ISO2 for Ethiopia PDR
pop_raw$country_iso2_code[pop_raw$country_name == "Sudan (former)"] <- "SP" # Fix lack of ISO2 for Sudan (former)

# Join geographical coordinates

cou_coordinates <- read_csv("https://raw.githubusercontent.com/google/dspl/master/samples/google/canonical/countries.csv", na = "") %>%
  rename(ISO2 = country) %>%
  select(ISO2, latitude, longitude) %>%
  add_row(ISO2 = "BX", latitude = 50.108601, longitude = 5.318428) %>% # Add Belgium-Luxembourg's coordinates using a random point between Brussels and Luxembourg
  add_row(ISO2 = "CE", latitude = 50.0755, longitude = 14.4378) %>% # Add Czechoslovakia's coordinates using its capital's coordinates
  add_row(ISO2 = "EX", latitude = 9.0192, longitude = 38.7525) %>% # Add Ethiopia PDR's coordinates using its capital's coordinates
  add_row(ISO2 = "SP", latitude = 15.5974, longitude = 32.5356) %>% # Add Sudan (former)'s coordinates using its capital's coordinates
  add_row(ISO2 = "XX", latitude = -50, longitude = 0) %>% # Add Other nei's coordinates, setting them arbitrarily in the Southern Ocean
  add_row(ISO2 = "SU", latitude = 61.524010, longitude = 105.318756) %>% # Add USSR using Russia's coordinates
  add_row(ISO2 = "BQ", latitude = 12.201890, longitude = -68.262383) %>% # Add Bonaire's coordinates
  add_row(ISO2 = "CW", latitude = 12.169570, longitude = -68.990021) %>% # Add Curaçao's coordinates
  add_row(ISO2 = "CS", latitude = 44.016521, longitude = 21.005859) %>% # Add Serbia and Montenegro using Serbia's coordinates
  add_row(ISO2 = "SS", latitude = 4.859363, longitude = 31.571251) %>% # Add South Sudan's coordinates
  add_row(ISO2 = "YU", latitude = 44.016521, longitude = 21.005859) %>% # Add Yugoslavia using Serbia's coordinates
  add_row(ISO2 = "ZZ", latitude = -6.165917, longitude = 39.202641) %>% # Add Zanzibar using its capital's coordinates
  add_row(ISO2 = "CP", latitude = 49.354417, longitude = -2.372106) %>% # Add Channel Islands using an arbitrary point in the English Channel
  add_row(ISO2 = "SX", latitude = 18.0237, longitude = -63.0458) %>% # Add Sint Maarten using its capital's coordinates
  add_row(ISO2 = "SF", latitude = 18.0731, longitude = -63.0822) %>% # Add Saint-Martin (French) using its capital's coordinates
  add_row(ISO2 = "SW", latitude = 17.897908, longitude = -62.850556) # Add Saint Barthélemy using its capital's coordinates

# Aggregate data at species group level

fbs_group <- fbs_raw %>%
  mutate(conc = paste(country_iso2_code, "|", year)) %>%
  left_join(pop_raw %>% mutate(conc = paste(country_iso2_code, "|", year)) %>% select(conc, population), by = "conc", relationship = "many-to-one") %>%
  select(-conc) %>%
  mutate(supply_capita_kg = total_food_supply/population*1000) %>%
  left_join(y = cou_coordinates, by = c("country_iso2_code" = "ISO2")) %>%
  rename(lat = latitude, lon = longitude)

saveRDS(fbs_group, "fbs_group.RDS")

# Aggregated data

fbs_total <- fbs_raw %>%
  group_by(across(c(-faostat_group_name, -total_food_supply))) %>%
  summarise(total_food_supply = sum(total_food_supply)) %>%
  ungroup() %>%
  mutate(conc = paste(country_iso2_code, "|", year)) %>%
  left_join(pop_raw %>% mutate(conc = paste(country_iso2_code, "|", year)) %>% select(conc, population), by = "conc") %>%
  select(-conc) %>%
  mutate(supply_capita_kg = total_food_supply/population*1000) %>%
  left_join(y = cou_coordinates, by = c("country_iso2_code" = "ISO2")) %>%
  rename(lat = latitude, lon = longitude) %>%
  mutate(faostat_group_name = "All species") %>%
  select(country_name, country_iso2_code, faostat_group_name, element_name, unit_name, year, total_food_supply, population, supply_capita_kg, lat, lon)

saveRDS(fbs_total, "fbs_total.RDS")

# Download map for HC viz

# map <- download_map_data(url = "custom/world-continents.js", showinfo = FALSE, quiet = FALSE)
# 
# saveRDS(map, "map.RDS")