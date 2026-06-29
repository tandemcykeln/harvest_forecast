# This main script will run all of the other scripts.
#

rm(list = ls())

library(pxweb)
library(tidyverse)
library(jsonlite)
library(httr)

library(glmnet)
library(Matrix)
library(scales)
library(openxlsx)
library(here)

# Make a variable that contain the current date
date_str <- format(Sys.Date(), "%Y%m%d")
print(date_str)

source(paste0(here(),"/code/get_smhi_data.R"))

# Make a tibble with all of the station ids and Produktionsområden
station_ids <- tribble(
  ~PO, ~Productionarea, ~temperatureID, ~precipID,
  "PO1", "Götalands södra slättbygder", 53430, 53430,
  "PO2", "Götalands mellanbygder", 64020, 64020,
  "PO3", "Götalands norra slättbygder", 82260, 82260,
  "PO4", "Svealands slättbygder", 97200, 97520, 
  "PO5", "Götalands skogsbygder", 74440, 74180,
  "PO6", "Mellersta Sveriges skogsbygder", 105370, 105370, 
  "PO7", "Nedre Norrland", 127380, 127380,
  "PO8", "Övre Norrland", 148330, 148330
)

# --- Download the latest Temperature data ---
tmp_temperature_new2 <- NULL

for (station_id in station_ids$temperatureID) {

  tmp_temperature_new <- fetch_smhi_current(
    parameter_id = 2,
    station_id = station_id,
    data_type_name = "Temperature"
  )
  
  tmp_temperature_new2 <- bind_rows(tmp_temperature_new2, tmp_temperature_new)
  
}

# --- Download the latest Precipitation data ---
tmp_precipitation_new2 <- NULL

for (station_id in station_ids$precipID) {
  
  tmp_precipitation_new <- fetch_smhi_current(
    parameter_id = 5,
    station_id = station_id,
    data_type_name = "Precipitation"
  )
  
  tmp_precipitation_new2 <- bind_rows(tmp_precipitation_new2, tmp_precipitation_new)
  
}

# --- Download old Temperature data ---
tmp_temperature_old2 <- NULL

for (station_id in station_ids$temperatureID) {
  
  tmp_temperature_old <- fetch_smhi_old(
    station_id = station_id,
    parameter_id = 2,
    data_type_name = "Temperature"
  )
  
  tmp_temperature_old2 <- bind_rows(tmp_temperature_old2, tmp_temperature_old)
  
}

# --- Download old Precipitation data ---
tmp_precipitation_old2 <- NULL

for (station_id in station_ids$precipID) {
  
  tmp_precipitation_old <- fetch_smhi_old(
    station_id = station_id,
    parameter_id = 5,
    data_type_name = "Precipitation"
  )
  
  tmp_precipitation_old2 <- bind_rows(tmp_precipitation_old2, tmp_precipitation_old)
  
}

# Join all temperature data
temperature <- anti_join(tmp_temperature_new2, tmp_temperature_old2, by = c("datetime_utc", "StationID")) |> 
  bind_rows(tmp_temperature_old2) |> 
  left_join(station_ids, by = join_by("StationID"=="temperatureID")) |> 
  select(-precipID, -StationID)

# Join all precipitation data
precipitation <- anti_join(tmp_precipitation_new2, tmp_precipitation_old2, by = c("datetime_utc", "StationID")) |> 
  bind_rows(tmp_precipitation_old2) |> 
  left_join(station_ids, by = join_by("StationID"=="precipID")) |> 
  select(-temperatureID, -StationID)

rm(list = ls(pattern = "^tmp"))

# Combine temperature and precipitation
# Calculate year and week
weather <- full_join(temperature, precipitation, by = c("datetime_utc", "PO", "Productionarea")) |> 
  arrange(PO, datetime_utc) |> 
  filter(datetime_utc >= as.Date("1965-01-01")) |> 
  mutate (
    year = year(datetime_utc),
    week_nr = isoweek(datetime_utc),
    week_nr = sprintf("%02d", week_nr),
    yearweek = paste0(year, week_nr)
  )

# Summarize the data per week
weather_week <- weather |> 
  group_by(PO, Productionarea, year, week_nr, yearweek) |> 
  summarize(
    precip_w = sum(Precipitation, na.rm = TRUE),
    temp_w = mean(Temperature, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(temp_w2 = temp_w^2)

# Pivot the data to a longer format
# I need to make some imputations as well
weather_week_long <- weather_week |> 
  pivot_longer(cols = c(precip_w, temp_w, temp_w2), names_to = "variable", values_to = "value") |> 
  mutate(value = if_else(PO == "PO8" & yearweek == "198226" & variable == "temp_w", 14.71, value),
         value = if_else(PO == "PO8" & yearweek == "198226" & variable == "temp_w2", 222.01, value),
         value = if_else(PO == "PO8" & yearweek == "198232" & variable == "temp_w", 12.93, value),
         value = if_else(PO == "PO8" & yearweek == "198232" & variable == "temp_w2", 168.05, value),
         value = if_else(PO == "PO8" & yearweek == "199429" & variable == "temp_w", 15.04, value),
         value = if_else(PO == "PO8" & yearweek == "199429" & variable == "temp_w2", 229.11, value),
         value = if_else(PO == "PO8" & yearweek == "199430" & variable == "temp_w", 15.46, value),
         value = if_else(PO == "PO8" & yearweek == "199430" & variable == "temp_w2", 241.07, value))

# Pivot the data to a wider format
weather_week_wide <- weather_week_long |> 
  pivot_wider(
    id_cols = c(PO, year),           
    names_from = c(week_nr, variable),           
    values_from = value,                         
    names_glue = "{week_nr}_{variable}",         
    values_fill = 0                              
  ) |> 
  mutate(year = as.character(year)) |> 
  rename(Year = year)

# Fetch areas and harvest
source(paste0(here(), "/code/arealer.R"))
source(paste0(here(), "/code/Skord per PO.R"))

# Only use the days of the current year
current_date <- today()
day_of_year <- yday(current_date)
current_week <- isoweek(current_date)
print(current_date)
print(day_of_year)
print(current_week)

# We have four different variables plus two variables in the beginning
nr_var <- 3

# === IF I WANT TO MAKE CALCULATIONS FROM A SPECIFIC WEEK, HERE IS THE PLACE TO DO IT ===
current_week <- 26

weather_week_wide2<- weather_week_wide[, 1:((current_week*nr_var)+2)]

# Load function that tries to estimate the harvest
source(paste0(here(), "/code/estimate_harvest.R"))

# Year that I want to predict
predict_year <- "2026"
harvest <- NULL

# --- CEREALS ---

# Höstvete and råg
PO <- c("PO1","PO2","PO3","PO4","PO5","PO6")
crops <- c("Höstvete", "Råg")
startyear <- "1965"

for (po in PO) {
  for (crop in crops)
  {
    harvest <- bind_rows(harvest, predict_harvest(crop, po, predict_year, 1, startyear))
  }
}


# Havre and Vårkorn
PO <- c("PO1","PO2","PO3","PO4","PO5","PO6", "PO7", "PO8")
crops <- c("Havre", "Vårkorn")
startyear <- "1965"

for (po in PO) {
  for (crop in crops)
  {
    harvest <- bind_rows(harvest, predict_harvest(crop, po, predict_year, 1, startyear))
  }
}

# Höstkorn for four Productionareas
PO <- c("PO1","PO2","PO3","PO5")
crops <- c("Höstkorn")
startyear <- "1995"

for (po in PO) {
  for (crop in crops)
  {
    harvest <- bind_rows(harvest, predict_harvest(crop, po, predict_year, 1, startyear))
  }
}


# Höstkorn for Svealands slättbygder
PO <- c("PO4")
crops <- c("Höstkorn")
startyear <- "2015"

for (po in PO) {
  for (crop in crops)
  {
    harvest <- bind_rows(harvest, predict_harvest(crop, po, predict_year, 1, startyear))
  }
}

# Rågvete
PO <- c("PO1","PO2","PO3","PO4","PO5","PO6")
crops <- c("Rågvete")
startyear <- "1995"

for (po in PO) {
  for (crop in crops)
  {
    harvest <- bind_rows(harvest, predict_harvest(crop, po, predict_year, 1, startyear))
  }
}

# Vårvete
PO <- c("PO1","PO2","PO3","PO4","PO5","PO6")
crops <- c("Vårvete")
startyear <- "1995"

for (po in PO) {
  for (crop in crops)
  {
    harvest <- bind_rows(harvest, predict_harvest(crop, po, predict_year, 1, startyear))
  }
}

# Vårvete in Nedre Norrland
PO <- c("PO7")
crops <- c("Vårvete")
startyear <- "2011"

for (po in PO) {
  for (crop in crops)
  {
    harvest <- bind_rows(harvest, predict_harvest(crop, po, predict_year, 0, startyear))
  }
}

# Join the predictions with the actual name of the production areas
harvest2 <- harvest |> 
  left_join(station_ids, by = "PO") |> 
  select(-temperatureID, -precipID) |> 
  rename(Harvest = lambda.min)

# We also want the areas
areas <- pd_areas4 |> 
  filter(Crop %in% c("Höstvete", "Råg", "Rågvete", "Vårvete", "Höstkorn", "Vårkorn", "Havre") & Year == predict_year) |> 
  left_join(station_ids, by = "Productionarea") |> 
  mutate(Year = as.character(Year)) |> 
  select(-temperatureID, -precipID)

# Combine the two tables
harvest3 <- harvest2 |> 
  full_join(areas, by = c("Year", "PO", "Productionarea", "Crop")) |> 
  mutate(Harvest = if_else(is.na(Harvest), 1000, Harvest),
         Hectares = if_else(is.na(Hectares), 0, Hectares),
         Total_harvest = round(Harvest * Hectares / 1E6, 0)) |> 
  arrange(PO, Crop)

# The total harvest for all of the country
harvest_country <- harvest3 |> 
  group_by(Year, Crop) |> 
  summarize(Total_harvest = sum(Total_harvest),
            .groups = "drop") |> 
  mutate(PO = "Country",
         Productionarea = "Country")

# The total harvest for the production areas
harvest_total <- harvest3 |>
  select(-c("Harvest", "Hectares")) |>
  bind_rows(harvest_country) |> 
  pivot_wider(names_from = Crop, values_from = Total_harvest) |> 
  mutate(Tot_cereals = rowSums(across(c(Höstvete, Vårvete, Höstkorn, Vårkorn, Havre, Råg, Rågvete)))) |> 
  select(Year, PO, Productionarea, Höstvete, Vårvete, Höstkorn, Vårkorn, Havre, Råg, Rågvete, Tot_cereals)

rm("harvest", "harvest2", "harvest3", "harvest_country", "areas")

write.xlsx(harvest_total, paste0(here(), "/output/cereals_", current_date, ".xlsx"))


# Potatoes
predict_year <- "2026"
harvest_potatoes <- NULL

PO <- c("PO1","PO2","PO3","PO4","PO5","PO6", "PO7", "PO8")
crops <- c("Matpotatis")
startyear <- "2002"

for (po in PO) {
  for (crop in crops)
  {
    harvest_potatoes <- bind_rows(harvest_potatoes, predict_harvest(crop, po, predict_year, 0.9, startyear))
  }
}

# Join the predictions with the actual name of the production areas
harvest_potatoes2 <- harvest_potatoes |> 
  left_join(station_ids, by = "PO") |> 
  select(-temperatureID, -precipID) |> 
  rename(Harvest = lambda.min)

# We also want the areas
areas <- pd_areas4 |> 
  filter(Crop %in% c("Matpotatis") & Year == predict_year) |> 
  left_join(station_ids, by = "Productionarea") |> 
  mutate(Year = as.character(Year)) |> 
  select(-temperatureID, -precipID)

# Combine the two tables
harvest_potatoes3 <- harvest_potatoes2 |> 
  full_join(areas, by = c("Year", "PO", "Productionarea", "Crop")) |> 
  mutate(Harvest = if_else(is.na(Harvest), 1000, Harvest),
         Hectares = if_else(is.na(Hectares), 0, Hectares),
         Total_harvest = round(Harvest * Hectares / 1E6, 0)) |> 
  arrange(PO, Crop)

# The total harvest for all of the country
harvest_potatoes_country <- harvest_potatoes3 |> 
  group_by(Year, Crop) |> 
  summarize(Total_harvest = sum(Total_harvest),
            .groups = "drop") |> 
  mutate(PO = "Country",
         Productionarea = "Country")

# The total harvest for the production areas
harvest_potatoes_total <- harvest_potatoes3 |>
  select(-c("Harvest", "Hectares")) |>
  bind_rows(harvest_potatoes_country) |> 
  pivot_wider(names_from = Crop, values_from = Total_harvest)

rm("harvest_potatoes", "harvest_potatoes2", "harvest_potatoes3", "harvest_potatoes_country", "areas")

write.xlsx(harvest_potatoes_total, paste0(here(), "/output/potatoes_", current_date, ".xlsx"))


# Slåttervall
predict_year <- "2026"
harvest_slatter <- NULL

PO <- c("PO1","PO2","PO3","PO4","PO5","PO6", "PO7", "PO8")
crops <- c("Slåttervall")
startyear <- "2002"

for (po in PO) {
  for (crop in crops)
  {
    harvest_slatter <- bind_rows(harvest_slatter, predict_harvest(crop, po, predict_year, 1.0, startyear))
  }
}

# Join the predictions with the actual name of the production areas
harvest_slatter2 <- harvest_slatter |> 
  left_join(station_ids, by = "PO") |> 
  select(-temperatureID, -precipID) |> 
  rename(Harvest = lambda.min)

# We also want the areas
areas <- pd_areas4 |> 
  filter(Crop %in% c("Slåttervall") & Year == predict_year) |> 
  left_join(station_ids, by = "Productionarea") |> 
  mutate(Year = as.character(Year)) |> 
  select(-temperatureID, -precipID)

# Combine the two tables
harvest_slatter3 <- harvest_slatter2 |> 
  full_join(areas, by = c("Year", "PO", "Productionarea", "Crop")) |> 
  mutate(Harvest = if_else(is.na(Harvest), 1000, Harvest),
         Hectares = if_else(is.na(Hectares), 0, Hectares),
         Total_harvest = round(Harvest * Hectares / 1E6, 0)) |> 
  arrange(PO, Crop)

# The total harvest for all of the country
harvest_slatter_country <- harvest_slatter3 |> 
  group_by(Year, Crop) |> 
  summarize(Total_harvest = sum(Total_harvest),
            .groups = "drop") |> 
  mutate(PO = "Country",
         Productionarea = "Country")

# The total harvest for the production areas
harvest_slatter_total <- harvest_slatter3 |>
  select(-c("Harvest", "Hectares")) |>
  bind_rows(harvest_slatter_country) |> 
  pivot_wider(names_from = Crop, values_from = Total_harvest)

rm("harvest_slatter", "harvest_slatter2", "harvest_slatter3", "harvest_slatter_country", "areas")

write.xlsx(harvest_slatter_total, paste0(here(), "/output/slattervall_", current_date, ".xlsx"))
