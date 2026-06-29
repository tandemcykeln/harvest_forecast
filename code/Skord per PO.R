# Download the harvest per production area in Sweden

# Query to the database
pxweb_query_list <- 
  list("Produktionsområde"=c("1","2","3","4","5","6","7","8"),
       "Gröda"=c("0","1","2","3","4","5","6","7","8","23","22","25","26","27"),
       "Variabel"=c("0","2","3"),
       "Tabelluppgift"=c("0"),
       "År"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59", "60"))

# Download data 
px_data <- 
  pxweb_get(url = "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets%20statistikdatabas/Skordar/JO0601J02.px",
            query = pxweb_query_list)

# Convert to data.frame 
pd_harvest <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Drop a column that we don't need
pd_harvest <- subset(pd_harvest, select = -c(Tabelluppgift))

# Change the names
pd_harvest2 <- pd_harvest %>%
  rename(
    Harvest_ha = "Hektarskörd, kg/hektar",
    Productionarea = "Produktionsområde",
    Area = "Areal, hektar",
    Tot_harvest = "Totalskörd, ton",
    Crop = "Gröda",
    Year = "År"
  )

# Change rågvete 
df_sum <- pd_harvest2 %>%
  filter((Crop=="Vårrågvete" | Crop=="Höstrågvete") & Year>=2015 & Year<=2020) |> 
  group_by(Productionarea, Year) |> 
  summarize(
    Area = sum(Area, na.rm = TRUE),
    Tot_harvest = sum(Tot_harvest, na.rm = TRUE),
    .groups = 'drop'
  ) |> 
  mutate(Crop = "Rågvete",
         Harvest_ha = Tot_harvest/Area*1000)

# Add the two dataframes
pd_harvest2 <- pd_harvest2 %>%
  filter(!Crop %in% c("Vårrågvete", "Höstrågvete")) |> 
  filter(!(Crop == "Rågvete" & Year>=2015 & Year<=2020))


pd_harvest3 <- bind_rows(pd_harvest2, df_sum) |> 
  arrange(Productionarea,Crop,Year)

# We need to impute some values
pd_harvest4 <- pd_harvest3 %>%
  mutate(
    Harvest_ha = case_when(
      # Övre Norrland - Havre
      Productionarea == "Övre Norrland" & Crop == "Havre" & Year == "1966" ~ 1000,
      Productionarea == "Övre Norrland" & Crop == "Havre" & Year == "1967" ~ 2000,
      Productionarea == "Övre Norrland" & Crop == "Havre" & Year == "1993" ~ 2100,
      Productionarea == "Övre Norrland" & Crop == "Havre" & Year == "1994" ~ 1900,
      Productionarea == "Övre Norrland" & Crop == "Havre" & Year == "1997" ~ 1000,
      Productionarea == "Övre Norrland" & Crop == "Havre" & Year == "1999" ~ 600,
      
      # Götalands skogsbygder - Råg
      Productionarea == "Götalands skogsbygder" & Crop == "Råg" & Year == "1994" ~ 3800,
      Productionarea == "Götalands skogsbygder" & Crop == "Råg" & Year == "1999" ~ 3700,
      Productionarea == "Götalands skogsbygder" & Crop == "Råg" & Year == "2011" ~ 4200,
      Productionarea == "Götalands skogsbygder" & Crop == "Råg" & Year == "2012" ~ 5600,
      Productionarea == "Götalands skogsbygder" & Crop == "Råg" & Year == "2018" ~ 4600,
      
      # Mellersta Sveriges skogsbygder - Råg
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "1994" ~ 4050,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "1995" ~ 5300,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "1997" ~ 3750,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "1998" ~ 3500,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "1999" ~ 3450,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2002" ~ 4400,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2007" ~ 4825,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2011" ~ 2950,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2012" ~ 4150,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2013" ~ 2270,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2014" ~ 3850,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2015" ~ 4225,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2016" ~ 4225,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2017" ~ 4150,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2018" ~ 3150,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2020" ~ 5650,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2024" ~ 4850,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Råg" & Year == "2025" ~ 5300,
      
      
      # Götalands norra slättbygder - Höstkorn
      Productionarea == "Götalands norra slättbygder" & Crop == "Höstkorn" & Year == "1996" ~ 4032,
      Productionarea == "Götalands norra slättbygder" & Crop == "Höstkorn" & Year == "1997" ~ 4794,
      Productionarea == "Götalands norra slättbygder" & Crop == "Höstkorn" & Year == "1998" ~ 4670,
      Productionarea == "Götalands norra slättbygder" & Crop == "Höstkorn" & Year == "1999" ~ 4852,
      Productionarea == "Götalands norra slättbygder" & Crop == "Höstkorn" & Year == "2001" ~ 6281,
      Productionarea == "Götalands norra slättbygder" & Crop == "Höstkorn" & Year == "2002" ~ 6702,
      Productionarea == "Götalands norra slättbygder" & Crop == "Höstkorn" & Year == "2003" ~ 5096,
      Productionarea == "Götalands norra slättbygder" & Crop == "Höstkorn" & Year == "2004" ~ 6740,
      Productionarea == "Götalands norra slättbygder" & Crop == "Höstkorn" & Year == "2005" ~ 6651,
      Productionarea == "Götalands norra slättbygder" & Crop == "Höstkorn" & Year == "2006" ~ 5147,
      
      # Götalands skogsbygder - Höstkorn
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "1995" ~ 4000,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "1998" ~ 3731,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "1999" ~ 3876,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2000" ~ 3472,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2001" ~ 3754,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2002" ~ 4005,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2003" ~ 3046,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2004" ~ 4028,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2005" ~ 3975,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2006" ~ 3076,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2007" ~ 3784,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2008" ~ 4226,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2011" ~ 3861,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2012" ~ 6724,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2013" ~ 5577,
      Productionarea == "Götalands skogsbygder" & Crop == "Höstkorn" & Year == "2014" ~ 6165,
      
      # Others (Rågvete, Vårkorn, Vårvete)
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Rågvete" & Year == "1995" ~ 4200,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Rågvete" & Year == "1999" ~ 4280,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Rågvete" & Year == "2011" ~ 3586,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Rågvete" & Year == "2013" ~ 3925,
      
      Productionarea == "Nedre Norrland" & Crop == "Vårkorn" & Year == "1993" ~ 3271,
      Productionarea == "Nedre Norrland" & Crop == "Vårkorn" & Year == "1994" ~ 2347,
      
      Productionarea == "Övre Norrland" & Crop == "Vårkorn" & Year == "1993" ~ 3522,
      Productionarea == "Övre Norrland" & Crop == "Vårkorn" & Year == "1994" ~ 4645,
      
      Productionarea == "Götalands skogsbygder" & Crop == "Vårvete" & Year == "1993" ~ 3716,
      Productionarea == "Götalands skogsbygder" & Crop == "Vårvete" & Year == "1994" ~ 2644,
      Productionarea == "Götalands skogsbygder" & Crop == "Vårvete" & Year == "1996" ~ 3540,
      
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Vårvete" & Year == "1989" ~ 3316,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Vårvete" & Year == "1990" ~ 4295,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Vårvete" & Year == "1991" ~ 4160,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Vårvete" & Year == "1992" ~ 2383,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Vårvete" & Year == "1993" ~ 4178,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Vårvete" & Year == "1994" ~ 2972,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Vårvete" & Year == "1995" ~ 3564,
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Vårvete" & Year == "1996" ~ 3980,
      
      # Potatis för stärkelse
      Productionarea == "Götalands södra slättbygder" & Crop == "Potatis för stärkelse" & Year == "2011" ~ 41195,
      Productionarea == "Götalands södra slättbygder" & Crop == "Potatis för stärkelse" & Year == "2013" ~ 45310,
      Productionarea == "Götalands södra slättbygder" & Crop == "Potatis för stärkelse" & Year == "2015" ~ 45233,
      Productionarea == "Götalands södra slättbygder" & Crop == "Potatis för stärkelse" & Year == "2018" ~ 36317,
      Productionarea == "Götalands södra slättbygder" & Crop == "Potatis för stärkelse" & Year == "2019" ~ 44488,
      
      # Slåttervall
      Productionarea == "Mellersta Sveriges skogsbygder" & Crop == "Slåttervall, total vallskörd" & Year == "2008" ~ 3378,
      
      # Default: keep existing value
      .default = Harvest_ha 
    ),
    # Rename crop after updating values
    Crop = if_else(Crop == "Slåttervall, total vallskörd", "Slåttervall", Crop)
  ) |> 
  filter(Crop != "Slåttervall, första skörd" & Crop != "Slåttervall, återväxt") |> 
  left_join(station_ids, by = "Productionarea") |> 
  select(-c("temperatureID", "precipID"))

rm("pd_harvest", "pd_harvest2", "pd_harvest3")


### Optional if you want the total real harvest according to a survey
# Get the total real harvest
pxweb_query_list <- 
  list("Produktionsområde"=c("0"),
       "Gröda"=c("0","1","2","3","4","5","6","7","8","11"),
       "Variabel"=c("0","2","3"),
       "Tabelluppgift"=c("0"),
       "År"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60"))

# Download data 
px_data <- 
  pxweb_get(url = "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets%20statistikdatabas/Skordar/JO0601J02.px",
            query = pxweb_query_list)

# Convert to data.frame 
pd_harvest_real <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Drop a column that we don't need
pd_harvest_real <- subset(pd_harvest_real, select = -c(Tabelluppgift))

# Change the names
pd_harvest_real2 <- pd_harvest_real %>%
  rename(
    Harvest_ha = "Hektarskörd, kg/hektar",
    Productionarea = "Produktionsområde",
    Area = "Areal, hektar",
    Tot_harvest = "Totalskörd, ton",
    Crop = "Gröda",
    Year = "År"
  )

# Change rågvete 
df_sum <- pd_harvest_real2 %>%
  filter((Crop=="Vårrågvete" | Crop=="Höstrågvete") & Year>=2015 & Year<=2020) %>%
  group_by(Productionarea, Year) %>%
  summarize(
    Tot_harvest = sum(Tot_harvest, na.rm = TRUE),
    .groups = 'drop'
  )
df_sum$Crop <- "Rågvete"

# Add the two dataframes
remove_crops <- c("Vårrågvete", "Höstrågvete")
pd_harvest_real2 <- pd_harvest_real2 %>%
  filter(!Crop %in% remove_crops)

pd_harvest_real2 <- pd_harvest_real2 %>%
  filter(!(Crop == "Rågvete" & Year>=2015 & Year<=2020))


# Notera att den totala skörden inte finns för vissa år
pd_harvest_real3 <- bind_rows(pd_harvest_real2, df_sum) %>%
  select(-c(Harvest_ha, Area)) %>%
  arrange(Productionarea,Crop,Year) %>%
  rename(predictedYear = Year) %>%
  mutate(
    Crop = if_else(Crop == "Spannmål totalt", "Cereals", Crop)
  )
print(head(pd_harvest_real3))

# We need the total harvest of cereals for 1965-2017
tot_cereals <- pd_harvest_real3 %>%
  filter(predictedYear>=1965 & predictedYear<=2017 & !Crop == "Cereals") %>%
  group_by(predictedYear) %>%
  summarize(
    Tot_harvest = sum(Tot_harvest, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Productionarea = "Riket",
    Crop = "Cereals"
  )

# Combine the two data frames
pd_harvest_real4 <- pd_harvest_real3 %>%
  filter(!(predictedYear>=1965 & predictedYear<=2017 & Crop == "Cereals")) %>%
  bind_rows(tot_cereals) %>%
  arrange(Productionarea,Crop,predictedYear)

rm("pd_harvest_real", "pd_harvest_real2", "pd_harvest_real3")


# Get the total real harvest of potatoes

pxweb_query_list <- 
  list("Produktionsområde"=c("0"),
       "Gröda"=c("22","23"),
       "Variabel"=c("0","2","3"),
       "Tabelluppgift"=c("0"),
       "År"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59"))

# Download data 
px_data <- 
  pxweb_get(url = "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets%20statistikdatabas/Skordar/JO0601J02.px",
            query = pxweb_query_list)

# Convert to data.frame 
pd_harvest_real_potatoes <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Drop a column that we don't need
pd_harvest_real_potatoes <- subset(pd_harvest_real_potatoes, select = -c(Tabelluppgift))

# Change the names
pd_harvest_real_potatoes <- pd_harvest_real_potatoes %>%
  rename(
    Harvest_ha = "Hektarskörd, kg/hektar",
    Productionarea = "Produktionsområde",
    Area = "Areal, hektar",
    Tot_harvest = "Totalskörd, ton",
    Crop = "Gröda",
    Year = "År"
  )

#print(head(pd_harvest_real_potatoes))

# Get the total real harvest of grass

pxweb_query_list <- 
  list("Produktionsområde"=c("0"),
       "Gröda"=c("27"), # "25","26",
       "Variabel"=c("0","2","3"),
       "Tabelluppgift"=c("0"),
       "År"=c("37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59"))

# Download data 
px_data <- 
  pxweb_get(url = "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets%20statistikdatabas/Skordar/JO0601J02.px",
            query = pxweb_query_list)

# Convert to data.frame 
pd_harvest_real_grass <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Drop a column that we don't need
pd_harvest_real_grass <- subset(pd_harvest_real_grass, select = -c(Tabelluppgift))

# Change the names
pd_harvest_real_grass <- pd_harvest_real_grass %>%
  rename(
    Harvest_ha = "Hektarskörd, kg/hektar",
    Productionarea = "Produktionsområde",
    Area = "Areal, hektar",
    Tot_harvest = "Totalskörd, ton",
    Crop = "Gröda",
    Year = "År"
  ) %>%
  mutate(
    Crop = if_else(Crop=="Slåttervall, total vallskörd", "Slåttervall", Crop)
  )
#print(head(pd_harvest_real_grass))
