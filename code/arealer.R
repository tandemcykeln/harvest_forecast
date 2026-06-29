### Download the areas for crops

# Previous years areas
# Since the database does not keep the year, we must handle it accordingly
# The current year, using two numbers
current_year <- as.integer(format(Sys.Date(), "%Y")) - (2000)
last_year <- current_year - 1

# The database says that "År=0" is equal to 2001, therefore we have to adapt to that
year <- as.character(1:last_year-1)

# Query to the database    
pxweb_query_list <- 
  list("Produktionsområde"=c("1","2","3","4","5","6","7","8"),
       "Gröda"=c("2","3","4","5","6","7","8","9","10","11","18","21","20"), #,"18"),
       "Variabel"=c("0"),
       "År"=year)

# Download data 
px_data <- 
  pxweb_get(url = "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets%20statistikdatabas/Arealer/2%20Produktionsomr%C3%A5de%20storleksindelning/JO0104B19.px",
            query = pxweb_query_list)

# Convert to data.frame 
pd_areas <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")


# Preliminary data for the current year
# The variable År has to be manually calculated, since they use such a weird naming
year2 <- as.character(current_year - 18)

# Query to the database
pxweb_query_list <- 
  list("Produktionsområde"=c("1","2","3","4","5","6","7","8"),
       "Gröda"=c("0","1","2","3","4","5","6","7","8","17","18","15"),
       "År"=year2
       )

# Download data 
px_data <- 
  pxweb_get(url = "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets%20statistikdatabas/Arealer/Preliminar%20arealstatistik/JO0104C02.px",
            query = pxweb_query_list)

# Convert to data.frame 
pd_areas_current <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Change the names
pd_areas_current2 <- pd_areas_current |> 
  rename(
    "Areal, hektar" = "Preliminära grödarealer"
  )

# Add the two data frames together
pd_areas2 <- bind_rows(pd_areas, pd_areas_current2) |> 
  arrange(Produktionsområde, Gröda, År) |> 
  # We need to change the name of "Slåttervall" to correspond to the normal name
  mutate( 
    Gröda = if_else(Gröda=="Slåtter- och betesvall som utnyttjas", "Slåttervall", Gröda)
  )


# Change the names
pd_areas3 <- pd_areas2 %>%
  rename(
    Productionarea = Produktionsområde,
    Year = År,
    Crop = Gröda,
    Hectares = "Areal, hektar"
  )

# Change rågvete 
df_area_sum <- pd_areas3 |> 
  filter((Crop=="Vårrågvete" | Crop=="Höstrågvete") & Year>=2015) |> 
  filter(!is.na(Hectares)) |> 
  group_by(Productionarea, Year) %>%
  summarize(
    Hectares = sum(Hectares),
    .groups = "drop"
  ) |> 
  mutate(Crop = "Rågvete")

# Add the two dataframes
pd_areas4 <- pd_areas3  |> 
  filter(!Crop %in% c("Vårrågvete", "Höstrågvete")) |> 
  filter(!is.na(Hectares)) |> 
  bind_rows(df_area_sum) |> 
  arrange(Productionarea, Crop, Year) |> 
  mutate(Crop = if_else(Crop == "Blandsäd (stråsäd)", "Blandsäd", Crop),
         Year = as.double(Year))

rm("pd_areas", "df_area_sum", "pd_areas_current", "pd_areas_current2", "pd_areas2", "pd_areas3")


