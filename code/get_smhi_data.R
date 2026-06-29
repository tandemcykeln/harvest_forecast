# --- Function to fetch and process data for the latest dates using JSON ---
fetch_smhi_current <- function(parameter_id, station_id, period, data_type_name) {
  base_url <- "https://opendata-download-metobs.smhi.se/api/version/1.0/parameter/"
  api_url <- paste0(base_url, parameter_id, "/station/", station_id, "/period/latest-months", "/data.json")
  
  cat(paste0("Fetching ", data_type_name, " data from URL:\n", api_url, "\n\n"))
  
  tryCatch({
    smhi_data_raw <- fromJSON(api_url)
    
    # Extract the relevant data (values)
    if (!is.null(smhi_data_raw$value)) {
      data_df <- smhi_data_raw$value
      # print(head(data_df))
      
      # Rename columns for clarity
      names(data_df) <- c("from", "to", "ref", data_type_name, "quality_code")
      
      # Extract just the date part for daily aggregation
      data_df$date <- as.POSIXct(data_df$ref, tz = "UTC")
      data_df$datetime_utc <- as.Date(data_df$date)
      
      # Add the station_id
      data_df$StationID = station_id
      # print(head(data_df))
      
      # Select and reorder relevant columns
      if (parameter_id == 2) {
        data_df <- data_df |> 
          select(datetime_utc, Temperature, StationID) |> 
          mutate(Temperature = as.numeric(Temperature))
      } else if (parameter_id == 5) {
        data_df <- data_df |> 
          select(datetime_utc, Precipitation, StationID) |> 
          mutate(Precipitation = as.numeric(Precipitation))
      }
      return(data_df)
    } else {
      message(paste0("No 'value' element found in ", data_type_name, " data from API response. Check API documentation or station/parameter combination."))
      return(NULL)
    }
  }, error = function(e) {
    message(paste0("An error occurred while fetching or processing ", data_type_name, " data:"))
    message(e$message)
    message("Please check the station ID, parameter ID, period, or API URL.")
    return(NULL)
  })
}

# --- Function to fetch and process data for the corrected_archive using CSV ---

# Function to construct URL and fetch data
fetch_smhi_old <- function(parameter_id, station_id, data_type_name) {
  base_url <- "https://opendata-download-metobs.smhi.se/api/version/1.0/parameter/"
  api_url <- paste0(base_url, parameter_id, "/station/", station_id, "/period/corrected-archive", "/data.csv")

  cat(paste0("Fetching ", data_type_name, " data from URL:\n", api_url, "\n\n"))
  
  response <- GET(api_url)
  
  if (http_status(response)$category == "Success") {
    data_content <- content(response, "text", encoding = "UTF-8")
    # Since the first row can appear on different rows, we must find the first row
    # that we are interested in.
    data_content2 <- strsplit(data_content, "\n")[[1]]
    search_phrase <- "Från Datum Tid (UTC)"
    first_row <- which(grepl(search_phrase, data_content2, fixed = TRUE))[1]
    
    df <- read.csv(text = data_content, sep = ";", skip = (first_row-1), header = TRUE,
                   dec = ".", stringsAsFactors = FALSE)
    
    # Clean up column names (remove leading "X.") and rename
    names(df) <- gsub("X\\.\\.|X\\.", "", names(df))
    names(df) <- gsub("\\.", "", names(df)) # Remove any remaining dots

    # print(" === skriver ut ===")
    # print(head(df))
    # The actual data columns might vary depending on the parameter.
    # Rename relevant columns for clarity
    if ("Representativtdygn" %in% names(df)) {
      names(df)[names(df) == "Representativtdygn"] <- "datetime_utc"
    }
    if ("Nederbördsmängd" %in% names(df)) {
      names(df)[names(df) == "Nederbördsmängd"] <- "Precipitation"
    }
    if ("Lufttemperatur" %in% names(df)) {
      names(df)[names(df) == "Lufttemperatur"] <- "Temperature"
    }
    
    # convert the date to datetime
    df$datetime_utc <- ymd(df$datetime_utc, tz = "UTC", quiet = TRUE)

    # Select and reorder relevant columns
    if (parameter_id == 2) {
      df_clean <- df |> 
        select(datetime_utc, Temperature)  |> 
        mutate(StationID = station_id)
    } else if (parameter_id == 5) {
      df_clean <- df |> 
        select(datetime_utc, Precipitation)  |> 
        mutate(StationID = station_id)
    }
    
    return(df_clean)
    
  } else {
    warning(paste("Failed to retrieve data for parameter", parameter_id, "at station", station_id, ":", http_status(response)$reason))
    return(NULL)
  }
}

# temp_data <- get_smhi_data(temp_param_id, station_id, period)
# 
# # Download precipitation data
# prec_data <- get_smhi_data(prec_param_id, station_id, period)
