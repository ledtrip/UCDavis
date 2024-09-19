library(prism)
library(terra)
# install.packages("openxlsx")
library(openxlsx)
library(dplyr)

# Set the download directory for PRISM data
# You will need to modify this for a specific location in your computer
prism_set_dl_dir("/Users/lewisdaniel/R\ Folder/UCDavis/data/PRISM")

# Define the latitude and longitude
lat <- 39.0258
lon <- -121.5337

# Define the date range
start_date <- as.Date("2016-05-16")
end_date <- as.Date("2016-10-13")

# Download the PRISM data for the specified date range and type

###
# Note that this is downloading all the PRISM data for the date range, 
# not just for the specific location of interest. So for a larger date range it will take a while
# but you should be able to use that data for multiple locations within the same time period
###

get_prism_dailys(type = "tmax", minDate = start_date, maxDate = end_date, keepZip = FALSE)
get_prism_dailys(type = "tmin", minDate = start_date, maxDate = end_date, keepZip = FALSE)

# List the downloaded files
tmax_files <- prism_archive_subset("tmax", "daily", minDate = start_date, maxDate = end_date)
tmin_files <- prism_archive_subset("tmin", "daily", minDate = start_date, maxDate = end_date)

# Set the file location
file_location <- ("/Users/lewisdaniel/R\ Folder/UCDavis/data/PRISM/")

# Function to extract data for a specific location from a PRISM file using terra
extract_prism_data <- function(file_location, files, lat, lon) {
  data_list <- lapply(files, function(file) {
    bil_file <- list.files(paste0(file_location,file), pattern = "\\.bil$", full.names = TRUE)
    raster_data <- terra::rast(bil_file)
    data <- terra::extract(raster_data, terra::vect(cbind(lon, lat)), cells = TRUE)
    date <- as.Date(substr(basename(bil_file), 25, 32), format = "%Y%m%d")
    data.frame(date = date, value = data[,2])
  })
  do.call(rbind, data_list)
}

# Extract data for the specified location
tmax_data <- extract_prism_data(file_location, tmax_files, lat, lon)
tmin_data <- extract_prism_data(file_location, tmin_files, lat, lon)

# for in loop for data downloading, extraction and dataframes:

library(dplyr)

PRISMdf <- read.csv("data/PRISM_set2.csv", fileEncoding="latin1", na.strings=c("","NA")) 

# PRISMdf <- PRISMdf[23, ] # Testing with few rows

PRISMdf$start_date <- as.Date(PRISMdf$start_date, format = "%Y-%m-%d") 
PRISMdf$end_date <- as.Date(PRISMdf$end_date, format = "%Y-%m-%d")

PRISM_temp <- data_frame(date = NA, value_tmax = NA, value_tmin = NA, Location = NA)

for (i in 1:length(PRISMdf$ï..Location)) {
              lat_i <- PRISMdf$lat[i]
              lon_i <- PRISMdf$lon[i]
              start_date_i <- PRISMdf$start_date[i]
              end_date_i <- PRISMdf$end_date[i]
              get_prism_dailys(type = "tmax", minDate = start_date_i, maxDate = end_date_i, keepZip = FALSE)
              get_prism_dailys(type = "tmin", minDate = start_date_i, maxDate = end_date_i, keepZip = FALSE)
              
              # List the downloaded files
              tmax_files_i <- prism_archive_subset("tmax", "daily", minDate = start_date_i, maxDate = end_date_i)
              tmin_files_i <- prism_archive_subset("tmin", "daily", minDate = start_date_i, maxDate = end_date_i)
              
              # Extract data for the specified location
              tmax_data_i <- extract_prism_data(file_location, tmax_files_i, lat_i, lon_i)
              tmin_data_i <- extract_prism_data(file_location, tmin_files_i, lat_i, lon_i)
              
              # Combine the data into one data frame
              prism_i <- merge(tmax_data_i, tmin_data_i, by = "date", suffixes = c("_tmax", "_tmin")) # check how tmin and tmax columns are called in prism_data df and rename these columns in PRISMdf
              prism_i$Location <- PRISMdf$ï..Location[i]
              
              PRISM_temp <- rbind(PRISM_temp, prism_i)
}

PRISM_temp$date <- as.Date(PRISM_temp$date, format = "%Y-%m-%d")

PRISM_temp <- PRISM_temp %>% 
                mutate(
                  PRISMMinTempF = (PRISM_temp$value_tmin * 9/5) + 32,
                  PRISMaxTempF = (PRISM_temp$value_tmax * 9/5) + 32
                )

PRISMmerge <- PRISM_temp[ , c("date", "Location", "PRISMMinTempF", "PRISMaxTempF")]

names(PRISMmerge)[names(PRISMmerge) == "date"] <- "Date"
  
  
# write.xlsx(PRISM_temp,"outputs/PRISM_temp.xlsx",rowNames = TRUE)
# PRISM_temp <- read_xlsx("outputs/PRISM_temp.xlsx")
