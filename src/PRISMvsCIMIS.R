library(prism)
library(terra)
# install.packages("openxlsx")
library(openxlsx)
library(dplyr)

# Set the download directory for PRISM data
# You will need to modify this for a specific location in your computer
prism_set_dl_dir("/Users/lewisdaniel/R\ Folder/UCDavis/data/PRISM")

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

PRISM_CIMIS_pars <- read.csv("data/PRISM_CIMIS_Comparison.csv", fileEncoding="latin1", na.strings=c("","NA")) 

# PRISM_CIMIS_pars <- PRISM_CIMIS_pars[c(8, 9), ] # Testing with few rows

PRISM_CIMIS_pars$start_date <- as.Date(PRISM_CIMIS_pars$start_date, format = "%Y-%m-%d") 
PRISM_CIMIS_pars$end_date <- as.Date(PRISM_CIMIS_pars$end_date, format = "%Y-%m-%d")

PRISM_CIMIS <- data_frame(date = NA, value_tmax = NA, value_tmin = NA, Location = NA)

for (i in 1:length(PRISM_CIMIS_pars$ï..Location)) {
                lat_i <- PRISM_CIMIS_pars$lat[i]
                lon_i <- PRISM_CIMIS_pars$lon[i]
                start_date_i <- PRISM_CIMIS_pars$start_date[i]
                end_date_i <- PRISM_CIMIS_pars$end_date[i]
                get_prism_dailys(type = "tmax", minDate = start_date_i, maxDate = end_date_i, keepZip = FALSE)
                get_prism_dailys(type = "tmin", minDate = start_date_i, maxDate = end_date_i, keepZip = FALSE)
                
                # List the downloaded files
                tmax_files_i <- prism_archive_subset("tmax", "daily", minDate = start_date_i, maxDate = end_date_i)
                tmin_files_i <- prism_archive_subset("tmin", "daily", minDate = start_date_i, maxDate = end_date_i)
                
                # Extract data for the specified location
                tmax_data_i <- extract_prism_data(file_location, tmax_files_i, lat_i, lon_i)
                tmin_data_i <- extract_prism_data(file_location, tmin_files_i, lat_i, lon_i)
                
                # Combine the data into one data frame
                prism_i <- merge(tmax_data_i, tmin_data_i, by = "date", suffixes = c("_tmax", "_tmin")) # check how tmin and tmax columns are called in prism_data df and rename these columns in PRISM_CIMIS_pars
                prism_i$Location <- PRISM_CIMIS_pars$ï..Location[i]
                
                PRISM_CIMIS <- rbind(PRISM_CIMIS, prism_i)
}

PRISM_CIMIS$date <- as.Date(PRISM_CIMIS$date, format = "%Y-%m-%d")

PRISM_CIMIS <- PRISM_CIMIS %>% 
                mutate(
                  PRISMMinTempF = (PRISM_CIMIS$value_tmin * 9/5) + 32,
                  PRISMaxTempF = (PRISM_CIMIS$value_tmax * 9/5) + 32
                )

write.xlsx(PRISM_LOCAL, file = "outputs/PRISM_LOCAL.xlsx")
