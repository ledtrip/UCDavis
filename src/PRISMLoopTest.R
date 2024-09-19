i = 1


  loc_i <- PRISMdf$Location[i]
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
  prism_i$Location <- PRISMdf$Location[i]
  
  PRISM_temp <- cbind(prism_i)
