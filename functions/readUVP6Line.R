readUVP6Line <- function(UVPstring){

  # Extracting date and time from the string
  date_time <- substr(UVPstring, 2, 15)
  date <- substr(date_time, 1, 8)
  time <- substr(date_time, 10, 15)
  
  # Extracting the data after ":"
  data_part <- sub(".*:", "", UVPstring)
  
  # Splitting the data into a list
  data_list <- strsplit(data_part, ";")[[1]]
  
  # Creating a data frame
  df <- data.frame(Date = rep(date, length(data_list)),
                   Time = rep(time, length(data_list)),
                   stringsAsFactors = FALSE)
  
  # Adding columns to the data frame based on the numbers after ":" and ";"
  for (i in seq_along(data_list)) {
    values <- strsplit(data_list[i], ",")[[1]]
    col_name <- as.character(values[1])
    df[[paste0(col_name, "_pixel")]] <- as.numeric(values[2])
  }
  
  df_unique <- unique(df)
  
  # Printing the resulting data frame
  return(df)
  
}
