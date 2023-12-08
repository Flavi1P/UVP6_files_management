source("/remote/complex/home/fpetit/APERO_UVP6/functions/readUVP6Line.R")

readUVP6 <- function(datatxt){
  file_dat <- read_file(datatxt)
  dat_split <- stri_split_regex(file_dat, ";\n", simplify = TRUE)
  
  datalines <- dat_split[3:length(dat_split)]
  final_df <- data.frame()
  for(i in seq_along(datalines)){
    t_df <- readUVP6Line(datalines[i])
    final_df <- bind_rows(final_df, t_df)
  }
  final_df <- unique(final_df)
  final_df[is.na(final_df)] <- 0
  
  return(final_df)
}

