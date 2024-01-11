library(tidyverse)
library(stringi)
library(yaml)

setwd("/remote/complex/home/fpetit/UVP6_files_management")
#Read the config file
cfg <- read_yaml("config.yaml")
#Set the working directory in the UVP6 directory
uvp6_path <- cfg$Working_directory
setwd(uvp6_path)

#Store the name of the messy folder and the output folder
project_path <- cfg$Project_to_clean
new_cleaned_project <- cfg$Output_folder

#Extract the name of all the files in the messy folder and extract the path of data.txt and vignettes
full_files <- list.files(project_path, recursive = TRUE, full.names = TRUE)
my_data <- full_files[stri_detect_regex(full_files, "data.txt")]
my_vignettes <- full_files[stri_detect_regex(full_files, "(\\.vig)|(\\.raw)")]

#Extract the date from data.txt files
name_of_data_file <- unlist(stri_extract_all_regex(my_data, "([^\\/]+$)"))
date_from_name <- unlist(stri_extract_all_regex(name_of_data_file, "[0-9]{8}\\-[0-9]{6}"))

#Filter the unique dates
unique_date <- unique(date_from_name)

#Check which date are duplicated
check <- data.frame(table(date_from_name))
duplicates <- filter(check, Freq > 1)

###
#We will loop through all unique date, check if we have multiple data.txt, in which case, we only keep the smallest file (the biggest file are full of corrupted character)
###

#Initiate the memory of vectors
keeps <- c() #will contain the path of the datatxt we keep
sizes <- c()
pb <- txtProgressBar(min = 1, max = nrow(check), style = 3)
p = 1

for(i in check$date_from_name){
  path_i <- paste0(i, "[^^\\/].*data.txt")
  files_to_check <- my_data[stri_detect_regex(my_data, path_i)]#All the data.txt of the iteration date
  files_to_check_no_pax <- files_to_check[stri_detect_regex(files_to_check, "/PaxHeader/", negate = TRUE)] #Paxheader correspond to a folder where the data.txt is stored with no values in it. We want to delete them.
  for(j in 1:length(files_to_check_no_pax)){
    size_j <- file.info(files_to_check_no_pax[[j]])$size
    sizes <- c(sizes, size_j)
  }
  keep <- files_to_check_no_pax[[which.min(sizes)]]
  keeps <- c(keeps, keep)
  sizes <- c()
  if(length(keep)>1){
    print(i)
    break()
  }
  setTxtProgressBar(pb, p)
  p = p+1
}


# Make new directories for corresponding data_txt -------------------------

#Now we need to create a new architecture of folder and to copy the data.txt in the appropriate place
pb <- txtProgressBar(min = 1, max = length(keeps), style = 3)
p = 1
for(i in keeps){#Loop through the data.txt we want to keep
  
  #Initiate the right directory
  project <- cfg$Project_name
  date <- stri_extract_last_regex(i, "[0-9]{8}\\-[0-9]{6}")
  proj_path <- paste(new_cleaned_project, project, sep = "/")
  subproj_name <- stri_extract_last_regex(i, "[0-9]{8}\\-[0-9]{6}")
  path_to_copy_datatxt <- paste(new_cleaned_project, project, "raw", subproj_name, sep = "/")
  path_to_write_datatxt <- paste0(path_to_copy_datatxt, "/", subproj_name, "_data.txt")
  
  #If the directory does not exist, create it
  if(!dir.exists(proj_path)){
    dir.create(proj_path)
    dir.create(paste(proj_path, "raw", sep = "/"))
  }
  if(!dir.exists(path_to_copy_datatxt)){
    dir.create(path_to_copy_datatxt)
    dir.create(paste(path_to_copy_datatxt, "images", sep = "/"))
  }
  #copy the data.txt
  if(cfg$remove_one_line == TRUE){
    temp_file <- readLines(i)
    nlines_to_keep <- length(temp_file) - 1
    temp_file <- temp_file[1:nlines_to_keep]
    write_lines(temp_file, path_to_write_datatxt)
  }
  else{
    file.copy(from = i, to = path_to_copy_datatxt, overwrite = TRUE)
  }
  # Check if the copy was successful
  if (!file.exists(path_to_copy_datatxt)) {
    cat("Error in data copy.\n")
  }
}


# Copy images from datatxt lines ------------------------------------------
#Now that we have a clean architecure with the right data.txt we can safely copy vignettes in the img folders

#List the datatxt and images folder of the NEW clean project
files_path <- list.files(paste(new_cleaned_project, project, "raw", sep = "/"), full.names = TRUE, recursive = TRUE)
datatxt_path <- files_path[stri_detect_regex(files_path, "data.txt")]
images_path <- files_path[stri_detect_regex(files_path, "/images")]
date_vig_list <- stri_extract_last_regex(my_vignettes, "[0-9]{8}\\-[0-9]{6}", simplify = TRUE)

#we will loop over all the clean datatxt to get vignettes that correspond to all the dates of the files and check if we have the right amount of vignette, considering a given threshold in pixel.
#Initiate vector storage
list_of_diff <- c()
date_list <- c()

#Here 74 correspond to 620ESD, may be modified to be depend of the ACQ line
threshold_in_pixel <- 74

pb <- txtProgressBar(min = 1, max = length(datatxt_path), style = 3)
p = 1

for(i in seq_along(datatxt_path)){#For all dtatxt
  #Look into the data.txt
  data_lines <- readLines(datatxt_path[i]) #Read the file
  hw_conf <- data_lines[1] #Read the hardware configuration
  acq_conf <- data_lines[3] #Read the acquisition configuration
  blocks <- data_lines[which(stri_detect_regex(data_lines, "[0-9]{8}\\-[0-9]{6}"))] #read all the lines of data
  
  #If we don't have data lines, we go to the next datatxt
  if(length(blocks) <= 1){ 
    next()
  }
  
  #A loop to get all the lines that have at least one object above the pixel threshold. We want to keep info about overexposure as it may produce vignettes
  #This loop only produce the number of expected vignettes 
  date_time_vignettes <- c()
  overexposed <- 0
  n_expected_vignettes <- 0
  for(k in seq_along(blocks)){
    biggest_obj <- stri_extract_last_regex(blocks[k], ";[0-9]+") |> stri_replace_all_regex(";", "") |> as.numeric()
    if(is.na(biggest_obj)){
      if(stri_detect_regex(blocks[k], "OVER_EXPOSED")){
        overexposed <- overexposed + 1
        date_time_vig <- stri_extract_all_regex(blocks[k], "[0-9]{8}\\-[0-9]{6}", simplify = TRUE)
        date_time_vignettes <- c(date_time_vignettes, date_time_vig)
      }
      next()
    }
    if(biggest_obj >= threshold_in_pixel){
      n_expected_vignettes <- n_expected_vignettes + 1
      date_time_vig <- stri_extract_all_regex(blocks[k], "[0-9]{8}\\-[0-9]{6}", simplify = TRUE)
      date_time_vignettes <- c(date_time_vignettes, date_time_vig)
    }
  }
  #Final number of expected vignettes
  real_vignettes <- n_expected_vignettes + overexposed
  
  #If no vignettes we just move on
  if(real_vignettes == 0){
    next()
  }
  
  #Check if we find the same amount of files
  #Get all the vig that have a date corresponding to a date of the datatxt
  date_from_lines <- stri_extract_all_regex(blocks, "[0-9]{8}\\-[0-9]{6}", simplify = TRUE) |> na.omit()
  vignettes_from_file <- my_vignettes[which(date_vig_list %in% date_from_lines)]

  folder_to_copy_vig <- paste0(dirname(datatxt_path[i]), "/images")
  
  vignette_names <- stri_extract_last_regex(vignettes_from_file, "[^/]+(\\.vig)|[^/]+(\\.raw)")
  
  #Create a dataframe as it is easier to manipulate string and reconstruct the structure of the image path
  vignettes_list_unique <- data.frame(table(vignettes_from_file)) |>
    mutate(vig_name = stri_extract_last_regex(vignettes_from_file, "[^/]+(\\.vig)|[^/]+(\\.raw)"),
           vig_path = as.character(vignettes_from_file),
           date = stri_extract_all_regex(vig_name, "[0-9]{8}\\-[0-9]{6}")) |> 
    group_by(vig_name) |> 
    mutate(occ_id = seq_along(vig_name)) |> 
    ungroup() |> 
    filter(occ_id == 1) |> 
    mutate(new_path = paste0(folder_to_copy_vig, stri_extract_last_regex(vig_path, "/[0-9]+/"), vig_name))
  
  #The difference between the expected number of vignettes and the actual number of vignettes
  vig_diff <- real_vignettes - length(vignettes_list_unique$date)
  
  list_of_diff <- c(list_of_diff, vig_diff)
  date_of_loop <- stri_extract_first_regex(datatxt_path[i], "[0-9]{8}\\-[0-9]{6}")
  date_list <- c(date_list, date_of_loop)
  
  #Copy the vignettes
  folders_list <- unique(stri_extract_last_regex(vignettes_list_unique$vignettes_from_file, "/[0-9]+/"))
  folders_to_create <- paste0(folder_to_copy_vig, folders_list)
  for(j in folders_to_create){
    if(!dir.exists(j)){
      dir.create(j)
    }
  }
  for(n in seq_along(vignettes_list_unique$vig_path)){
    new_vignette_path <- vignettes_list_unique$new_path[n]
    old_vignette_path <- vignettes_list_unique$vig_path[n]
    file.copy(old_vignette_path, new_vignette_path, overwrite = TRUE)
  }
  setTxtProgressBar(pb, p)
  p = p+1
}

error_data <- data.frame("date" = date_list, "diff" = list_of_diff)

#An other code to compute the diff of vignettes number between the clean project and the expected number of vignettes.
my_new_folders <- list.files(paste(new_cleaned_project, project, "raw", sep = "/"), full.names = TRUE)

diff_results <- c()
date_memory <- c()

pb <- txtProgressBar(min = 1, max = length(my_new_folders), style = 3)
p = 1
t <- 0
for(i in my_new_folders){
  n_vignettes <- length(list.files(paste0(i, "/images"), recursive = TRUE))
  datatxt <- list.files(i, full.names = TRUE)[grep("data", list.files(i))]
  #Then look into the data.txt
  data_lines <- readLines(datatxt) #Read the file
  hw_conf <- data_lines[1] #Read the hardware configuration
  acq_conf <- data_lines[3] #Read the acquisition configuration
  blocks <- data_lines[4:length(data_lines)] #read all the lines of data
  if(length(blocks) <= 1){
    t <-  t +1 
    next()
  }
  #Calculate the number of pixel corresponding to the threshold ESD
  threshold_in_pixel <- 74
  
  n_expected_vignettes <- 0
  for(k in seq_along(blocks)){
    biggest_obj <- stri_extract_last_regex(blocks[k], ";[0-9]+") |> stri_replace_all_regex(";", "") |> as.numeric()
    if(is.na(biggest_obj)){
      next()
    }
    if(biggest_obj >= threshold_in_pixel){
      n_expected_vignettes <- n_expected_vignettes + 1
    }
  }
  
  diff = n_expected_vignettes - n_vignettes
  diff_results <- c(diff_results, diff)
  
  setTxtProgressBar(pb, p)
  p = p+1
}

  ggplot()+
  geom_boxplot(aes(y = diff_results))
mean(diff_results)
sd(diff_results)

results_df <- data.frame("date" = date_memory, "diff" = diff_results)

#A small reminder of the equivalence between pixel size and threshold in ESD (see Picheral 2020)
df <- data.frame("npix" = seq(1,100), "Aa" = 2300, "exp" = 1.136, "lpix" = 73) |> 
  mutate("s_um" = Aa * (npix^exp),
         "ESD_um" = 2*sqrt(s_um/pi))

