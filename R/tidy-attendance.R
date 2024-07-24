library(readr)
library(curl)

read_attendance <- function(){
  url <- "https://raw.githubusercontent.com/dfe-analytical-services/attendance-data-dashboard/main/data/attendance_data_dashboard.csv"
  x<- Sys.getenv("HOME") |> strsplit("\\\\")
  data_folder <- paste0(paste0( x[[1]][1:3], collapse = '/'), "/test-data/api-attendance/")
  data_file <- "attendance_data_dashboard.csv"
  if(!file.exists(paste0(data_folder, data_file))){
    message(paste0(data_folder, data_file, "\n not found. Downloading from repository."))
    att_wide <- read_csv(url)
    att_wide |> write_csv(paste0(data_folder, data_file))
  } else{
    message(paste0(data_folder, data_file, " found. Readng in from file
                   ."))
    att_wide <- read_csv(paste0(data_folder, data_file))
  }
  att_wide
}

create_reasons_tidy <- function(){
  att_underlying <- read_attendance()
  
}
