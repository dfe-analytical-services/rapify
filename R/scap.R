library(readr)

urn_fix <- function(data_file, output_file, gias_file='data/gias_establishment_codes.csv'){
  data <- read_csv(data_file) %>%
    mutate(school_laestab = as.character(school_laestab))
  message(paste("Read in", nrow(data), "rows from",data_file))
  # Check for any duplicates in original file:
  original_duplicates <- data %>% 
    summarise(count=n(), .by=c(time_period, school_laestab) ) %>%
    filter(count>1) %>% arrange(school_laestab)
  # Extract a sample of awkward schools:
  school_strings <- c('spetisbury', 'cumwhinton', 'danesholme', 'barkerend', 'squirrels')
  school_sample <- data %>% 
    filter(grepl(paste0(school_strings,collapse='|'), school_name, ignore.case=TRUE)) %>% 
    select(time_period, la_name, school_laestab, school_urn, school_name) %>% 
    arrange(school_laestab, time_period)
  gias <- read_csv(gias_file)
  gias_clean <- gias %>%
    filter(!is.na(EstablishmentNumber)) %>%
    mutate(
      school_laestab = paste0(`LA (code)`, EstablishmentNumber),
      school_urn='x',
      OpenDate = as.Date(OpenDate, format='%d-%m-%Y'),
      CloseDate = as.Date(CloseDate, format='%d-%m-%Y'),
    ) %>%
    select(URN, school_laestab, EstablishmentName, OpenDate, CloseDate)
  data_joined <- data %>%
    mutate(
      time_period_ref_date = as.Date(paste0(stringr::str_sub(as.character(time_period+100),1,4),"-05-01"))
      ) %>%
    left_join(gias_clean)
  data_joined_clean <- data_joined %>%
    mutate(
      OpenDate = if_else(is.na(OpenDate), as.Date("1990-09-01"), OpenDate),
      CloseDate = if_else(is.na(CloseDate), Sys.Date(), CloseDate)
    ) %>%
    filter(
      time_period_ref_date < CloseDate,
      time_period_ref_date >= OpenDate,
    )
  message(paste("Found", nrow(data_joined_clean), " records in joined data."))
  
  
  joined_duplicates <- data_joined_clean %>% 
    summarise(count=n(), .by=c(time_period, school_laestab, school_name) ) %>%
    filter(count>1) %>% arrange(school_laestab)
  message(paste("Found", nrow(joined_duplicates), "in joined data."))
  
  message("Now rejoining this on to original data to add back in unmacthed rows")
  data_reprocessed <- data %>% 
    left_join(
      data_joined_clean %>% select(time_period, school_laestab, URN, EstablishmentName)
    )
  
  message(paste("About to write out", nrow(data), "rows to",output_file))
}