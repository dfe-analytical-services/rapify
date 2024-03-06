library(readr)
library(dplyr)


# To run the fix, provide a data file and downloaded GIAS data
# Example:
# data_file <- "data/scap/school-capacity_200910-202122.csv"
# gias_file <- 'data/gias_establishment_codes.csv'
# urn_fix(data_file, gias_file = gias_file)
# The function will write out three files:
# 1. new data file with filled URNs and harmonised school names where possible (_clean.csv)
# 2. QA table of LAEstabs, names and URNs from the original and replacements from GIAS
# 3. Table of unmatched schools, where the matching hasn't managed to find an 
#       appropriate match in GIAS. Might be some extra LA code changes to 
#       account for here.

urn_fix <- function(data_file, gias_file='data/gias_establishment_codes.csv'){
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
      URN = format(URN, scientific=FALSE),
      OpenDate = as.Date(OpenDate, format='%d-%m-%Y'),
      CloseDate = as.Date(CloseDate, format='%d-%m-%Y'),
    ) %>%
    select(URN, school_laestab, EstablishmentName, OpenDate, CloseDate)
  data_reprocessed <- match_to_gias_data(data, gias_clean)
  
  
  la_evolution <- data.frame(
    la_code_new = c("942", "943", "838", "839", "839", "940", "941"),
    la_code_previous = c("909", "909", "835", "836", "837", "928", "928")
  )
  data_round2 <- update_rows_with_new_lacodes(
    data_reprocessed, la_evolution
    )
  la_evolution_2 <- data.frame(
    la_code_new = c("839"),
    la_code_previous = c("835")
  )
  data_round3 <- update_rows_with_new_lacodes(
    data_round2, la_evolution_2
  )

  unmatched_rows_round3 <- data_round3 %>% 
    filter(school_urn != URN | is.na(school_urn)| school_urn=='x') %>% 
    select(time_period, old_la_code, la_name, school_laestab, school_name) %>%
    mutate(old_la_code = as.character(old_la_code)) %>%
    arrange(school_laestab)
  message(paste("Found ", nrow(unmatched_rows_round3), "unmatched rows in the reporocessed data"))
  write.csv(unmatched_rows_round3, gsub(".csv", "_unmatched.csv", data_file), row.names = FALSE)
  
  name_matches <- data_round3 %>% 
    select(time_period, old_la_code, la_name, school_laestab, school_urn, school_name) %>% 
    distinct() %>% 
    left_join(
      data %>% 
        select(time_period, school_laestab, original_name = school_name, original_urn = school_urn)
      ) %>%
    select(old_la_code, la_name, school_laestab, original_urn, school_urn, original_name, school_name) %>% distinct() %>% 
    arrange( school_name, school_laestab, original_name)
  write.csv(name_matches, gsub(".csv", "_gias_name_matching_qa.csv", data_file), row.names = FALSE)
  
  data_cleaned <- data_round3 %>% 
    select(all_of(names(data))) %>%
    mutate(school_urn = if_else(is.na(school_urn), "x", school_urn))
  write.csv(data_cleaned, gsub(".csv", "_cleaned.csv", data_file), row.names = FALSE)
  
  message(paste("About to write out", nrow(data), "rows to",output_file))
}


match_to_gias_data <- function (data, gias_clean){
  data_joined <- data %>%
    select(time_period, school_laestab, school_urn) %>%
    distinct() %>%
    mutate(
      time_period_ref_date = as.Date(paste0(stringr::str_sub(as.character(time_period+100),1,4),"-05-01"))
    ) %>%
    left_join(gias_clean)
  school_time_period_lookup <- data_joined %>%
    mutate(
      OpenDate = if_else(is.na(OpenDate), as.Date("1990-09-01"), OpenDate),
      CloseDate = if_else(is.na(CloseDate), Sys.Date(), CloseDate)
    ) %>%
    filter(
      time_period_ref_date < CloseDate,
      time_period_ref_date >= OpenDate,
    )
  message(paste("Found", nrow(school_time_period_lookup), " records in lookup table."))
  lookup_duplicates <- school_time_period_lookup %>% 
    summarise(count=n(), .by=c(time_period, school_laestab, school_urn) ) %>%
    filter(count>1) %>% arrange(school_laestab)
  print(
    lookup_duplicates %>% left_join(school_time_period_lookup) %>% 
      select(-time_period, -time_period_ref_date) %>% distinct() %>% 
      arrange(school_laestab), 
    n=50)
  message(paste("Found", nrow(lookup_duplicates), " duplicates in lookup table."))
  
  message("Now rejoining this on to original data to add back in unmacthed rows")
  data_reprocessed <- data %>% 
    left_join(
      school_time_period_lookup %>% select(time_period, school_laestab, URN, EstablishmentName)
    ) %>%
    mutate(
      school_urn = case_when(
        school_urn=='x' ~ URN,
        is.na(school_urn) ~ URN,
        .default = school_urn
      ),
      school_name = case_when(
        !is.na(EstablishmentName) ~ EstablishmentName,
        .default = school_name
      )
    )
  message(paste("I've got ", nrow(data_reprocessed), "rows in the reporocessed data (compared to ",nrow(data),"in the original data"))
  return(data_reprocessed)
}

update_rows_with_new_lacodes <- function(data, la_mapping){
  unmatched_rows <- data %>% 
    filter(school_urn != URN | is.na(school_urn)| school_urn=='x') %>% 
    select(time_period, old_la_code, la_name, school_laestab, school_name) %>%
    mutate(old_la_code = as.character(old_la_code))
  message(paste("Found ", nrow(unmatched_rows), "unmatched rows in the reporocessed data"))
  changed_la_lookup <- unmatched_rows %>% 
    select(-time_period) %>%
    distinct() %>%
    left_join(la_mapping, by=c("old_la_code"="la_code_previous")) %>% 
    mutate(school_laestab_new = paste0(la_code_new, stringr::str_sub(school_laestab,4,7))) %>%
    left_join(gias_clean, by=c("school_laestab_new"="school_laestab")) %>%
    filter(!is.na(URN)) %>%
    select(
      URN, school_laestab, EstablishmentName, OpenDate, CloseDate
    ) %>%
    distinct() %>%
    arrange(school_laestab)
  data_processed <- match_to_gias_data(
    data %>%
      select(-EstablishmentName, -URN), 
    changed_la_lookup
  )
}
