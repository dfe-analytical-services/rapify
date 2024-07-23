library(readxl)
library(dplyr)
library(tidyr)

las <- readr::read_csv(
  "https://raw.githubusercontent.com/dfe-analytical-services/dfe-published-data-qa/master/data/las.csv"
  ) %>%
  filter(status == "live" | old_la_code == "909") %>%
  select(new_la_code, la_name, old_la_code) %>%
  distinct()

home_folder <- Sys.getenv("HOME") %>% gsub("Documents","", .)

data_folder <- paste0(home_folder, "test-data/api-cscp/ks4/")

read_ks4_data <-  function(){
 read_xlsx(paste0(data_folder, "2022-2023_england_ks4_provisional.xlsx")) %>%
  filter(!is.na(LEA)) %>%
  mutate(across(where(is.numeric), format, scientific = FALSE, trim = TRUE))
}

attainment_counts <- c("TPRIORLO", "TPRIORAV", "TPRIORHI")
attainment_percents <- c("PTPRIORLO", "PTPRIORAV", "PTPRIORHI")

info_cols <- c(
  "RECTYPE", "SCHNAME_AC",
  "ADDRESS1", "ADDRESS2", "ADDRESS3", "TOWN", "PCODE", "TELNUM",
  "CONTFLAG", "ICLOSE", "NFTYPE", "RELDENOM", "ADMPOL", "ADMPOL_PT", "EGENDER",
  "FEEDER", "TABKS2", "TAB1618", "AGERANGE"
)

p8_id_cols <- c("P8_HIDE_FLAG", "P8_BANDING", "P8_BANDING_FULL")


tidy_progress8 <- function() {
  ks4_data <- read_ks4_data()

  ks4_data_noinfo <- ks4_data %>%
    select(-all_of(info_cols))

  p8cols <- names(ks4_data) %>%
    as.data.frame() %>%
    filter(grepl("P8", .), !(. %in% p8_id_cols)) %>%
    pull(.)


  ks4_p8 <- ks4_data_noinfo %>%
    select(all_of(c("LEA", "ESTAB", "URN", "SCHNAME", p8_id_cols, p8cols))) %>%
    pivot_longer(
      -all_of(c("LEA", "ESTAB", "URN", "SCHNAME", p8_id_cols)), 
      names_to = "master_filter"
      ) %>%
    mutate(
      metric = case_when(
        grepl("CILOW", master_filter) ~ "attainment_score_lower_confidence",
        grepl("CIUP", master_filter) ~ "attainment_score_upper_confidence",
        grepl("P8MEA", master_filter) ~ "attainment_score",
        grepl("P8PUP", master_filter) ~ "pupil_count",
        grepl("TP8ADJ", master_filter) ~ "pupil_count_progress8_adjustment",
        grepl("HIDE_FLAG", master_filter) ~ "hide_flag",
        .default = "dunno_yet"
      ),
      sex = case_when(
        grepl("BOYS", master_filter) ~ "Male",
        grepl("GIRLS", master_filter) ~ "Female",
        .default = "All pupils"
      ),
      free_school_meal_status = case_when(
        grepl("_NFSM", master_filter) ~ "Not free school meals",
        grepl("_FSM", master_filter) ~ "Free school meals",
        .default = "All pupils"
      ),
      time_period = case_when(
        grepl("_22", master_filter) ~ "202122",
        grepl("_21", master_filter) ~ "202021",
        .default = "202223"
      ),
      mobility_status = case_when(
        grepl("NMOB", master_filter) ~ "Non-mobile",
        .default = "All pupils"
      ),
      eal_status = case_when(
        grepl("EAL", master_filter) ~ "English as another language",
        .default = "All pupils"
      ),
      attainment_area = case_when(
        grepl("MEAENG", master_filter) ~ "English",
        grepl("MEAMAT", master_filter) ~ "Maths",
        grepl("MEAEBAC", master_filter) ~ "EBACC pupils",
        grepl("MEAOPEN", master_filter) ~ "Open",
        grepl("MEACOV", master_filter) ~ "Cov - no idea what this is",
        grepl("DIFFN_P8MEA", master_filter) ~ "diffn_p8_measure",
        .default = "All subjects"
      ),
      prior_attainment = case_when(
        grepl("_LO", master_filter) ~ "Low",
        grepl("_MID", master_filter) ~ "Mid",
        grepl("_HI", master_filter) ~ "High",
        .default = "All pupils"
      ),
      version = case_when(
        grepl("_ORIG", master_filter) ~ "Original",
        .default = "unoriginal"
      ),
      value = case_when(
        value == "NE" ~ "z",
        value == "NP" ~ "x",
        value == "SUPP" ~ "c",
        value == "LOWCOV" ~ "low",
        stringr::str_trim(value) == "NA" ~ "z",
        is.na(value) ~ "z",
        .default = format(round(as.numeric(value), digits = 2), digits = 2, scientific = FALSE, trim = TRUE)
      ),
      P8_BANDING = case_when(
        is.na(P8_BANDING) ~ "z",
        P8_BANDING == "NP" ~ "x",
        .default = P8_BANDING
      ),
      P8_BANDING_FULL = case_when(
        is.na(P8_BANDING_FULL) ~ "z",
        P8_BANDING_FULL == "NP" ~ "x",
        .default = P8_BANDING_FULL
      )
    )


  ks4_p8_tidied <- ks4_p8 %>%
    select(-master_filter) %>%
    pivot_wider(names_from = metric, values_from = value, values_fill = "z") %>%
    left_join(las, by = join_by(LEA == old_la_code))


  ks4_p8_ees <- ks4_p8_tidied %>%
    mutate(
      school_laestab = case_when(
        is.na(ESTAB) ~ "",
        ESTAB == "NA" ~ "",
        .default = paste0(LEA, ESTAB)
      ),
      school_urn = case_when(
        is.na(URN) ~ "",
        URN == "NA" ~ "",
        .default = paste0(URN)
      ),
      school_name = case_when(
        is.na(SCHNAME) ~ "",
        SCHNAME == "NA" ~ "",
        .default = SCHNAME
      ),
      time_identifier = "Academic year",
      geographic_level = case_when(
        is.na(ESTAB) ~ "Local authority",
        ESTAB == "NA" ~ "Local authority",
        .default = "School"
      ),
      country_code = "E92000001",
      country_name = "England"
    ) %>%
    select(
      time_period,
      time_identifier,
      geographic_level,
      country_code,
      country_name,
      old_la_code = LEA,
      new_la_code,
      la_name,
      school_laestab,
      school_urn,
      school_name,
      version,
      sex,
      free_school_meal_status,
      eal_status,
      mobility_status,
      prior_attainment,
      attainment_area,
      progree8_hide_flag = P8_HIDE_FLAG,
      progree8_banding = P8_BANDING,
      progree8_banding_full = P8_BANDING_FULL,
      pupil_count,
      pupil_count_progress8_adjustment,
      attainment_score,
      attainment_score_lower_confidence,
      attainment_score_upper_confidence
    ) %>%
    arrange(-as.numeric(time_period), old_la_code, school_laestab, version, sex, free_school_meal_status, eal_status, mobility_status, attainment_area)

  write.csv(
    ks4_p8_ees %>%
      filter(geographic_level == "School"),
    paste0(data_folder, "cscp_ks4_progress8_202223.csv"),
    row.names = FALSE
  )

  ks4_p8_meta_data <- data.frame(col_name = names(ks4_p8_ees)) %>%
    filter(!(col_name %in% c("time_period", "time_identifier", "geographic_level", "country_code", "country_name", "old_la_code", "new_la_code", "la_name", "school_laestab", "school_urn", "school_name"))) %>%
    mutate(
      col_type = "Filter",
      label = stringr::str_to_sentence(gsub("_", " ", col_name)),
      indicator_grouping = "",
      indicator_unit = "",
      indicator_dp = "",
      filter_hint = "",
      filter_grouping_column = ""
    )

  ks4_p8_meta_data$col_type[8:15] <- "Indicator"
  ks4_p8_meta_data$indicator_dp[11:12] <- "0"
  ks4_p8_meta_data$indicator_dp[13:15] <- "2"

  print(ks4_p8_meta_data)
  write.csv(ks4_p8_meta_data, paste0(data_folder, "cscp_ks4_progress8_202223.meta.csv"), row.names = FALSE)
}


ks4_attainment8 <- function(){
  ks4_data <- read_ks4_data()
  a8cols <- names(ks4_data) %>%
    as.data.frame() %>%
    filter(grepl("ATT8", .), !(. %in% p8_id_cols)) %>%
    pull(.)

  ks4_data_noinfo <- ks4_data %>%
    select(-all_of(info_cols))
  
  ks4_a8 <- ks4_data_noinfo %>%
    select(all_of(c("LEA", "ESTAB", "URN", "SCHNAME", a8cols))) %>%
    pivot_longer(-all_of(c("LEA", "ESTAB", "URN", "SCHNAME")), names_to = "master_filter")  %>%
    mutate(
      metric = case_when(
        grepl("ATT8SCR", master_filter) ~ "attainment_score_average",
        grepl("TOTATT8", master_filter) ~ "attainment_score_total",
        .default = "unknown"
      ),
      breakdown_topic = case_when(
        grepl("BOYS", master_filter) ~ "Sex",
        grepl("GIRLS", master_filter) ~ "Sex",
        grepl("_NFSM", master_filter) ~ "Free school meals status",
        grepl("_FSM", master_filter) ~ "Free school meals status",
        grepl("NMOB", master_filter) ~ "Mobility status",
        grepl("EAL", master_filter) ~ "Language status",
        grepl("_LO", master_filter) ~ "Prior attainment",
        grepl("_MID", master_filter) ~ "Prior attainment",
        grepl("_HI", master_filter) ~ "Prior attainment",
        grepl("DIFFN", master_filter) ~ "Prior attainment",
        .default = "All pupils"
      ),
      breakdown = case_when(
        grepl("BOYS", master_filter) ~ "Male",
        grepl("GIRLS", master_filter) ~ "Female",
        grepl("_NFSM", master_filter) ~ "Not free school meals",
        grepl("_FSM", master_filter) ~ "Free school meals",
        grepl("NMOB", master_filter) ~ "Non-mobile",
        grepl("EAL", master_filter) ~ "English as another language",
        grepl("_LO", master_filter) ~ "Low",
        grepl("_MID", master_filter) ~ "Mid",
        grepl("_HI", master_filter) ~ "High",
        grepl("DIFFN", master_filter) ~ "Difference between High and Low",
        .default = "All pupils"
      ),
      time_period = case_when(
        grepl("_22", master_filter) ~ "202122",
        grepl("_21", master_filter) ~ "202021",
        .default = "202223"
      ),
      attainment_area = case_when(
        grepl("OPENNG", master_filter) ~ "Open non-GCSE",
        grepl("OPENG", master_filter) ~ "Open GCSE",
        grepl("OPEN", master_filter) ~ "Open",
        grepl("ENG", master_filter) ~ "English",
        grepl("MAT", master_filter) ~ "Maths",
        grepl("EBAC", master_filter) ~ "EBACC pupils",
        .default = "All subjects"
      ),
      value = case_when(
        value == "NE" ~ "z",
        value == "NP" ~ "x",
        value == "SUPP" ~ "c",
        value == "LOWCOV" ~ "low",
        stringr::str_trim(value) == "NA" ~ "z",
        is.na(value) ~ "z",
        .default = format(round(as.numeric(value), digits = 2), digits = 2, scientific = FALSE, trim = TRUE)
      )
    )

  ks4_a8_tidied <- ks4_a8 %>%
    select(-master_filter) %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    left_join(las, by = join_by(LEA == old_la_code))
  
  
  ks4_a8_ees <- ks4_a8_tidied %>%
    mutate(
      across(c("attainment_score_total", "attainment_score_average"), ~  if_else(is.na(.), 'z', .)),
      school_laestab = case_when(
        is.na(ESTAB) ~ "",
        ESTAB == "NA" ~ "",
        .default = paste0(LEA, ESTAB)
      ),
      school_urn = case_when(
        is.na(URN) ~ "",
        URN == "NA" ~ "",
        .default = paste0(URN)
      ),
      school_name = case_when(
        is.na(SCHNAME) ~ "",
        SCHNAME == "NA" ~ "",
        .default = SCHNAME
      ),
      time_identifier = "Academic year",
      geographic_level = case_when(
        is.na(ESTAB) ~ "Local authority",
        ESTAB == "NA" ~ "Local authority",
        .default = "School"
      ),
      country_code = "E92000001",
      country_name = "England"
    ) %>%
    select(
      time_period,
      time_identifier,
      geographic_level,
      country_code,
      country_name,
      old_la_code = LEA,
      new_la_code,
      la_name,
      school_laestab,
      school_urn,
      school_name,
      breakdown_topic,
      breakdown, 
      attainment_area,
      attainment_score_total,
      attainment_score_average
    ) %>%
    arrange(
      -as.numeric(time_period), old_la_code, school_laestab, 
      breakdown_topic, breakdown, attainment_area
      )

  write.csv(
    ks4_a8_ees %>%
      filter(geographic_level == "School"),
    paste0(data_folder, "cscp_ks4_attainment8_202223.csv"),
    row.names = FALSE
  )
  
  ks4_a8_meta_data <- data.frame(col_name = names(ks4_a8_ees)) %>%
    filter(
      !(col_name %in% c("time_period", "time_identifier", "geographic_level", 
                        "country_code", "country_name", 
                        "old_la_code", "new_la_code", "la_name", 
                        "school_laestab", "school_urn", "school_name"))) %>%
    mutate(
      col_type = "Filter",
      label = stringr::str_to_sentence(gsub("_", " ", col_name)),
      indicator_grouping = "",
      indicator_unit = "",
      indicator_dp = "",
      filter_hint = "",
      filter_grouping_column = ""
    )
  
  ks4_a8_meta_data$col_type[4:5] <- "Indicator"
  ks4_a8_meta_data$indicator_dp[4] <- "1"
  ks4_a8_meta_data$indicator_dp[5] <- "2"

  print(ks4_a8_meta_data)
  write.csv(ks4_a8_meta_data, paste0(data_folder, "cscp_ks4_attainment8_202223.meta.csv"), row.names = FALSE)
  
    
}



