library(readxl)
library(dplyr)
library(tidyr)


tidy_progress8 <- function() {
  las <- readr::read_csv("https://raw.githubusercontent.com/dfe-analytical-services/dfe-published-data-qa/master/data/las.csv") %>%
    filter(status == "live" | old_la_code == "909") %>%
    select(new_la_code, la_name, old_la_code) %>%
    distinct()

  ks4_data <- read_xlsx("data/cscp_raw/2022-2023_england_ks4_provisional.xlsx") %>%
    filter(!is.na(LEA)) %>%
    mutate(across(where(is.numeric), format, scientific = FALSE, trim = TRUE))


  info_cols <- c(
    "RECTYPE", "SCHNAME_AC",
    "ADDRESS1", "ADDRESS2", "ADDRESS3", "TOWN", "PCODE", "TELNUM",
    "CONTFLAG", "ICLOSE", "NFTYPE", "RELDENOM", "ADMPOL", "ADMPOL_PT", "EGENDER",
    "FEEDER", "TABKS2", "TAB1618", "AGERANGE"
  )

  school_info <- ks4_data %>% select(all_of(c("LEA", "ESTAB", "URN", "SCHNAME", info_cols)))

  ks4_data_noinfo <- ks4_data %>%
    select(-all_of(info_cols))

  ks4_sex <- ks4_data_noinfo %>%
    select(LEA, ESTAB, URN, TOTPUPS, NUMBOYS, NUMGIRLS) %>%
    rename(Male = NUMBOYS, Female = NUMGIRLS, Total = TOTPUPS) %>%
    pivot_longer(c(Total, Male, Female), names_to = "sex") %>%
    mutate(pupil_count = if_else(is.na(value), "z", value)) %>%
    select(-value)

  attainment_counts <- c("TPRIORLO", "TPRIORAV", "TPRIORHI")
  attainment_percents <- c("PTPRIORLO", "PTPRIORAV", "PTPRIORHI")

  p8_id_cols <- c("P8_HIDE_FLAG", "P8_BANDING", "P8_BANDING_FULL")
  p8cols <- names(ks4_data) %>%
    as.data.frame() %>%
    filter(grepl("P8", .), !(. %in% p8_id_cols)) %>%
    pull(.)


  ks4_p8 <- ks4_data_noinfo %>%
    select(all_of(c("LEA", "ESTAB", "URN", "SCHNAME", p8_id_cols, p8cols))) %>%
    pivot_longer(-all_of(c("LEA", "ESTAB", "URN", "SCHNAME", p8_id_cols)), names_to = "master_filter") %>%
    mutate(
      metric = case_when(
        grepl("CILOW", master_filter) ~ "attainment_score_lower_confidence",
        grepl("CIUP", master_filter) ~ "attainment_score_upper_confidence",
        grepl("P8MEA", master_filter) ~ "attainment_score",
        grepl("P8PUP", master_filter) ~ "pupil_count",
        grepl("TP8ADJ", master_filter) ~ "some_weird_adjustment_number",
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
        grepl("DIFFN_P8MEA", master_filter) ~ "Suspect this belongs somewhere else, but don't know what it is so leaving it here for now",
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
      some_weird_adjustment_number,
      attainment_score,
      attainment_score_lower_confidence,
      attainment_score_upper_confidence
    ) %>%
    arrange(-as.numeric(time_period), old_la_code, school_laestab, version, sex, free_school_meal_status, eal_status, mobility_status, attainment_area)

  print(ks4_p8_ees, n = 240)
  write.csv(
    ks4_p8_ees %>%
      filter(geographic_level == "School"),
    "data/ees_tidy/cscp_ks4_progress8_202223.csv",
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
  write.csv(ks4_p8_meta_data, "data/ees_tidy/cscp_ks4_progress8_202223.meta.csv", row.names = FALSE)
}
