library(readr)
library(dplyr)

data_dir <- "../../offline-data/childcare-early-years-provider-survey/"

provider_staff <- function(){
  apprentices <- read_csv(
    paste0(data_dir,"sceyp24_providers_employing_apprentices.csv")
  )|>
    mutate(
      staff_type = "Apprentices", .after = provider_type) |>
    rename(
      staff_count = number_apprentices
    )
  
  temporary <- read_csv(
    paste0(data_dir,"sceyp24_providers_employing_temporary_staff.csv")
  ) |>
    mutate(staff_type = "Temporary staff", .after = provider_type) |>
    rename(staff_count = number_staff)
  volunteer <- read_csv(
    paste0(data_dir,"sceyp24_providers_employing_volunteer_staff.csv")
  ) |>
    mutate(staff_type = "Volunteer staff", .after = provider_type) |>
    rename(staff_count = number_vol_staff)
  
  staff <- bind_rows(
    apprentices,
    temporary,
    volunteer
  )
  
  staff_meta <- data.frame(col_name = names(staff)) %>%
    filter(
      !(col_name %in% c("time_period", "time_identifier", "geographic_level", 
                        "country_code", "country_name", "old_la_code", 
                        "new_la_code", "la_name", "school_laestab", 
                        "school_urn", "school_name"))) %>%
    mutate(
      col_type = case_when(
        grepl("type", col_name) ~ "Filter",
        .default = "Indicator"
      ),
      label = stringr::str_to_sentence(gsub("_", " ", col_name)),
      indicator_grouping = "",
      indicator_unit = "",
      indicator_dp = "",
      filter_hint = "",
      filter_grouping_column = ""
    )
  
  write.csv(staff, paste0(data_dir, "sceyp24_provider_staff_type.csv"), row.names = FALSE)
  write.csv(staff_meta, paste0(data_dir, "sceyp24_provider_staff_type.meta.csv"), row.names = FALSE)
  
}