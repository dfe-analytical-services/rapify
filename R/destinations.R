library(readr)
library(dplyr)
library(tidyr)

tidy_destinations <- function(data_file, meta_file=NULL){
  if(is.null(meta_file)){meta_file <- gsub(".csv",".meta.csv",data_file)}
  data <- read_csv(data_file)
  meta <- read_csv(meta_file)
  outcomes <- meta %>% filter(col_type == "Indicator", col_name != "inst_count")
  tidy <- data %>% 
    mutate(across(where(is.numeric), as.character)) %>%
    pivot_longer(
      outcomes$col_name,
      names_to = "col_name",
      values_to = "value"
      ) %>%
    left_join(outcomes %>% select(col_name, label, indicator_grouping)) %>%
    pivot_wider(
      names_from = data_type,
      values_from = value
    ) %>%
    select(-col_name) %>%
    rename(
      outcome_group = indicator_grouping, 
      outcome = label,
      pupil_count = `Number of pupils`,
      pupil_percent = Percentage
      ) %>%
    mutate(
      pupil_percent = if_else(is.na(outcome_group), "100", pupil_percent),
      outcome_group = if_else(is.na(outcome_group), "Total", outcome_group),
      pupil_percent = if_else(is.na(pupil_percent), "x", pupil_percent)
    )
  write.csv(tidy, gsub(".csv", "_tidy.csv", data_file), row.names = FALSE)
  tidy_meta <- meta %>% 
    filter(!(col_name %in% outcomes$col_name), col_name != "data_type") %>%
    rows_append(
      data.frame(
        col_name = c("outcome", "pupil_count", "pupil_percent"),
        col_type = c("Filter", "Indicator", "Indicator"),
        label    = c("Outcome", "Number of pupils", "Percent of pupils"),
        filter_grouping_column = c("outcome_group", "", "")
      )
    ) %>%
    mutate(across(everything(), ~ if_else(is.na(.), "", .)))
  write.csv(tidy_meta, gsub(".csv", "_tidy.meta.csv", data_file), row.names = FALSE)
  
}