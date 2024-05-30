library(readr)
library(dplyr)
library(tidyr)


#' Tidy destinations
#'
#' @param data_file 
#' @param meta_file 
#'
#' @return list(data, meta)
#' @export
#'
#' @examples
#' output <- tidy_destinations("data/destinations/ks4_dm_ud_202122_nat_rev.csv")
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
    left_join(
      outcomes %>% select(col_name, label, indicator_grouping),
      by = join_by(col_name)
      ) %>%
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
  message(paste0("Written tidy data to: ", gsub(".csv", "_tidy.csv", data_file)))
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
  message(paste0("Written meta info to: ", gsub(".csv", "_tidy.meta.csv", data_file)))
  return(list(data = tidy, meta = tidy_meta))
}