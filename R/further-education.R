library(readr)
library(dplyr)
library(lubridate)

# Convert times to attendance structure
# Clean up abbreviations and acronyms
# Make sure filter_grouping_columns are being used

app_monthly <- read_csv("../../offline-data/further-education/app-monthly-starts-202425-jan.csv")
app_monthly_api <- app_monthly |>
  rename(
    age_group = age_summary,
    assessment_type = std_fwk_flag,
    apprenticeship_level = apps_level,
    apprenticeship_start_count = starts
  ) |>
  mutate(
    start_month = if_else(
      start_month != "Total",
      paste("01", start_month, "2024") |>
        dmy() |>
        month(label = TRUE, abbr = FALSE),
      start_month
    ),
    apprenticeship_level = stringr::str_to_sentence(apprenticeship_level)
  )

app_monthly_api |> write_csv(
  "../../offline-data/further-education/app-monthly-starts-202425-jan_api_demo.csv"
)


fes_aims_achievments <- read_csv("../../offline-data/further-education/fes-underlying-data-aims-achievements-202425-q1.csv")
