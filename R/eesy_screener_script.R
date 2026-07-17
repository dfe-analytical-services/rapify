# install.packages("remotes")
# remotes::install_github("dfe-analytical-services/eesyscreener")

library(eesyscreener)

home_dir <- Sys.getenv("HOME") |> strsplit("\\\\")

data_folder <- "./attendance data/"

eesyscreener::screen_csv(
  file.path(data_folder, "pupil-attendance-reasons_2026_week26.csv"),
  file.path(data_folder, "pupil-attendance-reasons_2026_week26.meta.csv")
)
