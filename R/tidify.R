library(dplyr)
library(tidyr)
pivot_filter_indicator <- function(df, filter_name = "grade",
                                   keep_filters = c(
                                     "time_period", "time_identifier", "version",
                                     "geographic_level", "country_code",
                                     "country_name",
                                     "school_type", "subject"
                                   )) {
  dftidied <- df %>% pivot_longer(!any_of(c(keep_filters)),
    names_to = c(".value", filter_name),
    names_pattern = "(.*?)_(.*)"
  )
  return(dftidied)
}
# The pivot longer function- it transforms data from a wide format to a long format
# is a function of the (tidyr) package, which is part of the tidyverse collection of packages
# in a wide format data is spread across multiple columns with different variables,
# in a long format data is gathered into key-value pairs which makes it easier.
# the syntax : pivot_longer (data,cols, names_to = "name", values_to = "value")
# this enables more flexible, data manipulation and analysis