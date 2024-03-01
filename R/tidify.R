pivot_filter_indicator <- function(df, filter_name = "grade",
                                   keep_filters = c(
                                     "time_period", "time_identifier", "version",
                                     "geographic_level", "country_code",
                                     "country_name",
                                     "school_type", "subject"
                                   )) {
  dftidied <- df %>% pivot_longer(!any_of(c(keep_filters)),
    names_to = c(".value", filter_name),
    names_pattern = "(.*)_(.*)"
  )
  return(dftidied)
}
