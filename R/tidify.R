pivot_filter_indicator <- function(df, filter_name='grade',
                                   keep_filters = c("time_period","time_identifier","version",
                                                    "geographic_level", "country_code", 
                                                    "country_name", 
                                                    "school_type", "subject"),
                                   order='vf'){
  if (order=='vf'){names_to <- c(".value", filter_name)} else {names_to <- c(filter_name,".value")}
  dftidied <- df %>% pivot_longer(!any_of(c(keep_filters)),
                                  names_to = names_to,
                                  names_pattern = "(.*)_(.*)")
  return(dftidied)  
}
