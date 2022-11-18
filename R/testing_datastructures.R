
scenario_characteristics <- function(
    original='data/workforce_teacher_characteristics_NatRegLA_201021.csv'){
  dfchars <- read.csv(original, stringsAsFactors = FALSE)
  dfnational <- dfchars %>% 
    filter(geographic_level=='National')
  return(dfnational)
}



write.csv(dfexmp,'data/ks4_subject_main_wide_202122.csv',row.names=FALSE)


x<-pivot_filter_indicator(dfexmp,
                          keep_filters=c("time_period","time_identifier",
                                         "geographic_level","country_code","country_name",
                                         "gender","school_type",
                                         "admission_type",
                                         "religious_denomination","subject",
                                         "qualification_type","subject_entry"))
write.csv(x,'data/ks4_subject_main_tidy_202122.csv',row.names=FALSE)


