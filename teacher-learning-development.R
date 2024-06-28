library(readr)
source('R/tidify.R') 

teacher_leader_development_ecf_2022_23 <- read_csv("data/teacher_leader_development_ecf_2022-23.csv")
tidydata <-pivot_filter_indicator(teacher_leader_development_ecf_2022_23 ,
                                  keep_filters = c("time_period"   ,                                      "time_identifier"  ,                                  
                                                   "geographic_level"     ,                               "country_code"  ,                                     
                                                   "country_name" ,                                       "region_code" ,                                       
                                                 "region_name"  ,                                       "la_name"   ,                                         
                                              "old_la_code"  ,                                       "new_la_code",                                        
                                            "breakdown"  ,                                         "group_name"  ) )

View(tidydata)
print(tidydata$grade %>% unique())

# Read in meta file
ecf_meta <- read_csv("data/teacher_leader_development_ecf_2022-23.meta.csv")
View(ecf_meta)
