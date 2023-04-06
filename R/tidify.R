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

demo_destack <- function(){
  dfin <- read.csv('data/workforce_teacher_characteristics_NatReg_202022.csv',stringsAsFactors = FALSE)
  df <- dfin %>% filter(geographic_level=='National') %>%
    select(time_period, geographic_level, school_type,
           characteristic_group, characteristic, 
           full_time_equivalent, headcount)
}

# destack_filters <- function(df){
#   filters <- df %>% filter(!grepl('and',characteristic_group)) %>% select(characteristic_group, characteristic) %>% distinct()
#   
#   for (filter in filters$characteristic_group){
#     filter_options <- filters %>% filter(characteristic_group==filter) %>% pull(characteristic)
#     df <- df %>% mutate(get(filter)=ifelse(grepl(filter,characteristic_group),characteristic,'Total'))
#   }
# }
