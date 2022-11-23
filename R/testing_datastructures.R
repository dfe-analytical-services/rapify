source('global.R')

scenario_characteristics <- function(
    original='data/workforce_teacher_characteristics_NatReg_202022.csv'){
  # Create filters versus characteristics filter data file pair.
  dfchars <- read.csv(original, stringsAsFactors = FALSE)
  dfnational <- dfchars %>% 
    filter(geographic_level %in% c('National','Regional'),
           time_period %in% c(202122,202021)) %>%
    select(-old_la_code, -new_la_code, -la_name) %>% 
    distinct()

  df_filters <- dfnational %>% select(-characteristic,-characteristic_group) %>%
    distinct() %>% 
    mutate(ethnicity_major=trimws(ethnicity_major), 
           ethnicity_minor=trimws(ethnicity_minor),
           ethnic_minority=trimws(ethnic_minority))
  write.csv(df_filters,'data/data-structure-testing/scenCharFilt-filters-workforce_teacher_chars_NatReg_202022.csv',row.names=FALSE)
  df_characs <- dfnational %>% select(-grade,-gender,-age_group,-working_pattern,
                                      -qts_status,-on_route,-ethnicity_major,
                                      -ethnicity_minor,-ethnic_minority) %>%
    distinct() %>% mutate(characteristic=trimws(characteristic), 
                          characteristic_group=trimws(characteristic_group))
  
  write.csv(df_characs,'data/data-structure-testing/scenCharFilt-characteristics-workforce_teacher_chars_NatReg_202022.csv',row.names=FALSE)
  return(dfnational)
}

scenario_smushing <- function(){
  # Create a wide format and tidy format data file pair.
  dfexmp <- read.csv('data/ks4_subject_main_wide_202122.csv')
  
  write.csv(dfexmp,'data/data-structure-testing/scenSmushing-wide-ks4_subject_main_202122.csv',row.names=FALSE)
  
  x<-pivot_filter_indicator(
    dfexmp,
    keep_filters=c("time_period","time_identifier",
                   "geographic_level","country_code","country_name",
                   "gender","school_type",
                   "admission_type",
                   "religious_denomination","subject",
                   "qualification_type","subject_entry")) %>%
    filter(!(total == 'z' & percentage == 'z'))
  write.csv(x,'data/data-structure-testing/scenSmushing-tidy-ks4_subject_main_202122.csv',row.names=FALSE)
}

scenario_doubletimeframes <- function(){
  # Create different options from pupil projections input using multiple timeframe data.
  dfppp <- read.csv('data/pupilprojprincipal_2022.csv',stringsAsFactors = FALSE)
  dfwide <- dfppp %>% rbind(
    dfppp %>% mutate(time_period=2021, X2022_actual='z')
  )
  write.csv(dfwide,"data/data-structure-testing/scenDbleTime-wide-pupilprojections.csv",row.names = FALSE)
  
  
  dfnew <- pivot_filter_indicator(
    dfwide, filter_name='Projection year', 
    keep_filters=c("time_period","time_identifier","geographic_level","country_code",
                   "country_name","projection_type","school_type","pupil_status","pupil_age"),
    order='fv') %>% rename(projection_year=`Projection year`) %>%
    mutate(projection=as.numeric(gsub(',','',projection)),
           projection_year=as.numeric(gsub("X", "",projection_year)))
  
  dftidy <- dfnew %>% filter(!is.na(projection)) %>% mutate(actual='z') %>%
    rbind(dfnew %>% filter(!is.na(actual)) %>% 
            mutate(time_period=projection_year,
                   projection_year='z',
                   projection='z')) %>%
    distinct() %>%
    mutate(actual=ifelse(actual %in% c('z','c'),actual,as.numeric(gsub(",","",actual)))) %>%
    arrange(time_period, projection_year)
  write.csv(dftidy,"data/data-structure-testing/scenDbleTime-tidy-a-pupilprojections.csv",row.names = FALSE)
  
  dftidy_alt <- dftidy %>% filter(actual != 'z') %>% mutate(population=actual, projection_year='Actual') %>%
    rbind(dftidy %>% filter(!is.na(projection)) %>% mutate(population=projection)) %>%
    select(-projection, -actual) %>%
    filter(!(projection_year=='z' & population == 'z')) %>%
    arrange(pupil_status, school_type, pupil_status, time_period, projection_year)
  write.csv(dftidy_alt,"data/data-structure-testing/scenDbleTime-tidy-b-pupilprojections.csv",row.names = FALSE)
  
  return(dftidy_alt)
}

scenario_annualchange <- function(){
  dfneet <- read.csv("data/neet_and_net_estimates_from_the_lfs_2021.csv",stringsAsFactors = FALSE)
  categories <- c("NEET", "NET", "ILOunemployedless6months", "ILOunemployedmore6months",
                  "inactivewantsajob", "inactivedoesnotwantajob", "inactivelookingafterfamilyorhome",
                  "inactivelong.termortemporarysick", "inactiveother")
  filterlabs <- c("NEET", "NET", "ILO unemployed less then 6 months", "ILO unemployed more then 6 months",
                  "Inactive - wants a job", "Inactive - does not want a job", "Inactive - looking after family or home",
                  "Inactive - long-term or temporary sick", "Inactive - other")
  
  colnames(dfneet)[12:92] <- gsub("reason","",gsub("_","",colnames(dfneet)[12:92]))
  for (cat in categories){
    colnames(dfneet) <- gsub(cat,paste0("_",cat),colnames(dfneet))
  }
  for (col in colnames(dfneet)){
    if (type_sum(dfneet[,col])=='dbl'){dfneet[,col] <- as.character(dfneet[,col])}
  }
  filters <- c("time_period","time_identifier","quarter","geographic_level","country_code",
               "country_name","region_code","region_name","gender","age")
  
  dfnew <- pivot_filter_indicator(
    dfneet, filter_name='status', 
    keep_filters=filters,
    order='vf') %>% filter(!is.na(status))
  dfnew <- dfnew %>% select(-lowerboundnumber,-lowerboundpercent,-lowerboundpercentannualchange,
                            -upperboundnumber,-upperboundpercent,-upperboundpercentannualchange,
                            -confintervalplusminus,-percentconfintervalplusminus,-percentannualchangeconfintervalplusminus) %>%
    distinct() %>%
    mutate(percentannualchange = ifelse(is.na(percentannualchange),'x',percentannualchange),
           employmentrate = ifelse(is.na(employmentrate),'x',employmentrate)) %>%
    rename(annual_change=percentannualchange,
           employment_rate=employmentrate) %>%
    filter(!(number=='z' & percent=='z' & annual_change=='x' & employment_rate=='x')) %>%
    arrange(time_period,time_identifier,geographic_level,country_code,country_name,region_code,region_name,gender,age,status)
  for(i in 1:length(categories)){dfnew$status <- gsub(categories[i],filterlabs[i],dfnew$status)}
  write.csv(dfnew,"data/data-structure-testing/scenAnnualChange-indicator-neet.csv",row.names = FALSE)
  return(dfnew)
}