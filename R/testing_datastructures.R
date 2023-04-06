source('global.R')

scenario_characteristics <- function(
    original='data/workforce_teacher_characteristics_NatReg_202022.csv'){
  # Create filters versus characteristics filter data file pair.
  dfchars <- read.csv('data/workforce_teacher_characteristics_NatReg_202022.csv', stringsAsFactors = FALSE)
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
  dfexmp <- dfexmp %>% filter(table!='S7', religious_denomination=='Total', 
                              admission_type=='Total', school_type=='All schools') %>% 
    select(-table, -religious_denomination, -admission_type, -school_type, 
           -percentage_level1pass, -percentage_level2pass,-total_level1pass, -total_level2pass)
  write.csv(dfexmp,'data/data-structure-testing/scenSmushing-wide-ks4_subject_main_202122.csv',row.names=FALSE)
  
  x<-pivot_filter_indicator(
    dfexmp,
    keep_filters=c("time_period","time_identifier",
                   "geographic_level","country_code","country_name",
                   "gender","subject",
                   "qualification_type","subject_entry")) %>%
    filter(!(total == 'z' & percentage == 'z'), !(grade %in% c("level1pass", "level2pass"))) %>%
    distinct() %>% 
    group_by(time_period, time_identifier, geographic_level, country_code, country_name, gender, 
             subject, qualification_type, grade) %>% 
    summarise(subject_entry=max(subject_entry),total=max(total), percentage=max(percentage)) %>%
    mutate(grade=case_when(grade=='atob' ~ "A to B",
                           grade=='atoe' ~ "A to E",
                           grade=='9to1' ~ "9 to 1",
                           grade=='9to4' ~ "9 to 4",
                           grade=='9to5' ~ "9 to 5",
                           TRUE ~ grade))
  write.csv(x,'data/data-structure-testing/scenSmushing-tidy-ks4_subject_main_202122.csv',row.names=FALSE)
  return(x)
}

scenario_doubletimeframes <- function(){
  # Create different options from pupil projections input using multiple timeframe data.
  dfwide <- read.csv('data/pupilprojections_202122.csv',stringsAsFactors = FALSE) %>%
    filter(school_type=='All schools', pupil_age=='All', pupil_status=='FTE') %>% 
    select(-school_type,-pupil_age,-pupil_status)
  # dfwide <- dfwide %>% rbind(dfwide %>% mutate(time_period=2021,X2022_actual='z'))
  colnames(dfwide) <- gsub('X','',colnames(dfwide))
  write.csv(dfwide,"data/data-structure-testing/scenDbleTime-wide-pupilprojections.csv",row.names = FALSE)
  
  dfnew <- pivot_filter_indicator(
    dfwide, filter_name='Projection year', 
    keep_filters=c("time_period","time_identifier","geographic_level","country_code",
                   "country_name","projection_type"),
    order='fv') %>% rename(projection_year=`Projection year`) %>%
    mutate(projection=as.numeric(gsub(',','',projection)),
           projection_year=as.numeric(gsub("X", "",projection_year)))
  
  dftidy <- dfnew %>% filter(!is.na(projection)) %>% mutate(actual='z') %>%
    rbind(dfnew %>% filter(!is.na(actual) & actual != 'z') %>% 
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
    arrange(time_period, projection_year)
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

attainemnt_1619 <- function(
    folder = 'c:/Users/RBIELBY/Department\ for\ Education/Data\ Insights\ and\ Statistics\ -\ CSSU/Statistics\ Development\ Team/Explore\ Education\ Statistics/User\ research/dataset_examples/time_periods/age19_attainment/',
    file = 'level_2_3_ages_16_19_characteristics_original.csv'
    ){
  data <- read.csv(paste0(folder,file))
  data_cohort <- data %>% 
    filter(age != '19 not 16', characteristic != 'Attainment gap') %>%
    mutate(age = as.numeric(age)) %>%
    mutate(
      cohort_year = paste0('CY ',gsub('^([0-9]{4})([0-9]+)$', '\\1/\\2', time_period)),
      time_period = time_period-101*(19-age)
      ) %>%
    pivot_longer(!any_of(c("time_period", "time_identifier", "geographic_level", 
                    "country_code", "country_name", "cohort_year", "age", "characteristic_group",
                    "characteristic", "number_or_percentage")),
                 names_to='attainment'
                    ) %>%
    pivot_wider(names_from = number_or_percentage, values_from = value) %>%
    mutate(
      Percentage = ifelse(attainment == 'number_in_ss_cohort','z',Percentage),
      attainment = case_when(
        attainment == "number_in_ss_cohort" ~ 'State sector cohort',
        attainment == 'Level_2' ~ 'Level 2',
        attainment == 'Level_3' ~ 'Level 3',
        attainment == 'L2_eng_GCSE_ac' ~ 'Level 2 in English (GCSE only)',
        attainment == 'L2_eng_GCSE_othL2' ~ 'Level 2 in English (all qualifications)',
        attainment == 'L2_maths_GCSE_ac' ~ 'Level 2 in Maths (GCSE only)',
        attainment == 'L2_maths_GCSE_othL2' ~ 'Level 2 in Maths (all qualifications)',
        attainment == 'L2_em_GCSE_ac' ~ 'Level 2 in English and Maths (GCSEs only)',
        attainment == 'L2_em_GCSE_othL2' ~ 'Level 2 in English and Maths (all qualifications)',
        attainment == 'L2_with_EM' ~ 'Level 2 with English and Maths',
        attainment == 'L2_eng_FSQ' ~ 'Level 2 English via Functional Skills Qualifications',
        attainment == 'L2_maths_FSQ' ~ 'Level 2 maths via Functional Skills Qualifications',
        TRUE ~ attainment
      ),
      attainment_group = case_when(
        attainment == "number_in_ss_cohort" ~ 'Total',
        attainment == 'Level_2' ~ 'Level 2',
        attainment == 'Level_3' ~ 'Level 3',
        attainment == 'L2_with_EM' ~ 'Level 2 with English and Maths',
        TRUE ~ 'Level 2 English and Maths'
      ),
      Number = ifelse(is.na(Number),'x',Number),
      Percentage = ifelse(is.na(Percentage),'x',Percentage)
    ) %>%
    arrange(-time_period, country_code, cohort_year, attainment) %>%
    select(time_period, time_identifier, geographic_level, country_code, country_name, 
           cohort_year, age, characteristic, characteristic_group,
           attainment,number=Number, percentage=Percentage)
  
  all_cohort <- data_cohort %>% 
    group_by(time_period, time_identifier, geographic_level, country_code, country_name,
             age, characteristic, characteristic_group, attainment) %>% 
    summarise(
      number=as.character(sum(as.numeric(number),rm.na=TRUE)), 
      percentage=as.character(mean(as.numeric(percentage),rm.na=TRUE))
      ) %>% 
    mutate(cohort_year='All') %>% 
    select(time_period, time_identifier, geographic_level, country_code, country_name, 
           cohort_year, age, characteristic, characteristic_group,
           attainment,number, percentage)
  all_ages <- data_cohort %>% 
    group_by(time_period, time_identifier, geographic_level, country_code, country_name,
             cohort_year, characteristic, characteristic_group, attainment) %>% 
    summarise(
      number=as.character(sum(as.numeric(number),rm.na=TRUE)), 
      percentage=as.character(mean(as.numeric(percentage),rm.na=TRUE))
    ) %>% 
    mutate(age='All') %>%
    select(time_period, time_identifier, geographic_level, country_code, country_name, 
           cohort_year, age, characteristic, characteristic_group,
           attainment,number, percentage)
  data_1 <- data_cohort %>% rbind(all_cohort) %>% rbind(all_ages) %>%
    mutate(
      number = ifelse(is.na(number),'x',number),
      percentage = ifelse(is.na(percentage),'x',percentage)
    ) %>%
    arrange(-time_period, country_code, cohort_year, age, attainment)
  write.csv(data_1, paste0(folder, "level_2_3_ages_16_19_characteristics_tidy_1.csv"),row.names = FALSE)

  
  all_ages_2 <- data_cohort %>% 
    group_by(time_period, time_identifier, geographic_level, country_code, country_name,
             characteristic, characteristic_group, attainment) %>% 
    summarise(
      number=as.character(sum(as.numeric(number),rm.na=TRUE)), 
      percentage=as.character(mean(as.numeric(percentage),rm.na=TRUE))
    ) %>% 
    mutate(age='All') %>%
    select(time_period, time_identifier, geographic_level, country_code, country_name, 
           age, characteristic, characteristic_group,
           attainment,number, percentage)

    data_2 <- data_cohort %>% 
      select(-cohort_year) %>% 
      rbind(all_ages_2) %>%
    mutate(
      number = ifelse(is.na(number),'x',number),
      percentage = ifelse(is.na(percentage),'x',percentage)
    )  %>%
    arrange(-time_period, country_code, age, attainment)
  write.csv(data_2, paste0(folder, "level_2_3_ages_16_19_characteristics_tidy_2.csv"),row.names = FALSE)
  list(data_1=data_1,data_2=data_2)
}

