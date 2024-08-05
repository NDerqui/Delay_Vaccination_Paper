#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Timeliness of 24 childhood immunisations and evolution of vaccination delay:#
#-#-#-#-#- analysis of data from 54 low- and middle-income countries -#-#-#-#-#


# 2/ Clean raw data: DHS files



# SET UP ------------------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(geoutils)
library(rdhs)



# REFERENCE ---------------------------------------------------------------


#### DHS ####

countries <- dhs_countries() %>%
  select(CountryName, WHO_CountryCode, DHS_CountryCode, ISO3_CountryCode, RegionName, SubregionName)
colnames(countries) <- c("country", "admin0_code", "dhs_country", "iso3", "region", "subregion")

unique(countries$region)


#### WHO ####

data("admin0")

whoregions <- admin0 %>%
  select(who_region_code, admin0_code)
rm(admin0)



# LOOP -----------------------------------------------------------------


steps <- c(1:2)


#### Data ####

for (step in 1:2) {
  
  
  #### Get ####
  
  data <- readRDS(file = paste0("Data/All_Raw_Vax_data_Most_recent_", steps[step], ".Rds"))
  
  
  #### Set Variables ####
  
  information <- read.csv("Data/All_Vax_available_Most_recent.csv")
  
  var_basic <- c("CASEID", "V000", "V001", "V002", "V005", "V006", "V007")
  var_basic_names <- c("id", "country-phase", "cluster", "hh", "weight", "month", "year")
  var_basic_dir <- data.frame(var = var_basic, name = var_basic_names)
  
  var_hh <- c("V102", "V103", "V190", "V106", "V108", "V501", "MV717", "V701", "V703", "V705", "V201")
  var_hh_names <- c("urban", "city", "wealth", "mother_education", "mother_literacy", "married", "mother_occupation", "husband_edu", "husband_lit", "husband_ocu", "total_kids")
  var_hh_dir <- data.frame(var = var_hh, name = var_hh_names)
  
  var_demo <- c("B1", "B2", "B17", "B4", "B5", "B8", "B19", "H1", "BORD")
  var_demo_names <- c("dob_month", "dob_year", "dob_day", "sex", "alive", "age_y", "age_m", "health_card", "birth_order")
  var_demo_dir <- data.frame(var = var_demo, name = var_demo_names)
  
  var_vax <- c("H2", "H2D", "H2M", "H2Y",
               "H0", "H0D", "H0M", "H0Y",
               "H3", "H3D", "H3M", "H3Y", "H4", "H4D", "H4M", "H4Y",
               "H5", "H5D", "H5M", "H5Y", "H6", "H6D", "H6M", "H6Y",
               "H7", "H7D", "H7M", "H7Y", "H8", "H8D", "H8M", "H8Y",
               "H9", "H9D", "H9M", "H9Y", "H9A", "H9AD", "H9AM", "H9AY",
               "H60", "H60D", "H60M", "H60Y",
               "H50", "H50D", "H50M", "H50Y", "H61", "H61D", "H61M", "H61Y", "H62", "H62D", "H62M", "H62Y", "H63", "H63D", "H63M", "H63Y",
               "H54", "H54D", "H54M", "H54Y", "H55", "H55D", "H55M", "H55Y", "H56", "H56D", "H56M", "H56Y",
               "H57", "H57D", "H57M", "H57Y", "H58", "H58D", "H58M", "H58Y", "H59", "H59D", "H59M", "H59Y",
               "H64", "H64D", "H64M", "H64Y", "H65", "H65D", "H65M", "H65Y", "H66", "H66D", "H66M", "H66Y",
               "H51", "H51D", "H51M", "H51Y", "H52", "H52D", "H52M", "H52Y", "H53", "H53D", "H53M", "H53Y")
  var_vax_names <- c("BCG", "BCG_d", "BCG_mo", "BCG_y",
                     "OPV0", "OPV0_d", "OPV0_mo", "OPV0_y",
                     "DTP1", "DTP1_d", "DTP1_mo", "DTP1_y", "OPV1", "OPV1_d", "OPV1_mo", "OPV1_y",
                     "DTP2", "DTP2_d", "DTP2_mo", "DTP2_y", "OPV2", "OPV2_d", "OPV2_mo", "OPV2_y",
                     "DTP3", "DTP3_d", "DTP3_mo", "DTP3_y", "OPV3", "OPV3_d", "OPV3_mo", "OPV3_y",
                     "MCV1", "MCV1_d", "MCV1_mo", "MCV1_y", "MCV2", "MCV2_d", "MCV2_mo", "MCV2_y",
                     "IPV1", "IPV1_d", "IPV1_mo", "IPV1_y",
                     "HepB0", "HepB0_d", "HepB0_mo", "HepB0_y", "HepB1", "HepB1_d", "HepB1_mo", "HepB1_y", "HepB2", "HepB2_d", "HepB2_mo", "HepB2_y", "HepB3", "HepB3_d", "HepB3_mo", "HepB3_y",
                     "Pneumo1", "Pneumo1_d", "Pneumo1_mo", "Pneumo1_y", "Pneumo2", "Pneumo2_d", "Pneumo2_mo", "Pneumo2_y", "Pneumo3", "Pneumo3_d", "Pneumo3_mo", "Pneumo3_y",
                     "Rota1", "Rota1_d", "Rota1_mo", "Rota1_y", "Rota2", "Rota2_d", "Rota2_mo", "Rota2_y", "Rota3", "Rota3_d", "Rota3_mo", "Rota3_y",
                     "Hib1", "Hib1_d", "Hib1_mo", "Hib1_y", "Hib2", "Hib2_d", "Hib2_mo", "Hib2_y", "Hib3", "Hib3_d", "Hib3_mo", "Hib3_y",
                     "Penta1", "Penta1_d", "Penta1_mo", "Penta1_y", "Penta2", "Penta2_d", "Penta2_mo", "Penta2_y", "Penta3", "Penta3_d", "Penta3_mo", "Penta3_y")
  var_vax_dir <- data.frame(var = var_vax, name = var_vax_names)
  
  var_atypical <- unique(information$var[information$typical == 0])
  
  # Checks
  
  table(var_basic %in% colnames(data))
  table(var_demo %in% colnames(data))
  table(var_hh %in% colnames(data))
  
  table(var_vax %in% colnames(data))      # Only antigen specific vars
  table(var_atypical %in% colnames(data)) # Only antigen specific vars
  
  
  #### Colnames ####
  
  # Renaming to easy-to-handle names
  
  for (i in 1:ncol(data)) {
    
    if(names(data)[i] %in% var_basic_dir$var) {
      names(data)[i] <- var_basic_dir$name[var_basic_dir$var == names(data)[i]]
    }
    if(names(data)[i] %in% var_demo_dir$var) {
      names(data)[i] <- var_demo_dir$name[var_demo_dir$var == names(data)[i]]
    }
    if(names(data)[i] %in% var_hh_dir$var) {
      names(data)[i] <- var_hh_dir$name[var_hh_dir$var == names(data)[i]]
    }
    if(names(data)[i] %in% var_vax_dir$var) {
      names(data)[i] <- var_vax_dir$name[var_vax_dir$var == names(data)[i]]
    }
  }
  
  rm(var_basic, var_demo, var_hh, var_vax, var_atypical)
  
  # Set our vaccination variables of interest
  
  var_vax_names_info <- c("BCG", "OPV0", "DTP1", "OPV1", "DTP2", "OPV2", "DTP3", "OPV3",
                          "MCV1", "MCV2", "IPV1", "HepB0", "HepB1", "HepB2", "HepB3",
                          "Pneumo1", "Pneumo2", "Pneumo3", "Rota1", "Rota2", "Rota3",
                          "Hib1", "Hib2", "Hib3", "Penta1", "Penta2", "Penta3")
  var_vax_names_time <- var_vax_names[!(var_vax_names %in% var_vax_names_info)]
  
  var_atypical_names <- unique(information$var[information$typical == 0 & information$date_info == 0])
  var_atypical_dates <- unique(information$var[information$typical == 0 & information$date_info == 1])
  
  # Subset to the variables we have (depending on each data set - different)
  
  var_vax_names_info <- var_vax_names_info[var_vax_names_info %in% colnames(data)]
  var_vax_names_time <- var_vax_names_time[var_vax_names_time %in% colnames(data)]
  var_atypical_names <- var_atypical_names[var_atypical_names %in% colnames(data)]
  var_atypical_dates <- var_atypical_dates[var_atypical_dates %in% colnames(data)]
  
  # Same for socio-economic, because some vars are picked and others aren't
  
  var_demo_names <- var_demo_names[var_demo_names %in% colnames(data)]
  var_hh_names <- var_hh_names[var_hh_names %in% colnames(data)]
  
  
  #### General ####
  
  # Get the country
  
  data <- data %>%
    mutate(dhs_country = substr(`country-phase`, 1, 2))
  data <- merge(data, countries, by = "dhs_country", all.x = TRUE)
  
  # Get the region
  
  data <- merge(data, whoregions, by = "admin0_code", all.x = TRUE)
  
  # Time: phase and year
  
  data <- data %>%
    mutate(year = as.numeric(year)) %>%
    mutate(month = as.numeric(month)) %>%
    mutate(year = case_when(year >= 1000 ~ year,
                            year < 1000 & year > 10 ~ year+1900,
                            year <= 10 ~ year+2000)) %>%
    mutate(year = case_when(year >= 1980 & year <= 2023 ~ year,
                            year < 1980 | year > 2023 ~ NA_real_)) %>%
    mutate(phase = as.numeric(substr(`country-phase`, 3, 3)))
  
  # ID
  
  data <- data %>%
    mutate(cluster = as.numeric(cluster)) %>%
    mutate(hh = as.numeric(hh)) %>%
    mutate(DHSID = paste0(dhs_country, year, str_pad(cluster, 8, pad = "0"))) %>%
    mutate(cluster_unique = paste0(dhs_country, "_", year, "_", cluster)) %>%
    mutate(hh_unique = paste0(dhs_country, "_", year, "_", cluster, "_", hh)) 
  
  # Class of vars: demographic and then others
  
  data <- data %>%
    mutate(dob_month = as.numeric(dob_month)) %>%
    mutate(dob_year = as.numeric(dob_year)) %>%
    mutate(dob_day = as.numeric(dob_day)) %>%
    mutate(sex = as.factor(sex)) %>%
    mutate(age_y = as.numeric(age_y)) %>%
    mutate(age_m = as.numeric(age_m)) %>%
    mutate(alive = as.factor(alive)) %>%
    mutate(health_card = as.numeric(health_card))  %>%
    mutate(birth_order = as.numeric(birth_order))
  
  data <- data %>%
    mutate_at(var_hh_names, list( ~ as.numeric(.))) %>%
    mutate_at(var_vax_names_info, list( ~ as.numeric(.))) %>%
    mutate_at(var_vax_names_time, list( ~ as.numeric(.))) %>%
    mutate_at(var_atypical_names, list( ~ as.numeric(.))) %>%
    mutate_at(var_atypical_dates, list( ~ as.numeric(.)))
  
  
  #### Decoding ####
  
  ## All vaccination vars to binary
  
  data <- data %>%
    mutate_at(c(var_vax_names_info, var_atypical_names),
              list( ~ case_when(. >= 1 & . <= 3 ~ 1,
                                . == 0 ~ 0,
                                . > 3 ~ NA_real_)))
  
  ## Demographic and socio-economic: if present, decode / if not, create NA
  
  data <- data %>%
    mutate(urban = case_when(urban <= 2 ~ urban,
                             urban > 2 ~ NA_real_))
  
  if ("city" %in% colnames(data)) {
    data <- data %>%
      mutate(city = case_when(city >= 1 & city <= 3 ~ city,
                              city < 1 | city > 3 ~ NA_real_))
  } else {
    data <- data %>%
      mutate(city = NA)
  }
  
  if ("wealth" %in% colnames(data)) {
    data <- data %>%
      mutate(wealth = case_when(wealth >= 1 & wealth <= 5 ~ wealth,
                                wealth < 1 | wealth > 5 ~ NA_real_))
  } else {
    data <- data %>%
      mutate(wealth = NA)
  }
  
  if ("mother_education" %in% colnames(data)) {
    data <- data %>%
      mutate(mother_education = case_when(mother_education <= 3 ~ mother_education,
                                          mother_education > 3 ~ NA_real_))
  } else {
    data <- data %>%
      mutate(mother_education = NA)
  }
  
  if ("mother_literacy" %in% colnames(data)) {
    data <- data %>%
      mutate(mother_literacy = case_when(mother_literacy >= 1 & mother_literacy <= 3 ~ mother_literacy,
                                         mother_literacy < 1 | mother_literacy > 3 ~ NA_real_))
  } else {
    data <- data %>%
      mutate(mother_literacy = NA)
  }
  
  if ("mother_occupation" %in% colnames(data)) {
    data <- data %>%
      mutate(mother_occupation = case_when(mother_occupation == 0 ~ 0,
                                           mother_occupation == 1 | mother_occupation == 2 | mother_occupation == 3 ~ 1,
                                           mother_occupation == 4 | mother_occupation == 5 ~ 2,
                                           mother_occupation == 6 | mother_occupation == 7 ~ 3,
                                           mother_occupation == 8 | mother_occupation == 9 ~ 4,
                                           mother_occupation > 9 ~ NA_real_))
  } else {
    data <- data %>%
      mutate(mother_occupation = NA)
  }
  
  if ("married" %in% colnames(data)) {
    data <- data %>%
      mutate(married = case_when(married == 0 ~ 0,
                                 married == 1 | married == 2 ~ 1,
                                 married == 3 | married == 4 | married == 5 ~ 2,
                                 married > 5 ~ NA_real_))
  } else {
    data <- data %>%
      mutate(married = NA)
  }
  
  if ("husband_edu" %in% colnames(data)) {
    data <- data %>%
      mutate(husband_edu = case_when(husband_edu <= 3 ~ husband_edu,
                                     husband_edu > 3 ~ NA_real_))
  } else {
    data <- data %>%
      mutate(husband_edu = NA)
  }
  
  if ("husband_lit" %in% colnames(data)) {
    data <- data %>%
      mutate(husband_lit = case_when(husband_lit >= 1 & husband_lit <= 3 ~ husband_lit,
                                     husband_lit < 1 | husband_lit > 3 ~ NA_real_))
  } else {
    data <- data %>%
      mutate(husband_lit = NA)
  }
  
  if ("husband_ocu" %in% colnames(data)) {
    data <- data %>%
      mutate(husband_ocu = case_when(husband_ocu == 0 ~ 0,
                                     husband_ocu == 1 | husband_ocu == 2 | husband_ocu == 3 ~ 1,
                                     husband_ocu == 4 | husband_ocu == 5 ~ 2,
                                     husband_ocu == 6 | husband_ocu == 7 ~ 3,
                                     husband_ocu == 8 | husband_ocu == 9 ~ 4,
                                     husband_ocu > 9 ~ NA_real_))
  } else {
    data <- data %>%
      mutate(husband_ocu = NA)
  }
  
  
  #### Reorder ####
  
  names(data)
  
  data <- data %>%
    select("phase", "year", "month", "country-phase", "dhs_country", "iso3",
           "country", "region", "subregion", "who_region_code",
           "DHSID", "cluster_unique", "cluster", "hh_unique", "hh",
           "id", "weight",
           "alive", "sex", "dob_month", "dob_year", "dob_day", "age_m", "age_y",
           "health_card", "total_kids", "birth_order",
           "urban", "city", "wealth",
           "mother_education", "mother_literacy", "married", "mother_occupation",
           "husband_edu", "husband_lit", "husband_ocu",
           all_of(c(var_vax_names_info, var_vax_names_time,
                    var_atypical_names, var_atypical_dates)))
  
  names(data)
  
  
  #### Save ####
  
  
  saveRDS(data, file = paste0("Data/Clean_vax_data_Most_recent_", steps[step], ".Rds")) 
  
  
}
