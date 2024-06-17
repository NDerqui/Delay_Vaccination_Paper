#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Timeliness of 24 childhood immunisations and evolution of vaccination delay:#
#-#-#-#-#- analysis of data from 54 low- and middle-income countries -#-#-#-#-#


# 1/ Extract data from DHS files



# SET UP ------------------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(foreign)    # read.dbf
library(haven)      # read_sav

# Folder to save data

dir.create("Data")

# Dummy data folder for the unzipped file

dir.create("Temp folder")



# DHS VAX INFO ------------------------------------------------------------


## AIM ##

# Read all the DHS datasets
# Get information on which variables are there


#### Loop ####

# Set variables' groups

antigens <- c("BCG", "DTP", "DPT", "DT", "Td", "DTaP", "Tdap",
              "Polio", "polio", "OPV", "IPV", 
              "Measles", "measles", "MCV", "MMR", 
              "HB", "HepB", "Hib", "Pneumo", "pneumo", "Rota", "rota")

# Set your folder

folder <- "Most_recent/KR"

# Get list of elements

list_dhs <- grep(list.files(paste0("C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/DHS/Data/", folder)),
                 pattern = "^desktop\\.ini$", value = TRUE, invert = TRUE)

# Get an empty database to start loop

information <- data.frame(antigen = NA, var = NA, description = NA,
                          country_phase = NA, year = NA)

# Loop

for (i in 1:length(list_dhs)) {
  
  # Get the data
  
  unzip(zipfile = paste0("C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/DHS/Data/", folder, "/", list_dhs[i]),
        files = paste0(substr(list_dhs[i], 1, 6), "FL.SAV"), exdir = "Temp folder")
  
  x <- read_sav(paste0("Temp folder/", substr(list_dhs[i], 1, 6), "FL.SAV"))
  
  # Colnames to CAP
  
  colnames(x) <- toupper(colnames(x))
  
  # Loop through every antigen

  for (j in 1:length(antigens)) {
    for (k in colnames(x)) {
      
      if (!is.null(attr(x[[k]], which = "label"))) {
        
        result <- grepl(pattern = antigens[j], x = attr(x[[k]], which = "label"),
                        fixed = TRUE)
        
      } else {
        result <- FALSE
      }
      
      if(result == TRUE) {
          information[nrow(information)+1, 1] <- antigens[j]
          information[nrow(information), 2] <- colnames(x[k])
          information[nrow(information), 3] <- attr(x[[k]], which = "label")
          information[nrow(information), 4] <- x["V000"][1,1]
          information[nrow(information), 5] <- x["V007"][1,1]
      } 
        
      
    }
  }
  
}

# All data extracted, clean environment

rm(i, j, k, result, x)
unlink("Temp folder/*")


#### Dictionary ####

# Get rid of NA

information <- information[complete.cases(information),]

# Cleaning

information <- information %>%
  # Signal the "typical" DHS vars
  mutate(typical = case_when(
    var %in% c("H2", "H2Y", "H2M", "H2D",
               "H0", "H0D", "H0M", "H0Y",
               "H3", "H3Y", "H3M", "H3D",
               "H4", "H4Y", "H4M", "H4D",
               "H5", "H5Y", "H5M", "H5D",
               "H6", "H6Y", "H6M", "H6D",
               "H7", "H7Y", "H7M", "H7D",
               "H8", "H8Y", "H8M", "H8D",
               "H9", "H9Y", "H9M", "H9D",
               "H9A", "H9AY", "H9AM", "H9AD",
               "H50", "H50Y", "H50M", "H50D",
               "H60", "H60Y", "H60M", "H60D",
               "H61", "H61Y", "H61M", "H61D",
               "H62", "H62Y", "H62M", "H62D",
               "H63", "H63Y", "H63M", "H63D",
               "H54", "H54Y", "H54M", "H54D",
               "H55", "H55Y", "H55M", "H55D",
               "H56", "H56Y", "H56M", "H56D",
               "H57", "H57Y", "H57M", "H57D",
               "H58", "H58Y", "H58M", "H58D",
               "H59", "H59Y", "H59M", "H59D",
               "H64", "H64Y", "H64M", "H64D",
               "H65", "H65Y", "H65M", "H65D",
               "H66", "H66Y", "H66M", "H66D",
               "H51", "H51Y", "H51M", "H51D",
               "H52", "H52Y", "H52M", "H52D",
               "H53", "H53Y", "H53M", "H53D") ~ 1)) %>%
  mutate(typical = case_when(typical == 1 ~ 1,
                             is.na(typical) ~ 0)) %>%
  select(typical, antigen, var, description, country_phase, year) %>%
  # Reduce the options of antigens
  mutate(antigen = case_when(antigen == "BCG" ~ "BCG",
                             antigen == "DTP" | antigen == "DPT" | antigen == "DT" | antigen == "Td" | antigen == "Tdap" | antigen == "DTaP" ~ "DTP",
                             antigen == "HB" | antigen == "HepB" ~ "HepB",
                             antigen == "Hib" ~ "Hib", 
                             antigen == "Measles" | antigen == "measles" | antigen == "MCV" ~ "MCV",
                             antigen == "MMR" ~ "MMR",
                             antigen == "Polio" | antigen == "polio" | antigen == "OPV" ~ "OPV",
                             antigen == "IPV" ~ "IPV",
                             antigen == "Pneumo" | antigen == "pneumo" ~ "Pneumo",
                             antigen == "Rota" | antigen == "rota" ~ "Rota")) %>%
  arrange(typical, antigen, var)

# Unique values

information <- information %>%
  group_by(var) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Extracting information from the description

information <- information %>%
  mutate(dose_0 = case_when(grepl(" 0", description, fixed = TRUE) == TRUE ~ TRUE,
                            grepl("-0", description, fixed = TRUE) == TRUE ~ TRUE,
                            grepl("0 ", description, fixed = TRUE) == TRUE ~ TRUE)) %>%
  mutate(dose_1 = case_when(grepl(" 1", description, fixed = TRUE) == TRUE ~ TRUE,
                            grepl("-1", description, fixed = TRUE) == TRUE ~ TRUE,
                            grepl("1 ", description, fixed = TRUE) == TRUE ~ TRUE)) %>%
  mutate(dose_2 = case_when(grepl(" 2", description, fixed = TRUE) == TRUE ~ TRUE,
                            grepl("-2", description, fixed = TRUE) == TRUE ~ TRUE,
                            grepl("2 ", description, fixed = TRUE) == TRUE ~ TRUE)) %>%
  mutate(dose_3 = case_when(grepl(" 3", description, fixed = TRUE) == TRUE ~ TRUE,
                            grepl("-3", description, fixed = TRUE) == TRUE ~ TRUE,
                            grepl("3 ", description, fixed = TRUE) == TRUE ~ TRUE)) %>%
  mutate(dose_4 = case_when(grepl(" 4", description, fixed = TRUE) == TRUE ~ TRUE,
                            grepl("-4", description, fixed = TRUE) == TRUE ~ TRUE,
                            grepl("4 ", description, fixed = TRUE) == TRUE ~ TRUE)) %>%
  mutate(dose = case_when(dose_0 == TRUE ~ 0,
                          dose_1 == TRUE ~ 1,
                          dose_2 == TRUE ~ 2,
                          dose_3 == TRUE ~ 3,
                          dose_4 == TRUE ~ 4)) %>%
  mutate(booster = case_when(grepl("Booster", description, fixed = TRUE) == TRUE ~ 1,
                             grepl("booster", description, fixed = TRUE) == TRUE ~ 1,
                             grepl("BOOSTER", description, fixed = TRUE) == TRUE ~ TRUE)) %>%
  mutate(booster = case_when(booster == 1 ~ 1,
                             is.na(booster) ~ 0))  %>%
  mutate(date_day = case_when(grepl("day", description, fixed = TRUE) == TRUE ~ TRUE,
                              grepl("Day", description, fixed = TRUE) == TRUE ~ TRUE,
                              grepl("DAY", description, fixed = TRUE) == TRUE ~ TRUE)) %>%
  mutate(date_month = case_when(grepl("month", description, fixed = TRUE) == TRUE ~ TRUE,
                                grepl("Month", description, fixed = TRUE) == TRUE ~ TRUE,
                                grepl("MONTH", description, fixed = TRUE) == TRUE ~ TRUE)) %>%
  mutate(date_year = case_when(grepl("year", description, fixed = TRUE) == TRUE ~ TRUE,
                               grepl("Year", description, fixed = TRUE) == TRUE ~ TRUE,
                               grepl("YEAR", description, fixed = TRUE) == TRUE ~ TRUE)) %>%
  mutate(date_info = case_when(date_day == TRUE | date_month == TRUE | date_year == TRUE ~ 1,
                               is.na(date_day) & is.na(date_month) & is.na(date_year) ~ 0))

for (i in 1:length(antigens)) {
  
  result <- grepl(pattern = antigens[i], x = information$description,
                    fixed = TRUE)
  information[, paste0(antigens[i])] <- result
}

# Remove unwanted vars

unique(information$description[information$typical == 0])

omitting <- c("BCG place", "Secteur BCG", "Shoulder where BCG is placed",
              "Received BCG - Facility", "DPT 1 place", "DPT 2 place", "DPT 3 place",
              "Last Birth - Place of DPT vaccine", "Last Birth - Place of second DPT vaccine",
              "Last Birth - Place of third DPT vaccine",
              "Number of DPT vaccines", "Number of DPT booster vaccines",
              "Type of vaccination administered Combo/Penta/DPT",
              "Place received DPT 1", "Place received DPT 2", "Place received DPT 3",
              "DPT 1 day (Ethiopia calendar)", "DPT 2 day (Ethiopia calendar)", "DPT 3 day (Ethiopia calendar)",
              "DPT-HepB-Hib 1 day (Ethiopia calendar)", "DPT-HepB-Hib 2 day (Ethiopia calendar)", "DPT-HepB-Hib 3 day (Ethiopia calendar)",
              "DPT 1 month (Ethiopia calendar)", "DPT 2 month (Ethiopia calendar)", "DPT 3 month (Ethiopia calendar)",
              "DPT-HepB-Hib 1 month (Ethiopia calendar)", "DPT-HepB-Hib 2 month (Ethiopia calendar)", "DPT-HepB-Hib 3 month (Ethiopia calendar)",
              "Number of penta+DPT from mother", "Secteur DTC 1",  "Secteur DTC 2",  "Secteur DTC 3",
              "ADDTL NO OF BOYS DESIRED", "ADDTL NO OF GIRLS DESIRED",
              "Result of Thalassemia test: HB-E", "Result of Thalassemia test: HB-D",
              "Result of Thalassemia test: HB-C", "Result of Thalassemia test: HB-S",
              "Place received MMR", "Number of MMR vaccines",
              "Mother's report of Measles or MMR injection (number)",
              "Had measles", "Treated the measles", "Treated measles. - antibiotic",                                          
              "Treated measles- liquid", "Treated measles - tablet",
              "Treated measles - injection", "Treated measles - hospital,PMI",
              "Treated measles - other", "Treated measles -  Don\"t know",
              "Age had measles (months)", "Ever ill with measles",
              "Ever Have Measles", "Ever have Measles", "Measles place", "Place received Measles",
              "Age for 1st vacc measles", "Already had measles", "Age when child had measles",
              "Age when had measles", "Measles before death",
              "Had Measles last 2 weeks", "Age when had measles" ,
              "Measles day (Ethiopia calendar)", "Measles month (Ethiopia calendar)",
              "Number of measles vaccines", "2 wks Bef. Death: had measles",
              "CS Sample weight : Measles/tetanus sub-sample",
              "Last Birth - Place of first Measles vaccine", "Last Birth - Place of second Measles vaccine",
              "Had Measles last 2 weeks", "Measles before death",
              "Penta 1: día", "Penta 2: día", "Penta 3: día",
              "Penta 1: mes", "Penta 2: mes", "Penta 3: mes",
              "Penta 1: año", "Penta 2: año", "Penta 3: año",
              "Number of Tetracoq/Pentacoq doses", "How many times received Penta?",
              "Number of Pentavalent vaccines", "Pentavalent from public/private",
              "Number of penta+DPT from mother",
              "Other illnesses associated with TB: pneumonia",
              "Type of chronic illness: Broncho-pneumonia",
              "Med cause death: pneumonia", "Rec. pneumonia: wheezing",
              "Rec pneumonia: rapid breathing", "Rec. pneumonia: fever",                        
              "Rec. pneumonia:retracted chest", "Rec. pneumonia: dif. breathing",
              "Rec. pneumonia: does not eat", "Rec. pneumonia: other",
              "Rec. pneumonia: DK",
              "Type of chronic disability: Polio", "Polio reported by mother",
              "Injections for Polio", "Ever have Polio",
              "Poliomyélite", "Number of vaccinations against  Poliomyélite",
              "Polio 1 place", "Polio 2 place", "Polio 3 place",
              "Number of Polio vaccines (NID)", "Number of Polio vaccines",
              "Number of times received polio vaccine from NID",
              "Age at first polio vaccine", "Remember Bye Bye Polio Campaign",
              "Secteur Polio 1", "Secteur Polio 2", "Secteur Polio 3",   
              "Polio: numero de doses", "Polio, reforco: dia",
              "Polio, reforco: mes", "Polio, reforco: ano",
              "Number of Inactive Polio vaccines",
              "Number of times polio dose given (excluding natl immun days)",
              "Polio at birth day (Ethiopia calendar)", "Polio 1 day (Ethiopia calendar)",
              "Polio 2 day (Ethiopia calendar)", "Polio 3 day (Ethiopia calendar)",
              "Polio at birth month (Ethiopia calendar)", "Polio 1 month (Ethiopia calendar)",
              "Polio 2 month (Ethiopia calendar)", "Polio 3 month (Ethiopia calendar)",
              "Polio at birth year (Ethiopia calendar)", "Polio 1 year (Ethiopia calendar)",
              "Polio 2 year (Ethiopia calendar)", "Polio 3 year (Ethiopia calendar)",
              "Pneumonia", "Number of rotavirus vaccinations",
              "When first rotavirus was given", 
              "Rotavirus 1 day (Ethiopia calendar)", "Rotavirus 2 day (Ethiopia calendar)",
              "Rotavirus 3 day (Ethiopia calendar)" , "Rotavirus 1 month (Ethiopia calendar)",
              "Rotavirus 2 month (Ethiopia calendar)", "Rotavirus 3 month (Ethiopia calendar)",
              "Rotavirus 1 year (Ethiopia calendar)", "Rotavirus 2 year (Ethiopia calendar)",
              "Rotavirus 3 year (Ethiopia calendar)",
              "Given Ersefluril/ typhomicine",
              "Cough: advice of Tdl herbalist", "Fever: advice of Tdl herbalist",
              "Country specific other food: Yellow Fruits",
              "Place heard of AIDS: Yellow line (MTN)",
              "Country specific other food: Yellow fruits",
              "Last 7 days - Fruits/Yellow Vegetables",
              "Times gave child apricot, palm nuts, or yellow melon", "Last 7 days - apricot, palm nuts, or yellow melon",
              "Symptoms indicating that should take child to health facility for treatment : Hot skin and yellowed eyes",
              "Hanging lamp (yellow with no cover) in house", "Malaria symptoms: yellow urine / dark colored urine",
              "Given first yellow breastmilk", "Gave yellow milk to child",
              "Vitamin A: yellow fruits", "Vitamin A: yellow legumes",
              "Gave child pumpkin carrots, squash (yellow or orange inside)",
              "Baby: mango, papaya, durian, jackfruit or yellow/red fruits",
              "Mother: mango, papaya, durian, jackfruit or yellow/red fruit",
              "Gave child: yellow vegetables (carrots, pumpkin)",
              "Times last day was given: yellow vegetables (carrots, pumpkin)",
              "Gave child: yellow fruits (mango, papaya, zapote, chontaduro)",                                           
              "Times last day was given: yellow fruits (mango, papaya, zapote, chontaduro)",
              "Number of vaccinations against yellow fever",
              "Mother ate: pumpkin, carrots, squash or sweet potatoes that or yellow or orange inside",
              "Mother ate: ripe mangoes, papayas, melon or any other fruit yellow or orange inside",
              "Woman ate: Any fruits that are dark yellow or orange inside",
              "Woman ate: Any fruits that are dark yellow or orange inside",
              "Respondent ate: any fruits that are dark yellow or orange inside",
              "Drank/ate: Sweet red bell pepper, pumpkin or carrots that are yellow or orange inside",
              "Drank/ate: Ripe persimmons, or ripe fresh apricots, dried apricots or dried peaches or other fruits that are dark yellow or orange inside",
              "Symptoms: yellow eyes", "Seen/heard of yellow FP flower",
              "Gave child pumpkin, carrots, squash (yellow or orange inside)",
              "Diarrhea: Dry/yellow skin", "Times gave child apricot, palm nuts, yellow melon",
              "NA - Mother had pumpkin, carrots, squash (yellow or orange inside)",
              "Heard or seen malaria messages in the last 6 months about intra-domiciliary DDT spraying campaign",
              "Amoxicillin DT tablets", "Ever had German measles vaccin",
              "Gave the first yellow milk", "Reason of not given the first yellow milk",
              "have you heard of human papillomavirus (HPV)?")

information <- information %>% filter(!(description %in% omitting))


#### Save ####

write.csv(information, file = "Data/All_Vax_available_Most_recent.csv", row.names = FALSE)



# DHS VAX DATA ----------------------------------------------------------------


## AIM ##

# Read all the DHS databases
# Basic function: data <- read_table("AOIR71FL.DAT", col_names = FALSE)
# But we want a nice loop to read all the elements

# In this case, different phases have diff number of vars - problem
# Thus, extract only specific variables (vaccine related)
# Use "var" disctionary to do this


## COMMENTS ##

# Decoding some vars (if no detail, decoding is obvious)

# V102: 1 - urban // 2 - rural
# V103: 1 - city // 2 - town // 3 - countryside
# MV190: 1 - poorest // 2 - poorer // 3 - middle
#        4 - richer // 5 - richest

# V106: 0 - none // 1 - primary // 2 - secondary // 3 - higher
# V108: 1 - read good // 2 - read difficult // 3 - cannot read
# V501: 0 - not married // 1 - married // 2 - living together
#       3 - widowed // 4 - divorced // 5 - not living together
# MV717: 0 - No work // 1 - Prof/Manager // 2 - Clerical // 3 - Sales
#        4 - Agri Self Employed // 5 - Agri Employed // 6 - Household
#        7 - Services // 8 - Skilled manual // 9 - Unskilled manual

# V701: same as V106
# V703: same as V108
# V705: same as MV717

# B4: 1 - male // 2 - female
# H1: 0 - No health card // 1 - Has health card, seen // 2 - Has, not seen
# All vaccinations: 0 - No // 1 - Yes // 2 - Reported by mother


#### Loop All ####

# Set variables' groups

var_basic <- c("CASEID", "V000", "V001", "V002", "V005", "V006", "V007")
var_basic_names <- c("id", "country-phase", "cluster", "hh", "weight", "month", "year")

var_hh <- c("V102", "V103", "V190",
            "V106", "V108", "V501", "MV717",
            "V701", "V703", "V705",
            "V201")
var_hh_names <- c("urban", "city", "wealth",
                  "mother_education", "mother_literacy", "married", "mother_occupation",
                  "husband_edu", "husband_lit", "husband_ocu",
                  "total_kids")

var_demo <- c("B1", "B2", "B4", "B5",
              "B8", "B19", "H1", "BORD")
var_demo_names <- c("dob_month", "dob_year", "sex", "alive",
                    "age_y", "age_m", "health_card", "birth_order")

var_vax <- c("H2", "H2Y", "H2M", "H2D",
             "H0", "H0Y", "H0M", "H0D",
             "H3", "H3Y", "H3M", "H3D",
             "H4", "H4Y", "H4M", "H4D",
             "H5", "H5Y", "H5M", "H5D",
             "H6", "H6Y", "H6M", "H6D",
             "H7", "H7Y", "H7M", "H7D",
             "H8", "H8Y", "H8M", "H8D",
             "H9", "H9Y", "H9M", "H9D",
             "H9A", "H9AY", "H9AM", "H9AD",
             "H60", "H60Y", "H60M", "H60D",
             "H50", "H50Y", "H50M", "H50D",
             "H61", "H61Y", "H61M", "H61D",
             "H62", "H62Y", "H62M", "H62D",
             "H63", "H63Y", "H63M", "H63D",
             "H54", "H54Y", "H54M", "H54D",
             "H55", "H55Y", "H55M", "H55D",
             "H56", "H56Y", "H56M", "H56D",
             "H57", "H57Y", "H57M", "H57D",
             "H58", "H58Y", "H58M", "H58D",
             "H59", "H59Y", "H59M", "H59D",
             "H64", "H64Y", "H64M", "H64D",
             "H65", "H65Y", "H65M", "H65D",
             "H66", "H66Y", "H66M", "H66D",
             "H51", "H51Y", "H51M", "H51D",
             "H52", "H52Y", "H52M", "H52D",
             "H53", "H53Y", "H53M", "H53D")
var_vax_names <- c("BCG", "BCG_y", "BCG_mo", "BCG_d",
                   "OPV0", "OPV0_y", "OPV0_m", "OPV0_d",
                   "DTP1", "DTP1_y", "DTP1_mo", "DTP1_d",
                   "OPV1", "OPV1_y", "OPV1_mo", "OPV1_d",
                   "DTP2", "DTP2_y", "DTP2_mo", "DTP2_d",
                   "OPV2", "OPV2_y", "OPV2_mo", "OPV2_d",
                   "DTP3", "DTP3_y", "DTP3_mo", "DTP3_d",
                   "OPV3", "OPV3_y", "OPV3_mo", "OPV3_d",
                   "MCV1", "MCV1_y", "MCV1_mo", "MCV1_d",
                   "MCV2", "MCV2_y", "MCV2_mo", "MCV2_d",
                   "IPV", "IPV_y", "IPV_mo", "IPV_d",
                   "HepB0", "HepB0_y", "HepB0_mo", "HepB0_d",
                   "HepB1", "HepB1_y", "HepB1_mo", "HepB1_d",
                   "HepB2", "HepB2_y", "HepB2_mo", "HepB2_d",
                   "HepB3", "HepB3_y", "HepB3_mo", "HepB3_d",
                   "Pneumo1", "Pneumo1_y", "Pneumo1_mo", "Pneumo1_d",
                   "Pneumo2", "Pneumo2_y", "Pneumo2_mo", "Pneumo2_d",
                   "Pneumo3", "Pneumo3_y", "Pneumo3_mo", "Pneumo3_d",
                   "Rota1", "Rota1_y", "Rota1_mo", "Rota1_d",
                   "Rota2", "Rota2_y", "Rota2_mo", "Rota2_d",
                   "Rota3", "Rota3_y", "Rota3_mo", "Rota3_d",
                   "Hib1", "Hib1_y", "Hib1_mo", "Hib1_d",
                   "Hib2", "Hib2_y", "Hib2_mo", "Hib2_d",
                   "Hib3", "Hib3_y", "Hib3_mo", "Hib3_d",
                   "Penta1", "Penta1_y", "Penta1_mo", "Penta1_d",
                   "Penta2", "Penta2_y", "Penta2_mo", "Penta2_d",
                   "Penta3", "Penta3_y", "Penta3_mo", "Penta3_d")

var_atypical <- unique(information$var[information$typical == 0])

# Set your folder

folder <- "Most_recent/KR"

# Get list of elements

list_dhs <- grep(list.files(paste0("C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/DHS/Data/", folder)),
                 pattern = "^desktop\\.ini$", value = TRUE, invert = TRUE)

# Separate the data extraction into 2 steps

steps <- c(1:2)
steps_starters <- c(1, 51)
steps_ends <- c(50, length(list_dhs))

# Loop for every step

for (step in 1:2) {
  
  # Get an empty database to start loop
  
  data_vax <- data.frame()
  
  # Create a loop
  
  for (i in steps_starters[step]:steps_ends[step]) {
    
    unzip(zipfile = paste0("C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/DHS/Data/", folder, "/", list_dhs[i]),
          files = paste0(substr(list_dhs[i], 1, 6), "FL.SAV"), exdir = "Temp folder")
    
    x <- read_sav(paste0("Temp folder/", substr(list_dhs[i], 1, 6), "FL.SAV"))
    
    x <- as.data.frame(as.matrix(x))
    
    # Covert variables all to upper case
    
    colnames(x) <- toupper(colnames(x))
    
    # All vax data
    
    add <- x %>%
      select(any_of(c(var_basic, var_hh, var_demo, var_vax, var_atypical)))
    
    # All together
    
    data_vax <- merge(data_vax, add, all = TRUE)
    
  }  
  
  # All data extracted, clean environment
  
  rm(i, x, add)
  unlink("Temp folder/*")
  
  
  #### Save ####
  
  saveRDS(data_vax, file = paste0("Data/All_Raw_vax_data_Most_recent_", steps[step], ".Rds"))
  
}
