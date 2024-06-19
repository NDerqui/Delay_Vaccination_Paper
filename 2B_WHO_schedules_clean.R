#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Timeliness of 24 childhood immunisations and evolution of vaccination delay:#
#-#-#-#-#- analysis of data from 54 low- and middle-income countries -#-#-#-#-#


# 2/ Clean raw data: National vaccination age recommendations (WHO source)



# SET UP ------------------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(readxl)



# WHO data -----------------------------------------------------------------


# Getting the schedule recommended vaccination age
# Data source: WHO / UNICEF immunization portal


#### BCG ####

data <- read_excel("C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Schedule/Vaccination schedule for Tuberculosis.xlsx")

# Basic clean: variables and scope of interest

bcg <- data %>%
  # Getting the Routine Immunization at National level ONLY
  filter(TARGETPOP_DESCRIPTION == "General/routine") %>%
  filter(GEOAREA == "NATIONAL") %>%
  # Getting data for first dose
  filter(SCHEDULEROUNDS <= 1) %>%
  # Cleaning
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, VACCINECODE, SCHEDULEROUNDS, AGEADMINISTERED)
  
# Code the recommended age in weeks, months and years

bcg <- bcg %>%
  # Age: birth
  mutate(age = case_when(substr(AGEADMINISTERED, 1, 1) == "B" ~ "Birth",
                         substr(AGEADMINISTERED, 1, 1) == "D" ~ "Few Days",
                         substr(AGEADMINISTERED, 1, 1) == "W" | substr(AGEADMINISTERED, 1, 1) == "M" |
                         substr(AGEADMINISTERED, 1, 1) == "Y" | substr(AGEADMINISTERED, 1, 1) == "<" ~ "W1 - Y1")) %>%
  # Age: weeks
  mutate(weeks = case_when(substr(AGEADMINISTERED, 1, 1) == "W" ~ AGEADMINISTERED)) %>%
  mutate(weeks = substr(weeks, 2, 3)) %>%
  mutate(weeks = case_when(substr(weeks, 2, 2) == "." | substr(weeks, 2, 2) == "-" ~ substr(weeks, 1, 1),
                           substr(weeks, 2, 2) != "." | substr(weeks, 2, 2) != "-" ~ weeks)) %>%
  mutate(weeks = as.numeric(weeks)) %>%
  # Age: months
  mutate(months = case_when(substr(AGEADMINISTERED, 1, 1) == "M" ~ AGEADMINISTERED)) %>%
  mutate(months = substr(months, 2, 3)) %>%
  mutate(months = case_when(substr(months, 2, 2) == "." | substr(months, 2, 2) == "-" ~ substr(months, 1, 1),
                            substr(months, 2, 2) != "." | substr(months, 2, 2) != "-" ~ months)) %>%
  mutate(months = as.numeric(months)) %>%
  # Age: years
  mutate(years = case_when(substr(AGEADMINISTERED, 1, 1) == "Y" ~ AGEADMINISTERED,
                           substr(AGEADMINISTERED, 1, 2) == "<Y" ~ AGEADMINISTERED)) %>%
  mutate(years = substr(years, 2, 3)) %>%
  mutate(years = case_when(substr(years, 2, 2) == "." | substr(years, 2, 2) == "-" ~ substr(years, 1, 1),
                           substr(years, 1, 1) == "Y" ~ substr(years, 2, 2),
                           substr(years, 2, 2) != "." | substr(years, 2, 2) != "-" ~ years)) %>%
  mutate(years = as.numeric(years))

# Code the final recommended age of vaccination and clean the dataset

bcg <- bcg %>%
  # Age: all in months
  mutate(age_mo = case_when(age == "Birth" | age == "Few Days" ~ 0,
                            !is.na(weeks) ~ weeks/4.3,
                            !is.na(years) ~ years*12,
                            !is.na(months) ~ months)) %>%
  mutate(age_mo = round(age_mo, digits = 1)) %>%
  # Age: all in weeks
  mutate(age_we = case_when(age == "Birth" | age == "Few Days" ~ 0,
                            !is.na(weeks) ~ weeks,
                            !is.na(years) ~ years*12*4.3,
                            !is.na(months) ~ months*4.3)) %>%
  mutate(age_we = round(age_we)) %>%
  # Filter to only childhood immunization (max 4 year)
  filter(age_we <= 192) %>%
  # Clean
  mutate(vaccine = "BCG") %>%
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, AGEADMINISTERED, vaccine, age_mo, age_we) 


#### DTP ####

data <- read_excel("C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Schedule/Vaccination schedule for Diphtheria.xlsx")

# Basic clean: variables and scope of interest

dtp <- data %>%
  # Getting the Routine Immunization at National level ONLY
  filter(TARGETPOP_DESCRIPTION == "General/routine") %>%
  filter(GEOAREA == "NATIONAL") %>%
  # Getting data for first four doses
  filter(SCHEDULEROUNDS <= 4) %>%
  # Cleaning
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, VACCINECODE, SCHEDULEROUNDS, AGEADMINISTERED) %>%
  # DTP special: Get only the complete DTaP or DTwP
  filter(substr(VACCINECODE, 1, 4) == "DTAP" | substr(VACCINECODE, 1, 4) == "DTWP" | substr(VACCINECODE, 1, 4) == "TDAP")
  
# Code the recommended age in weeks, months and years
  
dtp <- dtp %>%
  # Age: weeks
  mutate(weeks = case_when(substr(AGEADMINISTERED, 1, 1) == "W" ~ AGEADMINISTERED)) %>%
  mutate(weeks = substr(weeks, 2, 3)) %>%
  mutate(weeks = case_when(substr(weeks, 2, 2) == "." | substr(weeks, 2, 2) == "-" ~ substr(weeks, 1, 1),
                           substr(weeks, 2, 2) != "." | substr(weeks, 2, 2) != "-" ~ weeks)) %>%
  mutate(weeks = as.numeric(weeks)) %>%
  # Age: months
  mutate(months = case_when(substr(AGEADMINISTERED, 1, 1) == "M" ~ AGEADMINISTERED)) %>%
  mutate(months = substr(months, 2, 3)) %>%
  mutate(months = case_when(substr(months, 2, 2) == "." | substr(months, 2, 2) == "-" ~ substr(months, 1, 1),
                            substr(months, 2, 2) != "." | substr(months, 2, 2) != "-" ~ months)) %>%
  mutate(months = as.numeric(months)) %>%
  # Age: years
  mutate(years = case_when(substr(AGEADMINISTERED, 1, 1) == "Y" ~ AGEADMINISTERED)) %>%
  mutate(years = substr(years, 2, 3)) %>%
  mutate(years = case_when(substr(years, 2, 2) == "." | substr(years, 2, 2) == "-" ~ substr(years, 1, 1),
                           substr(years, 2, 2) != "." | substr(years, 2, 2) != "-" ~ years)) %>%
  mutate(years = as.numeric(years))

# Code the final recommended age of vaccination and clean the dataset
  
dtp <- dtp %>%
  # Age: all in months
  mutate(age_mo = case_when(!is.na(weeks) ~ weeks/4.3,
                            !is.na(years) ~ years*12,
                            !is.na(months) ~ months)) %>%
  mutate(age_mo = round(age_mo, digits = 1)) %>%
  # Age: all in weeks
  mutate(age_we = case_when(!is.na(weeks) ~ weeks,
                            !is.na(years) ~ years*12*4.3,
                            !is.na(months) ~ months*4.3)) %>%
  mutate(age_we = round(age_we)) %>%
  # Filter to only childhood immunization (max 4 year)
  filter(age_we <= 192) %>%
  # Clean
  mutate(vaccine = paste0("DTP-D", SCHEDULEROUNDS)) %>%
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, AGEADMINISTERED, vaccine, age_mo, age_we) 


#### POL ####

data <- read_excel("C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Schedule/Vaccination schedule for Poliomyelitis.xlsx")

# Basic clean: variables and scope of interest

pol <- data %>%
  # Getting the Routine Immunization at National level ONLY
  filter(TARGETPOP_DESCRIPTION == "General/routine") %>%
  filter(GEOAREA == "NATIONAL") %>%
  # Getting data for first couple of doses
  filter(SCHEDULEROUNDS <= 4) %>%
  # Polio special: IPV or OPV
  mutate(VACCINECODE = case_when(SCHEDULERCODE == "OPV" ~ "OPV",
                                 SCHEDULERCODE == "IPV" | SCHEDULERCODE == "IPVf" ~ "IPV")) %>%
  # Cleaning
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, VACCINECODE, SCHEDULEROUNDS, AGEADMINISTERED)

# Making the dose at birth a dose zero without erasing other dose 1 data! (IMP: ONLY FOR OPV)

pol <- pol %>%
  mutate(number_1 = case_when(SCHEDULEROUNDS == 1 ~ 1)) %>%
  ## Per country: know the number doses1 and whether there is a BirthDose
  group_by(COUNTRYNAME, VACCINECODE) %>%
  # Find out number of doses1 available (sometimes, one is BD, one is dose1; sometimes the one dose1 is BD)
  mutate(number_1 = sum(number_1, na.rm = TRUE)) %>%
  # Find out whether there is a BD in the group
  mutate(birth_yes = case_when(substr(AGEADMINISTERED, 1, 1) == "B" ~ 1)) %>%
  mutate(birth_yes = max(birth_yes, na.rm = TRUE)) %>%
  ungroup() %>%
  ## Change the BD to schedule round 0
  mutate(SCHEDULEROUNDS = case_when(substr(AGEADMINISTERED, 1, 1) == "B" ~ 0,
                                    substr(AGEADMINISTERED, 1, 1) != "B" ~ SCHEDULEROUNDS)) %>%
  # Find out if we need to demote one the SCHEDULE rounds...
  mutate(demote = case_when(birth_yes == 1 & number_1 == 1 ~ 1, 
                            birth_yes == "-Inf" | number_1 >= 2 ~ 0)) %>%
  # Demote the schedule rounds if Info for BD, but only one dose1 (obv keep BD as 0)
  mutate(SCHEDULEROUNDS = case_when(demote == 0 | SCHEDULEROUNDS == 0 ~ SCHEDULEROUNDS,
                                    demote == 1 & SCHEDULEROUNDS > 0 ~ (SCHEDULEROUNDS - 1)))

# Code the recommended age in weeks, months and years

pol <- pol %>%
  # Age: birth
  mutate(age = case_when(substr(AGEADMINISTERED, 1, 1) == "B" ~ "Birth",
                         substr(AGEADMINISTERED, 1, 1) == "D" ~ "Few Days",
                         substr(AGEADMINISTERED, 1, 1) == "W" | substr(AGEADMINISTERED, 1, 1) == "M" |
                           substr(AGEADMINISTERED, 1, 1) == "Y" | substr(AGEADMINISTERED, 1, 1) == "<" ~ "W1 - Y1")) %>%
  # Age: weeks
  mutate(weeks = case_when(substr(AGEADMINISTERED, 1, 1) == "W" ~ AGEADMINISTERED)) %>%
  mutate(weeks = substr(weeks, 2, 3)) %>%
  mutate(weeks = case_when(substr(weeks, 2, 2) == "." | substr(weeks, 2, 2) == "-" ~ substr(weeks, 1, 1),
                           substr(weeks, 2, 2) != "." | substr(weeks, 2, 2) != "-" ~ weeks)) %>%
  mutate(weeks = as.numeric(weeks)) %>%
  # Age: months
  mutate(months = case_when(substr(AGEADMINISTERED, 1, 1) == "M" ~ AGEADMINISTERED)) %>%
  mutate(months = substr(months, 2, 3)) %>%
  mutate(months = case_when(substr(months, 2, 2) == "." | substr(months, 2, 2) == "-" ~ substr(months, 1, 1),
                            substr(months, 2, 2) != "." | substr(months, 2, 2) != "-" ~ months)) %>%
  mutate(months = as.numeric(months)) %>%
  # Age: years
  mutate(years = case_when(substr(AGEADMINISTERED, 1, 1) == "Y" ~ AGEADMINISTERED,
                           substr(AGEADMINISTERED, 1, 2) == "<Y" ~ AGEADMINISTERED)) %>%
  mutate(years = substr(years, 2, 3)) %>%
  mutate(years = case_when(substr(years, 2, 2) == "." | substr(years, 2, 2) == "-" ~ substr(years, 1, 1),
                           substr(years, 1, 1) == "Y" ~ substr(years, 2, 2),
                           substr(years, 2, 2) != "." | substr(years, 2, 2) != "-" ~ years)) %>%
  mutate(years = as.numeric(years))

# Code the final recommended age of vaccination and clean the dataset

pol <- pol %>%
  # Age: all in months
  mutate(age_mo = case_when(age == "Birth" | age == "Few Days" ~ 0,
                            !is.na(weeks) ~ weeks/4.3,
                            !is.na(years) ~ years*12,
                            !is.na(months) ~ months)) %>%
  mutate(age_mo = round(age_mo, digits = 1)) %>%
  # Age: all in weeks
  mutate(age_we = case_when(age == "Birth" | age == "Few Days" ~ 0,
                            !is.na(weeks) ~ weeks,
                            !is.na(years) ~ years*12*4.3,
                            !is.na(months) ~ months*4.3)) %>%
  # Filter to only childhood immunization (max 4 year)
  filter(age_we <= 192) %>%
  # Clean
  mutate(vaccine = paste0(VACCINECODE, "-D", SCHEDULEROUNDS)) %>%
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, AGEADMINISTERED, vaccine, age_mo, age_we) 


#### MCV ####

data <- read_excel("C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Schedule/Vaccination schedule for Measles.xlsx")

# Basic clean: variables and scope of interest

mcv <- data %>%
  # Getting the Routine Immunization at National level ONLY
  filter(TARGETPOP_DESCRIPTION == "General/routine") %>%
  filter(GEOAREA == "NATIONAL") %>%
  # Getting data for first two doses
  filter(SCHEDULEROUNDS <= 2) %>%
  # Cleaning
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, VACCINECODE, SCHEDULEROUNDS, AGEADMINISTERED)

# Code the recommended age in weeks, months and years

mcv <- mcv %>%
  # Age: weeks
  mutate(weeks = case_when(substr(AGEADMINISTERED, 1, 1) == "W" ~ AGEADMINISTERED)) %>%
  mutate(weeks = substr(weeks, 2, 3)) %>%
  mutate(weeks = case_when(substr(weeks, 2, 2) == "." | substr(weeks, 2, 2) == "-" ~ substr(weeks, 1, 1),
                           substr(weeks, 2, 2) != "." | substr(weeks, 2, 2) != "-" ~ weeks)) %>%
  mutate(weeks = as.numeric(weeks)) %>%
  # Age: months
  mutate(months = case_when(substr(AGEADMINISTERED, 1, 1) == "M" ~ AGEADMINISTERED)) %>%
  mutate(months = substr(months, 2, 3)) %>%
  mutate(months = case_when(substr(months, 2, 2) == "." | substr(months, 2, 2) == "-" ~ substr(months, 1, 1),
                            substr(months, 2, 2) != "." | substr(months, 2, 2) != "-" ~ months)) %>%
  mutate(months = as.numeric(months)) %>%
  # Age: years
  mutate(years = case_when(substr(AGEADMINISTERED, 1, 1) == "Y" ~ AGEADMINISTERED)) %>%
  mutate(years = substr(years, 2, 3)) %>%
  mutate(years = case_when(substr(years, 2, 2) == "." | substr(years, 2, 2) == "-" ~ substr(years, 1, 1),
                           substr(years, 2, 2) != "." | substr(years, 2, 2) != "-" ~ years)) %>%
  mutate(years = as.numeric(years))

# Code the final recommended age of vaccination and clean the dataset

mcv <- mcv %>%
  # Age: all in months
  mutate(age_mo = case_when(!is.na(weeks) ~ weeks/4.3,
                            !is.na(years) ~ years*12,
                            !is.na(months) ~ months)) %>%
  mutate(age_mo = round(age_mo, digits = 1)) %>%
  # Age: all in weeks
  mutate(age_we = case_when(!is.na(weeks) ~ weeks,
                            !is.na(years) ~ years*12*4.3,
                            !is.na(months) ~ months*4.3)) %>%
  mutate(age_we = round(age_we)) %>%
  # Filter to only childhood immunization (max 4 year)
  filter(age_we <= 192) %>%
  # Clean
  mutate(vaccine = paste0("MCV-D", SCHEDULEROUNDS)) %>%
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, AGEADMINISTERED, vaccine, age_mo, age_we) 


#### Hep B ####

data <- read_excel("C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Schedule/Vaccination schedule for Hepatitis B.xlsx")

# Basic clean: variables and scope of interest

hep <- data %>%
  # Getting the Routine Immunization at National level ONLY
  filter(TARGETPOP_DESCRIPTION == "General/routine") %>%
  filter(GEOAREA == "NATIONAL") %>%
  # Getting data for first four doses
  filter(SCHEDULEROUNDS <= 4) %>%
  # Cleaning
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, VACCINECODE, SCHEDULEROUNDS, AGEADMINISTERED)
  
# Making the dose at birth a dose zero without erasing other dose 1 data!

hep <- hep %>%
  mutate(number_1 = case_when(SCHEDULEROUNDS == 1 ~ 1)) %>%
  ## Per country: know the number doses1 and whther there is a BirthDose
  group_by(COUNTRYNAME) %>%
  # Find out number of doses1 available (sometimes, one is BD, one is dose1; sometimes the one dose1 is BD)
  mutate(number_1 = sum(number_1, na.rm = TRUE)) %>%
  # Find out whether there is a BD in the group
  mutate(birth_yes = case_when(substr(AGEADMINISTERED, 1, 1) == "B" ~ 1)) %>%
  mutate(birth_yes = max(birth_yes, na.rm = TRUE)) %>%
  ungroup() %>%
  ## Change the BD to schedule round 0
  mutate(SCHEDULEROUNDS = case_when(substr(AGEADMINISTERED, 1, 1) == "B" ~ 0,
                                    substr(AGEADMINISTERED, 1, 1) != "B" ~ SCHEDULEROUNDS)) %>%
  # Find out if we need to demote one the SCHEDULE rounds...
  mutate(demote = case_when(birth_yes == 1 & number_1 == 1 ~ 1, 
                            birth_yes == "-Inf" | number_1 >= 2 ~ 0)) %>%
  # Demote the schedule rounds if Info for BD, but only one dose1 (obv keep BD as 0)
  mutate(SCHEDULEROUNDS = case_when(demote == 0 | SCHEDULEROUNDS == 0 ~ SCHEDULEROUNDS,
                                    demote == 1 & SCHEDULEROUNDS > 0 ~ (SCHEDULEROUNDS - 1)))

# Code the recommended age in weeks, months and years

hep <- hep %>%
  # Age: birth
  mutate(age = case_when(substr(AGEADMINISTERED, 1, 1) == "B" ~ "Birth",
                         substr(AGEADMINISTERED, 1, 1) == "D" ~ "Few Days",
                         substr(AGEADMINISTERED, 1, 1) == "W" | substr(AGEADMINISTERED, 1, 1) == "M" |
                         substr(AGEADMINISTERED, 1, 1) == "Y" | substr(AGEADMINISTERED, 1, 1) == "<" ~ "W1 - Y1")) %>%
  # Age: weeks
  mutate(weeks = case_when(substr(AGEADMINISTERED, 1, 1) == "W" ~ AGEADMINISTERED)) %>%
  mutate(weeks = substr(weeks, 2, 3)) %>%
  mutate(weeks = case_when(substr(weeks, 2, 2) == "." | substr(weeks, 2, 2) == "-" ~ substr(weeks, 1, 1),
                           substr(weeks, 2, 2) != "." | substr(weeks, 2, 2) != "-" ~ weeks)) %>%
  mutate(weeks = as.numeric(weeks)) %>%
  # Age: months
  mutate(months = case_when(substr(AGEADMINISTERED, 1, 1) == "M" ~ AGEADMINISTERED)) %>%
  mutate(months = substr(months, 2, 3)) %>%
  mutate(months = case_when(substr(months, 2, 2) == "." | substr(months, 2, 2) == "-" ~ substr(months, 1, 1),
                            substr(months, 2, 2) != "." | substr(months, 2, 2) != "-" ~ months)) %>%
  mutate(months = as.numeric(months)) %>%
  # Age: years
  mutate(years = case_when(substr(AGEADMINISTERED, 1, 1) == "Y" ~ AGEADMINISTERED)) %>%
  mutate(years = substr(years, 2, 3)) %>%
  mutate(years = case_when(substr(years, 2, 2) == "." | substr(years, 2, 2) == "-" ~ substr(years, 1, 1),
                           substr(years, 2, 2) != "." | substr(years, 2, 2) != "-" ~ years)) %>%
  mutate(years = as.numeric(years))

# Code the final recommended age of vaccination and clean the dataset

hep <- hep %>%
  # Age: all in months
  mutate(age_mo = case_when(age == "Birth" | age == "Few Days" ~ 0,
                            !is.na(weeks) ~ weeks/4.3,
                            !is.na(years) ~ years*12,
                            !is.na(months) ~ months)) %>%
  mutate(age_mo = round(age_mo, digits = 1)) %>%
  # Age: all in weeks
  mutate(age_we = case_when(age == "Birth" | age == "Few Days" ~ 0,
                            !is.na(weeks) ~ weeks,
                            !is.na(years) ~ years*12*4.3,
                            !is.na(months) ~ months*4.3)) %>%
  # Filter to only childhood immunization (max 4 year)
  filter(age_we <= 192) %>%
  # Clean
  mutate(vaccine = paste0("HepB-D", SCHEDULEROUNDS)) %>%
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, AGEADMINISTERED, vaccine, age_mo, age_we)


#### Hib ####

data <- read_excel("C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Schedule/Vaccination schedule for Haemophilus influenzae.xlsx")

# Basic clean: variables and scope of interest

hib <- data %>%
  # Getting the Routine Immunization at National level ONLY
  filter(TARGETPOP_DESCRIPTION == "General/routine") %>%
  filter(GEOAREA == "NATIONAL") %>%
  # Getting data for first three doses
  filter(SCHEDULEROUNDS <= 3) %>%
  # Cleaning
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, VACCINECODE, SCHEDULEROUNDS, AGEADMINISTERED)

# Code the recommended age in weeks, months and years

hib <- hib %>%
  # Age: weeks
  mutate(weeks = case_when(substr(AGEADMINISTERED, 1, 1) == "W" ~ AGEADMINISTERED)) %>%
  mutate(weeks = substr(weeks, 2, 3)) %>%
  mutate(weeks = case_when(substr(weeks, 2, 2) == "." | substr(weeks, 2, 2) == "-" ~ substr(weeks, 1, 1),
                           substr(weeks, 2, 2) != "." | substr(weeks, 2, 2) != "-" ~ weeks)) %>%
  mutate(weeks = as.numeric(weeks)) %>%
  # Age: months
  mutate(months = case_when(substr(AGEADMINISTERED, 1, 1) == "M" ~ AGEADMINISTERED)) %>%
  mutate(months = substr(months, 2, 3)) %>%
  mutate(months = case_when(substr(months, 2, 2) == "." | substr(months, 2, 2) == "-" ~ substr(months, 1, 1),
                            substr(months, 2, 2) != "." | substr(months, 2, 2) != "-" ~ months)) %>%
  mutate(months = as.numeric(months)) %>%
  # Age: years
  mutate(years = case_when(substr(AGEADMINISTERED, 1, 1) == "Y" ~ AGEADMINISTERED)) %>%
  mutate(years = substr(years, 2, 3)) %>%
  mutate(years = case_when(substr(years, 2, 2) == "." | substr(years, 2, 2) == "-" ~ substr(years, 1, 1),
                           substr(years, 2, 2) != "." | substr(years, 2, 2) != "-" ~ years)) %>%
  mutate(years = as.numeric(years))

# Code the final recommended age of vaccination and clean the dataset

hib <- hib %>%
  # Age: all in months
  mutate(age_mo = case_when(!is.na(weeks) ~ weeks/4.3,
                            !is.na(years) ~ years*12,
                            !is.na(months) ~ months)) %>%
  mutate(age_mo = round(age_mo, digits = 1)) %>%
  # Age: all in weeks
  mutate(age_we = case_when(!is.na(weeks) ~ weeks,
                            !is.na(years) ~ years*12*4.3,
                            !is.na(months) ~ months*4.3)) %>%
  # Filter to only childhood immunization (max 4 year)
  filter(age_we <= 192) %>%
  # Clean
  mutate(vaccine = paste0("Hib-D", SCHEDULEROUNDS)) %>%
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, AGEADMINISTERED, vaccine, age_mo, age_we)


#### Pneumo ####

data <- read_excel("C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Schedule/Vaccination schedule for Pneumococcal disease.xlsx")

# Basic clean: variables and scope of interest

pne <- data %>%
  # Getting the Routine Immunization at National level ONLY
  filter(TARGETPOP_DESCRIPTION == "General/routine") %>%
  filter(GEOAREA == "NATIONAL") %>%
  # Getting data for first four doses
  filter(SCHEDULEROUNDS <= 4) %>%
  # Cleaning
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, VACCINECODE, SCHEDULEROUNDS, AGEADMINISTERED)

# Code the recommended age in weeks, months and years

pne <- pne %>%
  # Age: weeks
  mutate(weeks = case_when(substr(AGEADMINISTERED, 1, 1) == "W" ~ AGEADMINISTERED)) %>%
  mutate(weeks = substr(weeks, 2, 3)) %>%
  mutate(weeks = case_when(substr(weeks, 2, 2) == "." | substr(weeks, 2, 2) == "-" ~ substr(weeks, 1, 1),
                           substr(weeks, 2, 2) != "." | substr(weeks, 2, 2) != "-" ~ weeks)) %>%
  mutate(weeks = as.numeric(weeks)) %>%
  # Age: months
  mutate(months = case_when(substr(AGEADMINISTERED, 1, 1) == "M" ~ AGEADMINISTERED)) %>%
  mutate(months = substr(months, 2, 3)) %>%
  mutate(months = case_when(substr(months, 2, 2) == "." | substr(months, 2, 2) == "-" ~ substr(months, 1, 1),
                            substr(months, 2, 2) != "." | substr(months, 2, 2) != "-" ~ months)) %>%
  mutate(months = as.numeric(months)) %>%
  # Age: years
  mutate(years = case_when(substr(AGEADMINISTERED, 1, 1) == "Y" ~ AGEADMINISTERED)) %>%
  mutate(years = substr(years, 2, 3)) %>%
  mutate(years = case_when(substr(years, 2, 2) == "." | substr(years, 2, 2) == "-" ~ substr(years, 1, 1),
                           substr(years, 2, 2) != "." | substr(years, 2, 2) != "-" ~ years)) %>%
  mutate(years = as.numeric(years))

# Code the final recommended age of vaccination and clean the dataset

pne <- pne %>%
  # Age: all in months
  mutate(age_mo = case_when(!is.na(weeks) ~ weeks/4.3,
                            !is.na(years) ~ years*12,
                            !is.na(months) ~ months)) %>%
  mutate(age_mo = round(age_mo, digits = 1)) %>%
  # Age: all in weeks
  mutate(age_we = case_when(!is.na(weeks) ~ weeks,
                            !is.na(years) ~ years*12*4.3,
                            !is.na(months) ~ months*4.3)) %>%
  # Filter to only childhood immunization (max 4 year)
  filter(age_we <= 192) %>%
  # Clean
  mutate(vaccine = paste0("PCV-D", SCHEDULEROUNDS)) %>%
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, AGEADMINISTERED, vaccine, age_mo, age_we)


#### Rota ####

data <- read_excel("C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Schedule/Vaccination schedule for Rotavirus.xlsx")

# Basic clean: variables and scope of interest

rot <- data %>%
  # Getting the Routine Immunization at National level ONLY
  filter(TARGETPOP_DESCRIPTION == "General/routine") %>%
  filter(GEOAREA == "NATIONAL") %>%
  # Getting data for first three doses
  filter(SCHEDULEROUNDS <= 3) %>%
  # Cleaning
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, VACCINECODE, SCHEDULEROUNDS, AGEADMINISTERED)

# Code the recommended age in weeks, months and years

rot <- rot %>%
  # Age: weeks
  mutate(weeks = case_when(substr(AGEADMINISTERED, 1, 1) == "W" ~ AGEADMINISTERED)) %>%
  mutate(weeks = substr(weeks, 2, 3)) %>%
  mutate(weeks = case_when(substr(weeks, 2, 2) == "." | substr(weeks, 2, 2) == "-" ~ substr(weeks, 1, 1),
                           substr(weeks, 2, 2) != "." | substr(weeks, 2, 2) != "-" ~ weeks)) %>%
  mutate(weeks = as.numeric(weeks)) %>%
  # Age: months
  mutate(months = case_when(substr(AGEADMINISTERED, 1, 1) == "M" ~ AGEADMINISTERED)) %>%
  mutate(months = substr(months, 2, 3)) %>%
  mutate(months = case_when(substr(months, 2, 2) == "." | substr(months, 2, 2) == "-" ~ substr(months, 1, 1),
                            substr(months, 2, 2) != "." | substr(months, 2, 2) != "-" ~ months)) %>%
  mutate(months = as.numeric(months)) %>%
  # Age: years
  mutate(years = case_when(substr(AGEADMINISTERED, 1, 1) == "Y" ~ AGEADMINISTERED)) %>%
  mutate(years = substr(years, 2, 3)) %>%
  mutate(years = case_when(substr(years, 2, 2) == "." | substr(years, 2, 2) == "-" ~ substr(years, 1, 1),
                           substr(years, 2, 2) != "." | substr(years, 2, 2) != "-" ~ years)) %>%
  mutate(years = as.numeric(years))

# Code the final recommended age of vaccination and clean the dataset

rot <- rot %>%
  # Age: all in months
  mutate(age_mo = case_when(!is.na(weeks) ~ weeks/4.3,
                            !is.na(years) ~ years*12,
                            !is.na(months) ~ months)) %>%
  mutate(age_mo = round(age_mo, digits = 1)) %>%
  # Age: all in weeks
  mutate(age_we = case_when(!is.na(weeks) ~ weeks,
                            !is.na(years) ~ years*12*4.3,
                            !is.na(months) ~ months*4.3)) %>%
  # Filter to only childhood immunization (max 4 year)
  filter(age_we <= 192) %>%
  # Clean
  mutate(vaccine = paste0("RV-D", SCHEDULEROUNDS)) %>%
  select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, AGEADMINISTERED, vaccine, age_mo, age_we)


#### Merge ####

# Merge all together and filter by countries with available DHS

who_data <- bcg %>% rbind(dtp) %>% rbind(mcv) %>% rbind(pol) %>%
  rbind(hep) %>% rbind(hib) %>% rbind(pne) %>% rbind(rot) %>%
  mutate(origin = "Recommended vaccination age (accord to schedule, WHO data)")
colnames(who_data) <- c("iso3", "country", "who_region_code", "recomm_age", "vaccine", "recomm_months", "recomm_weeks", "origin")

rm(data, bcg, dtp, mcv, pol, hep, hib, pne, rot)

who_data[who_data == "HepB-D0"] <- "HepB-BD"
who_data[who_data == "OPV-D0"] <- "OPV-BD"

who_data$country[who_data$country == "Gambia"] <- "The Gambia"


#### Save ####

write.csv(who_data, file = "Data/WHO_schedules_reference.csv", row.names = FALSE)
