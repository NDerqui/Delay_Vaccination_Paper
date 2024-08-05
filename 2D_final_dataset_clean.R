#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Timeliness of 24 childhood immunisations and evolution of vaccination delay:#
#-#-#-#-#- analysis of data from 54 low- and middle-income countries -#-#-#-#-#


# 2/ Clean raw data: final dataset for analyses



# SET UP ------------------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(matrixStats)
library(lubridate)  # make_date



# REFERENCE ---------------------------------------------------------------


#### Variables ####

information <- read.csv("Data/All_vax_available_Most_recent.csv")


#### WHO ####

# Get data from WHO
# Recommended vaccination schedules

who_data <- read.csv(file = "Data/WHO_schedules_reference.csv")

# Prepare for further analyses, omit age in months

who_data <- who_data %>%
  select(iso3, vaccine, recomm_weeks) %>%
  arrange(vaccine, iso3) %>%
  group_by(iso3, vaccine) %>%
  filter(recomm_weeks == min(recomm_weeks)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  pivot_wider(names_from = vaccine, values_from = recomm_weeks)

colnames(who_data)

colnames(who_data) <- c("iso3", "rec_bcg",
                        "rec_dtp1", "rec_dtp2", "rec_dtp3",  "rec_dtp4",
                        "rec_hepB0", "rec_hepB1", "rec_hepB2", "rec_hepB3", "rec_hepB4",
                        "rec_hib1", "rec_hib2", "rec_hib3",
                        "rec_ipv1", "rec_ipv2", "rec_ipv3", "rec_ipv4",
                        "rec_mcv1", "rec_mcv2",
                        "rec_opv0", "rec_opv1", "rec_opv2", "rec_opv3", "rec_opv4",
                        "rec_pneumo1", "rec_pneumo2", "rec_pneumo3",  "rec_pneumo4",
                        "rec_rota1", "rec_rota2", "rec_rota3")

# Remove unwanted vars (to avoid confusion)

who_data <- who_data %>%
  select(-c(rec_dtp4, rec_hepB4, rec_ipv3, rec_ipv4, rec_opv4, rec_pneumo4))

# Get data from WHO
# Introduction of vaccination in each country

who_intro <- read.csv(file = "Data/WHO_vaccination_introduction_reference.csv")

# Prepare for further analyses

who_intro <- who_intro %>%
  select(iso3, vaccine, intro_year) %>%
  arrange(vaccine, iso3) %>%
  pivot_wider(names_from = vaccine, values_from = intro_year)

# Add missing vaccines and doses (assume basic vaccines were before 2000,
# but using here 2000 as that is earlier than our timeframe of interest)

who_intro <- who_intro %>%
  mutate(introyear_bcg = 2000) %>%
  mutate(introyear_dtp1 = 2000) %>%
  mutate(introyear_dtp2 = 2000) %>%
  mutate(introyear_dtp3 = 2000) %>%
  mutate(introyear_hepB0 = `HepB-BD`) %>%
  mutate(introyear_hepB1 = HepB) %>%
  mutate(introyear_hepB2 = HepB) %>%
  mutate(introyear_hepB3 = HepB) %>%
  mutate(introyear_hib1 = Hib) %>%
  mutate(introyear_hib2 = Hib) %>%
  mutate(introyear_hib3 = Hib) %>%
  mutate(introyear_ipv1 = `IPV-D1`) %>%
  mutate(introyear_ipv2 = `IPV-D2`) %>%
  mutate(introyear_mcv1 = 2000) %>%
  mutate(introyear_mcv2 = `MCV-D2`) %>%
  mutate(introyear_opv0 = 2000) %>%
  mutate(introyear_opv1 = 2000) %>%
  mutate(introyear_opv2 = 2000) %>%
  mutate(introyear_opv3 = 2000) %>%
  mutate(introyear_pneumo1 = PCV) %>%
  mutate(introyear_pneumo2 = PCV) %>%
  mutate(introyear_pneumo3 = PCV) %>%
  mutate(introyear_rota1 = RV) %>%
  mutate(introyear_rota2 = RV) %>%
  mutate(introyear_rota3 = RV) %>%
  select(iso3, introyear_bcg,
         introyear_dtp1, introyear_dtp2, introyear_dtp3,
         introyear_hepB0, introyear_hepB1, introyear_hepB2, introyear_hepB3,
         introyear_hib1, introyear_hib2, introyear_hib3,
         introyear_ipv1, introyear_ipv2,
         introyear_mcv1, introyear_mcv2,
         introyear_opv0, introyear_opv1, introyear_opv2, introyear_opv3,
         introyear_pneumo1, introyear_pneumo2, introyear_pneumo3,
         introyear_rota1, introyear_rota2, introyear_rota3)



# LOOP -----------------------------------------------------------------


steps <- c(1:2)

## Cleaning the data in two chunks and then merging

for (step in 1:2) {
  
  
  ## Data
  
  data_vax <- readRDS(paste0("Data/Clean_vax_data_Most_recent_", steps[step], ".Rds"))
  
  
  # Start cleaning the vax data
  
  vaxdata <- data_vax
  
  
  
  # LESS VACCINE VARS ----------------------------------------------------------
  
  
  #### Identify extra vars ####
  
  # Establish the extra vars for each antigen
  
  # Identify all the vars we need for each antigen
  #1 Identify with the dictionary
  #2 Remove NAs
  #3 Select vars present in each database
  
  # Need to do this by hand for each vaccine: specs on dose
  
  ##### General #####
  
  # BCG
  vars_BCG <- information$var[information$antigen == "BCG"
                              & information$typical == 0 & information$date_info == 0]
  vars_BCG <- vars_BCG[!is.na(vars_BCG)]
  vars_BCG <- vars_BCG[vars_BCG %in% colnames(vaxdata)]
  
  # DTP1
  vars_DTP1 <- information$var[information$antigen == "DTP" & information$dose == 1
                               & information$typical == 0 & information$date_info == 0]
  vars_DTP1 <- vars_DTP1[!is.na(vars_DTP1)]
  vars_DTP1 <- vars_DTP1[vars_DTP1 %in% colnames(vaxdata)]
  
  # DTP2
  vars_DTP2 <- information$var[information$antigen == "DTP" & information$dose == 2
                               & information$typical == 0 & information$date_info == 0]
  vars_DTP2 <- vars_DTP2[!is.na(vars_DTP2)]
  vars_DTP2 <- vars_DTP2[vars_DTP2 %in% colnames(vaxdata)]
  
  # DTP3
  vars_DTP3 <- information$var[information$antigen == "DTP" & information$dose == 3
                               & information$typical == 0 & information$date_info == 0]
  vars_DTP3 <- vars_DTP3[!is.na(vars_DTP3)]
  vars_DTP3 <- vars_DTP3[vars_DTP3 %in% colnames(vaxdata)]
  
  # OPV0
  vars_OPV0 <- information$var[information$antigen == "OPV" & information$dose == 0
                               & information$typical == 0 & information$date_info == 0]
  vars_OPV0 <- vars_OPV0[!is.na(vars_OPV0)]
  vars_OPV0 <- vars_OPV0[vars_OPV0 %in% colnames(vaxdata)]
  
  # OPV1
  vars_OPV1 <- information$var[information$antigen == "OPV" & information$dose == 1
                                 & information$typical == 0 & information$date_info == 0]
  vars_OPV1 <- vars_OPV1[!is.na(vars_OPV1)]
  vars_OPV1 <- vars_OPV1[vars_OPV1 %in% colnames(vaxdata)]
  
  # OPV2
  vars_OPV2 <- information$var[information$antigen == "OPV" & information$dose == 2
                                 & information$typical == 0 & information$date_info == 0]
  vars_OPV2 <- vars_OPV2[!is.na(vars_OPV2)]
  vars_OPV2 <- vars_OPV2[vars_OPV2 %in% colnames(vaxdata)]
  
  # OPV3
  vars_OPV3 <- information$var[information$antigen == "OPV" & information$dose == 3
                                 & information$typical == 0 & information$date_info == 0]
  vars_OPV3 <- vars_OPV3[!is.na(vars_OPV3)]
  vars_OPV3 <- vars_OPV3[vars_OPV3 %in% colnames(vaxdata)]
  
  # IPV1
  vars_IPV1 <- information$var[information$antigen == "IPV" & information$dose == 1
                                 & information$typical == 0 & information$date_info == 0]
  vars_IPV1 <- vars_IPV1[!is.na(vars_IPV1)]
  vars_IPV1 <- vars_IPV1[vars_IPV1 %in% colnames(vaxdata)]
  
  # IPV2
  vars_IPV2 <- information$var[information$antigen == "IPV" & information$dose == 2
                                 & information$typical == 0 & information$date_info == 0]
  vars_IPV2 <- vars_IPV2[!is.na(vars_IPV2)]
  vars_IPV2 <- vars_IPV2[vars_IPV2 %in% colnames(vaxdata)]
  
  # MCV1
  vars_MCV1 <- information$var[(information$antigen == "MCV" | information$antigen == "MMR")
                               & (information$dose == 1 | is.na(information$dose))
                               & information$typical == 0 & information$date_info == 0]
  vars_MCV1 <- vars_MCV1[!is.na(vars_MCV1)]
  vars_MCV1 <- vars_MCV1[vars_MCV1 %in% colnames(vaxdata)]
  
  # MCV2
  vars_MCV2 <- information$var[(information$antigen == "MCV" | information$antigen == "MMR")
                               & information$dose == 2
                               & information$typical == 0 & information$date_info == 0]
  vars_MCV2 <- vars_MCV2[!is.na(vars_MCV2)]
  vars_MCV2 <- vars_MCV2[vars_MCV2 %in% colnames(vaxdata)]
  
  # HepB0
  vars_HepB0 <- information$var[information$antigen == "HepB" & information$dose == 0
                                & information$typical == 0 & information$date_info == 0]
  vars_HepB0 <- vars_HepB0[!is.na(vars_HepB0)]
  vars_HepB0 <- vars_HepB0[vars_HepB0 %in% colnames(vaxdata)]
  
  # HepB1
  vars_HepB1 <- information$var[information$antigen == "HepB" & information$dose == 1
                                & information$typical == 0 & information$date_info == 0]
  vars_HepB1 <- vars_HepB1[!is.na(vars_HepB1)]
  vars_HepB1 <- vars_HepB1[vars_HepB1 %in% colnames(vaxdata)]
  
  # HepB2
  vars_HepB2 <- information$var[information$antigen == "HepB" & information$dose == 2
                                & information$typical == 0 & information$date_info == 0]
  vars_HepB2 <- vars_HepB2[!is.na(vars_HepB2)]
  vars_HepB2 <- vars_HepB2[vars_HepB2 %in% colnames(vaxdata)]
  
  # HepB3
  vars_HepB3 <- information$var[information$antigen == "HepB" & information$dose == 3
                                & information$typical == 0 & information$date_info == 0]
  vars_HepB3 <- vars_HepB3[!is.na(vars_HepB3)]
  vars_HepB3 <- vars_HepB3[vars_HepB3 %in% colnames(vaxdata)]
  
  # Hib1
  vars_Hib1 <- information$var[information$antigen == "Hib" & information$dose == 1
                               & information$typical == 0 & information$date_info == 0]
  vars_Hib1 <- vars_Hib1[!is.na(vars_Hib1)]
  vars_Hib1 <- vars_Hib1[vars_Hib1 %in% colnames(vaxdata)]
  
  # Hib2
  vars_Hib2 <- information$var[information$antigen == "Hib" & information$dose == 2
                               & information$typical == 0 & information$date_info == 0]
  vars_Hib2 <- vars_Hib2[!is.na(vars_Hib2)]
  vars_Hib2 <- vars_Hib2[vars_Hib2 %in% colnames(vaxdata)]
  
  # Hib3
  vars_Hib3 <- information$var[information$antigen == "Hib" & information$dose == 3
                               & information$typical == 0 & information$date_info == 0]
  vars_Hib3 <- vars_Hib3[!is.na(vars_Hib3)]
  vars_Hib3 <- vars_Hib3[vars_Hib3 %in% colnames(vaxdata)]
  
  # Pneumo1
  vars_Pneumo1 <- information$var[information$antigen == "Pneumo" & information$dose == 1
                                  & information$typical == 0 & information$date_info == 0]
  vars_Pneumo1 <- vars_Pneumo1[!is.na(vars_Pneumo1)]
  vars_Pneumo1 <- vars_Pneumo1[vars_Pneumo1 %in% colnames(vaxdata)]
  
  # Pneumo2
  vars_Pneumo2 <- information$var[information$antigen == "Pneumo" & information$dose == 2
                                  & information$typical == 0 & information$date_info == 0]
  vars_Pneumo2 <- vars_Pneumo2[!is.na(vars_Pneumo2)]
  vars_Pneumo2 <- vars_Pneumo2[vars_Pneumo2 %in% colnames(vaxdata)]
  
  # Pneumo3
  vars_Pneumo3 <- information$var[information$antigen == "Pneumo" & information$dose == 3
                                  & information$typical == 0 & information$date_info == 0]
  vars_Pneumo3 <- vars_Pneumo3[!is.na(vars_Pneumo3)]
  vars_Pneumo3 <- vars_Pneumo3[vars_Pneumo3 %in% colnames(vaxdata)]
  
  # Rota1
  vars_Rota1 <- information$var[information$antigen == "Rota" & information$dose == 1
                                & information$typical == 0 & information$date_info == 0]
  vars_Rota1 <- vars_Rota1[!is.na(vars_Rota1)]
  vars_Rota1 <- vars_Rota1[vars_Rota1 %in% colnames(vaxdata)]
  
  # Rota2
  vars_Rota2 <- information$var[information$antigen == "Rota" & information$dose == 2
                                & information$typical == 0 & information$date_info == 0]
  vars_Rota2 <- vars_Rota2[!is.na(vars_Rota2)]
  vars_Rota2 <- vars_Rota2[vars_Rota2 %in% colnames(vaxdata)]
  
  # Rota3
  vars_Rota3 <- information$var[information$antigen == "Rota" & information$dose == 3
                                & information$typical == 0 & information$date_info == 0]
  vars_Rota3 <- vars_Rota3[!is.na(vars_Rota3)]
  vars_Rota3 <- vars_Rota3[vars_Rota3 %in% colnames(vaxdata)]
  
  ##### Dates ##### 
  
  ######## Year
  
  # BCG
  vars_y_BCG <- information$var[information$antigen == "BCG"
                                & information$typical == 0 & information$date_year == TRUE]
  vars_y_BCG <- vars_y_BCG[!is.na(vars_y_BCG)]
  vars_y_BCG <- vars_y_BCG[vars_y_BCG %in% colnames(vaxdata)]
  
  # DTP1
  vars_y_DTP1 <- information$var[information$antigen == "DTP" & information$dose == 1
                                 & information$typical == 0 & information$date_year == TRUE]
  vars_y_DTP1 <- vars_y_DTP1[!is.na(vars_y_DTP1)]
  vars_y_DTP1 <- vars_y_DTP1[vars_y_DTP1 %in% colnames(vaxdata)]
  
  # DTP2
  vars_y_DTP2 <- information$var[information$antigen == "DTP" & information$dose == 2
                                 & information$typical == 0 & information$date_year == TRUE]
  vars_y_DTP2 <- vars_y_DTP2[!is.na(vars_y_DTP2)]
  vars_y_DTP2 <- vars_y_DTP2[vars_y_DTP2 %in% colnames(vaxdata)]
  
  # DTP3
  vars_y_DTP3 <- information$var[information$antigen == "DTP" & information$dose == 3
                                 & information$typical == 0 & information$date_year == TRUE]
  vars_y_DTP3 <- vars_y_DTP3[!is.na(vars_y_DTP3)]
  vars_y_DTP3 <- vars_y_DTP3[vars_y_DTP3 %in% colnames(vaxdata)]
  
  # OPV0
  vars_y_OPV0 <- information$var[information$antigen == "OPV" & information$dose == 0
                                   & information$typical == 0 & information$date_year == TRUE]
  vars_y_OPV0 <- vars_y_OPV0[!is.na(vars_y_OPV0)]
  vars_y_OPV0 <- vars_y_OPV0[vars_y_OPV0 %in% colnames(vaxdata)]
  
  # OPV1
  vars_y_OPV1 <- information$var[information$antigen == "OPV" & information$dose == 1
                                   & information$typical == 0 & information$date_year == TRUE]
  vars_y_OPV1 <- vars_y_OPV1[!is.na(vars_y_OPV1)]
  vars_y_OPV1 <- vars_y_OPV1[vars_y_OPV1 %in% colnames(vaxdata)]
  
  # OPV2
  vars_y_OPV2 <- information$var[information$antigen == "OPV" & information$dose == 2
                                   & information$typical == 0 & information$date_year == TRUE]
  vars_y_OPV2 <- vars_y_OPV2[!is.na(vars_y_OPV2)]
  vars_y_OPV2 <- vars_y_OPV2[vars_y_OPV2 %in% colnames(vaxdata)]
  
  # OPV3
  vars_y_OPV3 <- information$var[information$antigen == "OPV" & information$dose == 3
                                   & information$typical == 0 & information$date_year == TRUE]
  vars_y_OPV3 <- vars_y_OPV3[!is.na(vars_y_OPV3)]
  vars_y_OPV3 <- vars_y_OPV3[vars_y_OPV3 %in% colnames(vaxdata)]
  
  # IPV1
  vars_y_IPV1 <- information$var[information$antigen == "IPV" & information$dose == 1
                                   & information$typical == 0 & information$date_year == TRUE]
  vars_y_IPV1 <- vars_y_IPV1[!is.na(vars_y_IPV1)]
  vars_y_IPV1 <- vars_y_IPV1[vars_y_IPV1 %in% colnames(vaxdata)]
  
  # IPV2
  vars_y_IPV2 <- information$var[information$antigen == "IPV" & information$dose == 2
                                   & information$typical == 0 & information$date_year == TRUE]
  vars_y_IPV2 <- vars_y_IPV2[!is.na(vars_y_IPV2)]
  vars_y_IPV2 <- vars_y_IPV2[vars_y_IPV2 %in% colnames(vaxdata)]
  
  # MCV1
  vars_y_MCV1 <- information$var[(information$antigen == "MCV" | information$antigen == "MMR")
                                 & (information$dose == 1 | is.na(information$dose))
                                 & information$typical == 0 & information$date_year == TRUE]
  vars_y_MCV1 <- vars_y_MCV1[!is.na(vars_y_MCV1)]
  vars_y_MCV1 <- vars_y_MCV1[vars_y_MCV1 %in% colnames(vaxdata)]
  
  # MCV2
  vars_y_MCV2 <- information$var[(information$antigen == "MCV" | information$antigen == "MMR")
                                 & information$dose == 2
                                 & information$typical == 0 & information$date_year == TRUE]
  vars_y_MCV2 <- vars_y_MCV2[!is.na(vars_y_MCV2)]
  vars_y_MCV2 <- vars_y_MCV2[vars_y_MCV2 %in% colnames(vaxdata)]
  
  # HepB0
  vars_y_HepB0 <- information$var[information$antigen == "HepB" & information$dose == 0
                                  & information$typical == 0 & information$date_year == TRUE]
  vars_y_HepB0 <- vars_y_HepB0[!is.na(vars_y_HepB0)]
  vars_y_HepB0 <- vars_y_HepB0[vars_y_HepB0 %in% colnames(vaxdata)]
  
  # HepB1
  vars_y_HepB1 <- information$var[information$antigen == "HepB" & information$dose == 1
                                  & information$typical == 0 & information$date_year == TRUE]
  vars_y_HepB1 <- vars_y_HepB1[!is.na(vars_y_HepB1)]
  vars_y_HepB1 <- vars_y_HepB1[vars_y_HepB1 %in% colnames(vaxdata)]
  
  # HepB2
  vars_y_HepB2 <- information$var[information$antigen == "HepB" & information$dose == 2
                                  & information$typical == 0 & information$date_year == TRUE]
  vars_y_HepB2 <- vars_y_HepB2[!is.na(vars_y_HepB2)]
  vars_y_HepB2 <- vars_y_HepB2[vars_y_HepB2 %in% colnames(vaxdata)]
  
  # HepB3
  vars_y_HepB3 <- information$var[information$antigen == "HepB" & information$dose == 3
                                  & information$typical == 0 & information$date_year == TRUE]
  vars_y_HepB3 <- vars_y_HepB3[!is.na(vars_y_HepB3)]
  vars_y_HepB3 <- vars_y_HepB3[vars_y_HepB3 %in% colnames(vaxdata)]
  
  # Hib1
  vars_y_Hib1 <- information$var[information$antigen == "Hib" & information$dose == 1
                                 & information$typical == 0 & information$date_year == TRUE]
  vars_y_Hib1 <- vars_y_Hib1[!is.na(vars_y_Hib1)]
  vars_y_Hib1 <- vars_y_Hib1[vars_y_Hib1 %in% colnames(vaxdata)]
  
  # Hib2
  vars_y_Hib2 <- information$var[information$antigen == "Hib" & information$dose == 2
                                 & information$typical == 0 & information$date_year == TRUE]
  vars_y_Hib2 <- vars_y_Hib2[!is.na(vars_y_Hib2)]
  vars_y_Hib2 <- vars_y_Hib2[vars_y_Hib2 %in% colnames(vaxdata)]
  
  # Hib3
  vars_y_Hib3 <- information$var[information$antigen == "Hib" & information$dose == 3
                                 & information$typical == 0 & information$date_year == TRUE]
  vars_y_Hib3 <- vars_y_Hib3[!is.na(vars_y_Hib3)]
  vars_y_Hib3 <- vars_y_Hib3[vars_y_Hib3 %in% colnames(vaxdata)]
  
  # Pneumo1
  vars_y_Pneumo1 <- information$var[information$antigen == "Pneumo" & information$dose == 1
                                    & information$typical == 0 & information$date_year == TRUE]
  vars_y_Pneumo1 <- vars_y_Pneumo1[!is.na(vars_y_Pneumo1)]
  vars_y_Pneumo1 <- vars_y_Pneumo1[vars_y_Pneumo1 %in% colnames(vaxdata)]
  
  # Pneumo2
  vars_y_Pneumo2 <- information$var[information$antigen == "Pneumo" & information$dose == 2
                                    & information$typical == 0 & information$date_year == TRUE]
  vars_y_Pneumo2 <- vars_y_Pneumo2[!is.na(vars_y_Pneumo2)]
  vars_y_Pneumo2 <- vars_y_Pneumo2[vars_y_Pneumo2 %in% colnames(vaxdata)]
  
  # Pneumo3
  vars_y_Pneumo3 <- information$var[information$antigen == "Pneumo" & information$dose == 3
                                    & information$typical == 0 & information$date_year == TRUE]
  vars_y_Pneumo3 <- vars_y_Pneumo3[!is.na(vars_y_Pneumo3)]
  vars_y_Pneumo3 <- vars_y_Pneumo3[vars_y_Pneumo3 %in% colnames(vaxdata)]
  
  # Rota1
  vars_y_Rota1 <- information$var[information$antigen == "Rota" & information$dose == 1
                                  & information$typical == 0 & information$date_year == TRUE]
  vars_y_Rota1 <- vars_y_Rota1[!is.na(vars_y_Rota1)]
  vars_y_Rota1 <- vars_y_Rota1[vars_y_Rota1 %in% colnames(vaxdata)]
  
  # Rota2
  vars_y_Rota2 <- information$var[information$antigen == "Rota" & information$dose == 2
                                  & information$typical == 0 & information$date_year == TRUE]
  vars_y_Rota2 <- vars_y_Rota2[!is.na(vars_y_Rota2)]
  vars_y_Rota2 <- vars_y_Rota2[vars_y_Rota2 %in% colnames(vaxdata)]
  
  # Rota3
  vars_y_Rota3 <- information$var[information$antigen == "Rota" & information$dose == 3
                                  & information$typical == 0 & information$date_year == TRUE]
  vars_y_Rota3 <- vars_y_Rota3[!is.na(vars_y_Rota3)]
  vars_y_Rota3 <- vars_y_Rota3[vars_y_Rota3 %in% colnames(vaxdata)]
  
  ######## Month
  
  # BCG
  vars_m_BCG <- information$var[information$antigen == "BCG"
                                & information$typical == 0 & information$date_month == TRUE]
  vars_m_BCG <- vars_m_BCG[!is.na(vars_m_BCG)]
  vars_m_BCG <- vars_m_BCG[vars_m_BCG %in% colnames(vaxdata)]
  
  # DTP1
  vars_m_DTP1 <- information$var[information$antigen == "DTP" & information$dose == 1
                                 & information$typical == 0 & information$date_month == TRUE]
  vars_m_DTP1 <- vars_m_DTP1[!is.na(vars_m_DTP1)]
  vars_m_DTP1 <- vars_m_DTP1[vars_m_DTP1 %in% colnames(vaxdata)]
  
  # DTP2
  vars_m_DTP2 <- information$var[information$antigen == "DTP" & information$dose == 2
                                 & information$typical == 0 & information$date_month == TRUE]
  vars_m_DTP2 <- vars_m_DTP2[!is.na(vars_m_DTP2)]
  vars_m_DTP2 <- vars_m_DTP2[vars_m_DTP2 %in% colnames(vaxdata)]
  
  # DTP3
  vars_m_DTP3 <- information$var[information$antigen == "DTP" & information$dose == 3
                                 & information$typical == 0 & information$date_month == TRUE]
  vars_m_DTP3 <- vars_m_DTP3[!is.na(vars_m_DTP3)]
  vars_m_DTP3 <- vars_m_DTP3[vars_m_DTP3 %in% colnames(vaxdata)]
  
  # OPV0
  vars_m_OPV0 <- information$var[information$antigen == "OPV" & information$dose == 0
                                   & information$typical == 0 & information$date_month == TRUE]
  vars_m_OPV0 <- vars_m_OPV0[!is.na(vars_m_OPV0)]
  vars_m_OPV0 <- vars_m_OPV0[vars_m_OPV0 %in% colnames(vaxdata)]
  
  # OPV1
  vars_m_OPV1 <- information$var[information$antigen == "OPV" & information$dose == 1
                                   & information$typical == 0 & information$date_month == TRUE]
  vars_m_OPV1 <- vars_m_OPV1[!is.na(vars_m_OPV1)]
  vars_m_OPV1 <- vars_m_OPV1[vars_m_OPV1 %in% colnames(vaxdata)]
  
  # OPV2
  vars_m_OPV2 <- information$var[information$antigen == "OPV" & information$dose == 2
                                   & information$typical == 0 & information$date_month == TRUE]
  vars_m_OPV2 <- vars_m_OPV2[!is.na(vars_m_OPV2)]
  vars_m_OPV2 <- vars_m_OPV2[vars_m_OPV2 %in% colnames(vaxdata)]
  
  # OPV3
  vars_m_OPV3 <- information$var[information$antigen == "OPV" & information$dose == 3
                                   & information$typical == 0 & information$date_month == TRUE]
  vars_m_OPV3 <- vars_m_OPV3[!is.na(vars_m_OPV3)]
  vars_m_OPV3 <- vars_m_OPV3[vars_m_OPV3 %in% colnames(vaxdata)]
  
  # IPV1
  vars_m_IPV1 <- information$var[information$antigen == "IPV" & information$dose == 1
                                   & information$typical == 0 & information$date_month == TRUE]
  vars_m_IPV1 <- vars_m_IPV1[!is.na(vars_m_IPV1)]
  vars_m_IPV1 <- vars_m_IPV1[vars_m_IPV1 %in% colnames(vaxdata)]
  
  # IPV2
  vars_m_IPV2 <- information$var[information$antigen == "IPV" & information$dose == 2
                                   & information$typical == 0 & information$date_month == TRUE]
  vars_m_IPV2 <- vars_m_IPV2[!is.na(vars_m_IPV2)]
  vars_m_IPV2 <- vars_m_IPV2[vars_m_IPV2 %in% colnames(vaxdata)]
  
  # MCV1
  vars_m_MCV1 <- information$var[(information$antigen == "MCV" | information$antigen == "MMR")
                                 & (information$dose == 1 | is.na(information$dose))
                                 & information$typical == 0 & information$date_month == TRUE]
  vars_m_MCV1 <- vars_m_MCV1[!is.na(vars_m_MCV1)]
  vars_m_MCV1 <- vars_m_MCV1[vars_m_MCV1 %in% colnames(vaxdata)]
  
  # MCV2
  vars_m_MCV2 <- information$var[(information$antigen == "MCV" | information$antigen == "MMR")
                                 & information$dose == 2
                                 & information$typical == 0 & information$date_month == TRUE]
  vars_m_MCV2 <- vars_m_MCV2[!is.na(vars_m_MCV2)]
  vars_m_MCV2 <- vars_m_MCV2[vars_m_MCV2 %in% colnames(vaxdata)]
  
  # HepB0
  vars_m_HepB0 <- information$var[information$antigen == "HepB" & information$dose == 0
                                  & information$typical == 0 & information$date_month == TRUE]
  vars_m_HepB0 <- vars_m_HepB0[!is.na(vars_m_HepB0)]
  vars_m_HepB0 <- vars_m_HepB0[vars_m_HepB0 %in% colnames(vaxdata)]
  
  # HepB1
  vars_m_HepB1 <- information$var[information$antigen == "HepB" & information$dose == 1
                                  & information$typical == 0 & information$date_month == TRUE]
  vars_m_HepB1 <- vars_m_HepB1[!is.na(vars_m_HepB1)]
  vars_m_HepB1 <- vars_m_HepB1[vars_m_HepB1 %in% colnames(vaxdata)]
  
  # HepB2
  vars_m_HepB2 <- information$var[information$antigen == "HepB" & information$dose == 2
                                  & information$typical == 0 & information$date_month == TRUE]
  vars_m_HepB2 <- vars_m_HepB2[!is.na(vars_m_HepB2)]
  vars_m_HepB2 <- vars_m_HepB2[vars_m_HepB2 %in% colnames(vaxdata)]
  
  # HepB3
  vars_m_HepB3 <- information$var[information$antigen == "HepB" & information$dose == 3
                                  & information$typical == 0 & information$date_month == TRUE]
  vars_m_HepB3 <- vars_m_HepB3[!is.na(vars_m_HepB3)]
  vars_m_HepB3 <- vars_m_HepB3[vars_m_HepB3 %in% colnames(vaxdata)]
  
  # Hib1
  vars_m_Hib1 <- information$var[information$antigen == "Hib" & information$dose == 1
                                 & information$typical == 0 & information$date_month == TRUE]
  vars_m_Hib1 <- vars_m_Hib1[!is.na(vars_m_Hib1)]
  vars_m_Hib1 <- vars_m_Hib1[vars_m_Hib1 %in% colnames(vaxdata)]
  
  # Hib2
  vars_m_Hib2 <- information$var[information$antigen == "Hib" & information$dose == 2
                                 & information$typical == 0 & information$date_month == TRUE]
  vars_m_Hib2 <- vars_m_Hib2[!is.na(vars_m_Hib2)]
  vars_m_Hib2 <- vars_m_Hib2[vars_m_Hib2 %in% colnames(vaxdata)]
  
  # Hib3
  vars_m_Hib3 <- information$var[information$antigen == "Hib" & information$dose == 3
                                 & information$typical == 0 & information$date_month == TRUE]
  vars_m_Hib3 <- vars_m_Hib3[!is.na(vars_m_Hib3)]
  vars_m_Hib3 <- vars_m_Hib3[vars_m_Hib3 %in% colnames(vaxdata)]
  
  # Pneumo1
  vars_m_Pneumo1 <- information$var[information$antigen == "Pneumo" & information$dose == 1
                                    & information$typical == 0 & information$date_month == TRUE]
  vars_m_Pneumo1 <- vars_m_Pneumo1[!is.na(vars_m_Pneumo1)]
  vars_m_Pneumo1 <- vars_m_Pneumo1[vars_m_Pneumo1 %in% colnames(vaxdata)]
  
  # Pneumo2
  vars_m_Pneumo2 <- information$var[information$antigen == "Pneumo" & information$dose == 2
                                    & information$typical == 0 & information$date_month == TRUE]
  vars_m_Pneumo2 <- vars_m_Pneumo2[!is.na(vars_m_Pneumo2)]
  vars_m_Pneumo2 <- vars_m_Pneumo2[vars_m_Pneumo2 %in% colnames(vaxdata)]
  
  # Pneumo3
  vars_m_Pneumo3 <- information$var[information$antigen == "Pneumo" & information$dose == 3
                                    & information$typical == 0 & information$date_month == TRUE]
  vars_m_Pneumo3 <- vars_m_Pneumo3[!is.na(vars_m_Pneumo3)]
  vars_m_Pneumo3 <- vars_m_Pneumo3[vars_m_Pneumo3 %in% colnames(vaxdata)]
  
  # Rota1
  vars_m_Rota1 <- information$var[information$antigen == "Rota" & information$dose == 1
                                  & information$typical == 0 & information$date_month == TRUE]
  vars_m_Rota1 <- vars_m_Rota1[!is.na(vars_m_Rota1)]
  vars_m_Rota1 <- vars_m_Rota1[vars_m_Rota1 %in% colnames(vaxdata)]
  
  # Rota2
  vars_m_Rota2 <- information$var[information$antigen == "Rota" & information$dose == 2
                                  & information$typical == 0 & information$date_month == TRUE]
  vars_m_Rota2 <- vars_m_Rota2[!is.na(vars_m_Rota2)]
  vars_m_Rota2 <- vars_m_Rota2[vars_m_Rota2 %in% colnames(vaxdata)]
  
  # Rota3
  vars_m_Rota3 <- information$var[information$antigen == "Rota" & information$dose == 3
                                  & information$typical == 0 & information$date_month == TRUE]
  vars_m_Rota3 <- vars_m_Rota3[!is.na(vars_m_Rota3)]
  vars_m_Rota3 <- vars_m_Rota3[vars_m_Rota3 %in% colnames(vaxdata)]
  
  ######## Day
  
  # BCG
  vars_d_BCG <- information$var[information$antigen == "BCG"
                                & information$typical == 0 & information$date_day == TRUE]
  vars_d_BCG <- vars_d_BCG[!is.na(vars_d_BCG)]
  vars_d_BCG <- vars_d_BCG[vars_d_BCG %in% colnames(vaxdata)]
  
  # DTP1
  vars_d_DTP1 <- information$var[information$antigen == "DTP" & information$dose == 1
                                 & information$typical == 0 & information$date_day == TRUE]
  vars_d_DTP1 <- vars_d_DTP1[!is.na(vars_d_DTP1)]
  vars_d_DTP1 <- vars_d_DTP1[vars_d_DTP1 %in% colnames(vaxdata)]
  
  # DTP2
  vars_d_DTP2 <- information$var[information$antigen == "DTP" & information$dose == 2
                                 & information$typical == 0 & information$date_day == TRUE]
  vars_d_DTP2 <- vars_d_DTP2[!is.na(vars_d_DTP2)]
  vars_d_DTP2 <- vars_d_DTP2[vars_d_DTP2 %in% colnames(vaxdata)]
  
  # DTP3
  vars_d_DTP3 <- information$var[information$antigen == "DTP" & information$dose == 3
                                 & information$typical == 0 & information$date_day == TRUE]
  vars_d_DTP3 <- vars_d_DTP3[!is.na(vars_d_DTP3)]
  vars_d_DTP3 <- vars_d_DTP3[vars_d_DTP3 %in% colnames(vaxdata)]
  
  # OPV0
  vars_d_OPV0 <- information$var[information$antigen == "OPV" & information$dose == 0
                                   & information$typical == 0 & information$date_day == TRUE]
  vars_d_OPV0 <- vars_d_OPV0[!is.na(vars_d_OPV0)]
  vars_d_OPV0 <- vars_d_OPV0[vars_d_OPV0 %in% colnames(vaxdata)]
  
  # OPV1
  vars_d_OPV1 <- information$var[information$antigen == "OPV" & information$dose == 1
                                   & information$typical == 0 & information$date_day == TRUE]
  vars_d_OPV1 <- vars_d_OPV1[!is.na(vars_d_OPV1)]
  vars_d_OPV1 <- vars_d_OPV1[vars_d_OPV1 %in% colnames(vaxdata)]
  
  # OPV2
  vars_d_OPV2 <- information$var[information$antigen == "OPV" & information$dose == 2
                                   & information$typical == 0 & information$date_day == TRUE]
  vars_d_OPV2 <- vars_d_OPV2[!is.na(vars_d_OPV2)]
  vars_d_OPV2 <- vars_d_OPV2[vars_d_OPV2 %in% colnames(vaxdata)]
  
  # OPV3
  vars_d_OPV3 <- information$var[information$antigen == "OPV" & information$dose == 3
                                   & information$typical == 0 & information$date_day == TRUE]
  vars_d_OPV3 <- vars_d_OPV3[!is.na(vars_d_OPV3)]
  vars_d_OPV3 <- vars_d_OPV3[vars_d_OPV3 %in% colnames(vaxdata)]
  
  # IPV1
  vars_d_IPV1 <- information$var[information$antigen == "IPV" & information$dose == 1
                                   & information$typical == 0 & information$date_day == TRUE]
  vars_d_IPV1 <- vars_d_IPV1[!is.na(vars_d_IPV1)]
  vars_d_IPV1 <- vars_d_IPV1[vars_d_IPV1 %in% colnames(vaxdata)]
  
  # IPV2
  vars_d_IPV2 <- information$var[information$antigen == "IPV" & information$dose == 2
                                   & information$typical == 0 & information$date_day == TRUE]
  vars_d_IPV2 <- vars_d_IPV2[!is.na(vars_d_IPV2)]
  vars_d_IPV2 <- vars_d_IPV2[vars_d_IPV2 %in% colnames(vaxdata)]
  
  # MCV1
  vars_d_MCV1 <- information$var[(information$antigen == "MCV" | information$antigen == "MMR")
                                 & (information$dose == 1 | is.na(information$dose))
                                 & information$typical == 0 & information$date_day == TRUE]
  vars_d_MCV1 <- vars_d_MCV1[!is.na(vars_d_MCV1)]
  vars_d_MCV1 <- vars_d_MCV1[vars_d_MCV1 %in% colnames(vaxdata)]
  
  # MCV2
  vars_d_MCV2 <- information$var[(information$antigen == "MCV" | information$antigen == "MMR")
                                 & information$dose == 2
                                 & information$typical == 0 & information$date_day == TRUE]
  vars_d_MCV2 <- vars_d_MCV2[!is.na(vars_d_MCV2)]
  vars_d_MCV2 <- vars_d_MCV2[vars_d_MCV2 %in% colnames(vaxdata)]
  
  # HepB0
  vars_d_HepB0 <- information$var[information$antigen == "HepB" & information$dose == 0
                                  & information$typical == 0 & information$date_day == TRUE]
  vars_d_HepB0 <- vars_d_HepB0[!is.na(vars_d_HepB0)]
  vars_d_HepB0 <- vars_d_HepB0[vars_d_HepB0 %in% colnames(vaxdata)]
  
  # HepB1
  vars_d_HepB1 <- information$var[information$antigen == "HepB" & information$dose == 1
                                  & information$typical == 0 & information$date_day == TRUE]
  vars_d_HepB1 <- vars_d_HepB1[!is.na(vars_d_HepB1)]
  vars_d_HepB1 <- vars_d_HepB1[vars_d_HepB1 %in% colnames(vaxdata)]
  
  # HepB2
  vars_d_HepB2 <- information$var[information$antigen == "HepB" & information$dose == 2
                                  & information$typical == 0 & information$date_day == TRUE]
  vars_d_HepB2 <- vars_d_HepB2[!is.na(vars_d_HepB2)]
  vars_d_HepB2 <- vars_d_HepB2[vars_d_HepB2 %in% colnames(vaxdata)]
  
  # HepB3
  vars_d_HepB3 <- information$var[information$antigen == "HepB" & information$dose == 3
                                  & information$typical == 0 & information$date_day == TRUE]
  vars_d_HepB3 <- vars_d_HepB3[!is.na(vars_d_HepB3)]
  vars_d_HepB3 <- vars_d_HepB3[vars_d_HepB3 %in% colnames(vaxdata)]
  
  # Hib1
  vars_d_Hib1 <- information$var[information$antigen == "Hib" & information$dose == 1
                                 & information$typical == 0 & information$date_day == TRUE]
  vars_d_Hib1 <- vars_d_Hib1[!is.na(vars_d_Hib1)]
  vars_d_Hib1 <- vars_d_Hib1[vars_d_Hib1 %in% colnames(vaxdata)]
  
  # Hib2
  vars_d_Hib2 <- information$var[information$antigen == "Hib" & information$dose == 2
                                 & information$typical == 0 & information$date_day == TRUE]
  vars_d_Hib2 <- vars_d_Hib2[!is.na(vars_d_Hib2)]
  vars_d_Hib2 <- vars_d_Hib2[vars_d_Hib2 %in% colnames(vaxdata)]
  
  # Hib3
  vars_d_Hib3 <- information$var[information$antigen == "Hib" & information$dose == 3
                                 & information$typical == 0 & information$date_day == TRUE]
  vars_d_Hib3 <- vars_d_Hib3[!is.na(vars_d_Hib3)]
  vars_d_Hib3 <- vars_d_Hib3[vars_d_Hib3 %in% colnames(vaxdata)]
  
  # Pneumo1
  vars_d_Pneumo1 <- information$var[information$antigen == "Pneumo" & information$dose == 1
                                    & information$typical == 0 & information$date_day == TRUE]
  vars_d_Pneumo1 <- vars_d_Pneumo1[!is.na(vars_d_Pneumo1)]
  vars_d_Pneumo1 <- vars_d_Pneumo1[vars_d_Pneumo1 %in% colnames(vaxdata)]
  
  # Pneumo2
  vars_d_Pneumo2 <- information$var[information$antigen == "Pneumo" & information$dose == 2
                                    & information$typical == 0 & information$date_day == TRUE]
  vars_d_Pneumo2 <- vars_d_Pneumo2[!is.na(vars_d_Pneumo2)]
  vars_d_Pneumo2 <- vars_d_Pneumo2[vars_d_Pneumo2 %in% colnames(vaxdata)]
  
  # Pneumo3
  vars_d_Pneumo3 <- information$var[information$antigen == "Pneumo" & information$dose == 3
                                    & information$typical == 0 & information$date_day == TRUE]
  vars_d_Pneumo3 <- vars_d_Pneumo3[!is.na(vars_d_Pneumo3)]
  vars_d_Pneumo3 <- vars_d_Pneumo3[vars_d_Pneumo3 %in% colnames(vaxdata)]
  
  # Rota1
  vars_d_Rota1 <- information$var[information$antigen == "Rota" & information$dose == 1
                                  & information$typical == 0 & information$date_day == TRUE]
  vars_d_Rota1 <- vars_d_Rota1[!is.na(vars_d_Rota1)]
  vars_d_Rota1 <- vars_d_Rota1[vars_d_Rota1 %in% colnames(vaxdata)]
  
  # Rota2
  vars_d_Rota2 <- information$var[information$antigen == "Rota" & information$dose == 2
                                  & information$typical == 0 & information$date_day == TRUE]
  vars_d_Rota2 <- vars_d_Rota2[!is.na(vars_d_Rota2)]
  vars_d_Rota2 <- vars_d_Rota2[vars_d_Rota2 %in% colnames(vaxdata)]
  
  # Rota3
  vars_d_Rota3 <- information$var[information$antigen == "Rota" & information$dose == 3
                                  & information$typical == 0 & information$date_day == TRUE]
  vars_d_Rota3 <- vars_d_Rota3[!is.na(vars_d_Rota3)]
  vars_d_Rota3 <- vars_d_Rota3[vars_d_Rota3 %in% colnames(vaxdata)]
  
  
  #### Save lists ####
  
  # List for normal var information
  
  vars_vax <- list(names = c("BCG", "DTP1", "DTP2", "DTP3", "OPV0", "OPV1", "OPV2", "OPV3", 
                             "IPV1", "IPV2", "MCV1", "MCV2",
                             "HepB0", "HepB1", "HepB2", "HepB3", "Hib1", "Hib2", "Hib3",
                             "Pneumo1", "Pneumo2", "Pneumo3", "Rota1", "Rota2", "Rota3"),
                   vars = list(vars_BCG, vars_DTP1, vars_DTP2, vars_DTP3,
                               vars_OPV0, vars_OPV1, vars_OPV2, vars_OPV3,
                               vars_IPV1, vars_IPV2,
                               vars_MCV1, vars_MCV2,
                               vars_HepB0, vars_HepB1, vars_HepB2, vars_HepB3,
                               vars_Hib1, vars_Hib2, vars_Hib3,
                               vars_Pneumo1, vars_Pneumo2, vars_Pneumo3,
                               vars_Rota1, vars_Rota2, vars_Rota3),
                   labels = c("bcg", "dtp1", "dtp2", "dtp3", "opv0", "opv1", "opv2", "opv3",
                              "ipv1", "ipv2", "mcv1", "mcv2",
                              "hepB0", "hepB1", "hepB2", "hepB3", "hib1", "hib2", "hib3",
                              "pneumo1", "pneumo2", "pneumo3", "rota1", "rota2", "rota3"))
  
  # List for year information
  
  vars_yea <- list(names = c("BCG_y", "DTP1_y", "DTP2_y", "DTP3_y", "OPV0_y", "OPV1_y", "OPV2_y", "OPV3_y", 
                             "IPV1_y", "IPV2_y", "MCV1_y", "MCV2_y",
                             "HepB0_y", "HepB1_y", "HepB2_y", "HepB3_y", "Hib1_y", "Hib2_y", "Hib3_y",
                             "Pneumo1_y", "Pneumo2_y", "Pneumo3_y", "Rota1_y", "Rota2_y", "Rota3_y"),
                   vars = list(vars_y_BCG, vars_y_DTP1, vars_y_DTP2, vars_y_DTP3,
                               vars_y_OPV0, vars_y_OPV1, vars_y_OPV2, vars_y_OPV3,
                               vars_y_IPV1, vars_y_IPV2,
                               vars_y_MCV1, vars_y_MCV2,
                               vars_y_HepB0, vars_y_HepB1, vars_y_HepB2, vars_y_HepB3,
                               vars_y_Hib1, vars_y_Hib2, vars_y_Hib3,
                               vars_y_Pneumo1, vars_y_Pneumo2, vars_y_Pneumo3,
                               vars_y_Rota1, vars_y_Rota2, vars_y_Rota3),
                   labels = c("bcg_year", "dtp1_year", "dtp2_year", "dtp3_year", "opv0_year", "opv1_year", "opv2_year", "opv3_year",
                              "ipv1_year", "ipv2_year", "mcv1_year", "mcv2_year",
                              "hepB0_year", "hepB1_year", "hepB2_year", "hepB3_year", "hib1_year", "hib2_year", "hib3_year",
                              "pneumo1_year", "pneumo2_year", "pneumo3_year", "rota1_year", "rota2_year", "rota3_year"))
  
  #  List for month information
  
  vars_mon <- list(names = c("BCG_mo", "DTP1_mo", "DTP2_mo", "DTP3_mo", "OPV0_mo", "OPV1_mo", "OPV2_mo", "OPV3_mo", 
                             "IPV1_mo", "IPV2_mo", "MCV1_mo", "MCV2_mo",
                             "HepB0_mo", "HepB1_mo", "HepB2_mo", "HepB3_mo", "Hib1_mo", "Hib2_mo", "Hib3_mo",
                             "Pneumo1_mo", "Pneumo2_mo", "Pneumo3_mo", "Rota1_mo", "Rota2_mo", "Rota3_mo"),
                   vars = list(vars_m_BCG, vars_m_DTP1, vars_m_DTP2, vars_m_DTP3,
                               vars_m_OPV0, vars_m_OPV1, vars_m_OPV2, vars_m_OPV3,
                               vars_m_IPV1, vars_m_IPV2,
                               vars_m_MCV1, vars_m_MCV2,
                               vars_m_HepB0, vars_m_HepB1, vars_m_HepB2, vars_m_HepB3,
                               vars_m_Hib1, vars_m_Hib2, vars_m_Hib3,
                               vars_m_Pneumo1, vars_m_Pneumo2, vars_m_Pneumo3,
                               vars_m_Rota1, vars_m_Rota2, vars_m_Rota3),
                   labels = c("bcg_month", "dtp1_month", "dtp2_month", "dtp3_month", "opv0_month", "opv1_month", "opv2_month", "opv3_month",
                              "ipv1_month", "ipv2_month", "mcv1_month", "mcv2_month",
                              "hepB0_month", "hepB1_month", "hepB2_month", "hepB3_month", "hib1_month", "hib2_month", "hib3_month",
                              "pneumo1_month", "pneumo2_month", "pneumo3_month", "rota1_month", "rota2_month", "rota3_month"))
  
  # List for day information
  
  vars_day <- list(names = c("BCG_d", "DTP1_d", "DTP2_d", "DTP3_d", "OPV0_d", "OPV1_d", "OPV2_d", "OPV3_d", 
                             "IPV1_d", "IPV2_d", "MCV1_d", "MCV2_d",
                             "HepB0_d", "HepB1_d", "HepB2_d", "HepB3_d", "Hib1_d", "Hib2_d", "Hib3_d",
                             "Pneumo1_d", "Pneumo2_d", "Pneumo3_d", "Rota1_d", "Rota2_d", "Rota3_d"),
                   vars = list(vars_d_BCG, vars_d_DTP1, vars_d_DTP2, vars_d_DTP3,
                               vars_d_OPV0, vars_d_OPV1, vars_d_OPV2, vars_d_OPV3,
                               vars_d_IPV1, vars_d_IPV2,
                               vars_d_MCV1, vars_d_MCV2,
                               vars_d_HepB0, vars_d_HepB1, vars_d_HepB2, vars_d_HepB3,
                               vars_d_Hib1, vars_d_Hib2, vars_d_Hib3,
                               vars_d_Pneumo1, vars_d_Pneumo2, vars_d_Pneumo3,
                               vars_d_Rota1, vars_d_Rota2, vars_d_Rota3),
                   labels = c("bcg_day", "dtp1_day", "dtp2_day", "dtp3_day", "opv0_day", "opv1_day", "opv2_day", "opv3_day",
                              "ipv1_day", "ipv2_day", "mcv1_day", "mcv2_day",
                              "hepB0_day", "hepB1_day", "hepB2_day", "hepB3_day", "hib1_day", "hib2_day", "hib3_day",
                              "pneumo1_day", "pneumo2_day", "pneumo3_day", "rota1_day", "rota2_day", "rota3_day"))
  
  
  #### Pooled vars ####
  
  # Create a variable for each vaccine - combi of: Reference vars from above +
  # The information on the standard DHS vars
  
  ##### General #####
  
  for (vaccine in 1:25) {
    
    number_cols <- dim(vaxdata)[2]
    
    if (vars_vax[[1]][vaccine] != "IPV2" & vars_vax[[1]][vaccine] != "IPV3") {
      
      add <- vaxdata %>%
        select(c(vars_vax[[1]][vaccine], all_of(unlist(vars_vax[[2]][vaccine])))) %>%
        as.matrix() %>%
        rowMaxs(na.rm = TRUE)
      
    } else {
      
      add <- vaxdata %>%
        select(c(all_of(unlist(vars_vax[[2]][vaccine])))) %>%
        as.matrix() %>%
        rowMaxs(na.rm = TRUE)
      
    }
    
    add[add == "-Inf"] <- NA
    add[add == "Inf"] <- NA
    
    vaxdata <- cbind(vaxdata, add)
    
    colnames(vaxdata)[number_cols+1] <- paste0(vars_vax[[3]][vaccine])
    
  }
  
  ##### Dates #####
  
  # Year
  
  for (vaccine in 1:25) {
    
    number_cols <- dim(vaxdata)[2]
    
    if (vars_vax[[1]][vaccine] != "IPV2" & vars_vax[[1]][vaccine] != "IPV3") {
      
      add <- vaxdata %>%
        select(c(vars_yea[[1]][vaccine], all_of(unlist(vars_yea[[2]][vaccine])))) %>%
        as.matrix() %>%
        rowMins(na.rm = TRUE)
      
    } else {
      
      add <- vaxdata %>%
        select(c(all_of(unlist(vars_yea[[2]][vaccine])))) %>%
        as.matrix() %>%
        rowMins(na.rm = TRUE)
      
    }
    
    add[add == "-Inf"] <- NA
    add[add == "Inf"] <- NA
    
    vaxdata <- cbind(vaxdata, add)
    
    colnames(vaxdata)[number_cols+1] <- paste0(vars_yea[[3]][vaccine])
    
  }
  
  # Month
  
  for (vaccine in 1:25) {
    
    number_cols <- dim(vaxdata)[2]
    
    if (vars_vax[[1]][vaccine] != "IPV2" & vars_vax[[1]][vaccine] != "IPV3") {
      
      add <- vaxdata %>%
        select(c(vars_mon[[1]][vaccine], all_of(unlist(vars_mon[[2]][vaccine])))) %>%
        as.matrix() %>%
        rowMins(na.rm = TRUE)
      
    } else {
      
      add <- vaxdata %>%
        select(c(all_of(unlist(vars_mon[[2]][vaccine])))) %>%
        as.matrix() %>%
        rowMins(na.rm = TRUE)
      
    }
    
    add[add == "-Inf"] <- NA
    add[add == "Inf"] <- NA
    
    vaxdata <- cbind(vaxdata, add)
    
    colnames(vaxdata)[number_cols+1] <- paste0(vars_mon[[3]][vaccine])
    
  }
  
  # Day
  
  for (vaccine in 1:25) {
    
    number_cols <- dim(vaxdata)[2]
    
    if (vars_vax[[1]][vaccine] != "IPV2" & vars_vax[[1]][vaccine] != "IPV3") {
      
      add <- vaxdata %>%
        select(c(vars_day[[1]][vaccine], all_of(unlist(vars_day[[2]][vaccine])))) %>%
        as.matrix() %>%
        rowMins(na.rm = TRUE)
      
    } else {
      
      add <- vaxdata %>%
        select(c(all_of(unlist(vars_day[[2]][vaccine])))) %>%
        as.matrix() %>%
        rowMins(na.rm = TRUE)
    
    }
    
    add[add == "-Inf"] <- NA
    add[add == "Inf"] <- NA
    
    vaxdata <- cbind(vaxdata, add)
    
    colnames(vaxdata)[number_cols+1] <- paste0(vars_day[[3]][vaccine])
    
  }
  
  rm(number_cols, vaccine, add)
  
  
  #### Clean data ####
  
  # Removing all the original vaccination variables (standard question + extra)
  # Not keeping original DHS vars for this analyses
  # Only the newly made vars that contain information of all DHSnormal+extra vars
  
  # Also reducing a bit the no of columns
  
  vaxdata <- vaxdata %>%
    select("phase", "year", "month", "iso3", "country", "who_region_code",
           "DHSID", "cluster", "hh", "id", "weight",
           "alive", "sex", "dob_month", "dob_year", "dob_day", "age_m", "age_y",
           "health_card", "total_kids", "birth_order",
           "urban", "city", "wealth",
           "mother_education", "mother_literacy", "married", "mother_occupation",
           "husband_edu", "husband_lit", "husband_ocu",
           "bcg", "dtp1", "dtp2", "dtp3", "opv0", "opv1", "opv2", "opv3",
           "ipv1", "ipv2", "mcv1", "mcv2",
           "hepB0", "hepB1", "hepB2", "hepB3", "hib1", "hib2", "hib3",
           "pneumo1", "pneumo2", "pneumo3", "rota1", "rota2", "rota3",
           "bcg_year", "dtp1_year", "dtp2_year", "dtp3_year", "opv0_year", "opv1_year", "opv2_year", "opv3_year",
           "ipv1_year", "ipv2_year", "mcv1_year", "mcv2_year",
           "hepB0_year", "hepB1_year", "hepB2_year", "hepB3_year", "hib1_year", "hib2_year", "hib3_year",
           "pneumo1_year", "pneumo2_year", "pneumo3_year", "rota1_year", "rota2_year", "rota3_year",
           "bcg_month", "dtp1_month", "dtp2_month", "dtp3_month", "opv0_month", "opv1_month", "opv2_month", "opv3_month",
           "ipv1_month", "ipv2_month", "mcv1_month", "mcv2_month",
           "hepB0_month", "hepB1_month", "hepB2_month", "hepB3_month", "hib1_month", "hib2_month", "hib3_month",
           "pneumo1_month", "pneumo2_month", "pneumo3_month", "rota1_month", "rota2_month", "rota3_month",
           "bcg_day", "dtp1_day", "dtp2_day", "dtp3_day", "opv0_day", "opv1_day", "opv2_day", "opv3_day",
           "ipv1_day", "ipv2_day", "mcv1_day", "mcv2_day",
           "hepB0_day", "hepB1_day", "hepB2_day", "hepB3_day", "hib1_day", "hib2_day", "hib3_day",
           "pneumo1_day", "pneumo2_day", "pneumo3_day", "rota1_day", "rota2_day", "rota3_day")
  
  
  
  # EXTRA INDICATORS ---------------------------------------------------
  
  
  # Creating individual-level indicators
  
  
  #### DOB and age at survey ####
  
  # DOB
  
  # To create a date, use 15th of each month as approx when DOB-day is NA
  
  vaxdata <- vaxdata %>%
    mutate(dob_year = case_when(dob_year <= 10 ~ dob_year+2000,
                                dob_year > 10 & dob_year < 1000 ~ dob_year+1900,
                                dob_year >= 1000 & dob_year <= 2022 ~ dob_year,
                                dob_year > 2022 ~ NA_real_)) %>%
    mutate(dob_day = case_when(!is.na(dob_day) ~ dob_day,
                               is.na(dob_day) ~ 15)) %>%
    mutate(dint = make_date(year, month, 15)) %>%
    mutate(dob = make_date(dob_year, dob_month, dob_day))
  
  # Date of interview and dob --> age of kids in months at interview
  
  vaxdata <- vaxdata %>%
    mutate(age_months = round(as.numeric(dint - dob)/30, digits = 0))
  
  # Getting year at which each kid is 12, 24, 36, 48 mos
  
  vaxdata <- vaxdata %>%
    mutate(date_12 = as.Date(365*1 + dob)) %>%
    mutate(date_24 = as.Date(365*2 + dob)) %>%
    mutate(date_36 = as.Date(365*3 + dob)) %>%
    mutate(date_48 = as.Date(365*4 + dob)) %>%
    mutate(year_12 = as.numeric(format(date_12, '%Y'))) %>%
    mutate(year_24 = as.numeric(format(date_24, '%Y'))) %>%
    mutate(year_36 = as.numeric(format(date_36, '%Y'))) %>%
    mutate(year_48 = as.numeric(format(date_48, '%Y')))
  
  
  #### Vaccination age and delay ####
  
  # Lists of vars
  
  vars_list <- list(vax = c("bcg", "dtp1", "dtp2", "dtp3", "opv0", "opv1", "opv2", "opv3",
                            "ipv1", "ipv2", "mcv1", "mcv2",
                            "hepB0", "hepB1", "hepB2", "hepB3", "hib1", "hib2", "hib3",
                            "pneumo1", "pneumo2", "pneumo3", "rota1", "rota2", "rota3"),
                    yea = c("bcg_year", "dtp1_year", "dtp2_year", "dtp3_year", "opv0_year", "opv1_year", "opv2_year", "opv3_year",
                            "ipv1_year", "ipv2_year", "mcv1_year", "mcv2_year",
                            "hepB0_year", "hepB1_year", "hepB2_year", "hepB3_year", "hib1_year", "hib2_year", "hib3_year",
                            "pneumo1_year", "pneumo2_year", "pneumo3_year", "rota1_year", "rota2_year", "rota3_year"),
                    mon = c("bcg_month", "dtp1_month", "dtp2_month", "dtp3_month", "opv0_month", "opv1_month", "opv2_month", "opv3_month",
                            "ipv1_month", "ipv2_month", "mcv1_month", "mcv2_month",
                            "hepB0_month", "hepB1_month", "hepB2_month", "hepB3_month", "hib1_month", "hib2_month", "hib3_month",
                            "pneumo1_month", "pneumo2_month", "pneumo3_month", "rota1_month", "rota2_month", "rota3_month"),
                    day = c("bcg_day", "dtp1_day", "dtp2_day", "dtp3_day", "opv0_day", "opv1_day", "opv2_day", "opv3_day",
                            "ipv1_day", "ipv2_day", "mcv1_day", "mcv2_day",
                            "hepB0_day", "hepB1_day", "hepB2_day", "hepB3_day", "hib1_day", "hib2_day", "hib3_day",
                            "pneumo1_day", "pneumo2_day", "pneumo3_day", "rota1_day", "rota2_day", "rota3_day"),
                    dat = c("date_bcg", "date_dtp1", "date_dtp2", "date_dtp3", "date_opv0", "date_opv1", "date_opv2", "date_opv3",
                            "date_ipv1", "date_ipv2", "date_mcv1", "date_mcv2",
                            "date_hepB0", "date_hepB1", "date_hepB2", "date_hepB3", "date_hib1", "date_hib2", "date_hib3",
                            "date_pneumo1", "date_pneumo2", "date_pneumo3", "date_rota1", "date_rota2", "date_rota3"),
                    age = c("age_bcg", "age_dtp1", "age_dtp2", "age_dtp3", "age_opv0", "age_opv1", "age_opv2", "age_opv3",
                            "age_ipv1", "age_ipv2", "age_mcv1", "age_mcv2",
                            "age_hepB0", "age_hepB1", "age_hepB2", "age_hepB3", "age_hib1", "age_hib2", "age_hib3",
                            "age_pneumo1", "age_pneumo2", "age_pneumo3", "age_rota1", "age_rota2", "age_rota3"),
                    rec = c("rec_bcg", "rec_dtp1", "rec_dtp2", "rec_dtp3", "rec_opv0", "rec_opv1", "rec_opv2", "rec_opv3",
                            "rec_ipv1", "rec_ipv2", "rec_mcv1", "rec_mcv2",
                            "rec_hepB0", "rec_hepB1", "rec_hepB2", "rec_hepB3", "rec_hib1", "rec_hib2", "rec_hib3",
                            "rec_pneumo1", "rec_pneumo2", "rec_pneumo3", "rec_rota1", "rec_rota2", "rec_rota3"),
                    dif = c("delay_bcg", "delay_dtp1", "delay_dtp2", "delay_dtp3", "delay_opv0", "delay_opv1", "delay_opv2", "delay_opv3",
                            "delay_ipv1", "delay_ipv2", "delay_mcv1", "delay_mcv2",
                            "delay_hepB0", "delay_hepB1", "delay_hepB2", "delay_hepB3", "delay_hib1", "delay_hib2", "delay_hib3",
                            "delay_pneumo1", "delay_pneumo2", "delay_pneumo3", "delay_rota1", "delay_rota2", "delay_rota3"),
                    del = c("cat_bcg", "cat_dtp1", "cat_dtp2", "cat_dtp3", "cat_opv0", "cat_opv1", "cat_opv2", "cat_opv3",
                            "cat_ipv1", "cat_ipv2", "cat_mcv1", "cat_mcv2",
                            "cat_hepB0", "cat_hepB1", "cat_hepB2", "cat_hepB3", "cat_hib1", "cat_hib2", "cat_hib3",
                            "cat_pneumo1", "cat_pneumo2", "cat_pneumo3", "cat_rota1", "cat_rota2", "cat_rota3"),
                    iye = c("introyear_bcg", "introyear_dtp1", "introyear_dtp2", "introyear_dtp3", "introyear_opv0", "introyear_opv1", "introyear_opv2", "introyear_opv3",
                            "introyear_ipv1", "introyear_ipv2", "introyear_mcv1", "introyear_mcv2",
                            "introyear_hepB0", "introyear_hepB1", "introyear_hepB2", "introyear_hepB3", "introyear_hib1", "introyear_hib2", "introyear_hib3",
                            "introyear_pneumo1", "introyear_pneumo2", "introyear_pneumo3", "introyear_rota1", "introyear_rota2", "introyear_rota3"),
                    int = c("intro_bcg", "intro_dtp1", "intro_dtp2", "intro_dtp3", "intro_opv0", "intro_opv1", "intro_opv2", "intro_opv3",
                            "intro_ipv1", "intro_ipv2", "intro_mcv1", "intro_mcv2",
                            "intro_hepB0", "intro_hepB1", "intro_hepB2", "intro_hepB3", "intro_hib1", "intro_hib2", "intro_hib3",
                            "intro_pneumo1", "intro_pneumo2", "intro_pneumo3", "intro_rota1", "intro_rota2", "intro_rota3"),
                    buf = c("buffer_bcg", "buffer_dtp1", "buffer_dtp2", "buffer_dtp3", "buffer_opv0", "buffer_opv1", "buffer_opv2", "buffer_opv3",
                            "buffer_ipv1", "buffer_ipv2", "buffer_mcv1", "buffer_mcv2",
                            "buffer_hepB0", "buffer_hepB1", "buffer_hepB2", "buffer_hepB3", "buffer_hib1", "buffer_hib2", "buffer_hib3",
                            "buffer_pneumo1", "buffer_pneumo2", "buffer_pneumo3", "buffer_rota1", "buffer_rota2", "buffer_rota3"))
  
  # Doing this in a loop
  
  for (vaccine in 1:25) {
    
    number_cols <- dim(vaxdata)[2]
    
    # Correcting the year
    
    vaxdata <- vaxdata %>%
      mutate_at(vars_list[[2]][vaccine],
                list( ~ case_when(. <= 22 ~ .+2000,
                                  . > 22 & . < 80 ~ NA_real_,
                                  . >= 80 & . < 1900 ~ .+1900,
                                  . >= 1900 & . <= 2022 ~ .,
                                  . > 2022 ~ NA_real_)))
    
    # Correcting the month
    
    vaxdata <- vaxdata %>%
      mutate_at(vars_list[[3]][vaccine],
                list( ~ case_when(. <= 0 ~ NA_real_,
                                  . > 0 & . <= 12 ~ .,
                                  . > 12 ~ NA_real_)))
    
    # Correcting the day
    
    vaxdata <- vaxdata %>%
      mutate_at(vars_list[[4]][vaccine],
                list( ~ case_when(. <= 0 ~ NA_real_,
                                  . > 0 & . <= 31 ~ .,
                                  . > 31 ~ NA_real_)))
    
    # Calculating date of vaccination and age at vax
    
    date <- make_date(vaxdata[,vars_list[[2]][vaccine]], vaxdata[,vars_list[[3]][vaccine]], vaxdata[,vars_list[[4]][vaccine]])
    age <- round(as.numeric(date - vaxdata$dob)/7, digits = 0) # Calc age, by default in days, and transform to weeks
    age[age < -2] <- NA # If age is "negative" (allow 2 we error), put NA
    
    # Adding to main database
    
    vaxdata <- cbind(vaxdata, date)
    vaxdata <- cbind(vaxdata, age)
    
    # Changing colnames
    
    colnames(vaxdata)[number_cols+1] <- paste0(vars_list[[5]][vaccine])
    colnames(vaxdata)[number_cols+2] <- paste0(vars_list[[6]][vaccine])
    
  }
  
  rm(vaccine, number_cols, age, date)
  
  
  #### Delay calcs ####
  
  vaxdata <- merge(vaxdata, who_data, by = "iso3", all.x = TRUE)
  
  # Calculating vaccination delay in a loop
  # Using the same list of vars
  
  for (vaccine in 1:25) {
    
    number_cols <- dim(vaxdata)[2]
    
    # Calculating difference
    
    delay <- round(as.numeric(vaxdata[,vars_list[[6]][vaccine]] - vaxdata[,vars_list[[7]][vaccine]]), digits = 0)
    
    # Adding to main database
    
    vaxdata <- cbind(vaxdata, delay)
    
    # Creating a categorical var for the delay (account for the possible 2-week error)
    
    vaxdata <- vaxdata %>%
      mutate(cat = case_when(delay < -2 ~ "Early",
                             delay >= -2 & delay <= 2 ~ "At recomm",
                             delay >= 3 & delay <= 4 ~ "1-2 weeks late",
                             delay >= 5 & delay <= 6 ~ "3-4 weeks late",
                             delay >= 7 & delay <= 8 ~ "5-6 weeks late",
                             delay >= 9 & delay <= 10 ~ "7-8 weeks late",
                             delay >= 11 & delay <= 12 ~ "9-10 weeks late",
                             delay >= 13 & delay <= 14 ~ "11-12 weeks late",
                             delay >= 15 & delay <= 26 ~ "13-24 weeks late",
                             delay >= 17 & delay <= 50 ~ "25-48 weeks late",
                             delay > 50 ~ "48+ weeks late"))
    
    # Changing colnames
    
    colnames(vaxdata)[number_cols+1] <- paste0(vars_list[[8]][vaccine])
    colnames(vaxdata)[number_cols+2] <- paste0(vars_list[[9]][vaccine])
    
  }
  
  rm(vaccine, number_cols, delay)
  
  
  #### Intro ####
  
  vaxdata <- merge(vaxdata, who_intro, by = "iso3", all.x = TRUE)
  
  # Introducing year of vaccination
  # and variable for introduced by time of survey or not
  
  for (vaccine in 1:25) {
    
    number_cols <- dim(vaxdata)[2]
    
    # Binary to see if year of introduction is < year of survey (introduced)
    # Let's consider that if the vaccine was introduced on the same
    # year as survey, it wasn't on time
    
    add <- (vaxdata[, vars_list[[10]][vaccine]] < vaxdata[, "year"])
    add[add == TRUE] <- 1
    add[add == FALSE] <- 0
    
    vaxdata <- cbind(vaxdata, add)
    
    # Changing colnames
    
    colnames(vaxdata)[number_cols+1] <- paste0(vars_list[[11]][vaccine])
    
  }
  
  rm(vaccine, number_cols, add)
  
  
  #### Buffer ####
  
  # Introduce a control on time between recommended date (for each kid)
  # and time of survey
  
  # A measure of "buffer" time between ideal vaccination and survey,
  # to see if there has been enough time to measure large delays
  
  # Create a measure of time between
  # (i) DOB+Recommended_age (Recommended_date) and (ii) survey
  
  for (vaccine in 1:25) {
    
    number_cols <- dim(vaxdata)[2]
    
    buffer <- round(as.numeric(vaxdata$dint - (vaxdata$dob+vaxdata[,vars_list[[7]][vaccine]]))/7, digits = 0)
    # Difference between      SURVEY DATE   & IDEAL DATE OF VACCINATION (DOB + RECOMM AGE)
    # NOTE THIS IS COMING UP IN WEEKS
    
    vaxdata <- cbind(vaxdata, buffer)
    
    # Changing colnames
    
    colnames(vaxdata)[number_cols+1] <- paste0(vars_list[[12]][vaccine])
  }
  
  rm(vaccine, number_cols, buffer)
  
  
  #### BackUp ####
  
  assign(paste0("data_", step), vaxdata)
  
}



# MERGE -------------------------------------------------------------------


# Now that we have all the two databases reduced,
# (removed all extra vaccination vars and added information to a single var)
# and with the same columns, APPEND!

if (identical(colnames(data_1), colnames(data_2))) {
  
  vaxdata <- rbind(data_1, data_2)

} else {
  
  cat("REVISE SCRIPT!")
}

# Checks and filters to ensure:
# Alive children

analysis_main <- vaxdata %>%
  
  # Remove surveys below 2010
  filter(year > 2010) %>%
  
  # Remove countries with barely no data (total no << 2,500)
  filter(country != "Albania" & country != "Armenia") %>%
  
  # Remove countries with no vaccination data at all
  filter(country != "Colombia") %>%
  
  # Alive children
  filter(alive == 1)

# Correction

analysis_main$who_region_code [analysis_main$country == "Timor-Leste"] <- "SEARO"
analysis_main$country [analysis_main$country == "Gambia"] <- "The Gambia"

# Save

saveRDS(analysis_main, file = "Data/Analysis_FinalDataset_WideFormat.Rds")


#### Long format ####

# Keeping original vaccination data (status + Y/M/D) in wide,
# but all delay calcs and refernece vars to long

# We have the 743,694 children from the first drafts

nrow(analysis_main)

# Put the data into long format for further better handling

names(analysis_main)

# Do a spec to perform a pivot_longer_spec

spec <- data.frame(.name = sort(c(colnames(analysis_main)[143:342])),
                   .value = c(rep("age_", times = 25), rep("buffer_", times = 25),
                              rep("cat_", times = 25), rep(c("date_"), times = 25),
                              rep(c("delay_"), times = 25), rep("intro_", times = 25),
                              rep("introyear_", times = 25), rep("rec_", times = 25)),
                   vaccine = rep(sort(c("bcg", "dtp1", "dtp2", "dtp3", "opv0", "opv1", "opv2", "opv3",
                                     "ipv1", "ipv2", "mcv1", "mcv2",
                                     "hepB0", "hepB1", "hepB2", "hepB3", "hib1", "hib2", "hib3",
                                     "pneumo1", "pneumo2", "pneumo3", "rota1", "rota2", "rota3")), times = 8))

analysis_main <- analysis_main %>%
  pivot_longer_spec(spec)

# Check new number of rows is:
# 743,694 children from the first drafts x-times-x no of vaccines

nrow(analysis_main)/743694

# Organise and clean (NOTE: eliminate 9 further unneeded cols)

colnames(analysis_main)[143:151] <- c("vaccine", "age_at_vax", "t_buffer_rec_to_survey",
                                      "cat_delay", "date_vax", "delay",
                                      "intro_vax", "introyear_vax", "rec_vax_age")

analysis_main <- analysis_main %>%
  select("phase", "year", "month", "iso3", "country", "who_region_code",
         "DHSID", "cluster", "hh", "id", "weight",
         "alive", "sex", "dob_month", "dob_year", "dob_day", "age_m", "age_months", "age_y",
         "health_card", "total_kids", "birth_order",
         "urban", "city", "wealth",
         "mother_education", "mother_literacy", "married", "mother_occupation",
         "husband_edu", "husband_lit", "husband_ocu",
         "bcg", "dtp1", "dtp2", "dtp3", "opv0", "opv1", "opv2", "opv3",
         "ipv1", "ipv2", "mcv1", "mcv2",
         "hepB0", "hepB1", "hepB2", "hepB3", "hib1", "hib2", "hib3",
         "pneumo1", "pneumo2", "pneumo3", "rota1", "rota2", "rota3",
         "bcg_year", "dtp1_year", "dtp2_year", "dtp3_year", "opv0_year", "opv1_year", "opv2_year", "opv3_year",
         "ipv1_year", "ipv2_year", "mcv1_year", "mcv2_year",
         "hepB0_year", "hepB1_year", "hepB2_year", "hepB3_year", "hib1_year", "hib2_year", "hib3_year",
         "pneumo1_year", "pneumo2_year", "pneumo3_year", "rota1_year", "rota2_year", "rota3_year",
         "bcg_month", "dtp1_month", "dtp2_month", "dtp3_month", "opv0_month", "opv1_month", "opv2_month", "opv3_month",
         "ipv1_month", "ipv2_month", "mcv1_month", "mcv2_month",
         "hepB0_month", "hepB1_month", "hepB2_month", "hepB3_month", "hib1_month", "hib2_month", "hib3_month",
         "pneumo1_month", "pneumo2_month", "pneumo3_month", "rota1_month", "rota2_month", "rota3_month",
         "bcg_day", "dtp1_day", "dtp2_day", "dtp3_day", "opv0_day", "opv1_day", "opv2_day", "opv3_day",
         "ipv1_day", "ipv2_day", "mcv1_day", "mcv2_day",
         "hepB0_day", "hepB1_day", "hepB2_day", "hepB3_day", "hib1_day", "hib2_day", "hib3_day",
         "pneumo1_day", "pneumo2_day", "pneumo3_day", "rota1_day", "rota2_day", "rota3_day",
         "dint", "dob", "vaccine", "date_vax", "age_at_vax",
         "rec_vax_age", "delay", "cat_delay", "t_buffer_rec_to_survey",
         "intro_vax", "introyear_vax")

# Save

saveRDS(analysis_main, file = "Data/Analysis_FinalDataset_IndividualData.Rds")
