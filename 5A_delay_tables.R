#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Timeliness of 24 childhood immunisations and evolution of vaccination delay:#
#-#-#-#-#- analysis of data from 54 low- and middle-income countries -#-#-#-#-#


# 5/ Vaccination delay



# SET UP ------------------------------------------------------------------


rm(list = ls())

library(tidyverse)

# Results folder

dir.create("Results_DelayVaccination")



# DATA -----------------------------------------------------------------


#### Buffer ####

# How much time are we allowing between recomm date of vax and survey?

bufflist_cutoff <- c(52, 104)
bufflist_folders <- c("Buffer Time 1-year Threshold",
                      "Buffer Time 2-year Threshold")

buff_index <- 1L

buffer <- bufflist_cutoff[buff_index] 
buffer_folder <- bufflist_folders[buff_index]

# Create a subfolder for this

dir.create(paste0("Results_DelayVaccination/", buffer_folder))
dir.create(paste0("Results_DelayVaccination/", buffer_folder, "/Tables"))


#### Data ####

analysis_main <- readRDS("Data/Analysis_FinalDataset_IndividualData.Rds")


#### Variables ####

vaccines <- c("bcg", "dtp1", "dtp2", "dtp3", "opv0", "opv1", "opv2", "opv3",
              "ipv1", "mcv1", "mcv2",
              "hepB0", "hepB1", "hepB2", "hepB3", "hib1", "hib2", "hib3",
              "pneumo1", "pneumo2", "pneumo3", "rota1", "rota2", "rota3")

titles <- c("BCG", "DTP-D1", "DTP-D2", "DTP-D3", "OPV-BD", "OPV-D1", "OPV-D2", "OPV-D3",
            "IPV-D1", "MCV-D1", "MCV-D2",
            "HepB-BD", "HepB-D1", "HepB-D2", "HepB-D3", "Hib-D1", "Hib-D2", "Hib-D3",
            "PCV-D1", "PCV-D2", "PCV-D3", "RV-D1", "RV-D2", "RV-D3")



# TABLES TOTAL -----------------------------------------------------------


#### All timepoints ####

# Vector to store results for all vaccines

delay_result <- data.frame()

# Loop through vaccines

for (vaccine in 1:length(vaccines)) {
  
  print(vaccines[vaccine])
  
  ## Prepare the data
  
  # Filter data for each vaccine (count each child once)
  
  data_subset <- analysis_main[analysis_main[["vaccine"]] == vaccines[vaccine] & analysis_main[["t_buffer_rec_to_survey"]] >= buffer,]
  
  # Only want the vector of each vaccine's delay 
  
  data_subset <- data_subset[["delay"]]
  
  data_subset <- data_subset[!is.na(data_subset)]
  
  # Get some basic stats
  
  median <- round(as.numeric(median(data_subset)), digits = 1)
  
  iqr <- paste0("(", summary(data_subset)[2], ", ", summary(data_subset)[5], ")")
  
  mean <- round(as.numeric(mean(data_subset)), digits = 1)
  
  sd <- paste0("(", round(as.numeric(sd(data_subset)), digits = 2), ")")
  
  range <- paste0("(", summary(data_subset)[1], ", ", summary(data_subset)[6], ")")
  
  quan_01 <- round(as.numeric(quantile(data_subset, probs = 0.01)), digits = 1)
  
  quan_05 <- round(as.numeric(quantile(data_subset, probs = 0.05)), digits = 1)
  
  quan_15 <- round(as.numeric(quantile(data_subset, probs = 0.15)), digits = 1)
  
  quan_25 <- round(as.numeric(quantile(data_subset, probs = 0.25)), digits = 1)
  
  quan_50 <- round(as.numeric(quantile(data_subset, probs = 0.50)), digits = 1)
  
  quan_75 <- round(as.numeric(quantile(data_subset, probs = 0.75)), digits = 1)
  
  quan_85 <- round(as.numeric(quantile(data_subset, probs = 0.85)), digits = 1)
  
  quan_95 <- round(as.numeric(quantile(data_subset, probs = 0.95)), digits = 1)
  
  quan_99 <- round(as.numeric(quantile(data_subset, probs = 0.99)), digits = 1)
  
  ## Store results
  
  delay_vaccine <- data.frame(country =  "All",
                              vaccine = titles[vaccine],
                              median, iqr, mean, sd, range,
                              quan_01, quan_05, quan_15, quan_25, quan_50,
                              quan_75, quan_85, quan_95, quan_99)
  
  # Add to previous results with all vaccines
  
  delay_result <- rbind(delay_result, delay_vaccine)
  
}

write.csv(delay_result,
          file = paste0("Results_DelayVaccination/", buffer_folder, "/Tables/All_delay_vaccination.csv"),
          row.names = FALSE)


#### Birth cohort ####

# Vector to store results for all vaccines

delay_result <- data.frame()

# Get the birth cohorts

cohorts <- sort(unique(analysis_main$dob_year))

# Loop through vaccines

for (vaccine in 1:length(vaccines)) {
  
  print(vaccines[vaccine])
  
  # Vector to store results of that vaccine
  
  delay_vaccine <- data.frame()
  
  # Loop through birth cohorts
  
  for (year in 1:length(cohorts)) {
    
    ## Prepare the data
    
    # Filter data for each vaccine (count each child once)
    
    data_subset <- analysis_main[analysis_main[["vaccine"]] == vaccines[vaccine] & analysis_main[["t_buffer_rec_to_survey"]] >= buffer,]
    
    # Subset to that birth cohort
    
    data_subset <- data_subset[data_subset[["dob_year"]] == cohorts[year],]
    
    # Only want the vector of each vaccine's delay 
    
    data_subset <- data_subset[["delay"]]
    
    data_subset <- data_subset[!is.na(data_subset)]
    
    # Get some basic stats
    
    median <- round(as.numeric(median(data_subset)), digits = 1)
    
    iqr <- paste0("(", summary(data_subset)[2], ", ", summary(data_subset)[5], ")")
    
    mean <- round(as.numeric(mean(data_subset)), digits = 1)
    
    sd <- paste0("(", round(as.numeric(sd(data_subset)), digits = 2), ")")
    
    range <- paste0("(", summary(data_subset)[1], ", ", summary(data_subset)[6], ")")
    
    quan_01 <- round(as.numeric(quantile(data_subset, probs = 0.01)), digits = 1)
    
    quan_05 <- round(as.numeric(quantile(data_subset, probs = 0.05)), digits = 1)
    
    quan_15 <- round(as.numeric(quantile(data_subset, probs = 0.15)), digits = 1)
    
    quan_25 <- round(as.numeric(quantile(data_subset, probs = 0.25)), digits = 1)
    
    quan_50 <- round(as.numeric(quantile(data_subset, probs = 0.50)), digits = 1)
    
    quan_75 <- round(as.numeric(quantile(data_subset, probs = 0.75)), digits = 1)
    
    quan_85 <- round(as.numeric(quantile(data_subset, probs = 0.85)), digits = 1)
    
    quan_95 <- round(as.numeric(quantile(data_subset, probs = 0.95)), digits = 1)
    
    quan_99 <- round(as.numeric(quantile(data_subset, probs = 0.99)), digits = 1)
    
    ## Store results
    
    delay_birth <- data.frame(country =  "All",
                              vaccine = titles[vaccine],
                              birth_cohort = cohorts[year],
                              median, iqr, mean, sd, range,
                              quan_01, quan_05, quan_15, quan_25, quan_50,
                              quan_75, quan_85, quan_95, quan_99)
    
    # Add to previous results with all birth cohorts
    
    delay_vaccine <- rbind(delay_vaccine, delay_birth)
    
  }
  
  # Add to previous results with all vaccines
  
  delay_result <- rbind(delay_result, delay_vaccine)
}

write.csv(delay_result,
          file = paste0("Results_DelayVaccination/", buffer_folder, "/Tables/Birth_cohort_delay_vaccination.csv"),
          row.names = FALSE)



# TABLES COUNTRY ----------------------------------------------------------


#### All timepoints ####

# Vector to store results for all vaccines and countries

delay_result <- data.frame()

# Get the countries

countries <- sort(unique(analysis_main$country))

# Loop through countries

for (country in 1:length(countries)) {
  
  print(countries[country])
  
  # Vector to store results from that country
  
  delay_country <- data.frame()
  
  # Loop through vaccines
  
  for (vaccine in 1:length(vaccines)) {
    
    print(vaccines[vaccine])
    
    ## Prepare the data
    
    # Filter data for each vaccine (count each child once)
    
    data_subset <- analysis_main[analysis_main[["vaccine"]] == vaccines[vaccine] & analysis_main[["t_buffer_rec_to_survey"]] >= buffer,]
    
    # Subset to our country
    
    data_subset <- data_subset[data_subset[["country"]] == countries[country],]
    
    # Only want the vector of each vaccine's delay 
    
    data_subset <- data_subset[["delay"]]
    
    data_subset <- data_subset[!is.na(data_subset)]
    
    # Get some basic stats
    
    median <- round(as.numeric(median(data_subset)), digits = 1)
    
    iqr <- paste0("(", summary(data_subset)[2], ", ", summary(data_subset)[5], ")")
    
    mean <- round(as.numeric(mean(data_subset)), digits = 1)
    
    sd <- paste0("(", round(as.numeric(sd(data_subset)), digits = 2), ")")
    
    range <- paste0("(", summary(data_subset)[1], ", ", summary(data_subset)[6], ")")
    
    quan_01 <- round(as.numeric(quantile(data_subset, probs = 0.01)), digits = 1)
    
    quan_05 <- round(as.numeric(quantile(data_subset, probs = 0.05)), digits = 1)
    
    quan_15 <- round(as.numeric(quantile(data_subset, probs = 0.15)), digits = 1)
    
    quan_25 <- round(as.numeric(quantile(data_subset, probs = 0.25)), digits = 1)
    
    quan_50 <- round(as.numeric(quantile(data_subset, probs = 0.50)), digits = 1)
    
    quan_75 <- round(as.numeric(quantile(data_subset, probs = 0.75)), digits = 1)
    
    quan_85 <- round(as.numeric(quantile(data_subset, probs = 0.85)), digits = 1)
    
    quan_95 <- round(as.numeric(quantile(data_subset, probs = 0.95)), digits = 1)
    
    quan_99 <- round(as.numeric(quantile(data_subset, probs = 0.99)), digits = 1)
    
    ## Store results
    
    delay_vaccine <- data.frame(country =  countries[country],
                                vaccine = titles[vaccine],
                                median, iqr, mean, sd, range,
                                quan_01, quan_05, quan_15, quan_25, quan_50,
                                quan_75, quan_85, quan_95, quan_99)
    
    # Add to previous results in country with all vaccines
    
    delay_country <- rbind(delay_country, delay_vaccine)
    
  }
  
  # Add to results of all countries
  
  delay_result <- rbind(delay_result, delay_country)
}

write.csv(delay_result,
          file = paste0("Results_DelayVaccination/", buffer_folder, "/Tables/Country_delay_vaccination.csv"),
          row.names = FALSE)

