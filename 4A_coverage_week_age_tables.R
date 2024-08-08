#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Timeliness of 24 childhood immunisations and evolution of vaccination delay:#
#-#-#-#-#- analysis of data from 54 low- and middle-income countries -#-#-#-#-#


# 4/ Coverage per week of age



# SET UP ------------------------------------------------------------------


rm(list = ls())

library(tidyverse)

# Folder for results

dir.create("Results_CoverageWeekAge")



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

dir.create(paste0("Results_CoverageWeekAge/", buffer_folder))
dir.create(paste0("Results_CoverageWeekAge/", buffer_folder, "/Tables"))


#### Data ####

## Individual data

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

survival_result <- data.frame()

# Loop through vaccines

for (vaccine in 1:length(vaccines)) {
  
  
  ## Prepare the data
  
  # Filter data for each vaccine (count each child once)
  
  data_subset <- analysis_main[analysis_main[["vaccine"]] == vaccines[vaccine] & analysis_main[["t_buffer_rec_to_survey"]] >= buffer,]
  
  # Only want the vector of each vaccine's age
  # We know not everyone has vaccine age, but only focus on those who do
  
  data_subset <- data_subset[["age_at_vax"]]
  
  data_subset <- data_subset[!is.na(data_subset)]
  
  # Make sure no ages are below -2 w and chose a top week (5 years)
  
  data_subset <- data_subset[data_subset >= -2 & data_subset <= 260]
  
  
  ## Cumulative proportion of children vaccinated with the vaccine
  
  # Get coverage for that vaccine
  
  coverage <- read.csv("Demographics/Basic tables/no_children_all.csv")
  
  if (buffer == 52) {
    coverage <- coverage[coverage$vaccine == titles[vaccine], "prop_vax1"] %>% as.numeric()}
  if (buffer == 104) {
    coverage <- coverage[coverage$vaccine == titles[vaccine], "prop_vax2"] %>% as.numeric()}
  
  # Loop for every week
  
  loop_result <- data.frame(start = 0)
  
  for (week in seq(260)) {
    
    x <- (data_subset <= week)
    
    y <- round(length(which(x == TRUE))/length(x), digits = 4)
    
    y <- y*coverage # Cum proportion at that age times the coverage
    
    loop_result <- cbind(loop_result, y)
    
    colnames(loop_result)[1+week] <- paste0("vax_week_", week)
  }
  
  rm(x, y, week)
  
  # Remove starter col
  
  loop_result <- loop_result[, -1]
  
  
  ## Put results with names
  
  survival_result_vaccine <- data.frame(country = "All",
                                        vaccine = titles[vaccine],
                                        loop_result)
  
  
  ## Add to previous results
  
  survival_result <- rbind(survival_result, survival_result_vaccine)
}

rm(data_subset, loop_result, survival_result_vaccine, vaccine)

# Save

write.csv(survival_result,
          file = paste0("Results_CoverageWeekAge/", buffer_folder, "/Tables/All_cum_vaccination.csv"),
          row.names = FALSE)



# TABLES COUNTRY ----------------------------------------------------------


#### All timepoints ####

# Vector to store results for all vaccines and countries

survival_result <- data.frame()

# Getting the countries

countries <- sort(unique(analysis_main$country))

# Loop through countries

for (country in 1:length(countries)) {
  
  # Vector to store results from country
  
  survival_result_country <- data.frame()
  
  # Get the region
  
  who_region <- unique(analysis_main[["who_region_code"]][analysis_main[["country"]] == countries[country]])
  who_region <- who_region[!is.na(who_region)]
  
  # Loop through vaccines
  
  for (vaccine in 1:length(vaccines)) {
    
    
    ## Prepare the data
    
    # Filter data for each vaccine (count each child once)
    
    data_subset <- analysis_main[analysis_main[["vaccine"]] == vaccines[vaccine] & analysis_main[["t_buffer_rec_to_survey"]] >= buffer,]
    
    # Subset to our country of interest
    
    data_subset <- data_subset[data_subset[["country"]] == countries[country],]
    
    # Only want the vector of each vaccine's age
    # We know not everyone has vaccine age, but only focus on those who do
    
    data_subset <- data_subset[["age_at_vax"]]
    
    data_subset <- data_subset[!is.na(data_subset)]
    
    
    ## Cumulative proportion of children vaccinated with the vaccine
    
    # Get coverage for that vaccine
    
    coverage <- read.csv("Demographics/Basic tables/no_children_country.csv")
    
    coverage <- coverage[coverage$country == countries[country], ]
    
    if (buffer == 52) {
      coverage <- coverage[coverage$vaccine == titles[vaccine], "prop_vax1"] %>% as.numeric()}
    if (buffer == 104) {
      coverage <- coverage[coverage$vaccine == titles[vaccine], "prop_vax2"] %>% as.numeric()}
    
    # Loop for every week
    
    loop_result <- data.frame(start = 0)
    
    for (week in seq(260)) {
      
      x <- (data_subset <= week)
      
      y <- round(length(which(x == TRUE))/length(x), digits = 4)
      
      y <- y*coverage # Cum proportion at that age times the coverage
      
      loop_result <- cbind(loop_result, y)
      
      colnames(loop_result)[1+week] <- paste0("vax_week_", week)
    }
    
    rm(x, y, week)
    
    # Remove starter col
    
    loop_result <- loop_result[, -1]
    
    
    ## Put results with names
    
    survival_result_vaccine <- data.frame(country = countries[country],
                                          who_region = who_region,
                                          vaccine = titles[vaccine],
                                          loop_result)
    
    
    ## Add to previous results of that country
    
    survival_result_country <- rbind(survival_result_country, survival_result_vaccine)
  }
  
  
  ## Add to all countries' results
  
  survival_result <- rbind(survival_result, survival_result_country)
}


rm(data_subset, loop_result,
   survival_result_country, survival_result_vaccine, vaccine, country)

# Save

write.csv(survival_result,
          file = paste0("Results_CoverageWeekAge/", buffer_folder, "/Tables/Country_cum_vaccination.csv"),
          row.names = FALSE)