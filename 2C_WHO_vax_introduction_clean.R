#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Timeliness of 24 childhood immunisations and evolution of vaccination delay:#
#-#-#-#-#- analysis of data from 54 low- and middle-income countries -#-#-#-#-#


# 2/ Clean raw data: Countries' year of vaccination introduction (WHO source)



# SET UP ------------------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(readxl)


# WHO data -----------------------------------------------------------------


# Getting the timeline of introduction
# Data source: WHO / UNICEF immunization portal

# Unlike the schedules, all the data sets seem to be organised and
# to be cleaned the same way

# So do in a loop

# Lists of variables and data sets to clean

data_list <- c(
  "C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Introduction/Introduction of Hepatitis B vaccine 2024-07-02 11-26 UTC.xlsx",
  "C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Introduction/Introduction of HepB birth dose 2024-07-02 11-26 UTC.xlsx",
  "C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Introduction/Introduction of Hib vaccine 2024-07-02 11-26 UTC.xlsx",
  "C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Introduction/Introduction of IPV (Inactivated polio vaccine) 2nd dose 2024-07-02 11-27 UTC.xlsx",
  "C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Introduction/Introduction of IPV (Inactivated polio vaccine) 2024-07-02 11-27 UTC.xlsx",
  "C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Introduction/Introduction of Measles-containing vaccine 2nd dose 2024-07-02 11-27 UTC.xlsx",
  "C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Introduction/Introduction of PCV (Pneumococcal conjugate vaccine) 2024-07-02 11-27 UTC.xlsx",
  # "C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Introduction/Introduction of PPV (Pneumococcal polysaccharide vaccine) 2024-07-02 11-27 UTC.xlsx",
  "C:/Users/nd1316/OneDrive - Imperial College London/PhD/Projects/Data/WHO-UNICEF/Introduction/Introduction of Rotavirus vaccine 2024-07-02 11-28 UTC.xlsx")

vaccines <- c("HepB", "HepB-BD", "Hib", "IPV-D2", "IPV-D1",
              "MCV-D2", "PCV", "RV")

# Vector to store results

data_all <- data.frame()


#### Loop ####

for (v in 1:length(vaccines)) {
  
  data <- read_excel(paste0(data_list[v]))
  
  clean <- data %>%
    
    # We want the earliest year in which vaccine was introduced ("Yes" on INTRO)
    filter(INTRO == "Yes") %>%
    group_by(COUNTRYNAME) %>%
    mutate(year_intro = min(YEAR)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    
    # Add vaccine and select cols of interest
    mutate(vaccine = vaccines[v]) %>%
    select(ISO_3_CODE, COUNTRYNAME, WHO_REGION, vaccine, year_intro)
  
  data_all<- rbind(data_all, clean)
  
}

rm(v, data, clean)

colnames(data_all) <- c("iso3", "country", "who_region_code", "vaccine", "intro_year")


#### Save ####

write.csv(data_all, file = "Data/WHO_vaccination_introduction_reference.csv", row.names = FALSE)

