#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Timeliness of 24 childhood immunisations and evolution of vaccination delay:#
#-#-#-#-#- analysis of data from 54 low- and middle-income countries -#-#-#-#-#


# 7/ Association between delay and course completeness



# SET UP ------------------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(rcartocolor)
library(ggpubr)

# Create folder for results

dir.create("Results_VaxCourseCompleteness")



# DATA -----------------------------------------------------------------


#### Buffer ####

# How much time are we allowing between recomm dtae of vax and survey?

bufflist_cutoff <- c(52, 104)
bufflist_folders <- c("Buffer 1-year RecoVax-Survey",
                      "Buffer 2-year RecoVax-Survey")

buff_index <- 1L

buffer <- bufflist_cutoff[buff_index] 
buffer_folder <- bufflist_folders[buff_index]

# Create a subfolder for this

dir.create(paste0("Results_VaxCourseCompleteness/", buffer_folder))


#### Variables ####

list_firstvax <- c("dtp1", "hepB1", "hib1", "mcv1", "opv1", "pneumo1", "rota1")
list_firstnames <- c("DTP-D1", "HepB-D1", "Hib-D1", "MCV-D1", "OPV-D1", "PCV-D1", "RV-D1")

list_lastvax2 <- c("dtp2", "hepB2", "hib2", "mcv2", "opv2", "pneumo2", "rota2")
list_lastvax3 <- c("dtp3", "hepB3", "hib3", "mcv2", "opv3", "pneumo3", "rota3")
list_lastnames2 <- c("DTP-D2", "HepB-D2", "Hib-D2", "MCV-D2", "OPV-D2", "PCV-D2", "RV-D2")
list_lastnames3 <- c("DTP-D3", "HepB-D3", "Hib-D3", "MCV-D2", "OPV-D3", "PCV-D3", "RV-D3")

list_data <- list()


#### Data ####

# Get the data in (semi)long format:
# Completeness is wide and delay is long

analysis_main <- readRDS("Data/Analysis_FinalDataset_IndividualData.Rds")

## Subset to analyse delay vs completeness in each pair

for (vax in 1:length(list_firstvax)) {
  
  add <- analysis_main %>%
    # Filter to our delay var of interest (first dose) %>%
    filter(vaccine == list_firstvax[vax]) %>%
    # Filter according to buffer
    filter(t_buffer_rec_to_survey >= buffer) %>%
    # Vars of interest %>%
    select(who_region_code, iso3, country, dob_year,
           sex, health_card, urban, city, wealth,
           mother_education, mother_literacy, married, mother_occupation,
           husband_edu, husband_lit, husband_ocu,
           dtp1, dtp2, dtp3, hepB1, hepB2, hepB3, hib1, hib2, hib3, mcv1, mcv2,
           opv1, opv2, opv3, pneumo1, pneumo2, pneumo3, rota1, rota2, rota3,
           vaccine, delay)
  
  list_data[[vax]] <- add
  
  rm(add)
  
}

# Clean space

rm(vax)



# LOGIT MODELS ------------------------------------------------------------


#### Variables ####

list_firstvax <- c("dtp1", "hepB1", "hib1", "mcv1", "opv1", "pneumo1", "rota1",
                   "dtp1", "hepB1", "hib1", "opv1", "pneumo1", "rota1")
list_firstnames <- c("DTP-D1", "HepB-D1", "Hib-D1", "MCV-D1", "OPV-D1", "PCV-D1", "RV-D1",
                     "DTP-D1", "HepB-D1", "Hib-D1", "OPV-D1", "PCV-D1", "RV-D1")

list_lastvax <- c("dtp2", "hepB2", "hib2", "mcv2", "opv2", "pneumo2", "rota2",
                  "dtp3", "hepB3", "hib3", "opv3", "pneumo3", "rota3")
list_lastnames <- c("DTP-D2", "HepB-D2", "Hib-D2", "MCV-D2", "OPV-D2", "PCV-D2", "RV-D2",
                     "DTP-D3", "HepB-D3", "Hib-D3", "OPV-D3", "PCV-D3", "RV-D3")


#### Univariable ####

##### Normal loop #####

list_data[c(8:13)] <- list_data[c(1:3, 5:7)]

result <- data.frame()

for (vax in 1:length(list_firstvax)) {
  
  # Run the logit
  
  logit <- glm(list_data[[vax]][[list_lastvax[vax]]] ~ delay, data = list_data[[vax]],
               family = "binomial"(link = "logit"))
  
  # Store results
  
  summary <- cbind(regress = paste0(list_firstvax[vax], "_", list_lastvax[vax]),
                   delay = paste0(list_firstnames[vax]),
                   completeness = paste0(list_lastnames[vax]),
                   intercept = coef(logit)[1],
                   slope = coef(logit)[2],
                   or = round(exp(coef(logit))[2], digits = 4),
                   ci95low = round(exp(confint(logit))[2,1], digits = 4),
                   ci95up = round(exp(confint(logit))[2,2], digits = 4),
                   aic = logit$aic,
                   null_dev = logit$null.deviance,
                   res_dev = logit$deviance)
  
  # Add
  
  result <- rbind(result, summary)
  
}

rm(vax, logit, summary)

# Save and clean

result <- result %>%
  mutate(or = as.numeric(or)) %>% mutate(ci95low = as.numeric(ci95low)) %>% mutate(ci95up = as.numeric(ci95up)) %>%
  mutate(OR_CI = paste0(round(or, digits = 3), " (", round(ci95low, digits = 3), ", ", round(ci95up, digits = 3), ")")) %>%
  select(delay, completeness, OR_CI)

write.csv(result,row.names = FALSE,
          file = paste0("Results_VaxCourseCompleteness/", buffer_folder, "/Table1_DelayCompleteness_Assoc.csv"))
