#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Timeliness of 24 childhood immunisations and evolution of vaccination delay:#
#-#-#-#-#- analysis of data from 54 low- and middle-income countries -#-#-#-#-#


# 6/ Survival analysis



# SET UP ------------------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(survival)
library(rcartocolor)
library(ggpubr)
library(patchwork)

# Create folder for results

dir.create("Results_SurvivalAnalysis")



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

dir.create(paste0("Results_SurvivalAnalysis/", buffer_folder))


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



# SURVIVAL ANALYSIS -------------------------------------------------------


#### Data loop ####

list_data <- list()

for (vax in 1:length(vaccines)) {
  
  # First, focus on the vaccine and the buffer
  
  data <- analysis_main %>%
    # Filter to our delay var of interest %>%
    filter(vaccine == vaccines[vax]) %>%
    # Filter according to buffer
    filter(t_buffer_rec_to_survey >= buffer)
  
  # Then, create a var for vax status named all the same for all vaccines
  
  data$event <- data[[vaccines[vax]]]
  
  # Final, prepare data for survival analysis
  
  # Having our vaccine or not is our event (although here event is good)
  # Typically: 1 = event / 0 = censored
  
  # Date of outcome is date of vaccination
  # Date of start is DOB+REC-VAX-AGE (ideal date of vaccination)
  # Survival time is vaccination delay
  # Need some censoring time for unvaccinated children: put 5 years
  
  data <- data %>%
    # Not interested in ppl with no vax data
    filter(!is.na(event)) %>%
      # Get the theoretical recomm date of vax for each child (just as FYI)
      mutate(rec_date = dob+rec_vax_age*7) %>%
      # Create a censoring time for unvaccinated (max 5 years)
      mutate(time = case_when(event == 0 ~ 52*5,
                              event == 1 ~ delay)) %>%
      # Remove vaccinated children with no date data (no time var)
      filter(!is.na(time)) %>%
      # Select vars of interest
      select(who_region_code, iso3, country,
             year, month, dob_year, dob_month, dob,
             sex, health_card, urban, city, wealth,
             mother_education, mother_literacy, married, mother_occupation,
             husband_edu, husband_lit, husband_ocu,
             event, rec_date, date_vax, delay, time)
  
  list_data[[vax]] <- data
  
  rm(data)
}


#### Multivariable Cox ####

risk_factors <- c("sex", "urban", "wealth", "mother_education",
                  "married", "husband_ocu")

risk_ids <- c("Sex at birth", "Urban/Rural", "Wealth Quintile",
              "Mother's Education", "Mother's Marital Status", "Husband's Occupation")

risk_levels <- list(c("1", "2"),c("1", "2"), c("1", "2", "3", "4", "5"),
                    c("0", "1", "2", "3"), c("0", "1", "2"),
                    c("0", "1", "2", "3", "4"))

risk_names <- list(c("Male", "Female"), c("Urban", "Rural"),
                   c("Poorest", "Poor", "Middle", "Rich", "Richest"),
                   c("None", "Primary", "Secondary", "Higher"),
                   c("Not married", "Married/Living together",
                     "Widowed/Divorced/Not living together"),
                   c("No work", "Prof/Manager/Clerical/Sales",
                     "Agricultural", "Household", "Manual"))

## Nothing seems to be very strongly correlated
# In some vaccines, wealth and urban/rural get close to 0.5,
# but this is only in some and very little over 0.5

library(riskRegression)
library(StepReg)

# Create table to store results from model selection

coxmodel_selection1 <- as.data.frame(matrix(NA, nrow = length(vaccines),
                                            ncol = length(risk_factors)))
coxmodel_selection2 <- as.data.frame(matrix(NA, nrow = length(vaccines),
                                            ncol = length(risk_factors)))
colnames(coxmodel_selection1) <- risk_factors
colnames(coxmodel_selection2) <- risk_factors

## AUTOMATIC SELECTION

for (vax in 1:length(vaccines)) {
  
  print(titles[vax])
  
  # Get data for that vaccine
  
  surv_data <- list_data[[vax]]
  
  # Stepwise regression: OPTION #1 (selectCox is BACKWARDS)
  
  model <- selectCox(Surv(time, event) ~ sex + urban + wealth + mother_education +
                       married + husband_ocu, data = surv_data, rule = "aic")
  vars1 <- sort(model[[2]])
  print(vars1)
  
  # Stepwise regression: OPTION #2 (stepwiseCox here is FORWARD)
  
  surv_data <- list_data[[vax]] %>%
    mutate_at(risk_factors, as.numeric) %>%
    filter_at(all_of(risk_factors), all_vars(!is.na(.)))
  
  model <- stepwiseCox(Surv(time, event) ~ sex + urban + wealth + mother_education +
                         married + husband_ocu, data = surv_data,
                       selection = "forward", select = "AIC")
  vars2 <- sort(as.character(model[[4]]))
  print(vars2)
  
  # Signal if var was picked up
  
  for (risk in 1:length(risk_factors)) {
    
    if (colnames(coxmodel_selection1)[risk] %in% vars1) {
      coxmodel_selection1[vax, risk] <- "x"
    }
    
    if (colnames(coxmodel_selection2)[risk] %in% vars2) {
      coxmodel_selection2[vax, risk] <- "x"
    }
  }
  
}

# Create a final table

coxmodel_selection <- cbind(as.data.frame(titles),
                            coxmodel_selection1, coxmodel_selection2)
colnames(coxmodel_selection) <- c("Vaccine", rep(risk_ids, times = 2))

write.csv(coxmodel_selection, file = paste0("Results_SurvivalAnalysis/", buffer_folder, "/SuppTable6_CoxModelSelection.csv"), row.names = FALSE)

## Clean

rm(vax, surv_data, model, vars1, vars2, risk, coxmodel_selection1, coxmodel_selection2)


#### Final Cox ####

risk_factors <- c("sex", "urban", "wealth", "mother_education",
                  "married", "husband_ocu")

risk_ids <- c("Sex at birth", "Urban/Rural", "Wealth Quintile",
              "Mother's Education", "Mother's Marital Status", "Husband's Occupation")

risk_levels <- list(c("1", "2"),c("1", "2"), c("1", "2", "3", "4", "5"),
                    c("0", "1", "2", "3"), c("0", "1", "2"),
                    c("0", "1", "2", "3", "4"))

risk_names <- list(c("Male", "Female"), c("Urban", "Rural"),
                   c("Poorest", "Poor", "Middle", "Rich", "Richest"),
                   c("None", "Primary", "Secondary", "Higher"),
                   c("Not married", "Married/Living together",
                     "Widowed/Divorced/Not living together"),
                   c("No work", "Prof/Manager/Clerical/Sales",
                     "Agricultural", "Household", "Manual"))

# Including all factors in one

library(coxme)

all_results <- data.frame()

for (vax in 1:length(vaccines)) {
  
  print(vaccines[vax])
  
  # Get data for that vaccine
  
  surv_data <- list_data[[vax]]
  
  # Model
  
  model <- coxme(Surv(time, event) ~ as.factor(sex) + as.factor(urban) + as.factor(wealth) +
                   as.factor(mother_education) + as.factor(married) + as.factor(husband_ocu) +
                   (1 | country),
                 data = surv_data)
  
  # Extract results
  
  coefs <- as.data.frame(coef(model))
  coef_exp <- as.data.frame(exp(coef(model)))
  ci95_exp <- as.data.frame(exp(confint(model)))
  
  # Append all the results
  
  vaccine <- rep(titles[vax], times = 21)
  
  var <- vector()
  for (risk in 1:length(risk_factors)) {
    var <- c(var, rep(risk_ids[risk], times = length(risk_levels[[risk]])))
  }
  
  char <- vector()
  for (risk in 1:length(risk_factors)) {
    char <- c(char, risk_names[[risk]])
  }
  
  coef <- c(NA, coefs[1, 1], NA, coefs[2, 1],
            NA, coefs[3:6, 1], NA, coefs[7:9, 1],
            NA, coefs[10:11, 1], NA, coefs[12:15, 1])
  
  hr <- c(1, coef_exp[1, 1], 1, coef_exp[2, 1],
          1, coef_exp[3:6, 1], 1, coef_exp[7:9, 1],
          1, coef_exp[10:11, 1], 1, coef_exp[12:15, 1])
  
  ci_low <- c(1, ci95_exp[1, 1], 1, ci95_exp[2, 1],
              1, ci95_exp[3:6, 1], 1, ci95_exp[7:9, 1],
              1, ci95_exp[10:11, 1], 1, ci95_exp[12:15, 1])
  
  ci_upp <- c(1, ci95_exp[1, 2], 1, ci95_exp[2, 2],
              1, ci95_exp[3:6, 2], 1, ci95_exp[7:9, 2],
              1, ci95_exp[10:11, 2], 1, ci95_exp[12:15, 2])
  
  summary <- cbind(vaccine, var, char, coef, hr, ci_low, ci_upp)
  rm(vaccine, var, char, coef, hr, ci_low, ci_upp)
  
  # Add to all results
  
  all_results <- rbind(all_results, summary)
}

# Clean and save

rm(vax, risk, surv_data, model, coefs, coef_exp, ci95_exp, summary)

write.csv(all_results, file = paste0("Results_SurvivalAnalysis/", buffer_folder, "/MultivarCox_RandomCountry_Regression.csv"), row.names = FALSE)

## In case, starting from zero

all_results <- read.csv(paste0("Results_SurvivalAnalysis/", buffer_folder, "/MultivarCox_RandomCountry_Regression.csv"))

library(patchwork)

# Plot

order <- c("BCG", "HepB-BD", "OPV-BD",
           "DTP-D1", "HepB-D1", "Hib-D1", "IPV-D1", "OPV-D1", "PCV-D1", "RV-D1",
           "DTP-D2", "HepB-D2", "Hib-D2", "OPV-D2", "PCV-D2", "RV-D2",
           "DTP-D3", "HepB-D3", "Hib-D3", "OPV-D3", "PCV-D3", "RV-D3",
           "MCV-D1", "MCV-D2")

plot_list <- list()

for (i in 1:length(risk_ids)) {
  
  p <- ggplot(data = filter(all_results, var == risk_ids[i]),
              aes(x = hr, y = factor(vaccine, order))) +
    geom_linerange(aes(xmin = as.numeric(ci_low), xmax = as.numeric(ci_upp),
                       color = char)) +
    geom_point(aes(color = char), shape = 15, size = 3) +
    scale_y_discrete(limits = rev) +
    scale_color_manual(breaks = risk_names[[i]],
                       values = carto_pal(name = "Safe")) +
    labs(title = paste0(risk_ids[i]), x = "Hazard Ratio (95% CI)") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold"),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom")
  
  plot_list[[i]] <- p
}

rm(i, p)

# For the paper

png(paste0("Results_SurvivalAnalysis/", buffer_folder, "/Figure4_RiskFactors_HR.png"),
    width = 16, height = 7, res = 1200, units = "in")
wrap_plots(plot_list[c(2, 3, 4)])
dev.off()
pdf(paste0("Results_SurvivalAnalysis/", buffer_folder, "/Figure4_RiskFactors_HR.pdf"),
    width = 16, height = 7)
wrap_plots(plot_list[c(2, 3, 4)])
dev.off()

png(paste0("Results_SurvivalAnalysis/", buffer_folder, "/SuppFigure5_RiskFactors_HR.png"),
    width = 18, height = 7, res = 1200, units = "in")
wrap_plots(plot_list[c(1, 5, 6)])
dev.off()
pdf(paste0("Results_SurvivalAnalysis/", buffer_folder, "/SuppFigure5_RiskFactors_HR.pdf"),
    width = 18, height = 7)
wrap_plots(plot_list[c(1, 5, 6)])
dev.off()