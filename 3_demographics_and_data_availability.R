#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Timeliness of 24 childhood immunisations and evolution of vaccination delay:#
#-#-#-#-#- analysis of data from 54 low- and middle-income countries -#-#-#-#-#


# 3/ Data available and demographics of population



# SET UP ------------------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(rcartocolor)
library(ggpubr)
library(ggrepel)
library(rdhs)
library(scales)  # comma

# Folder for results

dir.create("Demographics")
dir.create("Demographics/Basic tables")



# DATA -----------------------------------------------------------------


#### Load ####

## Individual data

analysis_main <- readRDS("Data/Analysis_FinalDataset_IndividualData.Rds")


#### Variables ####

vaccines <- c("bcg", "dtp1", "dtp2", "dtp3", "opv0", "opv1", "opv2", "opv3",
              "ipv1", "ipv2", "mcv1", "mcv2",
              "hepB0", "hepB1", "hepB2", "hepB3", "hib1", "hib2", "hib3",
              "pneumo1", "pneumo2", "pneumo3", "rota1", "rota2", "rota3")

titles <- c("BCG", "DTP-D1", "DTP-D2", "DTP-D3", "OPV-BD", "OPV-D1", "OPV-D2", "OPV-D3",
            "IPV-D1", "IPV-D2", "MCV-D1", "MCV-D2",
            "HepB-BD", "HepB-D1", "HepB-D2", "HepB-D3", "Hib-D1", "Hib-D2", "Hib-D3",
            "PCV-D1", "PCV-D2", "PCV-D3", "RV-D1", "RV-D2", "RV-D3")



# REFERENCE TABLES --------------------------------------------------------


#### Year country ####

country_year <- analysis_main %>%
  group_by(country) %>%
  # Get a unique year of survey (sometimes survey spans two years)
  mutate(max_year = max(year)) %>%
  # One obs per country
  filter(row_number() == 1) %>%
  ungroup() %>%
  # Clean
  arrange(country) %>%
  select(country, max_year) %>%
  mutate(country_year = paste0(country, " (", max_year, ")"))

write.csv(country_year, file = "Demographics/Basic tables/year_country.csv", row.names = FALSE)


#### iso3 ####

iso3_ref <- dhs_countries() %>%
  select(CountryName, ISO3_CountryCode)
colnames(iso3_ref) <- c("country", "iso3")

country_year <- merge(country_year, iso3_ref, by = "country", all.x = TRUE) %>%
  select(iso3, country, max_year, country_year)



# SUPP TABLES -------------------------------------------------------------------


#### Vaccine introduction ####

supp_t2 <- read.csv("Data/WHO_vaccination_introduction_reference.csv") %>%
  select(iso3, vaccine, intro_year)

# Merge the two data sets

supp_t2 <- merge(country_year, supp_t2, by = "iso3", all.x = TRUE)

supp_t2 <- supp_t2 %>%
  select(country_year, vaccine, intro_year) %>%
  arrange(country_year, vaccine) %>%
  pivot_wider(names_from = "vaccine", values_from = "intro_year")

write.csv(supp_t2, file = "Demographics/SuppTable2_VaxIntroduction.csv", row.names = FALSE)


#### Basic tables ####

## Total number of children

# Vector to store all results

summary_all <- data.frame()

# Loop through all vaccines data availability

for (v in 1:length(vaccines)) {
  
  # Subset to each vax: that way children only count for one
  data_subset <- analysis_main %>%
    filter(vaccine == vaccines[v])
  
  ## Not vaccine-specific measures
  
  # Number of children in survey
  total_survey_children = nrow(data_subset)
  
  # Number of males
  total_male = length(which(data_subset[["sex"]] == 1))
  
  # Number of females
  total_female  = length(which(data_subset[["sex"]] == 2))
  
  # Age range
  age_range = paste0("(", summary(data_subset[["age_months"]])[1], ", ", summary(data_subset[["age_months"]])[6], ")")
  
  # Median age
  median_age = summary(data_subset[["age_months"]])[3]
  
  # IQR age
  age_iqr = paste0("(", summary(data_subset[["age_months"]])[2], ", ", summary(data_subset[["age_months"]])[5], ")")
  
  ## Vaccine-specific measures: countries
  
  # Total no of countries
  total_countries <- length(unique(data_subset$country))
  
  # Number of countries that had introduced the vaccine by survey time
  no_countries_intro <- length(unique(data_subset$country[data_subset$intro_vax == 1]))
  
  # Proportion of countries with vaccine intro
  prop_countries_intro <- round(no_countries_intro*100/total_countries)
  
  # Number of countries that HAD NOT introduced the vaccine by survey time
  no_countries_notvax <- length(unique(data_subset$country[data_subset$intro_vax == 0]))
  
  # Proportion of countries with NOT vaccine intro
  prop_countries_notvax <- round(no_countries_notvax*100/total_countries)
  
  # Number of countries with data for that vaccine
  coun_w_data <- length(unique(data_subset$country[!is.na(data_subset[[vaccines[v]]])]))
  
  # Proportion of countries with data for that vaccine (that could have had the data in the first place)
  prop_coun_w_data <- round(coun_w_data*100/no_countries_intro)
  
  ## Vaccine-specific measures: children
  
  # Those in survey with vax intro and within the year buffer periods
  
  # Number of children in survey that had introduced the vaccine
  total_children_intro = nrow(data_subset[data_subset$intro_vax == 1,])
  
  # Proportion of children in survey that had introduced the vaccine
  prop_children_intro = round(total_children_intro*100/total_survey_children, digits = 2)
  
  # Number of children with vaccination data
  total_w_data = length(which(!is.na(data_subset[[vaccines[v]]])))
  
  # Proportion of children with vaccination data (among those with introduction)
  prop_w_data = round(total_w_data*100/total_children_intro, digits = 2)
  
  # Number of children with vaccination data in the 1-year buffer cut
  total_1ye_buff_intro = nrow(data_subset[!is.na(data_subset[[vaccines[v]]]) & data_subset$t_buffer_rec_to_survey >= 52,])
  
  # Proportion of children with vaccination data in the 1-year buffer cut
  prop_1ye_buff_intro = round(total_1ye_buff_intro*100/total_w_data, digits = 2)
  
  # Number of children with vaccination data in the 2-year buffer cut
  total_2ye_buff_intro = nrow(data_subset[!is.na(data_subset[[vaccines[v]]]) & data_subset$t_buffer_rec_to_survey >= 52*2,])
  
  # Proportion of children with vaccination data in the 2-year buffer cut
  prop_2ye_buff_intro = round(total_2ye_buff_intro*100/total_w_data, digits = 2)
  
  # Data subsets with each of the buffers
  
  data_subset1 <- data_subset[data_subset$t_buffer_rec_to_survey >= 52, ]
  data_subset2 <- data_subset[data_subset$t_buffer_rec_to_survey >= 52*2, ]
  
  # For children within 1 year between recomm and survey
  
  # Number of children unvaccinated
  total_unvax1 = length(which(data_subset1[[vaccines[v]]] == 0))
  
  # Proportion of unvaccinated children
  prop_unvax1 = round(total_unvax1*100/total_1ye_buff_intro, digits = 2)
  
  # Number of children vaccinated
  total_vax1 = length(which(data_subset1[[vaccines[v]]] == 1))
  
  # Proportion of vaccinated children - over all with vax data (coverage)
  prop_vax1 = round(total_vax1*100/total_1ye_buff_intro, digits = 2)
  
  # Number of children with vaccination date data
  total_vax_date_data1 = length(which(!is.na(data_subset1$age_at_vax)))
  
  # Proportion of children with date data - over all vaccinated
  prop_vax_date1 = round(total_vax_date_data1*100/total_vax1, digits = 2)
  
  # For children within 2 year between recomm and survey
  
  # Number of children unvaccinated
  total_unvax2 = length(which(data_subset2[[vaccines[v]]] == 0))
  
  # Proportion of unvaccinated children
  prop_unvax2 = round(total_unvax2*100/total_2ye_buff_intro, digits = 2)
  
  # Number of children vaccinated
  total_vax2 = length(which(data_subset2[[vaccines[v]]] == 1))
  
  # Proportion of vaccinated children - over all with vax data (coverage)
  prop_vax2 = round(total_vax2*100/total_2ye_buff_intro, digits = 2)
  
  # Number of children with vaccination date data
  total_vax_date_data2 = length(which(!is.na(data_subset2$age_at_vax)))
  
  # Proportion of children with date data - over all vaccinated
  prop_vax_date2 = round(total_vax_date_data2*100/total_vax2, digits = 2)
  
  # All estimates together
  summary <- cbind(country = "All", year = NA,
                   total_survey_children,
                   total_male, total_female,
                   age_range, median_age, age_iqr,
                   vaccine = titles[v],
                   ## Vaccine-specific: countries
                   total_countries, no_countries_intro, prop_countries_intro,
                   no_countries_notvax, prop_countries_notvax,
                   coun_w_data, prop_coun_w_data,
                   ## Vaccine-specific: children
                   # Children who introduced the vaccine
                   total_children_intro, prop_children_intro,
                   total_w_data, prop_w_data,
                   total_1ye_buff_intro, prop_1ye_buff_intro,
                   total_2ye_buff_intro, prop_2ye_buff_intro,
                   # Data availability depending on year buffer
                   total_unvax1, prop_unvax1, total_vax1, prop_vax1,
                   total_vax_date_data1, prop_vax_date1,
                   total_unvax2, prop_unvax2, total_vax2, prop_vax2,
                   total_vax_date_data2, prop_vax_date2)
  
  summary <- as.data.frame(summary)
  
  # Concatenate to all results
  summary_all <- rbind(summary_all, summary)
}

# Aesthetics
summary_all <- summary_all %>%
  arrange(country, year, vaccine)

write.csv(summary_all,
          file = "Demographics/Basic tables/no_children_all.csv", row.names = FALSE)

## Total no of children per country

# Vector to store all results

summary_all <- data.frame()

# All countries

countries <- unique(analysis_main$country)

# Loop through all vaccines data availability

for (c in 1:length(countries)) {
  
  # Vector to store all results from the country
  
  summary_all_country <- data.frame()
  
  for (v in 1:length(vaccines)) {
    
    # Filter for each country & for each vax (children only count for one)
    data_subset <- analysis_main %>%
      filter(vaccine == vaccines[v]) %>%
      filter(country == countries[c])
    
    ## Not vaccine-specific measures
    
    # Number of children in survey
    total_survey_children = nrow(data_subset)
    
    # Number of males
    total_male = length(which(data_subset[["sex"]] == 1))
    
    # Number of females
    total_female  = length(which(data_subset[["sex"]] == 2))
    
    # Age range
    age_range = paste0("(", summary(data_subset[["age_months"]])[1], ", ", summary(data_subset[["age_months"]])[6], ")")
    
    # Median age
    median_age = summary(data_subset[["age_months"]])[3]
    
    # IQR age
    age_iqr = paste0("(", summary(data_subset[["age_months"]])[2], ", ", summary(data_subset[["age_months"]])[5], ")")
    
    ## Vaccine-specific measures: children
    
    # Those in survey with vax intro and within the year buffer periods
    
    # Number of children in survey that had introduced the vaccine
    total_children_intro = nrow(data_subset[data_subset$intro_vax == 1,])
    
    # Proportion of children in survey that had introduced the vaccine
    prop_children_intro = round(total_children_intro*100/total_survey_children, digits = 2)
    
    # Number of children with vaccination data
    total_w_data = length(which(!is.na(data_subset[[vaccines[v]]])))
    
    # Proportion of children with vaccination data (among those with introduction)
    prop_w_data = round(total_w_data*100/total_children_intro, digits = 2)
    
    # Number of children with vaccination data in the 1-year buffer cut
    total_1ye_buff_intro = nrow(data_subset[!is.na(data_subset[[vaccines[v]]]) & data_subset$t_buffer_rec_to_survey >= 52,])
    
    # Proportion of children with vaccination data in the 1-year buffer cut
    prop_1ye_buff_intro = round(total_1ye_buff_intro*100/total_w_data, digits = 2)
    
    # Number of children with vaccination data in the 2-year buffer cut
    total_2ye_buff_intro = nrow(data_subset[!is.na(data_subset[[vaccines[v]]]) & data_subset$t_buffer_rec_to_survey >= 52*2,])
    
    # Proportion of children with vaccination data in the 2-year buffer cut
    prop_2ye_buff_intro = round(total_2ye_buff_intro*100/total_w_data, digits = 2)
    
    # Data subsets with each of the buffers
    
    data_subset1 <- data_subset[data_subset$t_buffer_rec_to_survey >= 52, ]
    data_subset2 <- data_subset[data_subset$t_buffer_rec_to_survey >= 52*2, ]
    
    # For children within 1 year between recomm and survey
    
    # Number of children unvaccinated
    total_unvax1 = length(which(data_subset1[[vaccines[v]]] == 0))
    
    # Proportion of unvaccinated children
    prop_unvax1 = round(total_unvax1*100/total_1ye_buff_intro, digits = 2)
    
    # Number of children vaccinated
    total_vax1 = length(which(data_subset1[[vaccines[v]]] == 1))
    
    # Proportion of vaccinated children - over all with vax data (coverage)
    prop_vax1 = round(total_vax1*100/total_1ye_buff_intro, digits = 2)
    
    # Number of children with vaccination date data
    total_vax_date_data1 = length(which(!is.na(data_subset1$age_at_vax)))
    
    # Proportion of children with date data - over all vaccinated
    prop_vax_date1 = round(total_vax_date_data1*100/total_vax1, digits = 2)
    
    # For children within 2 year between recomm and survey
    
    # Number of children unvaccinated
    total_unvax2 = length(which(data_subset2[[vaccines[v]]] == 0))
    
    # Proportion of unvaccinated children
    prop_unvax2 = round(total_unvax2*100/total_2ye_buff_intro, digits = 2)
    
    # Number of children vaccinated
    total_vax2 = length(which(data_subset2[[vaccines[v]]] == 1))
    
    # Proportion of vaccinated children - over all with vax data (coverage)
    prop_vax2 = round(total_vax2*100/total_2ye_buff_intro, digits = 2)
    
    # Number of children with vaccination date data
    total_vax_date_data2 = length(which(!is.na(data_subset2$age_at_vax)))
    
    # Proportion of children with date data - over all vaccinated
    prop_vax_date2 = round(total_vax_date_data2*100/total_vax2, digits = 2)
    
    # All estimates together
    summary <- cbind(country = countries[c], year = max(data_subset$year, na.rm = TRUE),
                     total_survey_children,
                     total_male, total_female,
                     age_range, median_age, age_iqr,
                     vaccine = titles[v],
                     ## Vaccine-specific: children
                     # Children who introduced the vaccine
                     total_children_intro, prop_children_intro,
                     total_w_data, prop_w_data,
                     total_1ye_buff_intro, prop_1ye_buff_intro,
                     total_2ye_buff_intro, prop_2ye_buff_intro,
                     # Data availability depending on year buffer
                     total_unvax1, prop_unvax1, total_vax1, prop_vax1,
                     total_vax_date_data1, prop_vax_date1,
                     total_unvax2, prop_unvax2, total_vax2, prop_vax2,
                     total_vax_date_data2, prop_vax_date2)
    
    summary <- as.data.frame(summary)
    
    # Concatenate to all results from the country
    summary_all_country <- rbind(summary_all_country, summary)
    
  }
  
  # Concatenate to all results
  
  summary_all <- rbind(summary_all, summary_all_country)
  
}

# Aesthetics
summary_all <- summary_all %>%
  arrange(country, year, vaccine)

write.csv(summary_all,
          file = "Demographics/Basic tables/no_children_country.csv", row.names = FALSE)

## Total no of children per birth cohort

# Vector to store all results

summary_all <- data.frame()

# Birth cohorts

years <- unique(analysis_main$dob_year)

# Loop through all vaccines data availability

for (v in 1:length(vaccines)) {
  
  # Vector to store result from each vaccine
  
  summary_all_vaccine <- data.frame()
  
  # Loop through every birth cohort for every vaccine
  
  for (y in 1:length(years)) {
    
    # Filter for each DOB-ye  & for each vax (children only count for one)
    data_subset <- analysis_main %>%
      filter(vaccine == vaccines[v]) %>%
      filter(dob_year == years[y])
    
    ## Not vaccine-specific measures
    
    # Number of children in survey
    total_survey_children = nrow(data_subset)
    
    # Number of males
    total_male = length(which(data_subset[["sex"]] == 1))
    
    # Number of females
    total_female  = length(which(data_subset[["sex"]] == 2))
    
    # Age range
    age_range = paste0("(", summary(data_subset[["age_months"]])[1], ", ", summary(data_subset[["age_months"]])[6], ")")
    
    # Median age
    median_age = summary(data_subset[["age_months"]])[3]
    
    # IQR age
    age_iqr = paste0("(", summary(data_subset[["age_months"]])[2], ", ", summary(data_subset[["age_months"]])[5], ")")
    
    ## Vaccine-specific measures: countries
    
    # Total no of countries
    total_countries <- length(unique(data_subset$country))
    
    # Number of countries that had introduced the vaccine by survey time
    no_countries_intro <- length(unique(data_subset$country[data_subset$intro_vax == 1]))
    
    # Proportion of countries with vaccine intro
    prop_countries_intro <- round(no_countries_intro*100/total_countries)
    
    # Number of countries that HAD NOT introduced the vaccine by survey time
    no_countries_notvax <- length(unique(data_subset$country[data_subset$intro_vax == 0]))
    
    # Proportion of countries with NOT vaccine intro
    prop_countries_notvax <- round(no_countries_notvax*100/total_countries)
    
    # Number of countries with data for that vaccine
    coun_w_data <- length(unique(data_subset$country[!is.na(data_subset[[vaccines[v]]])]))
    
    # Proportion of countries with data for that vaccine (that could have had the data in the first place)
    prop_coun_w_data <- round(coun_w_data*100/no_countries_intro)
    
    ## Vaccine-specific measures: children
    
    # Those in survey with vax intro and within the year buffer periods
    
    # Number of children in survey that had introduced the vaccine
    total_children_intro = nrow(data_subset[data_subset$intro_vax == 1,])
    
    # Proportion of children in survey that had introduced the vaccine
    prop_children_intro = round(total_children_intro*100/total_survey_children, digits = 2)
    
    # Number of children with vaccination data
    total_w_data = length(which(!is.na(data_subset[[vaccines[v]]])))
    
    # Proportion of children with vaccination data (among those with introduction)
    prop_w_data = round(total_w_data*100/total_children_intro, digits = 2)
    
    # Number of children with vaccination data in the 1-year buffer cut
    total_1ye_buff_intro = nrow(data_subset[!is.na(data_subset[[vaccines[v]]]) & data_subset$t_buffer_rec_to_survey >= 52,])
    
    # Proportion of children with vaccination data in the 1-year buffer cut
    prop_1ye_buff_intro = round(total_1ye_buff_intro*100/total_w_data, digits = 2)
    
    # Number of children with vaccination data in the 2-year buffer cut
    total_2ye_buff_intro = nrow(data_subset[!is.na(data_subset[[vaccines[v]]]) & data_subset$t_buffer_rec_to_survey >= 52*2,])
    
    # Proportion of children with vaccination data in the 2-year buffer cut
    prop_2ye_buff_intro = round(total_2ye_buff_intro*100/total_w_data, digits = 2)
    
    # Data subsets with each of the buffers
    
    data_subset1 <- data_subset[data_subset$t_buffer_rec_to_survey >= 52, ]
    data_subset2 <- data_subset[data_subset$t_buffer_rec_to_survey >= 52*2, ]
    
    # For children within 1 year between recomm and survey
    
    # Number of children unvaccinated
    total_unvax1 = length(which(data_subset1[[vaccines[v]]] == 0))
    
    # Proportion of unvaccinated children
    prop_unvax1 = round(total_unvax1*100/total_1ye_buff_intro, digits = 2)
    
    # Number of children vaccinated
    total_vax1 = length(which(data_subset1[[vaccines[v]]] == 1))
    
    # Proportion of vaccinated children - over all with vax data (coverage)
    prop_vax1 = round(total_vax1*100/total_1ye_buff_intro, digits = 2)
    
    # Number of children with vaccination date data
    total_vax_date_data1 = length(which(!is.na(data_subset1$age_at_vax)))
    
    # Proportion of children with date data - over all vaccinated
    prop_vax_date1 = round(total_vax_date_data1*100/total_vax1, digits = 2)
    
    # For children within 2 year between recomm and survey
    
    # Number of children unvaccinated
    total_unvax2 = length(which(data_subset2[[vaccines[v]]] == 0))
    
    # Proportion of unvaccinated children
    prop_unvax2 = round(total_unvax2*100/total_2ye_buff_intro, digits = 2)
    
    # Number of children vaccinated
    total_vax2 = length(which(data_subset2[[vaccines[v]]] == 1))
    
    # Proportion of vaccinated children - over all with vax data (coverage)
    prop_vax2 = round(total_vax2*100/total_2ye_buff_intro, digits = 2)
    
    # Number of children with vaccination date data
    total_vax_date_data2 = length(which(!is.na(data_subset2$age_at_vax)))
    
    # Proportion of children with date data - over all vaccinated
    prop_vax_date2 = round(total_vax_date_data2*100/total_vax2, digits = 2)
    
    # All estimates together
    summary <- cbind(country = "All", birth_cohort = years[y],
                     total_survey_children,
                     total_male, total_female,
                     age_range, median_age, age_iqr,
                     vaccine = titles[v],
                     ## Vaccine-specific: countries
                     total_countries, no_countries_intro, prop_countries_intro,
                     no_countries_notvax, prop_countries_notvax,
                     coun_w_data, prop_coun_w_data,
                     ## Vaccine-specific: children
                     # Children who introduced the vaccine
                     total_children_intro, prop_children_intro,
                     total_w_data, prop_w_data,
                     total_1ye_buff_intro, prop_1ye_buff_intro,
                     total_2ye_buff_intro, prop_2ye_buff_intro,
                     # Data availability depending on year buffer
                     total_unvax1, prop_unvax1, total_vax1, prop_vax1,
                     total_vax_date_data1, prop_vax_date1,
                     total_unvax2, prop_unvax2, total_vax2, prop_vax2,
                     total_vax_date_data2, prop_vax_date2)
    
    summary <- as.data.frame(summary)
    
    # Concatenate to all results from the vaccine
    summary_all_vaccine <- rbind(summary_all_vaccine, summary)
    
  }
  
  # Concatenate to all results from the vaccine
  summary_all <- rbind(summary_all, summary_all_vaccine)
  
}

# Aesthetics
summary_all <- summary_all %>%
  arrange(country, vaccine, birth_cohort)

write.csv(summary_all,
          file = "Demographics/Basic tables/no_children_evolution.csv", row.names = FALSE)


#### Country/birth children ####

## Read the tables

no_children_all <- read.csv("Demographics/Basic tables/no_children_all.csv")
no_children_cou <- read.csv("Demographics/Basic tables/no_children_country.csv")
no_children_evo <- read.csv("Demographics/Basic tables/no_children_evolution.csv")

## Prep

no_children_all <- no_children_all %>%
  filter(row_number() == 1) %>%
  select(total_survey_children, total_male, total_female,
         median_age, age_iqr) %>%
  mutate(total_survey_children = paste0(comma(total_survey_children))) %>%
  mutate(total_male = paste0(comma(total_male))) %>%
  mutate(total_female = paste0(comma(total_female))) %>%
  mutate(age = paste0(median_age, " ", age_iqr)) %>%
  mutate(birth_country = "All") %>%
  select(birth_country, total_survey_children, total_male, total_female, age)

no_children_cou <- no_children_cou %>%
  group_by(country) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(country, total_survey_children, total_male, total_female,
         median_age, age_iqr) %>%
  mutate(total_survey_children = paste0(comma(total_survey_children))) %>%
  mutate(total_male = paste0(comma(total_male))) %>%
  mutate(total_female = paste0(comma(total_female))) %>%
  mutate(age = paste0(median_age, " ", age_iqr)) %>%
  merge(country_year) %>%
  mutate(birth_country = country_year) %>%
  select(birth_country, total_survey_children, total_male, total_female, age)

no_children_evo <- no_children_evo %>%
  group_by(birth_cohort) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(birth_cohort, total_survey_children, total_male, total_female,
         median_age, age_iqr) %>%
  mutate(total_survey_children = paste0(comma(total_survey_children))) %>%
  mutate(total_male = paste0(comma(total_male))) %>%
  mutate(total_female = paste0(comma(total_female))) %>%
  mutate(age = paste0(median_age, " ", age_iqr)) %>%
  mutate(birth_country = birth_cohort) %>%
  select(birth_country, total_survey_children, total_male, total_female, age)

no_children <- rbind(no_children_all, no_children_cou)
no_children <- rbind(no_children, no_children_evo)

colnames(no_children) <-c("Country/Birth Cohort", "Surveyed children, No.",
                          "Male, No.", "Female, No.", "Median age (IQR)")

## Re-export

write.csv(no_children, file = "Demographics/SuppTable3_NoChildren_AllCountryBirth.csv", row.names = FALSE)


#### Vaccine dat availability ####

## Read the table

no_children_vax <- read.csv("Demographics/Basic tables/no_children_all.csv")

## Prep

no_children_vax1 <- no_children_vax %>%
  mutate(coun_w_vax = paste0(no_countries_intro, " (", prop_countries_intro, ")")) %>%
  mutate(coun_not_vax = paste0(no_countries_notvax, " (", prop_countries_notvax, ")")) %>%
  mutate(coun_w_vaxdata = paste0(coun_w_data, " (", prop_coun_w_data, ")")) %>%
  mutate(children_intro = paste0(comma(total_children_intro), " (", prop_children_intro, ")")) %>%
  mutate(vax_data = paste0(comma(total_w_data), " (", prop_w_data, ")")) %>%
  mutate(buff1 = paste0(comma(total_1ye_buff_intro), " (", prop_1ye_buff_intro, ")")) %>%
  mutate(unvax = paste0(comma(total_unvax1), " (", prop_unvax1, ")")) %>%
  mutate(vax = paste0(comma(total_vax1), " (", prop_vax1, ")")) %>%
  mutate(age_data = paste0(comma(total_vax_date_data1), " (", prop_vax_date1, ")")) %>%
  select(vaccine,
         coun_w_vax, coun_not_vax, coun_w_vaxdata,
         children_intro, vax_data, buff1,
        unvax, vax, age_data)
  
colnames(no_children_vax1) <- c("Vaccine",
                               "Countries with vaccine introduced, No. (%)", "Countries with vaccine not introduced, No. (%)",
                               "Countries with vaccine introduced with vaccine data, No. (%)",
                               "Children from countries with vaccine introduced, No. (%)",
                               "Children with vaccine data, No. (%)",
                               "Children with vaccine data with 1-year buffer time, No. (%)",
                               "Unvaccinated children, No. (%)", "Vaccinated children, No. (%)",
                               "Vaccinated children with vaccination age data, No. (%)")

no_children_vax2 <- no_children_vax %>%
  mutate(coun_w_vax = paste0(no_countries_intro, " (", prop_countries_intro, ")")) %>%
  mutate(coun_not_vax = paste0(no_countries_notvax, " (", prop_countries_notvax, ")")) %>%
  mutate(coun_w_vaxdata = paste0(coun_w_data, " (", prop_coun_w_data, ")")) %>%
  mutate(children_intro = paste0(comma(total_children_intro), " (", prop_children_intro, ")")) %>%
  mutate(vax_data = paste0(comma(total_w_data), " (", prop_w_data, ")")) %>%
  mutate(buff2 = paste0(comma(total_2ye_buff_intro), " (", prop_2ye_buff_intro, ")")) %>%
  mutate(unvax = paste0(comma(total_unvax2), " (", prop_unvax2, ")")) %>%
  mutate(vax = paste0(comma(total_vax2), " (", prop_vax2, ")")) %>%
  mutate(age_data = paste0(comma(total_vax_date_data2), " (", prop_vax_date2, ")")) %>%
  select(vaccine,
         coun_w_vax, coun_not_vax, coun_w_vaxdata,
         children_intro, vax_data, buff2,
         unvax, vax, age_data)

colnames(no_children_vax2) <- c("Vaccine",
                                "Countries with vaccine introduced, No. (%)", "Countries with vaccine not introduced, No. (%)",
                                "Countries with vaccine introduced with vaccine data, No. (%)",
                                "Children from countries with vaccine introduced, No. (%)",
                                "Children with vaccine data, No. (%)",
                                "Children with vaccine data with 2-year buffer time, No. (%)",
                                "Unvaccinated children, No. (%)", "Vaccinated children, No. (%)",
                                "Vaccinated children with vaccination age data, No. (%)")

write.csv(no_children_vax1, file = "Demographics/SuppTable4_VaxDataAvailability_1yearBuffer.csv", row.names = FALSE)
write.csv(no_children_vax2, file = "Demographics/SuppTable4_VaxDataAvailability_2yearBuffer.csv", row.names = FALSE)



# PLOTS ---------------------------------------------------------------------


#### Vax recomm age ####

recomm <- read.csv("Data/WHO_schedules_reference.csv")

recomm[recomm == "Democratic Republic of the Congo"] <- "Congo Democratic Republic"
recomm[recomm == "Côte d'Ivoire"] <- "Cote d'Ivoire"
recomm[recomm == "Kyrgyzstan"] <- "Kyrgyz Republic"
recomm[recomm == "United Republic of Tanzania"] <- "Tanzania"
recomm[recomm == "Türkiye"] <- "Turkey"

# clean data

recomm <- recomm %>%
  select(country, vaccine, recomm_weeks) %>%
  filter(country %in% country_year$country) %>%
  filter(vaccine != "IPV-D2" & vaccine != "IPV-D3" & vaccine != "DTP-D4" & vaccine != "OPV-D4" &
           vaccine != "PCV-D4" & vaccine != "HepB-D4" & vaccine != "IPV-D4") %>%
  group_by(country, vaccine) %>%
  mutate(recomm_weeks = min(recomm_weeks, na.rm = TRUE)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  arrange(country, vaccine)

# add WHO general recommendations

add <- data.frame(country = rep(" WHO recomm", times = 24),
                  vaccine = unique(recomm$vaccine),
                  recomm_weeks = c(0, # Recomm for BCG
                                   6, 10, 14, # Recomm for DTP
                                   0, 6, 10, 14, # Recomm for Hep
                                   6, 10, 14, # Recomm for Hib
                                   6, # IPV
                                   39, 65, # Recomm for MCV
                                   0, 6, 10, 14, # Recomm for OPV
                                   6, 10, 14, # Recomm for PCV
                                   6, 10, 14)) # Recomm for RV
recomm <- rbind(add, recomm)

# Calculate difference between country and WHO and pivot separately

recomm <- recomm %>%
  group_by(vaccine) %>%
  mutate(who_recomm = recomm_weeks[country == " WHO recomm"]) %>%
  ungroup() %>%
  mutate(diff = recomm_weeks - who_recomm) %>%
  mutate(binary = case_when(diff > 0 ~ 1,
                            diff == 0 ~ 0,
                            diff < 0 ~ -1)) %>%
  mutate(font = case_when(diff > 40 ~ 1,
                          diff <= 40 ~ 0))

recomm_table <- recomm %>%
  select(-c(who_recomm, diff)) %>%
  pivot_wider(names_from = "vaccine", values_from = "recomm_weeks")

# visual

asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}

p <- ggplot(data = recomm,
            mapping = aes(y = country)) +
  geom_segment(aes(yend = country, x = 0, xend = diff)) +
  geom_vline(aes(xintercept = 0), color = "black") +
  geom_point(aes(x = diff, fill = as.factor(binary),
             color = as.factor(binary)), pch = 21) +
  scale_x_continuous(trans = "asinh",
                     breaks=c(-15,-2,0,2,7,20,80),
                     name = "Weeks of difference between each country's vaccination age recommendation and WHO's age recommendation") +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(breaks = c("-1", "0", "1"),
                    values = c(carto_pal(name = "Safe")[7], "white",
                               carto_pal(name = "Safe")[9])) +
  scale_color_manual(breaks = c("-1", "0", "1"),
                     values = c(carto_pal(name = "Safe")[7], "black",
                                carto_pal(name = "Safe")[9])) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 15, size = rel(0.5)),
        strip.text.x.top = element_text(angle = 15),
        strip.background = element_blank()) +
  guides(fill = "none", color = "none") +
  facet_grid(. ~ vaccine)

png("Demographics/SuppFigure1_AgeReccommendations.png",
    res = 1200, units = "in", height = 12, width = 15)
p
dev.off()

pdf("Demographics/SuppFigure1_AgeReccommendations.pdf",
    height = 12, width = 15)
p
dev.off()


#### Map ####

map <- read.csv("Demographics/Basic tables/no_children_country.csv")

map <- map %>%
  select(country, total_survey_children) %>%
  group_by(country) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(prop = total_survey_children/sum(total_survey_children))
  
## Packages and base maps

library(sf)
library(transformr)
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")

table(unique(map$country) %in% world$admin)

map[map  == "Congo"] <- "Republic of Congo"
map[map  == "Congo Democratic Republic"] <- "Democratic Republic of the Congo"
map[map  == "Cote d'Ivoire"] <- "Ivory Coast"
map[map  == "Kyrgyz Republic"] <- "Kyrgyzstan"
map[map  == "Tanzania"] <- "United Republic of Tanzania"
map[map  == "Timor-Leste"] <- "East Timor"

table(unique(map$country) %in% world$admin)

## Clean and merge data together

map_data <- world %>% select(admin, geometry)
colnames(map_data) <- c("country", "geometry")

map <- merge(map, map_data, all.x = TRUE)

## Plot

p <- ggplot() +
  geom_sf(data = world) +
  geom_sf(data = map, aes(fill = (prop), geometry = geometry)) +
  scale_fill_gradient(low = carto_pal(name = "Safe")[1],
                      high = carto_pal(name = "Safe")[5],
                      trans = log10_trans(),
                      breaks = trans_breaks("log10", function(x) 10^x),
                      labels = paste0(c(0.001, 0.01, 0.03, 0.10, 0.3)*100, "%"),
                      name = "Percentage of children from each country survey among all included in the analyses") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.key.height = unit(0.4,"line"),
        legend.key.width = unit(3, "line"),
        legend.spacing.x = unit(1.0, 'cm'))

pdf("Demographics/SuppFigure2_MapPropChildren.pdf",
    height = 8, width = 14)
p
dev.off()
png("Demographics/SuppFigure2_MapPropChildren.png",
    res = 1200, units = "in", height = 8, width = 14)
p
dev.off()


#### Sensibility ####

# Risk factors to analyse different prop of demographic categories
# in children with and without vaccine (date) data

risk_factors <- c("sex", "health_card", "urban", "city", "wealth",
                  "mother_education", "mother_literacy", "married", "mother_occupation",
                  "husband_edu", "husband_lit", "husband_ocu")

risk_titles <- c("Sex at birth", "Health Card", "Urban/Rural", "Residence", "Wealth Quintile",
                 "Mother's Education", "Mother's Literacy", "Mother's Marital Status", "Mother's Occupation",
                 "Husband's Education", "Husband's Literacy", "Husband's Occupation")

risk_names <- list(c("Male", "Female"), c("No card", "Yes - Seen", "Yes - Not Seen", "Lost", "Other"),
                   c("Urban", "Rural"), c("City", "Town", "Countryside"),
                   c("Poorest", "Poor", "Middle", "Rich", "Richest"),
                   c("None", "Primary", "Secondary", "Higher"),
                   c("Cannot read", "Reads - difficult", "Reads"),
                   c("Not married", "Married/Living together",
                     "Widowed/Divorced/Not living together"),
                   c("No work", "Prof/Manager/Clerical/Sales",
                     "Agricultural", "Household/Services", "Manual work"),
                   c("None", "Primary", "Secondary", "Higher"),
                   c("Cannot read", "Reads - difficult", "Reads"),
                   c("No work", "Prof/Manager/Clerical/Sales",
                     "Agricultural", "Household/Services", "Manual work"))

##### Loops #####

## Compare the demographics of children with and without vax data

# Vector to store all results

results_all <- data.frame()

# Loop

for (v in 1:length(vaccines)) {
  
  
  # Binary indicator for missing information
  
  data_subset <- analysis_main %>%
    filter(vaccine == vaccines[v]) %>%
    select(vaccines[v], all_of(risk_factors))
  
  data_subset$missing <- NA
  data_subset$missing[!is.na(data_subset[[vaccines[v]]])] <- 0
  data_subset$missing[is.na(data_subset[[vaccines[v]]])] <- 1
  
  # Vector to concatenate results
  
  result_vaccine <- data.frame()
  
  
  ## Loop through every variable
  
  for (var in 1:length(risk_factors)) {
    
    levels <- unique(data_subset[[risk_factors[var]]])
    levels <- levels[!is.na(levels)]
    levels <- sort(levels)
    
    ## Contingency table
    
    table <- table(data_subset[[risk_factors[var]]], data_subset[["missing"]])
    
    if(dim(table)[1] > 0) { # Checkpoint for values available
      
      # Chi2 test
      
      test_chi <- chisq.test(table)
      
      # Proportion: vertical %
      
      vmatrix <- matrix(nrow = length(levels), ncol = 2,
                        data = NA)
      
      for (i in 1:length(levels)) {
        
        vmatrix[i, 1] <- table[i, 1]/sum(table[, 1])
        vmatrix[i, 2] <- table[i, 2]/sum(table[, 2])
      }
      
      # Flag if the the differences in the % are more than xx%
      
      flag <- matrix(nrow = length(levels), ncol = 3,
                     data = NA)
      
      if (any(is.na(vmatrix))) {flag <- flag} else{
        
        for (i in 1:length(levels)) {
          
          if (abs(vmatrix[i, 1] - vmatrix[i, 2]) >= 0.05) {flag[i, 1] <- "*"} else {flag[i, 1] <- ""}
          if (abs(vmatrix[i, 1] - vmatrix[i, 2]) >= 0.10) {flag[i, 2] <- "**"} else {flag[i, 2] <- ""}
          if (abs(vmatrix[i, 1] - vmatrix[i, 2]) >= 0.20) {flag[i, 3] <- "***"} else {flag[i, 3] <- ""}
        }
      }
      
      ## Put all results together
      
      # cbind
      
      result <- cbind(c(titles[v], rep("", times = (length(levels)-1))),
                      c(risk_titles[var], rep("", times = length(levels)-1)),
                      risk_names[[var]], table, round(vmatrix*100, digits = 2),
                      c(test_chi[[3]], rep("", times = (length(levels)-1))),
                      flag)
      
      # Format as df and colnames
      
      result <- as.data.frame(result)
      
      colnames(result) <- c("Vaccine", "Characteristic", "Levels",
                            "Data Available (n)", "Data Not Available (n)",
                            "Data Available (%)", "Data Not Availanle (%)",
                            "Chi2", "Flag 5%", "Flag 10%", "Flag 20%")
      
      ## Add factor to previous results of that vaccine
      
      result_vaccine <- rbind(result_vaccine, result)
    }
  }
  
  
  ## Add results of each vaccine to the rest
  
  results_all <- rbind(results_all, result_vaccine)
}

rm(v, var, i, levels, table, test_chi, vmatrix, result, result_vaccine)

write.csv(results_all,
          file = "Demographics/Basic tables/sensitivity_vax_data_availability.csv",
          row.names = FALSE)

## Compare the demographics of children with and without vax date data
# IMP: Among all of those vaccinated

# Vector to store all results

results_all <- data.frame()

# Loop

for (v in 1:length(vaccines)) {
  
  
  # Subset to children vaccinated with that vaccine
  
  data_subset <- analysis_main[analysis_main[[vaccines[v]]] >= 1 & !is.na(analysis_main[[vaccines[v]]]),]
  
  # Binary indicator for missing information
  
  data_subset <- data_subset %>%
    filter(vaccine == vaccines[v]) %>%
    select(age_at_vax, all_of(risk_factors))
  
  data_subset$missing <- NA
  data_subset$missing[!is.na(data_subset$age_at_vax)] <- 0
  data_subset$missing[is.na(data_subset$age_at_vax)] <- 1
  
  # Vector to concatenate results
  
  result_vaccine <- data.frame()
  
  
  ## Loop through every variable
  
  for (var in 1:length(risk_factors)) {
    
    levels <- unique(data_subset[[risk_factors[var]]])
    levels <- levels[!is.na(levels)]
    levels <- sort(levels)
    
    ## Contingency table
    
    table <- table(data_subset[[risk_factors[var]]], data_subset[["missing"]])
    
    if(dim(table)[1] > 0) { # Checkpoint for values available
      
      # Chi2 test
      
      test_chi <- chisq.test(table)
      
      # Proportion: vertical %
      
      vmatrix <- matrix(nrow = length(levels), ncol = 2,
                        data = NA)
      
      for (i in 1:length(levels)) {
        
        vmatrix[i, 1] <- table[i, 1]/sum(table[, 1])
        vmatrix[i, 2] <- table[i, 2]/sum(table[, 2])
      }
      
      # Flag if the the differences in the % are more than xx%
      
      flag <- matrix(nrow = length(levels), ncol = 3,
                     data = NA)
      
      if (any(is.na(vmatrix))) {flag <- flag} else{
        
        for (i in 1:length(levels)) {
          
          if (abs(vmatrix[i, 1] - vmatrix[i, 2]) >= 0.05) {flag[i, 1] <- "*"} else {flag[i, 1] <- ""}
          if (abs(vmatrix[i, 1] - vmatrix[i, 2]) >= 0.10) {flag[i, 2] <- "**"} else {flag[i, 2] <- ""}
          if (abs(vmatrix[i, 1] - vmatrix[i, 2]) >= 0.20) {flag[i, 3] <- "***"} else {flag[i, 3] <- ""}
        }
      }
      
      ## Put all results together
      
      # cbind
      
      result <- cbind(c(titles[v], rep("", times = (length(levels)-1))),
                      c(risk_titles[var], rep("", times = length(levels)-1)),
                      risk_names[[var]], table, round(vmatrix*100, digits = 2),
                      c(test_chi[[3]], rep("", times = (length(levels)-1))),
                      flag)
      
      # Format as df and colnames
      
      result <- as.data.frame(result)
      
      colnames(result) <- c("Vaccine", "Characteristic", "Levels",
                            "Date Available (n)", "Date Not Available (n)",
                            "Date Available (%)", "Date Not Availanle (%)",
                            "Chi2", "Flag 5%", "Flag 10%", "Flag 20%")
      
      ## Add factor to previous results of that vaccine
      
      result_vaccine <- rbind(result_vaccine, result)
    }
  }
  
  
  ## Add results of each vaccine to the rest
  
  results_all <- rbind(results_all, result_vaccine)
}

rm(v, var, i, levels, table, test_chi, vmatrix, result, result_vaccine)

write.csv(results_all,
          file = "Demographics/Basic tables/sensitivity_vax_date_availability.csv",
          row.names = FALSE)

##### Plot #####

sensi_vax <- read.csv("Demographics/Basic tables/sensitivity_vax_data_availability.csv")
sensi_dat <- read.csv("Demographics/Basic tables/sensitivity_vax_date_availability.csv")

colnames(sensi_vax) <- c("Vaccine", "Characteristic", "Levels",
                         "Vaccination Data Available (n)", "Vaccination Data Not Available (n)",
                         "Vaccination Data Available (%)", "Vaccination Data Not Available (%)",
                         "Chi2", "Flag 5%", "Flag 10%", "Flag 20%")

colnames(sensi_dat) <- c("Vaccine", "Characteristic", "Levels",
                         "Vaccination Age Available (n)", "Vaccination Age Not Available (n)",
                         "Vaccination Age Available (%)", "Vaccination Age Not Available (%)",
                         "Chi2", "Flag 5%", "Flag 10%", "Flag 20%")

## Plots for easy look

# For all the data, vaccine availability

plot <- sensi_vax %>%
  mutate(combi = paste0(Characteristic, " - ", Levels))

order <- unique(plot$combi)

p <- ggplot(data = plot, mapping = aes(x = `Vaccination Data Available (%)`,
                                       y = `Vaccination Data Not Available (%)`)) +
  geom_point(mapping = aes(color = combi)) +
  geom_abline(slope = 1, intercept = 0, color = "darkgrey") +
  scale_color_manual(breaks = order,
                     values = c(rep(carto_pal(name = "Safe")[1], times = 2),
                                rep(carto_pal(name = "Safe")[10], times = 5),
                                rep(carto_pal(name = "Safe")[3], times = 2),
                                rep(carto_pal(name = "Safe")[8], times = 3),
                                rep(carto_pal(name = "Emrld")[3], times = 5),
                                rep(carto_pal(name = "Safe")[7], times = 4),
                                rep(carto_pal(name = "Peach")[1], times = 3),
                                rep(carto_pal(name = "Safe")[7], times = 1),
                                rep("lightgrey", times = 5))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title = element_text(face = "bold"))

# For all the data, date availability

plot <- sensi_dat %>%
  mutate(combi = paste0(Characteristic, " - ", Levels))

order <- unique(plot$combi)

p2 <- ggplot(data = plot, mapping = aes(x = `Vaccination Age Available (%)`,
                                        y = `Vaccination Age Not Available (%)`)) +
  geom_point(mapping = aes(color = combi)) +
  geom_abline(slope = 1, intercept = 0, color = "darkgrey") +
  scale_color_manual(breaks = order,
                     values = c(rep(carto_pal(name = "Safe")[1], times = 2),
                                rep(carto_pal(name = "Safe")[10], times = 5),
                                rep(carto_pal(name = "Safe")[3], times = 2),
                                rep(carto_pal(name = "Safe")[8], times = 3),
                                rep(carto_pal(name = "Emrld")[3], times = 5),
                                rep(carto_pal(name = "Safe")[7], times = 4),
                                rep(carto_pal(name = "Peach")[1], times = 3),
                                rep(carto_pal(name = "Safe")[7], times = 1),
                                rep("lightgrey", times = 5))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title = element_text(face = "bold"))

# Export as one figure

png(filename = "Demographics/SuppFigure3_ComparablePop.png",
    height = 12, width = 10, units = "in", res = 1200)
ggarrange(p, p2, ncol = 1, common.legend = TRUE, legend = "bottom", labels = c("A", "B"))
dev.off()

pdf(file = "Demographics/SuppFigure3_ComparablePop.pdf",
    height = 12, width = 10)
ggarrange(p, p2, ncol = 1, common.legend = TRUE, legend = "bottom", labels = c("A", "B"))
dev.off()

