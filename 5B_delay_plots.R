#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Timeliness of 24 childhood immunisations and evolution of vaccination delay:#
#-#-#-#-#- analysis of data from 54 low- and middle-income countries -#-#-#-#-#


# 5/ Vaccination delay



# SET UP ------------------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(rcartocolor)
library(readr)       # parse_number
library(ggridges)
library(ggpubr)
library(DescTools)  # MixColor



# DATA -----------------------------------------------------------------


#### Buffer ####

# How much time are we allowing between recomm date of vax and survey?

bufflist_cutoff <- c(52, 104)
bufflist_folders <- c("Buffer Time 1-year Threshold",
                      "Buffer Time 2-year Threshold")

buff_index <- 1L

buffer <- bufflist_cutoff[buff_index] 
buffer_folder <- bufflist_folders[buff_index]


#### Data ####

## Summary delay table per birth cohort

plot_birth <- read.csv(paste0("Results_DelayVaccination/", buffer_folder, "/Tables/Birth_cohort_delay_vaccination.csv"))

## Individual data

analysis_main <- readRDS("Data/Analysis_FinalDataset_IndividualData.Rds")

# Prepare for plot

plot_delay <- analysis_main %>%
  filter(t_buffer_rec_to_survey >= buffer) %>%
  select(iso3, country, dob_year,
         vaccine, delay) %>%
  filter(!is.na(delay)) %>%
  mutate(course = case_when(vaccine == "bcg" ~ "BCG",
                            vaccine == "dtp1" ~ "DTP", vaccine == "dtp2" ~ "DTP", vaccine == "dtp3" ~ "DTP",
                            vaccine == "hepB0" ~ "HepB", vaccine == "hepB1" ~ "HepB", vaccine == "hepB2" ~ "HepB", vaccine == "hepB3" ~ "HepB",
                            vaccine == "hib1" ~ "Hib", vaccine == "hib2" ~ "Hib", vaccine == "hib3" ~ "Hib",
                            vaccine == "ipv1" ~ "IPV", vaccine == "ipv2" ~ "IPV",
                            vaccine == "mcv1" ~ "MCV", vaccine == "mcv2" ~ "MCV",
                            vaccine == "pneumo1" ~ "PCV", vaccine == "pneumo2" ~ "PCV", vaccine == "pneumo3" ~ "PCV",
                            vaccine == "opv0" ~ "OPV", vaccine == "opv1" ~ "OPV", vaccine == "opv2" ~ "OPV", vaccine == "opv3" ~ "OPV", 
                            vaccine == "rota1" ~ "RV", vaccine == "rota2" ~ "RV", vaccine == "rota3" ~ "RV")) %>%
  mutate(dose = abs(parse_number(vaccine))) %>%
  mutate(dose = case_when(!is.na(dose) ~ dose, is.na(dose) ~ 0)) %>%
  filter(vaccine != "ipv2")


#### Variables ####

vaccines <- c("bcg", "dtp1", "dtp2", "dtp3", "opv0", "opv1", "opv2", "opv3",
              "ipv1", "mcv1", "mcv2",
              "hepB0", "hepB1", "hepB2", "hepB3", "hib1", "hib2", "hib3",
              "pneumo1", "pneumo2", "pneumo3", "rota1", "rota2", "rota3")

vaccine_vars <- c("BCG",
                  "DTP-D1", "DTP-D2", "DTP-D3",
                  "HepB-BD", "HepB-D1", "HepB-D2", "HepB-D3",
                  "Hib-D1", "Hib-D2", "Hib-D3", "IPV-D1",
                  "MCV-D1", "MCV-D2", "OPV-BD", "OPV-D1", "OPV-D2", "OPV-D3",
                  "PCV-D1", "PCV-D2", "PCV-D3", "RV-D1", "RV-D2", "RV-D3")

# Equivalence to match vaccines names

equivalence <- data.frame(vaccine = sort(vaccines),
                          new_vax = sort(vaccine_vars))

plot_delay <- merge(plot_delay, equivalence) %>%
  mutate(vaccine = new_vax) %>%
  select(-new_vax)

# Countries

countries <- unique(plot_delay$country)



# PLOTS -------------------------------------------------------------------


## Options

colors <- c(carto_pal(name = "OrYel")[4],
            carto_pal(name = "Safe")[4], carto_pal(name = "Emrld")[3], carto_pal(name = "Emrld")[2], # DTP in greens
            carto_pal(name = "Safe")[8], carto_pal(name = "SunsetDark")[7], carto_pal(name = "SunsetDark")[6], carto_pal(name = "SunsetDark")[5], # HepB in sunsets
            carto_pal(name = "Earth")[1], carto_pal(name = "Earth")[2], carto_pal(name = "Earth")[3], # Hib in Earthy
            carto_pal(name = "Safe")[5], # IPV  in blue too
            carto_pal(name = "Burg")[7], carto_pal(name = "Burg")[5], # MCV in wine
            carto_pal(name = "Safe")[6], carto_pal(name = "Safe")[5], carto_pal(name = "Safe")[11], carto_pal(name = "Safe")[1], # OPV in blues
            carto_pal(name = "Prism")[12], carto_pal(name = "Safe")[12], carto_pal(name = "Vivid")[12], # PCV in greys
            carto_pal(name = "Mint")[7], carto_pal(name = "Mint")[5], carto_pal(name = "Mint")[3]) # RV in minty


#### ggridges ####

override.linetype <- c(1, 1, 3, 5, 1, 1, 3, 5,
                       1, 3, 5, 1, 1, 3, 1, 1, 3, 5,
                       1, 3, 5, 1, 3, 5)

paper_plot1 <- ggplot(data = plot_delay,
                      mapping = aes(x = delay, y = course)) +
  geom_vline(xintercept = 0, color = carto_pal(name = "Safe")[9], linetype = 2) +
  geom_density_ridges2(mapping = aes(fill = factor(vaccine, levels = vaccine_vars),
                                     linetype = factor(dose, levels = c("0", "1", "2", "3"))),
                       bandwidth = 0.8, alpha = 0.40, scale = 1, size = 0.2) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(breaks = vaccine_vars, values = colors) +
  scale_linetype_manual(breaks = c("0", "1", "2", "3"), values = c(1, 1, 3, 5)) +
  scale_x_continuous(limits = c(-25, 50)) +
  labs(x = "Delay (weeks)",
       color = "Vaccine", fill = "Vaccine", linetype = "Vaccine") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        title = element_text(size = rel(0.80)),
        axis.title.x = element_text(size = rel(1.2)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 90, size = rel(1.4), hjust = 0),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.3, "cm"),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE,
                             override.aes = list(linetype = override.linetype)),
         linetype = "none")

# Saving in a couple of options

png(filename = paste0("Results_DelayVaccination/", buffer_folder, "/Figure2_Delay_AllVaccines.png"),
    height = 9, width = 7, units = "in", res = 1200)
print(paper_plot1)
dev.off()
pdf(file = paste0("Results_DelayVaccination/", buffer_folder, "/Figure2_Delay_AllVaccines.pdf"),
    height = 9, width = 7)
print(paper_plot1)
dev.off()

tiff(filename = paste0("Results_DelayVaccination/", buffer_folder, "/Figure2_Delay_AllVaccines.tiff"),
    height = 8.75, width = 6.8, units = "in", res = 600, compression = "lzw")
print(paper_plot1)
dev.off()


#### Ribbon ####

# Prepare data

plot_birth <- plot_birth %>%
  filter(birth_cohort >= 2008 & birth_cohort < 2022) %>%
  filter(!is.na(median))
  
## Do the plot for each vaccine in a loop

plot_list <- list()

for (v in 1:length(vaccine_vars)) {
  
  plot_ribbon <- plot_birth %>%
    filter(vaccine == vaccine_vars[v])
  
  colors_plot <- c(MixColor(colors[v], "white", 0.25),
                   MixColor(colors[v], "white", 0.50),
                   MixColor(colors[v], "white", 0.75),
                   MixColor(colors[v], "white", 0.95))
  
  p <- ggplot(data = plot_ribbon, mapping = aes(x = birth_cohort, y = median)) +
    geom_hline(yintercept = 0, color = carto_pal(name = "Safe")[9], linetype = 2) +
    geom_ribbon(mapping = aes(ymin = quan_01, ymax = quan_99, fill = "p99"), alpha = 0.6) +
    geom_ribbon(mapping = aes(ymin = quan_05, ymax = quan_95, fill = "p95"), alpha = 0.6) +
    geom_ribbon(mapping = aes(ymin = quan_15, ymax = quan_85, fill = "p85"), alpha = 0.6) +
    geom_ribbon(mapping = aes(ymin = quan_25, ymax = quan_75, fill = "iqr"), alpha = 0.6) +
    geom_line(mapping = aes(y = median, linetype = "median")) +
    geom_line(mapping = aes(y = mean, linetype = "mean")) +
    scale_fill_manual(breaks = c("p99", "p95", "p85", "iqr"),
                      values = colors_plot,
                      labels = c("1% - 99% p", "5% - 95% p", "15% - 85% p", "25% - 75% p")) +
    scale_linetype_manual(breaks = c("median", "mean"),
                          values = c("solid", "dashed"),
                          labels = c("Median", "Mean")) +
    coord_cartesian(ylim = c(-5, 20)) +
    labs(title = paste0(vaccine_vars[v]),
         x = "Birth cohort", y = "Vaccination delay (weeks)") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold"),
          axis.title = element_text(size = rel(1.2)),
          axis.text = element_text(size = rel(1.2)),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = rel(1.2)))
  
  plot_list[[v]] <- p
}

rm(p, v)

## Combi figure

plot <- list(ggplot() + theme_minimal())

plot_list1 <- c(plot_list[1], plot, plot, plot, plot_list[c(2:4)], plot,
                plot_list[c(5:11)])
paper_plot2a <- ggarrange(plotlist = plot_list1, common.legend = TRUE, align = "hv", legend = "bottom")

plot_list2 <- c(plot_list[c(12)], plot, plot_list[c(13:14)], plot_list[(15:18)],
                plot_list[(19:21)], plot, plot_list[c(22:24)], plot)
paper_plot2b <- ggarrange(plotlist = plot_list2, common.legend = TRUE, align = "hv", legend = "bottom")

# Saving in a couple of options

png(filename = paste0("Results_DelayVaccination/", buffer_folder, "/Figure3a_Delay_Ribbon.png"),
    height = 12, width = 15, units = "in", res = 1200)
print(paper_plot2a)
dev.off()
pdf(file = paste0("Results_DelayVaccination/", buffer_folder, "/Figure3a_Delay_Ribbon.pdf"),
    height = 12, width = 15)
print(paper_plot2a)
dev.off()

png(filename = paste0("Results_DelayVaccination/", buffer_folder, "/Figure3b_Delay_Ribbon.png"),
    height = 12, width = 15, units = "in", res = 1200)
print(paper_plot2b)
dev.off()
pdf(file = paste0("Results_DelayVaccination/", buffer_folder, "/Figure3b_Delay_Ribbon.pdf"),
    height = 12, width = 15)
print(paper_plot2b)
dev.off()

## For the paper

plot_list <- list()

for (v in 1:length(vaccine_vars)) {
  
  plot_ribbon <- plot_birth %>%
    filter(vaccine == vaccine_vars[v])
  
  colors_plot <- c(MixColor(colors[v], "white", 0.25),
                   MixColor(colors[v], "white", 0.50),
                   MixColor(colors[v], "white", 0.75),
                   MixColor(colors[v], "white", 0.95))
  
  p <- ggplot(data = plot_ribbon, mapping = aes(x = birth_cohort, y = median)) +
    geom_hline(yintercept = 0, color = carto_pal(name = "Safe")[9], linetype = 2) +
    geom_ribbon(mapping = aes(ymin = quan_01, ymax = quan_99, fill = "p99"), alpha = 0.6) +
    geom_ribbon(mapping = aes(ymin = quan_05, ymax = quan_95, fill = "p95"), alpha = 0.6) +
    geom_ribbon(mapping = aes(ymin = quan_15, ymax = quan_85, fill = "p85"), alpha = 0.6) +
    geom_ribbon(mapping = aes(ymin = quan_25, ymax = quan_75, fill = "iqr"), alpha = 0.6) +
    geom_line(mapping = aes(y = median, linetype = "median")) +
    geom_line(mapping = aes(y = mean, linetype = "mean")) +
    scale_fill_manual(breaks = c("p99", "p95", "p85", "iqr"),
                      values = colors_plot,
                      labels = c("1% - 99% p", "5% - 95% p", "15% - 85% p", "25% - 75% p")) +
    scale_linetype_manual(breaks = c("median", "mean"),
                          values = c("solid", "dashed"),
                          labels = c("Median", "Mean")) +
    coord_cartesian(ylim = c(-5, 20)) +
    labs(title = paste0(vaccine_vars[v]),
         x = "Birth cohort", y = "Vaccination delay (weeks)") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", size = rel(0.8)),
          axis.title = element_text(size = rel(0.7)),
          axis.text = element_text(size = rel(0.7)),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = rel(0.7)))
  
  plot_list[[v]] <- p
}

rm(p, v)

plot <- list(ggplot() + theme_minimal())

plot_list1 <- c(plot_list[1], plot, plot, plot, plot_list[c(2:4)], plot,
                plot_list[c(5:11)])
paper_plot2a <- ggarrange(plotlist = plot_list1, common.legend = TRUE, align = "hv", legend = "bottom")

tiff(filename = paste0("Results_DelayVaccination/", buffer_folder, "/Figure3a_Delay_Ribbon.tiff"),
     height = 7, width = 8.75, units = "in", res = 600, compression = "lzw")
print(paper_plot2a)
dev.off()

plot_list2 <- c(plot_list[c(12)], plot, plot_list[c(13:14)], plot_list[(15:18)],
                plot_list[(19:21)], plot, plot_list[c(22:24)], plot)
paper_plot2b <- ggarrange(plotlist = plot_list2, common.legend = TRUE, align = "hv", legend = "bottom")

tiff(filename = paste0("Results_DelayVaccination/", buffer_folder, "/Figure3b_Delay_Ribbon.tiff"),
     height = 7, width = 8.75, units = "in", res = 600, compression = "lzw")
print(paper_plot2b)
dev.off()
  

#### Extra ####
  
# Store results
  
dir.create(paste0("Results_DelayVaccination/", buffer_folder, "/By_country_Figure2"))
  
# Do in a loop for each country
  
for (c in 1:length(countries)) {
  
  paper_plot1 <- ggplot(data = filter(plot_delay, country == countries[c]),
                        mapping = aes(x = delay, y = course)) +
    geom_vline(xintercept = 0, color = carto_pal(name = "Safe")[9], linetype = 2) +
    geom_density_ridges2(mapping = aes(fill = factor(vaccine, levels = vaccine_vars),
                                       linetype = factor(dose, levels = c("0", "1", "2", "3"))),
                         bandwidth = 0.8, alpha = 0.40, scale = 1, size = 0.2) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(breaks = vaccine_vars, values = colors) +
    scale_linetype_manual(breaks = c("0", "1", "2", "3"), values = c(1, 1, 3, 5)) +
    scale_x_continuous(limits = c(-25, 50)) +
    labs(title = paste0(countries[c]), x = "Delay (weeks)",
         color = "Vaccine", fill = "Vaccine", linetype = "Vaccine") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold"),
          title = element_text(size = rel(0.80)),
          axis.title.x = element_text(size = rel(1.2)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.title.y = element_blank(),
          axis.text.y = element_text(angle = 90, size = rel(1.5), hjust = 0),
          axis.ticks.y = element_blank(),
          legend.title = element_blank(),
          legend.key.size = unit(0.3, "cm"),
          legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE),
           linetype = "none")
  
  # Saving in a couple of options
  
  png(filename = paste0("Results_DelayVaccination/", buffer_folder, "/By_country_Figure2/", countries[c], "_Figure2_Delay_AllVaccines.png"),
      height = 9, width = 7, units = "in", res = 1200)
  print(paper_plot1)
  dev.off()
  pdf(file = paste0("Results_DelayVaccination/", buffer_folder, "/By_country_Figure2/", countries[c], "_Figure2_Delay_AllVaccines.pdf"),
      height = 9, width = 7)
  print(paper_plot1)
  dev.off()
}
  


# SUPP TABLES -------------------------------------------------------------


## Creating a supp table with delay per country and vaccine

year_country <- read.csv("Demographics/Basic tables/year_country.csv")

## Getting vax delay

delay_all <- read.csv(paste0("Results_DelayVaccination/", buffer_folder, "/Tables/All_delay_vaccination.csv"))

# Prep

delay_all <- delay_all %>%
  select(country, vaccine, median, iqr) %>%
  mutate(median_iqr = paste0(median, " ", iqr)) %>%
  select(country, vaccine, median_iqr)

## Getting vax delay by country

delay_country <- read.csv(paste0("Results_DelayVaccination/", buffer_folder, "/Tables/Country_delay_vaccination.csv"))

# Prep

delay_country <- delay_country %>%
  select(country, vaccine, median, iqr) %>%
  mutate(median_iqr = paste0(median, " ", iqr)) %>%
  select(country, vaccine, median_iqr) %>%
  merge(year_country) %>%
  mutate(country = country_year) %>%
  select(-c(country_year, max_year))

## All together

delay_together <- rbind(delay_all, delay_country) %>%
  pivot_wider(names_from = "vaccine", values_from = "median_iqr")

write.csv(delay_together, file = paste0("Results_DelayVaccination/", buffer_folder, "/SuppTable5_DelayCountry.csv"), row.names = FALSE)

