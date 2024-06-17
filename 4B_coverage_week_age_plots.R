#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Timeliness of 24 childhood immunisations and evolution of vaccination delay:#
#-#-#-#-#- analysis of data from 54 low- and middle-income countries -#-#-#-#-#


# 4/ Coverage per week of age



# SET UP ------------------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(readr)       # parse_number
library(rcartocolor)
library(patchwork)



# DATA --------------------------------------------------------------------


#### Buffer ####

# How much time are we allowing between recomm date of vax and survey?

bufflist_cutoff <- c(52, 104)
bufflist_folders <- c("Buffer Time 1-year Threshold",
                      "Buffer Time 2-year Threshold")

buff_index <- 1L

buffer <- bufflist_cutoff[buff_index] 
buffer_folder <- bufflist_folders[buff_index]


#### Cum vax data ####

# Aggregated result of cum proportion of children vaccinated

result_all <- read.csv(file = paste0("Results_CoverageWeekAge/", buffer_folder, "/Tables/All_cum_vaccination.csv"))

# Aggregated result of cum proportion of children vaccinated (by country)

result_country <- read.csv(file = paste0("Results_CoverageWeekAge/", buffer_folder, "/Tables/Country_cum_vaccination.csv"))


#### Variables ####

vaccine_vars <- c("BCG",
                  "DTP-D1", "DTP-D2", "DTP-D3",
                  "HepB-BD", "HepB-D1", "HepB-D2", "HepB-D3",
                  "Hib-D1", "Hib-D2", "Hib-D3", "IPV-D1",
                  "MCV-D1", "MCV-D2", "OPV-BD", "OPV-D1", "OPV-D2", "OPV-D3",
                  "PCV-D1", "PCV-D2", "PCV-D3", "RV-D1", "RV-D2", "RV-D3")

countries <- unique(result_country$country)



# PLOTS ------------------------------------------------------------------


#### Country Age-coverage ####

plot_data <- result_all %>% rbind(result_country)

# Subset and prepare data for plot

plot_data <- plot_data %>%
  pivot_longer(cols = -c(country, vaccine),
               names_to = "week", values_to = "proportion") %>%
  mutate(proportion = round(proportion, digits = 1)) %>%
  mutate(prop_group = case_when(proportion < 20 | proportion > 80 ~ "clear",
                                proportion >= 20 & proportion <= 80 ~ "normal"))

# Recomm vaccination ages (per country)

recomm <- read.csv("Data/WHO_schedules_reference.csv")

recomm[recomm == "Democratic Republic of the Congo"] <- "Congo Democratic Republic"
recomm[recomm == "Côte d'Ivoire"] <- "Cote d'Ivoire"
recomm[recomm == "Kyrgyzstan"] <- "Kyrgyz Republic"
recomm[recomm == "United Republic of Tanzania"] <- "Tanzania"
recomm[recomm == "Türkiye"] <- "Turkey"

recomm <- recomm %>%
  select(country, vaccine, recomm_weeks) %>%
  filter(country %in% plot_data$country) %>%
  filter(vaccine != "IPV-2" & vaccine != "IPV-3" & vaccine != "DTP-4" & vaccine != "OPV-4" &
           vaccine != "PCV-4" & vaccine != "HepB-4" & vaccine != "IPV-4") %>%
  group_by(country, vaccine) %>%
  mutate(recomm_weeks = min(recomm_weeks, na.rm = TRUE)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  arrange(country, vaccine)

# Adding general WHO recommendations to the per-country ones

who_recomm <- c(1, # Recomm for BCG
                6, 10, 14, # Recomm for DTP
                1, 6, 10, 14, # Recomm for Hep
                6, 10, 14, # Recomm for Hib
                6, # IPV
                39, 65, # Recomm for MCV
                1, 6, 10, 14, # Recomm for OPV
                6, 10, 14, # Recomm for PCV
                6, 10, 14) # Recomm for RV

add <- data.frame(country = rep("All", times = 24),
                  vaccine = vaccine_vars,
                  recomm_weeks = who_recomm) 
recomm <- rbind(add, recomm)

# Getting the coverages at the ages of interest for each vaccine & country:
# recomm age (for each vaccine and country) + 2w/4w/6w/8w/4mo/6mo late

plot_data <- merge(plot_data, recomm, all = TRUE)
plot_data$recomm_weeks[plot_data$recomm_weeks == 0] <- 1

rm(add, recomm)

plot_data <- plot_data %>%
  mutate(recomm_weeks = round(recomm_weeks)) %>%
  mutate(stop1 = paste0("vax_week_", recomm_weeks+0)) %>%
  mutate(stop2 = paste0("vax_week_", recomm_weeks+2)) %>%
  mutate(stop3 = paste0("vax_week_", recomm_weeks+4)) %>%
  mutate(stop4 = paste0("vax_week_", recomm_weeks+6)) %>%
  mutate(stop5 = paste0("vax_week_", recomm_weeks+8)) %>%
  mutate(stop6 = paste0("vax_week_", recomm_weeks+17)) %>%
  mutate(stop7 = paste0("vax_week_", recomm_weeks+26)) %>%
  mutate(stop8 = paste0("vax_week_260"))  %>%
  filter(week == stop1 | week == stop2 | week == stop3 | week == stop4 |
           week == stop5 | week == stop6 | week == stop7 | week == stop8)

# Preparing the data

plot_data$week <- as.numeric(parse_number(plot_data$week))

plot_data <- plot_data %>%
  arrange(country, vaccine, week) %>%
  group_by(country, vaccine) %>%
  mutate(plot_order = rank(week)) %>%
  ungroup() %>%
  select(country, vaccine, proportion, prop_group, week, plot_order)

## Do plot in a loop

# Objects to store the results

plot_list <- list()

for (v in 1:length(vaccine_vars)) {
  
  message(v)
  
  # Create a mini-data subset for each vaccine
  
  data_subset <- plot_data %>%
    filter(vaccine == vaccine_vars[v])
  
  # Plot and save
  
  plot_list[[v]] <- local({
    
    v <- v
    
    ggplot(data = data_subset,
           mapping = aes(x = as.factor(plot_order), y = country)) +
      geom_tile(mapping = aes(fill = proportion), color = "white") +
      geom_text(mapping = aes(label = proportion, color = prop_group),
                size = 1.7) +
      scale_x_discrete(labels = c("Recomm. age", "2 w later", "4 w later", "6 w later",
                                  "8 w later", "4 m later", "6 m later", "Final")) +
      scale_y_discrete(limits = rev) +
      scale_color_manual(breaks = c("clear", "normal"),
                         values = c("white", "black")) +
      scale_fill_carto_c(palette = "Geyser", direction = -1, limits = c(0, 100),
                         name = "Coverage per week of age") +
      guides(color = "none",
             x =  guide_axis(angle = 45)) +
      coord_fixed() +
      labs(title = paste0(vaccine_vars[v])) +
      theme(plot.title = element_text(face = "bold"),
            axis.title = element_blank(),
            axis.text.x = element_text(size = rel(0.7)),
            legend.text = element_text(size = rel(0.7)),
            legend.title = element_text(size = rel(0.8), face = "bold"),
            legend.key.height = unit(0.4,"line"),
            legend.key.width = unit(5, "line"),
            legend.spacing.x = unit(1.0, 'cm')) +
      if(vaccine_vars[v] == "BCG" | vaccine_vars[v] == "Hib-D1" | vaccine_vars[v] == "OPV-D2"){
        theme()
      } else {
        theme(axis.text.y = element_blank())
      }
  })
  
  
}

# Clean the space

rm(v, data_subset)

## Combining

paper_plot1 <- wrap_plots(plot_list[1:8], nrow = 1) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

paper_plot2 <- wrap_plots(plot_list[9:16], nrow = 1) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

paper_plot3 <- wrap_plots(plot_list[17:24], nrow = 1) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Saving with a couple of options

png(filename = paste0("Results_CoverageWeekAge/", buffer_folder, "/SuppFigure4a_AgeCoverage1.png"),
    width = 15, height = 12, units = "in", res = 1200)
print(paper_plot1)
dev.off()
pdf(file = paste0("Results_CoverageWeekAge/", buffer_folder, "/SuppFigure4a_AgeCoverage1.pdf"),
    width = 15, height = 12)
print(paper_plot1)
dev.off()

png(filename = paste0("Results_CoverageWeekAge/", buffer_folder, "/SuppFigure4b_AgeCoverage2.png"),
    width = 15, height = 12, units = "in", res = 1200)
print(paper_plot2)
dev.off()
pdf(file = paste0("Results_CoverageWeekAge/", buffer_folder, "/SuppFigure4b_AgeCoverage2.pdf"),
    width = 15, height = 12)
print(paper_plot2)
dev.off()

png(filename = paste0("Results_CoverageWeekAge/", buffer_folder, "/SuppFigure4c_AgeCoverage3.png"),
    width = 15, height = 12, units = "in", res = 1200)
print(paper_plot3)
dev.off()
pdf(file = paste0("Results_CoverageWeekAge/", buffer_folder, "/SuppFigure4c_AgeCoverage3.pdf"),
    width = 15, height = 12)
print(paper_plot3)
dev.off()


#### Age-coverage ####

# Only the overall (all countries) estimates

data_subset <- plot_data %>% filter(country == "All")

# Plot

p <- ggplot(data = data_subset,
            mapping = aes(x = as.factor(plot_order),
                          y = vaccine)) +
  geom_tile(mapping = aes(fill = proportion), color = "white") +
  geom_text(mapping = aes(label = proportion, color = prop_group),
            size = 3) +
  scale_x_discrete(breaks = as.factor(seq(1:8)),
                   labels = c("WHO recomm", "2 we later", "4 we later", "6 we later",
                              "8 we later", "4 mo later", "6 mo later", "Final")) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(breaks = c("clear", "normal"),
                     values = c("white", "black")) +
  scale_fill_carto_c(palette = "Geyser", direction = -1, limits = c(0, 100),
                     name = "Coverage per week of age") +
  guides(color = "none",
         x =  guide_axis(angle = 45)) +
  coord_fixed() +
  labs(title = NULL) +
  theme(plot.title = element_text(face = "bold"),
        axis.title = element_blank(),
        legend.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8), face = "bold"),
        legend.key.height = unit(0.4,"line"),
        legend.key.width = unit(3, "line"),
        legend.spacing.x = unit(1.0, 'cm'),
        legend.position = "bottom")

png(filename = paste0("Results_CoverageWeekAge/", buffer_folder, "/Figure1_AgeCoverage.png"),
    width = 6, height = 10, units = "in", res = 1200)
print(p)
dev.off()
pdf(file = paste0("Results_CoverageWeekAge/", buffer_folder, "/Figure1_AgeCoverage.pdf"),
    width = 6, height = 10)
print(p)
dev.off()


#### Extra ####

# Store results

dir.create(paste0("Results_CoverageWeekAge/", buffer_folder, "/By_country_Figure1"))

# Only country-specific

data_subset <- plot_data %>% filter(country != "All")

# Plot

for (c in 1:length(countries)) {
  
  p <- ggplot(data = filter(data_subset, country == countries[c]),
              mapping = aes(x = as.factor(plot_order),
                            y = vaccine)) +
    geom_tile(mapping = aes(fill = proportion), color = "white") +
    geom_text(mapping = aes(label = proportion, color = prop_group),
              size = 3) +
    scale_x_discrete(breaks = as.factor(seq(1:8)),
                     labels = c("Recomm. age", "2 we later", "4 we later", "6 we later",
                                "8 we later", "4 mo later", "6 mo later", "Final")) +
    scale_y_discrete(limits = rev) +
    scale_color_manual(breaks = c("clear", "normal"),
                       values = c("white", "black")) +
    scale_fill_carto_c(palette = "Geyser", direction = -1, limits = c(0, 100),
                       name = "Coverage per week of age") +
    guides(color = "none",
           x =  guide_axis(angle = 45)) +
    coord_fixed() +
    labs(title = paste0(countries[c])) +
    theme(plot.title = element_text(face = "bold"),
          axis.title = element_blank(),
          legend.text = element_text(size = rel(0.8)),
          legend.title = element_text(size = rel(0.8), face = "bold"),
          legend.key.height = unit(0.4,"line"),
          legend.key.width = unit(3, "line"),
          legend.spacing.x = unit(1.0, 'cm'),
          legend.position = "bottom")
  
  png(filename = paste0("Results_CoverageWeekAge/", buffer_folder, "/By_country_Figure1/", countries[c], "_Figure1_AgeCoverage.png"),
      width = 6, height = 10, units = "in", res = 1200)
  print(p)
  dev.off()
  pdf(file = paste0("Results_CoverageWeekAge/", buffer_folder, "/By_country_Figure1/", countries[c], "_Figure1_AgeCoverage.pdf"),
      width = 6, height = 10)
  print(p)
  dev.off()
  
}

