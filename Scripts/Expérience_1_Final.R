#Outils d'analyse
library(lme4)
library(lmerTest)
library(tidyverse)
library(tidyplots)

#importation du tableau de donnée
full_data <- read.table("Expérience1Souches.txt", header = T, check.names = F)

#Séparation des données en tableau distincts####
initial_mass <- full_data %>% 
  select(c(1:3))
head(initial_mass)

initial_number <- full_data %>% 
  select(c(1,2,4))
head(initial_number)

#Création et enregistrement des différents tableaux à analyser
mass_data <- full_data %>% 
  select(c(5:21)) %>% 
  merge(initial_mass, by = "Rep") %>% 
  pivot_longer(cols = c(4:17,19), values_to = "Mass", names_to = "Time")
head(mass_data)

number_data <- full_data %>% 
  select(5:7,22:35) %>% 
  merge(initial_number, by = "Rep") %>% 
  pivot_longer(cols = c(4:17,19), values_to = "Number", names_to = "Time")
head(number_data)

surv_data <- full_data %>% 
  select(c(36:52)) %>% 
  merge(initial_number, by = "Rep") %>% 
  pivot_longer(cols = c(4:17,19), values_to = "Survival", names_to = "Time")
head(surv_data)  


#Création et enregistrement des différents tableaux à analyser
mean_data <- mass_data %>% 
  merge(number_data, by = c("Rep", "Bloc", "Trt", "Strain", "Time")) %>% 
  mutate(Mean = Mass/Number) 
write_csv(mean_data,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_1_csv/mean_Experiment_1.csv")
write_csv(mean_data,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_1_csv/mean_Experiment_1.csv")

biom_data <- mean_data %>% 
  merge(surv_data, by = c("Rep", "Bloc", "Trt", "Strain", "Time")) %>% 
  mutate(Biomass = Mean * Survival) %>% 
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Strain = as.factor(Strain)) %>% 
  mutate(Trt = as.factor(Trt)) %>% 
  mutate(Bloc = as.factor(Bloc)) %>% 
  mutate(Rep = as.factor(Rep))
write_csv(biom_data,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_1_csv/biom_Experiment_1.csv")
write_csv(biom_data,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_1_csv/biom_Experiment_1.csv")


mort_data <- surv_data %>% 
  mutate(Mortality = (10-Survival)*10) %>% 
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Strain = as.factor(Strain)) %>% 
  mutate(Trt = as.factor(Trt)) %>% 
  mutate(Bloc = as.factor(Bloc)) %>% 
  mutate(Rep = as.factor(Rep)) 
write_csv(mort_data,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_1_csv/mort_Experiment_1.csv")
write_csv(mort_data,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_1_csv/mort_Experiment_1.csv")

growth_data_1 <- mean_data %>% 
  select(-Mass,-Number) %>% 
  pivot_wider(names_from = "Time", values_from = "Mean") 


growth_data_2 <- growth_data_1 %>% 
  mutate(initial_mean = growth_data_1$'0') %>% 
  mutate(final_mean = growth_data_1$'14') %>% 
  mutate("0" = (growth_data_1$'0' - growth_data_1$'0' )) %>% 
  mutate("1" = (growth_data_1$'1' - growth_data_1$'0')) %>%
  mutate("2" = (growth_data_1$'2' - growth_data_1$'0')) %>%
  mutate("3" = (growth_data_1$'3' - growth_data_1$'0')) %>%
  mutate("4" = (growth_data_1$'4' - growth_data_1$'0')) %>% 
  mutate("5" = (growth_data_1$'5' - growth_data_1$'0')) %>%
  mutate("6" = (growth_data_1$'6' - growth_data_1$'0')) %>%
  mutate("7" = (growth_data_1$'7' - growth_data_1$'0')) %>%
  mutate("8" = (growth_data_1$'8' - growth_data_1$'0')) %>% 
  mutate("9" = (growth_data_1$'9' - growth_data_1$'0')) %>% 
  mutate("10" = (growth_data_1$'10' - growth_data_1$'0')) %>%
  mutate("11" = (growth_data_1$'11' - growth_data_1$'0')) %>%
  mutate("12" = (growth_data_1$'12' - growth_data_1$'0')) %>%
  mutate("13" = (growth_data_1$'13' - growth_data_1$'0')) %>% 
  mutate("14" = (growth_data_1$'14' - growth_data_1$'0')) 

growth_data <- growth_data_2 %>% 
  mutate(final_growth = growth_data_2$'14') %>% 
  pivot_longer(cols = c(5:19), names_to = "Time", values_to = "Growth") %>% 
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Strain = as.factor(Strain)) %>% 
  mutate(Trt = as.factor(Trt)) %>% 
  mutate(Bloc = as.factor(Bloc)) %>% 
  mutate(Rep = as.factor(Rep))
write_csv(growth_data,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_1_csv/growth_Experiment_1.csv")
write_csv(growth_data,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_1_csv/growth_Experiment_1.csv")

growth_rate_data <- growth_data_1 %>% 
  mutate("0" = (growth_data_1$'0' - growth_data_1$'0' )) %>% 
  mutate("1" = (growth_data_1$'1' - growth_data_1$'0')) %>%
  mutate("2" = (growth_data_1$'2' - growth_data_1$'1')) %>%
  mutate("3" = (growth_data_1$'3' - growth_data_1$'2')) %>%
  mutate("4" = (growth_data_1$'4' - growth_data_1$'3')) %>% 
  mutate("5" = (growth_data_1$'5' - growth_data_1$'4')) %>%
  mutate("6" = (growth_data_1$'6' - growth_data_1$'5')) %>%
  mutate("7" = (growth_data_1$'7' - growth_data_1$'6')) %>%
  mutate("8" = (growth_data_1$'8' - growth_data_1$'7')) %>% 
  mutate("9" = (growth_data_1$'9' - growth_data_1$'8')) %>% 
  mutate("10" = (growth_data_1$'10' - growth_data_1$'9')) %>%
  mutate("11" = (growth_data_1$'11' - growth_data_1$'10')) %>%
  mutate("12" = (growth_data_1$'12' - growth_data_1$'11')) %>%
  mutate("13" = (growth_data_1$'13' - growth_data_1$'12')) %>% 
  mutate("14" = (growth_data_1$'14' - growth_data_1$'13')) %>% 
  pivot_longer(cols = c(5:19), names_to = "Time", values_to = "Growth") %>% 
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Strain = as.factor(Strain)) %>% 
  mutate(Trt = as.factor(Trt)) %>% 
  mutate(Bloc = as.factor(Bloc)) %>% 
  mutate(Rep = as.factor(Rep))
write_csv(growth_rate_data,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_1_csv/growth_rate_Experiment_1.csv")
write_csv(growth_rate_data,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_1_csv/growth_rate_Experiment_1.csv")

#Graphs####

#Évolution de la mortalité ####
library(tidyplots)
mort_data_sum <- mort_data %>% 
  group_by(Strain, Time, Trt) %>% 
  summarize(Mortality = mean(Mortality)) %>% 
  mutate(Strain = case_when(
    Strain == "All" ~ "Germany", 
    Strain == "Esp" ~ "Spain",
    Strain == "Fra" ~ "France",
    Strain == "Gre" ~ "Greece",
    Strain == "Inv1" ~ "Norway 1",
    Strain == "Inv2" ~ "Norway 2",
    Strain == "Ita1" ~ "Italy 1",
    Strain == "Ita2" ~ "Italy 2",
    Strain == "Tur" ~ "Turkey",
    Strain == "Wor" ~ "Canada",
    T ~ Strain)) %>% 
  mutate(Wound = case_when(
    Trt == "C" ~ "Sterile",
    Trt == "B" ~ "Septic"
  )) %>% 
  mutate(Time = case_when(
    Time == "0" ~ "0",
    Time == "1" ~ "2",
    Time == "2" ~ "3",
    Time == "3" ~ "4",
    Time == "4" ~ "5",
    Time == "5" ~ "6",
    Time == "6" ~ "7",
    Time == "7" ~ "8",
    Time == "8" ~ "9",
    Time == "9" ~ "10",
    Time == "10" ~ "11",
    Time == "11" ~ "12",
    Time == "12" ~ "13",
    Time == "13" ~ "14",
    Time == "14" ~ "15"
    )) %>% 
  mutate( Time = as.integer(Time))
  


mort_data_sum %>% 
  filter(Wound == "Septic") %>% 
  tidyplot(x = Time, y = Mortality, color = Strain) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(colors_discrete_okabeito) %>% 
  adjust_y_axis(limits = c(0,40), title = "Mean cumulative mortality (%)") %>%
  adjust_x_axis(padding = c(0,0), limits = c(0,15.1), title = "Time (Days)") %>% 
  adjust_font(fontsize = 12, family = "serif") %>% 
  adjust_size(width = 200, height = 100) %>% 
  add_annotation_line(x = 8.2, xend = 8.2, y = 34, yend = 35, color = "black") %>% 
  add_annotation_line(x = 7.8, xend = 7.8, y = 34, yend = 35, color = "black") %>%
  add_annotation_line(x = 7.8, xend = 8.2, y = 35, yend = 35, color = "black") %>% 
  add_annotation_line(x = 10, xend = 10, y = 34, yend = 35, color = 'black') %>% 
  add_annotation_line(x = 15, xend = 15, y =34, yend = 35, color = 'black') %>% 
  add_annotation_line(x = 10, xend = 15, y = 35, yend = 35, color = 'black') %>% 
  add_annotation_text(x = 8, y = 35.5, "*", fontsize = 12) %>% 
  add_annotation_text(x = 12.5, y = 35.5, "***", fontsize = 12) %>% 
  save_plot("C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Mortality_Septic_Exp1.tif")

mort_data_sum %>% 
  filter(Wound == "Sterile") %>% 
  tidyplot(x = Time, y = Mortality, color = Strain) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(colors_discrete_okabeito) %>% 
  adjust_y_axis(limits = c(0,40), title = "Mean cumulative mortality (%)") %>%
  adjust_x_axis(padding = c(0,0), limits = c(0,15.1), title = "Time (Days)") %>% 
  adjust_font(fontsize = 12, family = "serif") %>%
  adjust_size(width = 200, height = 100) %>% 
  save_plot("C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Mortality_Sterile_Exp1.tif")




#Mortalité Finale#####

mort_data_final_1 <- mort_data %>% 
  mutate(Strain = case_when(
    Strain == "All" ~ "Germany", 
    Strain == "Esp" ~ "Spain",
    Strain == "Fra" ~ "France",
    Strain == "Gre" ~ "Greece",
    Strain == "Inv1" ~ "Norway 1",
    Strain == "Inv2" ~ "Norway 2",
    Strain == "Ita1" ~ "Italy 1",
    Strain == "Ita2" ~ "Italy 2",
    Strain == "Tur" ~ "Turkey",
    Strain == "Wor" ~ "Canada",
    T ~ Strain)) %>% 
  mutate(Wound = case_when(
    Trt == "C" ~ "Sterile",
    Trt == "B" ~ "Septic"
  )) %>% 
  select(-Survival) %>% 
  mutate(Time = case_when(
    Time == "0" ~ "0",
    Time == "1" ~ "2",
    Time == "2" ~ "3",
    Time == "3" ~ "4",
    Time == "4" ~ "5",
    Time == "5" ~ "6",
    Time == "6" ~ "7",
    Time == "7" ~ "8",
    Time == "8" ~ "9",
    Time == "9" ~ "10",
    Time == "10" ~ "11",
    Time == "11" ~ "12",
    Time == "12" ~ "13",
    Time == "13" ~ "14",
    Time == "14" ~ "15"
  )) %>% 
  mutate( Time = as.integer(Time)) %>% 
  pivot_wider(names_from = "Time", values_from = "Mortality")

mort_data_final <- mort_data_final_1 %>% 
  mutate(final_mort = mort_data_final_1$'15') %>% 
  select(-'2',-'3',-'4',-'5',-'6',-'7',-'8',-'9',-'10',-'11',-'12',-'13',-'14',-'15',-'0')

source("group_by_summary_stats.R")
Stats_mort <- group_by_summary_stats(mort_data_final, final_mort, Strain, Wound) 

write.csv(Stats_mort,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_1_csv/Stats_mort_exp1.csv" )

Stats_mean <- Stats_mort %>% 
  select(Mean, Strain, Wound, SD) %>% 
  ungroup() %>% 
  mutate(Mortality = as.numeric(Mean)) %>% 
  mutate(Wound = as.factor(Wound)) %>% 
  mutate(Strain = as.factor(Strain))

Vector_Mean <- Stats_mean %>% 
  filter(Wound == "Septic") %>% 
  select(-Strain, -Wound, -Mortality)

Vector_plot <- Vector_Mean$Mean +1
Vector_plot1 <- Vector_Mean$Mean + 0.5
Vector_plot2 <- Vector_Mean$Mean + 1.5
Vector_P_value <- c('*','n.s.','*','**','***','***','**','**','*','*')

Mort_final <- mort_data_final %>% 
  tidyplot(x = Strain, y = final_mort, color = Wound) %>% 
  add_mean_bar(alpha = 0.3) %>% 
  add_mean_dash(linewidth = 0.6) %>%
  #add_data_points_jitter(jitter_height = 0) %>%
  #add_violin(alpha = 0) %>% 
  adjust_colors(c("#012456","#096")) %>% 
  adjust_size(width = 200, height = 100) %>% 
  add_annotation_line(x = 0.8, xend = 1.2, y = Vector_plot[1], yend = Vector_plot[1]) %>% 
  add_annotation_line(x = 1.8, xend = 2.2, y = Vector_plot[2], yend = Vector_plot[2]) %>%
  add_annotation_line(x = 2.8, xend = 3.2, y = Vector_plot[3], yend = Vector_plot[3]) %>%
  add_annotation_line(x = 3.8, xend = 4.2, y = Vector_plot[4], yend = Vector_plot[4]) %>%
  add_annotation_line(x = 4.8, xend = 5.2, y = Vector_plot[5], yend = Vector_plot[5]) %>%
  add_annotation_line(x = 5.8, xend = 6.2, y = Vector_plot[6], yend = Vector_plot[6]) %>%
  add_annotation_line(x = 6.8, xend = 7.2, y = Vector_plot[7], yend = Vector_plot[7]) %>%
  add_annotation_line(x = 7.8, xend = 8.2, y = Vector_plot[8], yend = Vector_plot[8]) %>%
  add_annotation_line(x = 8.8, xend = 9.2, y = Vector_plot[9], yend = Vector_plot[9]) %>%
  add_annotation_line(x = 9.8, xend = 10.2, y = Vector_plot[10], yend = Vector_plot[10]) %>% 
  add_annotation_line(x = 0.8, xend = 0.8, y = Vector_plot1[1], yend = Vector_plot[1]) %>% 
  add_annotation_line(x = 1.8, xend = 1.8, y = Vector_plot1[2], yend = Vector_plot[2]) %>% 
  add_annotation_line(x = 2.8, xend = 2.8, y = Vector_plot1[3], yend = Vector_plot[3]) %>% 
  add_annotation_line(x = 3.8, xend = 3.8, y = Vector_plot1[4], yend = Vector_plot[4]) %>% 
  add_annotation_line(x = 4.8, xend = 4.8, y = Vector_plot1[5], yend = Vector_plot[5]) %>% 
  add_annotation_line(x = 5.8, xend = 5.8, y = Vector_plot1[6], yend = Vector_plot[6]) %>% 
  add_annotation_line(x = 6.8, xend = 6.8, y = Vector_plot1[7], yend = Vector_plot[7]) %>% 
  add_annotation_line(x = 7.8, xend = 7.8, y = Vector_plot1[8], yend = Vector_plot[8]) %>% 
  add_annotation_line(x = 8.8, xend = 8.8, y = Vector_plot1[9], yend = Vector_plot[9]) %>% 
  add_annotation_line(x = 9.8, xend = 9.8, y = Vector_plot1[10], yend = Vector_plot[10]) %>%  
  add_annotation_line(x = 1.2, xend = 1.2, y = Vector_plot1[1], yend = Vector_plot[1]) %>% 
  add_annotation_line(x = 2.2, xend = 2.2, y = Vector_plot1[2], yend = Vector_plot[2]) %>% 
  add_annotation_line(x = 3.2, xend = 3.2, y = Vector_plot1[3], yend = Vector_plot[3]) %>% 
  add_annotation_line(x = 4.2, xend = 4.2, y = Vector_plot1[4], yend = Vector_plot[4]) %>% 
  add_annotation_line(x = 5.2, xend = 5.2, y = Vector_plot1[5], yend = Vector_plot[5]) %>% 
  add_annotation_line(x = 6.2, xend = 6.2, y = Vector_plot1[6], yend = Vector_plot[6]) %>% 
  add_annotation_line(x = 7.2, xend = 7.2, y = Vector_plot1[7], yend = Vector_plot[7]) %>% 
  add_annotation_line(x = 8.2, xend = 8.2, y = Vector_plot1[8], yend = Vector_plot[8]) %>% 
  add_annotation_line(x = 9.2, xend = 9.2, y = Vector_plot1[9], yend = Vector_plot[9]) %>% 
  add_annotation_line(x = 10.2, xend = 10.2, y = Vector_plot1[10], yend = Vector_plot[10]) %>% 
  add_annotation_text(x = 1.02, y = Vector_plot2[1], text = Vector_P_value[1], fontsize = 10) %>%
  add_annotation_text(x = 2.03, y = 10, text = Vector_P_value[2], fontsize = 8) %>%
  add_annotation_text(x = 3.02, y = Vector_plot2[3], text = Vector_P_value[3], fontsize = 10) %>%
  add_annotation_text(x = 4.02, y = Vector_plot2[4], text = Vector_P_value[4], fontsize = 10) %>%
  add_annotation_text(x = 5.02, y = Vector_plot2[5], text = Vector_P_value[5], fontsize = 10) %>%
  add_annotation_text(x = 6.02, y = Vector_plot2[6], text = Vector_P_value[6], fontsize = 10) %>%
  add_annotation_text(x = 7.02, y = Vector_plot2[7], text = Vector_P_value[7], fontsize = 10) %>%
  add_annotation_text(x = 8.02, y = Vector_plot2[8], text = Vector_P_value[8], fontsize = 10) %>%
  add_annotation_text(x = 9.02, y = Vector_plot2[9], text = Vector_P_value[9], fontsize = 10) %>%
  add_annotation_text(x = 10.02, y = Vector_plot2[10],text = Vector_P_value[10], fontsize = 10) %>% 
  adjust_font(fontsize = 12, family = "serif") %>%
  adjust_y_axis(title = "Mean cumulative mortality (%)") %>% 
  adjust_x_axis(title = "Mealworm Strains", rotate_labels = T) %>% 
  save_plot("C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Final_Mortality_Exp1.tif")
  
Mort_final

#Évolution de la croissance#####
growth_data_sum_strain <- growth_data %>% 
  group_by(Strain, Time) %>% 
  summarize(Growth = mean(Growth)) %>% 
  mutate(Strain = case_when(
    Strain == "All" ~ "Germany", 
    Strain == "Esp" ~ "Spain",
    Strain == "Fra" ~ "France",
    Strain == "Gre" ~ "Greece",
    Strain == "Inv1" ~ "Norway 1",
    Strain == "Inv2" ~ "Norway 2",
    Strain == "Ita1" ~ "Italy 1",
    Strain == "Ita2" ~ "Italy 2",
    Strain == "Tur" ~ "Turkey",
    Strain == "Wor" ~ "Canada",
    T ~ Strain)) 


growth_data_sum_strain %>% 
  tidyplot(x = Time, y = Growth, color = Strain) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(colors_discrete_okabeito) %>% 
  adjust_y_axis(padding = c(0,0), title = "Mean accumulated biomass (µg/larvae)") %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_size(width = 200, height = 100) %>% 
  save_plot("C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Growth_by_Strain_Exp1.tif")


growth_data_sum_Wound<- growth_data %>% 
  group_by(Trt, Time) %>% 
  summarize(Growth = mean(Growth)) %>% 
  mutate(Wound = case_when(
    Trt == "C" ~ "Sterile",
    Trt == "B" ~ "Septic"
  ))


growth_data_sum_Wound %>% 
  tidyplot(x = Time, y = Growth, color = Wound) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(c("#012456","#096")) %>% 
  adjust_y_axis(padding = c(0,0),title = "Mean accumulated biomass (µg/larvae)") %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_size(width = 200, height = 100) %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Growth_by_Wound_Exp1.tif")

#Évolution de la moyenne####
Mean_data_sum_strain <- mean_data %>% 
  group_by(Strain, Time) %>% 
  mutate(Time = as.numeric(Time)) %>% 
  summarize(Mean = mean(Mean)) %>% 
  mutate(Strain = case_when(
    Strain == "All" ~ "Germany", 
    Strain == "Esp" ~ "Spain",
    Strain == "Fra" ~ "France",
    Strain == "Gre" ~ "Greece",
    Strain == "Inv1" ~ "Norway 1",
    Strain == "Inv2" ~ "Norway 2",
    Strain == "Ita1" ~ "Italy 1",
    Strain == "Ita2" ~ "Italy 2",
    Strain == "Tur" ~ "Turkey",
    Strain == "Wor" ~ "Canada",
    T ~ Strain)) 


Mean_data_sum_strain %>% 
  tidyplot(x = Time, y = Mean, color = Strain) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(colors_discrete_okabeito) %>% 
  adjust_y_axis(title = "Mean biomass (µg/larvae)") %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_size(width = 200, height = 100) %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Mean_by_Strain_Exp1.tif")


Mean_data_sum_Wound<- Mean_data %>% 
  group_by(Trt, Time) %>% 
  summarize(Mean = mean(Mean)) %>% 
  mutate(Wound = case_when(
    Trt == "C" ~ "Sterile",
    Trt == "B" ~ "Septic"
  ))


Mean_data_sum_Wound %>% 
  tidyplot(x = Time, y = Mean, color = Wound) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(c("#012456","#096")) %>% 
  adjust_y_axis(title = "Mean biomass (µg/larvae)") %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_size(width = 200, height = 100) %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Mean_by_Wound_Exp1.tif")

#Évolution de la Biomasse####
biom_data_sum_strain <- biom_data %>% 
  group_by(Strain, Time) %>% 
  mutate(Time = as.numeric(Time)) %>% 
  summarize(Biomass = mean(Biomass)) %>% 
  mutate(Strain = case_when(
    Strain == "All" ~ "Germany", 
    Strain == "Esp" ~ "Spain",
    Strain == "Fra" ~ "France",
    Strain == "Gre" ~ "Greece",
    Strain == "Inv1" ~ "Norway 1",
    Strain == "Inv2" ~ "Norway 2",
    Strain == "Ita1" ~ "Italy 1",
    Strain == "Ita2" ~ "Italy 2",
    Strain == "Tur" ~ "Turkey",
    Strain == "Wor" ~ "Canada",
    T ~ Strain)) 


biom_data_sum_strain %>% 
  tidyplot(x = Time, y = Biomass, color = Strain) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(colors_discrete_okabeito) %>% 
  adjust_y_axis(title = "Total biomass (µg)") %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_size(width = 200, height = 100) %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Strain_Exp1.tif")


biom_data_sum_Wound<- biom_data %>% 
  group_by(Trt, Time) %>% 
  mutate(Time = as.numeric(Time)) %>% 
  summarize(Biomass = mean(Biomass)) %>% 
  mutate(Wound = case_when(
    Trt == "C" ~ "Sterile",
    Trt == "B" ~ "Septic"
  ))


biom_data_sum_Wound %>% 
  tidyplot(x = Time, y = Biomass, color = Wound) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(c("#012456","#096")) %>% 
  adjust_y_axis(title = "Total biomass (µg)") %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_size(width = 200, height = 100) %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Wound_Exp1.tif")
