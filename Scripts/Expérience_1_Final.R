#Outils d'analyse
library(lme4)
library(lmerTest)
library(tidyverse)
library(tidyplots)
library(ggrepel)
library(ggtext)
library(cowplot)
library(forcats)

setwd("C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data")

#importation du tableau de donnée
full_data <- read.table("Expérience1Souches.txt", header = T, check.names = F)

get#Séparation des données en tableau distincts####
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
    Strain == "Wor" ~ "Canada 1",
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
  


MortSep <- mort_data_sum %>% 
  filter(Wound == "Septic") %>% 
  tidyplot(x = Time, y = Mortality, color = Strain) %>% 
  add_line(linewidth = 1.6, alpha = 0.75) %>% 
  adjust_y_axis(limits = c(0,40), title = "Mean cumulative mortality (%)") %>%
  adjust_x_axis(padding = c(0,0), limits = c(0,17), breaks = c(0,3,6,9,12,15), title = "Time (Days)") %>% 
  adjust_font(fontsize = 24, family = "serif") %>% 
  #adjust_size(width = 200, height = 100) %>% 
  add_annotation_line(x = 8.2, xend = 8.2, y = 34, yend = 35, color = "black") %>% 
  add_annotation_line(x = 7.8, xend = 7.8, y = 34, yend = 35, color = "black") %>%
  add_annotation_line(x = 7.8, xend = 8.2, y = 35, yend = 35, color = "black") %>% 
  add_annotation_line(x = 10, xend = 10, y = 34, yend = 35, color = 'black') %>% 
  add_annotation_line(x = 15, xend = 15, y =34, yend = 35, color = 'black') %>% 
  add_annotation_line(x = 10, xend = 15, y = 35, yend = 35, color = 'black') %>% 
  add_annotation_text(x = 8, y = 35.5, "*", fontsize = 20) %>% 
  add_annotation_text(x = 12.5, y = 35.5, "***", fontsize = 20) %>% 
  My_Style(Size = "Wide", Color = "Strain", Limits = NULL) %>% 
  remove_legend()

mort_data_sum_sep <- mort_data_sum %>% 
  filter(Wound == "Septic")

last_points_sep <- mort_data_sum_sep%>%
  group_by(Strain) %>%
  filter(Time == max(Time)) %>%
  ungroup()

MortSep1 <- MortSep + 
  geom_text_repel(
    data = last_points_sep,
    aes(color = Strain, label = Strain),
    family = "serif",
    size = 7,
    direction = "y",
    xlim = c(15.8, NA),
    hjust = 0,
    segment.size = .75,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .25,
    segment.curvature = .05,
    segment.ncp = 0.1,
    segment.angle = 5
  ) +
  annotate(
    "text",
    x = 16.2,
    y = 35,
    label = "Strain",
    family = "serif",
    size = 7,
    fontface = "bold"
  ) +
  theme(
    axis.line.x = element_blank()  # Remove default axis line
  ) +
  annotate(
    "segment",
    x = 0, xend = 15,  # Only draw line up to 15
    y = 0, yend = 0,
    color = "black",
    linewidth = 0.5
  ) + 
  theme(
    axis.line.y = element_blank()
  ) +
  annotate(
    "segment",
    x = 0, xend = 0,
    y = 0, yend = 50,
    color = "black",
    linewidth  = 0.5
  )

MortCon <- mort_data_sum %>% 
  filter(Wound == "Sterile") %>% 
  tidyplot(x = Time, y = Mortality, color = Strain) %>% 
  add_line(linewidth = 1.6, alpha = 0.75) %>% 
  adjust_y_axis(limits = c(0,40), title = "Mean cumulative mortality (%)") %>%
  adjust_x_axis(padding = c(0,0), limits = c(0,17), breaks = c(0,3,6,9,12,15), title = "Time (Days)") %>% 
  adjust_font(fontsize = 24, family = "serif") %>%
  #adjust_size(width = 200, height = 100) %>% 
  add_annotation_line(x = 2, xend = 2, y = 34, yend = 35, color = 'black') %>% 
  add_annotation_line(x = 3, xend = 3, y =34, yend = 35, color = 'black') %>% 
  add_annotation_line(x = 2, xend = 3, y = 35, yend = 35, color = 'black') %>%
  add_annotation_text(x = 2.5, y = 35.5, "***", fontsize = 20) %>% 
  My_Style(Size = "Wide", Color = "Strain", Limits = NULL) %>% 
  remove_legend()


mort_data_sum_ste <- mort_data_sum %>% 
  filter(Wound == "Sterile")

last_points_ste <- mort_data_sum_ste%>%
  group_by(Strain) %>%
  filter(Time == max(Time)) %>%
  ungroup()

MortCon1 <- MortCon + 
  geom_text_repel(
    data = last_points_ste,
    aes(color = Strain, label = Strain),
    family = "serif",
    size = 7,
    direction = "y",
    xlim = c(15.8, NA),
    hjust = 0,
    segment.size = .75,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .25,
    segment.curvature = .05,
    segment.ncp = 0.1,
    segment.angle = 5
  ) +
  annotate(
    "text",
    x = 16.2,
    y = 16,
    label = "Strain",
    family = "serif",
    size = 7,
    fontface = "bold"
  ) +
  theme(
    axis.line.x = element_blank()  # Remove default axis line
  ) +
  annotate(
    "segment",
    x = 0, xend = 15,  # Only draw line up to 15
    y = 0, yend = 0,
    color = "black",
    linewidth = 0.5
  ) + 
  theme(
    axis.line.y = element_blank()
  ) +
  annotate(
    "segment",
    x = 0, xend = 0,
    y = 0, yend = 50,
    color = "black",
    linewidth  = 0.5
  )


library(cowplot)
MortPercent <- plot_grid(
  MortSep1 + theme(plot.margin = margin(t = 20, b = 10)),  # Adjusting margin for spacing
  MortCon1+ theme(plot.margin = margin(t = 20, b = 10)), 
  labels = c("(A)", "(B)"),  # Adding labels
  label_size = 24,           # Set label font size
  label_fontface = "bold",   # Bold label font
  label_colour = "black",    # Label color
  ncol = 1 # Arrange plots in 2 columns
)

tidyplots::save_plot(MortPercent, "C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Mortality_Sterile_Exp1.tif", width = 620, height = 500, limitsize = F)

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
    Strain == "Wor" ~ "Canada 1",
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
Vector_P_value <- c('*','n.s.','*','**','**','***','**','**','*','n.s.')

Mort_final <- mort_data_final %>% 
  tidyplot(x = Strain, y = final_mort, color = Wound) %>% 
  add_mean_bar(alpha = 0.3) %>% 
  add_mean_dash(linewidth = 1.2) %>%
  #add_data_points_jitter(jitter_height = 0) %>%
  #add_violin(alpha = 0) %>% 
  adjust_colors(wound_colors) %>% 
  adjust_size(width = 400, height = 200) %>% 
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
  add_annotation_text(x = 1.02, y = Vector_plot2[1], text = Vector_P_value[1], fontsize = 20) %>%
  add_annotation_text(x = 2.03, y = 10, text = Vector_P_value[2], fontsize = 16) %>%
  add_annotation_text(x = 3.02, y = Vector_plot2[3], text = Vector_P_value[3], fontsize = 20) %>%
  add_annotation_text(x = 4.02, y = Vector_plot2[4], text = Vector_P_value[4], fontsize = 20) %>%
  add_annotation_text(x = 5.02, y = Vector_plot2[5], text = Vector_P_value[5], fontsize = 20) %>%
  add_annotation_text(x = 6.02, y = Vector_plot2[6], text = Vector_P_value[6], fontsize = 20) %>%
  add_annotation_text(x = 7.02, y = Vector_plot2[7], text = Vector_P_value[7], fontsize = 20) %>%
  add_annotation_text(x = 8.02, y = Vector_plot2[8], text = Vector_P_value[8], fontsize = 20) %>%
  add_annotation_text(x = 9.02, y = Vector_plot2[9], text = Vector_P_value[9], fontsize = 20) %>%
  add_annotation_text(x = 10.02, y = Vector_plot2[10] +0.5,text = Vector_P_value[10], fontsize = 20) %>% 
  adjust_font(fontsize = 24, family = "serif") %>%
  adjust_y_axis(title = "Mean cumulative mortality (%)") %>% 
  adjust_x_axis(title = "Mealworm Strains", rotate_labels = T) %>% 
  tidyplots::save_plot("C:/Users/user/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Final_Mortality_Exp1.tif")
  
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
    T ~ Strain))  %>% 
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



CroSou <- growth_data_sum_strain %>% 
  tidyplot(x = Time, y = Growth, color = Strain) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(colors_discrete_okabeito) %>% 
  adjust_y_axis(padding = c(0,0), title = "Mean Growth (µg/larvae)", limits = c(0,455)) %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)", limits = c(0,15.2)) %>% 
  adjust_size(width = 100, height = 100) %>% 
  add_annotation_line(x = 2, xend = 2, y = 430, yend = 445, color = 'black') %>% 
  add_annotation_line(x = 15, xend = 15, y = 430, yend = 445, color = 'black') %>% 
  add_annotation_line(x = 2, xend = 15, y = 445, yend = 445, color = 'black') %>%
  add_annotation_text(x = 9.5, y = 450, "***", fontsize = 12) %>% 
  adjust_font(fontsize = 12, family = "serif") # %>%
  #save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Growth_by_Strain_Exp1.tif")


growth_data_sum_Wound<- growth_data %>% 
  group_by(Trt, Time) %>% 
  summarize(Growth = mean(Growth)) %>% 
  mutate(Wound = case_when(
    Trt == "C" ~ "Sterile",
    Trt == "B" ~ "Septic"
  ))  %>% 
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


CroWound <- growth_data_sum_Wound %>% 
  tidyplot(x = Time, y = Growth, color = Wound) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(c("#012456","#096")) %>% 
  adjust_y_axis(padding = c(0,0), title = "Mean Growth (µg/larvae)", limits = c(0,455)) %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)", limits = c(0,15.2)) %>% 
  adjust_size(width = 100, height = 100) %>% 
  add_annotation_line(x = 3, xend = 3, y = 430, yend = 445, color = 'black') %>% 
  add_annotation_line(x = 15, xend = 15, y = 430, yend = 445, color = 'black') %>% 
  add_annotation_line(x = 3, xend = 15, y = 445, yend = 445, color = 'black') %>%
  add_annotation_text(x = 9.5, y = 450, "***", fontsize = 12) %>% 
  adjust_font(fontsize = 12, family = "serif") #%>%
 # save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Growth_by_Wound_Exp1.tif")

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
    T ~ Strain)) %>% 
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


Mean_data_sum_strain %>% 
  tidyplot(x = Time, y = Mean, color = Strain) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(colors_discrete_okabeito) %>% 
  adjust_y_axis(title = "Mean biomass (µg/larvae)") %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_size(width = 200, height = 100) %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Mean_by_Strain_Exp1.tif")




Mean_data_sum_Wound<- mean_data %>% 
  group_by(Trt, Time) %>% 
  summarize(Mean = mean(Mean)) %>% 
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


Mean_data_sum_Wound %>% 
  tidyplot(x = Time, y = Mean, color = Wound) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(c("#012456","#096")) %>% 
  adjust_y_axis(title = "Mean biomass (µg/larvae)") %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_size(width = 280, height = 100) %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Mean_by_Wound_Exp1.tif")

#Strain comparisons on day 0 vs day 15

mean_data_1 <- mean_data %>% 
  mutate(Time = as.numeric(Time)) %>% 
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
  

mean_data_final_1 <- mean_data_1 %>% 
  select(-Mass, -Number) %>% 
  pivot_wider(names_from = "Time", values_from = "Mean")

mean_data_final <- mean_data_final_1 %>% 
  mutate('Day 0' = mean_data_final_1$'0') %>% 
  mutate('Day 15' = mean_data_final_1$'15') %>% 
  select(-'2',-'3',-'4',-'5',-'6',-'7',-'8',-'9',-'10',-'11',-'12',-'13',-'14',-'15', -'0') %>% 
  pivot_longer(cols = c(5,6), names_to = "Time", values_to = "Mean") %>% 
  mutate(Mean = as.numeric(Mean)) %>% 
  mutate(Strain = as.factor(Strain)) %>% 
  mutate(Time = as.factor(Time))


Stats_mean_final <- mean_data_final %>% 
  group_by(Time, Strain) %>%
  summarize(
    mean = mean(Mean),
    sd = sd(Mean),
  ) %>% 
  mutate(Placement = mean + sd + 20)
  

Dataplace <- Stats_mean_final %>% 
  ungroup() %>% 
  mutate(label = c('cde','de','a','c','cd','c','cde','e','c','b','bc','c','a','ab','abc','a','abc','c','abc','a')) %>% 
  group_by(Time, Strain)

Mean_final <- mean_data_final %>% 
  tidyplot(x = Time, y = Mean, color = Strain) %>% 
  add_mean_bar(alpha = 0.4, dodge_width = 0.9) %>% 
  add_mean_dash(linewidth = 0.8,dodge_width = 0.9) %>% 
  add_sd_errorbar(linewidth = 0.5,dodge_width = 0.9) %>% 
  adjust_size(width = 240, height = 100) %>% 
  adjust_colors(colors_discrete_okabeito) %>% 
  adjust_y_axis(title = "Mean biomass (µg/larvae)") %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_font(fontsize = 12, family = "serif")
  

Mean_Final_1<- Mean_final +  ggplot2::geom_text(data = Dataplace, aes(y = Placement, label = label),
position = position_dodge(width = 0.9), show.legend = F)


#save_plot(Mean_Final_1,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Initial_and_Final_Mean_Exp1.tif")


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
    T ~ Strain)) %>% 
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


biom_data_sum_strain %>% 
  tidyplot(x = Time, y = Biomass, color = Strain) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(colors_discrete_okabeito) %>% 
  adjust_y_axis(title = "Total biomass (µg)") %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)", limits = c(-0.2,15.2)) %>% 
  adjust_size(width = 200, height = 100) %>% 
  add_annotation_line(x = 0, xend = 0, y = 6250, yend = 6350, color = 'black') %>% 
  add_annotation_line(x = 15, xend = 15, y = 6250, yend = 6350, color = 'black') %>% 
  add_annotation_line(x = 0, xend = 15, y = 6350, yend = 6350, color = 'black') %>%
  add_annotation_text(x = 9.5, y = 6375, "***", fontsize = 12) %>% 
<<<<<<< HEAD
  adjust_font(fontsize = 12, family = "serif") %>%
  save_plot("C:/Users/user/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Strain_Exp1.tif")
=======
<<<<<<< HEAD
  adjust_font(fontsize = 12, family = "serif") #%>%
#  save_plot("C:/Users/user/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Strain_Exp1.tif")
=======
  adjust_font(fontsize = 12, family = "serif") %>%
  save_plot("C:/Users/user/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Strain_Exp1.tif")
>>>>>>> c7f00415a89012c514cf3651aa2e4913bd18dace
>>>>>>> 90e0dd1 (trying)


biom_data_sum_Wound<- biom_data %>% 
  group_by(Trt, Time) %>% 
  mutate(Time = as.numeric(Time)) %>% 
  summarize(Biomass = mean(Biomass)) %>% 
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


biom_data_sum_Wound %>% 
  tidyplot(x = Time, y = Biomass, color = Wound) %>% 
  add_line(linewidth = 0.8, alpha = 0.75) %>% 
  adjust_colors(c("#012456","#096")) %>% 
  adjust_y_axis(title = "Total biomass (µg)") %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)", limits = c(0,15.2)) %>% 
  adjust_size(width = 200, height = 100) %>% 
  add_annotation_line(x = 2, xend = 2, y = 6250, yend = 6350, color = 'black') %>% 
  add_annotation_line(x = 15, xend = 15, y = 6250, yend = 6350, color = 'black') %>% 
  add_annotation_line(x = 2, xend = 15, y = 6350, yend = 6350, color = 'black') %>%
  add_annotation_text(x = 9.5, y = 6375, "***", fontsize = 12) %>% 
  adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/user/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Wound_Exp1.tif")

#Final biomass:

biom_data_1 <- biom_data %>% 
  mutate(Time = as.numeric(Time)) %>% 
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


biom_data_final_1 <- biom_data_1 %>% 
  select(-Mass, -Number, -Survival, -Mean) %>% 
  pivot_wider(names_from = "Time", values_from = "Biomass")

biom_data_final <- biom_data_final_1 %>% 
  select(-'2',-'3',-'4',-'5',-'6',-'7',-'8',-'9',-'10',-'11',-'12',-'13',-'14', -'0') %>% 
  mutate(Biomass = biom_data_final_1$'15') %>% 
  select(-'15') %>% 
  mutate(Biomass = as.numeric(Biomass)) %>% 
  mutate(Strain = as.factor(Strain)) 


Stats_biom_final <- biom_data_final %>% 
  group_by(Strain) %>%
  summarize(
    mean = mean(Biomass),
    sd = sd(Biomass),
  ) %>% 
  mutate(Placement = mean + sd + 250)




Dataplace <- Stats_biom_final %>% 
  ungroup() %>% 
  mutate(label = c('abc','abc','ab','abc','bc','abc','c','c','c','a')) %>% 
  group_by(Strain)

biom_final <- biom_data_final %>% 
  tidyplot(x = Strain, y = Biomass, color = Strain) %>% 
  add_mean_bar(alpha = 0.4, dodge_width = 0.9) %>% 
  add_mean_dash(linewidth = 0.8,dodge_width = 0.9) %>% 
  add_sd_errorbar(linewidth = 0.5,dodge_width = 0.9) %>% 
  adjust_size(width = 260, height = 100) %>% 
  adjust_colors(colors_discrete_okabeito) %>% 
  adjust_y_axis(title = "Final Biomass (µg)") %>%
  adjust_x_axis(title = "Mealworm Strains", rotate_labels = T) %>%
  adjust_font(fontsize = 12, family = "serif") %>% 
  remove_legend()

biom_Final_1<- biom_final +  ggplot2::geom_text(data = Dataplace, aes(y = Placement, label = label),
                                                position = position_dodge(width = 0.9), show.legend = F)


#save_plot(biom_Final_1,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Final_biom_Exp1.tif")


#Graph 3 article

library(cowplot)
Plot_3_A <- plot_grid(
  CroSou + theme(plot.margin = margin(t = 10, b = 10)),  # Premier graphique (CroSou)
  CroWound + theme(plot.margin = margin(t = 10, b = 10)),  # Deuxième graphique (CroWound) 
  labels = c("(A)", "(B)"),  # Adding labels
  label_size = 14,           # Set label font size
  label_fontface = "bold",   # Bold label font
  label_colour = "black",    # Label color
  ncol = 2 # Arrange plots in 2 columns
)

Plot_3_B = plot_grid(
  Mean_Final_1 + theme(plot.margin = margin(t = 10, b = 10, r = 10)),  # Premier graphique (CroSou)
  biom_Final_1 + theme(plot.margin = margin(t = 10, b = 10, r = 10)),  # Deuxième graphique (CroWound) 
  labels = c("(C)", "(D)"),  # Adding labels
  label_size = 14,           # Set label font size
  label_fontface = "bold",   # Bold label font
  label_colour = "black",    # Label color
  ncol = 1 # Arrange plots in 2 columns
)

Plot_3_C <- plot_grid(
  Plot_3_A,
  Plot_3_B,
  ncol = 1
)

  
  
tidyplots::save_plot(Plot_3_C, "C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Mortality_Sterile_Exp1.tif", width = 300, height = 600,
                     limitsize = F)
