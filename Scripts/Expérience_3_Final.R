#Ouverture données
New <- read.table("PostVérif.txt", header = T, check.names = F)
Mort <- read.table("PostMort.txt", header = T, check.names = F)

library(tidyverse)
library(tidyplots)
library(lme4)
library(lmerTest)
library(vegan)


New$Identity <- as.factor(New$Identity) 
New$Bloc <- as.factor(New$Bloc) 
New$Trt <- as.factor(New$Trt)  
New$Wound <- as.factor(New$Wound)
New$Strain <- as.factor(New$Strain) 


Mort$Identity <- as.factor(Mort$Identity) 
Mort$Bloc <- as.factor(Mort$Bloc) 
Mort$Trt <- as.factor(Mort$Trt)  
Mort$Wound <- as.factor(Mort$Wound)
Mort$Strain <- as.factor(Mort$Strain) 

#Création d'une nouvelle table de donnée de mortalité
Mort_C <- Mort %>% 
  mutate("0" = (Mort$'0' - Mort$'0' )) %>% 
  mutate("2" = (Mort$'0' - Mort$'2' )) %>%
  mutate("4" = (Mort$'0' - Mort$'4' )) %>%
  mutate("7" = (Mort$'0' - Mort$'7' )) %>%
  mutate("9" = (Mort$'0' - Mort$'9' )) %>% 
  mutate("11" = (Mort$'0' - Mort$'11' )) %>%
  mutate("14" = (Mort$'0' - Mort$'14' )) %>%
  mutate("16" = (Mort$'0' - Mort$'16' )) %>%
  mutate("18" = (Mort$'0' - Mort$'18' )) %>% 
  mutate(group = paste(Mort$Strain, Mort$Wound)) %>% 
  pivot_longer(cols = (6:14), names_to = 'Time', values_to = 'Mort_Acc') %>%
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Times = as.factor(Time))

#write_csv(Mort_C,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_mortalité.csv") #MSI
#write_csv(Mort_C,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_mortalité.csv") #DELL



#Création et manipulation des données de masses et de moyennes -> M = Mass; N = Number
New.M <- New %>% 
  select(c(1:6,8:15)) %>% 
  pivot_longer(cols = c(7:14), names_to = "Day", values_to = "Mass") #format long

New.M$Day <- as.numeric(New.M$Day)


New.N <- New %>% 
  select(c(1:5,7,16:23)) %>% 
  pivot_longer(cols = c(7:14), names_to = "Day", values_to = "Number")

#New.F = Format long avec l'entièreté des données de masse et de Moyennes -> colonnes initales aussi
New.F <- New.M %>% 
  merge(New.N, by = c('Identity', 'Bloc', 'Trt', 'Wound', 'Strain', 'Day')) %>% 
  mutate(Mean = Mass/Number) %>% 
  select(c(1:7,9,11)) %>% 
  pivot_wider(names_from = "Day", values_from = "Mean") %>% 
  mutate(Initial_Mean = Initial_Mass/Initial_Number)

#Création tableau de données des masses en réduisant les erreurs de manipulation
Surv_long <- Mort %>% 
  pivot_longer(cols = c(6:14), names_to = "Day", values_to = "Survival")

#Ici on multiplie la moyenne par la 'vrai mortalité' -> réduit les erreurs lié à la perte accidentelle ou à l'oubli d'un ténébrion dans un pot un jour spécifique
New.T <- New.M %>% 
  merge(New.N, by = c('Identity', 'Bloc', 'Trt', 'Wound', 'Strain', 'Day')) %>% 
  mutate(Mean = Mass/Number) %>% 
  merge(Surv_long, by = c('Identity', 'Bloc', 'Trt', 'Wound', 'Strain', 'Day')) %>% 
  mutate(True_Mass = Mean * Survival)

write_csv(New.T,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Masse_Transformée.csv") #MSI
write_csv(New.T,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Masse_Transformée.csv") #DELL

#Création tableau de croissances moyennes
Growth_C <- New.F %>% 
  mutate("2" = (New.F$'2' - New.F$Initial_Mean)/3) %>%
  mutate("4" = (New.F$'4' - New.F$'2')/2) %>%
  mutate("7" = (New.F$'7' - New.F$'4')/3) %>%
  mutate("9" = (New.F$'9' - New.F$'7')/2) %>% 
  mutate("11" = (New.F$'11' - New.F$'9')/2) %>%
  mutate("14" = (New.F$'14' - New.F$'11')/3) %>%
  mutate("16" = (New.F$'16' - New.F$'14')/2) %>%
  mutate("18" = (New.F$'18' - New.F$'16')/2) %>% 
  mutate(group = paste(New.F$Wound, New.F$Trt)) %>% 
  pivot_longer(cols = (8:15), names_to = 'Time', values_to = 'Growth_Rate') %>%
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Times = as.factor(Time))

write_csv(Growth_C,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Moyenne.csv") #MSI
write_csv(Growth_C,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Moyenne.csv") #DELL

#Création tableau de la croissance cumulée
Growth_M <- New.F %>% 
  mutate("2" = (New.F$'2' - New.F$Initial_Mean)) %>%
  mutate("4" = (New.F$'4' - New.F$Initial_Mean)) %>%
  mutate("7" = (New.F$'7' - New.F$Initial_Mean)) %>%
  mutate("9" = (New.F$'9' - New.F$Initial_Mean)) %>% 
  mutate("11" = (New.F$'11' - New.F$Initial_Mean)) %>%
  mutate("14" = (New.F$'14' - New.F$Initial_Mean)) %>%
  mutate("16" = (New.F$'16' - New.F$Initial_Mean)) %>%
  mutate("18" = (New.F$'18' - New.F$Initial_Mean)) %>% 
  mutate(group = paste(New.F$Wound, New.F$Trt)) %>% 
  pivot_longer(cols = (8:15), names_to = 'Time', values_to = 'Growth_Acc') %>%
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Times = as.factor(Time)) 

write_csv(Growth_M,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Cumulée.csv") #MSI
write_csv(Growth_M,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Cumulée.csv") #DELL


#Gestion des graphs
Mort_C_graph <- Mort_C %>% 
  mutate(Mortality = Mort_Acc * 10) %>% 
  group_by(Wound, Time) %>% 
  summarise(Mortality = mean(Mortality)) %>% 
  mutate(Wound = case_when(
  Wound == "Ste" ~ "Sterile",
  Wound == "Ser" ~ "Septic",
  Wound == "Non" ~ "Control"
))

library(tidyplots)

Mort_C_graph %>% 
  tidyplot(x = Time, y = Mortality, colour = Wound) %>% 
  add_line(linewidth = 0.8,alpha = 0.8, dodge_width = ) %>% 
  adjust_colors(c("#013928","darkred","sienna")) %>% 
  add_annotation_line(x = 9, xend = 18, y = 16, yend = 16) %>% 
  add_annotation_line(x = 9, xend = 9, y = 15.5, yend = 16) %>% 
  add_annotation_line(x = 18, xend = 18, y = 15.5, yend = 16) %>% 
  add_annotation_text("***", x = 13.535, y = 16.2, fontsize = 14) %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_y_axis(padding = c(0,0), limits = c(0,16.8), title = "Mortality (%)") %>% 
  adjust_size(width = 200, height = 100) %>% 
  save_plot("C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Figures/Mortality_Exp3.png")









