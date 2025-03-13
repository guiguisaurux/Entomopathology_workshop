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
  mutate("0" = (New.F$Initial_Mean - New.F$Initial_Mean)) %>%
  mutate("2" = (New.F$'2' - New.F$Initial_Mean)) %>%
  mutate("4" = (New.F$'4' - New.F$Initial_Mean)) %>%
  mutate("7" = (New.F$'7' - New.F$Initial_Mean)) %>%
  mutate("9" = (New.F$'9' - New.F$Initial_Mean)) %>% 
  mutate("11" = (New.F$'11' - New.F$Initial_Mean)) %>%
  mutate("14" = (New.F$'14' - New.F$Initial_Mean)) %>%
  mutate("16" = (New.F$'16' - New.F$Initial_Mean)) %>%
  mutate("18" = (New.F$'18' - New.F$Initial_Mean)) %>% 
  mutate(group = paste(New.F$Wound, New.F$Trt)) %>% 
  pivot_longer(cols = c(8:15,17), names_to = 'Time', values_to = 'Growth_Acc') %>%
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Times = as.factor(Time)) 

write_csv(Growth_M,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Cumulée.csv") #MSI
write_csv(Growth_M,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Cumulée.csv") #DELL

#Biomasse totale:
Biom_T <- New.F %>% 
  mutate('0' = Initial_Mean)
Biom_L <- Biom_T %>% 
  pivot_longer(cols = c(8:15,17), names_to = "Time",values_to = "Mean")

Biom_L1 <- Biom_L %>% 
  merge(Mort_C, by = c('Identity', 'Bloc', 'Trt', 'Wound', 'Strain', 'Time')) %>% 
  mutate(Biomass = Mean*(10-Mort_Acc))
write_csv(Biom_L1, "C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Biomasse.csv") 


Max_Biom <- Biom_L1 %>% 
  group_by(Bloc, Trt, Wound, Strain) %>% 
  mutate(MaxBiom = max(Biomass)) %>% 
  ungroup() %>% 
  select(-Initial_Mass,-Initial_Number,-Initial_Mean,-Mort_Acc,-group,-Times,-Mean) %>% 
  pivot_wider(names_from = "Time", values_from = "Biomass") %>% 
  select(-c(7,8,10:15)) 

Jour_14 <- Max_Biom %>% 
  mutate(Jour14 = Max_Biom$'14') %>% 
  mutate(Diff = MaxBiom - Max_Biom$'14')


#Gestion des graphs####
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
  adjust_x_axis(padding = c(0,0), limits = c(0,18.1), title = "Time (Days)") %>% 
  adjust_y_axis(padding = c(0,0), limits = c(0,16.8), title = "Accumulated mortality (%)") %>% 
  adjust_size(width = 200, height = 100) %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Mortality_Exp3.png")


Growth_C_graph <- Growth_M %>% 
  group_by(Wound, Time) %>% 
  summarise(Growth = mean(Growth_Acc),
            SD_G = sd(Growth_Acc)) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  ))


G1 <- Growth_C_graph %>% 
  tidyplot(x = Time, y = Growth, colour = Wound) %>% 
  add_line(linewidth = 0.8,alpha = 0.8, dodge_width = ) %>% 
  adjust_colors(c("#013928","darkred","sienna")) %>% 
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_y_axis(padding = c(0,0.01),title = "Accumulated biomass (%)") %>% 
  adjust_size(width = 200, height = 100) 

G2 <- G1 + ggplot2::geom_ribbon(data = Growth_C_graph, aes(x = Time,ymin = Growth - SD_G, ymax = Growth + SD_G, fill = Wound),linetype = 0, alpha = 0.2)  
save_plot(G1,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Growth_Exp3.png")
save_plot(G2,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Growth2_Exp3.png")


#####PotentialGraphs#####
Max_Biom %>% 
  tidyplot(x = Strain, y = MaxBiom, colour = Trt) %>% 
  add_mean_bar(alpha = 0.65) %>% 
  add_mean_dash() %>% 
  add_sd_errorbar() %>% 
  adjust_y_axis(limits = c(4000,17000)) %>% 
  split_plot(by = Wound, widths = 100, heights = 75)


Jour_14 %>% 
  tidyplot(x = Strain, y = Diff, colour = Trt) %>% 
  add_boxplot() %>% 
  adjust_y_axis() %>% 
  add_data_points_jitter(jitter_height = 0) %>% 
  split_plot(by = Wound, widths = 100, heights = 75)

Jour_14 %>% 
  tidyplot(x = Trt, y = Jour14, colour = Strain) %>% 
  add_mean_bar(alpha = 0.65) %>% 
  add_mean_dash() %>% 
  
  add_sd_errorbar() %>% 
  adjust_y_axis(limits = c(4000,17000)) %>% 
  split_plot(by = Wound, widths = 100, heights = 75)



Max_Biom %>% 
  tidyplot(x = Trt, y = MaxBiom, colour = Strain) %>% 
  add_mean_bar(alpha = 0.65) %>% 
  add_mean_dash() %>% 
  add_sd_errorbar() %>% 
  adjust_y_axis(limits = c(4000,17000)) %>% 
  split_plot(by = Wound, widths = 100, heights = 75)

Max_Biom %>% 
  tidyplot(x = Wound, y = MaxBiom, colour = Wound) %>% 
  add_mean_bar() %>% 
  add_mean_dash()




Real_max <- New.M %>% 
  group_by(Identity) %>% 
  mutate(RealMax = max(Mass)) %>% 
  pivot_wider(names_from = "Day", values_from = "Mass") %>% 
  ungroup()

Real_max_with_14 <- Real_max %>% 
  mutate(Mass14 = Real_max$'14') %>% 
  select
  
  
Real_max <- New.M %>% 
  group_by(Identity) %>% 
  mutate(RealMax = max(Mass)) %>% 
  ungroup() %>% 
  mutate(Diff = RealMax - Mass) %>% 
  filter(Diff == 0) %>% 
  mutate(Initial_Mass = Biom_T$'0')



Real_max %>% 
  mutate(Day = as.numeric(Day)) %>% 
  tidyplot(x = Strain, y = Initial_Mass, colour = Trt) %>% 
  add_mean_bar(alpha = 0.6) %>% 
  #adjust_y_axis(limits = c(7,20)) %>% 
  add_data_points_jitter() %>% 
  split_plot(by = Wound, widths = 100, heights = 75)
