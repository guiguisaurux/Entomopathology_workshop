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

#write_csv(New.T,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Masse_Transformée.csv") #MSI
#write_csv(New.T,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Masse_Transformée.csv") #DELL

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

#write_csv(Growth_C,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Moyenne.csv") #MSI
#write_csv(Growth_C,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Moyenne.csv") #DELL

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

#write_csv(Growth_M,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Cumulée.csv") #MSI
#write_csv(Growth_M,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Cumulée.csv") #DELL

#Biomasse totale:
Biom_T <- New.F %>% 
  mutate('0' = Initial_Mean)
Biom_L <- Biom_T %>% 
  pivot_longer(cols = c(8:15,17), names_to = "Time",values_to = "Mean")

Biom_L1 <- Biom_L %>% 
  merge(Mort_C, by = c('Identity', 'Bloc', 'Trt', 'Wound', 'Strain', 'Time')) %>% 
  mutate(Biomass = Mean*(10-Mort_Acc))
#write_csv(Biom_L1, "C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Biomasse.csv") 


Max_Biom <- Biom_L1 %>% 
  group_by(Bloc, Trt, Wound, Strain) %>% 
  mutate(MaxBiom = max(Biomass)) %>% 
  ungroup() %>% 
  select(-Initial_Mass,-Initial_Number,-Initial_Mean,-Mort_Acc,-group,-Times,-Mean) %>% 
  pivot_wider(names_from = "Time", values_from = "Biomass") %>% 
  select(-c(7,8,10:13,15)) 

Jour_14 <- Max_Biom %>% 
  mutate(Jour14 = Max_Biom$'14') %>% 
  mutate(Diff = MaxBiom - Max_Biom$'14')

Jour_7 <- Max_Biom %>% 
  mutate(Jour7 = Max_Biom$'7') %>% 
  mutate(Diff = MaxBiom - Max_Biom$'7')


#Gestion des graphs####
Mort_C_graph <- Mort_C %>% 
  mutate(Mortality = Mort_Acc * 10) %>% 
  group_by(Wound, Time) %>% 
  summarise(Mortality = mean(Mortality)) %>% 
  mutate(Wound = case_when(
  Wound == "Ste" ~ "Sterile",
  Wound == "Ser" ~ "Septic",
  Wound == "Non" ~ "None"
)) 





Mort_C_graph$Wound = factor(Mort_C_graph$Wound, levels = c("Septic", "Sterile", "None"))


library(tidyplots)

Mort_C_graph %>% 
  tidyplot(x = Time, y = Mortality, colour = Wound) %>% 
  add_line(linewidth = 0.8,alpha = 0.8, dodge_width = ) %>% 
  adjust_colors(c("#012456","#096","gold")) %>% 
  add_annotation_line(x = 9, xend = 18, y = 16, yend = 16) %>% 
  add_annotation_line(x = 9, xend = 9, y = 15.5, yend = 16) %>% 
  add_annotation_line(x = 18, xend = 18, y = 15.5, yend = 16) %>% 
  add_annotation_text("***", x = 13.535, y = 16.2, fontsize = 14) %>%
  adjust_x_axis(padding = c(0,0), limits = c(0,18.1), title = "Time (Days)") %>% 
  adjust_y_axis(padding = c(0,0), limits = c(0,16.8), title = "Accumulated Mortality (%)") %>% 
  adjust_size(width = 200, height = 100) %>% 
  adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Mortality_Exp3.tiff")


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
  add_line(linewidth = 0.8,alpha = 0.8) %>% 
  adjust_colors(c("#013928","darkred","lightblue")) %>% 
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_y_axis(padding = c(0,0.01),title = "Accumulated biomass (%)") %>% 
  adjust_size(width = 200, height = 100) %>% 
  adjust_font(fontsize = 12, family = "serif") 

G2 <- G1 + ggplot2::geom_ribbon(data = Growth_C_graph, 
                                aes(x = Time,ymin = Growth - SD_G, ymax = Growth + SD_G, fill = Wound),
                                linetype = 0, alpha = 0.2)  
save_plot(G1,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Growth_Exp3.png")
save_plot(G2,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Growth2_Exp3.png")


#Biomass en fonction du temps #####
Biom_Graph_1 <- Biom_L1 %>% 
  mutate(Time <- as.factor(Biom_L1$Time)) %>%  
  mutate(Trt <- as.factor(Biom_L1$Trt)) %>% 
  mutate(Wound <- as.factor(Biom_L1$Wound)) %>% 
  mutate(Strain <- as.factor(Biom_L1$Strain)) %>% 
  group_by(Time, Strain, Trt) %>% 
  summarise(Biomass = mean(Biomass)) %>% 
  #mutate(Wound = case_when(
   # Wound == "Ste" ~ "Sterile",
    #Wound == "Ser" ~ "Septic",
  #  Wound == "Non" ~ "Control"
  #)) %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran",
    Trt == "Bac" ~ "Bactocell",
    Trt == "Lev" ~ "Levucell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Time = as.numeric(Time))

Biom_Graph_2 <- Biom_L1 %>% 
  mutate(Time <- as.factor(Biom_L1$Time)) %>%  
  mutate(Trt <- as.factor(Biom_L1$Trt)) %>% 
  mutate(Wound <- as.factor(Biom_L1$Wound)) %>% 
  mutate(Strain <- as.factor(Biom_L1$Strain)) %>% 
  group_by(Time, Wound) %>% 
  summarise(Biomass = mean(Biomass)) %>%
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  mutate(Time = as.numeric(Time))


Biom_Graph_2 %>% 
  tidyplot(y = Biomass, x = Time, color = Wound) %>%  
  add_line(linewidth = 0.8,alpha = 0.8) %>% 
  adjust_colors(c("#013928","darkred","lightblue")) %>% 
  adjust_x_axis(padding = c(0,0), title = "Time (Days)") %>% 
  adjust_y_axis(padding = c(0,0.01),title = "Total biomass (%)") %>% 
  adjust_size(width = 200, height = 100) %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Biomass_by_wound_Exp3.png")


Bio_Treat <- Biom_Graph_1 %>% 
  tidyplot(y = Biomass, x = Time, color = Strain) %>% 
  add_line(linewidth = 0.8,alpha = 0.8) %>% 
  adjust_colors(c("#812","#123765","#876212")) %>% 
  adjust_x_axis(title = "Time (Days)") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  split_plot(by = Treatment, widths = 150, heights = 100)
    
Bio_Strain <- Biom_Graph_1 %>% 
  tidyplot(y = Biomass, x = Time, color = Treatment) %>% 
  add_line(linewidth = 0.8,alpha = 0.8) %>% 
  adjust_colors(c("#013","#116735","#786512")) %>% 
  adjust_x_axis(title = "Time (Days)") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  split_plot(by = Strain, widths = 150, heights = 100)

save_plot(Bio_Treat,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/BiomTreat_Exp3.png")
save_plot(Bio_Strain,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/BiomStrain_Exp3.png")


#Biomass Jour7 et 14 PU BON !!!####
Jour_7_Graph_CanGre <- Jour_7 %>% 
  mutate(Trt <- as.factor(Jour_7$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_7$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_7$Strain)) %>% 
  filter(Strain == c("Can","Gre")) %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran",
    Trt == "Bac" ~ "Bactocell",
    Trt == "Lev" ~ "Levucell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) 

Jour_7_Graph <- Jour_7_Graph_CanGre %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran",
    Trt == "Bac" ~ "Bactocell",
    Trt == "Lev" ~ "Levucell"
  ))  %>% 
  group_by(Strain, Treatment) %>% 
  summarise(Jour7 = mean(Jour7),
            Position = mean(Jour7) + 350
            ) %>% 
  ungroup() %>% 
  mutate(label = c("AB", "A", "B","A","A","B"))
 
  
  
Jour_7_Graph %>% 
  tidyplot(x = Strain, y = Jour7, color = Treatment) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013","#116735","#786512")) %>% 
  adjust_x_axis(title = "Mealworm Strain") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_7_Graph$label, y = Jour_7_Graph$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277), color = Treatment) %>% 
  adjust_size(width = 150, height = 150) %>% 
  adjust_font(fontsize = 12, family = "serif")

Jour7Biom<- biom_final +  ggplot2::geom_text(data = Dataplace, aes(y = Placement, label = label),
                                                position = position_dodge(width = 0.9), show.legend = F)

  save_plot(Jour7Biom, "C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Biomass_by_Trt_Day7_Exp3.png")
  
##Jour 14
  
Jour_14_Graph <- Jour_14_Graph_CanGre %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran",
    Trt == "Bac" ~ "Bactocell",
    Trt == "Lev" ~ "Levucell"
  ))  %>% 
  group_by(Strain, Treatment) %>% 
  summarise(Jour14 = mean(Jour14),
            Position = mean(Jour14) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("B", "A", "B","A","A","A"))


Jour_14_Graph %>% 
  tidyplot(x = Strain, y = Jour14, color = Treatment) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013","#116135","#786512")) %>% 
  adjust_x_axis(title = "Mealworm Strain") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_14_Graph$label, y = Jour_14_Graph$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277)) %>% 
  adjust_size(width = 150, height = 150) %>% 
  save_plot("C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Figures/Biomass_by_trt_Day14_Exp3.png")


#Souche italienne####
Jour_7_Graph_Ita <- Jour_7 %>% 
  mutate(Trt <- as.factor(Jour_7$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_7$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_7$Strain)) %>% 
  filter(Strain == c("Ita")) %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran",
    Trt == "Bac" ~ "Bactocell",
    Trt == "Lev" ~ "Levucell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Ita" ~ "Italy",
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Wound, Treatment) %>% 
  summarise(Jour7 = mean(Jour7),
            Position = mean(Jour7) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("a","a","a","c","a","b","b","a","a"))

Jour_7_Graph_Ita %>% 
  tidyplot(x = Wound, y = Jour7, color = Treatment) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013","#116735","#786512")) %>% 
  adjust_x_axis(title = "Wound type") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_7_Graph_Ita$label, y = Jour_7_Graph_Ita$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% 
  adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Ita_Wound7_Exp3.tif")
  
#Jour14

Jour_14_Graph_Ita <- Jour_14 %>% 
  mutate(Trt <- as.factor(Jour_14$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_14$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_14$Strain)) %>% 
  filter(Strain == c("Ita")) %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran",
    Trt == "Bac" ~ "Bactocell",
    Trt == "Lev" ~ "Levucell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Ita" ~ "Italy",
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Wound, Treatment) %>% 
  summarise(Jour14 = mean(Jour14),
            Position = mean(Jour14) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("a", "a", "a","b","a","ab","b","a","ab"))

Jour_14_Graph_Ita %>% 
  tidyplot(x = Wound, y = Jour14, color = Treatment) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013","#116735","#786512")) %>%
  adjust_x_axis(title = "Wound type") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_14_Graph_Ita$label, y = Jour_14_Graph_Ita$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277),fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% 
  adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Ita_Wound14_Exp3.tif")


#Souche Grelienne####
Jour_7_Graph_Gre <- Jour_7 %>% 
  mutate(Trt <- as.factor(Jour_7$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_7$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_7$Strain)) %>% 
  filter(Strain == c("Gre")) %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran",
    Trt == "Bac" ~ "Bactocell",
    Trt == "Lev" ~ "Levucell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Gre" ~ "Greece",
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Wound, Treatment) %>% 
  summarise(Jour7 = mean(Jour7),
            Position = mean(Jour7) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("b", "a", "b","ab","a","b","a","a","b"))

Jour_7_Graph_Gre %>% 
  tidyplot(x = Wound, y = Jour7, color = Treatment) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013","#116735","#786512")) %>% 
  adjust_x_axis(title = "Wound type") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_7_Graph_Gre$label, y = Jour_7_Graph_Gre$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% 
  adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Gre_Wound_Exp3.tif")

#Jour14

Jour_14_Graph_Gre <- Jour_14 %>% 
  mutate(Trt <- as.factor(Jour_14$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_14$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_14$Strain)) %>% 
  filter(Strain == c("Gre")) %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran",
    Trt == "Bac" ~ "Bactocell",
    Trt == "Lev" ~ "Levucell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Gre" ~ "Greece",
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Wound, Treatment) %>% 
  summarise(Jour14 = mean(Jour14),
            Position = mean(Jour14) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("b", "a", "ab","a","a","a","a","a","a"))

Jour_14_Graph_Gre %>% 
  tidyplot(x = Wound, y = Jour14, color = Treatment) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013","#116735","#786512")) %>%
  adjust_x_axis(title = "Wound type") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_14_Graph_Gre$label, y = Jour_14_Graph_Gre$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277),fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% 
  adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Gre_Wound14_Exp3.tif")

#Souche Canlienne####
Jour_7_Graph_Can <- Jour_7 %>% 
  mutate(Trt <- as.factor(Jour_7$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_7$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_7$Strain)) %>% 
  filter(Strain == c("Can")) %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran",
    Trt == "Bac" ~ "Bactocell",
    Trt == "Lev" ~ "Levucell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Wound, Treatment) %>% 
  summarise(Jour7 = mean(Jour7),
            Position = mean(Jour7) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("a", "a", "a","a","a","a","a","a","a"))

Jour_7_Graph_Can %>% 
  tidyplot(x = Wound, y = Jour7, color = Treatment) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013","#116735","#786512")) %>% 
  adjust_x_axis(title = "Wound type") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_7_Graph_Can$label, y = Jour_7_Graph_Can$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% 
  adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Can_Wound_Exp3.tif")

#Jour14

Jour_14_Graph_Can <- Jour_14 %>% 
  mutate(Trt <- as.factor(Jour_14$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_14$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_14$Strain)) %>% 
  filter(Strain == c("Can")) %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran",
    Trt == "Bac" ~ "Bactocell",
    Trt == "Lev" ~ "Levucell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Wound, Treatment) %>% 
  summarise(Jour14 = mean(Jour14),
            Position = mean(Jour14) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("a", "a", "a","a","a","a","ab","a","b"))

Jour_14_Graph_Can %>% 
  tidyplot(x = Wound, y = Jour14, color = Treatment) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013","#116735","#786512")) %>%
  adjust_x_axis(title = "Wound type") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_14_Graph_Can$label, y = Jour_14_Graph_Can$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277),fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% 
  adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Can_Wound14_Exp3.tif")

#Traitement Son de blé####
Jour_7_Graph_Con <- Jour_7 %>% 
  mutate(Trt <- as.factor(Jour_7$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_7$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_7$Strain)) %>% 
  filter(Trt == c("Con")) %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Wound, Strain) %>% 
  summarise(Jour7 = mean(Jour7),
            Position = mean(Jour7) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("b", "ab", "a","b","a","a","b","b","a"))

Jour_7_Graph_Con %>% 
  tidyplot(x = Wound, y = Jour7, color = Strain) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#812","#123765","#876212")) %>% 
  adjust_x_axis(title = "Wound type") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_7_Graph_Con$label, y = Jour_7_Graph_Con$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Con_Wound7_Exp3.tif")

#Jour14

#Traitement Son de blé####
Jour_14_Graph_Con <- Jour_14 %>% 
  mutate(Trt <- as.factor(Jour_14$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_14$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_14$Strain)) %>% 
  filter(Trt == c("Con")) %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Wound, Strain) %>% 
  summarise(Jour14 = mean(Jour14),
            Position = mean(Jour14) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("b", "a", "a","b","a","a","b","ab","a"))

Jour_14_Graph_Con %>% 
  tidyplot(x = Wound, y = Jour14, color = Strain) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#812","#123765","#876212")) %>% 
  adjust_x_axis(title = "Wound type") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_14_Graph_Con$label, y = Jour_14_Graph_Con$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Con_Wound14_Exp3.tif")
#Traitement Bactocell####
Jour_7_Graph_Bac <- Jour_7 %>% 
  mutate(Trt <- as.factor(Jour_7$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_7$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_7$Strain)) %>% 
  filter(Trt == c("Bac")) %>% 
  mutate(Treatment = case_when(
    Trt == "Bac" ~ "Bactocell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Wound, Strain) %>% 
  summarise(Jour7 = mean(Jour7),
            Position = mean(Jour7) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("b", "a", "ab","b","a","b","b","a","b"))

Jour_7_Graph_Bac %>% 
  tidyplot(x = Wound, y = Jour7, color = Strain) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#812","#123765","#876212")) %>% 
  adjust_x_axis(title = "Wound type") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_7_Graph_Bac$label, y = Jour_7_Graph_Bac$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Bac_Wound7_Exp3.tif")

#Jour14

#Traitement Bactocell####
Jour_14_Graph_Bac <- Jour_14 %>% 
  mutate(Trt <- as.factor(Jour_14$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_14$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_14$Strain)) %>% 
  filter(Trt == c("Bac")) %>% 
  mutate(Treatment = case_when(
    Trt == "Bac" ~ "Bactocell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Wound, Strain) %>% 
  summarise(Jour14 = mean(Jour14),
            Position = mean(Jour14) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("a", "a", "a","b","a","b","b","a","ab"))

Jour_14_Graph_Bac %>% 
  tidyplot(x = Wound, y = Jour14, color = Strain) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#812","#123765","#876212")) %>% 
  adjust_x_axis(title = "Wound type") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_14_Graph_Bac$label, y = Jour_14_Graph_Bac$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Bac_Wound14_Exp3.tif")

#Traitement Levucell####
Jour_7_Graph_Lev <- Jour_7 %>% 
  mutate(Trt <- as.factor(Jour_7$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_7$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_7$Strain)) %>% 
  filter(Trt == c("Lev")) %>% 
  mutate(Treatment = case_when(
    Trt == "Lev" ~ "Levucell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Wound, Strain) %>% 
  summarise(Jour7 = mean(Jour7),
            Position = mean(Jour7) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("c", "a", "b","b","a","a","b","a","a"))

Jour_7_Graph_Lev %>% 
  tidyplot(x = Wound, y = Jour7, color = Strain) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#812","#123765","#876212")) %>% 
  adjust_x_axis(title = "Wound type") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_7_Graph_Lev$label, y = Jour_7_Graph_Lev$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Lev_Wound7_Exp3.tif")

#Jour14

#Traitement Levucell####
Jour_14_Graph_Lev <- Jour_14 %>% 
  mutate(Trt <- as.factor(Jour_14$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_14$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_14$Strain)) %>% 
  filter(Trt == c("Lev")) %>% 
  mutate(Treatment = case_when(
    Trt == "Lev" ~ "Levucell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Wound, Strain) %>% 
  summarise(Jour14 = mean(Jour14),
            Position = mean(Jour14) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("b", "a", "b","b","ab","a","a","a","a"))

Jour_14_Graph_Lev %>% 
  tidyplot(x = Wound, y = Jour14, color = Strain) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#812","#123765","#876212")) %>% 
  adjust_x_axis(title = "Wound type") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_14_Graph_Lev$label, y = Jour_14_Graph_Lev$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Lev_Wound14_Exp3.tif")

#Traitement Son de blé diff blessures####
Jour_7_Graph_Wound_Con <- Jour_7 %>% 
  mutate(Trt <- as.factor(Jour_7$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_7$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_7$Strain)) %>% 
  filter(Trt == c("Con")) %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Strain, Wound) %>% 
  summarise(Jour7 = mean(Jour7),
            Position = mean(Jour7) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("a", "b", "a","a","b","b","a","b","a"))

Jour_7_Graph_Wound_Con %>% 
  tidyplot(x = Strain, y = Jour7, color = Wound) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013928","darkred","lightblue")) %>% 
  adjust_x_axis(title = "Strain") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_7_Graph_Wound_Con$label, y = Jour_7_Graph_Wound_Con$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Con_Strain7_Exp3.tif")

#Jour14

#Traitement Son de blé diff blessures####
Jour_14_Graph_Wound_Con <- Jour_14 %>% 
  mutate(Trt <- as.factor(Jour_14$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_14$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_14$Strain)) %>% 
  filter(Trt == c("Con")) %>% 
  mutate(Treatment = case_when(
    Trt == "Con" ~ "Wheatbran"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Strain, Wound) %>% 
  summarise(Jour14 = mean(Jour14),
            Position = mean(Jour14) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("a", "c", "b","a","c","b","a","b","a"))

Jour_14_Graph_Wound_Con %>% 
  tidyplot(x = Strain, y = Jour14, color = Wound) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013928","darkred","lightblue")) %>% 
  adjust_x_axis(title = "Strain") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_14_Graph_Wound_Con$label, y = Jour_14_Graph_Wound_Con$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Con_Strain14_Exp3.tif")
#Traitement Bactocell diff blessures####
Jour_7_Graph_Wound_Bac <- Jour_7 %>% 
  mutate(Trt <- as.factor(Jour_7$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_7$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_7$Strain)) %>% 
  filter(Trt == c("Bac")) %>% 
  mutate(Treatment = case_when(
    Trt == "Bac" ~ "Bactocell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Strain, Wound) %>% 
  summarise(Jour7 = mean(Jour7),
            Position = mean(Jour7) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("a", "c", "b","a","b","a","a","c","b"))

Jour_7_Graph_Wound_Bac %>% 
  tidyplot(x = Strain, y = Jour7, color = Wound) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013928","darkred","lightblue")) %>% 
  adjust_x_axis(title = "Strain") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_7_Graph_Wound_Bac$label, y = Jour_7_Graph_Wound_Bac$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Bac_Strain7_Exp3.tif")

#Jour14

#Traitement Bactocell diff blessures####
Jour_14_Graph_Wound_Bac <- Jour_14 %>% 
  mutate(Trt <- as.factor(Jour_14$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_14$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_14$Strain)) %>% 
  filter(Trt == c("Bac")) %>% 
  mutate(Treatment = case_when(
    Trt == "Bac" ~ "Bactocell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Strain, Wound) %>% 
  summarise(Jour14 = mean(Jour14),
            Position = mean(Jour14) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("a", "c", "b","a","b","a","a","c","b"))

Jour_14_Graph_Wound_Bac %>% 
  tidyplot(x = Strain, y = Jour14, color = Wound) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013928","darkred","lightblue")) %>% 
  adjust_x_axis(title = "Strain") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_14_Graph_Wound_Bac$label, y = Jour_14_Graph_Wound_Bac$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Bac_Strain14_Exp3.tif")

#Traitement Levucell diff blessures####
Jour_7_Graph_Wound_Lev <- Jour_7 %>% 
  mutate(Trt <- as.factor(Jour_7$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_7$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_7$Strain)) %>% 
  filter(Trt == c("Lev")) %>% 
  mutate(Treatment = case_when(
    Trt == "Lev" ~ "Levucell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Strain, Wound) %>% 
  summarise(Jour7 = mean(Jour7),
            Position = mean(Jour7) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("a", "b", "a","a","c","b","a","b","a"))

Jour_7_Graph_Wound_Lev %>% 
  tidyplot(x = Strain, y = Jour7, color = Wound) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013928","darkred","lightblue")) %>% 
  adjust_x_axis(title = "Strain") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_7_Graph_Wound_Lev$label, y = Jour_7_Graph_Wound_Lev$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Lev_Strain7_Exp3.tif")

#Jour14

#Traitement Levucell diff blessures####
Jour_14_Graph_Wound_Lev <- Jour_14 %>% 
  mutate(Trt <- as.factor(Jour_14$Trt)) %>% 
  mutate(Wound <- as.factor(Jour_14$Wound)) %>% 
  mutate(Strain <- as.factor(Jour_14$Strain)) %>% 
  filter(Trt == c("Lev")) %>% 
  mutate(Treatment = case_when(
    Trt == "Lev" ~ "Levucell"
  )) %>% 
  mutate(Strain = case_when(
    Strain == "Can" ~ "Canada",
    Strain == "Ita" ~ "Italy",
    Strain == "Gre" ~ "Greece"
  )) %>% 
  mutate(Wound = case_when(
    Wound == "Ste" ~ "Sterile",
    Wound == "Ser" ~ "Septic",
    Wound == "Non" ~ "Control"
  )) %>% 
  group_by(Strain, Wound) %>% 
  summarise(Jour14 = mean(Jour14),
            Position = mean(Jour14) + 450
  ) %>% 
  ungroup() %>% 
  mutate(label = c("a", "b", "a","a","c","b","a","b","a"))

Jour_14_Graph_Wound_Lev %>% 
  tidyplot(x = Strain, y = Jour14, color = Wound) %>% 
  add_sum_bar(alpha = 0.5) %>% 
  add_sum_dash() %>% 
  adjust_colors(c("#013928","darkred","lightblue")) %>% 
  adjust_x_axis(title = "Strain") %>% 
  adjust_y_axis(title = "Total biomass (%)") %>% 
  add_annotation_text(text = Jour_14_Graph_Wound_Lev$label, y = Jour_14_Graph_Wound_Lev$Position, 
                      x = c(0.727, 1, 1.277,1.727,2,2.277,2.727, 3, 3.277), fontsize = 12) %>% 
  adjust_size(width = 150, height = 150) %>% adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Biomass_by_Lev_Strain14_Exp3.tif")

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
