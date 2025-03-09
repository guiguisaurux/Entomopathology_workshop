#Outils d'analyse
library(lme4)
library(lmerTest)
library(tidyverse)
library(tidyplots)

#importation du tableau de donnée
full_data <- read.table("Expérience1Souches.txt", header = T, check.names = F)

#Séparation des données en tableau distincts
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

#Graphs