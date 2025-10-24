library(tidyverse)

#reading cleaned data tables
m_and_n <- read.table("PostVérif.txt", header = T, check.names = F) #Mass and Number
survival <- read.table("PostMort.txt", header = T, check.names = F)

m_and_n$Identity <- as.factor(m_and_n$Identity) 
m_and_n$Bloc <- as.factor(m_and_n$Bloc) 
m_and_n$Trt <- as.factor(m_and_n$Trt)  
m_and_n$Wound <- as.factor(m_and_n$Wound)
m_and_n$Strain <- as.factor(m_and_n$Strain) 


survival$Identity <- as.factor(survival$Identity) 
survival$Bloc <- as.factor(survival$Bloc) 
survival$Trt <- as.factor(survival$Trt)  
survival$Wound <- as.factor(survival$Wound)
survival$Strain <- as.factor(survival$Strain) 

#Creating mortality data 
mortality <- survival %>% 
  mutate("0" = (survival$'0' - survival$'0' )) %>% 
  mutate("2" = (survival$'0' - survival$'2' )) %>%
  mutate("4" = (survival$'0' - survival$'4' )) %>%
  mutate("7" = (survival$'0' - survival$'7' )) %>%
  mutate("9" = (survival$'0' - survival$'9' )) %>% 
  mutate("11" = (survival$'0' - survival$'11' )) %>%
  mutate("14" = (survival$'0' - survival$'14' )) %>%
  mutate("16" = (survival$'0' - survival$'16' )) %>%
  mutate("18" = (survival$'0' - survival$'18' )) %>% 
  mutate(group = paste(survival$Strain, survival$Wound)) %>% 
  pivot_longer(cols = c(6:14), names_to = 'Time', values_to = 'Mort_Acc') %>%
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Times = as.factor(Time))

#write_csv(mortality,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_mortalité.csv") 
#write_csv(mortality,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_mortalité.csv") 



#Separating mass and number data and pivoting to long format
mass <- m_and_n %>% 
  dplyr::select(c(1:6,8:15)) %>% 
  pivot_longer(cols = c(7:14), names_to = "Day", values_to = "Mass") #format long

mass$Day <- as.numeric(mass$Day)

number <- m_and_n %>% 
  select(c(1:5,7,16:23)) %>% 
  pivot_longer(cols = c(7:14), names_to = "Day", values_to = "Number")

#merging mass and number
mnn_long <- mass %>% 
  merge(number, by = c('Identity', 'Bloc', 'Trt', 'Wound', 'Strain', 'Day')) %>% 
  mutate(Mean = Mass/Number) %>% 
  select(c(1:7,9,11)) %>% 
  pivot_wider(names_from = "Day", values_from = "Mean") %>% 
  mutate(Initial_Mean = Initial_Mass/Initial_Number)

#Création tableau de données des masses en réduisant les erreurs de manipulation
surv_long <- survival %>% 
  pivot_longer(cols = c(6:14), names_to = "Day", values_to = "Survival")

#Ici on multiplie la moyenne par la 'vrai mortalité' -> réduit les erreurs lié à la perte accidentelle ou à l'oubli d'un ténébrion dans un pot un jour spécifique
true_biomass <- mass %>% 
  merge(number, by = c('Identity', 'Bloc', 'Trt', 'Wound', 'Strain', 'Day')) %>% 
  mutate(Mean = Mass/Number) %>% 
  merge(surv_long, by = c('Identity', 'Bloc', 'Trt', 'Wound', 'Strain', 'Day')) %>% 
  mutate(True_Mass = Mean * Survival)

#write_csv(true_biomass,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Masse_Transformée.csv") #MSI
#write_csv(true_biomass,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Masse_Transformée.csv") #DELL

#Création tableau de croissances moyennes
growth_rate <- mnn_long %>% 
  mutate("2" = (mnn_long$'2' - mnn_long$Initial_Mean)/3) %>%
  mutate("4" = (mnn_long$'4' - mnn_long$'2')/2) %>%
  mutate("7" = (mnn_long$'7' - mnn_long$'4')/3) %>%
  mutate("9" = (mnn_long$'9' - mnn_long$'7')/2) %>% 
  mutate("11" = (mnn_long$'11' - mnn_long$'9')/2) %>%
  mutate("14" = (mnn_long$'14' - mnn_long$'11')/3) %>%
  mutate("16" = (mnn_long$'16' - mnn_long$'14')/2) %>%
  mutate("18" = (mnn_long$'18' - mnn_long$'16')/2) %>% 
  mutate(group = paste(mnn_long$Wound, mnn_long$Trt)) %>% 
  pivot_longer(cols = (8:15), names_to = 'Time', values_to = 'Growth_Rate') %>%
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Times = as.factor(Time))

#write_csv(growth_rate,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Moyenne.csv") #MSI
#write_csv(growth_rate,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Moyenne.csv") #DELL

#Création tableau de la croissance cumulée
growth <- mnn_long %>% 
  mutate("0" = (mnn_long$Initial_Mean - mnn_long$Initial_Mean)) %>%
  mutate("2" = (mnn_long$'2' - mnn_long$Initial_Mean)) %>%
  mutate("4" = (mnn_long$'4' - mnn_long$Initial_Mean)) %>%
  mutate("7" = (mnn_long$'7' - mnn_long$Initial_Mean)) %>%
  mutate("9" = (mnn_long$'9' - mnn_long$Initial_Mean)) %>% 
  mutate("11" = (mnn_long$'11' - mnn_long$Initial_Mean)) %>%
  mutate("14" = (mnn_long$'14' - mnn_long$Initial_Mean)) %>%
  mutate("16" = (mnn_long$'16' - mnn_long$Initial_Mean)) %>%
  mutate("18" = (mnn_long$'18' - mnn_long$Initial_Mean)) %>% 
  mutate(group = paste(mnn_long$Wound, mnn_long$Trt)) %>% 
  pivot_longer(cols = c(8:15,17), names_to = 'Time', values_to = 'Growth_Acc') %>%
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Times = as.factor(Time)) 

#write_csv(growth,"C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Cumulée.csv") #MSI
#write_csv(growth,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Croissance_Cumulée.csv") #DELL

#Yield
total_biomass <- mnn_long %>% 
  mutate('0' = Initial_Mean)
t_b_long <- total_biomass %>% 
  pivot_longer(cols = c(8:15,17), names_to = "Time",values_to = "Mean")

t_b_corrected <- t_b_long %>% 
  merge(mortality, by = c('Identity', 'Bloc', 'Trt', 'Wound', 'Strain', 'Time')) %>% 
  mutate(Biomass = Mean*(10-Mort_Acc))
#write_csv(t_b_corrected, "C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_3_csv/Expérience_3_Biomasse.csv") 

