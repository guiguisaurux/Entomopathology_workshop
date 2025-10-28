library(tidyverse)

#Importation des données####
Nb <- read.table("TenebNbCro.txt", header = T, check.names = F)
Ma <- read.table("TenebMaCro.txt", header =T, check.names = F)

# 0) modifier la colone Rep pour incorporer le bloc


Nb$Rep <- paste0(as.character(Nb$Rep), as.character(Nb$Bloc))
Ma$Rep <- paste0(as.character(Ma$Rep), as.character(Ma$Bloc))


Nb <- Nb %>%
  mutate(across(c(Souche, Rep, Bloc, Traitement, Piq), as.factor)) 
Nb[,c(6:17)] <- lapply(Nb[,c(6:17)],as.numeric)

Ma <- Ma %>%
  mutate(across(c(Souche, Rep, Bloc, Traitement, Piq), as.factor))
Ma[,c(6:17)] <- lapply(Ma[,c(6:17)],as.numeric)

#Traitement des données####


# 1) - long format
MaL <- Ma %>%
  pivot_longer(cols = c(6:17), 
               names_to = "Sem",
               values_to = "Ma")
NbL <- Nb %>%
  pivot_longer(cols = c(6:17), 
               names_to = "Sem",
               values_to = "Nb")

# 2) - Change 0s to NA
NbL[NbL== 0] <- NA

# 3) - Merging Data Frames

CroL <- merge(MaL, NbL, by = c(1:6)) 
CroL$Madiv <- CroL$Ma/CroL$Nb #Madiv = Mean
CroL <- CroL[, !(names(CroL) %in% c("Nb", "Ma"))] #Removing Mass and Number Data

# 4) - Switching to wide format

Cro <- CroL %>%
  pivot_wider(names_from = Sem, 
              values_from = Madiv)

# 4,1) - Evaluating 2 missing values (CanDSte et ItaDSte)

Cro <- Cro %>%
  rename( Day1 = '6',
          Day2 = '7'
  )


Cro <- Cro %>%
  group_by(Traitement, Souche) %>% # Group by "Traitement" and "Souche"
  mutate(
    growth = Day2 - Day1, 
    avg_growth = mean(growth, na.rm = TRUE),  # Group-specific average growth
    Day1 = ifelse(is.na(Day1), Day2 - avg_growth, Day1) # Estimate missing Day1
  ) %>%
  ungroup() %>%                               
  dplyr::select(-growth, -avg_growth) # Remove columns


Cro <- Cro %>%
  rename( '6' = Day1,
          '7' = Day2
  )



# 5) - Evaluating growth by subtracting initial mean

CroG <- Cro %>%
  dplyr::select( Rep,Bloc, Souche, Traitement,  Piq, '6') %>%
  mutate((Cro[,15] - Cro[,14])) %>%
  mutate((Cro[,16] - Cro[,14])) %>%
  mutate((Cro[,17] - Cro[,14])) %>%
  mutate((Cro[,6] - Cro[,14])) %>%
  mutate((Cro[,7] - Cro[,14])) #%>% #Too many NA's further
#  mutate((Cro[,8] - Cro[,14])) %>%
#mutate((Cro[,9] - Cro[,14])) %>%
#mutate((Cro[,10] - Cro[,14])) %>%
#mutate((Cro[,11] - Cro[,14])) %>%
#mutate((Cro[,12] - Cro[,14])) %>%
#mutate((Cro[,13] - Cro[,14])) 

# 6) - Long format

CroGL <- CroG %>%
  pivot_longer(cols = c(7:11), 
               names_to = "Sem",
               values_to = "Cro")
write_csv(CroGL,"C:/Users/user/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_2_csv/Expérience_2_Croissance.csv") 

colnames(CroG)[6] <- "Ini"

write_csv(CroG,"C:/Users/user/OneDrive/Documents/Entomopathologie_workshop/Data/Experiment_2_csv/Expérience_2_Mi.csv") 

