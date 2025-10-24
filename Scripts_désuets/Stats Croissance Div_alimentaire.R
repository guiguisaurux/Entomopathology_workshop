#Utiliser les flèches sur la gauche pour faciliter la navigation 

#Packages
#Fonction pour installation des packages
ensure_packages_installed <- function(packages) {
  for (pkg in unique(packages)) { 
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing package:", pkg))
      install.packages(pkg, dependencies = TRUE)
    } else {
      message(paste("Package already installed:", pkg))
    }
    library(pkg, character.only = TRUE)
  }
}

#Nécéssaire pour ce code
packages <- c("ggplot2", "lme4", "lmerTest", 
              "multcomp", "multcompView", "emmeans","tidyplots", "grDevices")


ensure_packages_installed(packages)

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(multcomp)
library(multcompView)
library(cowplot)
library(emmeans)
library(multcomp)
library(multcompView)
library(dplyr)
library(grDevices)

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


# 1) - mettre en format long
MaL <- Ma %>%
  pivot_longer(cols = c(6:17), 
               names_to = "Sem",
               values_to = "Ma")
NbL <- Nb %>%
  pivot_longer(cols = c(6:17), 
               names_to = "Sem",
               values_to = "Nb")

# 2) - Changer les 0 en NA
NbL[NbL== 0] <- NA

# 3) - Combiner les deux dataframes

CroL <- merge(MaL, NbL, by = c(1:6)) 
CroL$Madiv <- CroL$Ma/CroL$Nb
CroL <- CroL[, !(names(CroL) %in% c("Nb", "Ma"))] #On retire les colonnes Ma et Nb

# 4) - Remettre en format large

Cro <- CroL %>%
  pivot_wider(names_from = Sem, 
              values_from = Madiv)

# 4,1) - évaluer les valeurs manquantes (CanDSte et ItaDSte)

Cro <- Cro %>%
  rename( Day1 = '6',
          Day2 = '7'
          )


Cro <- Cro %>%
  group_by(Traitement, Souche) %>% # Group by Traitement and Souche
  mutate(
    growth = Day2 - Day1,                          # Calculate growth
    avg_growth = mean(growth, na.rm = TRUE),       # Group-specific average growth
    Day1 = ifelse(is.na(Day1), Day2 - avg_growth, Day1) # Estimate missing Day1
  ) %>%
  ungroup() %>%                                    # Ungroup to clean up
  dplyr::select(-growth, -avg_growth)                     # Remove helper columns


Cro <- Cro %>%
  rename( '6' = Day1,
          '7' = Day2
  )

# View the updated data frame
print(Cro)


# 5) - On retire la valeur initiale de chaque date

CroG <- Cro %>%
  dplyr::select( Rep,Bloc, Souche, Traitement,  Piq, '6') %>%
  mutate((Cro[,15] - Cro[,14])) %>%
  mutate((Cro[,16] - Cro[,14])) %>%
  mutate((Cro[,17] - Cro[,14])) %>%
  mutate((Cro[,6] - Cro[,14])) %>%
  mutate((Cro[,7] - Cro[,14])) #%>%
#  mutate((Cro[,8] - Cro[,14])) %>%
  #mutate((Cro[,9] - Cro[,14])) %>%
  #mutate((Cro[,10] - Cro[,14])) %>%
  #mutate((Cro[,11] - Cro[,14])) %>%
  #mutate((Cro[,12] - Cro[,14])) %>%
  #mutate((Cro[,13] - Cro[,14])) 

# 6) - Format long

CroGL <- CroG %>%
  pivot_longer(cols = c(7:11), 
               names_to = "Sem",
               values_to = "Cro")

colnames(CroG)[6] <- "Ini"
#Modèle####

# I - Vérfication de la masse initiale
ModVer <- lmer(Ini ~ Souche * Traitement + (1|Bloc) + (1|Bloc:Souche) + (1|Bloc:Traitement), data = CroG)

# Conditions d'application
plot(ModVer)
qqnorm(residuals(ModVer))

qqline(residuals(ModVer), col = "red")

#Anova
anova(ModVer, type = 3)


ModVer_Tukey <- emmeans(ModVer, ~Souche)

tuk.cld.ModVer <- cld(ModVer_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.ModVer

CroG <- CroG %>% 
  mutate(Traitement = case_when(
    Traitement == "C" ~ "Wheatbran",
    Traitement == "P" ~ "Bactocell",
    Traitement == "D" ~ "Levucell",
    Traitement == "E" ~ "Chickpea"
  )) %>% 
  mutate(Souche = case_when(
    Souche == "Canada" ~ "Canada",
    Souche == "Italie" ~ "Italy",
    Souche == "Grèce" ~ "Greece"
  ))


CroG %>% 
  tidyplot(x = Souche, y = Ini, color = Souche) %>% 
  add_mean_bar(alpha = 0.4) %>%
  add_sd_errorbar() %>% 
  add_mean_dash() %>% 
  add_test_asterisks(hide_info = TRUE,ref.group = 3, p.adjust.method = "holm") %>%
  remove_legend() %>%
  adjust_y_axis_title(title = 'Initial Mean Biomass (µg/larvae)') %>%
  adjust_colors(c("#812","#123765","#876212")) %>%
  adjust_size(width = 100, height = 100, unit = "mm") %>%
  adjust_font(fontsize = 12, family = "serif") %>% 
  save_plot("C:/Users/user/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Initial_Biom_Exp2.tif")
  


# II - Modèle# II - Motitle = dèle
Mod1 <- lmer(Cro ~ Souche * Traitement + Sem + Sem:Souche + Traitement:Sem + 
               (1|Bloc) + (1|Bloc:Souche) + (1|Bloc:Traitement) + (1|Rep), data = CroGL)

#Conditions d'application
plot(Mod1)
qqnorm(residuals(Mod1))
qqline(residuals(Mod1), col = "red") #Besoin de transformation


anova(Mod1)

#Séparation des données####
#Semaine 1####
CroG7 <- CroG %>%
  dplyr::select('7', Souche, Traitement, Bloc, Rep) %>%
  rename(Cro = '7')

Mod7 <- lmer(Cro ~ Souche * Traitement + 
               (1|Bloc) + (1|Bloc:Souche) + (1|Bloc:Traitement), data = CroG7)
plot(Mod7)
qqnorm(residuals(Mod7))
qqline(residuals(Mod7), col = "red")


anova(Mod7, type=3)

Mod7S_Tukey <- emmeans(Mod7, ~Souche)


tuk.cld.Mod7S <- cld(Mod7S_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod7S

lettersMod7S <- tuk.cld.Mod7S$.group
letters_df.Mod7S <- data.frame(Souche = tuk.cld.Mod7S$Souche, letters = lettersMod7S)
letters_df.Mod7S


Mod7T_Tukey <- emmeans(Mod7, ~Traitement)


tuk.cld.Mod7T <- cld(Mod7T_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod7T

lettersMod7T <- tuk.cld.Mod7T$.group
letters_df.Mod7T <- data.frame(Traitement = tuk.cld.Mod7T$Traitement, letters = lettersMod7T)
letters_df.Mod7T

CroG7 %>% 
  tidyplot(x = Souche, y = Cro, color = Souche) %>% 
  add_boxplot(alpha = 0.6) %>%
  add_data_labels(data = letters_df.Mod7S, label = letters, y = 470, color = 'black')%>%
  remove_legend() %>%
  adjust_y_axis(title = '',limits = c(0,500)) %>%
  adjust_colors(new_colors = colors_discrete_friendly) %>%
  adjust_size(width = 60, height = 60, unit = "mm") %>%
  view_plot(title = NULL) %>%
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Croissance semaine 7 Sou.png")

CroG7 %>%
  tidyplot(x = Traitement, y = Cro, color = Traitement) %>%
  add_boxplot(alpha = 0.6) %>%
  add_data_labels(data = letters_df.Mod7T, label = letters, y = 470, color = 'black') %>% 
  remove_legend() %>%
  adjust_y_axis(title = '', limits = c(0,500)) %>%
  adjust_colors(new_colors = colors_discrete_friendly_long) %>%
  adjust_size(width = 60, height = 60, unit = "mm") %>%
  view_plot(title = NULL)%>%
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Croissance semaine 7 Trt.png")
  
rename_x_axis_labels(PlotCro7T, new_names = c("D" = '$italic(S.~cerevisiae)$',
"P" = '$italic(P.~acidilactili)$',
"C" = "Contrôle",
"E" = "Élément naturel",
"D" = '$italic(S.~cerevisiae)$',
))

#Semaine 2####
CroG8 <- CroG %>%
  dplyr::select('8', Souche, Traitement, Bloc, Rep) %>%
  rename(Cro = '8')

Mod8 <- lmer(Cro ~ Souche * Traitement + 
               (1|Bloc) + (1|Bloc:Souche) + (1|Bloc:Traitement), data = CroG8)
plot(Mod8)
qqnorm(residuals(Mod8))
qqline(residuals(Mod8), col = "red")


anova(Mod8, type=3)

#Créations de tables séparées selon les souches et traîtements

# 1) Table Souche Semaine 2####
CroG8S <- CroG8 %>%
  pivot_wider(names_from = Souche, 
              values_from = Cro)
  
#Canada
Mod8C <- lmer(Canada ~ Traitement + (1|Bloc) + (1|Bloc:Traitement),data = CroG8S) 
plot(Mod8C)
qqnorm(residuals(Mod8C))
qqline(residuals(Mod8C), col = "red")

anova(Mod8C, type=3)

Mod8C_Tukey <- emmeans(Mod8C, ~Traitement)


tuk.cld.Mod8C <- cld(Mod8C_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod8C

lettersMod8C <- tuk.cld.Mod8C$.group
letters_df.Mod8C <- data.frame(Traitement = tuk.cld.Mod8C$Traitement, letters = lettersMod8C)
letters_df.Mod8C

#Italie
Mod8I <- lmer(Italy ~ Traitement + (1|Bloc) + (1|Bloc:Traitement),data = CroG8S) 
plot(Mod8I)
qqnorm(residuals(Mod8I))
qqline(residuals(Mod8I), col = "red")

anova(Mod8I, type=3)

Mod8I_Tukey <- emmeans(Mod8I, ~Traitement)


tuk.cld.Mod8I <- cld(Mod8I_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod8I

lettersMod8I <- tuk.cld.Mod8I$.group
letters_df.Mod8I <- data.frame(Traitement = tuk.cld.Mod8I$Traitement, letters = lettersMod8I)
letters_df.Mod8I

#Grèce
Mod8G <- lmer(Greece ~ Traitement + (1|Bloc) + (1|Bloc:Traitement),data = CroG8S) 
plot(Mod8G)
qqnorm(residuals(Mod8G))
qqline(residuals(Mod8G), col = "red")

anova(Mod8G, type=3)

Mod8G_Tukey <- emmeans(Mod8G, ~Traitement)


tuk.cld.Mod8G <- cld(Mod8G_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod8G

lettersMod8G <- tuk.cld.Mod8G$.group
letters_df.Mod8G <- data.frame(Traitement = tuk.cld.Mod8G$Traitement, letters = lettersMod8G)
letters_df.Mod8G

# 2) Table Traitement Semaine 2####
CroG8T <- CroG8 %>%
  pivot_wider(names_from = Traitement, 
              values_from = Cro)

#E
Mod8E <- lmer(Chickpea ~ Souche + (1|Bloc) + (1|Bloc:Souche),data = CroG8T) 
plot(Mod8E)
qqnorm(residuals(Mod8E))
qqline(residuals(Mod8E), col = "red")

anova(Mod8E, type=3)

Mod8E_Tukey <- emmeans(Mod8E, ~Souche)


tuk.cld.Mod8E <- cld(Mod8E_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod8E

lettersMod8E <- tuk.cld.Mod8E$.group
letters_df.Mod8E <- data.frame(Souche = tuk.cld.Mod8E$Souche, letters = lettersMod8E)
letters_df.Mod8E

#C
Mod8Cr <- lmer(Wheatbran ~ Souche + (1|Bloc) + (1|Bloc:Souche),data = CroG8T) 
plot(Mod8Cr)
qqnorm(residuals(Mod8Cr))
qqline(residuals(Mod8Cr), col = "red")

anova(Mod8Cr, type=3)

Mod8Cr_Tukey <- emmeans(Mod8Cr, ~Souche)


tuk.cld.Mod8Cr <- cld(Mod8Cr_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod8Cr

lettersMod8Cr <- tuk.cld.Mod8Cr$.group
letters_df.Mod8Cr <- data.frame(Souche = tuk.cld.Mod8Cr$Souche, letters = lettersMod8Cr)
letters_df.Mod8Cr


#D
Mod8D <- lmer(Levucell ~ Souche + (1|Bloc) + (1|Bloc:Souche),data = CroG8T) 
plot(Mod8D)
qqnorm(residuals(Mod8D))
qqline(residuals(Mod8D), col = "red")

anova(Mod8D, type=3)

Mod8D_Tukey <- emmeans(Mod8D, ~Souche)


tuk.cld.Mod8D <- cld(Mod8D_Tukey, Letters = letters, adjust = "Tukey")
tuk.cld.Mod8D

lettersMod8D <- tuk.cld.Mod8D$.group
letters_df.Mod8D <- data.frame(Souche = tuk.cld.Mod8D$Souche, letters = lettersMod8D)
letters_df.Mod8D

#P
Mod8P <- lmer(Bactocell ~ Souche + (1|Bloc) + (1|Bloc:Souche),data = CroG8T) 
plot(Mod8P)
qqnorm(residuals(Mod8P))
qqline(residuals(Mod8P), col = "red")

anova(Mod8P, type=3)

Mod8P_Tukey <- emmeans(Mod8P, ~Souche)


tuk.cld.Mod8P <- cld(Mod8P_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod8P

lettersMod8P <- tuk.cld.Mod8P$.group
letters_df.Mod8P <- data.frame(Souche = tuk.cld.Mod8P$Souche, letters = lettersMod8P)
letters_df.Mod8P


CroG8 %>% 
  tidyplot(x = Souche, y = Cro, color = Souche) %>% 
  add_boxplot(alpha = 0.6) %>%
  adjust_y_axis(title = '') %>%
  adjust_colors(new_colors = colors_discrete_friendly) %>%
  adjust_size(width = 60, height = 60, unit = "mm") %>%
  remove_legend() %>%
  remove_x_axis_title() %>%
  remove_y_axis_title() %>%
  split_plot(by = Traitement, ncol = 4) %>%
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Croissance semaine 8 Sou.png")




CroG8 %>% 
  tidyplot(x = Traitement, y = Cro, color = Traitement) %>% 
  add_boxplot(alpha = 0.6) %>%
  adjust_y_axis(title = '', limits = c(400,1400)) %>%
  adjust_colors(new_colors = colors_discrete_friendly) %>%
  adjust_size(width = 60, height = 60, unit = "mm") %>%
  remove_legend() %>%
  remove_x_axis_title() %>%
  remove_y_axis_title() %>%
  split_plot(by = Souche, ncol = 4) %>%
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Croissance semaine 8 Trt.png")

#Semaine 3####
CroG9 <- CroG %>%
  dplyr::select('9', Souche, Traitement, Bloc, Rep) %>%
  rename(Cro = '9')

Mod9 <- lmer(Cro ~ Souche * Traitement + 
               (1|Bloc) + (1|Bloc:Souche) + (1|Bloc:Traitement), data = CroG9)
plot(Mod9)
qqnorm(residuals(Mod9))
qqline(residuals(Mod9), col = "red")


anova(Mod9, type=3)

library(emmeans)
library(multcomp)
library(multcompView)
library(dplyr)

Mod9S_Tukey <- emmeans(Mod9, ~Souche)


tuk.cld.Mod9S <- cld(Mod9S_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod9S

lettersMod9S <- tuk.cld.Mod9S$.group
letters_df.Mod9S <- data.frame(Souche = tuk.cld.Mod9S$Souche, letters = lettersMod9S)
letters_df.Mod9S


Mod9T_Tukey <- emmeans(Mod9, ~Traitement)


tuk.cld.Mod9T <- cld(Mod9T_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod9T

lettersMod9T <- tuk.cld.Mod9T$.group
letters_df.Mod9T <- data.frame(Traitement = tuk.cld.Mod9T$Traitement, letters = lettersMod9T)
letters_df.Mod9T

CroG9 %>% 
  tidyplot(x = Souche, y = Cro, color = Souche) %>% 
  add_boxplot(alpha = 0.6) %>%
  add_data_labels(data = letters_df.Mod9S, label = letters, y = 1250, color = 'black')%>%
  remove_legend() %>%
  adjust_y_axis(title = '',limits = c(0,1300)) %>%
  adjust_colors(new_colors = colors_discrete_friendly) %>%
  adjust_size(width = 60, height = 60, unit = "mm") %>%
  view_plot(title = NULL) %>%
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Croissance semaine 9 Sou.png")

CroG9 %>%
  tidyplot(x = Traitement, y = Cro, color = Traitement) %>%
  add_boxplot(alpha = 0.6) %>%
  add_data_labels(data = letters_df.Mod9T, label = letters, y = 1250, color = 'black') %>% 
  remove_legend() %>%
  adjust_y_axis(title = '', limits = c(0,1300)) %>%
  adjust_colors(new_colors = colors_discrete_friendly_long) %>%
  adjust_size(width = 60, height = 60, unit = "mm") %>%
  view_plot(title = NULL)%>%
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Croissance semaine 9 Trt.png")

#Semaine 5####

CroG10 <- CroG %>%
  dplyr::select('10', Souche, Traitement, Bloc, Rep) %>%
  rename(Cro = '10')

Mod10 <- lmer(Cro ~ Souche * Traitement + 
               (1|Bloc) + (1|Bloc:Souche) + (1|Bloc:Traitement), data = CroG10)
plot(Mod10)
qqnorm(residuals(Mod10))
qqline(residuals(Mod10), col = "red")


anova(Mod10, type=3)


#Créations de tables séparées selon les souches et traîtements

# 1) Table Souche Semaine 5####
CroG10S <- CroG10 %>%
  pivot_wider(names_from = Souche, 
              values_from = Cro)

#Canada
Mod10C <- lmer(Canada ~ Traitement + (1|Bloc) + (1|Bloc:Traitement),data = CroG10S) 
plot(Mod10C)
qqnorm(residuals(Mod10C))
qqline(residuals(Mod10C), col = "red")

anova(Mod10C, type=3)

Mod10C_Tukey <- emmeans(Mod10C, ~Traitement)


tuk.cld.Mod10C <- cld(Mod10C_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod10C

lettersMod10C <- tuk.cld.Mod10C$.group
letters_df.Mod10C <- data.frame(Traitement = tuk.cld.Mod10C$Traitement, letters = lettersMod10C)
letters_df.Mod10C

#Italie
Mod10I <- lmer(Italy ~ Traitement + (1|Bloc) + (1|Bloc:Traitement),data = CroG10S) 
plot(Mod10I)
qqnorm(residuals(Mod10I))
qqline(residuals(Mod10I), col = "red")

anova(Mod10I, type=3)

Mod10I_Tukey <- emmeans(Mod10I, ~Traitement)


tuk.cld.Mod10I <- cld(Mod10I_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod10I

lettersMod10I <- tuk.cld.Mod10I$.group
letters_df.Mod10I <- data.frame(Traitement = tuk.cld.Mod10I$Traitement, letters = lettersMod10I)
letters_df.Mod10I

#Grèce
Mod10G <- lmer(Greece ~ Traitement + (1|Bloc) + (1|Bloc:Traitement),data = CroG10S) 
plot(Mod10G)
qqnorm(residuals(Mod10G))
qqline(residuals(Mod10G), col = "red")

anova(Mod10G, type=3)

Mod10G_Tukey <- emmeans(Mod10G, ~Traitement)


tuk.cld.Mod10G <- cld(Mod10G_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod10G

lettersMod10G <- tuk.cld.Mod10G$.group
letters_df.Mod10G <- data.frame(Traitement = tuk.cld.Mod10G$Traitement, letters = lettersMod10G)
letters_df.Mod10G

letter_df.Mod10S <- merge()

#CroG10 %>% 
 # tidyplot(x = Souche, y = Cro, color = Souche) %>% 
  #add_boxplot(alpha = 0.6) %>%
  #split_plot(by = Traitement) %>%
  #add_data_labels(data = letters_df.Mod10S, label = letters, y = 470, color = 'black')%>%
  #remove_legend() %>%
  #adjust_y_axis(title = '',limits = c(0,500)) %>%
  #adjust_colors(new_colors = colors_discrete_friendly) %>%
  #adjust_size(width = 60, height = 60, unit = "mm") %>%
  #view_plot(title = NULL) %>%
  #save_plot("Croissance semaine 7 Sou.png")

# 2) Table Traitement Semaine 5####
CroG10T <- CroG10 %>%
  pivot_wider(names_from = Traitement, 
              values_from = Cro)

#E
Mod10E <- lmer(Chickpea ~ Souche + (1|Bloc) + (1|Bloc:Souche),data = CroG10T) 
plot(Mod10E)
qqnorm(residuals(Mod10E))
qqline(residuals(Mod10E), col = "red")

anova(Mod10E, type=3)

Mod10E_Tukey <- emmeans(Mod10E, ~Souche)


tuk.cld.Mod10E <- cld(Mod10E_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod10E

lettersMod10E <- tuk.cld.Mod10E$.group
letters_df.Mod10E <- data.frame(Souche = tuk.cld.Mod10E$Souche, letters = lettersMod10E)
letters_df.Mod10E

#C
Mod10Cr <- lmer(Wheatbran ~ Souche + (1|Bloc) + (1|Bloc:Souche),data = CroG10T) 
plot(Mod10Cr)
qqnorm(residuals(Mod10Cr))
qqline(residuals(Mod10Cr), col = "red")

anova(Mod10Cr, type=3)

Mod10Cr_Tukey <- emmeans(Mod10Cr, ~Souche)


tuk.cld.Mod10Cr <- cld(Mod10Cr_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod10Cr

lettersMod10Cr <- tuk.cld.Mod10Cr$.group
letters_df.Mod10Cr <- data.frame(Souche = tuk.cld.Mod10Cr$Souche, letters = lettersMod10Cr)
letters_df.Mod10Cr


#D
Mod10D <- lmer(Levucell ~ Souche + (1|Bloc) + (1|Bloc:Souche),data = CroG10T) 
plot(Mod10D)
qqnorm(residuals(Mod10D))
qqline(residuals(Mod10D), col = "red")

anova(Mod10D, type=3)

Mod10D_Tukey <- emmeans(Mod10D, ~Souche)


tuk.cld.Mod10D <- cld(Mod10D_Tukey, Letters = letters, adjust = "Tukey")
tuk.cld.Mod10D

lettersMod10D <- tuk.cld.Mod10D$.group
letters_df.Mod10D <- data.frame(Souche = tuk.cld.Mod10D$Souche, letters = lettersMod10D)
letters_df.Mod10D

#P
Mod10P <- lmer(Bactocell ~ Souche + (1|Bloc) + (1|Bloc:Souche),data = CroG10T) 
plot(Mod10P)
qqnorm(residuals(Mod10P))
qqline(residuals(Mod10P), col = "red")

anova(Mod10P, type=3)

Mod10P_Tukey <- emmeans(Mod10P, ~Souche)


tuk.cld.Mod10P <- cld(Mod10P_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod10P

lettersMod10P <- tuk.cld.Mod10P$.group
letters_df.Mod10P <- data.frame(Souche = tuk.cld.Mod10P$Souche, letters = lettersMod10P)
letters_df.Mod10P

CroG10 %>% 
  tidyplot(x = Souche, y = Cro, color = Souche) %>% 
  add_boxplot(alpha = 0.6) %>%
  adjust_y_axis(title = '') %>%
  adjust_colors(new_colors = colors_discrete_friendly) %>%
  adjust_size(width = 60, height = 60, unit = "mm") %>%
  remove_legend() %>%
  remove_x_axis_title() %>%
  remove_y_axis_title() %>%
  split_plot(by = Traitement, ncol = 4) %>%
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Croissance semaine 10 Sou.png")


  

CroG10 %>% 
  tidyplot(x = Traitement, y = Cro, color = Traitement) %>% 
  add_boxplot(alpha = 0.6) %>%
  adjust_y_axis(title = '', limits = c(400,1400)) %>%
  adjust_colors(new_colors = colors_discrete_friendly) %>%
  adjust_size(width = 60, height = 60, unit = "mm") %>%
  remove_legend() %>%
  remove_x_axis_title() %>%
  remove_y_axis_title() %>%
  split_plot(by = Souche, ncol = 4) %>%
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Croissance semaine 10 Trt.png")


#Semaine 6####
CroG11 <- CroG %>%
  dplyr::select('11', Souche, Traitement, Bloc, Rep) %>%
  rename(Cro = '11')

Mod11 <- lmer(Cro ~ Souche * Traitement + 
                (1|Bloc) + (1|Bloc:Souche) + (1|Bloc:Traitement), data = CroG11)
plot(Mod11)
qqnorm(residuals(Mod11))
qqline(residuals(Mod11), col = "red")


anova(Mod11, type=3)

library(emmeans)
library(multcomp)
library(multcompView)
library(dplyr)

Mod11S_Tukey <- emmeans(Mod11, ~Souche)


tuk.cld.Mod11S <- cld(Mod11S_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod11S

lettersMod11S <- tuk.cld.Mod11S$.group
letters_df.Mod11S <- data.frame(Souche = tuk.cld.Mod11S$Souche, letters = lettersMod11S)
letters_df.Mod11S


Mod11T_Tukey <- emmeans(Mod11, ~Traitement)


tuk.cld.Mod11T <- cld(Mod11T_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mod11T

lettersMod11T <- tuk.cld.Mod11T$.group
letters_df.Mod11T <- data.frame(Traitement = tuk.cld.Mod11T$Traitement, letters = lettersMod11T)
letters_df.Mod11T


CroG11 %>% 
  tidyplot(x = Souche, y = Cro, color = Souche) %>% 
  add_boxplot(alpha = 0.6) %>%
  add_data_labels(data = letters_df.Mod11S, label = letters, y = 1600, color = 'black')%>%
  remove_legend() %>%
  adjust_y_axis(title = '',limits = c(0,1650)) %>%
  adjust_colors(new_colors = colors_discrete_friendly) %>%
  adjust_size(width = 60, height = 60, unit = "mm") %>%
  view_plot(title = NULL) %>%
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Croissance semaine 11 Sou.png")

CroG11 %>%
  tidyplot(x = Traitement, y = Cro, color = Traitement) %>%
  add_boxplot(alpha = 0.6) %>%
  add_data_labels(data = letters_df.Mod11T, label = letters, y = 1600, color = 'black') %>% 
  remove_legend() %>%
  adjust_y_axis(title = '', limits = c(0,1750)) %>%
  adjust_colors(new_colors = colors_discrete_friendly_long) %>%
  adjust_size(width = 60, height = 60, unit = "mm") %>%
  view_plot(title = NULL)%>%
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Croissance semaine 11 Trt.png")



#Anovas supplémentaires

#graphique####

CroGLsum <- CroGL  %>%
  group_by(Souche, Traitement, Sem)  %>%
  summarise(
    n=n(),
    mean=mean(Cro),
    sd=sd(Cro)
  )

CroGL

ggplot(CroGL,aes(Souche, Cro, colour = Traitement)) +
  geom_boxplot() +
  facet_wrap(~Sem)

CroGLsum$Sem<-as.numeric(CroGLsum$Sem)

ggplot(data = CroGLsum, aes(Sem,mean, shape = Traitement, colour = Traitement)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Souche) +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme_classic() +
  scale_x_continuous(breaks = round(seq(min(CroGLsum$Sem), max(CroGLsum$Sem)),1)) +
  theme(panel.grid.major.y = element_line(color = "lightgray",
                                          size = 0.5,
                                          linetype = 2), legend.position = "right")


ggplot(data = CroGLsum, aes(x = Sem, y = mean, colour = Souche)) + 
  geom_line() +
  geom_point() +
  ylab("") +
  xlab("") +
  labs(colour = "") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "lightgray",
                                          size = 0.5,
                                          linetype = 2))



CroGL %>%
  tidyplot(x = Sem, y = Cro, color = Traitement, dodge_width = 0) %>%
  add_sem_ribbon(alpha = 0.6) %>%
  add_boxplot(box_width = 0.4, dodge_width = 0.41, color = 'black', alpha = 0.9) %>%
  adjust_y_axis(limits = c(0,1500), breaks = c(0,300,600,900,1200,1500)) %>%
  reorder_x_axis_labels(c('7','8','9')) %>%
  remove_x_axis_title() %>%
  remove_y_axis_title() %>%
  adjust_colors(new_colors = colors_discrete_friendly) %>%
  split_plot(by = Souche, widths = 110, heights = 80)  %>%
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Courbe1.png")
  
CroC <- Cro %>%
  dplyr::select( Rep,Bloc, Souche, Traitement,  Piq) %>%
  mutate((Cro[,15] - Cro[,14])) %>%
  mutate((Cro[,16] - Cro[,14])) %>%
  mutate((Cro[,17] - Cro[,14])) %>%
  mutate((Cro[,6] - Cro[,14])) %>%
  mutate((Cro[,7] - Cro[,14])) %>% 
  mutate((Cro[,14] - Cro[,14])) %>% 
  pivot_longer(c(6:11),names_to = 'Time',values_to = 'Growth') %>% 
  mutate(Time = as.integer(Time))

CroCL <- CroC %>% 
  mutate(Treatment = case_when(
    Traitement == "C" ~ "Wheatbran",
    Traitement == "P" ~ "Bactocell",
    Traitement == "D" ~ "Levucell",
    Traitement == "E" ~ "Chickpea"
  )) %>% 
  mutate(Strain = case_when(
    Souche == "Canada" ~ "Canada",
    Souche == "Italie" ~ "Italy",
    Souche == "Grèce" ~ "Greece"
  ))
 
  

CroCL %>%
  tidyplot(x = Time, y = Growth, color = Strain, dodge_width = 0) %>%
  add_mean_line(linewidth = 0.8) %>%
  add_mean_dot(alpha = 0.9) %>%
  adjust_y_axis(limits = c(0,1500), breaks = c(0,300,600,900,1200,1500), title = "Mealworm Growth (µg)" ) %>%
  adjust_x_axis(padding = c(0,0), title = "Time (Weeks)") %>% 
  add_sd_errorbar() %>% 
  adjust_colors(new_colors = colors_discrete_friendly) %>%
  split_plot(by = Traitment, widths = 130, heights = 80) %>%
  save_plot("C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/courbe2.png")

#####Honors Graphs

CroCL10 <- CroCL %>% 
  filter(Time == 10) 

StatsCro10 <- CroCL10 %>% 
  group_by(Treatment, Strain) %>%
  summarize(
    mean = mean(Growth),
    sd = sd(Growth),
  ) %>% 
  mutate(Placement = mean + sd + 50)

print(CroCL10)


Dataplace <- StatsCro10 %>% 
  ungroup() %>% 
  mutate(labelS = c('a','b','ab','a','ab','b','a','b','a','a','b','b') )

CroCL10S <- CroCL10 %>% 
  tidyplot(x = Treatment, y = Growth, color = Strain) %>% 
  add_mean_bar(alpha = 0.4) %>%
  add_mean_dash() %>% 
  add_sd_errorbar() %>% 
  adjust_y_axis(title = 'Mean Mealworm Growth (µg/larvae)') %>%
  adjust_x_axis(title = 'Feed Treatments') %>% 
  adjust_colors(c("#812","#123765","#876212")) %>%
  adjust_size(width = 200, height = 100) %>% 
  adjust_font(fontsize = 12, family = "serif")


C1 <- CroCL10S +  ggplot2::geom_text(data = Dataplace, aes(y = Placement, label = labelS),
                                   position = position_dodge(width = 0.8), show.legend = F)
  
save_plot(C1, "C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Croissance_T_semaine_10_EXP2.tif")


StatsCro10 <- CroCL10 %>% 
  group_by(Strain, Treatment) %>%
  summarize(
    mean = mean(Growth),
    sd = sd(Growth),
  ) %>% 
  mutate(Placement = mean + sd + 50)

print(CroCL10)


Dataplace <- StatsCro10 %>% 
  ungroup() %>% 
  mutate(labelT = c('b','a','b','b','c','a','c','b','b','a','b','b')) 

CroCL10T <- CroCL10 %>% 
  tidyplot(x = Strain, y = Growth, color = Treatment) %>% 
  add_mean_bar(alpha = 0.4) %>%
  add_mean_dash() %>% 
  add_sd_errorbar() %>% 
  adjust_y_axis(title = 'Mean Mealworm Growth (µg/larvae)') %>%
  adjust_x_axis(title = 'Mealworm Strains') %>% 
  adjust_colors(c("#013","darkorange","#116735","#786512")) %>%
  adjust_size(width = 200, height = 100) %>%
  adjust_font(fontsize = 12, family = "serif")

C2 <- CroCL10T +  ggplot2::geom_text(data = Dataplace, aes(y = Placement, label = labelT),
                               position = position_dodge(width = 0.8), show.legend = F)

save_plot(C2, "C:/Users/gsain/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Croissance_S_semaine_10_EXP2.tif")

