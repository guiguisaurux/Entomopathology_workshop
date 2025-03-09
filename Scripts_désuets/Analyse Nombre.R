#Import data
Nombre1 <- read.table("TenebNombre1.txt", header = T, check.names = F)
Nombre1$Rep <- as.factor(Nombre1$Rep)

Nombre2 <- read.table("TenebNombre2.txt", header = T, check.names = F)
Nombre2$Rep <- as.factor(Nombre2$Rep)

library(dplyr)

#Merge data frames
TenebNombre <- merge(Nombre2, Nombre1, by = "Rep") 
TenebNombre$fin <- TenebNombre[,19]

hist(TenebNombreL$Nombre)

#Modify wide to long
library(tidyr)

TenebNombreL <- TenebNombre %>%
  pivot_longer(cols = c(3,6:19), 
               names_to = "Jour",
               values_to = "Nombre"
  )
TenebNombreL$Strain <- as.factor(TenebNombreL$Strain)
TenebNombreL$Bloc <- as.factor(TenebNombreL$Bloc)
TenebNombreL$Trt <- as.factor(TenebNombreL$Trt)
TenebNombreL$Jour <- as.numeric(TenebNombreL$Jour)
TenebNombreL$Mort <- 10-TenebNombreL$Nombre

NombreG <- TenebNombreL %>%
  pivot_wider(names_from = Trt, 
              values_from = Mort)

hist(NombreG$C)

#model



TenemodNombre <- glmer(Mort ~ Strain * Trt + Jour:Strain + Jour:Trt + (1|Bloc) + (1|Bloc:Trt) + (1|Bloc:Strain) + (1|Rep), data = TenebNombreL, family = poisson)

plot(TenemodNombre)

library(car)
car::Anova(TenemodNombre, type = 3)
summary(TenemodNombre)


TenebNombreLJ <- TenebNombreL %>%
  split(TenebNombreL$Jour)



#Jour 2####
TenebNombreL2 <- TenebNombreLJ$'2'

hist(TenebNombreL2$Mort)

TenemodMort2 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Trt) + (1|Bloc:Strain), family = poisson, data = TenebNombreL2)
plot(TenemodMort2)



car::Anova(TenemodMort2, type=3)

#Jour 3####
TenebNombreL3 <- TenebNombreLJ$'3'

TenemodMort3 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL3,
                      control = glmerControl(optimizer = "bobyqa"))
plot(TenemodMort3)



car::Anova(TenemodMort3, type=3)

#Jour 4####
TenebNombreL4 <- TenebNombreLJ$'4'

TenemodMort4 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL4,
                      control = glmerControl(optimizer = "bobyqa"))
plot(TenemodMort4)



car::Anova(TenemodMort4, type=3)



#Jour 5####
TenebNombreL5 <- TenebNombreLJ$'5'

TenemodMort5 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL5,
                      control = glmerControl(optimizer = "bobyqa"))
plot(TenemodMort5)



car::Anova(TenemodMort5, type=3)

#Jour 6####
TenebNombreL6 <- TenebNombreLJ$'6'

TenemodMort6 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL6,
                      control = glmerControl(optimizer = "bobyqa"))
plot(TenemodMort6)



car::Anova(TenemodMort6, type=3)

#Jour 7####
TenebNombreL7 <- TenebNombreLJ$'7'

TenemodMort7 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL7, control = glmerControl(optimizer = "bobyqa"))
plot(TenemodMort7)



car::Anova(TenemodMort7, type=3)

#Jour 8####
TenebNombreL8 <- TenebNombreLJ$'8'

TenemodMort8 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL8,
                      control = glmerControl(optimizer = "bobyqa"))
plot(TenemodMort8)



car::Anova(TenemodMort8, type=3)

#Jour 9####
TenebNombreL9 <- TenebNombreLJ$'9'

TenemodMort9 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL9,
                      control = glmerControl(optimizer = "bobyqa"))
plot(TenemodMort9)



car::Anova(TenemodMort9, type=3)

#Jour 10####
TenebNombreL10 <- TenebNombreLJ$'10'

TenemodMort10 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL10)
plot(TenemodMort10)


car::Anova(TenemodMort10, type=3)




library(emmeans)
library(multcompView)
library(multcomp)

modTenebNombreL10_Tukey <- emmeans(TenemodMort10, pairwise~Strain, adjust = "tukey")


tuk.cld.Mort10 <- cld(modTenebNombreL10_Tukey, Letters = letters)
tuk.cld.Mort10

lettersMort10 <- tuk.cld.Mort10$.group
letters_df.Mort10 <- data.frame(Strain = levels(TenebNombreL10$Strain), letters = lettersMort10)
letters_df.Mort10

placement <- TenebNombreL10 %>%
  group_by(Strain$Trt == "C") %>%
  summarise(mean(Mort*10)+5)

colnames(placement)[2] <- "Placement.Value"

letters.df.Mort10 <- left_join(letters_df.Mort10, placement)

TenebNombreL10 <- merge(TenebNombreL10, letters.df.Mort10, by = "Strain") 


#Jour 11####
TenebNombreL11 <- TenebNombreLJ$'11'

TenemodMort11 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL11,
                       control = glmerControl(optimizer = "bobyqa"))
plot(TenemodMort11)



car::Anova(TenemodMort11, type=3)


install.packages("emmeans")
library(emmeans)
modTenebNombreL11_Tukey <- emmeans(TenemodMort11, ~Strain*Trt)


tuk.cld.Mort11 <- cld(modTenebNombreL11_Tukey, Letters = letters, adjust = "none")
tuk.cld.Mort11

lettersMort11 <- tuk.cld.Mort11$.group
letters_df.Mort11 <- data.frame(Strain = levels(TenebNombreL11$Strain), letters = lettersMort11)
letters_df.Mort11

placement <- TenebNombreL11 %>%
  group_by(Strain) %>%
  summarise(mean(Mort*10)+5)

colnames(placement)[2] <- "Placement.Value"

letters.df.Mort11 <- left_join(letters_df.Mort11, placement)

TenebNombreL11 <- merge(TenebNombreL11, letters.df.Mort11, by = "Strain") 


#Jour 12####
TenebNombreL12 <- TenebNombreLJ$'12'

TenemodMort12 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL12,,
                       control = glmerControl(optimizer = "bobyqa"))
plot(TenemodMort12)



car::Anova(TenemodMort12, type=3)



install.packages("emmeans")
library(emmeans)
modTenebNombreL12_Tukey <- emmeans(TenemodMort12, ~Strain)


tuk.cld.Mort12 <- cld(modTenebNombreL12_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mort12

lettersMort12 <- tuk.cld.Mort12$.group
letters_df.Mort12 <- data.frame(Strain = levels(TenebNombreL12$Strain), letters = lettersMort12)
letters_df.Mort12

placement <- TenebNombreL12 %>%
  group_by(Strain) %>%
  summarise(mean(Mort*10)+5)

colnames(placement)[2] <- "Placement.Value"

letters.df.Mort12 <- left_join(letters_df.Mort12, placement)

TenebNombreL12 <- merge(TenebNombreL12, letters.df.Mort12, by = "Strain") 

#Jour 13####
TenebNombreL13 <- TenebNombreLJ$'13'

TenemodMort13 <- glmer(Mort ~ Trt * Strain  + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL13,
                       control = glmerControl(optimizer = "bobyqa"))
plot(TenemodMort13)



car::Anova(TenemodMort13, type=3)

install.packages("emmeans")
library(emmeans)
modTenebNombreL13_Tukey <- emmeans(TenemodMort13, ~Strain)


tuk.cld.Mort13 <- cld(modTenebNombreL13_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mort13

lettersMort13 <- tuk.cld.Mort13$.group
letters_df.Mort13 <- data.frame(Strain = levels(TenebNombreL13$Strain), letters = lettersMort13)
letters_df.Mort13

placement <- TenebNombreL13 %>%
  group_by(Strain) %>%
  summarise(mean(Mort*10)+5)

colnames(placement)[2] <- "Placement.Value"

letters.df.Mort13 <- left_join(letters_df.Mort13, placement)

TenebNombreL13 <- merge(TenebNombreL13, letters.df.Mort13, by = "Strain") 


#Jour 14####
TenebNombreL14 <- TenebNombreLJ$'14'

TenemodMort14 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL14,
                       control = glmerControl(optimizer = "bobyqa"))
plot(TenemodMort14)



car::Anova(TenemodMort14, type=3)

install.packages("emmeans")
library(emmeans)
modTenebNombreL14_Tukey <- emmeans(TenemodMort14, ~Strain)


tuk.cld.Mort14 <- cld(modTenebNombreL14_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mort14

lettersMort14 <- tuk.cld.Mort14$.group
letters_df.Mort14 <- data.frame(Strain = levels(TenebNombreL14$Strain), letters = lettersMort14)
letters_df.Mort14

placement <- TenebNombreL14 %>%
  group_by(Strain) %>%
  summarise(mean(Mort*10)+5)

colnames(placement)[2] <- "Placement.Value"

letters.df.Mort14 <- left_join(letters_df.Mort14, placement)

TenebNombreL14 <- merge(TenebNombreL14, letters.df.Mort14, by = "Strain") 

#Jour 15####
TenebNombreL15 <- TenebNombreLJ$'15'

hist(TenebNombreL15$Mort)

TenemodMort15 <- glmer(Mort ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), family = poisson, data = TenebNombreL15,
                       control = glmerControl(optimizer = "bobyqa"))
plot(TenemodMort15)
car::Anova(TenemodMort15, type=3)

install.packages("emmeans")
library(emmeans)
library(multcomp)
library(multcompView)
library(dplyr)

modTenebNombreL15_Tukey <- emmeans(TenemodMort15, ~Strain)


tuk.cld.Mort15 <- cld(modTenebNombreL15_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mort15

lettersMort15 <- tuk.cld.Mort15$.group
letters_df.Mort15 <- data.frame(Strain = tuk.cld.Mort15$Strain, letters = lettersMort15)
letters_df.Mort15


placement <- TenebNombreL15 %>%
  group_by(Strain) %>%
  summarise(50)

colnames(placement)[2] <- "Placement.Value"

letters.df.Mort15 <- left_join(letters_df.Mort15, placement)
letters.df.Mort15$Jour <- 15

TenebNombreL15 <- merge(TenebNombreL15, letters.df.Mort15, by = "Strain") 

#Graphiques
install.packages("tidyverse")
library(tidyverse)

letters.df.Mort10$Jour <- 10
letters.df.Mort11$Jour <- 11
letters.df.Mort12$Jour <- 12
letters.df.Mort13$Jour <- 13
letters.df.Mort14$Jour <- 14
 

TenebNombrelist <- list(letters.df.Mort10,
letters.df.Mort11, letters.df.Mort12, letters.df.Mort13, letters.df.Mort14, letters.df.Mort15)

TenebNombreLf <- TenebNombrelist %>%
  reduce(full_join, by=c('Strain', 'Jour', 'Placement.Value', 'letters'))

TenebNombreLfin <- full_join(TenebNombreL,TenebNombreLf, by = c('Strain', 'Jour'))




library(dplyr)

detach("package:tidyverse", unload=T)

TenebNombreLsum <- TenebNombreL  %>%
  group_by(Strain,Trt,Jour)  %>%
  summarise(
    n=n(),
    mean=mean(Mort*10),
    sd=sd(Mort*10)
  ) 
TenebNombreL15sum <- TenebNombreL15 %>%
  group_by(Strain,Trt)  %>%
  summarise(
    n=n(),
    mean=mean(Mort*10),
    sd=sd(Mort*10)
  )  %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
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

TenebNombreL15 <- TenebNombreL15 %>%
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


TenebNombreLsum <- TenebNombreLfin %>%
  group_by(Strain,Trt,Jour)  %>%
  summarise(
    n=n(),
    mean=mean(Mort*10),
    sd=sd(Mort*10)
  )  %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
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
  
  
  
  library(ggplot2)

P3  <- ggplot(data = TenebNombreLsum, aes(x = Jour, y = mean, colour = Trt)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~Strain, ncol = 5) +
  scale_color_manual(values = c("#800000","#469990")) +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme_classic() +
  ylab("") + 
  xlab("") +
  labs(fill = "") +
  scale_x_continuous(breaks = round(seq(min(TenebNombreLsum$Jour), max(TenebNombreLsum$Jour), by = 2),1)) +
  theme(panel.grid.major.y = element_line(color = "lightgray",
                                          size = 0.5,
                                          linetype = 2), legend.position = "none")

library(ggplot2)
ggplot(data = TenebNombreL15sum, aes(x = Strain, y = mean, fill = Trt)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(data = TenebNombreL15, aes(x = Strain, y = 38, label = letters), 
            size = 4, color = "black") +
  scale_fill_manual(values = c("#800000","#469990")) +
  theme_classic() +
  scale_y_continuous(lim = c(0,38)) +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme(panel.grid.major.y = element_line(color = "lightgray",
                                          size = 0.5,
                                          linetype = 2), legend.position = "none")

ggplot(data = CroG10, aes(x = Souche, y = Cro, fill = Traitement)) + 
  geom_boxplot() +
  theme_classic() +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme(panel.grid.major.y = element_line(color = "lightgray",
                                          size = 0.5,
                                          linetype = 2), legend.position = "none")

ggplot(data = CroG10, aes(x = Traitement, y = Cro, fill = Souche)) + 
  geom_boxplot() +
  theme_classic() +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme(panel.grid.major.y = element_line(color = "lightgray",
                                          size = 0.5,
                                          linetype = 2), legend.position = "none")


TenebNombresum <- TenebNombre  %>%
  group_by(Strain, Trt)  %>%
  summarise(
    n=n(),
    mean=mean(fin*10),
    sd=sd(fin*10)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) 


P3 <- ggplot(data = TenebNombresum, aes(Strain, mean, fill= Trt)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-ic, ymax=mean+ic), width=.3, position = position_dodge(.9))+
  scale_fill_manual(values = c("#800000","#469990")) +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme_classic()


ggplot(data = TenebNombreLsum, aes(x = Strain, y = mean, fill = Trt)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean - ic, ymax=mean+ic), width=.3, position = position_dodge(.9))+
  facet_wrap(~Jour, ncol = 5) +
  scale_color_manual(values = c("#800000","#469990")) +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  coord_cartesian(ylim = c(0,50))


TenebNombreL = TenebNombreL %>%
  mutate(Strain = case_when(
    Strain == "All" ~ "Allemagne", 
    Strain == "Esp" ~ "Espagne",
    Strain == "Fra" ~ "France",
    Strain == "Gre" ~ "Grèce",
    Strain == "Inv1" ~ "Norvège 1",
    Strain == "Inv2" ~ "Norvège 2",
    Strain == "Ita1" ~ "Italie 1",
    Strain == "Ita2" ~ "Italie 2",
    Strain == "Tur" ~ "Turquie",
    Strain == "Wor" ~ "Canada",
    T ~ Strain)) 

TenebNombreL$Mort = TenebNombreL$Mort*10

TenebNombreL %>%
  tidyplot(x = Jour, y = Mort, color = Trt, dodge_width = 0) %>%
  add_sem_ribbon(alpha = 0.6) %>%
  add_mean_line(size = 1, alpha = 0.9) %>%
  adjust_y_axis(limits = c(0,50), breaks = c(0,10,20,30,40,50)) %>%
  adjust_x_axis(breaks = c(0,3,6,9,12,15)) %>%
  remove_x_axis_title() %>%
  remove_y_axis_title() %>%
  split_plot(by = Strain, ncol = 5) %>%
  save_plot('courbe4.png')


