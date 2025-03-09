#Import data
Nombre1 <- read.table("TenebNombre1.txt", header = T, check.names = F)
Nombre1$Rep <- as.factor(Nombre1$Rep)

Nombre2 <- read.table("TenebNombre2.txt", header = T, check.names = F)
Nombre2$Rep <- as.factor(Nombre2$Rep)

library(dplyr)

#Merge data frames
TenebNombre <- merge(Nombre2, Nombre1, by = "Rep") 

TenebNombre <- TenebNombre %>%
  dplyr::select(-'1')

TenebNombreL <- TenebNombre %>%
  pivot_longer(cols = c(5:18), 
               names_to = "Jour",
               values_to = "Nombre"
  )
TenebNombreL$Strain <- as.factor(TenebNombreL$Strain)
TenebNombreL$Bloc <- as.factor(TenebNombreL$Bloc)
TenebNombreL$Trt <- as.factor(TenebNombreL$Trt)
TenebNombreL$Jour <- as.numeric(TenebNombreL$Jour)
TenebNombreL$Mort <- 10-TenebNombreL$Nombre


#Import data
Mass1 <- read.table("TenebMass1.txt", header = T, check.names = F)
Mass1$Rep <- as.factor(Mass1$Rep)

Mass2 <- read.table("TenebMass2.txt", header = T, check.names = F)
Mass2$Rep <- as.factor(Mass2$Rep)

library(dplyr)

#Merge data frames
TenebMass <- merge(Mass1, Mass2, by = "Rep") 

result <- merge(Data1, Data2)


#Modify wide to long
library(tidyr)

TenebMassL <- TenebMass %>%
  pivot_longer(cols = c(3,6:19), 
               names_to = "Jour",
               values_to = "Mass"
  )
TenebMassL$Strain <- as.factor(TenebMassL$Strain)
TenebMassL$Bloc <- as.factor(TenebMassL$Bloc)
TenebMassL$Trt <- as.factor(TenebMassL$Trt)
TenebMassL$Jour <- as.numeric(TenebMassL$Jour)


TenebBrute <- merge(TenebGrowthL, TenebNombreL, by = c('Rep','Jour','Strain','Bloc','Trt')) 
TenebBrute$Brute <- TenebBrute$Growth * TenebBrute$Nombre

#Format wide

TenebBruteW <- TenebBrute %>%
  dplyr::select(-Mass, -Nombre, -Mort) %>%
  pivot_wider(names_from = Jour,
              values_from = Brute)



TenebBrute1 <- TenebBruteW %>%
  dplyr::select(Bloc, Rep, Trt, Strain) %>%
  mutate("1" = (TenebBruteW$'2' - TenebBruteW$'1')) %>%
  mutate("2" = (TenebBruteW$'3' - TenebBruteW$'1')) %>%
  mutate("3" = (TenebBruteW$'4' - TenebBruteW$'1')) %>%
  mutate("4" = (TenebBruteW$'5' - TenebBruteW$'1')) %>%
  mutate("5" = (TenebBruteW$'5' - TenebBruteW$'1')) %>%
  mutate("6" = (TenebBruteW$'7' - TenebBruteW$'1')) %>%
  mutate("7" = (TenebBruteW$'8' - TenebBruteW$'1')) %>%
  mutate("8" = (TenebBruteW$'9' - TenebBruteW$'1')) %>%
  mutate("9" = (TenebBruteW$'10' - TenebBruteW$'1')) %>%
  mutate("10" = (TenebBruteW$'11' - TenebBruteW$'1')) %>%
  mutate("11" = (TenebBruteW$'12' - TenebBruteW$'1')) %>%
  mutate("12" = (TenebBruteW$'13' - TenebBruteW$'1')) %>%
  mutate("13" = (TenebBruteW$'14' - TenebBruteW$'1')) %>%
  mutate("14" = (TenebBruteW$'15' - TenebBruteW$'1')) 

TenebBrute <- TenebBrute1 %>%
  pivot_longer(cols = c(5:18), 
               names_to = "Jour",
               values_to = "Brute")



library(lme4)
library(lmerTest)

#Test 1
TenemodBrute <- lmer(Brute ~ Strain * Trt + Jour + Jour:Strain + Jour:Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt) + (1|Rep), data = TenebBrute)
plot(TenemodBrute)
qqnorm(residuals(TenemodBrute))
qqline(residuals(TenemodBrute), col = "red")






#Transformation 1
TenemodBrute1 <- lmer(sqrt(Brute) ~ Strain * Trt + Jour + Jour:Strain + Jour:Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt) + (1|Rep), data = TenebBrute)
plot(TenemodBrute1)
qqnorm(residuals(TenemodBrute1)) 
qqline(residuals(TenemodBrute1), col = "red")
summary(TenemodBrute1)


TenebBruteJ <- TenebBrute %>%
  split(TenebBrute$Jour)
#Jour 1####
TenebBrute1 <- TenebBruteJ$'1'

TenemodBrute1 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute1)
plot(TenemodBrute1)
qqnorm(residuals(TenemodBrute1))
qqline(residuals(TenemodBrute1), col = "red")


anova(TenemodBrute1, type=3)



#Jour 2####
TenebBrute2 <- TenebBruteJ$'2'

TenemodBrute2 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute2)
plot(TenemodBrute2)
qqnorm(residuals(TenemodBrute2))
qqline(residuals(TenemodBrute2), col = "red")


anova(TenemodBrute2, type=3)

#Jour 3####
TenebBrute3 <- TenebBruteJ$'3'

TenemodBrute3 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute3)
plot(TenemodBrute3)
qqnorm(residuals(TenemodBrute3))
qqline(residuals(TenemodBrute3), col = "red")


anova(TenemodBrute3, type=3)

#Jour 4####
TenebBrute4 <- TenebBruteJ$'4'

TenemodBrute4 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute4)
plot(TenemodBrute4)
qqnorm(residuals(TenemodBrute4))
qqline(residuals(TenemodBrute4), col = "red")


anova(TenemodBrute4, type=3)



#Jour 5####
TenebBrute5 <- TenebBruteJ$'5'

TenemodBrute5 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute5)
plot(TenemodBrute5)
qqnorm(residuals(TenemodBrute5))
qqline(residuals(TenemodBrute5), col = "red")


anova(TenemodBrute5, type=3)

#Jour 6####
TenebBrute6 <- TenebBruteJ$'6'

TenemodBrute6 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute6)
plot(TenemodBrute6)
qqnorm(residuals(TenemodBrute6))
qqline(residuals(TenemodBrute6), col = "red")


anova(TenemodBrute6, type=3)

#Jour 7####
TenebBrute7 <- TenebBruteJ$'7'

TenemodBrute7 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute7)
plot(TenemodBrute7)
qqnorm(residuals(TenemodBrute7))

qqline(residuals(TenemodBrute7), col = "red")


anova(TenemodBrute7, type=3)

#Jour 8####
TenebBrute8 <- TenebBruteJ$'8'

TenemodBrute8 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute8)
plot(TenemodBrute8)
qqnorm(residuals(TenemodBrute8))
qqline(residuals(TenemodBrute8), col = "red")


anova(TenemodBrute8, type=3)

#Jour 9####
TenebBrute9 <- TenebBruteJ$'9'

TenemodBrute9 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute9)
plot(TenemodBrute9)
qqnorm(residuals(TenemodBrute9))
qqline(residuals(TenemodBrute9), col = "red")


anova(TenemodBrute9, type=3)

#Jour 10####
TenebBrute10 <- TenebBruteJ$'10'

TenemodBrute10 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute10)
plot(TenemodBrute10)
qqnorm(residuals(TenemodBrute10))
qqline(residuals(TenemodBrute10), col = "red")


anova(TenemodBrute10, type=3)

#Jour 11####
TenebBrute11 <- TenebBruteJ$'11'

TenemodBrute11 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute11)
plot(TenemodBrute11)
qqnorm(residuals(TenemodBrute11))
qqline(residuals(TenemodBrute11), col = "red")


anova(TenemodBrute11, type=3)

#Jour 12####
TenebBrute12 <- TenebBruteJ$'12'

TenemodBrute12 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute12)
plot(TenemodBrute12)
qqnorm(residuals(TenemodBrute12))
qqline(residuals(TenemodBrute12), col = "red")


anova(TenemodBrute12, type=3)

#Jour 13####
TenebBrute13 <- TenebBruteJ$'13'

TenemodBrute13 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute13)
plot(TenemodBrute13)
qqnorm(residuals(TenemodBrute13))
qqline(residuals(TenemodBrute13), col = "red")


anova(TenemodBrute14, type=3)

#Jour 14####
TenebBrute14 <- TenebBruteJ$'14'

TenemodBrute14 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute14)
plot(TenemodBrute14)
qqnorm(residuals(TenemodBrute14))
qqline(residuals(TenemodBrute14), col = "red")


anova(TenemodBrute14, type=3)




#Jour 15####
TenebBrute15 <- TenebBruteJ$'15'

TenemodBrute15 <- lmer(Brute ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebBrute15)
plot(TenemodBrute15)
qqnorm(residuals(TenemodBrute15))
qqline(residuals(TenemodBrute15), col = "red")


anova(TenemodBrute15, type=3)





TenebBrutesum <- TenebBrute  %>%
  group_by(Strain, Trt, Jour)  %>%
  summarise(
    n=n(),
    mean=mean(Brute),
    sd=sd(Brute)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) 

TenebBrutesum <- TenebBrute  %>%
  group_by(Strain, Trt, Jour)  %>%
  summarise(
    n=n(),
    mean=mean(Brute),
    sd=sd(Brute)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  mutate(Strain = case_when(
    Strain == "All" ~ "Germany", 
    Strain == "Esp" ~ "Spain",
    Strain == "Fra" ~ "France",
    Strain == "Gre" ~ "Grece",
    Strain == "Inv1" ~ "Norway 1",
    Strain == "Inv2" ~ "Norway 2",
    Strain == "Ita1" ~ "Italy 1",
    Strain == "Ita2" ~ "Italy 2",
    Strain == "Tur" ~ "Turkey",
    Strain == "Wor" ~ "Canada",
    T ~ Strain)) 



library(ggplot2)

P4  <- ggplot(data = TenebBrutesum, aes(x = Jour, y = mean, colour = Trt)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~Strain, ncol = 5) +
  scale_color_manual(values = c("#800000","#469990")) +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme_classic() +
  scale_x_continuous(breaks = round(seq(min(TenebBrutesum$Jour), max(TenebBrutesum$Jour), by = 2),1)) +
  theme(panel.grid.major.y = element_line(color = "lightgray",
                                          size = 0.5,
                                          linetype = 2),  legend.position = "none")

ggplot(data = TenebBrutesum, aes(x = Strain, y = mean, fill = Trt)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=mean-ic, ymax=mean+ic), width=.3, position = position_dodge(.9))+
  facet_wrap(~Jour, ncol = 5) +
  scale_color_manual(values = c("#800000","#469990")) +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 50))


TenebBrute = TenebBrute %>%
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

TenebBrute %>%
  tidyplot(x = Jour, y = Brute, color = Trt, dodge_width = 0) %>%
  add_sem_ribbon(alpha = 0.6) %>%
  add_mean_line(linewidth = 1)  %>%
  remove_x_axis_title() %>%
  adjust_y_axis(limits = c(0,4500)) %>%
  remove_y_axis_title() %>%
  split_plot(by = Strain, ncol = 5, widths = 30, heights = 30) %>%
  save_plot('courbe6.png')
