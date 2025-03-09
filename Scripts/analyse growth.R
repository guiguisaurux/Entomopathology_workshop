#Import data
Mass1 <- read.table("TenebMass1.txt", header = T, check.names = F)
Mass1$Rep <- as.factor(Mass1$Rep)

Mass2 <- read.table("TenebMass2.txt", header = T, check.names = F)
Mass2$Rep <- as.factor(Mass2$Rep)

library(dplyr)

#Merge data frames
TenebMass <- merge(Mass1, Mass2, by = "Rep") 

#Data tranformations:
TenebGrowth <- TenebMass %>%
  dplyr::select(Bloc, Rep, Trt, Strain) %>%
  mutate("2" = (TenebMass[,6] - TenebMass[,3])) %>%
  mutate("3" = (TenebMass[,7] - TenebMass[,3])) %>%
  mutate("4" = (TenebMass[,8] - TenebMass[,3])) %>%
  mutate("5" = (TenebMass[,9] - TenebMass[,3])) %>%
  mutate("6" = (TenebMass[,10] - TenebMass[,3])) %>%
  mutate("7" = (TenebMass[,11] - TenebMass[,3])) %>%
  mutate("8" = (TenebMass[,12] - TenebMass[,3])) %>%
  mutate("9" = (TenebMass[,13] - TenebMass[,3])) %>%
  mutate("10" = (TenebMass[,14] - TenebMass[,3])) %>%
  mutate("11" = (TenebMass[,15] - TenebMass[,3])) %>%
  mutate("12" = (TenebMass[,16] - TenebMass[,3])) %>%
  mutate("13" = (TenebMass[,17] - TenebMass[,3])) %>%
  mutate("14" = (TenebMass[,18] - TenebMass[,3])) %>%
  mutate("15" = (TenebMass[,19] - TenebMass[,3])) 


TenebGrowthL <- TenebGrowth %>%
  pivot_longer(cols = c(5:18), 
               names_to = "Jour",
               values_to = "Growth"
  )
TenebGrowthL$Strain <- as.factor(TenebGrowthL$Strain)
TenebGrowthL$Bloc <- as.factor(TenebGrowthL$Bloc)
TenebGrowthL$Trt <- as.factor(TenebGrowthL$Trt)
TenebGrowthL$Jour <- as.numeric(TenebGrowthL$Jour)
TenebGrowthL$Growth <- as.numeric(TenebGrowthL$Growth)

TenebGrowthLJ <- TenebGrowthL %>%
  split(TenebGrowthL$Jour)


#Installing packages lme4 and lmerTest
library(lme4)
library(lmerTest)

#Test 1
TenemodGrowth <- lmer(Growth ~ Strain * Trt + Jour + Jour:Strain + Jour:Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt) + (1|Rep), data = TenebGrowthL)
plot(TenemodGrowth)
min(TenebGrowthL$Growth)

TenemodGrowthT <- lmer(log(Growth+53.00001) ~ Strain * Trt + Jour + Jour:Strain + Jour:Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt) + (1|Rep), data = TenebGrowthL, control = lmerControl(optimizer = "bobyqa"))
plot(TenemodGrowthT)
qqnorm(residuals(TenemodGrowthT))
qqline(residuals(TenemodGrowthT), col = "red")

mod2 <- anova(TenemodGrowthT, type = 3)


TenebGrowthLJ <- TenebGrowthL %>%
  split(TenebGrowthL$Jour)
#Jour 1####
TenebGrowthL1 <- TenebGrowthLJ$'1'

TenemodGrowth1 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL1)
plot(TenemodGrowth1)
qqnorm(residuals(TenemodGrowth1))
qqline(residuals(TenemodGrowth1), col = "red")


anova(TenemodGrowth1, type=3)



#Jour 2####
TenebGrowthL2 <- TenebGrowthLJ$'2'

TenemodGrowth2 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL2)
plot(TenemodGrowth2)
qqnorm(residuals(TenemodGrowth2))
qqline(residuals(TenemodGrowth2), col = "red")


anova(TenemodGrowth2, type=3)

#Jour 3####
TenebGrowthL3 <- TenebGrowthLJ$'3'

TenemodGrowth3 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL3)
plot(TenemodGrowth3)
qqnorm(residuals(TenemodGrowth3))
qqline(residuals(TenemodGrowth3), col = "red")


anova(TenemodGrowth3, type=3)

#Jour 4####
TenebGrowthL4 <- TenebGrowthLJ$'4'

TenemodGrowth4 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL4)
plot(TenemodGrowth4)
qqnorm(residuals(TenemodGrowth4))
qqline(residuals(TenemodGrowth4), col = "red")


anova(TenemodGrowth4, type=3)



#Jour 5####
TenebGrowthL5 <- TenebGrowthLJ$'5'

TenemodGrowth5 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL5)
plot(TenemodGrowth5)
qqnorm(residuals(TenemodGrowth5))
qqline(residuals(TenemodGrowth5), col = "red")


anova(TenemodGrowth5, type=3)

#Jour 6####
TenebGrowthL6 <- TenebGrowthLJ$'6'

TenemodGrowth6 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL6)
plot(TenemodGrowth6)
qqnorm(residuals(TenemodGrowth6))
qqline(residuals(TenemodGrowth6), col = "red")


anova(TenemodGrowth6, type=3)

#Jour 7####
TenebGrowthL7 <- TenebGrowthLJ$'7'

TenemodGrowth7 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL7)
plot(TenemodGrowth7)
qqnorm(residuals(TenemodGrowth7))

qqline(residuals(TenemodGrowth7), col = "red")


anova(TenemodGrowth7, type=3)

#Jour 8####
TenebGrowthL8 <- TenebGrowthLJ$'8'

TenemodGrowth8 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL8)
plot(TenemodGrowth8)
qqnorm(residuals(TenemodGrowth8))
qqline(residuals(TenemodGrowth8), col = "red")


anova(TenemodGrowth8, type=3)

#Jour 9####
TenebGrowthL9 <- TenebGrowthLJ$'9'

TenemodGrowth9 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL9)
plot(TenemodGrowth9)
qqnorm(residuals(TenemodGrowth9))
qqline(residuals(TenemodGrowth9), col = "red")


anova(TenemodGrowth9, type=3)

#Jour 10####
TenebGrowthL10 <- TenebGrowthLJ$'10'

TenemodGrowth10 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL10)
plot(TenemodGrowth10)
qqnorm(residuals(TenemodGrowth10))
qqline(residuals(TenemodGrowth10), col = "red")


anova(TenemodGrowth10, type=3)

#Jour 11####
TenebGrowthL11 <- TenebGrowthLJ$'11'

TenemodGrowth11 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL11)
plot(TenemodGrowth11)
qqnorm(residuals(TenemodGrowth11))
qqline(residuals(TenemodGrowth11), col = "red")


anova(TenemodGrowth11, type=3)

#Jour 12####
TenebGrowthL12 <- TenebGrowthLJ$'12'

TenemodGrowth12 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL12)
plot(TenemodGrowth12)
qqnorm(residuals(TenemodGrowth12))
qqline(residuals(TenemodGrowth12), col = "red")


anova(TenemodGrowth12, type=3)


#Jour 14####
TenebGrowthL14 <- TenebGrowthLJ$'14'

TenemodGrowth14 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL14)
plot(TenemodGrowth14)
qqnorm(residuals(TenemodGrowth14))
qqline(residuals(TenemodGrowth14), col = "red")


anova(TenemodGrowth14, type=3)

#Jour 15####
TenebGrowthL15 <- TenebGrowthLJ$'15'

TenemodGrowth15 <- lmer(Growth ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebGrowthL15)
plot(TenemodGrowth15)
qqnorm(residuals(TenemodGrowth15))
qqline(residuals(TenemodGrowth15), col = "red")


anova(TenemodGrowth15, type=3)


# Vieux trucs#####
library(emmeans)
library(multcomp)
library(multcompView)
library(dplyr)

modTenebGrowthL14_Tukey <- emmeans(TenemodGrowth14, ~Strain)


tuk.cld.Growth14 <- cld(modTenebGrowthL14_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Growth14

lettersGrowth14 <- tuk.cld.Growth14$.group
letters_df.Growth14 <- data.frame(Strain = tuk.cld.Growth14$Strain, letters = lettersGrowth14)
letters_df.Growth14

TenebGrowthL14 %>%
  tidyplot(x = Strain, y = Cro, color = Trt) %>%
  add_boxplot(alpha = 0.6) %>%
  add_data_labels(data = letters_df.Mod9T, label = letters, y = 1250, color = 'black') %>% 
  remove_legend() %>%
  adjust_y_axis(title = 'Croissance semaine 9', limits = c(0,1300)) %>%
  adjust_colors(new_colors = colors_discrete_friendly_long) %>%
  adjust_size(width = 60, height = 60, unit = "mm") %>%
  view_plot(title = NULL)%>%
  save_plot("Croissance semaine 9 Trt.png")

placement <- TenebGrowthL14 %>%
  group_by(Strain) %>%
  summarise(50)

colnames(placement)[2] <- "Placement.Value"

letters.df.Growth14 <- left_join(letters_df.Growth14, placement)
letters.df.Growth14$Jour <- 15

TenebGrowthL14 <- merge(TenebGrowthL14, letters.df.Growth14, by = "Strain") 



 





#Graphiques####

library(dplyr)

TenebLsum <- TenebGrowth  %>%
  group_by(Strain, Trt)  %>%
  summarise(
    n=n(),
    mean=mean(Growth),
    sd=sd(Growth)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) 

TenebLsum <- TenebGrowthL  %>%
  group_by(Strain, Trt, Jour)  %>%
  summarise(
    n=n(),
    mean=mean(Growth),
    sd=sd(Growth)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))  %>%
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

P2  <- ggplot(data = TenebLsum, aes(x = Jour, y = mean, colour = Trt)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~Strain, ncol = 5) +
  scale_color_manual(values = c("#800000","#469990")) +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme_classic() +
  scale_x_continuous(breaks = round(seq(min(TenebLsum$Jour), max(TenebLsum$Jour), by = 2),1)) +
  scale_y_continuous(breaks = round(seq(min(0), max(700), by = 50),1)) +
  theme(panel.grid.major.y = element_line(color = "lightgray",
                                          size = 0.5,
                                          linetype = 2),  legend.position = "none")


library(ggplot2)

ggplot(data = TenebGrowth, aes(Mi, Growth, shape = Strain)) +
  geom_point() +
  geom_abline()

ggplot(data = TenebLsum, aes(Strain, mean, fill= Trt)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("#800000","#469990")) +
  ylab("") + 
  xlab("") +
  labs(fill= "") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "lightgray",
                                          size = 0.5,
                                          linetype = 2), legend.position = "none")



TenebMisum <- TenebGrowthL  %>%
  group_by(Strain, Trt, Jour)  %>%
  summarise(
    n=n(),
    mean=mean(Growth),
    sd=sd(Growth)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) 

library(ggplot2)

P1 <- ggplot(data = TenebMisum, aes(Jour, mean, colour= Trt)) +
  geom_point(stat = "identity") +
  geom_line(stat= "identity") +
  facet_wrap(~Strain, ncol = 5) +
  scale_colour_manual(values = c("#800000","#469990")) +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme_classic()


TenebGrowthL = TenebGrowthL %>%
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

TenebGrowthL %>%
  tidyplot(x = Jour, y = Growth, color = Trt, dodge_width = 0) %>%
  add_sem_ribbon(alpha = 0.6) %>%
  add_mean_line(size = 1, alpha = 0.9) %>%
  adjust_y_axis(limits = c(0,600), breaks = c(0,200,400,600)) %>%
  adjust_x_axis(breaks = c(0,3,6,9,12,15)) %>%
  remove_x_axis_title() %>%
  remove_y_axis_title() %>%
  split_plot(by = Strain, ncol = 5) %>%
  save_plot('courbe3.png')


library(cowplot)

plot_grid(P1,P2)

