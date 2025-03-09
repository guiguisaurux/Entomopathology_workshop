
#Import data
Mass1 <- read.table("TenebMass1.txt", header = T, check.names = F)
Mass1$Rep <- as.factor(Mass1$Rep)

Mass2 <- read.table("TenebMass2.txt", header = T, check.names = F)
Mass2$Rep <- as.factor(Mass2$Rep)

library(dplyr)

#Merge data frames
TenebMass <- merge(Mass1, Mass2, by = "Rep") 



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


#Installing packages lme4 and lmerTest
library(lme4)
library(lmerTest)

#Test 1
Tenemodmass <- lmer(Mass ~ Strain * Trt + Jour + Jour:Strain + Jour:Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt) + (1|Rep), data = TenebMassL)
plot(Tenemodmass)
qqnorm(residuals(Tenemodmass))
qqline(residuals(Tenemodmass), col = "red")


#Transformation 1
Tenemodmass1 <- lmer(sqrt(Mass) ~ Strain * Trt + Jour + Jour:Strain + Jour:Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt) + (1|Rep), data = TenebMassL)
plot(Tenemodmass1)
qqnorm(residuals(Tenemodmass1)) 
qqline(residuals(Tenemodmass1), col = "red")
summary(Tenemodmass1)


aov1modmass  <- anova(Tenemodmass1, type = 3)
aov1modmass

TenebMassLJ <- TenebMassL %>%
  split(TenebMassL$Jour)
#Jour 1####
TenebMassL1 <- TenebMassLJ$'1'

TenemodMass1 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL1)
plot(TenemodMass1)
qqnorm(residuals(TenemodMass1))
qqline(residuals(TenemodMass1), col = "red")


anova(TenemodMass1, type=3)



#Jour 2####
TenebMassL2 <- TenebMassLJ$'2'

TenemodMass2 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL2)
plot(TenemodMass2)
qqnorm(residuals(TenemodMass2))
qqline(residuals(TenemodMass2), col = "red")


anova(TenemodMass2, type=3)

#Jour 3####
TenebMassL3 <- TenebMassLJ$'3'

TenemodMass3 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL3)
plot(TenemodMass3)
qqnorm(residuals(TenemodMass3))
qqline(residuals(TenemodMass3), col = "red")


anova(TenemodMass3, type=3)

#Jour 4####
TenebMassL4 <- TenebMassLJ$'4'

TenemodMass4 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL4)
plot(TenemodMass4)
qqnorm(residuals(TenemodMass4))
qqline(residuals(TenemodMass4), col = "red")


anova(TenemodMass4, type=3)



#Jour 5####
TenebMassL5 <- TenebMassLJ$'5'

TenemodMass5 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL5)
plot(TenemodMass5)
qqnorm(residuals(TenemodMass5))
qqline(residuals(TenemodMass5), col = "red")


anova(TenemodMass5, type=3)

#Jour 6####
TenebMassL6 <- TenebMassLJ$'6'

TenemodMass6 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL6)
plot(TenemodMass6)
qqnorm(residuals(TenemodMass6))
qqline(residuals(TenemodMass6), col = "red")


anova(TenemodMass6, type=3)

#Jour 7####
TenebMassL7 <- TenebMassLJ$'7'

TenemodMass7 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL7)
plot(TenemodMass7)
qqnorm(residuals(TenemodMass7))

qqline(residuals(TenemodMass7), col = "red")


anova(TenemodMass7, type=3)

#Jour 8####
TenebMassL8 <- TenebMassLJ$'8'

TenemodMass8 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL8)
plot(TenemodMass8)
qqnorm(residuals(TenemodMass8))
qqline(residuals(TenemodMass8), col = "red")


anova(TenemodMass8, type=3)

#Jour 9####
TenebMassL9 <- TenebMassLJ$'9'

TenemodMass9 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL9)
plot(TenemodMass9)
qqnorm(residuals(TenemodMass9))
qqline(residuals(TenemodMass9), col = "red")


anova(TenemodMass9, type=3)

#Jour 10####
TenebMassL10 <- TenebMassLJ$'10'

TenemodMass10 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL10)
plot(TenemodMass10)
qqnorm(residuals(TenemodMass10))
qqline(residuals(TenemodMass10), col = "red")


anova(TenemodMass10, type=3)

#Jour 11####
TenebMassL11 <- TenebMassLJ$'11'

TenemodMass11 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL11)
plot(TenemodMass11)
qqnorm(residuals(TenemodMass11))
qqline(residuals(TenemodMass11), col = "red")


anova(TenemodMass11, type=3)

#Jour 12####
TenebMassL12 <- TenebMassLJ$'12'

TenemodMass12 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL12)
plot(TenemodMass12)
qqnorm(residuals(TenemodMass12))
qqline(residuals(TenemodMass12), col = "red")


anova(TenemodMass12, type=3)


#Jour 14####
TenebMassL14 <- TenebMassLJ$'14'

TenemodMass14 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL14)
plot(TenemodMass14)
qqnorm(residuals(TenemodMass14))
qqline(residuals(TenemodMass14), col = "red")


anova(TenemodMass14, type=3)



#Jour 15####
TenebMassL15 <- TenebMassLJ$'15'

TenemodMass15 <- lmer(Mass ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL15)
plot(TenemodMass15)
qqnorm(residuals(TenemodMass15))
qqline(residuals(TenemodMass15), col = "red")


anova(TenemodMass15, type=3)


library(emmeans)
library(multcomp)
library(multcompView)
library(dplyr)

modTenebMassL15_Tukey <- emmeans(TenemodMass15, ~Strain)


tuk.cld.Mass15 <- cld(modTenebMassL15_Tukey, Letters = letters, adjust = "sidak")
tuk.cld.Mass15

lettersMass15 <- tuk.cld.Mass15$.group
letters_df.Mass15 <- data.frame(Strain = tuk.cld.Mass15$Strain, letters = lettersMass15)
letters_df.Mass15


placement <- TenebMassL15 %>%
  group_by(Strain) %>%
  summarise(50)

colnames(placement)[2] <- "Placement.Value"

letters.df.Mass15 <- left_join(letters_df.Mass15, placement)
letters.df.Mass15$Jour <- 15

TenebMassL15 <- merge(TenebMassL15, letters.df.Mass15, by = "Strain") 

#Transform

TenemodMassT15 <- lmer(sqrt(Mass) ~ Strain * Trt + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt), data = TenebMassL15)
plot(TenemodMassT15)
qqnorm(residuals(TenemodMassT15))
qqline(residuals(TenemodMassT15), col = "red")

anova(TenemodMassT15)
install.packages("ggplot2")

library(dplyr)

TenebMassLsum <- TenebMassL15  %>%
  group_by(Strain, Trt)  %>%
  summarise(
    n=n(),
    mean=mean(Mass),
    sd=sd(Mass)
  ) %>%
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
  
TenebMassL15 <- TenebMassL15 %>%
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

P1  <- ggplot(data = TenebMassLsum, aes(x = Jour, y = mean, colour = Trt)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~Strain, ncol = 5) +
  scale_color_manual(values = c("#800000","#469990")) +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme_classic() +
  scale_x_continuous(breaks = round(seq(min(TenebMassLsum$Jour), max(TenebMassLsum$Jour), by = 2),1)) +
  theme(panel.grid.major.y = element_line(color = "lightgray",
                                          size = 0.5,
                                          linetype = 2), legend.position = "none")


ggplot(data = TenebMassLsum, aes(x = Jour, y = mean, colour = Trt)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~Strain, ncol = 5) +
  scale_color_manual(values = c("#800000","#469990")) +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme_classic()


ggplot(data = TenebMassLsum, aes(x = Strain, y = mean, fill = Trt)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(data = TenebMassL15, aes(x = Strain, y = 750, label = letters), 
            size = 4, color = "black") +
  scale_fill_manual(values = c("#800000","#469990")) +
  theme_classic() +
  scale_y_continuous(lim = c(0,800)) +
  ylab("") + 
  xlab("") +
  labs(colour = "") +
  theme(panel.grid.major.y = element_line(color = "lightgray",
                                          size = 0.5,
                                          linetype = 2), legend.position = "none")

library(cowplot)

plot_grid(P1,P2,P3, P4)
P1
P2
P3
P4

