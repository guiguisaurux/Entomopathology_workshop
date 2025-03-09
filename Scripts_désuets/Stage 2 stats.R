Exp1 <- read.table("Exp1.txt", header =T)
Exp1$OD600 <- as.factor(Exp1$OD600)

Exp1 <- Exp1  %>%
  group_by(OD600)  %>%
  summarise(
    n=n(),
    mean=mean(Total*2),
    sd=sd(Total*2)
    ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) 

plotExp1 <- Exp1 %>%
  ggplot() +
  geom_bar(aes(x=OD600, y=mean, fill = OD600), stat="identity", alpha=1, size = 0.8, width = 0.5) +
  geom_errorbar( aes(x=OD600, ymin=mean-se, ymax=mean+se), width=0.4, colour="black", alpha=0.9, size=0.5) +
  theme_classic() +
  scale_fill_hue(c = 40) +
  xlab("Taitements") +
  ylab("Survie (%)") +
  annotate(geom = "text", x = 4.5, y = 105, label = "p-value > 0.05")+
  theme(legend.position = "none")

#Exp7####
Exp7.1 <- read.table("Exp7.1.txt", header = T)

mean(Exp7)
Exp7chi <- chisq.test(as.factor(Exp7.1$Traitement),as.factor(Exp7.1$Alive), correct = TRUE)

Exp7chi$expected
Exp7chi$p.value
Exp7chi$statistic
Exp7chi$residuals
Exp7chi$expected
Exp7chi$stdres


Exp7 <- read.table("exp7.txt", header=T)

Exp7 <- read.table("Exp7.txt", header =T)
Exp7$Blessure <- as.factor(Exp7$Blessure)

Exp7 <- Exp7  %>%
  group_by(Blessure)  %>%
  summarise(
    n=n(),
    mean=mean(NL3*10),
    sd=sd(NL3*10)
    ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

Exp7 <- Exp7 %>%
  mutate(Blessure = case_when(
    Traitement == "N" ~ "Sans blessure", 
    Blessure  == "S" ~ "Blessure stérile",
    Traitement == "B" ~ "Blessure septique",
    T ~ Blessure))


plotExp7 <- Exp7 %>%
  ggplot() +
  geom_bar(aes(x=(reorder(Blessure, -mean)), y=mean, fill = Blessure), stat="identity", alpha=1, size = 0.8, width = 0.5) +
  geom_errorbar( aes(x=Blessure, ymin=mean-se, ymax=mean+se), width=0.4, colour="black", alpha=0.9, size=0.5) +
  theme_classic() +
  scale_fill_hue(c = 40) +
  xlab("Taitements") +
  ylab("Survie (%)") +
  annotate(geom = "text", x = 3, y = 100, label = "p-value < 0.001") +
  annotate(geom = "text", x = 3, y = 77, label = "***", size = 8) +
  theme(legend.position = "none")




#exp8####
Exp8 <- read.table("Exp8.txt", header = T)
Exp8$Traitement <- as.factor(Exp8$Traitement)


Exp8 <- read.table("Exp8.txt", header = T)
Exp8$Traitement <- as.factor(Exp8$Traitement)



#Condition d'application
source("homovar.R")
homovar(Exp8$J4, Exp8$Traitement, centr = "MEDIAN", nperm = 999, alpha = 0.05)
shapiro.test(resid(aov(Exp8$J4 ~ Exp8$Traitement)))

#Résidus non normaux -> anova perm
source("anova.1way.R")
a1 <- anova.1way(J4 ~ Traitement,
           data = Exp8,
           nperm = 999)
#Significatif à 0.005
plot(aov(Exp8$J4 ~ Exp8$Traitement))

TukeyHSD(aov(J4 ~ Traitement, data = Exp8))

#Si significatif continuer au graphique
library(dplyr)
#5.1) Graphique
Exp8 <- Exp8  %>%
  group_by(Traitement)  %>%
  summarise(
    n=n(),
    mean=mean(J4),
    sd=sd(J4)
    ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

Exp8.1 <- read.table("Exp8.txt", header = T)
Exp8.1$Traitement <- as.factor(Exp8$Traitement)

Exp8 <- Exp8 %>%
  mutate(Traitement = case_when(
    Traitement == "C" ~ "Blessure stérile", 
    Traitement == "B" ~ "Blessure forme blanche",
    Traitement == "R" ~ "Blessure forme rouge",
    T ~ Traitement))


library(ggplot2)
plotExp8 <- Exp8 %>%
  ggplot(aes(x = reorder(Traitement, -mean), y=mean, fill = Traitement)) +
  geom_bar(stat="identity", alpha=1, size = 0.8, width = 0.5) +
  geom_errorbar( aes(x=Traitement, ymin=mean-se, ymax=mean+se), width=0.4, colour="black", alpha=0.9, linewidth=0.5) +
  theme_classic() +
  scale_fill_hue(c = 40) +
  xlab("Taitements") +
  ylab("Survie (%)") +
  annotate(geom = "text", x = 2.5, y = 100, label = "p-value globale = 0.002") +
  annotate(geom = "text", x = 1, y = 102, label = "A", size = 4) +
  annotate(geom = "text", x = 2, y = 80, label = "B", size = 4) +
  annotate(geom = "text", x = 3, y = 72, label = "B", size = 4) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) 
  
  
  
  






