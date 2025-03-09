#importation des données:
RoseSpi <- read.table("RoseSpi.txt", header = T)
RoseSpi$Plant <- as.factor(RoseSpi$Plant)
RoseSpi$Insecte <- as.factor(RoseSpi$Insecte)

#prétransformation (Ln(y+1))
RoseSpi$Nombre <- log(RoseSpi$Nombre + 1)


#conditions d'application
shapiro.test(resid(aov(RoseSpi$Nombre ~ RoseSpi$Plant)))
homovar(RoseSpi$Nombre, RoseSpi$Plant, centr = "MEDIAN", nperm=999, alpha=0.05)

source("anova.1way.R")
anova.1way(Nombre ~ Plant,
           data = RoseSpi,
           nperm = 999)
modSpi <- aov(Nombre ~ Plant, data = RoseSpi)
TukRSpi  <- TukeyHSD(aov(modSpi))
plot(TukRSpi)


#Lettres:
library(multcomp)
modSpi_tukey <- glht(modSpi, linfct = mcp(Plant = "Tukey"))

tuk.cld.Spi <- cld(modSpi_tukey)
tuk.cld.Spi

lettersSpi <- tuk.cld.Spi$mcletters$Letters
letters_df.Spi <- data.frame(Plant = levels(RoseSpi$Plant), letters = lettersSpi)

#Graphique:

library(ggplot2)
ggplot(RoseSpi, aes(x = Plant, y = Nombre, colour = Plant)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.25) +
  theme_classic()+
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
  geom_text(data = letters_df.Spi, aes(label = letters, y = 5), colour = "black", size = 5) +
  labs(y = "ln(x + 1)", x = "Type de plante")

