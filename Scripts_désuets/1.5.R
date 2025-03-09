exp3 <- read.table("Exp3entomopathologie.txt", header = T)
exp3

exp3$Replica <-as.factor(exp3$Replica)
exp3$OD600 <- as.factor(exp3$OD600)
exp3$TL <- as.factor(exp3$TL)
exp3$Vivantes2a <- as.numeric(exp3$Vivantes2a)

#anova du 2 avril
aov2av <- aov(exp3$Vivantes2a ~ exp3$TL* exp3$OD600 )
shapiro.test(resid(aov2av))

summary(aov2av)

hist(exp3$Vivantes2a)

interaction.plot(exp3$OD600,  exp3$TL,exp3$Vivantes2a)

#conditions d'application
exp3$grp <- interaction(exp3$OD600, exp3$TL, sep = ".")

source("homovar.R")
homovar(exp3$Vivantes2a, exp3$grp, centr = "MEDIAN", nperm=999, alpha=0.05)
bartlett.test(exp3$Vivantes2a ~ exp3$grp)
shapiro.test(resid(aov(exp3$Vivantes2a ~ exp3$grp)))

shapiro.test(resid(tenebs.aov.2w))#Respecté
teneb$grp<-interaction(teneb$Colonies, teneb$Treatment,sep=".")
bartlett.test(teneb$Maxbiomass~teneb$grp)#respecté




exp1 <- read.table("exp1.txt", header = T)
exp1

exp1$OD600 <- as.factor(exp1$OD600)

shapiro.test(resid(aov(exp1$TV2M ~ exp1$CFU)))
bartlett.test(exp1$TV2M ~ exp1$)

modAncova <- aov(TV2M ~ OD600*CFU, data=exp1)

shapiro.test(resid(modAncova))

homovar(exp1$TV2M, exp1$OD600, nperm = 999)

summary(modAncova)

modAncova2 <- aov(TV2M ~ OD600+CFU, data=exp1)
summary(modAncova2)
exp1$OD600
mod1 <- lm(exp1[exp1$OD600=="Ctrl","TV2M"] ~ exp1[exp1$OD600=="Ctrl","CFU"])
mod2 <- lm(exp1[exp1$OD600==0.267,"TV2M"] ~ exp1[exp1$OD600==0.267,"CFU"])
mod3 <- lm(exp1[exp1$OD600==-1,"TV2M"] ~ exp1[exp1$OD600==-1,"CFU"])
mod4 <- lm(exp1[exp1$OD600==-2,"TV2M"] ~ exp1[exp1$OD600==-2,"CFU"])
mod5 <- lm(exp1[exp1$OD600==-3,"TV2M"] ~ exp1[exp1$OD600==-3,"CFU"])
mod6 <- lm(exp1[exp1$OD600==-4,"TV2M"] ~ exp1[exp1$OD600==-4,"CFU"])
mod7 <- lm(exp1[exp1$OD600==-5,"TV2M"] ~ exp1[exp1$OD600==-5,"CFU"])

plot(exp1$CFU, exp1$TV2M, col = exp1$OD600,
     xlab = "CFU/g de son de blé", ylab = "Larvae alive on May 2", ylim=c(0,50))
abline(mod1)
abline(mod2)


Aov1 <- aov(TV2M ~ OD600, data = exp1)

summary(Aov1)

library(ggplot2)
ggplot(exp1, aes(x = OD600, y = TV2M, colour = OD600)) +
  geom_bar(stat = "identity")) +
  geom_jitter(width = 0.25) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
  labs(y = "Number of alive larvae on May 2", x = "Bacterial treatments")

boxplot(exp1$TV2M ~ exp1$OD600,
        ylab = "Number of alive insects on may 2", 
        xlab = "Treatment",
        col =c("lightcoral","lightblue","lightgreen","lightyellow","lightpink", "red", "green"))
barplot(TV2M~OD600, data=exp1 )


data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
exp1$CFU <- as.factor(exp1$CFU)
exp1$Day<- as.factor(exp1$Day)
exp

df3 <- data_summary(exp1, varname="Total", 
                    groupnames= c("Day","CFU"))



g <- ggplot(df3, aes(x=CFU, y=Total, fill=Day)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Total-sd, ymax=Total+sd), width=.2,
                position=position_dodge(.9)) +
  labs(y = "", x = "")

g + scale_fill_manual(values = c("lightblue", "gold"))

install.packages("ggpubr")
library(ggpubr)
theme_set(theme_pubr())

colol <- c("lightblue", "cyan")

f <- ggplot(df4, aes(x=CFU, y=TV27F,)) + 
  geom_bar(stat="identity", position=position_dodge(), width =0.5) +
  geom_errorbar(aes(ymin=TV27F-sd, ymax=TV27F+sd), width=.2,
                position=position_dodge(.9)) +
  labs(y = "", x = "")
g + f

p+ scale_fill_brewer(palette="Blues")
p + scale_fill_manual(values =c("gold", "wheat3", "plum1", "orchid3", "turquoise", "lightblue1", "grey"),
                     name  ="OD600",
                     breaks=c("Ctrl", "0.267", "-1", "-2", "-3", "-4", "-5"),
                     labels=c("Control","Treatment 1", "Treatment 2", "Treatment 3", "Treatment 4", "Treatment 5", "Treatment 6"))

