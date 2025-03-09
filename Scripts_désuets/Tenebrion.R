#####
#importation des données (tous tableaux confondus)
#####
# IMPORTATION DES DONNÉES SÉPARÉES
tenebcan<-read.table("tenebcan.txt", header=T)
tenebcan$Colonies<-as.factor(tenebcan$Colonies)
tenebcan$Incubator<-as.factor(tenebcan$Incubator)
tenebcan$Treatment<-as.factor(tenebcan$Treatment)

tenebcan_5<-read.table("tenebcan_5.txt", header=T)
tenebcan_5$Colonies<-as.factor(tenebcan_5$Colonies)
tenebcan_5$Incubator<-as.factor(tenebcan_5$Incubator)
tenebcan_5$Treatment<-as.factor(tenebcan_5$Treatment)

teneb<-read.table("teneb.txt", header=T)
teneb$Colonies<-as.factor(teneb$Colonies)
teneb$Incubator<-as.factor(teneb$Incubator)
teneb$Treatment<-as.factor(teneb$Treatment)

tenebgre<-read.table("tenebgre.txt", header=T)
tenebgre$Colonies<-as.factor(tenebgre$Colonies)
tenebgre$Incubator<-as.factor(tenebgre$Incubator)
tenebgre$Treatment<-as.factor(tenebgre$Treatment)

tenebgre_5<-read.table("tenebgre_5.txt", header=T)
tenebgre_5$Colonies<-as.factor(tenebgre_5$Colonies)
tenebgre_5$Incubator<-as.factor(tenebgre_5$Incubator)
tenebgre_5$Treatment<-as.factor(tenebgre_5$Treatment)

tenebit<-read.table("tenebit.txt", header=T)
tenebit$Colonies<-as.factor(tenebit$Colonies)
tenebit$Incubator<-as.factor(tenebit$Incubator)
tenebit$Treatment<-as.factor(tenebit$Treatment)

tenebit_5<-read.table("tenebit_5.txt", header=T)
tenebit_5$Colonies<-as.factor(tenebit_5$Colonies)
tenebit_5$Incubator<-as.factor(tenebit_5$Incubator)
tenebit_5$Treatment<-as.factor(tenebit_5$Treatment)

tenebbran<-read.table("tenebbran.txt", header=T)
tenebbran$Colonies<-as.factor(tenebbran$Colonies)
tenebbran$Incubator<-as.factor(tenebbran$Incubator)
tenebbran$Treatment<-as.factor(tenebbran$Treatment)

tenebag<-read.table("tenebag.txt", header=T)
tenebag$Colonies<-as.factor(tenebag$Colonies)
tenebag$Incubator<-as.factor(tenebag$Incubator)
tenebag$Treatment<-as.factor(tenebag$Treatment)

tenebeau<-read.table("tenebeau.txt", header=T)
tenebeau$Colonies<-as.factor(tenebeau$Colonies)
tenebeau$Incubator<-as.factor(tenebeau$Incubator)
tenebeau$Treatment<-as.factor(tenebeau$Treatment)

tenebpat<-read.table("tenebpat.txt", header=T)
tenebpat$Colonies<-as.factor(tenebpat$Colonies)
tenebpat$Incubator<-as.factor(tenebpat$Incubator)
tenebpat$Treatment<-as.factor(tenebpat$Treatment)

tenebcar<-read.table("tenebcar.txt", header=T)
tenebcar$Colonies<-as.factor(tenebcar$Colonies)
tenebcar$Incubator<-as.factor(tenebcar$Incubator)
tenebcar$Treatment<-as.factor(tenebcar$Treatment)

tenebrion<-read.table("tenebrion.txt", header=T)
tenebrion$Colonies<-as.factor(tenebrion$Colonies)
tenebrion$Incubator<-as.factor(tenebrion$Incubator)
tenebrion$Treatment<-as.factor(tenebrion$Treatment)
#####
#AOV 1 fact originale (à titre indicatif pour AOV subséquentes)
#####

#1: effet des colonies sur la croissance
#2: 0: toutes ont une croissance égale
#   1: croissance diff pour au moins 1 colonie
#3: Anova 1 fact
#4: conditions d'application: 
#Construction mod. ANOVA
teneb.aov<-aov(tenebrion$Maxbiomass~tenebrion$Colonies)
#ind. données
#Dis. normale résidus
shapiro.test(resid(teneb.aov))
#variance non normales (P-value 2.267e-11)
#barlett permutationnel
source("bartlett.perm.R")
bartlett.perm(tenebrion$Maxbiomass,tenebrion$Colonies, centr="MEDIAN", nperm=999, alpha=0.05)
#variances homogènes, test permutationnel
source("anova.1way.R")
anova.1way(teneb.aov,nperm=999)
summary(teneb.aov)
#conclusion: pas de diff entre les souches pour poids max


tenebcan<-tenebrion[tenebrion$Maxbiomass & tenebrion$Treatment == "Carrot"
                    & tenebrion$Treatment == "Potato"
                    & tenebrion$Treatment == "Agar"
                    & tenebrion$Treatment == "Water"
                    & tenebrion$Treatment == "Wheatbran"
                    & tenebrion$Colonies == "Canada"]
tenebgre<-tenebrion[tenebrion$Maxbiomass & tenebrion$Colonies == "Grece", ]
tenebita<-tenebrion[tenebrion$Maxbiomass & tenebrion$Colonies == "Italy", ]
Tenebtrait<-tenebrion[tenebcan & tenebgre & tenebita]
tenebcan

plot(tenebcan$Treatment , tenebrion$Maxbiomass)

plot(tenebrion$Colonies, tenebrion$Maxbiomass)
plot(tenebrion$Treatment, t)

teneb.aov2<-aov(tenebrion$Maxbiomass~tenebrion$Treatment)
shapiro.test(resid(teneb.aov2))
bartlett.test(tenebrion$Maxbiomass,tenebrion$Treatment)
#résidud pas cool, test kruskal wallis
kruskal.test(tenebrion$Maxbiomass, tenebrion$Treatment)
#procédure de Holm puisque P-value=3.794e-12

ctrl<-tenebrion$Maxbiomass[tenebrion$Treatment=="Wheatbran"]
eau<-tenebrion$Maxbiomass[tenebrion$Treatment=="Water"]
agar<-tenebrion$Maxbiomass[tenebrion$Treatment=="Agar"]
carotte<-tenebrion$Maxbiomass[tenebrion$Treatment=="Carrot"]
patate<-tenebrion$Maxbiomass[tenebrion$Treatment=="Potato"]
agar.eau<-tenebrion$Maxbiomass[tenebrion$Treatment=="Agar/water"]
car.eau<-tenebrion$Maxbiomass[tenebrion$Treatment=="Carrot/water"]
pat.eau<-tenebrion$Maxbiomass[tenebrion$Treatment=="Potato/water"]

wilcox.test(ctrl,eau) #1
wilcox.test(ctrl,agar) #2
wilcox.test(ctrl,carotte) #3
wilcox.test(ctrl,patate) #4
wilcox.test(agar,eau)#5
wilcox.test(agar,carotte)#6
wilcox.test(agar,patate)#7
wilcox.test(carotte,eau)#8
wilcox.test(carotte,patate)#9
wilcox.test(patate,eau)#10
t1<-4.408e-10
t2<-2.204e-10
t3<-2.204e-10
t4<-2.204e-10
t5<-0.0008281
t6<-0.1136
t7<-0.126
t8<-1.536e-06
t9<-0.8636
t10<-5.289e-09

#ajustement de holm

p.adjust(c(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10), method="holm")
# [1] 1: 3.0856e-09 
# 2: 2.2040e-09 
#3: 2.2040e-09 
#4: 2.2040e-09 
#5: 3.3124e-03
#6: 3.4080e-01
#7: 3.4080e-01 
#8: 7.6800e-06 
#9: 8.6360e-01 
#10: 3.1734e-08
#significatifs: 1, 2, 3, 4, 5, 8 et 10
#pas de significatif entre aguar-patate et carotte-patate.

# lettres: rien:a, eau:b, agar:c, carotte:d, patate:e,




#####
#Copie? à vérifier
#####
#le modèle semble respecter l'homoscélasticité
tenebs.2w.tuk<-TukeyHSD(tenebs.aov.2w)
tenebs.2w.tuk

#" find out which group means are statistically different from one another so we can add this information to the graph."
tenebs.plot.aov<-aov(teneb$Maxbiomass~teneb$Colonies:teneb$Treatment)
tenebs.plot.tuk<-TukeyHSD(tenebs.plot.aov)

plot(tenebs.plot.tuk, las=1, cex.axis=0.28)
par(mar=c(4,5,4,4))

# Box plot with two factor variables
boxplot(teneb$Maxbiomass ~ teneb$Colonies * teneb$Treatment, frame = FALSE, 
        col = c("#00AFBB", "#E7B800", "#00EFBC"))
# Two-way interaction plot
interaction.plot(x.factor = teneb$Colonies, trace.factor = teneb$Treatment, 
                 response = teneb$Maxbiomass, fun = mean, 
                 type = "b", legend = TRUE,
                 pch=c(1,19), col = c("#00AFBB", "#E7B800", "#00EFBC","#A1B800","#E1A200"))
interaction.plot(x.factor = teneb$Treatment, trace.factor = teneb$Colonies, 
                 response = teneb$Daysto4.5, fun = mean, 
                 type = "b", legend = TRUE,
                 pch=c(1,19), col = c("#00AFBB", "#E7B800", "#00EFBC","#A1B800","#E1A200"))

boxplot(teneb$Daysto4.5 ~ teneb$Colonies * teneb$Treatment, frame = FALSE, 
        col = c("#00AFBB", "#E7B800", "#00EFBC"))



#Voir laius pour vérifier si je peux utiliser les résultats de mon test post hoc.
#####
#laius :1 ggplot
#####

install.packages("ggplot2")
library(ggplot2)

ggplot(teneb, aes(x=teneb$Treatment, y=teneb$Maxbiomass, colour=teneb$Colonies, fill=teneb$Colonies))+
  geom_point(position=position_jitterdodge(dodge.width=0.7),size=2)+
  geom_boxplot(alpha=0.5,position = position_dodge(width=0.8))+
  ylab("Growth")

# Calcul rapide des moyennes et écart types
install.packages("Rmisc")
library(Rmisc)
teneb_avg <- summarySE(teneb,measurevar="Maxbiomass",
                       groupvars=c("Treatment","Colonies"))
teneb_avg


ggplot(teneb_avg, aes(x=teneb$Treatment, y=teneb$Maxbiomass, colour=teneb$Colonies, 
                    group=teneb$Colonies)) + 
  geom_errorbar(aes(ymin=teneb$Maxbiomass-ci, ymax=teneb$Maxbiomass+ci), 
                width=.1) +
  geom_line(size=2) +
  geom_point(size=1)+
  theme_classic()
#####
#série de tests wilcox (4.5g)
#####

p1<-wilcox.test(can.eau,can.agar)$p.value #1
p2<-wilcox.test(can.eau,can.carotte)$p.value#2
p3<-wilcox.test(can.eau,can.patate)$p.value #3
p4<-wilcox.test(can.eau,gre.eau)$p.value #4
p5<-wilcox.test(can.eau,gre.agar)$p.value #5
p6<-wilcox.test(can.eau,gre.patate)$p.value #6
p7<-wilcox.test(can.eau,gre.carotte)$p.value #7
p8<-wilcox.test(can.eau,tt.carotte)$p.value #8
p9<-wilcox.test(can.eau,tt.eau)$p.value #9
p10<-wilcox.test(can.eau,tt.patate)$p.value #10
p11<-wilcox.test(can.eau,tt.agar)$p.value #11

##CAN AGAR
p12<-wilcox.test(can.agar,can.carotte)$p.value#12
p13<-wilcox.test(can.agar,can.patate)$p.value #13
p14<-wilcox.test(can.agar,gre.eau)$p.value #14
p15<-wilcox.test(can.agar,gre.agar)$p.value #15
p16<-wilcox.test(can.agar,gre.patate)$p.value #16
p17<-wilcox.test(can.agar,gre.carotte)$p.value #17
p18<-wilcox.test(can.agar,tt.carotte)$p.value #18
p19<-wilcox.test(can.agar,tt.eau)$p.value #19
p20<-wilcox.test(can.agar,tt.patate)$p.value #20
p21<-wilcox.test(can.agar,tt.agar)$p.value #21

p22<-wilcox.test(can.carotte,can.patate)$p.value #22
p23<-wilcox.test(can.carotte,gre.eau)$p.value #23
p24<-wilcox.test(can.carotte,gre.agar)$p.value #24
p25<-wilcox.test(can.carotte,gre.patate)$p.value #25
p26<-wilcox.test(can.carotte,gre.carotte)$p.value #26
p27<-wilcox.test(can.carotte,tt.carotte)$p.value #27
p28<-wilcox.test(can.carotte,tt.eau)$p.value #28
p29<-wilcox.test(can.carotte,tt.patate)$p.value #29
p30<-wilcox.test(can.carotte,tt.agar)$p.value #30

#can pat
p31<-wilcox.test(can.patate,gre.eau)$p.value #31
p32<-wilcox.test(can.patate,gre.agar)$p.value #32
p33<-wilcox.test(can.patate,gre.patate)$p.value #33
p34<-wilcox.test(can.patate,gre.carotte)$p.value #34
p35<-wilcox.test(can.patate,tt.carotte)$p.value #35
p36<-wilcox.test(can.patate,tt.eau)$p.value #36
p37<-wilcox.test(can.patate,tt.patate)$p.value #37
p38<-wilcox.test(can.patate,tt.agar)$p.value #38

#gre eau
p39<-wilcox.test(gre.eau,gre.agar)$p.value #39
p40<-wilcox.test(gre.eau,gre.patate)$p.value #40
p41<-wilcox.test(gre.eau,gre.carotte)$p.value #41
p42<-wilcox.test(gre.eau,tt.carotte)$p.value #42
p43<-wilcox.test(gre.eau,tt.eau)$p.value #43
p44<-wilcox.test(gre.eau,tt.patate)$p.value #44
p45<-wilcox.test(gre.eau,tt.agar)$p.value #45

#gre agar
p46<-wilcox.test(gre.agar,gre.patate)$p.value #46
p47<-wilcox.test(gre.agar,gre.carotte)$p.value #47
p48<-wilcox.test(gre.agar,tt.carotte)$p.value #48
p49<-wilcox.test(gre.agar,tt.eau)$p.value #49
p50<-wilcox.test(gre.agar,tt.patate)$p.value #50
p51<-wilcox.test(gre.agar,tt.agar)$p.value #51

#gre pat
p52<-wilcox.test(gre.patate,gre.carotte)$p.value #52
p53<-wilcox.test(gre.patate,tt.carotte)$p.value #53
p54<-wilcox.test(gre.patate,tt.eau)$p.value #54
p55<-wilcox.test(gre.patate,tt.patate)$p.value #55
p56<-wilcox.test(gre.patate,tt.agar)$p.value #56


#gre car
p57<-wilcox.test(gre.carotte,tt.carotte)$p.value #57
p58<-wilcox.test(gre.carotte,tt.eau)$p.value #58
p59<-wilcox.test(gre.carotte,tt.patate)$p.value #59
p60<-wilcox.test(gre.carotte,tt.agar)$p.value #60

#tt.carot
p61<-wilcox.test(tt.carotte,tt.eau)$p.value #61
p62<-wilcox.test(tt.carotte,tt.patate)$p.value #62
p63<-wilcox.test(tt.carotte,tt.agar)$p.value #63

#tt eau
p64<-wilcox.test(tt.eau,tt.patate)$p.value #64
p65<-wilcox.test(tt.eau,tt.agar)$p.value #65


p66<-wilcox.test(tt.patate,tt.agar)$p.value #66

p100<-c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,
        p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
        p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,
        p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,
        p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,
        p51,p52,p53,p54,p55,p56,p57,p58,p59,p60,
        p61,p62,p63,p64,p65,p66)

p100

p.adjust(c(p100), method="holm")

#####
#tests de dunn (à titre indicatif) (4.5g)
#####
teneb$grp

dunn.test(teneb$Daysto4.5, teneb$grp, method="holm", alpha=0.05)

pairwise.wilcox.test(teneb$Daysto4.5, teneb$grp, p.adjust.method="fdr")




#####
#Test 4.5 g:
#####
teneb<-read.table("teneb.txt", header=T)
teneb$Colonies<-as.factor(teneb$Colonies)
teneb$Incubator<-as.factor(teneb$Incubator)
teneb$Treatment<-as.factor(teneb$Treatment)

aov_4.5<-aov(teneb$Daysto4.5~teneb$Colonies*teneb$Treatment)
aov_4.5


shapiro.test(resid(aov_4.5))   #Non respecté
teneb$grp<-interaction(teneb$Colonies, teneb$Treatment,sep=".")
bartlett.test(teneb$Daysto4.5~teneb$grp)  #Non respecté

kruskal.test(teneb$Daysto4.5~teneb$grp)


can.eau<-teneb$Daysto4.5[teneb$grp=="Can.Wa"]
can.agar<-teneb$Daysto4.5[teneb$grp=="Can.Ag"]
can.carotte<-teneb$Daysto4.5[teneb$grp=="Can.Car"]
can.patate<-teneb$Daysto4.5[teneb$grp=="Can.Pot"]
gre.eau<-teneb$Daysto4.5[teneb$grp=="Gre.Wa"]
gre.agar<-teneb$Daysto4.5[teneb$grp=="Gre.Ag"]
gre.carotte<-teneb$Daysto4.5[teneb$grp=="Gre.Car"]
gre.patate<-teneb$Daysto4.5[teneb$grp=="Gre.Pot"]
tt.eau<-teneb$Daysto4.5[teneb$grp=="It.Wa"]
tt.agar<-teneb$Daysto4.5[teneb$grp=="It.Ag"]
tt.carotte<-teneb$Daysto4.5[teneb$grp=="It.Car"]
tt.patate<-teneb$Daysto4.5[teneb$grp=="It.Pot"]

install.packages("dunn.test")
library(dunn.test)

#####
#AOV 2 facteur sur tableau teneb.txt (maxbiomass)
#####
teneb.aov.2w<-aov(tenebrion$Maxbiomass~tenebrion$Colonies*tenebrion$Treatment)
summary(teneb.aov.2w)
teneb.aov<-aov(tenebrion$Maxbiomass~tenebrion$Colonies)
summary(teneb.aov)
teneb.aov.2f<-aov(tenebrion$Maxbiomass~tenebrion$Colonies+tenebrion$Treatment)
summary(teneb.aov.2f)
teneb.aov.3w<-aov(tenebrion$Maxbiomass~tenebrion$Colonies+tenebrion$Treatment+tenebrion$Incubator)
summary(teneb.aov.3w)

#meilleur test
install.packages("AICcmodavg")
library(AICcmodavg)

teneb.set<-list(teneb.aov, teneb.aov.2f, teneb.aov.2w, teneb.aov.3w)
teneb.names<-c("teneb.aov","teneb.aov.2f", "teneb.aov.2w", "teneb.aov.3w")
aictab(teneb.set, modnames=teneb.names)

#Le modèle teneb.aov.2w est le meilleur

par(mfrow=c(2,2))
plot(teneb.aov.2w)
par(mfrow=c(1,1))

#le modèle semble respecter l'homoscélasticité
teneb.2w.tuk<-TukeyHSD(teneb.aov.2w)
teneb.2w.tuk



#" find out which group means are statistically different from one another so we can add this information to the graph."
teneb.plot.aov<-aov(tenebrion$Maxbiomass~tenebrion$Colonies:tenebrion$Treatment)
teneb.plot.tuk<-TukeyHSD(teneb.plot.aov)
plot(teneb.plot.tuk, las=1)



# IMPORTATION DES DONNÉES SÉPARÉES
tenebcan<-read.table("tenebcan.txt", header=T)
tenebcan$Colonies<-as.factor(tenebcan$Colonies)
tenebcan$Incubator<-as.factor(tenebcan$Incubator)
tenebcan$Treatment<-as.factor(tenebcan$Treatment)
teneb<-read.table("teneb.txt", header=T)
teneb$Colonies<-as.factor(teneb$Colonies)
teneb$Incubator<-as.factor(teneb$Incubator)
teneb$Treatment<-as.factor(teneb$Treatment)

#TESTS AOV SUR TENEB
tenebs.aov.2w<-aov(teneb$Maxbiomass~teneb$Colonies*teneb$Treatment)
summary(tenebs.aov.2w)
tenebs.aov<-aov(teneb$Maxbiomass~teneb$Colonies)
summary(tenebs.aov)
tenebs.aov.2f<-aov(teneb$Maxbiomass~teneb$Colonies+teneb$Treatment)
summary(tenebs.aov.2f)
tenebs.aov.3w<-aov(teneb$Maxbiomass~teneb$Colonies+teneb$Treatment+teneb$Incubator)
summary(tenebs.aov.3w)

#meilleur test
install.packages("AICcmodavg")
library(AICcmodavg)

tenebs.set<-list(tenebs.aov, tenebs.aov.2f, tenebs.aov.2w, tenebs.aov.3w)
tenebs.names<-c("tenebs.aov","tenebs.aov.2f", "tenebs.aov.2w", "tenebs.aov.3w")
aictab(tenebs.set, modnames=tenebs.names)

#Le modèle tenebs.aov.2w est le meilleur

par(mfrow=c(2,2))
plot(tenebs.aov.2w)
par(mfrow=c(1,1))

shapiro.test(resid(tenebs.aov.2w))#Respecté
teneb$grp<-interaction(teneb$Colonies, teneb$Treatment,sep=".")
bartlett.test(teneb$Maxbiomass~teneb$grp)#respecté

grp<-teneb$grp

teneb_modfin<-aov(teneb$Maxbiomass~grp)
summary(teneb_modfin)


modfin_tuk<-TukeyHSD(teneb_modfin)
modfin_tuk
plot(modfin_tuk)

install.packages("multcomp")
library(multcomp)
tntuk<-glht(teneb_modfin,linfct=mcp(grp="Tukey"))
tentuk.cld<-cld(tntuk)
tentuk.cld

letters_teneb<-tentuk.cld$mcletters$Letters
letters_dften<-data.frame(grp=levels(teneb$grp),letters=letters_teneb)

boxplot(teneb$Maxbiomass~teneb$grp,
        ylab = "temps requis pour l'atteinte de 4.5g pour 50 larves (j)", 
        xlab = "Traitements de nourriture humide",
        col =c("lightcoral","lightblue","lightgreen","lightyellow","lightpink"))
text(x= seq(1:15), y= rep(6.85,15), labels= letters_dften$letters)




#####
#test canada: (Maxbiomass)
#####
tenebcan$Treatment <- as.factor(tenebcan$Treatment)
grp<-tenebcan$Treatment
can_aov<-aov(tenebcan$Maxbiomass~grp)
shapiro.test(resid(can_aov))
bartlett.test(tenebcan$Maxbiomass~grp)

summary(can_aov)

tuk_can<-TukeyHSD(can_aov)
plot(TukeyHSD(can_aov), las=1 )
tuk_can

install.packages("multcomp")
library(multcomp)


ghtuk_can <- glht(can_aov,linfct=mcp(grp="Tukey"))
tuk_can_cld <- cld(ghtuk_can)

letters_tenebcan<-tuk_can_cld$mcletters$Letters
letters_dftencan<-data.frame(grp=levels(tenebcan$Treatment),letters=letters_tenebcan)

boxplot(tenebcan$Maxbiomass~tenebcan$Treatment,
        ylab = "Biomasse maximale atteinte par 50 larves (g)", 
        xlab = "Traitements de nourriture humide",
        col =c("lightcoral","lightblue","lightgreen","lightyellow","lightpink","darkblue","darkred","darkgreen"))
text(x= seq(1:15), y= rep(6.55,15), labels= letters_dftencan$letters)

#####
#test grèce: (Maxbiomass)
#####
grep<-tenebgre$Treatment
gre_aov<-aov(tenebgre$Maxbiomass~grep)
shapiro.test(resid(gre_aov))
bartlett.test(tenebgre$Maxbiomass~grep)

summary(gre_aov)

tuk_gre<-TukeyHSD(gre_aov)
plot(TukeyHSD(gre_aov), las=1 )
tuk_gre

#mar(par(4,10,5,5))

install.packages("multcomp")
library(multcomp)

ghtuk_gre <- glht(gre_aov,linfct=mcp(grep="Tukey"))
tuk_gre_cld <- cld(ghtuk_gre)

letters_tenebgre<-tuk_gre_cld$mcletters$Letters
letters_dftengre<-data.frame(grep=levels(tenebgre$Treatment),letters=letters_tenebgre)

boxplot(tenebgre$Maxbiomass~tenebgre$Treatment,
        ylab = "Biomasse maximale atteinte par 50 larves (g)", 
        xlab = "Traitements de nourriture humide",
        col =c("lightcoral","lightblue","lightgreen","lightyellow","lightpink"))
text(x= seq(1:15), y= rep(6.85,15), labels= letters_dftengre$letters)
#####
#test italie: (Maxbiomass)
#####
grip<-tenebit$Treatment
it_aov<-aov(tenebit$Maxbiomass~grip)
shapiro.test(resid(it_aov))
bartlett.test(tenebit$Maxbiomass~grip)

summary(it_aov)

tuk_it<-TukeyHSD(it_aov)
plot(TukeyHSD(it_aov), las=1 )
tuk_it
#mar(par(4,10,5,5))

install.packages("multcomp")
library(multcomp)

ghtuk_it <- glht(it_aov,linfct=mcp(grip="Tukey"))
tuk_it_cld <- cld(ghtuk_it)

letters_tenebit<-tuk_it_cld$mcletters$Letters
letters_dftenit<-data.frame(grip=levels(tenebit$Treatment),letters=letters_tenebit)

boxplot(tenebit$Maxbiomass~tenebit$Treatment,
        ylab = "Biomasse maximale atteinte par 50 larves (g)", 
        xlab = "Traitements de nourriture humide",
        col =c("lightcoral","lightblue","lightgreen","lightyellow","lightpink"))
text(x= seq(1:15), y= rep(6.39,15), labels= letters_dftenit$letters)



#####
#test eau: (Maxbiomass)
#####
grop<-tenebeau$Colonies
eau_aov<-aov(tenebeau$Maxbiomass~grop)
shapiro.test(resid(eau_aov))
bartlett.test(tenebeau$Maxbiomass~grop)

summary(eau_aov)

tuk_eau<-TukeyHSD(eau_aov)
plot(TukeyHSD(eau_aov), las=1 )

#mar(par(4,10,5,5))

install.packages("multcomp")
library(multcomp)

ghtuk_eau <- glht(eau_aov,linfct=mcp(grop="Tukey"))
tuk_eau_cld <- cld(ghtuk_eau)

letters_tenebeau<-tuk_eau_cld$mcletters$Letters
letters_dfteneau<-data.frame(grop=levels(tenebeau$Colonies),letters=letters_tenebeau)

boxplot(tenebeau$Maxbiomass~tenebeau$Colonies,
        ylab = "Biomasse maximale atteinte par 50 larves (g)", 
        xlab = bquote("Souches de " * italic("T. molitor")),
        col =c("lightcoral","lightblue","lightgreen"))
text(x= seq(1:15), y= rep(5.78,15), labels= letters_dfteneau$letters)

#####
#test patate: (Maxbiomass) (non signif)
#####
grat<-tenebpat$Colonies
pat_aov<-aov(tenebpat$Maxbiomass~grat)
shapiro.test(resid(pat_aov))
bartlett.test(tenebpat$Maxbiomass~grat)

summary(pat_aov)

tuk_pat<-TukeyHSD(pat_aov)
plot(TukeyHSD(pat_aov), las=1 )

#mar(par(4,10,5,5))

install.packages("multcomp")
library(multcomp)

ghtuk_pat <- glht(pat_aov,linfct=mcp(grat="Tukey"))
tuk_pat_cld <- cld(ghtuk_pat)

letters_tenebpat<-tuk_pat_cld$mcletters$Letters
letters_dftenpat<-data.frame(grat=levels(tenebpat$Colonies),letters=letters_tenebpat)

boxplot(tenebpat$Maxbiomass~tenebpat$Colonies,
        ylab = "Biomasse maximale atteinte par 50 larves (g)", 
        xlab = bquote("Souches de " * italic("T. molitor")),
        col =c("lightcoral","lightblue","lightgreen"))

text(x= seq(1:15), y= rep(6.39,15), labels= letters_dftenpat$letters)
#####
#test blé: (Maxbiomass) (non signif)
#####
gran<-tenebbran$Colonies
bran_aov<-aov(tenebbran$Maxbiomass~gran)
shapiro.test(resid(bran_aov))
bartlett.test(tenebbran$Maxbiomass~gran)

summary(bran_aov)

tuk_bran<-TukeyHSD(bran_aov)
plot(TukeyHSD(bran_aov), las=1 )

#mar(par(4,10,5,5))

install.packages("multcomp")
library(multcomp)

ghtuk_bran <- glht(bran_aov,linfct=mcp(gran="Tukey"))
tuk_bran_cld <- cld(ghtuk_bran)

letters_tenebbran<-tuk_bran_cld$mcletters$Letters
letters_dftenbran<-data.frame(gran=levels(tenebbran$Colonies),letters=letters_tenebbran)

boxplot(tenebbran$Maxbiomass~tenebbran$Colonies,
        ylab = "Biomasse maximale atteinte par 50 larves (g)", 
        xlab = bquote("Souches de " * italic("T. molitor")),
        col =c("lightcoral","lightblue","lightgreen"))
text(x= seq(1:15), y= rep(3.43,15), labels= letters_dftenbran$letters)


#####
#test agar: (Maxbiomass)
#####
grag<-tenebag$Colonies
ag_aov<-aov(tenebag$Maxbiomass~grag)
shapiro.test(resid(ag_aov))
bartlett.test(tenebag$Maxbiomass~grag)

summary(ag_aov)

tuk_ag<-TukeyHSD(ag_aov)
plot(TukeyHSD(ag_aov), las=1 )
tuk_ag

#mar(par(4,10,5,5))

install.packages("multcomp")
library(multcomp)

ghtuk_ag <- glht(ag_aov,linfct=mcp(grag="Tukey"))
tuk_ag_cld <- cld(ghtuk_ag)

letters_tenebag<-tuk_ag_cld$mcletters$Letters
letters_dftenag<-data.frame(grag=levels(tenebag$Colonies),letters=letters_tenebag)

boxplot(tenebag$Maxbiomass~tenebag$Colonies,
        ylab = "Biomasse maximale atteinte par 50 larves (g)", 
        xlab = bquote("Souches de " * italic("T. molitor")),
        col =c("lightcoral","lightblue","lightgreen"))
text(x= seq(1:15), y= rep(6.69,15), labels= letters_dftenag$letters)
#####
#test carotte: (Maxbiomass)
#####
grcar<-tenebcar$Colonies
car_aov<-aov(tenebcar$Maxbiomass~grcar)
shapiro.test(resid(car_aov))
bartlett.test(tenebcar$Maxbiomass~grcar)

summary(car_aov)

tuk_car<-TukeyHSD(car_aov)
plot(TukeyHSD(car_aov), las=1 )

#mar(par(4,10,5,5))


library(multcomp)

ghtuk_car <- glht(car_aov,linfct=mcp(grcar="Tukey"))
tuk_car_cld <- cld(ghtuk_car)

letters_tenebcar<-tuk_car_cld$mcletters$Letters
letters_dftencar<-data.frame(grcar=levels(tenebcar$Colonies),letters=letters_tenebcar)

boxplot(tenebcar$Maxbiomass~tenebcar$Colonies,
        ylab = "Biomasse maximale atteinte par 50 larves (g)", 
        xlab = bquote("Souches de " * italic("T. molitor")),
        col =c("lightcoral","lightblue","lightgreen"))
text(x= seq(1:15), y= rep(6.8,15), labels= letters_dftencar$letters)
#####
#Test 4.5 g: can
#####
grp4.5<-tenebcan_5$Treatment
can_aov4.5<-aov(tenebcan_5$Daysto4.5~grp4.5)
shapiro.test(resid(can_aov4.5))
bartlett.test(tenebcan_5$Daysto4.5~grp4.5)

summary(can_aov4.5)

tuk_can4.5<-TukeyHSD(can_aov4.5)
plot(TukeyHSD(can_aov4.5), las=1 )
tuk_can4.5

install.packages("multcomp")
library(multcomp)


ghtuk_can4.5 <- glht(can_aov4.5,linfct=mcp(grp4.5="Tukey"))
tuk_can_cld4.5 <- cld(ghtuk_can4.5)

letters_tenebcan4.5<-tuk_can_cld4.5$mcletters$Letters
letters_dftencan4.5<-data.frame(grp4.5=levels(tenebcan_5$Treatment),letters=letters_tenebcan4.5)
letters_dftencan4.5

boxplot(tenebcan_5$Daysto4.5~tenebcan_5$Treatment,
        ylab = "temps requis pour l'atteinte de 4.5g pour 50 larves", 
        xlab = "Traitements de nourriture humide",
        col =c("lightcoral","lightblue","lightgreen","lightyellow","lightpink", "blue","red"))
text(x= seq(1:15), y= rep(34.2,15), labels= letters_dftencan4.5$letters)
#####
#Test 4.5 g: gre
#####
grep4.5<-tenebgre_5$Treatment
gre_aov4.5<-aov(tenebgre_5$Daysto4.5~grep4.5)
shapiro.test(resid(gre_aov4.5))
bartlett.test(tenebgre_5$Daysto4.5~grep4.5)

tenebgre_5

summary(gre_aov4.5)

tuk_gre4.5<-TukeyHSD(gre_aov4.5)
plot(TukeyHSD(gre_aov4.5), las=1 )


install.packages("multcomp")
library(multcomp)


ghtuk_gre4.5 <- glht(gre_aov4.5,linfct=mcp(grep4.5="Tukey"))
tuk_gre_cld4.5 <- cld(ghtuk_gre4.5)

letters_tenebgre4.5<-tuk_gre_cld4.5$mcletters$Letters
letters_dftengre4.5<-data.frame(grep4.5=levels(tenebgre_5$Treatment),letters=letters_tenebgre4.5)
letters_dftengre4.5

boxplot(tenebgre_5$Daysto4.5~tenebgre_5$Treatment,
        ylab = "temps requis pour l'atteinte de 4.5g pour 50 larves (j)", 
        xlab = "Traitements de nourriture humide",
        col =c("lightcoral","lightblue","lightgreen","lightyellow","lightpink"))
text(x= seq(1:15), y= rep(29.15,15), labels= letters_dftengre4.5$letters)
#####
#Test 4.5 g: it (non signif)
#####
grip4.5<-tenebit_5$Treatment
it_aov4.5<-aov(tenebit_5$Daysto4.5~grip4.5)
shapiro.test(resid(it_aov4.5))
bartlett.test(tenebit_5$Daysto4.5~grip4.5)

tenebit_5

summary(it_aov4.5)

tuk_it4.5<-TukeyHSD(it_aov4.5)
plot(TukeyHSD(it_aov4.5), las=1 )


install.packages("multcomp")
library(multcomp)


ghtuk_it4.5 <- glht(it_aov4.5,linfct=mcp(grip4.5="Tukey"))
tuk_it_cld4.5 <- cld(ghtuk_it4.5)

letters_tenebit4.5<-tuk_it_cld4.5$mcletters$Letters
letters_dftenit4.5<-data.frame(grip4.5=levels(tenebit_5$Treatment),letters=letters_tenebit4.5)
letters_dftenit4.5

boxplot(tenebit_5$Daysto4.5~tenebit_5$Treatment,
        ylab = "temps requis pour l'atteinte de 4.5g pour 50 larves (j)", 
        xlab = "Traitements de nourriture humide",
        col =c("lightcoral","lightblue","lightgreen","lightyellow","lightpink"))
text(x= seq(1:15), y= rep(32,15), labels= letters_dftenit4.5$letters)
#####
#Test 4.5 g: eau
#####
gro4.5<-tenebeau$Colonies
eau_aov4.5<-aov(tenebeau$Daysto4.5~gro4.5)
shapiro.test(resid(eau_aov4.5))
bartlett.test(tenebeau$Daysto4.5~gro4.5)

tenebeau

summary(eau_aov4.5)

tuk_eau4.5<-TukeyHSD(eau_aov4.5)
plot(TukeyHSD(eau_aov4.5), las=1 )


install.packages("multcomp")
library(multcomp)


ghtuk_eau4.5 <- glht(eau_aov4.5,linfct=mcp(gro4.5="Tukey"))
tuk_eau_cld4.5 <- cld(ghtuk_eau4.5)

letters_tenebeau4.5<-tuk_eau_cld4.5$mcletters$Letters
letters_dfteneau4.5<-data.frame(gro4.5=levels(tenebeau$Colonies),letters=letters_tenebeau4.5)
letters_dfteneau4.5

boxplot(tenebeau$Daysto4.5~tenebeau$Colonies,
        ylab = "Temps requis pour l'atteinte de 4.5 g (j)", 
        xlab = bquote("Souches de " * italic("T. molitor")),
        col =c("lightcoral","lightblue","lightgreen"))
text(x= seq(1:15), y= rep(32,15), labels= letters_dfteneau4.5$letters)

#####
#Test 4.5 g: pat
#####
grat4.5<-tenebpat$Colonies
pat_aov4.5<-aov(tenebpat$Daysto4.5~grat4.5)
shapiro.test(resid(pat_aov4.5))
bartlett.test(tenebpat$Daysto4.5~grat4.5)

tenebpat

summary(pat_aov4.5)

tuk_pat4.5<-TukeyHSD(pat_aov4.5)
plot(TukeyHSD(pat_aov4.5), las=1 )


install.packages("multcomp")
library(multcomp)


ghtuk_pat4.5 <- glht(pat_aov4.5,linfct=mcp(grat4.5="Tukey"))
tuk_pat_cld4.5 <- cld(ghtuk_pat4.5)

letters_tenebpat4.5<-tuk_pat_cld4.5$mcletters$Letters
letters_dftenpat4.5<-data.frame(grat4.5=levels(tenebpat$Colonies),letters=letters_tenebpat4.5)
letters_dftenpat4.5

boxplot(tenebpat$Daysto4.5~tenebpat$Colonies,
        ylab = "Temps requis pour l'atteinte de 4.5 g (j)", 
        xlab = bquote("Souches de " * italic("T. molitor")),
        col =c("lightcoral","lightblue","lightgreen"))


text(x= seq(1:15), y= rep(34.2,15), labels= letters_dftenpat4.5$letters)

#####
#Test 4.5 g: ag
#####
grag4.5<-tenebag$Colonies
ag_aov4.5<-aov(tenebag$Daysto4.5~grag4.5)
shapiro.test(resid(ag_aov4.5))
bartlett.test(tenebag$Daysto4.5~grag4.5)

tenebag

summary(ag_aov4.5)

tuk_ag4.5<-TukeyHSD(ag_aov4.5)
plot(TukeyHSD(ag_aov4.5), las=1 )


install.packages("multcomp")
library(multcomp)


ghtuk_ag4.5 <- glht(ag_aov4.5,linfct=mcp(grag4.5="Tukey"))
tuk_ag_cld4.5 <- cld(ghtuk_ag4.5)

letters_tenebag4.5<-tuk_ag_cld4.5$mcletters$Letters
letters_dftenag4.5<-data.frame(grag4.5=levels(tenebag$Colonies),letters=letters_tenebag4.5)
letters_dftenag4.5

k <- boxplot(tenebag$Daysto4.5~tenebag$Colonies,
        ylab = "Temps requis pour l'atteinte de 4.5 g (j)", 
        xlab = bquote("Souches de " * italic("T. molitor")),
        col =c("lightcoral","lightblue","lightgreen"))
        
text(x= seq(1:15), y= rep(32.2,15), labels= letters_dftenag4.5$letters)
#####
#Test 4.5 g: car
#####
gracar4.5<-tenebcar$Colonies
car_aov4.5<-aov(tenebcar$Daysto4.5~gracar4.5)
shapiro.test(resid(car_aov4.5))
bartlett.test(tenebcar$Daysto4.5~gracar4.5)

tenebcar

summary(car_aov4.5)

tuk_car4.5<-TukeyHSD(car_aov4.5)
plot(TukeyHSD(car_aov4.5), las=1 )


install.packages("multcomp")
library(multcomp)


ghtuk_car4.5 <- glht(car_aov4.5,linfct=mcp(gracar4.5="Tukey"))
tuk_car_cld4.5 <- cld(ghtuk_car4.5)

letters_tenebcar4.5<-tuk_car_cld4.5$mcletters$Letters
letters_dftencar4.5<-data.frame(gracar4.5=levels(tenebcar$Colonies),letters=letters_tenebcar4.5)
letters_dftencar4.5

par(mar = c(5, 4, 4, 2) + 0.1)

L  <- ggplot(tenebcar$Daysto4.5 ~ tenebcar$Colonies, 
        ylab = "Temps requis pour l'atteinte de 4.5 g (j)", 
        xlab = bquote("Souches de " * italic("T. molitor")),
        col =c("lightcoral","lightblue","lightgreen"))
library(cowplot)

plot_grid(L,k)

text(x= seq(1:15), y= rep(32.2,15), labels= letters_dftencar4.5$letters)

##### Special boxplot colonies #####
tenebcan
# Assuming your data frame is named 'df'
library(dplyr)


tenebcan
# Assuming your original data frames are already loaded
# Merge tenebcan with letters_dftencan based on the common Treatment column
tenebcan <- merge(tenebcan, letters_dftencan, by.x = "Treatment", by.y = "grp",)
tenebcan <- tenebcan %>%
  mutate(Treatment = case_when(
    Treatment == "Wheatbran" ~ "Bran",
    Treatment == "Water" ~ "Wa",
    Treatment == "Agar" ~ "Ag",
    Treatment == "Agar/water" ~ "Ag/Wa",
    Treatment == "Potato" ~ "Pot",
    Treatment == "Potato/water" ~ "Pot/Wa",
    Treatment == "Carrot" ~ "Car",
    Treatment == "Carrot/water" ~ "Car/Wa",
    TRUE ~ Treatment
  ))
# Merge tenebgre with letters_dftengre based on the common Treatment column
tenebgre <- merge(tenebgre, letters_dftengre, by.x = "Treatment", by.y = "grep",)

# Merge tenebit with letters_dftenit based on the common Treatment column
tenebit <- merge(tenebit, letters_dftenit, by.x = "Treatment", by.y = "grip",)


tenebcan

tenebgre

tenebit

letters_dftencan
letters_dftengre
letters_dftenit

library(ggplot2)

# Combine the three data frames into one
combined_df <- rbind(
  transform(tenebcan, Strain = "Canada"),
  transform(tenebgre, Strain = "Grèce"),
  transform(tenebit, Strain = "Italie")
)
combined_df

install.packages("ggsignif")
library(ggsignif)

library(ggplot2)
install.packages("fct_inorder")
library(forcats)
# Create a ggplot

combined_df$Treatment <- factor(combined_df$Treatment, levels = c("Ag", "Ag/Wa", "Car", "Car/Wa", "Pot", "Pot/Wa", "Wa","Bran"))

Maxbio1 <- ggplot(combined_df, aes(x = Strain, y = Maxbiomass, group = interaction(Treatment, Strain), fill = Treatment)) +
  geom_boxplot() +
  labs(
    y = "Biomasse maximale atteinte par 50 larves (g)",
    x = bquote(Souches~de~italic(T.~molitor))
  ) +
  geom_vline(xintercept = c(1.5, 2.5), color = "black") +  # Add vertical lines
  stat_summary(
    fun.y = max,
    geom = "text",
    aes(label = letters),
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    size = 4
  ) +
  theme_bw()
Maxbio1 + 
  scale_fill_manual(values =c("gold", "wheat3", "plum1", "orchid3", "turquoise", "lightblue1", "grey", "#F1F1F1"),
                    name  ="Traitements",
                    breaks=c("Ag", "Ag/Wa", "Car", "Car/Wa", "Pot", "Pot/Wa", "Wa","Bran"),
                    labels=c("Agar", "Agar/eau", "Carotte", "Carotte/eau", "Patate", "Patate/eau", "Eau", "Contrôle"))





                 

##### Special boxplot colonies dayto4.5 #####
tenebcan_5
# Assuming your original data frames are already loaded
# Merge tenebcan_5 with letters_dftencan4.5 based on the common Treatment column
tenebcan_5 <- merge(tenebcan_5, letters_dftencan4.5, by.x = "Treatment", by.y = "grp4.5",)
tenebcan_5 <- tenebcan_5 %>%
  mutate(Treatment = case_when(
    Treatment == "Wheatbran" ~ "Bran",
    Treatment == "Water" ~ "Wa",
    Treatment == "Agar" ~ "Ag",
    Treatment == "Agar/water" ~ "Ag/Wa",
    Treatment == "Potato" ~ "Pot",
    Treatment == "Potato/water" ~ "Pot/Wa",
    Treatment == "Carrot" ~ "Car",
    Treatment == "Carrot/water" ~ "Car/Wa",
    TRUE ~ Treatment
  ))
tenebcan_5

# Merge tenebgre_5 with letters_dftengre4.5 based on the common Treatment column
tenebgre_5 <- merge(tenebgre_5, letters_dftengre4.5, by.x = "Treatment", by.y = "grep4.5",)
tenebgre_5

# Merge tenebit_5 with letters_dftenit4.5 based on the common Treatment column
tenebit_5 <- merge(tenebit_5, letters_dftenit4.5, by.x = "Treatment", by.y = "grip4.5",)
tenebit_5

tenebcan_5

tenebgre_5

tenebit_5

letters_dftencan4.5
letters_dftengre4.5
letters_dftenit4.5

library(ggplot2)

# Combine the three data frames into one
combined_df4.5 <- rbind(
  transform(tenebcan_5, Strain = "Canada"),
  transform(tenebgre_5, Strain = "Grèce"),
  transform(tenebit_5, Strain = "Italie")
)
combined_df4.5

install.packages("ggsignif")
library(ggsignif)

library(ggplot2)



# Create a ggplot
temps1 <- ggplot(combined_df4.5, aes(x = Strain, y = Daysto4.5, group = interaction(Treatment, Strain), fill = Treatment)) +
  geom_boxplot() +
  labs(
    y = "Jour estimé à l'atteinte de 4,5g par réplica",
    x = bquote(Souches~de~italic(T.~molitor))
  ) + 
  geom_vline(xintercept = c(1.5, 2.5), color = "black") +  # Add vertical lines
  stat_summary(
    fun = max,
    geom = "text",
    aes(label = letters),
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    size = 4
  ) +
  theme_bw()
temps1 +   scale_fill_manual(values =c("gold", "wheat3", "plum1", "orchid3", "turquoise", "lightblue1", "grey"),
                             name  ="Traitements",
                             breaks=c("Ag", "Ag/Wa", "Car", "Car/Wa", "Pot", "Pot/Wa", "Wa"),
                             labels=c("Agar", "Agar/eau", "Carotte", "Carotte/eau", "Patate", "Patate/eau", "Eau"))



####fun.ymax = ##### Speciamax.col()##### Speciamax_height()##### Special boxplot traitements maxbio #####
tenebeau <- merge(tenebeau, letters_dfteneau, by.x = "Colonies", by.y = "grop",)

# Merge tenebgre with letters_dftengre based on the common Treatment column
tenebag <- merge(tenebag, letters_dftenag, by.x = "Colonies", by.y = "grag",)

# Merge tenebit with letters_dftenit based on the common Treatment column
tenebpat <- merge(tenebpat, letters_dftenpat, by.x = "Colonies", by.y = "grat",)

tenebcar <- merge(tenebcar, letters_dftencar, by.x = "Colonies", by.y = "grcar",)

tenebbran <- merge(tenebbran, letters_dftenbran, by.x = "Colonies", by.y = "gran",)

tenebeau

tenebag

tenebpat

tenebcar

tenebbran

letters_dftenag
letters_dfteneau
letters_dftenpat
letters_dftencar
letters_dftenbran

library(ggplot2)

# Combine the three data frames into one
combined_df_col <- rbind(
  transform(tenebbran, Traitement = "Contrôle"),
  transform(tenebeau, Traitement = "Eau"),
  transform(tenebag, Traitement = "Agar"),
  transform(tenebcar, Traitement = "Carotte"),
  transform(tenebpat, Traitement = "Patate")
)
combined_df_col

install.packages("ggsignif")
library(ggsignif)

library(ggplot2)

# Create a ggplot
Maxbio2 <- ggplot(combined_df_col, aes(x = Traitement, y = Maxbiomass, group = interaction(Treatment, Colonies), fill = Colonies)) +
  geom_boxplot() +
  labs(
    y = "Biomasse maximale atteinte par 50 larves (g)",
    x = "Traitements"
  ) +
  geom_vline(xintercept = c(1.5, 2.5,3.5,4.5), color = "black") +  # Add vertical lines
  stat_summary(
    fun.y = max,
    geom = "text",
    aes(label = letters),
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    size = 4
  ) +
  theme_bw()
Maxbio2 + scale_fill_manual(values =c("gold", "plum1","turquoise"),
                            name  ="Souches",
                            breaks=c("Can", "Gre","It"),
                            labels=c("Canada", "Grèce", "Italie"))

##### Special boxplot traitements 4.5 #####
tenebeau4.5 <- merge(tenebeau4.5, letters_dfteneau4.5, by.x = "Colonies", by.y = "gro4.5",)

# Merge tenebgre with letters_dftengre based on the common Treatment column
tenebag4.5 <- merge(tenebag4.5, letters_dftenag4.5, by.x = "Colonies", by.y = "grag4.5",)

# Merge tenebit with letters_dftenit based on the common Treatment column
tenebpat4.5 <- merge(tenebpat4.5, letters_dftenpat4.5, by.x = "Colonies", by.y = "grat4.5",)

tenebcar4.5 <- merge(tenebcar4.5, letters_dftencar4.5, by.x = "Colonies", by.y = "gracar4.5",)



tenebeau4.5

tenebag4.5

tenebpat4.5

tenebcar4.5


letters_dftenag
letters_dfteneau
letters_dftenpat
letters_dftencar
letters_dftenbran

library(ggplot2)

# Combine the three data frames into one
combined_df_col4.5 <- rbind(
  transform(tenebeau4.5, Traitement = "Eau"),
  transform(tenebag4.5, Traitement = "Agar"),
  transform(tenebcar4.5, Traitement = "Carotte"),
  transform(tenebpat4.5, Traitement = "Patate")
)

combined_df_col4.5

install.packages("ggsignif")
library(ggsignif)

library(ggplot2)

# Create a ggplot
 temps2 <- ggplot(combined_df_col4.5, aes(x = Traitement, y = Daysto4.5, group = interaction(Traitement, Colonies), fill = Colonies)) +
  geom_boxplot() +
  labs(
    y = "Jour estimé à l'atteinte de 4.5g par réplica",
    x = "Traitements"
  ) +
  geom_vline(xintercept = c(1.5, 2.5,3.5), color = "black") +  # Add vertical lines
  stat_summary(
    fun.y = max,
    geom = "text",
    aes(label = letters),
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    size = 4
  ) +
  theme_bw()
Temps2 + scale_fill_manual(values = c("gold", "plum1", "turquoise"),
                           name = "Souches", 
                           breaks = c("Can", "Gre", "It"),
                           labels = c("Canada", "Grèce", "Italie"))

#Fonctionne

temps2 <- ggplot(combined_df_col4.5, aes(x = Traitement, y = Daysto4.5, group = interaction(Traitement, Colonies), fill = Colonies)) +
  geom_boxplot() +
  labs(
    y = "Jour estimé à l'atteinte de 4.5g par 50 larves",
    x = "Traitements"
  ) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), color = "black") +  # Add vertical lines
  stat_summary(
    fun.y = max,
    geom = "text",
    aes(label = letters),  # Using 'letters' for labeling
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    size = 4
  ) +
  theme_bw()

temps2 + scale_fill_manual(values = c("gold", "plum1", "turquoise"),
                           name = "Souches",
                           breaks = c("Can", "Gre", "It"),
                           labels = c("Canada", "Grèce", "Italie"))

length(combined_df_col4.5$letters)
length(combined_df_col4.5$Colonies)
##### Calcul





