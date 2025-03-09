#Lecture des données
New <- read.table("PostVérif.txt", header = T, check.names = F)
Mort <- read.table("PostMort.txt", header = T, check.names = F)

library(tidyverse)
library(tidyplots)
library(lme4)
library(lmerTest)
library(vegan)

New$Identity <- as.factor(New$Identity) 
New$Bloc <- as.factor(New$Bloc) 
New$Trt <- as.factor(New$Trt)  
New$Wound <- as.factor(New$Wound)
New$Strain <- as.factor(New$Strain) 


Mort$Identity <- as.factor(Mort$Identity) 
Mort$Bloc <- as.factor(Mort$Bloc) 
Mort$Trt <- as.factor(Mort$Trt)  
Mort$Wound <- as.factor(Mort$Wound)
Mort$Strain <- as.factor(Mort$Strain) 
  
Mort_C <- Mort %>% 
  mutate("0" = (Mort$'0' - Mort$'0' )) %>% 
  mutate("2" = (Mort$'0' - Mort$'2' )) %>%
  mutate("4" = (Mort$'0' - Mort$'4' )) %>%
  mutate("7" = (Mort$'0' - Mort$'7' )) %>%
  mutate("9" = (Mort$'0' - Mort$'9' )) %>% 
  mutate("11" = (Mort$'0' - Mort$'11' )) %>%
  mutate("14" = (Mort$'0' - Mort$'14' )) %>%
  mutate("16" = (Mort$'0' - Mort$'16' )) %>%
  mutate("18" = (Mort$'0' - Mort$'18' )) %>% 
  mutate(group = paste(Mort$Strain, Mort$Wound)) %>% 
  pivot_longer(cols = (6:14), names_to = 'Time', values_to = 'Mort_Acc') %>%
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Times = as.factor(Time)) 

ModM <- glmer(Mort_Acc ~ Strain*Trt*Wound + Time + Strain:Time + Trt:Time + Wound:Time + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt) + (1|Bloc:Wound) + (1|Identity), data = Mort_C, family = poisson)
plot(ModM)
qqnorm(residuals(ModM))
qqline(residuals(ModM), col = "red")
hist(Mort_C$Mort_Acc)
car::Anova(ModM, type = 3)

Mort_C %>%
  tidyplot(x = Time, y = Mort_Acc, color = Trt, dodge_width = 0) %>%
  add_data_points(dodge_width = 0.1, alpha = 0.8)  %>%
  remove_x_axis_title() %>%
  adjust_y_axis(limits = c(0,10)) %>%
  remove_y_axis_title() %>%
  split_plot(by = group, widths = 70, heights = 50)

New.M <- New %>% 
  select(c(1:6,8:15)) %>% 
  pivot_longer(cols = c(7:14), names_to = "Day", values_to = "Mass")

New.M$Day <- as.numeric(New.M$Day)

Plot.M <- New.M %>% 
  tidyplot(Mass, Day, color = Strain)


New.N <- New %>% 
  select(c(1:5,7,16:23)) %>% 
  pivot_longer(cols = c(7:14), names_to = "Day", values_to = "Number")

New.F <- New.M %>% 
  merge(New.N, by = c('Identity', 'Bloc', 'Trt', 'Wound', 'Strain', 'Day')) %>% 
  mutate(Mean = Mass/Number) %>% 
  select(c(1:7,9,11)) %>% 
  pivot_wider(names_from = "Day", values_from = "Mean") %>% 
  mutate(Initial_Mean = Initial_Mass/Initial_Number) %>% 
  merge(New.T, by = "Identity") %>% 
  mutate(Removed_Mean = Max_Mean - Initial_Mean) 
  

New.T <- New.M %>% 
  merge(New.N, by = c('Identity', 'Bloc', 'Trt', 'Wound', 'Strain', 'Day')) %>% 
  mutate(Mean = Mass/Number)
  
New.T %>% 
  tidyplot(x = Mean) %>% 
  add_histogram() %>% 
  split_plot(by = Day)



Growth_C <- New.F %>% 
  mutate("2" = (New.F$'2' - New.F$Initial_Mean)/3) %>%
  mutate("4" = (New.F$'4' - New.F$'2')/2) %>%
  mutate("7" = (New.F$'7' - New.F$'4')/3) %>%
  mutate("9" = (New.F$'9' - New.F$'7')/2) %>% 
  mutate("11" = (New.F$'11' - New.F$'9')/2) %>%
  mutate("14" = (New.F$'14' - New.F$'11')/3) %>%
  mutate("16" = (New.F$'16' - New.F$'14')/2) %>%
  mutate("18" = (New.F$'18' - New.F$'16')/2) %>% 
  mutate(group = paste(New.F$Wound, New.F$Trt)) %>% 
  pivot_longer(cols = (8:15), names_to = 'Time', values_to = 'Growth_Rate') %>%
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Times = as.factor(Time))

ModGr <- lmer(Growth_Rate ~ Strain*Trt*Wound + Time + Strain:Time + Trt:Time + Wound:Time + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt) + (1|Bloc:Wound) + (1|Identity), data = Growth_C)
plot(ModGr)
qqnorm(residuals(ModGr))
qqline(residuals(ModGr), col = "red")

anova(ModGr)

Growth_C %>% 
  tidyplot(x = Time, y = Growth_Rate, color = Strain) %>% 
  add_sem_ribbon() %>% 
  adjust_y_axis(limits = c(-50,150)) %>% 
  add_reference_lines(y = 0, linetype = 'solid', color = "gray") %>% 
  split_plot(by = group, heights = 50, widths =100)
  
Growth_M <- New.F %>% 
  mutate("2" = (New.F$'2' - New.F$Initial_Mean)) %>%
  mutate("4" = (New.F$'4' - New.F$Initial_Mean)) %>%
  mutate("7" = (New.F$'7' - New.F$Initial_Mean)) %>%
  mutate("9" = (New.F$'9' - New.F$Initial_Mean)) %>% 
  mutate("11" = (New.F$'11' - New.F$Initial_Mean)) %>%
  mutate("14" = (New.F$'14' - New.F$Initial_Mean)) %>%
  mutate("16" = (New.F$'16' - New.F$Initial_Mean)) %>%
  mutate("18" = (New.F$'18' - New.F$Initial_Mean)) %>% 
  mutate(group = paste(New.F$Wound, New.F$Trt)) %>% 
  pivot_longer(cols = (8:15), names_to = 'Time', values_to = 'Growth_Acc') %>%
  mutate(Time = as.numeric(Time)) %>% 
  mutate(Times = as.factor(Time)) 
  

ModG <- lmer(Growth_Acc ~ Strain*Trt*Wound + Time + Strain:Time + Trt:Time + Wound:Time + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt) + (1|Bloc:Wound) + (1|Identity), data = Growth_M)
plot(ModG)
qqnorm(residuals(ModG))
qqline(residuals(ModG), col = "red")

anova(ModG, type = 3)

Growth_M %>%
  tidyplot(x = Time, y = Growth_Acc, color = Strain, dodge_width = 0) %>%
  add_ci95_ribbon(alpha = 0.22) %>%
  add_mean_line(linewidth = 1, alpha = 0.8)  %>%
  remove_x_axis_title() %>%
  adjust_y_axis(limits = c(0,1000)) %>%
  remove_y_axis_title() %>%
  split_plot(by = group, widths = 70, heights = 50)

Growth_M %>% 
  tidyplot(x = Growth_Acc, color = Wound) %>% 
  add_histogram(alpha = 0.8) %>% 
  split_plot(by = group)


max(New.M$Mass)
min(New.M$Mass)

Plot1 <- New.F %>% 
  mutate(group = paste(New.F$Strain, New.F$Trt)) %>% 
  tidyplot(y = Max_Mean, x = Wound, color = group) %>% 
  adjust_colors(new_colors = c("darkblue", "blue","#123789","darkred","red","#987654","darkgreen", "#018987", "#098765")) %>% 
  add_boxplot() %>% 
  adjust_size(height = 100, width = 200)
  

#Biomasse maximale
Biom <- Mort %>% 
  pivot_longer(cols = (6:14), names_to = 'Time', values_to = 'Survival') %>% 
  merge(Growth_M, by = c('Identity', 'Bloc', 'Trt', 'Wound', 'Strain', 'Time')) %>% 
  mutate(Biom_Acc = Growth_Acc*Survival)

  

Biom %>%
  tidyplot(x = Times, y = Biom_Acc, color = Wound, dodge_width = 0) %>%
  add_sem_ribbon(alpha = 0.6) %>%
  add_mean_line(linewidth = 1, alpha = 0.8)  %>%
  add_boxplot(dodge_width = 0.5, box_width = 0.5) %>% 
  remove_x_axis_title() %>%
  adjust_y_axis(limits = c(0,10000)) %>%
  remove_y_axis_title() %>%
  split_plot(by = group, widths = 70, heights = 50)

ModG <- lmer(Biom_Acc ~ Strain*Trt*Wound + Time + Strain:Time + Trt:Time + Wound:Time + (1|Bloc) + (1|Bloc:Strain) + (1|Bloc:Trt) + (1|Bloc:Wound) + (1|Identity), data = Biom)
plot(ModG)
qqnorm(residuals(ModG))
qqline(residuals(ModG), col = "red")
anova(ModG, type = 3)
         