BruteG15T <- TenebBrute14 %>%
  pivot_wider(names_from = Trt, 
              values_from = Brute)



BruteG15T = BruteG15T %>%
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

B1 <- BruteG15T %>%
  tidyplot(x = Strain, y = B, color = Strain, dodge_width = 0,width = 140) %>%
  add_boxplot(alpha = 0.6) %>%
  remove_x_axis_title() %>%
  adjust_y_axis(limits = c(0,6000)) %>%
  remove_y_axis_title() %>%
  remove_legend()  %>%
  save_plot("BruteBac.png")

B2<-BruteG15T %>%
  tidyplot(x = Strain, y = C, color = Strain, dodge_width = 0,width = 140) %>%
  add_boxplot(alpha = 0.6) %>%
  remove_x_axis_title() %>%
  adjust_y_axis(limits = c(0,6000)) %>%
  remove_y_axis_title() %>%
  remove_legend() %>%
  add_data_labels(data = letters_df.Mod15C, y = 5800, label = letters, color = 'black') %>%
  save_plot("BruteC.png")

plot_grid(B1,B2)

#E
Mod15B <- lmer(B ~ Strain + (1|Bloc),data = BruteG15T) 
plot(Mod15B)
qqnorm(residuals(Mod15B))
qqline(residuals(Mod15B), col = "red")

anova(Mod15B, type=3)

Mod15B_Tukey <- emmeans(Mod15B, ~Strain)


tuk.cld.Mod15B <- cld(Mod15B_Tukey, Letters = letters, adjust = "none")
tuk.cld.Mod15B

lettersMod15B <- tuk.cld.Mod15B$.group
letters_df.Mod15B <- data.frame(Strain = tuk.cld.Mod15B$Strain, letters = lettersMod15B)
letters_df.Mod15B

#C
Mod15C<- lmer(C ~ Strain + (1|Bloc), data = BruteG15T) 
plot(Mod15C)
qqnorm(residuals(Mod15C))
qqline(residuals(Mod15C), col = "red")

anova(Mod15C, type=3)

Mod15C_Tukey <- emmeans(Mod15C, ~Strain)


tuk.cld.Mod15C<- cld(Mod15C_Tukey, Letters = letters, adjust = "none")
tuk.cld.Mod15C

lettersMod15C<- tuk.cld.Mod15C$.group
letters_df.Mod15C<- data.frame(Strain = tuk.cld.Mod15C$Strain, letters = lettersMod15C)
letters_df.Mod15C
