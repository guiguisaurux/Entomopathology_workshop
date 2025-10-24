library(tidyplots)
library(tidyverse)
library(ggrepel)
library(ggtext)
library(cowplot)
library(forcats)

growth_data_sum_strain <- growth_data %>% 
  group_by(Strain, Time) %>% 
  summarize(Growth = mean(Growth)) %>% 
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
    Strain == "Wor" ~ "Canada 1",
    T ~ Strain))  %>% 
  mutate(Time = case_when(
    Time == "0" ~ "0",
    Time == "1" ~ "2",
    Time == "2" ~ "3",
    Time == "3" ~ "4",
    Time == "4" ~ "5",
    Time == "5" ~ "6",
    Time == "6" ~ "7",
    Time == "7" ~ "8",
    Time == "8" ~ "9",
    Time == "9" ~ "10",
    Time == "10" ~ "11",
    Time == "11" ~ "12",
    Time == "12" ~ "13",
    Time == "13" ~ "14",
    Time == "14" ~ "15"
  )) %>% 
  mutate( Time = as.integer(Time))



CroSou1 <- growth_data_sum_strain %>% 
  tidyplot(x = Time, y = Growth, color = Strain) %>% 
  add_line(linewidth = 1.2, alpha = 0.8) %>% 
  adjust_y_axis_title(title = "Mean Growth (µg/larvae)") %>%
  adjust_x_axis_title(title = "Time (Day)") %>% 
  add_annotation_line(x = 2, xend = 2, y = 430, yend = 445, color = 'black') %>% 
  add_annotation_line(x = 15, xend = 15, y = 430, yend = 445, color = 'black') %>% 
  add_annotation_line(x = 2, xend = 15, y = 445, yend = 445, color = 'black') %>%
  add_annotation_text(x = 9.5, y = 450, "***", fontsize = 24) %>% 
  My_Style(Size = "Square.1", Color = "Strain", Limits = "Limits_1.1") %>% 
  adjust_legend_position(position = "none") 
  
last_points <- growth_data_sum_strain %>%
  group_by(Strain) %>%
  filter(Time == max(Time)) %>%
  ungroup()

CroSou <- CroSou1 + 
  geom_text_repel(
    data = last_points,
    aes(color = Strain, label = Strain),
    family = "serif",
    size = 6,
    direction = "y",
    xlim = c(15.8, NA),
    hjust = 0,
    segment.size = .75,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .25,
    segment.curvature = .05,
    segment.ncp = 0.1,
    segment.angle = 5
  ) +
  annotate(
    "text",
    x = 16.6,
    y = 280,
    label = "Strain",
    family = "serif",
    size = 6,
    fontface = "bold"
  ) +
  theme(
    axis.line.x = element_blank()  # Remove default axis line
  ) +
  annotate(
    "segment",
    x = 0, xend = 15,  # Only draw line up to 15
    y = 0, yend = 0,
    color = "black",
    linewidth = 0.5
  ) + 
  theme(
    axis.line.y = element_blank()
  ) +
  annotate(
    "segment",
    x = 0, xend = 0,
    y = 0, yend = 450,
    color = "black",
    linewidth  = 0.5
  )




tidyplots::save_plot(CroSou,"C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Growth_by_Strain_Exp1.tif")      


growth_data_sum_Wound<- growth_data %>% 
  group_by(Trt, Time) %>% 
  summarize(Growth = mean(Growth)) %>% 
  mutate(Wound = case_when(
    Trt == "C" ~ "Sterile",
    Trt == "B" ~ "Septic"
  ))  %>% 
  mutate(Time = case_when(
    Time == "0" ~ "0",
    Time == "1" ~ "2",
    Time == "2" ~ "3",
    Time == "3" ~ "4",
    Time == "4" ~ "5",
    Time == "5" ~ "6",
    Time == "6" ~ "7",
    Time == "7" ~ "8",
    Time == "8" ~ "9",
    Time == "9" ~ "10",
    Time == "10" ~ "11",
    Time == "11" ~ "12",
    Time == "12" ~ "13",
    Time == "13" ~ "14",
    Time == "14" ~ "15"
  )) %>% 
  mutate( Time = as.integer(Time))


CroWound1 <- growth_data_sum_Wound %>% 
  tidyplot(x = Time, y = Growth, color = Wound) %>% 
  add_line(linewidth = 1.2, alpha = 0.8) %>% 
  adjust_y_axis_title( title = "Mean Growth (µg/larvae)") %>%
  adjust_x_axis_title( title = "Time (Days)") %>% 
  add_annotation_line(x = 3, xend = 3, y = 430, yend = 445, color = 'black') %>% 
  add_annotation_line(x = 15, xend = 15, y = 430, yend = 445, color = 'black') %>% 
  add_annotation_line(x = 3, xend = 15, y = 445, yend = 445, color = 'black') %>%
  add_annotation_text(x = 9.5, y = 450, "***", fontsize = 24) %>% 
  My_Style("Square","Wound","Limits_1")

CroWound <- CroWound1 +
  theme(
    axis.line.x = element_blank()  # Remove default axis line
  ) +
  annotate(
    "segment",
    x = 0, xend = 15,  # Only draw line up to 15
    y = 0, yend = 0,
    color = "black",
    linewidth = 0.5
  ) + 
  theme(
    axis.line.y = element_blank()
  ) +
  annotate(
    "segment",
    x = 0, xend = 0,
    y = 0, yend = 450,
    color = "black",
    linewidth  = 0.5
  ) + 
  ggplot2::theme(legend.key.size = unit(1.2, "cm"))

mean_data_1 <- mean_data %>% 
  mutate(Time = as.numeric(Time)) %>% 
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
    Strain == "Wor" ~ "Canada 1",
    T ~ Strain)) %>% 
  mutate(Time = case_when(
    Time == "0" ~ "0",
    Time == "1" ~ "2",
    Time == "2" ~ "3",
    Time == "3" ~ "4",
    Time == "4" ~ "5",
    Time == "5" ~ "6",
    Time == "6" ~ "7",
    Time == "7" ~ "8",
    Time == "8" ~ "9",
    Time == "9" ~ "10",
    Time == "10" ~ "11",
    Time == "11" ~ "12",
    Time == "12" ~ "13",
    Time == "13" ~ "14",
    Time == "14" ~ "15"
  )) %>% 
  mutate( Time = as.integer(Time))


mean_data_final_1 <- mean_data_1 %>% 
  select(-Mass, -Number) %>% 
  pivot_wider(names_from = "Time", values_from = "Mean")

mean_data_final <- mean_data_final_1 %>% 
  mutate('Day 0' = mean_data_final_1$'0') %>% 
  mutate('Day 15' = mean_data_final_1$'15') %>% 
  select(-'2',-'3',-'4',-'5',-'6',-'7',-'8',-'9',-'10',-'11',-'12',-'13',-'14',-'15', -'0') %>% 
  pivot_longer(cols = c(5,6), names_to = "Time", values_to = "Mean") %>% 
  mutate(Mean = as.numeric(Mean)) %>% 
  mutate(Strain = as.factor(Strain)) %>% 
  mutate(Time = as.factor(Time))


mean_data_final_r <- mean_data_final %>%
  mutate(Strain = fct_reorder2(Strain, Time, Mean))

Stats_mean_final <- mean_data_final %>% 
  group_by(Time, Strain) %>%
  summarize(
    mean = mean(Mean),
    sd = sd(Mean),
  ) %>% 
  mutate(Placement = mean + sd + 20)

Stats_mean_final_1 <- mean_data_final_r %>% 
  group_by(Time, Strain) %>%
  summarize(
    mean = mean(Mean),
    sd = sd(Mean),
  ) %>% 
  mutate(Placement = mean + sd + 20)


Dataplace_1 <- Stats_mean_final %>% 
  ungroup() %>% 
  mutate(label = c('cde','de','a','c','cd','c','cde','e','c','b','bc','c','a','ab','abc','a','abc','c','abc','a')) %>% 
  group_by(Time, Strain)

Dataplace <- Stats_mean_final_1 %>% 
  group_by(Strain, Time, mean, sd, Placement) %>% 
  left_join(Dataplace_1) %>% 
  ungroup()



Mean_final <- mean_data_final_r %>% 
  tidyplot(x = Time, y = Mean, color = Strain) %>% 
  add_mean_bar(alpha = 0.65, dodge_width = 0.9) %>% 
  add_mean_dash(linewidth = 0.8,dodge_width = 0.9) %>% 
  add_sd_errorbar(linewidth = 0.5,dodge_width = 0.9) %>% 
  adjust_y_axis_title(title = "Mean biomass (µg/larvae)") %>%
  adjust_x_axis_title(title = "Time (Day)") %>% 
  My_Style(Size = "Wide", Color = "Strain", Limits = NULL) %>% 
  + ggplot2::theme(legend.key.size = unit(1.2, "cm"))

Mean_Final_1 <- Mean_final +  
  ggplot2::geom_text(
    data = Dataplace, 
    aes(y = Placement, label = label, fontface = "bold"), 
    position = position_dodge(width = 0.9),
    #family = "serif", 
    size = 6, # approx. 24 pts base on ggplot scaling
    show.legend = FALSE
  )



biom_data_1 <- biom_data %>% 
  mutate(Time = as.numeric(Time)) %>% 
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
    Strain == "Wor" ~ "Canada 1",
    T ~ Strain)) %>% 
  mutate(Time = case_when(
    Time == "0" ~ "0",
    Time == "1" ~ "2",
    Time == "2" ~ "3",
    Time == "3" ~ "4",
    Time == "4" ~ "5",
    Time == "5" ~ "6",
    Time == "6" ~ "7",
    Time == "7" ~ "8",
    Time == "8" ~ "9",
    Time == "9" ~ "10",
    Time == "10" ~ "11",
    Time == "11" ~ "12",
    Time == "12" ~ "13",
    Time == "13" ~ "14",
    Time == "14" ~ "15"
  )) %>% 
  mutate( Time = as.integer(Time))

ordered_strain_levels <- levels(mean_data_final_r$Strain)


biom_data_final_1 <- biom_data_1 %>% 
  select(-Mass, -Number, -Survival, -Mean) %>% 
  pivot_wider(names_from = "Time", values_from = "Biomass")

biom_data_final <- biom_data_final_1 %>% 
  select(-'2',-'3',-'4',-'5',-'6',-'7',-'8',-'9',-'10',-'11',-'12',-'13',-'14', -'0') %>% 
  mutate(Biomass = biom_data_final_1$'15') %>% 
  select(-'15') %>% 
  mutate(Biomass = as.numeric(Biomass)) %>% 
  mutate(Strain = as.factor(Strain)) 



Stats_biom_final <- biom_data_final %>% 
  group_by(Strain) %>%
  summarize(
    mean = mean(Biomass),
    sd = sd(Biomass),
  ) %>% 
  mutate(Placement = mean + sd + 250)

biom_data_final <- biom_data_final %>%
  mutate(Strain = factor(Strain, levels = ordered_strain_levels))


Dataplace <- Stats_biom_final %>% 
  ungroup() %>% 
  mutate(label = c('abc','abc','ab','abc','bc','abc','c','c','c','a')) %>% 
  group_by(Strain)

# Apply same order to Dataplace
Dataplace <- Dataplace %>%
  mutate(Strain = factor(Strain, levels = ordered_strain_levels))

biom_final <- biom_data_final %>% 
  tidyplot(x = Strain, y = Biomass, color = Strain) %>%
  add_mean_bar(alpha = 0.655, dodge_width = 0.9) %>% 
  add_mean_dash(linewidth = 0.8,dodge_width = 0.9) %>% 
  add_sd_errorbar(linewidth = 0.5,dodge_width = 0.9) %>% 
  adjust_y_axis_title(title = "Final Biomass (µg)") %>%
  adjust_x_axis(title = "Mealworm Strains", rotate_labels = T) %>%
  remove_x_axis_labels() %>% 
  My_Style(Size = "Wide", Color = "Strain") +
  ggplot2::theme(legend.key.size = unit(1.2, "cm"))
  

biom_Final_1<- biom_final +    
  ggplot2::geom_text(
    data = Dataplace, 
    aes(y = Placement, label = label, fontface = "bold"), 
    position = position_dodge(width = 0.9),
    #family = "serif", 
    size = 6, # approx. 24 pts base on ggplot scaling
    show.legend = FALSE
  )
  

library(cowplot)
Plot_3_A <- plot_grid(
  CroSou + theme(plot.margin = margin(t = 5, b = 5)),  # Premier graphique (CroSou)
  CroWound + theme(plot.margin = margin(t = 5, b = 5)),  # Deuxième graphique (CroWound) 
  labels = c("(A)", "(B)"),  # Adding labels
  label_size = 30,           # Set label font size
  label_fontface = "bold",   # Bold label font
  label_colour = "black",    # Label color
  ncol = 2,
  rel_widths = c(1, 1)# Arrange plots in 2 columns
)

Plot_3_B = plot_grid(
  Mean_Final_1 + theme(plot.margin = margin(t = 20, b = 10, r = 10)),  # Premier graphique (CroSou)
  biom_Final_1 + theme(plot.margin = margin(t = 20, b = 10, r = 10)),  # Deuxième graphique (CroWound) 
  labels = c("(C)", "(D)"),  # Adding labels
  label_size = 30,           # Set label font size
  label_fontface = "bold",   # Bold label font
  label_colour = "black",    # Label color
  ncol = 1,
  rel_heights = c(1,1),
  align = "left"# Arrange plots in 2 columns
)

Plot_3_C <- plot_grid(
  Plot_3_A,
  Plot_3_B,
  ncol = 1,
  rel_heights = c(1,2)
)



tidyplots::save_plot(Plot_3_C, "C:/Users/User/OneDrive/Documents/Entomopathologie_workshop/Figures/Tiff/Mortality_Sterile_Exp1.tif", width = 620, height = 780,
                     limitsize = F)

