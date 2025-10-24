strain_colors <- c(
  "Canada 1"   = "#AA0022",
  "Canada 2"   = "#880000", 
  "France"     = "#7796FF",  
  "Germany"    = "#EECC00",  
  "Greece"     = "#68CCEE", 
  "Italy 1"    = "#C09999",  
  "Italy 2"    = "#A0EE99",  
  "Norway 1"   = "#295251",  
  "Norway 2"   = "#258255",  
  "Spain"      = "#401010",  
  "Turkey"     = "#880099"
)


wound_colors <- c(
  "Septic" = "#012456",
  "Sterile" = "#096",
  "Unwounded" = "gold"
)

treatment_colors <- c(
  "Bactocell" = "#013",
  "Levucell" = "#116735",
  "Wheatbran" = "#786512",
  "Chickpea" = "yellow"
  
)
  
Size_Square <- function(x) {
    x %>% 
      adjust_font(fontsize = 24, family = "serif") %>% 
      adjust_size(height = 200, width = 200)
}

Size_Square.1 <- function(x) {
  x %>% 
    adjust_font(fontsize = 24, family = "serif") %>% 
    adjust_size(height = 200, width = 225)
}
                  
Size_Wide <- function(x) {
  x %>% 
    adjust_font(fontsize = 24, family = "serif") %>% 
    adjust_size(height = 200, width = 500)
    
}


Color_Wound <- function(x) {
  x %>% 
    adjust_colors(wound_colors)
}

Color_Strain <- function(x) {
  x %>% 
    adjust_colors(strain_colors)
}


Color_Treatment <- function(x) {
  x %>% 
    adjust_colors(treatment_colors)
}

Limits_1 <- function(x) {
  x %>% 
    adjust_x_axis(padding = c(0,0), limits = c(0,15), breaks = c(0,3,6,9,12,15)) %>% 
    adjust_y_axis(padding = c(0,0), limits = c(0, 480)) +
    ggplot2::theme(legend.key.size = unit(1.6, "cm"))
}
Limits_1.1 <- function(x) {
  x %>% 
    adjust_x_axis(padding = c(0,0), limits = c(0,18), breaks = c(0,3,6,9,12,15)) %>% 
    adjust_y_axis(padding = c(0,0), limits = c(0, 480)) +
    ggplot2::theme(legend.key.size = unit(1.6, "cm"))
}

Limits_2 <- function(x) {
  x %>% 
    adjust_x_axis(padding = c(0,0), limits = c(0,10.2)) %>% 
    adjust_y_axis(padding = c(0,0), limits = c(0, 1500)) +
    ggplot2::theme(legend.key.size = unit(1.6, "cm"))
}

Limits_3 <- function(x) {
  x %>% 
    adjust_x_axis(padding = c(0,0), limits = c(0,18.2)) %>% 
    adjust_y_axis(padding = c(0,0), limits = c(0, 1800)) +
    ggplot2::theme(legend.key.size = unit(1.6, "cm"))
}
  

My_Style <- function(plot, Size = NULL, Color = NULL, Limits = NULL) {
  
  # Function mapping
  size_funcs <- list(
    "Square" = Size_Square,
    "Square.1" = Size_Square.1,
    "Wide"   = Size_Wide
  )
  
  color_funcs <- list(
    "Strain"   = Color_Strain,
    "Wound"    = Color_Wound,
    "Treatment"= Color_Treatment
  )
  
  limits_funcs <- list(
    "Limits_1" = Limits_1,
    "Limits_1.1" = Limits_1.1,
    "Limits_2" = Limits_2,
    "Limits_3" = Limits_3
  )
  
  # Apply size
  if (!is.null(Size) && Size %in% names(size_funcs)) {
    plot <- size_funcs[[Size]](plot)
  }
  
  # Apply color
  if (!is.null(Color) && Color %in% names(color_funcs)) {
    plot <- color_funcs[[Color]](plot)
  }
  
  # Apply limits
  if (!is.null(Limits) && Limits %in% names(limits_funcs)) {
    plot <- limits_funcs[[Limits]](plot)
  }
  
  return(plot)
}

