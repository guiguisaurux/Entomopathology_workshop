# Expérience 1 ####
# Expérience 2 ####
# Expérience 3 ####
# Expérience 4 ####
# Expérience 5 ####
# Expérience 6 ####
exp6 <- read.table("exp6.txt", header = T)
exp6

#Importation package
install.packages(c("dplyr", "ggplot2", "janitor", "magrittr", "scales", "tidyr"))
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("survival", "survminer"))

install.packages("devtools")
library(devtools)
devtools::install_github("qhuitan/ggbulksurv")
library(ggbulksurv)




# Expérience 7 ####