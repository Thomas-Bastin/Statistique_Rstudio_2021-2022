###############################################################################
#                                                                             #
#   Arnone Matteo                                                             #
#   Bastin Thomas                                                             #
#   2201                                                                      #
#   I.1. Recuperation et mise en forme des donn?es                            #
#                                                                             #
###############################################################################

#HEADER
# Vider la memoire
rm(list=ls()) 
# creer une fonction printf  
printf <- function(...) cat (sprintf(...)) 

# creer une fonction pour effacer la console
cls <- function() printf("\014")          
cls()

setwd("E:\\HEPL\\R\\Statistique_Rstudio_2021-2022\\I Statistique Descriptive à une Dimension")
#FIN HEADER

grossistes <- data.frame()
grossistes <- read.csv(file = "WholesaleCustomersData.csv", sep = ",", na.strings = "NA", header = TRUE)
summary(grossistes)


summary(grossistes)