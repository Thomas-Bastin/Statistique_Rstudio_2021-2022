###############################################################################
#                                                                             #
#   Arnone Matteo                                                             #
#   Bastin Thomas                                                             #
#   2201                                                                      #
#   II.8. Interval de confiance de la moyenne des échantillons                #
#                                                                             #
###############################################################################

#HEADER
# Vider la memoire
rm(list=ls())
printf <- function(...) cat (sprintf(...))
cls <- function() printf("\014")
setwd("E:\\HEPL\\R\\Statistique_Rstudio_2021-2022\\II Inférence Statistique de Base et Estimation")
s <- function(X)
return(sqrt( sum((X - mean(X))^2) / (length(X))))
  
#FIN HEADER
cls()
