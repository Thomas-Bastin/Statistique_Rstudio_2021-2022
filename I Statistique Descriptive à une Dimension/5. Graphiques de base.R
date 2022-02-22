###############################################################################
#                                                                             #
#   Arnone Matteo                                                             #
#   Bastin Thomas                                                             #
#   2201                                                                      #
#   I.5. Graphiques de Base                                                   #
#                                                                             #
###############################################################################

#HEADER
# Vider la memoire
rm(list=ls())
printf <- function(...) cat (sprintf(...))
cls <- function() printf("\014")
cls()
setwd("E:\\HEPL\\R\\Statistique_Rstudio_2021-2022\\I Statistique Descriptive à une Dimension")

#Importation DataFrame
grossistes <- read.table(file = "WholesaleCustomersData.csv", sep = ",", na.strings = "NA", header = TRUE)

#Changement colonne as factor
grossistes$Channel <- as.factor(grossistes$Channel)
levels(grossistes$Channel) <- c("Restauration", "Détaillants")
grossistes$Region <- as.factor(grossistes$Region)
levels(grossistes$Region) <- c("Lisbonne", "Porto","Autre")

#Changement Nom Col
name <- colnames(grossistes)
name[which(name == 'Detergents_Paper', arr.ind = T)] <- 'DetergentsPaper'
colnames(grossistes) <- name
summary(grossistes)
str(grossistes)
#FIN HEADER

#1. un histogramme en densité des achats en produits laitiers; comment vérifier qu'il s'agit
#   bien d'une densité de probabilité?

datanorm <- rnorm(n = nrow(grossistes), mean = mean(grossistes$Milk), sd = sd(grossistes$Milk))
histogramme <- hist(datanorm, nclass = 20, freq = FALSE)

histogramme$breaks
histogramme$density
area <- sum( diff(histogramme$breaks) * histogramme$density)
printf("Area: %f",area)


breaks1 <- histogramme$density

#2. un histogramme amélioré qui regroupe les dernières classes;
datanorm <- rnorm(n = nrow(grossistes), mean = mean(grossistes$Milk), sd = sd(grossistes$Milk))
histogrammeAmeliore <- hist(datanorm, freq = FALSE, breaks = c(-30000,-10000,0,15000,20000,40000))

histogrammeAmeliore$breaks
histogrammeAmeliore$density
area <- sum( diff(histogrammeAmeliore$breaks) * histogrammeAmeliore$density)
printf("Area: %f",area)

#3. un graphique linéaire type "polygone des effectifs";

#4. la superposition de l'histogramme et du polygone des effectifs pour l'épicerie;

#5. un graphique des effectifs cumulés;

#6. la ventilation des observations par région

#7. une représentation des achats en surgelés en fonction des produits frais; une deuxième
#   représentation en se limitant à des achats en surgelés inférieurs ou égaux à 5000;

#8. une représentation des achats en produit frais par région; même question si on se limite à
#   des achats inférieurs à 4000;

#9. une représentation des achats en produit frais par région et par source des achats;

#10. une comparaison de la répartition Horeca/Détaillant selon la région;

#11. une comparaison des achats d'épicerie générale selon le canal et la région;

#12. deux nuages de points superposés des achats de produits frais en fonction des achats de
#    produits surgelés selon le canal d'achat;