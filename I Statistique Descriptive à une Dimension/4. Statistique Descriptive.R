###############################################################################
#                                                                             #
#   Arnone Matteo                                                             #
#   Bastin Thomas                                                             #
#   2201                                                                      #
#   I.4. Statistique Descriptive                                              #
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
rm(name)
summary(grossistes)
str(grossistes)
#FIN HEADER

Frozen <- sample(na.omit(grossistes$Frozen), size = 60)

# Il s'agit à présent de réaliser l'étude statistique descriptive complète de cet échantillon
# (moyenne, écart-type, médiane, …); en particulier, calculer manuellement l'écart-type de
# l'échantillon et le comparer à celui fourni par la fonction sd(); 

effectif <- length(Frozen)
moyenne <- mean(Frozen)
mediane <- median(Frozen)
ecart_type <- sd(Frozen)

# Calcul Ecart Type Echantillon sd() calcul donc uniquement les écart types d'échantillon, et non de population.
ecart_type2 <- sqrt((sum((Frozen - mean(Frozen))^2) / (effectif)))

# On demande aussi de construire au fur et à mesure un vecteur grossistes.surgeles.echantillon.descriptive 
# contenant l'effectif, la moyenne, l'écart-type (de l'échantillon) et la médiane 
# avec des noms de colonnes appropriés :

grossistes.surgeles.echantillon.descriptive <- data.frame(effectif, moyenne, ecart_type, mediane)