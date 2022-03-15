###############################################################################
#                                                                             #
#   Arnone Matteo                                                             #
#   Bastin Thomas                                                             #
#   2201                                                                      #
#   I.3. Echantillonage                                                       #
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


# On demande d'isoler les chiffres d'achats de produits surgelés
# (plusieurs moyens mais quelle différence ? comment obtenir le nombre d'éléments ?)
grossistes_frozen1 <- subset(grossistes, select = Frozen)
grossistes_frozen2 <- grossistes[,'Frozen']
grossistes_frozen3 <- grossistes$Frozen

nrow(grossistes_frozen1)
length(grossistes_frozen2)
length(grossistes_frozen3)

# puis de construire un échantillon de 60 éléments à partir des données
# complètes en utilisant la fonction R sample().
mysample <- sample(grossistes_frozen3, size = 60)

# Comment faire si on ne dispose pas d'une telle fonction ?

# Si on ne dispose pas d’une telle fonction, 
# il suffit d’associer des nombres aléatoires à chaque élément de la population. 
# Ensuite, de trier sur les nombres aléatoires et de prendre les 60 premiers pour constituer notre échantillon.
