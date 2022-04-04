###############################################################################
#                                                                             #
#   Arnone Matteo                                                             #
#   Bastin Thomas                                                             #
#   2201                                                                      #
#   I.2. Manipulations des données                                            #
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
# Set WorkingDirectory
setwd("E:\\HEPL\\R\\Statistique_Rstudio_2021-2022\\I Statistique Descriptive à une Dimension")
#FIN HEADER



#0. Importation de grossistes.csv
grossistes <- read.table(file = "WholesaleCustomersData.csv", sep = ",", na.strings = "NA", header = TRUE)

grossistes$Channel <- as.factor(grossistes$Channel)
grossistes$Region <- as.factor(grossistes$Region)
summary(grossistes)
str(grossistes)




#1. Changer le nom de la colonne "Detergents_Paper" en "DetergentsPaper"
name <- colnames(grossistes)    #Récupère le tableau de nom
i <- which(name == 'Detergents_Paper', arr.ind = T)  #Dans le tableau de nom, on cherche l'indice dont la valeur est 'Detergents_Paper' 
name[i] <- 'DetergentsPaper' #On modifie la colonne de nom, à l'indice x, on met DetergentsPaper

colnames(grossistes) <- name #On assigne la nouvelle colonne de nom à grossistes
str(grossistes)

#2. Récupérer dans une variable les données correspondant à des achats de surgelés pour 2000 
dataSurgeler2000 <- subset(x = grossistes, subset = Frozen >= 2000)

#3. Récupérer dans une variable les données correspondant à la région de Porto
dataRegionPorto <- subset(x = grossistes, subset = Region == 2)


#4. Récupérer dans une variable les données correspondant à la région de Lisbonne 
#   et à des achats de produits laitiers inférieurs à 500 m.u.
dataLisbonneProduitLaitiers <- subset(x = grossistes, subset = Region == 1 & Milk < 500)

#5. Combien y -a-t-il d'observations pour lesquelles toutes les variables ont une valeur connue ?
nrowNotNaN <- nrow(na.omit(grossistes))
#On récupère les tuples pour lesquels les cases sont nonvide, ensuite on fais un nrow deçus pour compté

#6. Créer un dataset "grossistes.quanti" ne contenant que les variables quantitatives
grossistes.quanti <- Filter(is.numeric, grossistes)

#7. Créer un dataset " grossistes.quali" ne contenant que les variables qualititatives
grossistes.quali <- Filter(is.factor, grossistes)

#8. Créer un dataset à partir de grossistes en retirant les lignes correspondant à la région
#"Other"
dataNotOther <- subset(x = grossistes, subset = Region != 3)

#9. Compter le nombre d'observations selon chaque région
nrowLisbonne <- nrow(subset(x = grossistes, subset = Region == 1))
nrowPorto <- nrow(subset(x = grossistes, subset = Region == 2))
nrowAutre <- nrow(subset(x = grossistes, subset = Region == 3))

#10. Compter le nombre de relevés selon chaque région et selon canal
nrowLisbonneHoreca <- nrow(subset(x = grossistes, subset = Region == 1 & Channel == 1))
nrowPortoHoreca <- nrow(subset(x = grossistes, subset = Region == 2 & Channel == 1))
nrowAutreHoreca <- nrow(subset(x = grossistes, subset = Region == 3 & Channel == 1))

nrowLisbonneRetail <- nrow(subset(x = grossistes, subset = Region == 1 & Channel == 2))
nrowPortoRetail <- nrow(subset(x = grossistes, subset = Region == 2 & Channel == 2))
nrowAutreRetail <- nrow(subset(x = grossistes, subset = Region == 3 & Channel == 2))

#11. Plus compliqué: Changer les modalités de la variable "Channel" en "Restauration" et "Détaillants".
levels(grossistes$Channel) <- c("Restauration", "Détaillants")
str(grossistes)
