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
a <- subset(x = grossistes, subset = Frozen >= 2000)

#3. Récupérer dans une variable les données correspondant à la région de Porto

#4. Récupérer dans une variable les données correspondant à la région de Lisbonne 
#   et à des achats de produits laitiers inférieurs à 500 m.u.  

#5. Combien y -a-t-il d'observations pour lesquelles toutes les variables ont une valeur connue ?

#6. Créer un dataset "grossistes.quanti" ne contenant que les variables quantitatives

#7. Créer un dataset " grossistes.quali" ne contenant que les variables qualititatives

#8. Créer un dataset à partir de grossistes en retirant les lignes correspondant à la région
#"Other"

#9. Compter le nombre d'observations selon chaque région

#10. Compter le nombre de relevés selon chaque région et selon canal

#11. Plus compliqué: Changer les modalités de la variable "Channel" en "Restauration" et
#"Détaillants"
