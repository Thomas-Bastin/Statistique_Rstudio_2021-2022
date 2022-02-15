###############################################################################
#                                                                             #
#   Arnone Matteo                                                             #
#   Bastin Thomas                                                             #
#   2201                                                                      #
#   I.1. Recuperation et mise en forme des données                            #
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


#1. Importation de valeur dans grossistes
grossistes <- read.csv(file = "WholesaleCustomersData.csv", sep = ",", na.strings = "NA", header = TRUE)
summary(grossistes)
str(grossistes)

#Le summary montre que l'on a importé certaines valeurs qualitative en temps que quantitative (Channel, Region)

#le str(grossistes) permet de voir les types des collonnes, ainsi que les premières variables de chaque colonnes
#Ainsi que les 8 colonnes(Variables) et les 439 Lignes (obs).

#2. Il y a 8 colonnes(variables) et 439 Lignes(enregistrement) 

#On met en variable stat qualitative(factor) la colonnes Channel et Region
grossistes$Channel <- as.factor(grossistes$Channel)
grossistes$Region <- as.factor(grossistes$Region)
summary(grossistes)
str(grossistes)


#3. Comment récupérer les noms des lignes et des colonnes ?
colnames(grossistes) #Récupère les noms de colonnes
rownames(grossistes) #Récupère les lignes de colonnes


#4. Créer une variable produitsFrais contenant la 3ème colonne du dataset de 3 manières différentes. 
produitsFrais1 <- grossistes$Fresh #En Colonnes
produitsFrais2 <- grossistes[,3]   #En Colonnes la virgule évite l'importation en dataset
produitsFrais3 <- grossistes[,'Fresh'] #En Colonnes la virgule évite l'importation en dataset
produitsFrais4 <- subset(x = grossistes, select = 'Fresh')$Fresh #En Colonne car on récupère uniquement la colonne Fresh du dataset généré

