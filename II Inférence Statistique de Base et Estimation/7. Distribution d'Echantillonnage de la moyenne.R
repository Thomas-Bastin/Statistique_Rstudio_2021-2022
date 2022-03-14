###############################################################################
#                                                                             #
#   Arnone Matteo                                                             #
#   Bastin Thomas                                                             #
#   2201                                                                      #
#   II.6. Utilisation de la loi normale                                       #
#                                                                             #
###############################################################################

#HEADER
# Vider la memoire
rm(list=ls())
printf <- function(...) cat (sprintf(...))
cls <- function() printf("\014")
cls()
setwd("E:\\HEPL\\R\\Statistique_Rstudio_2021-2022\\II Inférence Statistique de Base et Estimation")
s <- function(X)
return(sqrt( sum((X - mean(X))^2) / (length(X))))
#FIN HEADER

      

#   Il s'agit, dans l'environnement R, de simuler le tirage d'un grand nombre d'échantillons
#   d'une même population afin d'établir la forme de la distribution d'échantillonnage de la moyenne.

#   On suppose travailler sur une population de 500 pièces, 
#   population normale ayant une
#   moyenne pour le diamètre de 50 mm avec un écart-type de 1.7 mm.

mu <- 50
sigma <- 1.7

pop <- runif(n = 500, min = 0, max = 100)
#   ♦ on demande d'abord de construire un échantillon aléatoire selon le principe classique
#     suivant : 

#     on associe à chaque élément de la population un nombre aléatoire (disons entre 0 et 100), 
#     puis on trie selon ce nombre aléatoire et on conserve finalement les
#     20 ou 40 premiers éléments pour former l'échantillon; on récupérera la moyenne de cet échantillon;


#   ♦ on demande ensuite d'écrire un script R qui va générer 500 échantillons du type précédent;
#     les moyennes de ces échantillons seront placées dans un tableau "moyennes". échantillons;


#   ♦ il reste alors à représenter graphiquement la distribution de ces moyennes. 
      