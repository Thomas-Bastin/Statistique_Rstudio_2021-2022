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


#   8.1) Les boulons grand format PontLanaye007 sont des boulons d'un diamètre de 15 cm avec
#        un écart-type de 0.25 cm. 
#       
#        Le contrôleur de production prélève un premier échantillon de 15 boulons dans la production : 
#        il en ressort une moyenne de 15.31 cm avec une écart-type de 0.29 cm. 
#
#        Cet échantillon est-il à considérer comme "normal" ou pas pour un niveau de
#        confiance de 95% ?

mu <- 15
sigma <- 0.25

moyenneEchantillon <- 15.31
ecart_typeEchantillon <- 0.29
N <- 15

# a) Définir la plage de valeur acceptable pour moyenne d'échantillons

# Soit moyenne de la distrib <- mu et ecart type de la distrib <- sigma/sqrt(N)
muchapeau <- mu
sigmaetoile <- sigma/sqrt(N)

# marge = Z * sigmaetoile
# Z = 1.96 pour 95%

marge <- 1.96*sigmaetoile
muChapeauMax <- mu + marge
muChapeauMin <- mu - marge


if(moyenneEchantillon >= muChapeauMin && moyenneEchantillon<= muChapeauMax){
  printf("\nEst un échantillons Normal\n")
} else{
  printf("\nn'est pas un échantillons Normal\n")
}
# Dans notre cas, la moyenne de l'échantillons n'est pas normal pour un niveau de confiance de 95%



#   8.2) Le contrôleur de production n'est pas trop rassuré et provoque une deuxième contrôle, cette fois sur 40 boulons.
#        Les résultats de ses mesures sont :
echantillon <- c(15.21,15.56,15.63,15.54,15.22,15.77,
                 15.53,14.97,15.74,15.07,15.1,15.35,
                 15.42,14.24,16.1,14.97,15.53,15.04,
                 15.53,14.81,15.2,15.33,15.4,14.78,
                 16.44,15.36,15.35,15.06,14.99,15.99,
                 18.85,15.41,14.64,15.34,15.4,14.75,
                 15.76,14.8,15.42,15.88)

#        Il réalise au préalable un diagramme à moustaches pour détecter les éventuelles valeurs aberrantes, 
#        celles-ci étant éliminées du dataset. 

mu <- 15
sigma <- 0.25
N <- length(echantillon)

moyenneEchantillon <- mean(echantillon)
ecart_typeEchantillon <- sd(echantillon)

#        Ce deuxième échantillon est-il à considérer comme "normal" ou pas pour un niveau de confiance de 95% ? 
muchapeau <- mu
sigmaetoile <- sigma/sqrt(N)

# marge = Z * sigmaetoile
# Z = 1.96 pour 95%

marge <- 1.96*sigmaetoile
muChapeauMax <- mu + marge
muChapeauMin <- mu - marge

if(moyenneEchantillon >= muChapeauMin && moyenneEchantillon<= muChapeauMax){
  printf("\nEst un échantillons Normal\n")
} else{
  printf("\nn'est pas un échantillons Normal\n")
}
# Dans notre cas, la moyenne de l'échantillons n'est pas normal pour un niveau de confiance de 95%
