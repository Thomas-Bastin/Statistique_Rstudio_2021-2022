###############################################################################
#                                                                             #
#   Arnone Matteo                                                             #
#   Bastin Thomas                                                             #
#   2201                                                                      #
#   II.9. Problèmes classiques d'estimation de moyenne                        #
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

#Importation DataFrame
HER <- read.table(file = "SanteAlimentationUSA.csv", sep = ";", na.strings = "NA", header = TRUE, dec = ",", row.names = 'IDEN')

#Changement colonne as factor
HER$SEXE <- as.factor(HER$SEXE)
levels(HER$SEXE) <- c('Homme','Femme')

# Présentation
summary(HER)
str(HER)

s <- function(X)
  return( 
    sqrt( sum((X - mean(X))^2) / (length(X)) ) 
  )

HER <- na.omit(HER)

#FIN HEADER
cls()

# 9.1) Les mécaniciens d'une entreprise de fabrication métallique viennent s'approvisionner en
#      pièces à un guichet du magasin de l'usine. 
#      
#      D'après les contremaîtres, il se produit là une perte de temps non négligeable (est-ce volontaire ?)
#      Le chef de service a donc décidé de relever pendant 5 jours les temps d'attente à ce fameux guichet. 
#      Cela a donné 110 observations qui, une fois traitées, ont donné une moyenne de 8.5 min/visite 
#      avec un écart-type de 1.4 min/visite.
#      
#      On demande d'estimer par intervalle de confiance à 90% et à 99% la durée d'attente moyenne
#      qui sera le critère admissible dans la suite.

moyenneEchantillon <- 8.5
ecart_typeEchantillon <- 1.4
N <- 110

# Calcul interval Confiance pour mu à 90%  sigma inconnus
muChapeau <- moyenneEchantillon
sigmaEtoileChapeau <- ecart_typeEchantillon/sqrt(N-1)

q95 <- qnorm(0.95, mean = muChapeau, sd = sigmaEtoileChapeau)
q05 <- qnorm(0.05, mean = muChapeau, sd = sigmaEtoileChapeau)
interval90 <- data.frame(q05,q95)

# Calcul interval Confiance pour mu à 99%  sigma inconnus
q995 <- qnorm(0.995, mean = muChapeau, sd = sigmaEtoileChapeau)
q005 <- qnorm(0.005, mean = muChapeau, sd = sigmaEtoileChapeau)
interval99 <- data.frame(q005,q995)


# 9.2) On revient aux données du Centre national des statistiques de santé pour le comté de Hardee (Floride). 
#      Cette fois, on s'intéresse à la variable TTAILLE qui représente le tour de taille (en cm). 
#      
#      L'industrie vestimentaire indique que si elle constate un accroissement moyen de ce tour de taille 
#      au fil des ans, l'écart-type reste assez constant, soit 1.6 cm.
#      
#      On demande d'estimer sur cette base le tour de taille moyen de la population par un intervalle
#      de confiance à 95%. 

moyenneEchantillon <- mean(HER$TTAILLE)
ecart_typeEchantillon <- sd(HER$TTAILLE)
N = length(HER$TTAILLE)
sigma <- 1.6

# On cherche l'interval de confiance de mu à 95% sigma est connus
muChapeau <- moyenneEchantillon
sigmaetoile <- sigma/sqrt(N)

q975 <- qnorm(0.975, mean = muChapeau, sd = sigmaetoile)
q025 <- qnorm(0.025, mean = muChapeau, sd = sigmaetoile)
interval95TTAILLE <- data.frame(q025,q975)


# 9.3) Un classique : des essais sur 20 LED servant de témoins sur des panneaux de contrôle ont
#      donné les résultats suivants (en heures) :
echantillon <- c(
                 451,412,412,375,
                 407,454,375,393,
                 355,364,414,413,
                 345,432,392,329,
                 439,381,451,413
                )
  
# On demande d'estimer cette durée de vie moyenne pour toute la production par un intervalle
# de confiance à 95%.
N <- length(echantillon)
ecart_typeEchantillon <- sd(echantillon)
moyenneEchantillon <- mean(echantillon)

# mu inconnu, sigma inconnu
muChapeau <- moyenneEchantillon
sigmaEtoileChapeau <- ecart_typeEchantillon/sqrt(N-1)

q975 <- qnorm(0.975, mean = muChapeau, sd = sigmaetoile)
q025 <- qnorm(0.025, mean = muChapeau, sd = sigmaetoile)
interval95LED <- data.frame(q025,q975)

# Remarque: Le plus simple (si du moins on pense que l'on pourra refaire ce genre de calculs)
# est de placer les données dans un fichier Excel, puis de générer un fichier csv qui sera ensuite
# relu par read.table()).