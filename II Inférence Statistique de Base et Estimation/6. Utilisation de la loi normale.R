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
#FIN HEADER


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





#a)   Quels sont les effectif, étendue, moyenne et écart-type de l'échantillon de la variable
#     aléatoire poids ?
      
      max <- max(HER$POIDS)
      min <- min(HER$POIDS)
      etendue <- max - min
      
      effectif <- length(HER$POIDS) 
      moyenne <- mean(HER$POIDS)
      ecart_type <- sd(HER$POIDS)
      
      hist(x = HER$POIDS, freq = F)

#b)   Au niveau de la population, quelle est la probabilité qu'un citoyen(ne) choisi(e) au hasard
#     ait un poids inférieure à 75 kg ?
      p1 <- pnorm(q = 75, sd = ecart_type, mean = moyenne, lower.tail = T)
      
#c)   Quelle est la probabilité qu'un citoyen(ne) choisi(e) au hasard ait un poids compris dans la
#     fourchette [80,95] ?
      pmin <- pnorm(q = 80, sd = ecart_type, mean = moyenne, lower.tail = T)
      pmax <- pnorm(q = 95, sd = ecart_type, mean = moyenne, lower.tail = T)
      
      p2 <- pmax - pmin


#d)   Quelle est la probabilité qu'un citoyen(ne) choisi(e) au hasard pèse plus de 90 kg ?
      p3 <- pnorm(q = 90, sd = ecart_type, mean = moyenne, lower.tail = F)


#e)   Quelle est la probabilité qu'un citoyen(ne) choisi(e) au hasard ait un poids qui diffère de la
#     moyenne par moins de 5 kg ?
      pmin <- pnorm(q = (moyenne-5), sd = ecart_type, mean = moyenne, lower.tail = T)
      pmax <- pnorm(q = (moyenne+5), sd = ecart_type, mean = moyenne, lower.tail = T)
      
      p4 <- pmax - pmin


#f)   Pour une autre enquête aléatoire portant sur 1000 citoyens, combien auraient probablement
#     un poids supérieur à 70 kg ?
      p5 <- pnorm(q = 70, sd = ecart_type, mean = moyenne, lower.tail = F)

#g)   Même question pour les hommes et pour les femmes.
      HER_Femme <- subset(HER, subset = SEXE == 'Femme')
      HER_Homme <- subset(HER, subset = SEXE == 'Homme')
      
      p6 <- pnorm(q = 70, sd = sd(HER_Femme$POIDS), mean = mean(HER_Femme$POIDS), lower.tail = F)
      p7 <- pnorm(q = 70, sd = sd(HER_Homme$POIDS), mean = mean(HER_Homme$POIDS), lower.tail = F)


#h)   Quel est le poids maximum correspondant aux 50% des citoyen(ne)s les plus légers ?
      q1 <- qnorm(p = 0.50, sd = ecart_type, mean = moyenne, lower.tail = T)
      #q1 est bien égal à la moyenne
      
      
#i)   Quel est le poids minimum des 25% des citoyen(ne)s les plus lourds ?
      q2 <- qnorm(p = 0.25, sd = ecart_type, mean = moyenne, lower.tail = F)

      
#j)   Quel poids moyen devrait-on atteindre à l'issue d'un programme diététique généralisé pour
#     assurer qu'un(e) citoyen sur 10 seulement ait effectivement un poids supérieur à 85 kg ?
      
# Pas réussi
