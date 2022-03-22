###############################################################################
#                                                                             #
#   Arnone Matteo                                                             #
#   Bastin Thomas                                                             #
#   2201                                                                      #
#   III.10. Test de conformité de moyenne et de variance                      #
#                                                                             #
###############################################################################

#HEADER
# Vider la memoire
rm(list=ls())
printf <- function(...) cat (sprintf(...))
cls <- function() printf("\014")
setwd("E:\\HEPL\\R\\Statistique_Rstudio_2021-2022\\III Les tests d'hypothèse de conformité")
s <- function(X)
return(sqrt( sum((X - mean(X))^2) / (length(X))))

#FIN HEADER
cls()

#Importation DataFrame
FER <- read.table(file = "fers_a_repasser.csv", sep = ",", na.strings = "NA", header = TRUE, dec = ".", row.names = "ident")

# Présentation
summary(FER)
str(FER)

EchantFerRepasser <- sample(FER$duree, 40)

myboxplot <- boxplot(EchantFerRepasser)
myboxplot$out

if(length(myboxplot$out) == 0){
  printf("Pas de Valeur en abérente")
} else{
  printf("Retrait des Valeurs abérentes")
  EchantFerRepasser.Nettoye <- EchantFerRepasser
  
  for(k in myboxplot$out){ #Pour chaque valeur abhérente
    i <- which(EchantFerRepasser.Nettoye == k, arr.ind = T)
    EchantFerRepasser.Nettoye[i] <- NaN #On met à NaN les valeurs abhérentes.
  }
  rm(i)
  rm(k)
  
  EchantFerRepasser <- na.omit(EchantFerRepasser.Nettoye)
}

EchantFerRepasser
