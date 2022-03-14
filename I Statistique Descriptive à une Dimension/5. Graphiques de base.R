###############################################################################
#                                                                             #
#   Arnone Matteo                                                             #
#   Bastin Thomas                                                             #
#   2201                                                                      #
#   I.5. Graphiques de Base                                                   #
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

s <- function(X)
return(sqrt( sum((X - mean(X))^2) / (length(X))))

grossistes <- na.omit(grossistes)
#FIN HEADER

#1. un histogramme en densité des achats en produits laitiers; comment vérifier qu'il s'agit
#   bien d'une densité de probabilité?
x11()
histogramme <- hist(grossistes$Milk, freq = F, nclass = 30)

histogramme$breaks
histogramme$density

area <- sum( diff(histogramme$breaks) * histogramme$density)
printf("Area histogramme: %f",area)

#2. un histogramme amélioré qui regroupe les dernières classes;
x11()

mybreaks <- histogramme$breaks[1:12]
mybreaks[12]<-max(grossistes$Milk)

histogrammeAmeliore <- hist(grossistes$Milk, freq = FALSE, breaks = mybreaks)

histogrammeAmeliore$breaks
histogrammeAmeliore$density
area <- sum( diff(histogrammeAmeliore$breaks) * histogrammeAmeliore$density)
printf("Area histogramme ameliorer: %f",area)



#3. un graphique linéaire type "polygone des effectifs";
x11()
plot(histogramme$mids,histogramme$density, type = "b", lty = 1)


#4. la superposition de l'histogramme et du polygone des effectifs pour l'épicerie;
x11()
histogrammeEpicerie <- hist(grossistes$Grocery, freq = FALSE)
lines(x = histogrammeEpicerie$mids, y = histogrammeEpicerie$density, type="b", lty=1,xlab = "", ylab ="")


#5. un graphique des effectifs cumulés;
x11()
densitycumsum <- cumsum(histogrammeEpicerie$counts)
plot(x = histogrammeEpicerie$mids, y = densitycumsum, type="b",lty = 1)


#6. la ventilation des observations par région
x11()
# Creation des agrégats / ventilations en fonction des régions
VentilationFresh <- aggregate(x = grossistes[,3] , by = grossistes['Region'], FUN = sum)
VentilationMilk <- aggregate(x = grossistes[,4] , by = grossistes['Region'], FUN = sum)
VentilationGrocery <- aggregate(x = grossistes[,5] , by = grossistes['Region'], FUN = sum)
VentilationFrozen <- aggregate(x = grossistes[,6] , by = grossistes['Region'], FUN = sum)
VentilationDetergentsPaper <- aggregate(x = grossistes[,7] , by = grossistes['Region'], FUN = sum)
VentilationDelicassen <- aggregate(x = grossistes[,8] , by = grossistes['Region'], FUN = sum)

# Creation des zones d'affichage
zones <- matrix(c(1,2,3,4,5,6), ncol = 3)
layout(zones)
layout.show(max(zones))

# Creations des Camemberts
pie(x = VentilationFresh$x, labels = VentilationFresh$Region, main = "Fresh", radius = 1.2)
pie(x = VentilationMilk$x, labels = VentilationMilk$Region, main = "Milk", radius = 1.2)
pie(x = VentilationGrocery$x, labels = VentilationGrocery$Region, main = "Grocery", radius = 1.2)
pie(x = VentilationFrozen$x, labels = VentilationFrozen$Region, main = "Frozen", radius = 1.2)
pie(x = VentilationDetergentsPaper$x, labels = VentilationDetergentsPaper$Region, main = "DetergentsPaper", radius = 1.2)
pie(x = VentilationDelicassen$x, labels = VentilationDelicassen$Region, main = "Delicassen", radius = 1.2)


#7. une représentation des achats en surgelés en fonction des produits frais; une deuxième
#   représentation en se limitant à des achats en surgelés inférieurs ou égaux à 5000;
x11()
zones <- matrix(c(1,2), ncol = 2)
layout(zones)
layout.show(max(zones))

plot(x = grossistes$Frozen, y = grossistes$Fresh ,type="p", lty=1,  xlab = 'Frozen', ylab = 'Fresh')
achat <- subset(grossistes, subset = Frozen <= 5000) 
plot(x = achat$Frozen, y = achat$Fresh, type="p", lty=1, xlab = 'Frozen', ylab = 'Fresh')


#8. une représentation des achats en produit frais par région; 
#   même question si on se limite à des achats inférieurs à 4000;
x11()
zones <- matrix(c(1,2), ncol = 2)
layout(zones)
layout.show(max(zones))

achat <- subset(grossistes, select = c('Region','Fresh')) 
barplot(table(achat$Region)) #table calcul l'effectif de chaque groupe (Region)

barplot(table(subset(achat, subset = Fresh <= 4000)$Region))


#9. une représentation des achats en produit frais par région et par source des achats;
x11()
zones <- matrix(c(1,2), ncol = 2)
layout(zones)
layout.show(max(zones))

barplot(table(subset(grossistes, select = c('Channel','Fresh'))$Channel)) #table calcul l'effectif de chaque groupe (Source Achat)

barplot(table(subset(grossistes, select = c('Region','Fresh'))$Region)) #table calcul l'effectif de chaque groupe (Source Achat)


#10. une comparaison de la répartition Horeca/Détaillant selon la région;
x11()
Horeca_Detaillant <- subset(x = grossistes,select = c('Region','Channel'))
plot(Horeca_Detaillant$Channel~Horeca_Detaillant$Region, xlab = 'Region', ylab = 'Channel')


#11. une comparaison des achats d'épicerie générale selon le canal et la région;
x11()
data <- aggregate(x = grossistes['Grocery'] , by = c(grossistes['Region'],  grossistes['Channel']), FUN = sum)

data1 <- subset(data, subset = Channel == 'Restauration')
data2 <- subset(data, subset = Channel == 'Détaillants')

Vent_Grocery_RegionChannel <- matrix(c(data1$Grocery,data2$Grocery), nrow = 3)
barplot(Vent_Grocery_RegionChannel, beside = T, names.arg = c("Restauration","Détaillants"), col = c("bisque2","brown1","chocolate1"))
legend("topleft", legend = c("Lisbonne","Porto","Autre"), fill = c("bisque2","brown1","chocolate1"))


#12.  deux nuages de points superposés 
#     des achats de produits frais en fonction des achats de produits surgelés 
#     selon le canal d'achat;
x11()

gDét <- subset(x = grossistes, subset = (Channel == 'Détaillants'), select = c('Fresh','Frozen'))
plot(y = gDét$Fresh, x = gDét$Frozen, type="p", xlim = c(0,20000), ylim = c(0,45000), xlab = "Frozen", ylab = "Fresh", col = "chocolate2", pch = 20)

par(new = T)

gRest <- subset(x = grossistes, subset = (Channel == 'Restauration'), select = c('Fresh','Frozen'))
plot(y = gRest$Fresh, x = gRest$Frozen, type="p", xlim = c(0,20000), ylim = c(0,45000), xlab = "Frozen", ylab = "Fresh", col = "chartreuse3", pch = 20)
