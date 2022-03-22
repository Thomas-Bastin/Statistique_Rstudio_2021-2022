rm(list=ls()) # vider la memoire


printf <- function(...) cat (sprintf(...)) # creer une fonction printf
cls <- function() printf("\014")           # creer une fonction pour effacer la console
cls()
