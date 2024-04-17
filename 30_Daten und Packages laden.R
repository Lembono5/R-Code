###########################################################################
###########################################################################
###                                                                     ###
###                            ABSCHNITT 0:                             ###
###               LADEN DER PAKETE UND EINLESEN DER DATEN               ###
###                                                                     ###
###########################################################################
###########################################################################

# Arbeitsverzeichnis setzen
setwd("C:/Users/LocalAdmin/Desktop/R/Kurzerklärungen")

# Environment leeren
rm(list = ls())

# Pakete laden
library(Matrix)
library(foreign)
library(lme4)
library(MuMIn)
library(MASS)
library(tidyverse)
library(lmerTest)
library(multilevel)
library(ggplot2)


#Daten im Long-Format
data = read.csv2('cache/Kurzerklärungen.csv')

