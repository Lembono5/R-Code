###########################################################################
###########################################################################
###                                                                     ###
###                            ABSCHNITT 0:                             ###
###               LADEN DER PAKETE UND EINLESEN DER DATEN               ###
###                                                                     ###
###########################################################################
###########################################################################

# Arbeitsverzeichnis setzen
setwd("C:/Users/LocalAdmin/Desktop/R/Planungen")

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
library(car)
library(lsr)
library(psych)


#Daten im Long-Format
data = read.csv2('cache/Planungen.csv')
