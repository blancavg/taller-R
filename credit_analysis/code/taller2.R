#! /usr/bin/Rscript 
############################################
### File:  taller2.R 
### Author:	Blanca A. Vargas-Govea
### Email:	blanca.vg@gmail.com
############################################
# Análisis descriptivo: media, mediana, moda, cuantiles, rango, varianza, sd, 
# coeficiente de desviación, skewness y kurtosis


library(moments)                  # load the moments package 

setwd("/home/blanca/laestancia/presentaciones/tallerR/credit_analysis/code")
credit <- read.csv(file = "../data/credit-g.csv", sep = ",", na.strings = "NULL")
attach(credit)

######################
Mode <- function (x) {
    cngtable <- table(x)
    n <- length(cngtable)
    mode <- as.double(names(sort(cngtable)[n]))
    mode
}
######################
Rng <- function(x) {
    rangem <- diff(range(x))
    rangem
}
######################
Quantiles <- function(x) {
    quants <- quantile(x)
    quantval <- as.double(names(table(quants)))
    quantval
}
######################

# Media
media <- round(mean(age),2)
media
# Mediana
mediana <- round(median(age),2)
mediana
# Moda
moda <- Mode(age)
moda
# Range
rango <- Rng(age)
rango
# Quantiles
q <- Quantiles(sort(age))
q
# Variance
varianza <- round(var(age),2)
varianza
# Standard deviation
sd <- round(sd(age),2)
sd
# Kurtosis
kurtosis <- round(kurtosis(age),2)
kurtosis
# Skewness
sk <- round(skewness(age),2)
sk

# Summary
summary(age)


