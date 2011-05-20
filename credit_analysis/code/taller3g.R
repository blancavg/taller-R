#! /usr/bin/Rscript 
############################################
### File:  taller3g.R
### Author:	Blanca A. Vargas-Govea
### Email:	blanca.vg@gmail.com
############################################
library(ggplot2)
library(proto)
library(moments)                  # load the moments package 

setwd("/home/blancavg/laestancia/presentaciones/tallerR/credit_analysis/code")
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
##########################################

d <- ggplot(data = credit, aes(x = credit_amount, y = age)) 
d + geom_boxplot(outlier.shape = 4) + theme_bw() + scale_y_continuous(breaks = seq(0, 100, by = 5))
ggsave(file = "../figures/gg-boxplot.eps", width=5, height=5)

d <- ggplot(data = credit, aes(age)) 
d + geom_bar() + theme_bw()
ggsave(file = "../figures/gg-foo10.eps", width=5, height=5)

d <- ggplot(data = credit, aes(age)) 
d + geom_bar(binwidth = 0.1) + theme_bw()
ggsave(file = "../figures/gg-foo10a.eps", width=5, height=5)

d <- ggplot(data = credit, aes(age)) 
d + geom_histogram(aes(y = ..count..),binwidth = 0.5)  + 
    theme_bw() + 
   scale_x_continuous(breaks = c(media)) + 
    geom_vline(xintercept = media, size = 0.5, colour = "magenta") +
    geom_vline(xintercept = media+sd, size = 0.5, colour = "blue", linetype = 2) +
    geom_vline(xintercept = media-sd, size = 0.5, colour = "blue", linetype = 2) 
ggsave(file = "../figures/gg-foo11.eps", width=5, height=5)

# Subconjuntos
# Filtering by all age data ( oldmin < Age < oldmax )
mayor30 <- subset(credit, age >= 30)
renmayor30 <- nrow(mayor30)

en20y40 <- subset(credit, age >= 20  & age <= 40)
ren20y40 <- nrow(en20y40)
ren20y40

# Filtering by bins ( 10 <= Age < 75 )
agebin = cut(age,breaks = c(18,30,40,50,60,70,80))
agebinfile <- data.frame(purpose,credit_amount,personal_status,housing,job,age=agebin,class)
agebinfile

d <- ggplot(data = agebinfile, aes(age)) 
d + stat_bin(aes(ymax = ..count..), geom = "bar") + theme_bw()
ggsave(file = "../figures/gg-foo12.eps", width=5, height=5)

d <- ggplot(credit, aes(housing))
d + geom_bar()
ggsave(file = "../figures/gg-foo13.eps", width=5, height=5)

d <- ggplot(credit, aes(age,fill=housing))
d + geom_bar()
ggsave(file = "../figures/gg-foo14.eps", width=5, height=5)

d <- ggplot(credit, aes(age))
d + geom_bar()+facet_wrap(~ housing)
ggsave(file = "../figures/gg-foo15.eps", width=5, height=5)


dev.off()
