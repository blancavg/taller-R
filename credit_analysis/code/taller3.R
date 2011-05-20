#! /usr/bin/Rscript 
############################################
### File:  taller3.R
### Author:	Blanca A. Vargas-Govea
### Email:	blanca.vg@gmail.com
############################################
#library(ggplot2)
#library(proto)

setwd("/home/blanca/laestancia/presentaciones/tallerR/credit_analysis/code")
credit <- read.csv(file = "../data/credit-g.csv", sep = ",", na.strings = "NULL")
attach(credit)


# Muestra
set.seed(32) # Make the sample reproducible
dsmall <- credit[sample(nrow(credit), 5), ]
dsmall

# scatter
png(filename = "../figures/foo1.png")
plot(age,credit_amount)

png(filename = "../figures/foo1b.png")
plot(dsmall$age,type= "o",col="blue")

png(filename = "../figures/foo1c.png")
plot(dsmall$existing_credits, type="o", col="blue", ylim=c(0,12))
lines(dsmall$residence_since, type="o", pch=22, lty=2, col="red")
#title(main="Autos", col.main="red", font.main=4)
#legend(8,12, c("existing_credits","residence_since"), cex=0.8, 
#   col=c("blue","red"), pch=21:22, lty=1:2)
legend(7,12, c("existing_credits","residence_since"), 
   col=c("blue","red"), pch=21:22, lty=1:2)


# bigotes
png(filename = "../figures/foo2.png")
boxplot(age)

# histograma simple
png(filename = "../figures/foo3.png")
hist(credit_amount)

# histograma color con diferente no. de bins
png(filename = "../figures/foo4.png")
hist(credit_amount, breaks=30, col="blue") 

# Simple Bar Plot
png(filename = "../figures/foo5.png")
counts <- table(housing)
barplot(counts, main="Housing") 

# Simple Horizontal Bar Plot 
png(filename = "../figures/foo6.png")
counts <- table(class)
barplot(counts, main="Personal status", horiz=TRUE)

# Barras agrupadas 
png(filename = "../figures/foo6a.png")
myfile <- data.frame(dsmall$age,dsmall$residence_since,dsmall$duration)
barplot(as.matrix(myfile), main="Crédito", ylab= "Total",
   beside=TRUE, col=rainbow(5), cex.names=0.9)
legend("topright", c("e1","e2","e3","e4","e5"), bty="n", fill=rainbow(5));


#help(hist)

# densidad
png(filename = "../figures/foo7.png")
d <- density(credit_amount) # returns the density data
plot(d) # plots the results 

# Filled Density Plot
png(filename = "../figures/foo8.png")
d <- density(credit_amount)
plot(d)
polygon(d, col="red") 

# Basic Scatterplot Matrix
png(filename = "../figures/foo9.png")
pairs(~credit_amount+age+existing_credits+num_dependents,data=credit,
   main="Simple Scatterplot Matrix")

ammin <- min(credit_amount,na.rm = TRUE) # min without NULLS in Age
ammax <- max(credit_amount,na.rm = TRUE)

dev.off()
