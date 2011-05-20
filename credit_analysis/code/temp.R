#! /usr/bin/Rscript 
############################################
### File:  taller3g.R
### Author:	Blanca A. Vargas-Govea
### Email:	blanca.vg@gmail.com
############################################

library(ggplot2)
library(proto)
library(moments)                  # load the moments package 

setwd("/home/blanca/laestancia/presentaciones/tallerR/credit_analysis/code")
credit <- read.csv(file = "../data/credit-g.csv", sep = ",", na.strings = "NULL")
attach(credit)

############
d <- ggplot(credit, aes(age))
d + geom_bar()+facet_wrap(~ housing)
ggsave(file = "../figures/gg-foo14.eps", width=5, height=5)
