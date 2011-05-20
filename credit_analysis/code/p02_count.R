#! /usr/bin/Rscript --vanilla
############################################
### File:	univar.R
### Author:	Blanca A. Vargas-Govea
### Email:	blanca.vg@gmail.com
### Data:	clean_BX-Users.csv 
############################################
# Exploratory analysis: mean, median, mode, quantile, range, variance, sd, coefficient of deviation, skewness, kurtosis

library(ggplot2)
library(proto)
library(moments)                  # load the moments package 

users <- read.csv(file = "../data/clean_BX-Users.csv", sep = ",", na.strings = "NULL")
#"UserID"   "Location" "Age" 

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

# Mean
# Filtering by all age data ( oldmin < Age < oldmax )
oldmin <- min(users$Age,na.rm = TRUE) # min without NULLS in Age
oldmax <- max(users$Age,na.rm = TRUE)
alldata <- subset(users, Age >= oldmin  & Age <= oldmax)
allmean <- round(mean(alldata$Age),2)
# Median
allmedian <- round(median(alldata$Age),2)
# Mode
allmode <- Mode(alldata$Age)
# Range
allrange <- Rng(alldata$Age)
# Quantiles
allq <- Quantiles(sort(alldata$Age))
# Variance
allvariance <- round(var(alldata$Age),2)
# Standard deviation
allsd <- round(sd(alldata$Age),2)
# Kurtosis
allkurtosis <- round(kurtosis(alldata$Age),2) 
# Skewness
allsk <- round(skewness(alldata$Age),2)
allsk

# Summary
#summary(alldata$Age)
# Plot
d <- ggplot(data = alldata, aes(Age)) 
#d + geom_histogram() + theme_bw()
d + geom_histogram(aes(y = ..count..),binwidth = 0.5)  + 
    theme_bw() + 
   scale_x_continuous(breaks = c(allmean)) + 
    geom_vline(xintercept = allmean, size = 0.5, colour = "magenta") +
    geom_vline(xintercept = allmean+allsd, size = 0.5, colour = "blue", linetype = 2) +
    geom_vline(xintercept = allmean-allsd, size = 0.5, colour = "blue", linetype = 2) 
ggsave(file = "../figures/allage_hist.eps", width=5, height=5)

# Filtering by average data ( 10 <= Age <= 75 )
# Mean
agefilter <- subset(users, users$Age >= 10 & Age < 75)
nonfakemean <- round(mean(agefilter$Age),2)
# Median
nonfakemedian <- median(agefilter$Age)
# Mode
nonfakemode <- Mode(agefilter$Age)
# Range
nonfakerange <- Rng(agefilter$Age)
# Quantiles
nonfakeq <- Quantiles(sort(agefilter$Age))
#nonfakeq
# Variance
nonfakevariance <- var(agefilter$Age)
# Standard deviation
nonfakesd <- sd(agefilter$Age)
# Kurtosis
kurtnonfake <- round(kurtosis(agefilter$Age),2)
# Skewness
sknonfake <- round(skewness(agefilter$Age),2)
sknonfake

# Summary
#summary(agefilter$Age)
d <- ggplot(data = agefilter, aes(Age)) 
d + geom_histogram(aes(y = ..count..),binwidth = 0.5) + 
    theme_bw() + 
    scale_x_continuous(breaks = c(allmean)) + 
    geom_vline(xintercept = nonfakemean, size = 0.5, colour = "magenta") +
    geom_vline(xintercept = nonfakemean+nonfakesd, size = 0.5, colour = "blue", linetype = 2) +
    geom_vline(xintercept = nonfakemean-nonfakesd, size = 0.5, colour = "blue", linetype = 2) 

ggsave(file = "../figures/filtage_hist.eps", width=5, height=5)


## Boxplot with filtered data
#d <- ggplot(data = agefilter, aes(x = UserID, y = Age)) 
#d + geom_boxplot()
#ggsave(file = "../figures/boxplotfilt.png")

############# fake data
# Filtering by fake data ( Age < 10 )| Age >= 75
fakeydata <- subset(users, Age < 10)
fakeymean <- round(mean(fakeydata$Age),2)
fakeodata <- subset(users, Age >= 75)
fakeomean <- round(mean(fakeodata$Age),2)
# Median
fakeymedian <- median(fakeydata$Age)
fakeomedian <- median(fakeodata$Age)
# Mode
fakeymode <- Mode(fakeydata$Age)
fakeomode <- Mode(fakeodata$Age)
# Range
fakeyrange <- Rng(fakeydata$Age)
fakeorange <- Rng(fakeodata$Age)
#############

## Building results file
o1frame <- data.frame(mean=allmean,median=allmedian,mode=allmode,range=allrange,q1=allq[1],q2=allq[2],q3=allq[3],q4=allq[4],variance=allvariance,sd=allsd,allkurtosis,allsk)
write.csv(o1frame, row.names = FALSE, quote = FALSE, file="../data/allage_data.csv")
o1frame
o2frame <- data.frame(mean=nonfakemean,median=nonfakemedian,mode=nonfakemode,range=nonfakerange,q1=nonfakeq[1][1],q2=nonfakeq[2],q3=nonfakeq[3],q4=nonfakeq[4],variance=nonfakevariance,sd=nonfakesd,kurtnonfake,sknonfake)
write.csv(o2frame, row.names = FALSE, quote = FALSE, file="../data/nonfakeage_data.csv")

o3frame <- data.frame(fakeymean,fakeomean,fakeymode,fakeomode)
write.csv(o3frame, row.names = FALSE, quote = FALSE, file="../data/fakeage_data.csv")

dev.off()
q(status=1)

