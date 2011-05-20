#! /usr/bin/Rscript --vanilla
############################################
### File:	taller1.R
### Author:	Blanca A. Vargas-Govea
### Email:	blanca.vg@gmail.com
############################################
library(ggplot2)
library(proto)

#"UserID"   "Location" "Age" 
set.seed(1410) # Make the sample reproducible
dsmall <- users[sample(nrow(users), 30), ]

# Exploratory analysis: count
rawrow <- nrow(users)   # raw row count
rawcol <- ncol(users)

# Min and Max values
oldmin <- min(users$Age,na.rm = TRUE) # min without NULLS in Age
oldmax <- max(users$Age,na.rm = TRUE)
boxout <- min(boxplot(users$Age)$out)
boxout
d <- ggplot(data = users, aes(x = UserID, y = Age)) 
d + geom_boxplot(outlier.shape = 4) + theme_bw() + scale_y_continuous(breaks = seq(0, 244, by = 10))

ggsave(file = "../figures/boxplot.eps", width=5, height=5)

# Filtering by all age data ( oldmin < Age < oldmax )
alldata <- subset(users, Age >= oldmin  & Age <= oldmax)
nalldata <- nrow(alldata)
d <- ggplot(data = alldata, aes(Age)) 
d + geom_bar() + theme_bw()
#d + stat_bin(aes(ymax = ..count..), geom = "bar")
ggsave(file = "../figures/allage_bar.eps", width=5, height=5)

# Filtering by fake data ( 75 <= Age <= 10 )
fakedata <- subset(users, Age < 10 | Age >= 75 )
nfake <- nrow(fakedata)
d <- ggplot(data = fakedata, aes(Age)) 
d + stat_bin(aes(ymax = ..count..), geom = "bar") + theme_bw()
ggsave(file = "../figures/fakeage_bar.eps", width=5, height=5)

# Filtering by average data ( 10 <= Age <= 75 )
#mean(height.survey, na.rm=TRUE)
agefilter <- subset(users, users$Age >= 10 & Age < 75)
newrow <- nrow(agefilter)
newmin <- min(agefilter$Age,na.rm = TRUE)
newmax <- max(agefilter$Age,na.rm = TRUE)
write.csv(agefilter, row.names = FALSE, quote = FALSE, file="../data/agefilter.csv")
d <- ggplot(data = agefilter, aes(Age)) 
d + stat_bin(aes(ymax = ..count..), geom = "bar") + theme_bw()
ggsave(file = "../figures/age_bar.eps", width=5, height=5)
# Boxplot with filtered data
d <- ggplot(data = agefilter, aes(x = UserID, y = Age)) 
d + geom_boxplot(outlier.shape = 4) + theme_bw() + scale_y_continuous(breaks = seq(0, 75, by = 10))
ggsave(file = "../figures/boxplotfilt.eps", width=5, height=5)

# Filtering by NULL - missing age
nullds <- subset(users, is.na(users$Age))
nullrows <- nrow(nullds)
d <- ggplot(data = users, aes(is.na(users$Age))) 
d + stat_bin(aes(ymax = ..count..),geom = "bar") + theme_bw()
ggsave(file = "../figures/nullage_bar.eps", width=5, height=5)

# Filtering by bins ( 10 <= Age < 75 )
Agebin = cut(agefilter$Age,breaks = c(0,10,18,60,75))
agebinfile <- data.frame(agefilter$UserID, agefilter$Location, Age = Agebin)
d <- ggplot(data = agebinfile, aes(Age)) 
d + stat_bin(aes(ymax = ..count..), geom = "bar") + theme_bw()
ggsave(file = "../figures/agebin_bar.eps", width=5, height=5)

# Building results file
output = c(rawrow,nalldata,nfake,nullrows,newrow,oldmin,newmin,oldmax,newmax)
oframe = data.frame(raw=rawrow,nall=nalldata,fake=nfake,null=nullrows,newn=newrow,oldmin,newmin,oldmax,newmax)
write.csv(oframe, row.names = FALSE, quote = FALSE, file="../data/output01_data.csv")

dev.off()
q(status=1)

