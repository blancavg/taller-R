#! /usr/bin/Rscript --vanilla
############################################
### File:	p03_biv.R
### Author:	Blanca A. Vargas-Govea
### Email:	blanca.vg@gmail.com
### Data:	myfile.csv
############################################
# Exploratory analysis: mean, median, mode, quantile, range, variance, sd, coefficient of deviation, skewness, kurtosis

# correlation
# http://www.statmethods.net/stats/index.html
# http://www.inside-r.org/r-doc/stats/cor
# t-test
#http://www.wekaleamstudios.co.uk/posts/one-and-two-sample-hypothesis-testing/#more-244
# chi-square
# http://www.gardenersown.co.uk/Education/Lectures/R/basics.htm
# anova
# http://www.r-bloggers.com/r-tutorial-series-two-way-anova-with-pairwise-comparisons/

#Plot
#http://www.ling.upenn.edu/~joseff/rstudy/summer2010_ggplot2_intro.html

library(ggplot2)
library(proto)
library(corrgram)
#x11()
uir <- read.csv(file = "../data/myfile.csv", sep = ",", na.strings = "NULL")
#"userID","age","ISBN","yearPub","BookRating"

set.seed(3149) # 10% Make the sample reproducible
dsmall <- uir[sample(nrow(uir), 2000), ]

filter <- subset(dsmall, age >= 10 & age < 75 & yearPub >= 1950 & BookRating > 0)

selcols <- filter[c(2,4:5)] #only the numeric cols

#cor(filter$age,filter$BookRating,use = "na.or.complete",method = "pearson")
cor(filter$age,filter$BookRating,use = "na.or.complete",method = "spearman")

#cor(filter$yearPub,filter$BookRating,use = "na.or.complete",method = "pearson")
cor(filter$yearPub,filter$BookRating,use = "na.or.complete",method = "spearman")
#cor(selcols,use = "na.or.complete",method = "pearson")
cor(selcols,use = "na.or.complete",method = "spearman")

cor.test(filter$age, filter$BookRating, method = "spearman")

#x11()
#plot(filter$age, filter$BookRating)
#abline(lm(filter$BookRating ~ filter$age))
#savePlot(filename="../figures/corr_bas1.png",type="png")

#plot(filter$yearPub, filter$BookRating)
#abline(lm(filter$BookRating ~ filter$yearPub))
#savePlot(filename="../figures/corr_bas2.png",type="png")
#dev.off()

#http://www.statmethods.net/stats/index.html
#mytable <- table(filter$age, filter$BookRating)
#mytable

mychis <- chisq.test(selcols)
mychis$p.value

myttest <- t.test(selcols$age, selcols$BookRating)
myttest$p.value
myttest$conf.int

my.aov = aov(selcols$BookRating ~ selcols$age)
my.aov
summary(my.aov)

my.aov = aov(selcols$BookRating ~ selcols$age+selcols$yearPub)
my.aov
summary(my.aov)


# 2x2 Factorial MANOVA with 3 Dependent Variables.
#Y <- cbind(selcols$age,selcols$yearPub,selcols$BookRating)
#fit <- manova(Y ~ A*B)
#summary(fit, test="Pillai")
Y <- cbind(selcols$age,selcols$yearPub,selcols$BookRating)
fac1 <- factor(selcols$age)
gen <- factor(gl(1,2))
m1 <- manova(Y ~ gen)
summary.aov(m1)
summary(m1)
######################


## Building results file
#o1frame <- data.frame(mean=allmean,median=allmedian,mode=allmode,range=allrange,q1=allq[1],q2=allq[2],q3=allq[3],q4=allq[4],variance=allvariance,sd=allsd,allkurtosis,allsk)
#write.csv(o1frame, row.names = FALSE, quote = FALSE, file="../data/allage_data.csv")
#o1frame


#dev.off()
q(status=1)

