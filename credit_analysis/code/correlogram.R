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
# Plot age rating
d <- ggplot(data = filter, aes(age,BookRating)) 
d + geom_point(alpha = 1/3, size = 3.5)  + 
    scale_y_continuous(breaks = seq(0, 10, by = 1)) +
    theme_bw() 
#ggsave(file = "../figures/agerating1.jpg", width=5, height=5)

d <- ggplot(data = filter, aes(age,BookRating)) 
d + geom_point(aes(colour = yearPub))  + 
    scale_y_continuous(breaks = seq(0, 10, by = 1)) +
    theme_bw() 
#ggsave(file = "../figures/agerating2.jpg", width=5, height=5)

# Plot yearPub rating
d <- ggplot(data = filter, aes(yearPub,BookRating)) 
d + geom_point(alpha = 1/3, size = 3.5)  + 
    scale_y_continuous(breaks = seq(0, 10, by = 1)) +
    theme_bw() 
#ggsave(file = "../figures/yearating1.jpg", width=5, height=5)

d <- ggplot(data = filter, aes(yearPub,BookRating)) 
d + geom_point(aes(colour = age))  + 
    scale_y_continuous(breaks = seq(0, 10, by = 1)) +
    theme_bw() 
#ggsave(file = "../figures/yearating2.jpg", width=5, height=5)

# Plot age yearPub
d <- ggplot(data = filter, aes(age,yearPub)) 
d + geom_point(alpha = 1/3, size = 3.5)  + 
   theme_bw() 
#ggsave(file = "../figures/ayeyear1.jpg", width=5, height=5)

d <- ggplot(data = filter, aes(age,yearPub)) 
d + geom_point(aes(colour = BookRating))  + 
   theme_bw() 
#ggsave(file = "../figures/ayeyear2.jpg", width=5, height=5)

# Plot correlation
# http://rforge.org/category/use-r/page/2/
# http://theatavism.blogspot.com/2009/05/plotting-correlation-matrix-with.html
p <- ggplot(selcols, aes(age, yearPub, fill=BookRating)) 
p + theme_bw() + geom_tile()
#ggsave(file = "../figures/corrtile.eps", width=5, height=5)
x11()
d <- corrgram(selcols, order=TRUE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main="Test1") 
savePlot(filename="../figures/corr1.png",type="png")
#postscript(file="../figures/corr1.eps")

d <- corrgram(selcols, order=TRUE, lower.panel=panel.ellipse,
  upper.panel=panel.pts, text.panel=panel.txt,
  diag.panel=panel.minmax,
  main="Test2") 
savePlot(filename="../figures/corr2.png",type="png")
#postscript(file="../figures/corr2.eps")

d <- corrgram(selcols, order=NULL, lower.panel=panel.shade,
  upper.panel=NULL, text.panel=panel.txt,
  main="Test3")
savePlot(filename="../figures/corr3.png",type="png")
#postscript(file="../figures/corr3.eps")
dev.off()

# Exploratory analysis: count
rawrow <- nrow(uir)   # raw row count
rawrow 
rawcol <- ncol(uir)
dsrow <- nrow(dsmall)
dsrow
frow <- nrow(filter)
frow

#*** hacer la correlación a filter y graficar como en 
# http://theatavism.blogspot.com/2009/05/plotting-correlation-matrix-with.html
# http://www.statmethods.net/advgraphs/correlograms.html

# cor(uir$age,uir$BookRating)
# cor(dsmall,use = "na.or.complete",method = "pearson")

#dsageyrating <- dsmall[c(2,4:5)] #select age,yearPub,BookRating
#cor(dsageyrating,use = "na.or.complete",method = "pearson")
#cor(dsageyrating,use = "na.or.complete",method = "spearman")
#cor(dsageyrating,use = "na.or.complete",method = "kendall")

#ageyrating <- uir[c(2,4:5)] #select age,yearPub,BookRating
#cor(ageyrating,use = "na.or.complete",method = "pearson")
#cor(ageyrating,use = "na.or.complete",method = "spearman")
#cor(ageyrating,use = "na.or.complete",method = "kendall")


######################
Mode <- function (x) {
    cngtable <- table(x)
    n <- length(cngtable)
    mode <- as.double(names(sort(cngtable)[n]))
    mode
}
######################


## Building results file
#o1frame <- data.frame(mean=allmean,median=allmedian,mode=allmode,range=allrange,q1=allq[1],q2=allq[2],q3=allq[3],q4=allq[4],variance=allvariance,sd=allsd,allkurtosis,allsk)
#write.csv(o1frame, row.names = FALSE, quote = FALSE, file="../data/allage_data.csv")
#o1frame


#dev.off()
q(status=1)

