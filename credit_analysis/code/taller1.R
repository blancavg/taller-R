#! /usr/bin/Rscript 
############################################
### File:  taller1.R General, conteo, subconjuntos, muestras
### Author:	Blanca A. Vargas-Govea
### Email:	blanca.vg@gmail.com
############################################
#X11()
setwd("/home/blanca/laestancia/presentaciones/tallerR/credit_analysis/code")

# Tipos de datos: números, cadenas, dataframes
var1 <- 54
var1

var2 <- sqrt(var1*8)
var2

vector <- c(1,2,3,4,5)
vector[0]
vector[1]

cadena <- "uno"
cadena

lcadena <- c("casa","manzana","uva")
lcadena

vlogico <- c(TRUE,FALSE,TRUE,TRUE,FALSE)
vlogico

# Dataframes
c1 <- c(25,26,27,28)
c2 <- c("Ana", "Lola", "Luis", "Pedro")
c3 <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(c1,c2,c3)
names(mydata) <- c("ID","Nombre","Aprobado") # nombres de variables

# Leer del archivo
credit <- read.csv(file = "../data/credit-g.csv", sep = ",", na.strings = "NULL")


# Visualizando los datos
ls()  # lista de objetos
names(credit) # variables de credit
str(credit) # estructura de credit
levels(credit$foreign_worker) # niveles(valores de variable)
dim(credit) # dimensiones (ren x cols)
class(credit) # clase del objeto
credit  # objeto
head(credit, n=10)
tail(credit, n=10)
credit$purpose
#purpose
attach(credit)  # coloca la bd en el path
purpose
#detach(credit)  # elimina la bd del path
#purpose
#rm(credit)
#credit <- read.csv(file = "../data/credit-g.csv", sep = ",", na.strings = "NULL")
#attach(credit)

# Explorando conteo
ren <- nrow(credit)   # raw row count
col <- ncol(credit)

# Min y Max 
agemin <- min(age,na.rm = TRUE) # min without NULLS in Age
agemax <- max(age,na.rm = TRUE)

ammin <- min(credit_amount,na.rm = TRUE) # min without NULLS in Age
ammax <- max(credit_amount,na.rm = TRUE)

# Subconjuntos
# Filtering by all age data ( oldmin < Age < oldmax )
mayor30 <- subset(credit, age >= 30)
renmayor30 <- nrow(mayor30)
renmayor30

en20y40 <- subset(credit, age >= 20  & age <= 40)
ren20y40 <- nrow(en20y40)
ren20y40

# Muestra
set.seed(32) # Make the sample reproducible
dsmall <- credit[sample(nrow(credit), 10), ]
dsmall

# Filtering by bins 
agebin = cut(age,breaks = c(18,30,40,50,60,70,80))
agebinfile <- data.frame(purpose,credit_amount,personal_status,housing,job,age=agebin,class)
agebinfile

# Building results file
oframe = data.frame(purpose,credit_amount,personal_status,housing,job,age=agebin,class)
write.table(oframe, row.names = FALSE, sep = ";",quote = FALSE, file="../data/output01_data.csv")

# unicos
unicos <- unique(class)
unicos
nhousing <- length(unique(class))
nhousing

# ordenar
mycredit <- table(dsmall$housing)
mycredit
mylist <- sort(mycredit,decreasing = TRUE)
mylist

# table
mycredit <- table(dsmall$class)
mycredit
mycredit <- table(dsmall$age,dsmall$credit_amount)
mycredit
