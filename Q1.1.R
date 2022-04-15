install.packages("readr")
library(readr)
library(lattice)

cd <- read_tsv("clinical-data.tsv") 
spec(cd)
summary(cd)

##For each variable, compute histogram, count and histogram of relative frequencies##


##SEX##
barplot(prop.table(table(data.frame(cd$SEX))), main="sex",name=c("male","female"))

##AGE

age <- data.frame(cd$AGE)
age <- age[,1]
hist_age <- hist(age)
hist_age$counts
histogram(age,breaks=6)

##HGHT##

hght <- data.frame(cd$HGHT)
hght <- hght[,1]
hist_hght <- hist(hght)
hist_hght$counts
histogram(hght, breaks=9)

##WGHT##

wght <- data.frame(cd$WGHT)
wght <- wght[,1]
hist_wght <- hist(wght)
hist_wght$counts
histogram(wght, breaks=9)

##BMI##

bmi <- data.frame(cd$BMI)
bmi <- bmi[,1]
hist_bmi <- hist(bmi)
hist_bmi$counts
histogram(bmi)

##COHORT##

cohort <- data.frame(cd$COHORT)
table(cohort)
barplot(100*prop.table(table(cohort)))

##TRISCHD (ischemic time)##
trischd <- data.frame(cd$TRISCHD)
trischd <- trischd[,1]
hist_trischd <- hist(trischd)
hist_trischd$counts
histogram(trischd)

##DTHHRDY (hardy scale)##
dthhrdy <- data.frame(cd$DTHHRDY)
barplot(100*prop.table(table(dthhrdy)))


