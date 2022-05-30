library(readr)
library(tigerstats)
library(ggplot2)
library(gridExtra)

cd<-read_tsv("clinical-data.tsv") 

colors<-rep("black", 136)
colors[cd$COHORT=="Postmortem"] <- "red"

shapes = c(1, 3) 
shapes<-shapes[as.numeric(cd$SEX)]

## Noir = Organ Donor, Rouge = Postmortem
## Rond = Homme, Croix = Femme

q1<-plot(cd$AGE, cd$HGHT, col=colors, pch=shapes)
q2<-plot(cd$AGE, cd$WGHT, col=colors, pch=shapes)
q3<-plot(cd$AGE, cd$BMI, col=colors, pch=shapes)
q4<-plot(cd$AGE, cd$TRISCHD, col=colors, pch=shapes)
q5<-plot(cd$HGHT, cd$WGHT, col=colors, pch=shapes)
q6<-plot(cd$HGHT, cd$BMI, col=colors, pch=shapes)
q7<-plot(cd$HGHT, cd$TRISCHD, col=colors, pch=shapes)
q8<-plot(cd$WGHT, cd$BMI, col=colors, pch=shapes)
q9<-plot(cd$WGHT, cd$TRISCHD, col=colors, pch=shapes)
q10<-plot(cd$BMI, cd$TRISCHD, col=colors, pch=shapes)

g<-grid.arrange(grob(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10), ncol=5)

## Le temps ischemique et la cohorte sont de potentielles "confounding" values

clrs = c("black","red","blue","purple","green")
clrs<-clrs[as.numeric(cd$DTHHRDY)]

shapes = c(1, 3) 
shapes<-shapes[as.numeric(cd$SEX)]

## Couleur f° echelle de Hardy
## Rond = Homme, Croix = Femme

s1<-plot(cd$AGE, cd$HGHT, col=clrs, pch=shapes)
s2<-plot(cd$AGE, cd$WGHT, col=clrs, pch=shapes)
s3<-plot(cd$AGE, cd$BMI, col=clrs, pch=shapes)
s4<-plot(cd$AGE, cd$TRISCHD, col=clrs, pch=shapes)
s5<-plot(cd$HGHT, cd$WGHT, col=clrs, pch=shapes)
s6<-plot(cd$HGHT, cd$BMI, col=clrs, pch=shapes)
s7<-plot(cd$HGHT, cd$TRISCHD, col=clrs, pch=shapes)
s8<-plot(cd$WGHT, cd$BMI, col=clrs, pch=shapes)
s9<-plot(cd$WGHT, cd$TRISCHD, col=clrs, pch=shapes)
s10<-plot(cd$BMI, cd$TRISCHD, col=clrs, pch=shapes)

h<-grid.arrange(grob(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10), ncol=5)