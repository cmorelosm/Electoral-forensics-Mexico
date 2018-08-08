# Electoral-forensics
This project aims to evaluate if the results of the 2012 and 2018 presidential elections in Mexico suggest the presence of electoral manipulation at a state level.

##First we get the 2012 Mexican electoral data
#rm(list=ls())
getwd()
setwd("F:/Warwick/Project")
ndata <- read.csv("Presidente2012.csv")


##indexing the spetial pollstations with their max. num of votes
##750 is stated in the law to be the maximum
for (i in 1:length(ndata$LISTA_NOMINAL)){
  if(ndata$LISTA_NOMINAL[i] == 0){
    ndata$LISTA_NOMINAL[i]=750
  }
}

##take away poll stations with zero votes
ndata<-ndata[!(ndata$TOTAL_VOTOS==0),]

##create turnout and the final votes for each candidate (Coalitions)

## to create the PRI variable (adding the coalition votes)
ndata$newcolumn <- ndata$PRI+ ndata$PRI_PVEM + ndata$PVEM
colnames(ndata) <- c("cir", "id.estado", "estado", "id.distrito", "cabecera", "id.mun", "mun", "seccion", "casilla", "PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "numvotreg", "votosnulos", "votos", "listanominal", "ESTATUS_ACTA", "TEPJF", "OBSERVACIONES",  "RUTA_ACTA", "pri")

## to create the PRD variable (adding the coalition votes)
ndata$newcolumn <- ndata$PRD + ndata$PRD_MC + ndata$PRD_PT + ndata$PRD_PT_MC + ndata$PT + ndata$PT_MC + ndata$MC
colnames(ndata) <- c("cir", "id.estado", "estado", "id.distrito", "cabecera", "id.mun", "mun", "seccion", "casilla", "PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "PANAL", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "numvotreg", "votosnulos", "votos", "listanominal", "ESTATUS_ACTA", "TEPJF", "OBSERVACIONES",  "RUTA_ACTA", "pri", "prd")

## to create the turnout variable 
ndata$newcolumn<- (ndata$votos/ndata$listanominal)*100
colnames(ndata) <- c("cir", "id.estado", "estado", "id.distrito", "cabecera", "id.mun", "mun", "seccion", "casilla", "PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "PANAL", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "numvotreg", "nullvotes", "t", "N", "ESTATUS_ACTA", "TEPJF", "OBSERVACIONES",  "RUTA_ACTA", "v", "prd", "turnout")

## to create Absolute support for PRI variable 
ndata$newcolumn<- (ndata$v/ndata$N)*100
colnames(ndata) <- c("cir", "id.estado", "estado", "id.distrito", "cabecera", "id.mun", "mun", "seccion", "casilla", "PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "PANAL", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "numvotreg", "nullvotes", "t", "N", "ESTATUS_ACTA", "TEPJF", "OBSERVACIONES",  "RUTA_ACTA", "v", "prd", "turnout", "AbsPRI")

## to create Absolute support for PRD variable 
ndata$newcolumn<- (ndata$prd/ndata$N)*100
colnames(ndata) <- c("cir", "id.estado", "estado", "id.distrito", "cabecera", "id.mun", "mun", "seccion", "casilla", "PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "PANAL", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "numvotreg", "nullvotes", "t", "N", "ESTATUS_ACTA", "TEPJF", "OBSERVACIONES",  "RUTA_ACTA", "v", "prd", "turnout", "AbsPRI","AbsPRD")

## to create Absolute support for PAN variable 
ndata$newcolumn<- (ndata$PAN/ndata$N)*100
colnames(ndata) <- c("cir", "id.estado", "estado", "id.distrito", "cabecera", "id.mun", "mun", "seccion", "casilla", "PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "PANAL", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "numvotreg", "nullvotes", "t", "N", "ESTATUS_ACTA", "TEPJF", "OBSERVACIONES",  "RUTA_ACTA", "v", "prd", "turnout", "AbsPRI","AbsPRD","AbsPAN")

## to create Absolute support for PANAL variable 
ndata$newcolumn<- (ndata$PANAL/ndata$N)*100
colnames(ndata) <- c("cir", "id.estado", "estado", "id.distrito", "cabecera", "id.mun", "mun", "seccion", "casilla", "PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "PANAL", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "numvotreg", "nullvotes", "t", "N", "ESTATUS_ACTA", "TEPJF", "OBSERVACIONES",  "RUTA_ACTA", "v", "prd", "turnout", "AbsPRI","AbsPRD","AbsPAN", "AbsPANAL")

## to create Relative support for PRI variable 
ndata$newcolumn<- (ndata$v/ndata$t)*100
colnames(ndata) <- c("cir", "id.estado", "estado", "id.distrito", "cabecera", "id.mun", "mun", "seccion", "casilla", "PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "PANAL", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "numvotreg", "nullvotes", "t", "N", "ESTATUS_ACTA", "TEPJF", "OBSERVACIONES",  "RUTA_ACTA", "v", "prd", "turnout",  "AbsPRI","AbsPRD","AbsPAN", "AbsPANAL","RelaPRI") 

## to create Relative support for PRD variable
ndata$newcolumn<- (ndata$prd/ndata$t)*100
colnames(ndata) <- c("cir", "id.estado", "estado", "id.distrito", "cabecera", "id.mun", "mun", "seccion", "casilla", "PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "PANAL", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "numvotreg", "nullvotes", "t", "N", "ESTATUS_ACTA", "TEPJF", "OBSERVACIONES",  "RUTA_ACTA", "v", "prd", "turnout", "AbsPRI","AbsPRD","AbsPAN", "AbsPANAL","RelaPRI","RelaPRD")

## to create Relative support for PAN variable
ndata$newcolumn<- (ndata$PAN/ndata$t)*100
colnames(ndata) <- c("cir", "id.estado", "estado", "id.distrito", "cabecera", "id.mun", "mun", "seccion", "casilla", "PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "PANAL", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "numvotreg", "nullvotes", "t", "N", "ESTATUS_ACTA", "TEPJF", "OBSERVACIONES",  "RUTA_ACTA", "v", "prd", "turnout", "AbsPRI","AbsPRD","AbsPAN", "AbsPANAL","RelaPRI","RelaPRD","RelaPAN")

## to create Relative support for PANAL variable
ndata$newcolumn<- (ndata$PANAL/ndata$t)*100
colnames(ndata) <- c("cir", "id.estado", "estado", "id.distrito", "cabecera", "id.mun", "mun", "seccion", "casilla", "PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "PANAL", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "numvotreg", "nullvotes", "t", "N", "ESTATUS_ACTA", "TEPJF", "OBSERVACIONES",  "RUTA_ACTA", "v", "prd", "turnout", "AbsPRI","AbsPRD","AbsPAN", "AbsPANAL","RelaPRI","RelaPRD","RelaPAN","RelaPANAL")

##N -> Nominal list
##t-> Total votes
##V-> votes for pri (they won the election)


## to create a dataset with the total votes for each coalition and Absolute and Relative percentages of support
myvars <- names(ndata) %in% c("TEPJF", "OBSERVACIONES", "RUTA_ACTA", "ESTATUS_ACTA", "cir", "id.distrito","PVEM", "PT" , "MC", "PRI", "PRD", "PVEM", "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC")
ndata0 <- ndata[!myvars]

## to create a dataset with just the total votes for each coalition
myvars <- names(ndata0) %in% c("AbsPRI","AbsPRD","AbsPAN", "AbsPANAL","RelaPRI","RelaPRD","RelaPAN","RelaPANAL") 
ndata1 <- ndata0[!myvars]


myvars <- names(ndata0) %in% c("cir", "id.estado", "estado", "id.distrito", "cabecera", "id.mun", "mun", "seccion", "casilla", "PRI", "PRD", "PVEM", "PT" , "MC",  "PRI_PVEM", "PRD_PT_MC", "PRD_PT", "PRD_MC", "PT_MC", "numvotreg", "nullvotes", "ESTATUS_ACTA", "TEPJF", "OBSERVACIONES",  "RUTA_ACTA") 
ndata2 <- ndata0[!myvars]

## to create a dataset without the poll stations with more than 100% of turnout
ndata3<-ndata2[!(ndata2$turnout>100),]

## to create a dataset with the poll stations with more than 100% of turnout
ndata4<-ndata1[!(ndata1$turnout<=100),]

ndata5<-ndata1[!(ndata1$turnout>100),]

##General data
##To create the sum of votes for each party and nullvotes at a National Level
vpan <- sum(ndata$PAN)
vpri <- sum(ndata$v)
vprd <- sum(ndata$prd)
vpanal <- sum(ndata$PANAL)
vothers <- sum(ndata$numvotreg)
vnull <- sum(ndata$nullvotes)
ListNom <-sum(ndata$N)
TotalVotes <- sum(ndata$t) 

##To create the Absolute support values at a National Level
Abs.pri<- vpri/ListNom
Abs.prd<- vprd/ListNom
Abs.pan<- vpan/ListNom
Abs.panal<- vpanal/ListNom

##To create the Relative support values at a National Level
Rel.pri<- vpri/TotalVotes
Rel.prd<- vprd/TotalVotes
Rel.pan<- vpan/TotalVotes
Rel.panal<- vpanal/TotalVotes

##To look at the density distribution of Absolute 
##support values at a National Level for the first and second places in the election
summary(ndata$AbsPRI)
plot(density(ndata$AbsPRI))
plot(density(ndata$AbsPRD))

### Analysis of absolute support can provide information as to the type of fraud
##employed in an electoral process. If vote manipulation comes in the form of vote inflation, the absolute support
##histogram for the perpetrator will shift to the right 

library("dplyr")
library("ggpubr")

ggdensity(ndata$turnout, 
          main = "2012 Turnout",
          xlab = "Turnout")


ggdensity(ndata$PAN, 
          main = "2012 PAN's votes",
          xlab = "Votes for PAN")

ggdensity(ndata$v, 
          main = "2012 PRI's votes",
          xlab = "Votes for PRI")



###by state
data.edo<-list()
index.edo<-c(1:32)
for (i in 1:32){
  data.edo[[i]] <- ndata0[ndata0$id.estado==index.edo[i],c(8:24)]
}

ggdensity( data.edo[[1]]$v, 
           main = "Aguascalientes: 2012 PRI's votes",
           xlab = "PRI's Votes")

ggdensity( data.edo[[2]]$v, 
           main = "Baja California: 2012 PRI's votes",
           xlab = "PRI's Votes")




ggdensity( data.edo[[1]]$turnout, 
           main = "Aguascalientes: 2012 Turnout",
           xlab = "Turnout")
ggdensity( data.edo[[2]]$turnout, 
           main = "Baja California: 2012 Turnout",
           xlab = "Turnout")



#####################################
#####################################
#####################################
#####################################
#####################################



## Perform the test to compare distributions (turnout and votes for each party) with normal distribution
##to test if word occurrences in a set of documents follows a Normal distribution you can just use a density test and some qqplots

###to look at the turnout levels

summary(ndata1$turnout)
boxplot(ndata1$turnout, horizontal=TRUE, main="National Turnout, 2012")
boxplot(ndata1$v, horizontal=TRUE, main="PRI´s votes, 2012")
boxplot(ndata1$PAN, horizontal=TRUE, main="PAN´s votes, 2012")
boxplot(ndata1$prd, horizontal=TRUE, main="PRD´s votes, 2012")
boxplot(ndata1$PANAL, horizontal=TRUE, main="PANAL´s votes, 2012")
boxplot(ndata$AbsPRI, horizontal=TRUE, main="Absolute support for PRI, 2012")
boxplot(ndata$AbsPAN, horizontal=TRUE, main="Absolute support for PAN, 2012")
boxplot(ndata$AbsPANAL, horizontal=TRUE, main="Absolute support for PANAL, 2012")
boxplot(ndata$AbsPRD, horizontal=TRUE, main="Absolute support for PRD, 2012")

boxplot(data.edo[[1]]$v, horizontal=TRUE, main="Aguascalientes PRI´s votes, 2012")
boxplot(data.edo[[2]]$v, horizontal=TRUE, main="Baja California PRI´s votes, 2012")
.
.
.



kernelest1<-density(ndata1$turnout, bw = "nrd0", adjust = 1,
                    kernel = c("gaussian"),
                    weights = NULL, window = kernel, width,
                    give.Rkern = FALSE,
                    n = 513,  na.rm = TRUE) 
plot(kernelest1,main = "Estimated Kernel density: National Turnout 2012")


library(ggplot2)
##plot Absolute PRI and PRD Support 
qplot(ndata$AbsPRI, geom = 'histogram', binwidth = 0.001, main = 'Absolute Support for PRI, 2012', xlim=c(0,100), ylim=c(0,20)) + xlab('Absolute Support PRI')
qplot(ndata$AbsPRD, geom = 'histogram', binwidth = 0.001, main = 'Absolute Support for PRD, 2012', xlim=c(0,100), ylim=c(0,20)) + xlab('Absolute Support PRD')
qplot(ndata$AbsPAN, geom = 'histogram', binwidth = 0.001, main = 'Absolute Support for PAN, 2012', xlim=c(0,100), ylim=c(0,20)) + xlab('Absolute Support PAN')
qplot(ndata$AbsPANAL, geom = 'histogram', binwidth = 0.001, main = 'Absolute Support for PANAL, 2012', xlim=c(0,100), ylim=c(0,20)) + xlab('Absolute Support PANAL')
qplot(ndata0$v, geom = 'histogram', binwidth = 0.001, main = 'PRI´s votes, 2012', xlim=c(0,100), ylim=c(0,20)) + xlab('PRI´s votes')
qplot(ndata0$PAN, geom = 'histogram', binwidth = 0.001, main = 'PAN´s votes, 2012', xlim=c(0,100), ylim=c(0,20)) + xlab('PAN´s votes')
qplot(ndata0$prd, geom = 'histogram', binwidth = 0.001, main = 'PRD´s votes, 2012', xlim=c(0,100), ylim=c(0,20)) + xlab('PRD´s votes')
qplot(ndata0$PANAL, geom = 'histogram', binwidth = 0.001, main = 'PANAL´svotes, 2012', xlim=c(0,100), ylim=c(0,20)) + xlab('PANAL´s votes')


qplot(ndata$AbsPRD, geom = 'histogram', binwidth = 0.001, main = 'AbsoluteVote PRI', xlim=c(0,100), ylim=c(0,20)) + xlab('AbsoluteVote PRI')


##plot  National turnout
qplot(ndata1$turnout, geom = 'histogram', binwidth = 0.001, main = 'National turnout per poll station', xlim=c(0,100), ylim=c(0,20)) + xlab('National Turnout per poll station')
dataturnout<- data.frame(ndata1$turnout)
ggplot(dataturnout, aes(x = ndata1$turnout), binwidth = 0.01, bins= NULL, na.rm = FALSE, xlim=c(0,150), ylim=c(0,20)) + 
  geom_histogram(aes(y = ..density..),bins = 200, binwidth = 0.5, fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Turnout votes'))) + 
  ylab(expression(bold('Density')))

ggplot(dataturnout, aes(ndata1$turnout)) +
  geom_histogram(bins = 200)



## To analyze Skewness and Kurtosis for the data to look if the distribution behaves as normal
library(moments)
library(normtest)

##kurtosis is a measure of the "tailedness" of the probability distribution of a real-valued random variable
##H0: the distribution follows a normal distribution
##H1: the distribution follows a normal distribution
## p-value small. H0 is rejected.

kur.tot<- kurtosis.norm.test(ndata1$t)
kur.pri<- kurtosis.norm.test(ndata1$v)
kur.turnout<- kurtosis.norm.test(ndata1$turnout)
kur.prd<- kurtosis.norm.test(ndata1$prd)
kur.pan<- kurtosis.norm.test(ndata1$PAN)
kur.panal<- kurtosis.norm.test(ndata1$PANAL)

##skewness is a measure of symmetry (or the lack of it)
##H0: the distribution follows a normal distribution
##H1: the distribution follows a normal distribution
## p-value small. H0 is rejected.

skew.tot<- skewness.norm.test(ndata1$t)
skew.pri<- skewness.norm.test(ndata1$v)
skew.turnout<- skewness.norm.test(ndata1$turnout)
skew.prd<- skewness.norm.test(ndata1$prd)
skew.pan<- skewness.norm.test(ndata1$PAN)
skew.panal<- skewness.norm.test(ndata1$PANAL)

###to see boxplot of votes for each party
boxplot(ndata1$PAN, horizontal=TRUE, main="Votes for PAN")
hist(ndata1$PAN, breaks=15, main="Votes for PAN", 
     xlab = "Votes for PAN")
boxplot(ndata1$v, horizontal=TRUE, main="Votes for PRI")
hist(ndata1$v, breaks=15, main="Votes for PRI",xlab = "Votes for PRI")
boxplot(ndata1$prd, horizontal=TRUE, main="Votes for PRD")
hist(ndata1$prd, breaks=15, main="Votes for PRD", xlab = "Votes for PRD")
boxplot(ndata1$PANAL, horizontal=TRUE, main="Votes for PANAL")
hist(ndata1$PANAL, breaks=15, main="Votes for PANAL", xlab = "Votes for PANAL")



###############BendfordTest####################3

#the bendford test could be applied for the first, second and third digit of the data for each of the variables.

library(BenfordTests)
###chisq.benftest of PRI votes
chi.1.pri<- chisq.benftest(ndata1$v, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
chi.2.pri<-chisq.benftest(ndata1$v, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
chi.3.pri<-chisq.benftest(ndata1$v, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)

###chisq.benftest of total votes
chi.1.tot<-chisq.benftest(ndata1$t, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
chi.2.tot<-chisq.benftest(ndata1$t, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
chi.3.tot<-chisq.benftest(ndata1$t, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)

###chisq.benftest of PRD votes
chi.1.prd<-chisq.benftest(ndata1$prd, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
chi.2.prd<-chisq.benftest(ndata1$prd, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
chi.3.prd<-chisq.benftest(ndata1$prd, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)

###chisq.benftest of turnout
chi.1.turnout<-chisq.benftest(ndata1$turnout, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
chi.2.turnout<-chisq.benftest(ndata1$turnout, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
chi.3.turnout<-chisq.benftest(ndata1$turnout, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)

###chisq.benftest of PAN votes
chi.1.pan<-chisq.benftest(ndata1$PAN, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
chi.2.pan<-chisq.benftest(ndata1$PAN, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
chi.3.pan<-chisq.benftest(ndata1$PAN, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)

###chisq.benftest of PANAL votes
chi.1.panal<-chisq.benftest(ndata1$PANAL, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
chi.2.panal<-chisq.benftest(ndata1$PANAL, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
chi.3.panal<-chisq.benftest(ndata1$PANAL, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)



library(benford.analysis)
###benford test of PRI votes
benford(ndata1$v, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
bfd.mexpri <- benford(ndata3$v)
bfd.mexpri
plot(bfd.mexpri)

###benford test of total votes
benford(ndata1$t, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
bfd.mexvo <- benford(ndata3$t)
bfd.mexvo
plot(bfd.mexvo)

###benford test of PAN votes
bfd.mexpan <-benford(ndata3$PAN, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
plot(bfd.mexpan)
###benford test of PANAL votes
bfd.mexpanal <-benford(ndata3$PANAL, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
plot(bfd.mexpanal)
###benford test of PRD votes
bfd.mexprd <-benford(ndata3$prd, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
plot(bfd.mexprd)
###benford test of turnout 
bfd.mexturnout <-benford(ndata3$turnout, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
plot(bfd.mexturnout)



####################################################################
######################################################################
######################################################################

##Analysis per state

##to woork just with the 8th to 16th variable in the dataset ndata1 for each state.
#The objective is to work with less data and make the analysis easier for R
data.edo<-list()
index.edo<-c(1:32)
for (i in 1:32){
  data.edo[[i]] <- ndata1[ndata1$id.estado==index.edo[i],c(8:16)]
}

##Descriptives of data
###Total votes for PAN,  by state
vpan.edo <- list()
for (i in 1:32){
  vpan.edo[[i]] <- sum(data.edo[[i]]$PAN)
}

###Total votes for PRI,  by state
vpri.edo <- list()
for (i in 1:32){
  vpri.edo[[i]] <- sum(data.edo[[i]]$v)
}

###Total votes for PRD,  by state
vprd.edo <- list()
for (i in 1:32){
  vprd.edo[[i]] <- sum(data.edo[[i]]$prd)
}

###Total votes for PANAL,  by state
vpanal.edo <- list()
for (i in 1:32){
  vpanal.edo[[i]] <- sum(data.edo[[i]]$PANAL)
}

###Total votes for other non registred candidates,  by state
vothers.edo <- list()
for (i in 1:32){
  vothers.edo[[i]] <- sum(data.edo[[i]]$numvotreg)
}

###Total nullvotes for PAN,  by state
vnull.edo <- list()
for (i in 1:32){
  vnull.edo[[i]] <- sum(data.edo[[i]]$votosnulos)
}

###Total votes,  by state
votos.edo<-list()
for (i in 1:32){
  votos.edo[[i]]<- sum(data.edo[[i]]$t)
}

###Total posible votes,  by state
nominallist.edo<-list()
for (i in 1:32){
  nominallist.edo[[i]]<-sum(data.edo[[i]]$N)
}

###Turnout,  by state
turnout.edo<- list()
for (i in 1:32){
  #turnout.edo[[i]]<- (data.edo[[i]]$t)/(data.edo[[i]]$N)
  turnout.edo[[i]]<- votos.edo[[i]]/nominallist.edo[[i]]
}


###Absolute support value for PRI, per state
Abs.pri.edo.2<- list()
for (i in 1:32){
  Abs.pri.edo.2[[i]]<- (sum(vpri.edo[[i]])/sum(nominallist.edo[[i]]))*100
} 

###Absolute support value for PRD, per state
Abs.prd.edo.2<- list()
for (i in 1:32){
  Abs.prd.edo.2[[i]]<- (sum(vprd.edo[[i]])/sum(nominallist.edo[[i]]))*100
}

###Absolute support value for PAN, per state
Abs.pan.edo.2<- list()
for (i in 1:32){
  Abs.pan.edo.2[[i]]<- (sum(vpan.edo[[i]])/sum(nominallist.edo[[i]]))*100
}

###Absolute support value for PANAL, per state
Abs.panal.edo.2<- list()
for (i in 1:32){
  Abs.panal.edo.2[[i]]<- (sum(vpanal.edo[[i]])/sum(nominallist.edo[[i]]))*100
}


###Relative support value for PRI, per state
Rel.pri.edo.2<- list()
for (i in 1:32){
  Rel.pri.edo.2[[i]]<- (sum(vpri.edo[[i]])/sum(votos.edo[[i]]))*100
}

###Relative support value for PRD, per state
Rel.prd.edo.2<- list()
for (i in 1:32){
  Rel.prd.edo.2[[i]]<- (sum(vprd.edo[[i]])/sum(votos.edo[[i]]))*100
}

###Relative support value for PAN, per state
Rel.pan.edo.2<- list()
for (i in 1:32){
  Rel.pan.edo.2[[i]]<- (sum(vpan.edo[[i]])/sum(votos.edo[[i]]))*100
}


###Relative support value for PANAL, per state
Rel.panal.edo.2<- list()
for (i in 1:32){
  Rel.panal.edo.2[[i]]<- (sum(vpanal.edo[[i]])/sum(votos.edo[[i]]))*100
}

###################################
#####################Skewness###########
#########################################

library(normtest)
##skewness is a measure of symmetry (or the lack of it)
##H0: the distribution follows a normal distribution
##H1: the distribution follows a normal distribution
## p-value small. H0 is rejected.



##Skewness test of PRI votes distribution, by state
skewness.edo.pri<-list()
for(i in 1:32){
  skewness.edo.pri[[i]] <-skewness.norm.test(data.edo[[i]]$v)
}

##Skewness test of PRD votes distribution, by state
skewness.edo.prd<-list()
for(i in 1:32){
  skewness.edo.prd[[i]] <-skewness.norm.test(data.edo[[i]]$prd)
}

##Skewness test of PANAL votes distribution, by state
skewness.edo.panal<-list()
for(i in 1:32){
  skewness.edo.panal[[i]] <-skewness.norm.test(data.edo[[i]]$PANAL)
}

##Skewness test of PAN votes distribution, by state
skewness.edo.pan<-list()
for(i in 1:32){
  skewness.edo.pan[[i]] <-skewness.norm.test(data.edo[[i]]$PAN)
}

##Skewness test of total votes distribution, by state
skewness.edo.total<-list()
for(i in 1:32){
  skewness.edo.total[[i]] <-skewness.norm.test(data.edo[[i]]$t)
}

##Skewness test of turnout distribution, by state
skewness.edo.turnout<-list()
for(i in 1:32){
  skewness.edo.turnout[[i]] <-skewness.norm.test(data.edo[[i]]$turnout)
}

##Skewness test of Absolute Support for PRI, by state
Skew.Abs.pri.edo<-list()
for(i in 1:32){
  Skew.Abs.pri.edo[[i]] <- skewness.norm.test(Abs.pri.edo.2[[i]])
}

##Skewness test of Absolute Support for PRD, by state
Skew.Abs.prd.edo<-list()
for(i in 1:32){
  Skew.Abs.prd.edo[[i]] <- skewness.norm.test(Abs.prd.edo.2[[i]])
}

##Skewness test of Absolute Support for PAN, by state
Skew.Abs.pan.edo<-list()
for(i in 1:32){
  Skew.Abs.pan.edo[[i]] <- skewness.norm.test(Abs.pan.edo.2[[i]])
}

##Skewness test of Absolute Support for PANAL, by state
Skew.Abs.panal.edo<-list()
for(i in 1:32){
  Skew.Abs.panal.edo[[i]] <- skewness.norm.test(Abs.panal.edo.2[[i]])
}

##Skewness test of Relative Support for PRI, by state
Skew.Rel.pri.edo<-list()
for(i in 1:32){
  Skew.Rel.pri.edo[[i]] <- skewness.norm.test(Rel.pri.edo.2[[i]])
}

##Skewness test of Relative Support for PRD, by state
Skew.Rel.prd.edo<-list()
for(i in 1:32){
  Skew.Rel.prd.edo[[i]] <- skewness.norm.test(Rel.prd.edo.2[[i]])
}

##Skewness test of Relative Support for PAN, by state
Skew.Rel.pan.edo<-list()
for(i in 1:32){
  Skew.Rel.pan.edo[[i]] <- skewness.norm.test(Rel.pan.edo.2[[i]])
}

##Skewness test of Relative Support for PANAL, by state
Skew.Rel.panal.edo<-list()
for(i in 1:32){
  Skew.Rel.panal.edo[[i]] <- skewness.norm.test(Rel.panal.edo.2[[i]])
}

###################################
####################Kurtosis###########
#########################################
###############


library(moments)

##kurtosis is a measure of the "tailedness" of the probability distribution of a real-valued random variable
##H0: the distribution follows a normal distribution
##H1: the distribution follows a normal distribution
## p-value small. H0 is rejected.



##kurtosis test of PRI votes distribution, by state
kurtosis.edo.pri<-list()
for(i in 1:32){
  kurtosis.edo.pri[[i]] <- kurtosis.norm.test(data.edo[[i]]$v)
}

##kurtosis test of PRD votes distribution, by state
kurtosis.edo.prd<-list()
for(i in 1:32){
  kurtosis.edo.prd[[i]] <- kurtosis.norm.test(data.edo[[i]]$prd)
}

##kurtosis test of PAN votes distribution, by state
kurtosis.edo.pan<-list()
for(i in 1:32){
  kurtosis.edo.pan[[i]] <- kurtosis.norm.test(data.edo[[i]]$PAN)
}

##kurtosis test of PANAL votes distribution, by state
kurtosis.edo.panal<-list()
for(i in 1:32){
  kurtosis.edo.panal[[i]] <- kurtosis.norm.test(data.edo[[i]]$PANAL)
}

##kurtosis test of total votes distribution, by state
kurtosis.edo.total<-list()
for(i in 1:32){
  kurtosis.edo.total[[i]] <- kurtosis.norm.test(data.edo[[i]]$t)
}

##kurtosis test of turnout distribution, by state
kurtosis.edo.turnout<-list()
for(i in 1:32){
  kurtosis.edo.turnout[[i]] <- kurtosis.norm.test(data.edo[[i]]$turnout)
}

##kurtosis test of Absolute Support for PRI, by state
kur.Abs.pri.edo<-list()
for(i in 1:32){
  kur.Abs.pri.edo[[i]] <- kurtosis.norm.test(Abs.pri.edo.2[[i]])
}

##kurtosis test of Absolute Support for PRD, by state
kur.Abs.prd.edo<-list()
for(i in 1:32){
  kur.Abs.prd.edo[[i]] <- kurtosis.norm.test(Abs.prd.edo.2[[i]])
}

##kurtosis test of Absolute Support for PAN, by state
kur.Abs.pan.edo<-list()
for(i in 1:32){
  kur.Abs.pan.edo[[i]] <- kurtosis.norm.test(Abs.pan.edo.2[[i]])
}

##kurtosis test of Absolute Support for PANAL, by state
kur.Abs.panal.edo<-list()
for(i in 1:32){
  kur.Abs.panal.edo[[i]] <- kurtosis.norm.test(Abs.panal.edo.2[[i]])
}

##kurtosis test of Relative Support for PRI, by state
kur.Rel.pri.edo<-list()
for(i in 1:32){
  kur.Rel.pri.edo[[i]] <- kurtosis.norm.test(Rel.pri.edo.2[[i]])
}

##kurtosis test of Relative Support for PRD, by state
kur.Rel.prd.edo<-list()
for(i in 1:32){
  kur.Rel.prd.edo[[i]] <- kurtosis.norm.test(Rel.prd.edo.2[[i]])
}

##kurtosis test of Relative Support for PAN, by state
kur.Rel.pan.edo<-list()
for(i in 1:32){
  kur.Rel.pan.edo[[i]] <- kurtosis.norm.test(Rel.pan.edo.2[[i]])
}

##kurtosis test of Relative Support for PANAL, by state
kur.Rel.panal.edo<-list()
for(i in 1:32){
  kur.Rel.panal.edo[[i]] <- kurtosis.norm.test(Rel.panal.edo.2[[i]])
}


#####################################
#######################

####bendford analysis per State

library(benford.analysis)

###benford test of PRI votes, by state
bfd.edopri<-list()
for (i in 1:32){
  bfd.edopri[[i]]<- benford(data.edo[[i]]$v, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
}

##plot bendford test of PRI votes in Aguascalientes
plot(bfd.edopri[[1]])
plot(bfd.edopri[[2]])
.
.
.


###benford test of total votes, by state
bfd.edototal<-list()
for (i in 1:32){
  benford(data.edo[[i]]$t, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
  bfd.edototal[[i]]  <- benford(data.edo[[i]]$t)
}

###benford test of PAN votes, by state
bfd.edopan<-list()
for (i in 1:32){
  bfd.edopan[[i]] <-benford(data.edo[[i]]$PAN, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
}

###benford test of PANAL votes, by state
bfd.edopanal<-list()
for (i in 1:32){
  bfd.edopanal[[i]] <-benford(data.edo[[i]]$PANAL, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
}

###benford test of PRD votes, by state
bfd.edoprd<-list() 
for (i in 1:32){
  bfd.edoprd[[i]] <-benford(data.edo[[i]]$prd, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
}

###benford test of turnout, by state
bfd.edoturnout<-list()
for (i in 1:32){
  bfd.edoturnout[[i]] <-benford(data.edo[[i]]$turnout, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
}


####BendfordTests for the first, second and third digit.
################
library(BenfordTests)

data.edo<-list()
index.edo<-c(1:32)
for (i in 1:32){
  data.edo[[i]] <- ndata1[ndata1$id.estado==index.edo[i],c(8:16)]
}

###chisq.benftest of total votes, by state
##first digit
chisq.total.1d.edo<-list()
for (i in 1:32){
  chisq.total.1d.edo[[i]]<- chisq.benftest(data.edo[[i]]$t, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
}
##second digit
chisq.total.2d.edo<-list()
for (i in 1:32){
  chisq.total.2d.edo[[i]]<- chisq.benftest(data.edo[[i]]$t, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
}
##third digit
chisq.total.3d.edo<-list()
for (i in 1:32){
  chisq.total.3d.edo[[i]]<-chisq.benftest(data.edo[[i]]$t, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)
}


###chisq.benftest of PRI votes, by state
##first digit
chisq.pri.1d.edo<-list()
for (i in 1:32){
  chisq.pri.1d.edo[[i]]<- chisq.benftest(data.edo[[i]]$v, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
}
##second digit
chisq.pri.2d.edo<-list()
for (i in 1:32){
  chisq.pri.2d.edo[[i]]<- chisq.benftest(data.edo[[i]]$v, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
}
##third digit
chisq.pri.3d.edo<-list()
for (i in 1:32){
  chisq.pri.3d.edo[[i]]<-chisq.benftest(data.edo[[i]]$v, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)
}


###chisq.benftest of PRD votes, by state
##first digits
chisq.prd.1d.edo<-list()
for (i in 1:32){
  chisq.prd.1d.edo[[i]]<- chisq.benftest(data.edo[[i]]$prd, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
}
##second digits
chisq.prd.2d.edo<-list()
for (i in 1:32){
  chisq.prd.2d.edo[[i]]<- chisq.benftest(data.edo[[i]]$prd, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
}
##third digit
chisq.prd.3d.edo<-list()
for (i in 1:32){
  chisq.prd.3d.edo[[i]]<-chisq.benftest(data.edo[[i]]$prd, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)
}

###chisq.benftest of PAN votes, by state
##first digit
chisq.pan.1d.edo<-list()
for (i in 1:32){
  chisq.pan.1d.edo[[i]]<- chisq.benftest(data.edo[[i]]$PAN, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
}
##second digit
chisq.pan.2d.edo<-list()
for (i in 1:32){
  chisq.pan.2d.edo[[i]]<- chisq.benftest(data.edo[[i]]$PAN, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
}
##Third digit
chisq.pan.3d.edo<-list()
for (i in 1:32){
  chisq.pan.3d.edo[[i]]<-chisq.benftest(data.edo[[i]]$PAN, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)
}


###chisq.benftest of PANAL votes, by state
##first digit
chisq.panal.1d.edo<-list()
for (i in 1:32){
  chisq.panal.1d.edo[[i]]<- chisq.benftest(data.edo[[i]]$PANAL, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
}
##second digit
chisq.panal.2d.edo<-list()
for (i in 1:32){
  chisq.panal.2d.edo[[i]]<- chisq.benftest(data.edo[[i]]$PANAL, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
}
##third digit
chisq.panal.3d.edo<-list()
for (i in 1:32){
  chisq.panal.3d.edo[[i]]<-chisq.benftest(data.edo[[i]]$PANAL, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)
}

###chisq.benftest of turnou, by state
##first digit
chisq.turnout.1d.edo<-list()
for (i in 1:32){
  chisq.turnout.1d.edo[[i]]<- chisq.benftest(data.edo[[i]]$turnout, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
}
##second digit
chisq.turnout.2d.edo<-list()
for (i in 1:32){
  chisq.turnout.2d.edo[[i]]<- chisq.benftest(data.edo[[i]]$turnout, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
}
##third digit
chisq.turnout.3d.edo<-list()
for (i in 1:32){
  chisq.turnout.3d.edo[[i]]<-chisq.benftest(data.edo[[i]]$turnout, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)
}

## In order to apply Rozenas Test we need 
##to work just with the poll stations with 100% or less of turnout
##we take only the values of N, v and t for this analysis
data.edo<-list()
index.edo<-c(1:32)
for (i in 1:32){
  data.edo[[i]] <- ndata5[ndata5$id.estado==index.edo[i],c(12:14)]
}

library(spikes)

plot(data.edo[[1]], main = 'Aguascalientes: distribution 2012')
plot(data.edo[[2]], main="Baja California: distribution 2012")
plot(data.edo[[3]], main="Baja California Sur: distribution 2012")
plot(data.edo[[4]], main="Campeche: distribution 2012")
plot(data.edo[[5]], main="Coahuila: distribution 2012")
plot(data.edo[[6]], main="Colima: distribution 2012")
plot(data.edo[[7]], main="Chiapas: distribution 2012")
plot(data.edo[[8]], main="Chihuahua: distribution 2012")
plot(data.edo[[9]], main="Distrito Federal: distribution 2012")
plot(data.edo[[10]], main="Durango: distribution 2012")
plot(data.edo[[11]], main="Guanajuato: distribution 2012")
plot(data.edo[[12]], main="Guerrero: distribution 2012")
plot(data.edo[[13]], main="Hidalgo: distribution 2012")
plot(data.edo[[14]], main="Jalisco: distribution 2012")
plot(data.edo[[15]], main="México: distribution 2012")
plot(data.edo[[16]], main="Michoacán: distribution 2012")
plot(data.edo[[17]], main="Morelos: distribution 2012")
plot(data.edo[[18]], main="Nayarit: distribution 2012")
plot(data.edo[[19]], main="Nuevo León: distribution 2012")
plot(data.edo[[20]], main="Oaxaca: distribution 2012")
plot(data.edo[[21]], main="Puebla: distribution 2012")
plot(data.edo[[22]], main="Querétaro: distribution 2012")
plot(data.edo[[23]], main="Quintana Roo: distribution 2012")
plot(data.edo[[24]], main="San Luis Potosí: distribution 2012")
plot(data.edo[[25]], main="Sinaloa: distribution 2012")
plot(data.edo[[26]], main="Sonora: distribution 2012")
plot(data.edo[[27]], main="Tabasco: distribution 2012")
plot(data.edo[[28]], main="Tamaulipas: distribution 2012")
plot(data.edo[[29]], main="Tlaxcala: distribution 2012")
plot(data.edo[[30]], main="Veracruz: distribution 2012")
plot(data.edo[[31]], main="Yucatán: distribution 2012")
plot(data.edo[[32]], main="Zacatecas: distribution 2012")

##to get the level of Estimated percentage of fraudulent precincts(analizing just PRI votes), by state
spikes(data.edo[[32]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL )
out1 <- spikes(data.edo[[1]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out2 <- spikes(data.edo[[2]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out3 <- spikes(data.edo[[3]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out4 <- spikes(data.edo[[4]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out5 <- spikes(data.edo[[5]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out6 <- spikes(data.edo[[6]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out7 <- spikes(data.edo[[7]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out8 <- spikes(data.edo[[8]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out9 <- spikes(data.edo[[9]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out10 <- spikes(data.edo[[10]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out11 <- spikes(data.edo[[11]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out12 <- spikes(data.edo[[12]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out13 <- spikes(data.edo[[13]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out14 <- spikes(data.edo[[14]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out15 <- spikes(data.edo[[15]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out16 <- spikes(data.edo[[16]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out17 <- spikes(data.edo[[17]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out18 <- spikes(data.edo[[18]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out19 <- spikes(data.edo[[19]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out20 <- spikes(data.edo[[20]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out21 <- spikes(data.edo[[21]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out22 <- spikes(data.edo[[22]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out23 <- spikes(data.edo[[23]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out24 <- spikes(data.edo[[24]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)##Extracts estimate of fraud and 95 percent credible interval (if such is estimated) for the object of
out25 <- spikes(data.edo[[25]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out26 <- spikes(data.edo[[26]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out27 <- spikes(data.edo[[27]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out28 <- spikes(data.edo[[28]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out29 <- spikes(data.edo[[29]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out30 <- spikes(data.edo[[30]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out31 <- spikes(data.edo[[31]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out32 <- spikes(data.edo[[32]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)


##To plot the RDK results from Rozenas test for each state

plot(out1, main = "Rozenas Test: Aguascalientes 2012"  )
plot(out2, main = "Rozenas Test: Baja California 2012"  )
plot(out3, main = "Rozenas Test: Baja California Sur 2012"  )
plot(out4, main = "Rozenas Test: Campeche 2012" )
plot(out5, main = "Rozenas Test: Coahuila 2012"  )
plot(out6, main = "Rozenas Test: Colima 2012"  )
plot(out7, main = "Rozenas Test: Chiapas 2012"  )
plot(out8, main = "Rozenas Test: Chihuahua 2012"  )
plot(out9, main = "Rozenas Test: Distrito Federal 2012"  )
plot(out10, main = "Rozenas Test: Durango 2012"  )
plot(out11, main = "Rozenas Test: Guanajuato 2012"  )
plot(out12, main = "Rozenas Test: Guerrero 2012"  )
plot(out13, main = "Rozenas Test: Hidalgo 2012" )
plot(out14, main = "Rozenas Test: Jalisco 2012"  )
plot(out15, main = "Rozenas Test: México 2012"  )
plot(out16, main = "Rozenas Test: Michoacán 2012"  )
plot(out17, main = "Rozenas Test: Morelos 2012" )
plot(out18, main = "Rozenas Test: Nayarit 2012" )
plot(out19, main = "Rozenas Test: Nuevo León 2012" )
plot(out20, main = "Rozenas Test: Oaxaca 2012" )
plot(out21, main = "Rozenas Test: Puebla 2012" )
plot(out22, main = "Rozenas Test: Querétaro 2012" )
plot(out23, main = "Rozenas Test: Quintana Roo 2012"  )
plot(out24, main = "Rozenas Test: San Luis Potosí 2012" )
plot(out25, main ="Rozenas Test: Sinaloa 2012"  )
plot(out26, main = "Rozenas Test: Sonora 2012")
plot(out27, main = "Rozenas Test: Tabasco 2012"  )
plot(out28, main = "Rozenas Test: Tamaulipas 2012" )
plot(out29, main = "Rozenas Test: Tlaxcala 2012" )
plot(out30, main = "Rozenas Test: Veracruz 2012" )
plot(out31, main = "Rozenas Test: Yucatán 2012" )
plot(out32,  main = "Rozenas Test: Zacatecas 2012" )


##############################################
######Hartigans' dip test for unimodality / multimodality###
######################################
install.packages("diptest")
library(diptest)

dip.test(ndata1$turnout, simulate.p.value = FALSE, B = 2000)
dip.test(ndata1$v, simulate.p.value = FALSE, B = 2000)
dip.test(ndata1$prd, simulate.p.value = FALSE, B = 2000)

data.edo<-list()
index.edo<-c(1:32)
for (i in 1:32){
  data.edo[[i]] <- ndata1[ndata1$id.estado==index.edo[i],c(8:16)]
}

dip.test.edo.turnout<-list()
for(i in 1:32){
  dip.test.edo.turnout[[i]] <- dip.test(data.edo[[i]]$turnout, simulate.p.value = FALSE, B = 2000)
}

dip.test.edo.pri<-list()
for(i in 1:32){
  dip.test.edo.pri[[i]] <- dip.test(data.edo[[i]]$v, simulate.p.value = FALSE, B = 2000)
}

dip.test.edo.prd<-list()
for(i in 1:32){
  dip.test.edo.prd[[i]] <- dip.test(data.edo[[i]]$prd, simulate.p.value = FALSE, B = 2000)
}




########################################################################################################
#########################################################################################################
###########################################################################################################
#######################################################################################################

##we get the 2018 electorals data
#rm(list=ls())
getwd()
setwd("F:/Warwick/Project")
ndata.18.2 <- read.csv("presidencia2018.csv")

##take away poll stations with zero votes
ndata.18.2<-ndata.18.2[!(ndata.18.2$TOTAL_VOTOS_CALCULADOS==0),]

##create turnout and the final votes for each candidate (Coalitions)
## to create the PRI variable (adding the coalition votes)
ndata.18.2$newcolumn <- ndata.18.2$PRI+ ndata.18.2$PVEM + ndata.18.2$NUEVA.ALIANZA + ndata.18.2$PRI_NA + ndata.18.2$PRI_PVEM + ndata.18.2$PRI_PVEM_NA + ndata.18.2$PVEM_NA 
colnames(ndata.18.2) <- c("casilla","clave.acta", "id.estado", "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita","Bronco", "numvotreg", "votosnulos", "votos", "listanominal", "observaciones", "mecanismo", "Hora", "pri")

## to create the PAN variable (adding the coalition votes)
ndata.18.2$newcolumn <- ndata.18.2$PRD + ndata.18.2$PAN + ndata.18.2$MC + ndata.18.2$PAN_MC + ndata.18.2$PAN_PRD_MC + ndata.18.2$PAN_PRD + ndata.18.2$PRD_MC
colnames(ndata.18.2) <- c("casilla","clave.acta", "id.estado", "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita","Bronco", "numvotreg", "votosnulos", "votos", "listanominal", "observaciones", "mecanismo", "Hora", "pri", "pan")

## to create the MORENA variable (adding the coalition votes)
ndata.18.2$newcolumn <- ndata.18.2$MORENA + ndata.18.2$MORENA_PES + ndata.18.2$PT + ndata.18.2$PT_MORENA + ndata.18.2$PT_MORENA_PES + ndata.18.2$PT_PES + ndata.18.2$Enc.Social
colnames(ndata.18.2) <- c("casilla","clave.acta", "id.estado", "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita","Bronco", "numvotreg", "votosnulos", "votos", "listanominal", "observaciones", "mecanismo", "Hora", "pri", "pan", "morena")

## to create the turnout variable 
ndata.18.2$newcolumn<- (ndata.18.2$votos/ndata.18.2$listanominal)*100
colnames(ndata.18.2) <- c("casilla","clave.acta", "id.estado", "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita","Bronco", "numvotreg", "votosnulos", "votos", "listanominal", "observaciones", "mecanismo", "Hora", "pri","pan", "morena", "turnout")

## to create Absolute support for PRI variable 
ndata.18.2$newcolumn<- (ndata.18.2$pri/ndata.18.2$listanominal)*100
colnames(ndata.18.2) <- c("casilla","clave.acta", "id.estado", "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita","Bronco", "numvotreg", "votosnulos", "votos", "listanominal", "observaciones", "mecanismo", "Hora", "pri","pan", "morena", "turnout", "AbsPRI")

## to create Absolute support for MORENA variable 
ndata.18.2$newcolumn<- (ndata.18.2$morena/ndata.18.2$listanominal)*100
colnames(ndata.18.2) <- c("casilla","clave.acta", "id.estado", "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita","Bronco" ,"numvotreg", "votosnulos", "votos", "listanominal", "observaciones", "mecanismo", "Hora", "pri","pan", "morena", "turnout", "AbsPRI","Absmorena")

## to create Absolute support for PAN variable 
ndata.18.2$newcolumn<- (ndata.18.2$pan/ndata.18.2$listanominal)*100
colnames(ndata.18.2) <- c("casilla","clave.acta", "id.estado", "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita","Bronco", "numvotreg", "votosnulos", "votos", "listanominal", "observaciones", "mecanismo", "Hora", "pri","pan", "morena", "turnout", "AbsPRI","Absmorena","AbsPAN")

## to create Absolute support for Bronco variable 
ndata.18.2$newcolumn<- (ndata.18.2$Bronco/ndata.18.2$listanominal)*100
colnames(ndata.18.2) <- c( "casilla","clave.acta", "id.estado", "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita","Bronco", "numvotreg", "votosnulos", "votos", "listanominal", "observaciones", "mecanismo", "Hora", "pri","pan", "morena", "turnout", "AbsPRI","Absmorena","AbsPAN", "Absbronco")

## to create Relative support for PRI variable 
ndata.18.2$newcolumn<- (ndata.18.2$pri/ndata.18.2$votos)*100
colnames(ndata.18.2) <- c("casilla","clave.acta", "id.estado", "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita","Bronco", "numvotreg", "votosnulos", "votos", "listanominal", "observaciones", "mecanismo", "Hora", "pri","pan", "morena", "turnout", "AbsPRI","Absmorena","AbsPAN", "Absbronco", "RelPRI")

## to create Relative support for MORENA variable 
ndata.18.2$newcolumn<- (ndata.18.2$morena/ndata.18.2$votos)*100
colnames(ndata.18.2) <- c( "casilla","clave.acta", "id.estado", "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita","Bronco", "numvotreg", "votosnulos", "votos", "listanominal", "observaciones", "mecanismo", "Hora", "pri","pan", "morena", "turnout", "AbsPRI","Absmorena","AbsPAN", "Absbronco", "RelPRI", "Relmorena")

## to create Relative support for PAN variable 
ndata.18.2$newcolumn<- (ndata.18.2$pan/ndata.18.2$votos)*100
colnames(ndata.18.2) <- c( "casilla","clave.acta", "id.estado", "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita","Bronco", "numvotreg", "votosnulos", "votos", "listanominal", "observaciones", "mecanismo", "Hora", "pri","pan", "morena", "turnout", "AbsPRI","Absmorena","AbsPAN", "Absbronco", "RelPRI", "Relmorena","RelPAN")

## to create Relative support for Bronco variable 
ndata.18.2$newcolumn<- (ndata.18.2$Bronco/ndata.18.2$votos)*100
colnames(ndata.18.2) <- c( "casilla","clave.acta", "id.estado", "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita", "Bronco", "numvotreg", "votosnulos", "t", "N", "observaciones", "mecanismo", "Hora", "pri","pan", "v", "turnout", "AbsPRI","Absmorena","AbsPAN", "Absbronco", "RelPRI", "Relmorena","RelPAN","Relbronco")

##N -> Nominal list
##t-> Total votes
##V-> votes for MORENA (they won the election)

## to create a dataset with the total votes for each coalition and Absolute and Relative percentages of support
myvars <- names(ndata.18.2) %in% c("casilla","clave.acta",  "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES",  "observaciones", "mecanismo", "Hora")
ndata0.18.2 <- ndata.18.2[!myvars]

## to create a dataset with just the total votes for each coalition
myvars <- names(ndata0.18.2) %in% c("casilla","clave.acta",  "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES",  "observaciones", "mecanismo", "Hora", "AbsPRI","Absmorena","AbsPAN", "Absbronco", "RelPRI", "Relmorena","RelPAN","Relbronco") 
ndata1.18.2 <- ndata0.18.2[!myvars]
myvars <- names(ndata0.18.2) %in% c("casilla","clave.acta",  "estado", "id.distrito", "distrito",  "seccion", "id.casilla","casilla","contigua", "tipo.casilla","num.acta","PAN", "PRI", "PRD", "PVEM", "PT" , "MC", "NVA_ALIANZA", "MORENA", "Enc.Social", "PAN_PRD_MC", "PAN_PRD", "PAN_MC", "PRD_MC","PRI_PVEM_NA","PRI_PVEM","PRI_NA","PVEM_NA", "PT_MORENA_PES","PT_MORENA","PT_PES","MORENA_PES","Margarita", "numvotreg", "observaciones", "mecanismo", "Hora") 
ndata2.2 <- ndata0.18.2[!myvars]

## to create a dataset without the poll stations with more than 100% of turnout
ndata3.2<-ndata2.2[!(ndata2.2$turnout>100),]

## to create a dataset with the poll stations with more than 100% of turnout
ndata4.2<-ndata1.18.2[!(ndata1.18.2$turnout<=100),]


ndata5.2<-ndata1.18.2[!(ndata1.18.2$turnout>100),]
myvars <- names(ndata5.2) %in% c("Bronco","Margarita",  "numvotreg", "votosnulos", "pri",  "pan") 
ndata5.2 <- ndata5.2[!myvars]

ndata6.2<-ndata2.2[!(ndata2.2$t>=1000),]

##General data
##To create the sum of votes for each party and nullvotes at a National Level
vpan.18.2 <- sum(ndata1.18.2$pan)
vmorena.18.2 <- sum(ndata1.18.2$v)
vbronco.18.2 <- sum(ndata1.18.2$Bronco)
vmargarita.18<- sum(ndata1.18$Margarita)
vpri.18.2 <- sum(ndata1.18.2$pri)
vothers.18.2 <- sum(ndata1.18.2$numvotreg)
vnull.18.2 <- sum(ndata1.18.2$votosnulos)
ListNom.18.2 <-sum(ndata1.18.2$N)
TotalVotes.18.2 <- sum(ndata1.18.2$t)

##To create the Absolute support values at a National Level
Abs.pri.18.2<- vpri.18.2/ListNom.18.2
Abs.morena.18.2<- vmorena.18.2/ListNom.18.2
Abs.pan.18.2<- vpan.18.2/ListNom.18.2
Abs.bronco.18.2<- vbronco.18.2/ListNom.18.2

##To create the Relative support values at a National Level
Rel.pri.18.2<- vpri.18.2/TotalVotes.18.2
Rel.morena.18.2<- vmorena.18.2/TotalVotes.18.2
Rel.pan.18.2<- vpan.18.2/TotalVotes.18.2
Rel.bronco.18.2<- vbronco.18.2/TotalVotes.18.2

##To look at the density distribution of Absolute 
##support values at a National Level for the first and second places in the election
summary(ndata3.2$Absmorena)
plot(density(ndata3.2$AbsPAN))
plot(density(ndata3.2$Absmorena))


### Analysis of absolute support can provide information as to the type of fraud
##employed in an electoral process. If vote manipulation comes in the form of vote inflation, the absolute support
##histogram for the perpetrator will shift to the right 

library("ggpubr")
library("dplyr")

ggdensity(ndata3.2$Absmorena, 
          main = "Density plot of Absolute support to MORENA",
          xlab = "Absolute Support to MORENA")

ggqqplot(ndata3.2$Absmorena)

library("dplyr")
library("ggpubr")

ggdensity(ndata2.2$turnout, 
          main = "2018 Turnout",
          xlab = "Turnout")

ggdensity(ndata6.2$Bronco, 
          main = "2018 Bronco's votes",
          xlab = "Votes for Bronco")




###by state
data.edo<-list()
index.edo<-c(1:32)
for (i in 1:32){
  data.edo[[i]] <- ndata6.2[ndata6.2$id.estado==index.edo[i],c(2:17)]
}

ggdensity( data.edo[[1]]$v, 
           main = "Aguascalientes: 2018 MORENA's votes",
           xlab = "MORENA's Votes")

ggdensity( data.edo[[2]]$v, 
           main = "Baja California: 2018 MORENA's votes",
           xlab = "MORENA's Votes")



#####################################
#####################################
#####################################
#####################################
#####################################


## Perform the test to compare distributions (turnout and votes for each party) with normal distribution
##to test if word occurrences in a set of documents follows a Normal distribution you can just use a density test and some qqplots

###to look at the turnout levels

summary(ndata2.2$turnout)
boxplot(ndata2.2$turnout, horizontal=TRUE, main="National Turnout, 2018")
boxplot(ndata6.2$pri, horizontal=TRUE, main="PRI´s votes, 2018")
boxplot(ndata6.2$pan, horizontal=TRUE, main="PAN´s votes, 2018")
boxplot(ndata6.2$v, horizontal=TRUE, main="MORENA´s votes, 2018")
boxplot(ndata2.2$Bronco, horizontal=TRUE, main="Bronco´s votes, 2018")
boxplot(ndata2.2$AbsPRI, horizontal=TRUE, main="Absolute support for PRI, 2018")
boxplot(ndata2.2$AbsPAN, horizontal=TRUE, main="Absolute support for PAN, 2018")
boxplot(ndata2.2$Absbronco, horizontal=TRUE, main="Absolute support for Bronco, 2018")
boxplot(ndata2.2$Absmorena, horizontal=TRUE, main="Absolute support for MORENA, 2018")

boxplot(data.edo[[1]]$v, horizontal=TRUE, main="Aguascalientes MORENA´s votes, 2018")
boxplot(data.edo[[2]]$v, horizontal=TRUE, main="Baja California MORENA´s votes, 2018")
.
.
.


kernelest1<-density(ndata1$turnout, bw = "nrd0", adjust = 1,
                    kernel = c("gaussian"),
                    weights = NULL, window = kernel, width,
                    give.Rkern = FALSE,
                    n = 513,  na.rm = TRUE) 
plot(kernelest1,main = "Estimated Kernel density: National Turnout 2018")


library(ggplot2)
##plot Absolute PRI and PRD Support 
qplot(ndata2.2$AbsPRI, geom = 'histogram', binwidth = 0.001, main = 'Absolute Support for PRI, 2018', xlim=c(0,100), ylim=c(0,20)) + xlab('Absolute Support PRI')
qplot(ndata2.2$Absmorena, geom = 'histogram', binwidth = 0.001, main = 'Absolute Support for MORENA, 2018', xlim=c(0,100), ylim=c(0,20)) + xlab('Absolute Support MORENA')
qplot(ndata2.2$AbsPAN, geom = 'histogram', binwidth = 0.001, main = 'Absolute Support for PAN, 2018', xlim=c(0,100), ylim=c(0,20)) + xlab('Absolute Support PAN')
qplot(ndata2.2$Absbronco, geom = 'histogram', binwidth = 0.001, main = 'Absolute Support for Bronco, 2018', xlim=c(0,100), ylim=c(0,20)) + xlab('Absolute Support Bronco')
qplot(ndata6.2$pri, geom = 'histogram', binwidth = 0.001, main = 'PRI´s votes, 2018', xlim=c(0,100), ylim=c(0,20)) + xlab('PRI´s votes')
qplot(ndata6.2$pan, geom = 'histogram', binwidth = 0.001, main = 'PAN´s votes, 2018', xlim=c(0,100), ylim=c(0,20)) + xlab('PAN´s votes')
qplot(ndata6.2$v, geom = 'histogram', binwidth = 0.001, main = 'MORENA´s votes, 2018', xlim=c(0,100), ylim=c(0,20)) + xlab('MORENA´s votes')
qplot(ndata6.2$Bronco, geom = 'histogram', binwidth = 0.001, main = 'Broncos´svotes, 2018', xlim=c(0,100), ylim=c(0,20)) + xlab('Bronco´s votes')


## Perform the test to compare distributions (turnout and votes for each party) with normal distribution
##to test if word occurrences in a set of documents follows a Normal distribution you can just use a density test and some qqplots

###to look at the turnout levels

summary(ndata1.18.2$turnout)
boxplot(ndata1.18.2$turnout, horizontal=TRUE, main="Level of national turnout, 2018")

kernelest1.18.2<-density(ndata1.18.2$turnout, bw = "nrd0", adjust = 1,
                         kernel = c("gaussian"),
                         weights = NULL, window = kernel, width,
                         give.Rkern = FALSE,
                         n = 513,  na.rm = TRUE) 
plot(kernelest1.18.2,main = "Estimated Kernel density: National Turnout, 2018")

library(ggplot2)
##plot Absolute MORENA and PAN Support 
qplot(ndata3.2$Absmorena, geom = 'histogram', binwidth = 0.001, main = 'Absolute Vote MORENA', xlim=c(0,100), ylim=c(0,20)) + xlab('Absolute Vote MORENA 2018')
qplot(ndata3.2$AbsPAN, geom = 'histogram', binwidth = 0.001, main = 'Absolute Vote PAN', xlim=c(0,100), ylim=c(0,20)) + xlab('Absolute Vote PAN 2018')


##plot  National turnout
qplot(ndata1.18.2$turnout, geom = 'histogram', binwidth = 0.001, main = 'National turnout per poll station', xlim=c(0,100), ylim=c(0,20)) + xlab('National Turnout per poll station, 2018')
dataturnout.18.2<- data.frame(ndata1.18.2$turnout)
ggplot(dataturnout.18.2, aes(x = ndata1.18.2$turnout), binwidth = 0.01, bins= NULL, na.rm = FALSE, xlim=c(0,150), ylim=c(0,20)) + 
  geom_histogram(aes(y = ..density..),bins = 200, binwidth = 0.5, fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Turnout votes'))) + 
  ylab(expression(bold('Density')))

ggplot(dataturnout.18.2, aes(ndata1.18.2$turnout)) +
  geom_histogram(bins = 200)


## To analyze Skewness and Kurtosis for the data to look if the distribution behaves as normal
library(moments)

library(normtest)
##kurtosis is a measure of the "tailedness" of the probability distribution of a real-valued random variable
##H0: the distribution follows a normal distribution
##H1: the distribution follows a normal distribution
## p-value small. H0 is rejected.
kur.tot.2<- kurtosis.norm.test(ndata1.18.2$t)
kur.morena.2<- kurtosis.norm.test(ndata1.18.2$v)
kur.turnout.2<- kurtosis.norm.test(ndata1.18.2$turnout)
kur.pri.2<- kurtosis.norm.test(ndata1.18.2$pri)
kur.pan.2<- kurtosis.norm.test(ndata1.18.2$pan)
kur.bronco.2<- kurtosis.norm.test(ndata1.18.2$Bronco)


##skewness is a measure of symmetry (or the lack of it)
##H0: the distribution follows a normal distribution
##H1: the distribution follows a normal distribution
## p-value small. H0 is rejected.
skew.tot.2<- skewness.norm.test(ndata1.18.2$t)
skew.morena.2<- skewness.norm.test(ndata1.18.2$v)
skew.turnout.2<- skewness.norm.test(ndata1.18.2$turnout)
skew.pri.2<- skewness.norm.test(ndata1.18.2$pri)
skew.pan.2<- skewness.norm.test(ndata1.18.2$pan)
skew.bronco.2<- skewness.norm.test(ndata1.18.2$Bronco)

###to see boxplot of votes for each party
boxplot(ndata1.18.2$pan, horizontal=TRUE, main="Votes for PAN")
hist(ndata1.18$pan.2, breaks=15, main="Votes for PAN")
boxplot(ndata1.18.2$v, horizontal=TRUE, main="Votes for MORENA")
hist(ndata1.18.2$v, breaks=15, main="Votes for MORENA")
boxplot(ndata1.18.2$pri, horizontal=TRUE, main="Votes for PRI")
hist(ndata1.18.2$pri, breaks=15, main="Votes for PRI")
boxplot(ndata1.18.2$Bronco, horizontal=TRUE, main="Votes for Bronco")
hist(ndata1.18.2$Bronco, breaks=15, main="Votes for Bronco")

###to see histograms of votes for each state


hist(data.edo[[1]]$v, breaks=15, main="Aguascalientes:votes for MORENA 2018",xlab = "Votes for MORENA")
hist(data.edo[[2]]$v, breaks=15, main="Baja California: votes for MORENA 2018",xlab = "Votes for MORENA")
.
.
.
###

###############BendfordTest####################3

#the bendford test could be applied for the first, second and third digit of the data for each of the variables.


library(BenfordTests)
###chisq.benftest of MORENA votes
chi.1.morena.18<-chisq.benftest(ndata1.18.2$v, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
chi.2.morena.18<-chisq.benftest(ndata1.18.2$v, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
chi.3.morena.18<-chisq.benftest(ndata1.18.2$v, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)

###chisq.benftest of total votes
chi.1.tot.18<-chisq.benftest(ndata1.18.2$t, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
chi.2.tot.18<-chisq.benftest(ndata1.18.2$t, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
chi.3.tot.18<-chisq.benftest(ndata1.18.2$t, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)

###chisq.benftest of PRI votes
chi.1.pri.18<-chisq.benftest(ndata1.18.2$pri, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
chi.2.pri.18<-chisq.benftest(ndata1.18.2$pri, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
chi.3.pri.18<-chisq.benftest(ndata1.18.2$pri, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)

###chisq.benftest of turnout
chi.1.turnout.18<-chisq.benftest(ndata1.18.2$turnout, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
chi.2.turnout.18<-chisq.benftest(ndata1.18.2$turnout, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
chi.3.turnout.18<-chisq.benftest(ndata1.18.2$turnout, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)

###chisq.benftest of PAN votes
chi.1.pan.18<-chisq.benftest(ndata1.18.2$pan, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
chi.2.pan.18<-chisq.benftest(ndata1.18.2$pan, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
chi.3.pan.18<-chisq.benftest(ndata1.18.2$pan, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)

###chisq.benftest of Bronco votes
chi.1.bronco.18<-chisq.benftest(ndata1.18.2$Bronco, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
chi.2.bronco.18<-chisq.benftest(ndata1.18.2$Bronco, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
chi.3.bronco.18<-chisq.benftest(ndata1.18.2$Bronco, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)


##############################################
####bendford analysis


library(benford.analysis)
###benford test of PRI votes
benford(ndata1.18.2$v, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
bfd.mexmorena.2 <- benford(ndata1.18.2$v)
bfd.mexmorena.2

###benford test of MORENA votes
plot(bfd.mexmorena.2)
benford(ndata1.18.2$t, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)

###benford test of total votes
bfd.mexvo.2 <- benford(ndata1.18.2$t)
bfd.mexvo.2
plot(bfd.mexvo.2)

###benford test of PAN votes
bfd.mexpan.2 <-benford(ndata1.18.2$pan, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
plot(bfd.mexpan.2)

###benford test of Bronco's votes
bfd.mexbronco.2 <-benford(ndata1.18.2$Bronco, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
plot(bfd.mexbronco.2)

###benford test of PRI votes
bfd.mexpri.2<-benford(ndata1.18.2$pri, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
plot(bfd.mexpri.2)


###benford test of turnout
bfd.mexturnout.2 <-benford(ndata1.18.2$turnout, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
plot(bfd.mexturnout.2)



######################################################################
##Analysis per state

##to woork just with the 2nd to 11th variable in the dataset ndata1 for each state.
#The objective is to work with less data and make the analysis easier for R
data.edo.2<-list()
index.edo<-c(1:32)
for (i in 1:32){
  data.edo.2[[i]] <- ndata1.18.2[ndata1.18.2$id.estado==index.edo[i],c(2:11)]
}

###################
##Descriptives of data
###Total votes for PAN,  by state
vpan.edo.2 <- list()
for (i in 1:32){
  vpan.edo.2[[i]] <- sum(data.edo.2[[i]]$pan)
}

###Total votes for MORENA,  by state
vmorena.edo.2 <- list()
for (i in 1:32){
  vmorena.edo.2[[i]] <- sum(data.edo.2[[i]]$v)
}

###Total votes for PRI,  by state
vpri.edo.2 <- list()
for (i in 1:32){
  vpri.edo.2[[i]] <- sum(data.edo.2[[i]]$pri)
}

###Total votes for Bronco's,  by state
vbronco.edo.2 <- list()
for (i in 1:32){
  vbronco.edo.2[[i]] <- sum(data.edo.2[[i]]$Bronco)
}

###Total votes for non registered candidates,  by state
vothers.edo.2 <- list()
for (i in 1:32){
  vothers.edo.2[[i]] <- sum(data.edo.2[[i]]$numvotreg)
}

###Total null-votes,  by state
vnull.edo.2 <- list()
for (i in 1:32){
  vnull.edo.2[[i]] <- sum(data.edo.2[[i]]$votosnulos)
}

###Total votes,  by state
votos.edo.2<-list()
for (i in 1:32){
  votos.edo.2[[i]]<- sum(data.edo.2[[i]]$t)
}

###Total possible votes,  by state
nominallist.edo.2<-list()
for (i in 1:32){
  nominallist.edo.2[[i]]<-sum(data.edo.2[[i]]$N)
}

###Turnout,  by state
turnout.edo.2<- list()
for (i in 1:32){
  #turnout.edo[[i]]<- (data.edo[[i]]$t)/(data.edo[[i]]$N)
  turnout.edo.2[[i]]<- (votos.edo.2[[i]]/nominallist.edo.2[[i]])*100
}

###Absolute support value for PRI, per state
Abs.pri.edo.2.2<- list()
for (i in 1:32){
  Abs.pri.edo.2.2[[i]]<- (sum(vpri.edo.2[[i]])/sum(nominallist.edo.2[[i]]))*100
} 

###Absolute support value for MORENA, per state
Abs.morena.edo.2.2<- list()
for (i in 1:32){
  Abs.morena.edo.2.2[[i]]<- (sum(vmorena.edo.2[[i]])/sum(nominallist.edo.2[[i]]))*100
}

###Absolute support value for PAN, per state
Abs.pan.edo.2.2<- list()
for (i in 1:32){
  Abs.pan.edo.2.2[[i]]<- (sum(vpan.edo.2[[i]])/sum(nominallist.edo.2[[i]]))*100
}

###Absolute support value for Bronco, per state
Abs.bronco.edo.2.2<- list()
for (i in 1:32){
  Abs.bronco.edo.2.2[[i]]<- (sum(vbronco.edo.2[[i]])/sum(nominallist.edo.2[[i]]))*100
}


###Relative support value for PRI, per state
Rel.pri.edo.2.2<- list()
for (i in 1:32){
  Rel.pri.edo.2.2[[i]]<- (sum(vpri.edo.2[[i]])/sum(votos.edo.2[[i]]))*100
}

###Relative support value for MORENA, per state
Rel.morena.edo.2.2<- list()
for (i in 1:32){
  Rel.morena.edo.2.2[[i]]<- (sum(vmorena.edo.2[[i]])/sum(votos.edo.2[[i]]))*100
}

###Relative support value for PAN, per state
Rel.pan.edo.2.2<- list()
for (i in 1:32){
  Rel.pan.edo.2.2[[i]]<- (sum(vpan.edo.2[[i]])/sum(votos.edo.2[[i]]))*100
}

###Relative support value for Bronco, per state
Rel.bronco.edo.2.2<- list()
for (i in 1:32){
  Rel.bronco.edo.2[[i]]<- (sum(vbronco.edo.2[[i]])/sum(votos.edo.2[[i]]))*100
}

###################################
#####################Skewness###########
#########################################
###############
library(normtest)
##skewness is a measure of symmetry (or the lack of it)
##H0: the distribution follows a normal distribution
##H1: the distribution follows a normal distribution
## p-value small. H0 is rejected.


##Skewness test of PRI votes distribution, by state
skewness.edo.pri.2<-list()
for(i in 1:32){
  skewness.edo.pri.2[[i]] <-skewness.norm.test(data.edo.2[[i]]$pri)
}

##Skewness test of MORENA votes distribution, by state
skewness.edo.morena.2<-list()
for(i in 1:32){
  skewness.edo.morena.2[[i]] <-skewness.norm.test(data.edo.2[[i]]$v)
}

##Skewness test of Bronco's votes distribution, by state
skewness.edo.bronco.2<-list()
for(i in 1:32){
  skewness.edo.bronco.2[[i]] <-skewness.norm.test(data.edo.2[[i]]$Bronco)
}

##Skewness test of PAN votes distribution, by state
skewness.edo.pan.2<-list()
for(i in 1:32){
  skewness.edo.pan.2[[i]] <-skewness.norm.test(data.edo.2[[i]]$pan)
}

##Skewness test of total votes distribution, by state
skewness.edo.total.2<-list()
for(i in 1:32){
  skewness.edo.total.2[[i]] <-skewness.norm.test(data.edo.2[[i]]$t)
}

##Skewness test of turnout distribution, by state
skewness.edo.turnout.2<-list()
for(i in 1:32){
  skewness.edo.turnout.2[[i]] <-skewness.norm.test(data.edo.2[[i]]$turnout)
}

data.edo.2<-list()
index.edo<-c(1:32)
for (i in 1:32){
  data.edo.2[[i]] <- ndata2.2[ndata2.2$id.estado==index.edo[i],c(2:17)]
}
##Skewness test of Absolute Support for PRI, by state
Skew.Abs.pri.edo.2<-list()
for(i in 1:32){
  Skew.Abs.pri.edo.2[[i]] <- skewness.norm.test(data.edo.2[[i]]$AbsPRI)
}

##Skewness test of Absolute Support for MORENA, by state
Skew.Abs.morena.edo.2<-list()
for(i in 1:32){
  Skew.Abs.morena.edo.2[[i]] <- skewness.norm.test(data.edo.2[[i]]$Absmorena)
}

##Skewness test of Absolute Support for PAN, by state
Skew.Abs.pan.edo.2<-list()
for(i in 1:32){
  Skew.Abs.pan.edo.2[[i]] <- skewness.norm.test(data.edo.2[[i]]$AbsPAN)
}

##Skewness test of Absolute Support for Bronco, by state
Skew.Abs.bronco.edo.2<-list()
for(i in 1:32){
  Skew.Abs.bronco.edo.2[[i]] <- skewness.norm.test(data.edo.2[[i]]$Absbronco)
}

##Skewness test of Absolute Support for PRI, by state
Skew.Rel.pri.edo.2<-list()
for(i in 1:32){
  Skew.Rel.pri.edo.2[[i]] <- skewness.norm.test(data.edo.2[[i]]$RelPRI)
}

##Skewness test of Relative Support for MORENA, by state
Skew.Rel.morena.edo.2<-list()
for(i in 1:32){
  Skew.Rel.morena.edo.2[[i]] <- skewness.norm.test(data.edo.2[[i]]$Relmorena)
}

##Skewness test of Relative Support for PAN, by state
Skew.Rel.pan.edo.2<-list()
for(i in 1:32){
  Skew.Rel.pan.edo.2[[i]] <- skewness.norm.test(data.edo.2[[i]]$RelPAN)
}

##Skewness test of Relative Support for Bronco, by state
Skew.Rel.bronco.edo.2<-list()
for(i in 1:32){
  Skew.Rel.bronco.edo.2[[i]] <- skewness.norm.test(data.edo.2[[i]]$Relbronco)
}


###################################
####################Kurtosis###########
#########################################
###############
data.edo.2<-list()
index.edo<-c(1:32)
for (i in 1:32){
  data.edo.2[[i]] <- ndata2.2[ndata2.2$id.estado==index.edo[i],c(2:17)]
}


library(moments)
##kurtosis is a measure of the "tailedness" of the probability distribution of a real-valued random variable
##H0: the distribution follows a normal distribution
##H1: the distribution follows a normal distribution
## p-value small. H0 is rejected.



##kurtosis test of PRI votes distribution, by state
kurtosis.edo.pri.2<-list()
for(i in 1:32){
  kurtosis.edo.pri.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$pri)
}

##kurtosis test of MORENA votes distribution, by state
kurtosis.edo.morena.2<-list()
for(i in 1:32){
  kurtosis.edo.morena.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$v)
}

##kurtosis test of PAN votes distribution, by state
kurtosis.edo.pan.2<-list()
for(i in 1:32){
  kurtosis.edo.pan.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$pan)
}

##kurtosis test of Bronco votes distribution, by state
kurtosis.edo.bronco.2<-list()
for(i in 1:32){
  kurtosis.edo.bronco.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$Bronco)
}

##kurtosis test of total votes distribution, by state
kurtosis.edo.total.2<-list()
for(i in 1:32){
  kurtosis.edo.total.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$t)
}

##kurtosis test of turnout distribution, by state
kurtosis.edo.turnout.2<-list()
for(i in 1:32){
  kurtosis.edo.turnout.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$turnout)
}

##kurtosis test of Absolute Support for PRI, by state
kur.Abs.pri.edo.2<-list()
for(i in 1:32){
  kur.Abs.pri.edo.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$AbsPRI)
}

##kurtosis test of Absolute Support for MORENA, by state
kur.Abs.morena.2<-list()
for(i in 1:32){
  kur.Abs.morena.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$Absmorena)
}

##kurtosis test of Absolute Support for PAN, by state
kur.Abs.pan.edo.2<-list()
for(i in 1:32){
  kur.Abs.pan.edo.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$AbsPAN)
}

##kurtosis test of Absolute Support for Bronco, by state
kur.Abs.bronco.edo.2<-list()
for(i in 1:32){
  kur.Abs.bronco.edo.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$Absbronco)
}

##kurtosis test of Relative Support for PRI, by state
kur.Rel.pri.edo.2<-list()
for(i in 1:32){
  kur.Rel.pri.edo.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$RelPRI)
}

##kurtosis test of Relative Support for MORENA, by state
kur.Rel.morena.edo.2<-list()
for(i in 1:32){
  kur.Rel.morena.edo.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$Relmorena)
}

##kurtosis test of Relative Support for PAN, by state
kur.Rel.pan.edo.2<-list()
for(i in 1:32){
  kur.Rel.pan.edo.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$RelPAN)
}

##kurtosis test of Relative Support for Bronco, by state
kur.Rel.bronco.edo.2<-list()
for(i in 1:32){
  kur.Rel.bronco.edo.2[[i]] <- kurtosis.norm.test(data.edo.2[[i]]$Relbronco)
}



#####################################
#######################

####bendford analysis per State

data.edo.2<-list()
index.edo<-c(1:32)
for (i in 1:32){
  data.edo.2[[i]] <- ndata1.18.2[ndata1.18.2$id.estado==index.edo[i],c(2:11)]
}
library(benford.analysis)

###benford test of MORENA votes, by state
bfd.edomorena.2<-list()
for (i in 1:32){
  bfd.edomorena.2[[i]]<- benford(data.edo.2[[i]]$v, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
}


##plot bendford test of PRI votes in Aguascalientes
plot(bfd.edomorena.2[[1]])
plot(bfd.edomorena.2[[2]])
.
.
.

###benford test of total votes, by state
bfd.edototal.2<-list()
for (i in 1:32){
  benford(data.edo.2[[i]]$t, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
  bfd.edototal.2[[i]]  <- benford(data.edo.2[[i]]$t)
}

###benford test of PAN votes, by state
bfd.edopan.2<-list()
for (i in 1:32){
  bfd.edopan.2[[i]] <-benford(data.edo.2[[i]]$pan, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
}

###benford test of Bronco's votes, by state
bfd.edobronco.2<-list()
for (i in 1:32){
  bfd.edobronco.2[[i]] <-benford(data.edo.2[[i]]$Bronco, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
}

###benford test of PRI votes, by state
bfd.edopri.2<-list() 
for (i in 1:32){
  bfd.edopri.2[[i]] <-benford(data.edo.2[[i]]$pri, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
}

###benford test of turnout, by state
bfd.edoturnout.2<-list()
for (i in 1:32){
  bfd.edoturnout.2[[i]] <-benford(data.edo.2[[i]]$turnout, number.of.digits = 2, sign = "positive", discrete=TRUE, round=3)
}

####BendfordTests for the first, second and third digit.
################
library(BenfordTests)

###chisq.benftest of total votes, by state
##first digit
chisq.total.1d.edo.2<-list()
for (i in 1:32){
  chisq.total.1d.edo.2[[i]]<- chisq.benftest(data.edo.2[[i]]$t, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
}
##second digit
chisq.total.2d.edo.2<-list()
for (i in 1:32){
  chisq.total.2d.edo.2[[i]]<- chisq.benftest(data.edo.2[[i]]$t, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
}
##third digit
chisq.total.3d.edo.2<-list()
for (i in 1:32){
  chisq.total.3d.edo.2[[i]]<-chisq.benftest(data.edo.2[[i]]$t, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)
}


###chisq.benftest of PRI votes, by state
##first digit
chisq.pri.1d.edo.2<-list()
for (i in 1:32){
  chisq.pri.1d.edo.2[[i]]<- chisq.benftest(data.edo.2[[i]]$pri, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
}
##second digit
chisq.pri.2d.edo.2<-list()
for (i in 1:32){
  chisq.pri.2d.edo.2[[i]]<- chisq.benftest(data.edo.2[[i]]$pri, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
}
##third digit
chisq.pri.3d.edo.2<-list()
for (i in 1:32){
  chisq.pri.3d.edo.2[[i]]<-chisq.benftest(data.edo.2[[i]]$pri, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)
}


###chisq.benftest of MORENA votes, by state
##first digits
chisq.morena.1d.edo.2<-list()
for (i in 1:32){
  chisq.morena.1d.edo.2[[i]]<- chisq.benftest(data.edo.2[[i]]$v, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
}
##second digit
chisq.morena.2d.edo.2<-list()
for (i in 1:32){
  chisq.morena.2d.edo.2[[i]]<- chisq.benftest(data.edo.2[[i]]$v, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
}
##third digit
chisq.morena.3d.edo.2<-list()
for (i in 1:32){
  chisq.morena.3d.edo.2[[i]]<-chisq.benftest(data.edo.2[[i]]$v, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)
}


###chisq.benftest of PAN votes, by state
##first digit
chisq.pan.1d.edo.2<-list()
for (i in 1:32){
  chisq.pan.1d.edo.2[[i]]<- chisq.benftest(data.edo.2[[i]]$pan, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
}
##second digit
chisq.pan.2d.edo.2<-list()
for (i in 1:32){
  chisq.pan.2d.edo.2[[i]]<- chisq.benftest(data.edo.2[[i]]$pan, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
}
##third digit
chisq.pan.3d.edo.2<-list()
for (i in 1:32){
  chisq.pan.3d.edo.2[[i]]<-chisq.benftest(data.edo.2[[i]]$pan, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)
}


###chisq.benftest of Bronco's votes, by state
##first digit
chisq.bronco.1d.edo.2<-list()
for (i in 1:32){
  chisq.bronco.1d.edo.2[[i]]<- chisq.benftest(data.edo.2[[i]]$Bronco, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
}
##second digit
chisq.bronco.2d.edo.2<-list()
for (i in 1:32){
  chisq.bronco.2d.edo.2[[i]]<- chisq.benftest(data.edo.2[[i]]$Bronco, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
}
##third digit
chisq.bronco.3d.edo.2<-list()
for (i in 1:32){
  chisq.bronco.3d.edo.2[[i]]<-chisq.benftest(data.edo.2[[i]]$Bronco, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)
}


###chisq.benftest of turnout, by state
##first digit
chisq.turnout.1d.edo.2<-list()
for (i in 1:32){
  chisq.turnout.1d.edo.2[[i]]<- chisq.benftest(data.edo.2[[i]]$turnout, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000)
}
##second digit
chisq.turnout.2d.edo.2<-list()
for (i in 1:32){
  chisq.turnout.2d.edo.2[[i]]<- chisq.benftest(data.edo.2[[i]]$turnout, digits = 2, pvalmethod = "asymptotic", pvalsims = 10000)
}
#third digit
chisq.turnout.3d.edo.2<-list()
for (i in 1:32){
  chisq.turnout.3d.edo.2[[i]]<-chisq.benftest(data.edo.2[[i]]$turnout, digits = 3, pvalmethod = "asymptotic", pvalsims = 10000)
}


## In order to apply Rozenas Test we need 
##to work just with the poll stations with 100% or less of turnout
##we take only the values of N, v and t for this analysis
data.edo.2<-list()
index.edo<-c(1:32)
for (i in 1:32){
  data.edo.2[[i]] <- ndata5.2[ndata5.2$id.estado==index.edo[i],c(2:4)]
}

library(spikes)

plot(data.edo.2[[1]], main = 'Aguascalientes: distribution 2018')
plot(data.edo.2[[2]], main="Baja California: distribution 2018")
plot(data.edo.2[[3]], main="Baja California Sur: distribution 2018")
plot(data.edo.2[[4]], main="Campeche: distribution 2018")
plot(data.edo.2[[5]], main="Coahuila: distribution 2018")
plot(data.edo.2[[6]], main="Colima: distribution 2018")
plot(data.edo.2[[7]], main="Chiapas: distribution 2018")
plot(data.edo.2[[8]], main="Chihuahua: distribution 2018")
plot(data.edo.2[[9]], main="Distrito Federal: distribution 2018")
plot(data.edo.2[[10]], main="Durango: distribution 2018")
plot(data.edo.2[[11]], main="Guanajuato: distribution 2018")
plot(data.edo.2[[12]], main="Guerrero: distribution 2018")
plot(data.edo.2[[13]], main="Hidalgo: distribution 2018")
plot(data.edo.2[[14]], main="Jalisco: distribution 2018")
plot(data.edo.2[[15]], main="México: distribution 2018")
plot(data.edo.2[[16]], main="Michoacán: distribution 2018")
plot(data.edo.2[[17]], main="Morelos: distribution 2018")
plot(data.edo.2[[18]], main="Nayarit: distribution 2018")
plot(data.edo.2[[19]], main="Nuevo León: distribution 2018")
plot(data.edo.2[[20]], main="Oaxaca: distribution 2018")
plot(data.edo.2[[21]], main="Puebla: distribution 2018")
plot(data.edo.2[[22]], main="Querétaro: distribution 2018")
plot(data.edo.2[[23]], main="Quintana Roo: distribution 2018")
plot(data.edo.2[[24]], main="San Luis Potosí: distribution 2018")
plot(data.edo.2[[25]], main="Sinaloa: distribution 2018")
plot(data.edo.2[[26]], main="Sonora: distribution 2018")
plot(data.edo.2[[27]], main="Tabasco: distribution 2018")
plot(data.edo.2[[28]], main="Tamaulipas: distribution 2018")
plot(data.edo.2[[29]], main="Tlaxcala: distribution 2018")
plot(data.edo.2[[30]], main="Veracruz: distribution 2018")
plot(data.edo.2[[31]], main="Yucatán: distribution 2018")
plot(data.edo.2[[32]], main="Zacatecas: distribution 2018")

##to get the level of Estimated percentage of fraudulent precincts(analizing just PRI votes), by state

##to get the level of Estimated percentage of fraudulent precincts(analizing just PRI votes), by state


out1.18.2 <- spikes(data.edo.2[[1]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out2.18.2 <- spikes(data.edo.2[[2]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out3.18.2 <- spikes(data.edo.2[[3]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out4.18.2 <- spikes(data.edo.2[[4]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out5.18.2 <- spikes(data.edo.2[[5]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out6.18.2 <- spikes(data.edo.2[[6]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out7.18.2 <- spikes(data.edo.2[[7]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out8.18.2 <- spikes(data.edo.2[[8]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out9.18.2 <- spikes(data.edo.2[[9]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out10.18.2 <- spikes(data.edo.2[[10]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out11.18.2 <- spikes(data.edo.2[[11]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out12.18.2 <- spikes(data.edo.2[[12]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out13.18.2 <- spikes(data.edo.2[[13]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out14.18.2 <- spikes(data.edo.2[[14]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out15.18.2 <- spikes(data.edo.2[[15]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out16.18.2 <- spikes(data.edo.2[[16]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out17.18.2 <- spikes(data.edo.2[[17]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out18.18.2 <- spikes(data.edo.2[[18]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out19.18.2 <- spikes(data.edo.2[[19]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out20.18.2 <- spikes(data.edo.2[[20]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out21.18.2 <- spikes(data.edo.2[[21]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out22.18.2 <- spikes(data.edo.2[[22]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out23.18.2 <- spikes(data.edo.2[[23]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out24.18.2 <- spikes(data.edo.2[[24]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)##Extracts estimate of fraud and 95 percent credible interval (if such is estimated) for the object of
out25.18.2 <- spikes(data.edo.2[[25]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out26.18.2 <- spikes(data.edo.2[[26]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out27.18.2 <- spikes(data.edo.2[[27]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out28.18.2 <- spikes(data.edo.2[[28]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out29.18.2 <- spikes(data.edo.2[[29]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out30.18.2 <- spikes(data.edo.2[[30]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out31.18.2 <- spikes(data.edo.2[[31]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)
out32.18.2 <- spikes(data.edo.2[[32]], resamples = 1000, bw = 1e-04, grid = 1001, out = NULL)

##To plot the RDK results from Rozenas test for each state


plot(out1.18.2, main = "Rozenas Test: Aguascalientes 2018"  )
plot(out2.18.2, main = "Rozenas Test: Baja California 2018"  )
plot(out3.18.2, main = "Rozenas Test: Baja California Sur 2018"  )
plot(out4.18.2, main = "Rozenas Test: Campeche 2018" )
plot(out5.18.2, main = "Rozenas Test: Coahuila 2018"  )
plot(out6.18.2, main = "Rozenas Test: Colima 2018"  )
plot(out7.18.2, main = "Rozenas Test: Chiapas 2018"  )
plot(out8.18.2, main = "Rozenas Test: Chihuahua 2018"  )
plot(out9.18.2, main = "Rozenas Test: Distrito Federal 2018"  )
plot(out10.18.2, main = "Rozenas Test: Durango 2018"  )
plot(out11.18.2, main = "Rozenas Test: Guanajuato 2018"  )
plot(out12.18.2, main = "Rozenas Test: Guerrero 2018"  )
plot(out13.18.2, main = "Rozenas Test: Hidalgo 2018" )
plot(out14.18.2, main = "Rozenas Test: Jalisco 2018"  )
plot(out15.18.2, main = "Rozenas Test: México 2018"  )
plot(out16.18.2, main = "Rozenas Test: Michoacán 2018"  )
plot(out17.18.2, main = "Rozenas Test: Morelos 2018" )
plot(out18.18.2, main = "Rozenas Test: Nayarit 2018" )
plot(out19.18.2, main = "Rozenas Test: Nuevo León 2018" )
plot(out20.18.2, main = "Rozenas Test: Oaxaca 2018" )
plot(out21.18.2, main = "Rozenas Test: Puebla 2018" )
plot(out22.18.2, main = "Rozenas Test: Querétaro 2018" )
plot(out23.18.2, main = "Rozenas Test: Quintana Roo 2018"  )
plot(out24.18.2, main = "Rozenas Test: San Luis Potosí 2018" )
plot(out25.18.2, main ="Rozenas Test: Sinaloa 2018"  )
plot(out26.18.2, main = "Rozenas Test: Sonora 2018")
plot(out27.18.2, main = "Rozenas Test: Tabasco 2018"  )
plot(out28.18.2, main = "Rozenas Test: Tamaulipas 2018" )
plot(out29.18.2, main = "Rozenas Test: Tlaxcala 2018" )
plot(out30.18.2, main = "Rozenas Test: Veracruz 2018" )
plot(out31.18.2, main = "Rozenas Test: Yucatán 2018" )
plot(out32.18.2,  main = "Rozenas Test: Zacatecas 2018" )




##############################################
######Hartigans' dip test for unimodality / multimodality###
######################################
install.packages("diptest")
library(diptest)

dip.test(ndata1.18.2$turnout, simulate.p.value = FALSE, B = 2000)
dip.test(ndata1.18.2$v, simulate.p.value = FALSE, B = 2000)
dip.test(ndata1.18.2$pan, simulate.p.value = FALSE, B = 2000)

data.edo.2<-list()
index.edo<-c(1:32)
for (i in 1:32){
  data.edo.2[[i]] <- ndata1.18.2[ndata1.18.2$id.estado==index.edo[i],c(2:11)]
}

dip.test.edo.turnout.2<-list()
for(i in 1:32){
  dip.test.edo.turnout.2[[i]] <- dip.test(data.edo.2[[i]]$turnout, simulate.p.value = FALSE, B = 2000)
}

dip.test.edo.pri.2<-list()
for(i in 1:32){
  dip.test.edo.pri.2[[i]] <- dip.test(data.edo.2[[i]]$pri, simulate.p.value = FALSE, B = 2000)
}

dip.test.edo.morena.2<-list()
for(i in 1:32){
  dip.test.edo.morena.2[[i]] <- dip.test(data.edo.2[[i]]$v, simulate.p.value = FALSE, B = 2000)
}

dip.test.edo.pan.2<-list()
for(i in 1:32){
  dip.test.edo.pan.2[[i]] <- dip.test(data.edo.2[[i]]$pan, simulate.p.value = FALSE, B = 2000)
}


############################################
############################################
#######################################
#we get the regresion file with the extra variables to run the regressions
#rm(list=ls())
getwd()
setwd("F:/Warwick/Project")
datareg <- read.csv("Regresion.2.csv")
summary(datareg)


#####analisis for 2012
library(stats)

##to analize the effect of electoral manipulation on turnout


##to analyze Rozena 2017 as fraud measure 2012
roz2012.2 <- lm(datareg$turnout.12.2~ datareg$Rozenas.2.12+ datareg$votepri.12.2 + datareg$gini.12+ datareg$gdp.capi.12.2 + datareg$popularitypri.12.2 , datareg)
summary(roz2012.2)
confint(roz2012.2)

##bendford's law as fraud measure 2012
bendf2012.2 <- lm(datareg$turnout.12.2~  datareg$bendford.pri.12 + datareg$bendford.turnout.12+ datareg$votepri.12.2 + datareg$gini.12+ datareg$gdp.capi.12.2 + datareg$popularitypri.12.2 , datareg)
summary(bendf2012.2)
confint(bendf2012.2)

##unimodal's as fraud measure 2012
unimodal2012.2 <- lm(datareg$turnout.12.2~ datareg$Unimodalpri.12+ datareg$unimodal.turnout.12+ datareg$votepri.12.2 + datareg$gini.12+ datareg$gdp.capi.12.2 + datareg$popularitypri.12.2 , datareg)
summary(unimodal2012.2)
confint(unimodal2012.2)

##skewsness as fraud measure 2012
skew2012.2 <- lm(datareg$turnout.12.2~ datareg$skew.pri.12+ datareg$skew.turnout.12+ datareg$votepri.12.2 + datareg$gini.12+ datareg$gdp.capi.12.2 + datareg$popularitypri.12.2 , datareg)
summary(skew2012.2 )
confint(skew2012.2 )

##kurtosis as fraud measure 2012
kurt2012.2 <- lm(datareg$turnout.12.2~ datareg$kurt.pri.12+ datareg$kurt.turnout.12+ datareg$votepri.12.2 + datareg$gini.12+ datareg$gdp.capi.12.2 + datareg$popularitypri.12.2 , datareg)
summary(kurt2012.2)
confint(kurt2012.2)


#####analisis for 2018
library(stats)

##to analyze Rozenas 2 as fraud measure 2018
roz2018.1.2 <- lm(datareg$turnout.18.2~ datareg$rozenas.2.18+ datareg$votemorena.18.2 + datareg$gini.18+ datareg$gdp.capi.18.2 + datareg$popularitymorena.18.2 , datareg)
summary(roz2018.1.2)
confint(roz2018.1.2)

##bendford's law as fraud measure 2018
bendf2018.2 <- lm(datareg$turnout.18.2~ datareg$bendford.turnout.18+ datareg$bendford.morena.18+ datareg$votemorena.18.2 + datareg$gini.18+ datareg$gdp.capi.18.2 + datareg$popularitymorena.18.2, datareg)
summary(bendf2018.2)
confint(bendf2018.2)

##unimodal's
unimodal2018.2 <- lm(datareg$turnout.18.2~ datareg$Unimodalmorena.18+ datareg$unimodal.turnout.18+ datareg$votemorena.18.2 + datareg$gini.18+ datareg$gdp.capi.18.2 + datareg$popularitymorena.18.2 , datareg)
summary(unimodal2018.2)
confint(unimodal2018.2)

##skewsness as fraud measure 2018
skew2018.2 <- lm(datareg$turnout.18.2~ datareg$skew.morena.18+ datareg$skew.turnout.18+ datareg$votemorena.18.2+ datareg$gini.18+ datareg$gdp.capi.18.2 + datareg$popularitymorena.18.2 , datareg)
summary(skew2018.2 )
confint(skew2018.2 )

##kurtosis as fraud measure 2018
kurt2018.2 <- lm(datareg$turnout.18.2~ datareg$kurt.morena.18+ datareg$kurt.turnout.18+ datareg$votemorena.18.2 + datareg$gini.18+ datareg$gdp.capi.18.2 + datareg$popularitymorena.18.2 , datareg)
summary(kurt2018.2)
confint(kurt2018.2)


an2.roz1.2<-anova(roz2018.1.2, roz2012.2, test = "Chisq")
an2.ben.2<-anova(bendf2018.2, bendf2012.2, test = "Chisq")
an2.uni.2<-anova(unimodal2018.2, unimodal2012.2, test = "Chisq")
an2.skew.2<-anova(skew2018.2, skew2012.2, test = "Chisq")
an2.kurt.2<-anova(kurt2018.2, kurt2012.2, test = "Chisq")

