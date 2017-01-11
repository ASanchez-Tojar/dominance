# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 3rd of March, 2016
# Script last updated on the 9th of August, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script analyze the captive data and creates a final database
# for the statistical analyses.
#


########################################################################################################
# Packages needed
########################################################################################################

# packages needed to be loaded for this script (a couple of them might be only needed in the following
# script)

library(doBy)
library(reshape)
library(EloRating)
library(statnet)
library(plyr)
library(EloChoice)


# Clear memory and get to know where you are
rm(list=ls())
#getwd()



#########################################################################################################
# 13. CAPTIVE ANALYSES
#########################################################################################################

#########################################################################################################
# # 13.1. Getting the database ready for generating the StElo
#########################################################################################################

# loading the database

dom.cap <- read.table("captive-dom-levels-v1.csv",header=TRUE,sep=",")


# getting rid off some typos (e.g. level==0) and displacements

dom.cap.2 <- subset(dom.cap,dom.cap$Level!=0)

dom.cap.3 <- subset(dom.cap.2,dom.cap.2$Level!=1)


# assigning the aviary to each individual (i.e. Losers and Winners)

dom.cap.3$Loser_Aviary <- factor(paste(dom.cap.3$Loser,dom.cap.3$Aviary,sep="_"))

dom.cap.3$Winner_Aviary <- factor(paste(dom.cap.3$Winner,dom.cap.3$Aviary,sep="_"))


# loading dataset with age and ID!

ID <- read.table("captivesparrowsage2014-v2.csv",header=TRUE,sep=",")


# generating a colour code_Aviary identifier

ID$newcc_NewAv <- factor(paste(ID$newcc,ID$NewAv,sep="_"))

ID.2 <- ID[,c("newcc_NewAv","BTO")]


# Merging both databases in order to get the BTO (i.e. unique identifier)
# of each individual

dom.cap.ID <- merge(dom.cap.3,ID.2,
                    by.x="Loser_Aviary",by.y="newcc_NewAv",
                    all.x=TRUE)


# There seem to be some other typos (because BTO==NA in 9 cases)
# I therefore get rid off them

dom.cap.ID.2 <- subset(dom.cap.ID,!(is.na(dom.cap.ID$BTO)))


# I keep building the final database

dom.cap.ID.3 <- rename(dom.cap.ID.2, c(BTO="Loser2"))

dom.cap.ID.4 <- merge(dom.cap.ID.3,ID.2,
                      by.x="Winner_Aviary",by.y="newcc_NewAv",
                      all.x=TRUE)

dom.cap.ID.5 <- rename(dom.cap.ID.4, c(BTO="Winner2"))


# now the BTO's of Losers and Winners are added. It is time for the final database
# First, cleaning a bit, I don't need the rest of the variables.

final.dom.cap <- dom.cap.ID.5[,c("Date","Aviary","Loser2","Winner2","Level","Draw")]


# Then, renaming back and make date a factor. I want to exclude February from analyses
# This is because I'm missing data on the levels for part of February and Abril. Moises
# has it but I don't manage to contact him.

final.dom.cap.2 <- rename(final.dom.cap, c(Loser2="Loser",Winner2="Winner"))

final.dom.cap.2$Date <- factor(final.dom.cap.2$Date)

final.dom.cap.3 <- subset(final.dom.cap.2,final.dom.cap.2$Date!="2015-02-16")
final.dom.cap.3 <- subset(final.dom.cap.3,final.dom.cap.3$Date!="2015-02-23")


# re-setting the levels of the factor

final.dom.cap.3$Date <- factor(final.dom.cap.3$Date)


#########################################################################################################
# # 13.2. Obtaining the elo-scores for each individual
#########################################################################################################

# First, spliting the database by aviary

#     Creating a different database per aviary with this for loop, and at the same time, creating a 
# database per aviary with elo_scores

seqcheck(winner=final.dom.cap.3$Winner, 
         loser=final.dom.cap.3$Loser, 
         Date=final.dom.cap.3$Date, 
         draw=final.dom.cap.3$Draw)

# there are some further typos, Loser equals Winner, I'm going to remove them from the database

final.dom.cap.3<-final.dom.cap.3[final.dom.cap.3$Loser!=final.dom.cap.3$Winner,]

final.dom.cap.3$Aviary <- as.factor(final.dom.cap.3$Aviary)


########################################################################################################
# # # 13.2.1. Generating a list of the individuals included in the database
########################################################################################################

# Before I proceed to estimate the StElo of each individual, I want to do some countings

#       This generates a list of the individiuals by combining the columns Loser and Winner
# This way we can count interactions by counting number of times that each individual shows up in the
# list.


# List with all individuals interacting as Loser and converting it as data.frame

list.Loser <- final.dom.cap.3$Loser

list.Loser <- as.data.frame(list.Loser)


# List with all individuals interacting as Winner and converting it as dataframe

list.Winner <- final.dom.cap.3$Winner

list.Winner <- as.data.frame(list.Winner)


#     Here I rename the variable in list.Winner and list.Loser so that they have the same names, otherwise, rbind 
# does not work the way I want it to work

names(list.Loser) <- c("BirdID")
names(list.Winner) <- c("BirdID")


# Now I construct a variable by rbinding together the 2 dataframes

list.x <- rbind(list.Loser,list.Winner)


########################################################################################################
# # # 13.2.2. Counting number of interactions per individual for the whole database
########################################################################################################

# Generating a database with the individuals and their number of total interactions in the whole dataset

ind.counts <- count(list.x,"BirdID")

names(ind.counts)<-c("BirdID","totalfreq")


# Saving a histogram on the number of interactions

#to save the figure as tiff
tiff("plots/hist_interacions_Seewiesen.tiff",
     height=18, width=27,units='cm', compression="lzw", res=300)

#number of dates interacting per individual
hist(ind.counts$totalfreq,freq=TRUE,breaks=65,
     main = "",
     xlab = "Number of interactions",
     ylim = c(0,7),
     xlim = c(0,325),
     ylab = "",
     col="grey75",
     axes=FALSE,
     cex.lab=1.75,
     right=FALSE)
title(ylab="Number of individuals", line=2.2, cex.lab=1.75)
axis(1,at = seq(0,325,by=25),lwd=1)
axis(2,at = seq(0,7,by=1),lwd=1,line=-0.75, las=2)

dev.off()

########################################################################################################
# # # 13.2.3. Counting number of interactions per individual per date
########################################################################################################

#       Generating a database with the individuals and their number of interactions per date. This way I
# know which individuals are seen in more than one date

# Creating two data.frames: one with Loser and the date, and another with Winner and the date

list.Loser.date<-final.dom.cap.3[,c("Loser","Date")]
list.Winner.date<-final.dom.cap.3[,c("Winner","Date")]


# Renaming the variables so that they have the same name for the rbind

names(list.Loser.date) <- c("BirdID","date")
names(list.Winner.date) <- c("BirdID","date")


# Pasting them together using rbind

superlist.date<-rbind(list.Loser.date,list.Winner.date)


#       Now I can count the number of times each individual showed up in each date by counting the
# number of dates each individual showed up in

ind.counts.date <- count(superlist.date,c("BirdID","date"))

names(ind.counts.date) <- c("BirdID","date","freqperdate")

#hist(ind.counts.date$freqperdate,breaks=20)


# Saving a histogram on the number of interactions

#to save the figure as tiff
tiff("plots/hist_interacions_per_date_Seewiesen.tiff",
     height=18, width=27,units='cm', compression="lzw", res=300)

#number of dates interacting per individual
hist(ind.counts.date$freqperdate,freq=TRUE,breaks=102,
     main = "",
     xlab = "Number of interactions",
     ylim = c(0,80),
     xlim = c(0,105),
     ylab = "",
     col="grey75",
     axes=FALSE,
     cex.lab=1.75,
     right=FALSE)
title(ylab="Number of individuals", line=2.2, cex.lab=1.75)
axis(1,at = seq(0,105,by=15),lwd=1)
axis(2,at = seq(0,80,by=10),lwd=1,line=-0.75, las=2)

dev.off()


########################################################################################################
# # # 13.2.4. Counting number of dates each individual showed up in
########################################################################################################

# This allows me to count the number of dates each individual showed up in

onlyind <- ind.counts.date$BirdID

onlyind <- as.data.frame(onlyind)


# Counting number of dates per individual

superlist.num.date <- count(onlyind,"onlyind")

names(superlist.num.date) <- c("BirdID","freqofdates")


# Saving a histogram on the number of interactions

#to save the figure as tiff
tiff("plots/hist_dates_per_individual_Seewiesen.tiff",
     height=18, width=27,units='cm', compression="lzw", res=300)

#number of dates interacting per individual
hist(superlist.num.date$freqofdates,freq=TRUE,breaks=10,
     main = "",
     xlab = "Number of days",
     ylim = c(0,70),
     xlim = c(1,10),
     ylab = "",
     col="grey75",
     axes=FALSE,
     cex.lab=1.75,
     right=FALSE)
title(ylab="Number of individuals", line=2.2, cex.lab=1.75)
axis(1,at = seq(1,10,by=1),lwd=1)
axis(2,at = seq(0,70,by=10),lwd=1,line=-0.75, las=2)

dev.off()


# ########################################################################################################
# # # 13.3. Obtaining the elo-scores for each individual
# ########################################################################################################
# 
# ########################################################################################################
# # # # 13.3.1. Analyzing each aviary
# ########################################################################################################
# 
# counter <- 1
# 
# for(i in levels(final.dom.cap.3$Aviary)){
#   
#   x<-subset(final.dom.cap.3, final.dom.cap.3$Aviary==i)
#   assign(paste0("final.com.cap",counter),x)
#   y<-elo.seq(winner=x$Winner, 
#              loser=x$Loser, 
#              Date=x$Date, 
#              draw=x$Draw)
#   print(summary(y))
#   assign(paste0("cap_elo_scores.",counter),y)
#   counter <- counter + 1
# }
# 
# 
# ########################################################################################################
# # # # 13.3.2. Stability coefficient
# ########################################################################################################
# 
# sink("summaries/stabilitycoefficient_Seewiesen.txt")
# 
# cat("\nAviary 9: ")
# stab.elo(cap_elo_scores.1)
# 
# cat("\nAviary 10: ")
# stab.elo(cap_elo_scores.2)
# 
# cat("\nAviary 11: ")
# stab.elo(cap_elo_scores.3)
# 
# cat("\nAviary 12: ")
# stab.elo(cap_elo_scores.4)
# 
# sink()
# 
# ########################################################################################################
# # # # 13.3.3. Proportion of unknown dyads
# ########################################################################################################
# 
# # First I will create the matrixes for each Aviary, this is because I didn't manage
# # to make prunk() work using cap_elo_scores.X (eventhough I did for the previous script)
# 
# cap_dyad_matrix.1 <- creatematrix(cap_elo_scores.1,
#                               drawmethod="0.5") 
# 
# cap_dyad_matrix.2 <- creatematrix(cap_elo_scores.2, 
#                               drawmethod="0.5") 
# 
# cap_dyad_matrix.3 <- creatematrix(cap_elo_scores.3, 
#                               drawmethod="0.5") 
# 
# cap_dyad_matrix.4 <- creatematrix(cap_elo_scores.4, 
#                               drawmethod="0.5") 
# 
# # And now I can run prunk and print the results in a .txt
# 
# sink("summaries/prop_unknown_dyads_Seewiesen.txt")
# 
# cat("\nAviary 9\n")
# prunk(cap_dyad_matrix.1)
# 
# cat("\nAviary 10\n")
# prunk(cap_dyad_matrix.2)
# 
# cat("\nAviary 11\n")
# prunk(cap_dyad_matrix.3)
# 
# cat("\nAviary 12\n")
# prunk(cap_dyad_matrix.4)
# 
# sink()
# 
# 
# ########################################################################################################
# # # # 13.3.4. Extracting elo-ratings per individual
# ########################################################################################################
# 
# # this can be probably included in the for loop of line 2758
# 
# cap.elo_scores_ind.1 <- extract.elo(cap_elo_scores.1,standardize = TRUE)
# cap.elo_scores_ind.2 <- extract.elo(cap_elo_scores.2,standardize = TRUE)
# cap.elo_scores_ind.3 <- extract.elo(cap_elo_scores.3,standardize = TRUE)
# cap.elo_scores_ind.4 <- extract.elo(cap_elo_scores.4,standardize = TRUE)
# 
# 
# #     This code is to make a dataframe out of those individuals scores, please, change the number for the
# # event you want!
# 
# # creating the name of the rows, basically from 1 to total number of individuals
# 
# # Creating a database with elo_scores per event
# 
# counter <- 1
# 
# for(i in levels(final.dom.cap.3$Aviary)){
#   
#   x<-subset(final.dom.cap.3, final.dom.cap.3$Aviary==i)
#   y<-elo.seq(winner=x$Winner, 
#              loser=x$Loser, 
#              Date=x$Date, 
#              draw=x$Draw)
#   
#   w<-extract.elo(y,standardize = TRUE)
#   
#   rownames <- seq(1,length(w),
#                   1)
#   
#   # making a data.frame with the elo-ratings
#   scores <- as.data.frame(w,
#                           row.names = as.character(rownames))
#   
#   z <- cbind(attributes(w),
#              scores)
#   
#   z$Aviary <- counter
#   
#   names(z) <- c("individual","StElo","Aviary")
#   
#   assign(paste0("cap_elo_scores_ind.db.",counter),z)
#   
#   counter <- counter + 1
#   
# }
# 
# 
# # creating a database with all these observations
# cap_elo_scores_all_events <- rbind(cap_elo_scores_ind.db.1,
#                                    cap_elo_scores_ind.db.2,
#                                    cap_elo_scores_ind.db.3,
#                                    cap_elo_scores_ind.db.4)


########################################################################################################
# 1. Analyzing elo-random using EloChoice()
########################################################################################################

#using elochoice() to quicly estimate randomized Elo-ratings
counter <- 1

for(i in levels(final.dom.cap.3$Aviary)){
  
  x<-subset(final.dom.cap.3, final.dom.cap.3$Aviary==i)
  assign(paste0("final.com.cap",counter),x)
  y<-ratings(elochoice(x$Winner,x$Loser,
                       kval=200,startvalue=1000,
                       normprob=TRUE,runs=1000),
             drawplot=FALSE)
  w<-scale.elo(y)
  assign(paste0("cap_elo_scores.",counter),w)
  rownames <- seq(1,length(w),
                  1)
  scores <- as.data.frame(w,
                          row.names = as.character(rownames))
  
  z <- cbind(attributes(w),
             scores)
  
  z$eventSW <- counter
  
  names(z) <- c("individual","StElo","Aviary")
  
  assign(paste0("cap.elo_scores_ind.",counter),z)
  counter <- counter + 1
}


# creating a database with all these observations
cap_elo_scores_all_events <- rbind(cap.elo_scores_ind.1,
                                   cap.elo_scores_ind.2,
                                   cap.elo_scores_ind.3,
                                   cap.elo_scores_ind.4)

#########################################################################################################
# # 13.4. Adding the phenotypic data to the StElo data
#########################################################################################################

# importing database with phenotypic data

db<-read.table("DataBase_Badge_and_dominance_Seewiesen_Winter2014_20150411-ASTvariablesadded_v6_no24noMSFB_masscorrected-commas20160414.csv",
               header=TRUE,sep=',')


#transforming the necessary variables

db$Av<-as.factor(db$Av)
db$weeknumber2<-as.numeric(db$weeknumber2)
db$Ring.ID<-as.factor(db$Ring.ID)
db$Mass<-as.numeric(db$Mass)

#to remove the birds from which we don't have measurements, i.e. those that died during the experiment
db.noNA <- subset(db,db$VB1 != "")


# adding age to db.noNA

ID.age <- ID[,c("BTO","age2014")]


# adding 0.5 to make it comparable to what we have on Lundy

ID.age$age2014 <- ID.age$age2014+0.5


# merging it to the previous database

db.noNA.age<-merge(db.noNA,ID.age,by.x="Ring.ID",by.y="BTO",all.x=TRUE)


# reducing the database to my own measurements
# This is because we found that the other observer wasn't consistent enough in the measurements.
# She showed a steep increase from week 1 to 10 in the measurements.

db.noNA.age.AST<-subset(db.noNA.age,db.noNA.age$Obs=="AST")

# mod.bib.age.rpt <- lmer(meanVB~
#                           #scale(age2014)+
#                           (1|Ring.ID)+
#                           (1|weeknumber2),
#                         data=db.noNA.age.AST)
# 
# smod.bib.age.rpt<-sim(mod.bib.age.rpt,5000)
# 
# round(apply(smod.bib.age.rpt@fixef,2, mean),3)
# round(apply(smod.bib.age.rpt@fixef,2, quantile, c(0.025, 0.975)),3)
# 
# ID <- round(mean(apply(smod.bib.age.rpt@ranef$Ring.ID,1, var)),3)
# round(quantile(apply(smod.bib.age.rpt@ranef$Ring.ID,1, var),
#                c(0.025, 0.975)),3)
# 
# Event <- round(mean(apply(smod.bib.age.rpt@ranef$weeknumber2,1, var)),3)
# round(quantile(apply(smod.bib.age.rpt@ranef$weeknumber2,1, var),
#                c(0.025, 0.975)),3)
# 
# resid <- round(mean(smod.bib.age.rpt@sigma),3)
# round(quantile(smod.bib.age.rpt@sigma,c(0.025, 0.975)),3)
# 
# repeatability <- ID/(ID+Event+resid)

# mean VB and age, and then adding tarsus

id.meanVB.age <-summaryBy(meanVB + age2014 ~ Ring.ID, data = db.noNA.age.AST, 
                          FUN = list(mean))

# plotting histogram
p <- paste(", N=",nrow(id.meanVB.age))
x<-range(id.meanVB.age$meanVB.mean)
ran<-round(x[[2]]-x[[1]],0)
hist(id.meanVB.age$meanVB.mean,xlim = c(30,60),ylim=c(0,25),
     breaks=ran,
     main=paste("Captivity",p),col="grey75")
lines(c(mean(id.meanVB.age$meanVB.mean),mean(id.meanVB.age$meanVB.mean)),
      c(0,28),col="blue",lty=3,lwd=2.5)


id.tarsus <- subset(db.noNA.age.AST,!(is.na(db.noNA.age.AST$TarsusLength)))

id.tarsus2 <- id.tarsus[,c("Ring.ID","TarsusLength")]


id.meanVB.age.TL <- merge(id.meanVB.age,id.tarsus2,by="Ring.ID",all.x=TRUE)


# I also want to add the average meanVB restricted only to the first 10 weeks

db.noNA.age.AST.10weeks <- subset(db.noNA.age.AST,db.noNA.age.AST$weeknumber2<11)

id.meanVB.age.10weeks <-summaryBy(meanVB ~ Ring.ID, data = db.noNA.age.AST.10weeks, 
                                  FUN = list(mean))

# the mean value of measurements per individual is: 

numberofmeasurements <- as.numeric(table(db.noNA.age.AST.10weeks$Ring.ID))

summary(numberofmeasurements[numberofmeasurements!=0])


# I then add it to the database

id.meanVB.age.10weeks.2 <- rename(id.meanVB.age.10weeks, c(meanVB.mean="meanVB.mean10"))

id.meanVB.age.TL.2 <- merge(id.meanVB.age.TL,id.meanVB.age.10weeks.2,
                            by="Ring.ID", all.x=TRUE)


# Final database to run the analyses

final.cap.db <- merge(cap_elo_scores_all_events,id.meanVB.age.TL.2,
                      by.x="individual",by.y="Ring.ID",all.x=TRUE)


# Now I want to mean centre VB per aviary. This makes more sense for the analyses

WithinIndCentr <- function(x) x-mean(x,na.rm = TRUE)

final.cap.db.2 <- ddply(final.cap.db, 
                        c("Aviary"), 
                        transform, 
                        meanVBcentredAv = WithinIndCentr(meanVB.mean))


final.cap.db.3 <- ddply(final.cap.db.2, 
                        c("Aviary"), 
                        transform, 
                        meanVB.10centredAv = WithinIndCentr(meanVB.mean10))


# I'm saving this file for the following scripts

write.csv(final.cap.db.3,"final.cap.db.3.csv",row.names=FALSE)
