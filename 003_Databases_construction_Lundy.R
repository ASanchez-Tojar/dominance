
# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 3rd of March, 2016
# Script last updated on the 8th of August, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to get the databases ready for statistical analyses. This script needs data from the
# previous scripts (001_Dominance_databases_cleaning_and_summary and 002_Elo-rating_estimation).
#
# The section 10.1: it creates a database with all the individuals we have StElo from and adds
#                   all the individual information needed
#
# The section 10.2: it creates a database with all the individuals we have bib from and adds
#                   all the individual information needed
#
# The section 10.3: it creates a database with all the individuals we have age from and adds
#                   all the individual information needed


########################################################################################################
# Packages needed
########################################################################################################

# packages needed to be loaded for this script (a couple of them might be only needed in the following
# script)

library(doBy)
library(lme4)
library(arm)
library(plyr)


# Clear memory and get to know where you are
rm(list=ls())
#getwd()


# loading the clean database to estimate the ratings

elo_scores_all_events <- read.table("elo_scores_all_events.csv",header=TRUE,sep=",")
birdsex.1 <- read.table("birdsex.1.csv",header=TRUE,sep=",")
morethan8pereventSW <- read.table("morethan8pereventSW.csv",header=TRUE,sep=",")


#########################################################################################################
# 10. Getting all databases ready
#########################################################################################################

#########################################################################################################
# # 10.0. Generating the fitness databases that will be needed throughout
#########################################################################################################

#########################################################################################################
# # # 10.0.1 Social fitness
#########################################################################################################

# importing database with all those males that attempted breeding from 2014 to the 15th of July, 2016

male.breeding <- read.table("allbreedingbirdsfrom2014-20160715-2.csv",header=TRUE,sep=",")


# First, exclude IDCertain=FALSE

male.breeding2 <- male.breeding[male.breeding$SocialDadCertain==TRUE,]


# Then create and identifier

male.breeding2$BirdID_BroodName <- factor(paste(male.breeding2$SocialDadID,male.breeding2$BroodName,sep="_"))

male.breeding2 <- male.breeding2[,c("BirdID_BroodName","SocialDadID","BroodName")]


# getting the year for the estimates to estimate the annual number of fledglings/recruits

male.breeding2$yearCode <- factor(substr(male.breeding2$BroodName, 1, 1))

male.breeding2$year <- ifelse(male.breeding2$yearCode=="N",
                              2014,
                              ifelse(male.breeding2$yearCode=="O",
                                     2015,2016))

male.breeding2$BirdID_eventSW <- factor(paste(male.breeding2$SocialDadID,
                                              male.breeding2$year,
                                              sep="_"))

male.breeding3 <- male.breeding2[,c("BirdID_BroodName","BirdID_eventSW","SocialDadID","BroodName","year")]


#########################################################################################################
# # # # 10.0.1.1 (social) annual number of fledglings
#########################################################################################################

# importing database with all those males that got some ringed fledglings from 2014 to the 15th of July, 2016

male.fledglings <- read.table("leavingfledglingsfrom2014to20160715.csv",header=TRUE,sep=",")

male.fledglings$BirdID_BroodName <- factor(paste(male.fledglings$SocialDadID,male.fledglings$BroodName,sep="_"))

male.fledglings2 <- male.fledglings[,c("BirdID_BroodName","ChickCoun2")]


# Now we merge those two databases to include 0s in too

male.social.fledglings <- merge(male.breeding3,male.fledglings2,by="BirdID_BroodName",all.x=TRUE)


# convert the NA's in 0 as that's what they are (exceptions for the last layed broods from this year)

male.social.fledglings$socfledglings <- ifelse(is.na(male.social.fledglings$ChickCoun2),
                                               0,male.social.fledglings$ChickCoun2)


# getting the number of fledglings produced per year per individual

male.social.annualfledglings<-summaryBy(socfledglings ~ BirdID_eventSW,
                                        data = male.social.fledglings, 
                                        FUN = list(sum),na.rm=TRUE)


#########################################################################################################
# # # # 10.0.1.2 (social) annual number of recruits
#########################################################################################################

# this SQL code for this database is:

# SELECT tblBirdID.BirdID, tblBirdID.Cohort, tblBroods_1.SocialDadID, tblBroods_1.BroodName
# FROM (tblBirdID INNER JOIN tblBroods ON tblBirdID.BirdID = tblBroods.SocialDadID) INNER JOIN tblBroods AS tblBroods_1 ON tblBirdID.BroodRef = tblBroods_1.BroodRef
# WHERE (((tblBirdID.Cohort)>2013 And (tblBirdID.Cohort)<2016));

# This database includes all birds in 2014 and 2015 that became social fathers themselves. All these
# recruits are assigned to their social dad ID, this way, we can count the number of recurits per
# social Dad for each year.

male.recruits <- read.table("leavingrecruitsfrom2014to2015.csv",header=TRUE,sep=",")


# get rid off those without socialDadID as I cannot do anything with them.

male.recruits2 <- subset(male.recruits,!(is.na(male.recruits$SocialDadID)))


# removing duplicated rows

male.recruits3 <- unique(male.recruits2)


# counting the number of recruits per social Dad breeding in 2014 and 2015

recruit.per.Dad <- count(male.recruits3,c("SocialDadID","Cohort"))


recruit.per.Dad$BirdID_eventSW <- factor(paste(recruit.per.Dad$SocialDadID,
                                               recruit.per.Dad$Cohort,
                                               sep="_"))

recruit.per.Dad2 <- recruit.per.Dad[,c("BirdID_eventSW","freq")]


# Now I need to add 0 values for those that didn't get any recruit

# Now we merge those two databases to include 0s in too, but first I need a modification of male.breeding3

male.breeding4 <- male.breeding3[c("BirdID_eventSW","SocialDadID","year")]

male.breeding5 <- unique(subset(male.breeding4,male.breeding4$year!=2016))

male.social.recruits <- merge(male.breeding5,recruit.per.Dad2,by="BirdID_eventSW",all.x=TRUE)


# convert the NA's in 0 as that's what they are (exceptions for the last layed broods from this year)

male.social.recruits$socrecruits <- ifelse(is.na(male.social.recruits$freq),
                                           0,male.social.recruits$freq)


# last thing, make the ultimate recruit database

male.social.annualrecruits <- male.social.recruits[,c("BirdID_eventSW","socrecruits")]


#########################################################################################################
# # 10.1. RANK database containing all the neccessary information
#########################################################################################################

#########################################################################################################
# # # 10.1.1. Re-naming events as the year and .0/.5 depending on whether it was winter or summer
#########################################################################################################

#     elo_scores_all_events contains the elo-ratings per individual per winter/summer event, I just want
# to change the value of eventSW from 1-6 to 2013.5-2016.0, to use it to estimate age later on.

for(i in 1:nrow(elo_scores_all_events)){
  if(elo_scores_all_events$eventSW[i]==1){
    elo_scores_all_events$eventSW[i]<-as.numeric(2013.5)
  } else if(elo_scores_all_events$eventSW[i]==2){
    elo_scores_all_events$eventSW[i]<-as.numeric(2014.0)
  } else if(elo_scores_all_events$eventSW[i]==3){
    elo_scores_all_events$eventSW[i]<-as.numeric(2014.5)
  } else if(elo_scores_all_events$eventSW[i]==4){
    elo_scores_all_events$eventSW[i]<-as.numeric(2015.0)
  } else if(elo_scores_all_events$eventSW[i]==5){
    elo_scores_all_events$eventSW[i]<-as.numeric(2015.5)
  } else if(elo_scores_all_events$eventSW[i]==6){
    elo_scores_all_events$eventSW[i]<-as.numeric(2016.0)
  }
}


#########################################################################################################
# # # 10.1.2. Adding sex, cohort and age
#########################################################################################################

# With this one I'm getting cohort and sex from birdsex.1, and merging it with elo_scores later on

ccbirdidcohort <- birdsex.1[,c("Code","BirdID","SexEstimate","Cohort","CohortEstimated")]


# adding that information to the elo_scores_all_events. "yuhu" is the new database with all info

elo_scores_all_events_age <- merge(elo_scores_all_events,ccbirdidcohort,
                                   by.x="individual",by.y="Code",
                                   all.x=TRUE)


# estimating age as 0 (fledgling in summer), 0.5 (first winter), 1 (first summer), 1.5 (2nd winter)...

elo_scores_all_events_age$age <- elo_scores_all_events_age$eventSW - elo_scores_all_events_age$Cohort


# individual identifier per event

elo_scores_all_events_age$BirdID_eventSW <- paste(elo_scores_all_events_age$BirdID,
                                                  elo_scores_all_events_age$eventSW,sep="_")


elo_scores_all_events_age$BirdID_eventSW <- factor(elo_scores_all_events_age$BirdID_eventSW)


# Sorting the database according to individual colour code and eventSW

elo_scores_all_events_age <- elo_scores_all_events_age[order(elo_scores_all_events_age$individual, 
                                                             elo_scores_all_events_age$eventSW),]


# the mean value of StElos per individual for the whole study period: 

numberofStEloperbird <- 
  as.numeric(table(as.factor(elo_scores_all_events_age$BirdID)))

summary(numberofStEloperbird[numberofStEloperbird!=0])



#########################################################################################################
# # # 10.1.3. Adding tarsus length, mass and visible badge
#########################################################################################################

# loading the database with tarsus length and mass (Stage=3, so it doesn't include measures in nest)

tarsuslengthandMass <- read.table("BirdID-Tarsus-Mass.csv",header=TRUE,sep=",")


# the mean value of tarsus measurements per individual is: 

numberofTLmeasurements <- as.numeric(table(as.factor(tarsuslengthandMass$BirdID)))

summary(numberofTLmeasurements[numberofTLmeasurements!=0])


# loading the database with Visible Badge

visibleBadge <-  read.table("BirdID-VisibleBadge.csv",header=TRUE,sep=",")


# #########################################################################################################
# # # # # 10.1.3.0. Estimating tarsus length repeatability  
# #########################################################################################################
# 
# # first I get rid off Observer = NA, and also those that have less than 10 measurements,
# # i.e. CM, HD, TB
# 
# tarsuslength.rep <- subset(tarsuslengthandMass,
#                                   tarsuslengthandMass$Observer!="")
# 
# tarsuslength.rep.2 <- subset(tarsuslength.rep,
#                              tarsuslength.rep$Observer!="CM" &
#                                tarsuslength.rep$Observer!="HD" &
#                                tarsuslength.rep$Observer!="TB")
# 
# mod.tarsus.rep <- lmer(Tarsus ~ 1 + (1|BirdID) + (1|Observer), data=tarsuslength.rep.2)
# 
# BirdID.var <- 0.65435
# Observer.var <- 0.03409
# Residual.var <- 0.13961
# total.variance <- BirdID.var+Observer.var+Residual.var
# 
# repeatability <- BirdID.var/total.variance
# Observer.noise <- Observer.var/total.variance

#########################################################################################################
# # # # 10.1.3.1. Mean tarsus length and Mass per individual
#########################################################################################################

ind.TLandM <- summaryBy(Tarsus + Mass ~ BirdID, data = tarsuslengthandMass, 
                        FUN = list(mean), na.rm=TRUE)


#########################################################################################################
# # # # 10.1.3.2. Mean Visible Badge per individual
#########################################################################################################

# I'm only going to use my measurements

#lifehistory.VB <- subset(visibleBadge,visibleBadge$Observer=="AST")
visibleBadge.AST <- subset(visibleBadge,visibleBadge$Observer=="AST")


# This is to exclude a clear outlier from following analyses
visibleBadge.AST <- subset(visibleBadge.AST,visibleBadge.AST$AvgOfEstimate!=34)

# the mean value of bib measurements per individual is: 

numberofBibmeasurements <- as.numeric(table(as.factor(visibleBadge.AST$BirdID)))

summary(numberofBibmeasurements[numberofBibmeasurements!=0])


# Just want to create new variables with year and month so that I can do my subsetting later on

for(i in 1:nrow(visibleBadge.AST)){
  
  visibleBadge.AST$year[i] <- paste0("20",substr(visibleBadge.AST$CaptureDate[i],8,9))
  visibleBadge.AST$month[i] <- substr(visibleBadge.AST$CaptureDate[i],4,6)
  
}

visibleBadge.AST$year <- factor(visibleBadge.AST$year)
visibleBadge.AST$month <- factor(visibleBadge.AST$month)


# Depending on the month, I consider the measurement to be taken in winter or in summer
# Between October and February = winter
# Between March and August = summer

for(i in 1:nrow(visibleBadge.AST)){
  
  if(visibleBadge.AST$month[i]=="Apr" |
       visibleBadge.AST$month[i]=="Aug"|
       visibleBadge.AST$month[i]=="Jul"|
       visibleBadge.AST$month[i]=="Jun"|
       visibleBadge.AST$month[i]=="Mar"|
       visibleBadge.AST$month[i]=="May"){
    
    visibleBadge.AST$eventSW[i]<-as.numeric(as.character(visibleBadge.AST$year[i]))+as.numeric(0.0)
    
  } else if(visibleBadge.AST$month[i]=="Dec" |
              visibleBadge.AST$month[i]=="Nov"){
    
    visibleBadge.AST$eventSW[i]<-as.numeric(as.character(visibleBadge.AST$year[i]))+as.numeric(0.5)
    
  } else {
    
    visibleBadge.AST$eventSW[i]<-as.numeric(as.character(visibleBadge.AST$year[i]))-as.numeric(0.5)
    
  }
  
}


# # creating age in this database
# 
# visibleBadge.AST$age <- visibleBadge.AST$eventSW-visibleBadge.AST$Cohort


# Creating an identifier to merge this database witht the elo-rankings one

visibleBadge.AST$BirdID_eventSW <- paste(visibleBadge.AST$BirdID,visibleBadge.AST$eventSW,sep="_")


# Now we can finally estimate a mean visible badge per summer/winter

visibleBadge.AST$BirdID_eventSW <- factor(visibleBadge.AST$BirdID_eventSW)


ind.event.VB<-summaryBy(AvgOfEstimate ~ BirdID_eventSW, data = visibleBadge.AST, 
                        FUN = list(mean),na.rm=TRUE)


# the mean value of bib measurements per individual per event is: 

numberofBibmeasurementsperevent <- as.numeric(table(as.factor(visibleBadge.AST$BirdID_eventSW)))

summary(numberofBibmeasurementsperevent[numberofBibmeasurementsperevent!=0])


# Excluding those with no value

#ind.event.VB2 <- subset(ind.event.VB,!(is.na(ind.event.VB$AvgOfEstimate.mean)))


#ind.event.VB2$BirdID_eventSW <- factor(ind.event.VB2$BirdID_eventSW)


#########################################################################################################
# # # 10.1.4. Final RANK database
#########################################################################################################

# Now I can put the three databases together

# Let's start by adding the tarsus and mass averages

rank.TLandM <- merge(elo_scores_all_events_age,
                     ind.TLandM,by="BirdID",all.x=TRUE)

# adding visible badge too

rank.TLandM.VB <- merge(rank.TLandM,ind.event.VB,by="BirdID_eventSW",all.x=TRUE)


# # This is just to count number of individuals and observations for VB
# 
# w<-subset(lifehistory.VB,!(is.na(lifehistory.VB$AvgOfEstimate)))
# 
# 
# # For plotting purposes later on
# 
# z<-subset(full.lifehistory.2,!(is.na(full.lifehistory.2$AvgOfEstimate.mean)))


# Sorting the database according to individual colour code and eventSW

rank.TLandM.VB <- rank.TLandM.VB[order(rank.TLandM.VB$BirdID, 
                                       rank.TLandM.VB$eventSW),]


# writing the results in a .csv file

#write.csv(full.lifehistory.2,file="StElo-aggressive-20160704.csv",sep=",",na="",dec = ".",row.names=FALSE)


# creating a variable that separates everything between summer and winter

for (i in 1:nrow(rank.TLandM.VB)){  
  if (rank.TLandM.VB$eventSW[i]==2013.5){    
    rank.TLandM.VB$season[i] <- 0
    
  } else if (rank.TLandM.VB$eventSW[i]==2014.5){    
    rank.TLandM.VB$season[i] <- 0
    
  } else if (rank.TLandM.VB$eventSW[i]==2015.5){    
    rank.TLandM.VB$season[i] <- 0
    
  } else {    
    rank.TLandM.VB$season[i] <- 1    
  }
  
}


#full.lifehistory.2$season <- factor(full.lifehistory.2$season)


# WITHIN- and BETWEEN-INDIVIDUAL effects

AveByInd <- function(x) mean(x)

rank.TLandM.VB.2 <- do.call("rbind", as.list(
  by(rank.TLandM.VB, rank.TLandM.VB["BirdID"], transform, meanage=AveByInd(age))))  


WithinIndCentr <- function(x) x-mean(x)

rank.TLandM.VB.2 <- do.call("rbind", as.list(  
  by(rank.TLandM.VB.2, rank.TLandM.VB.2["BirdID"], transform, agewithin=WithinIndCentr(age))))


# excluding all VB=NA

rank.TLandM.VB.3 <- subset(rank.TLandM.VB.2,
                           !(is.na(rank.TLandM.VB$AvgOfEstimate.mean)))

# Mean centering VB per event

rank.TLandM.VB.4 <- ddply(rank.TLandM.VB.3, 
                          c("eventSW"), 
                          transform, 
                          VBcenterevent = WithinIndCentr(AvgOfEstimate.mean))

rank.TLandM.VB.4 <- rank.TLandM.VB.4[,c("BirdID_eventSW",
                                        "VBcenterevent")]

row.names(rank.TLandM.VB.2) <- NULL 

rank.TLandM.VB <- merge(rank.TLandM.VB.2,
                        rank.TLandM.VB.4,
                        by="BirdID_eventSW",
                        all.x=TRUE)


#########################################################################################################
# # # 10.1.5. Final RANK database with fitness included
#########################################################################################################

# First adding fledglings

rank.TLandM.VB.fledg <- merge(rank.TLandM.VB,male.social.annualfledglings,
                              by="BirdID_eventSW",all.x=TRUE)


# Second adding recruits

rank.TLandM.VB.fledg.recruits <- merge(rank.TLandM.VB.fledg,male.social.annualrecruits,
                                       by="BirdID_eventSW",all.x=TRUE)


# I'm saving this file for the following scripts

write.csv(rank.TLandM.VB.fledg.recruits,"rank.TLandM.VB.fledg.recruits.csv",row.names=FALSE)


#########################################################################################################
# # # 10.1.6. Final RANK database but only based on those individuals interacting more than 8 times
#########################################################################################################

# Subsetting the database to only those that interacted more than 8 times per event

intpereventSW <- morethan8pereventSW[,c("indevent","freqppereventSW")]

names(intpereventSW)<-c("ind_eventSW","freqppereventSW")

rank.TLandM.VB.fledg.recruits$ind_eventSW <- factor(paste(rank.TLandM.VB.fledg.recruits$individual,
                                                          rank.TLandM.VB.fledg.recruits$eventSW,sep="_"))

rank.TLandM.VB.fledg.recruits.9int <- merge(rank.TLandM.VB.fledg.recruits,intpereventSW,
                                            by="ind_eventSW",all.y=TRUE)


# I'm saving this file for the following scripts

write.csv(rank.TLandM.VB.fledg.recruits.9int,"rank.TLandM.VB.fledg.recruits.9int.csv",row.names=FALSE)


#########################################################################################################
# # 10.2. BIB, AGE and FITNESS database containing all the neccessary information
#########################################################################################################

#########################################################################################################
# # # 10.2.1. Adding Tarsus, Mass and Age
#########################################################################################################

# First generating BirdID and eventSW in the VB dataset version 2

ind.event.VB2 <- ind.event.VB

ind.event.VB2$BirdID <- factor(substr(ind.event.VB2$BirdID_eventSW, 1, 4)) 
ind.event.VB2$eventSW <- as.numeric(substr(ind.event.VB2$BirdID_eventSW, 6, 11)) 


# Now we can add tarsus length and mass

VB.TLandM <- merge(ind.event.VB2,ind.TLandM,by="BirdID",all.x=TRUE)


# Now we can add Cohort to estimate age. But first we have to delete duplicated BirdIDs in ccbirdidcohort

ccbirdidcohort2 <- ccbirdidcohort[,c("BirdID","Cohort","CohortEstimated")]
ccbirdidcohort3 <- unique(ccbirdidcohort2)


VB.TLandM.age <- merge(VB.TLandM,ccbirdidcohort3,by="BirdID",all.x=TRUE)


# the mean value of bib measurements per individual for the whole study period: 

numberofBibmeasurementsperstudyperido <- 
  as.numeric(table(VB.TLandM.age$BirdID))

summary(numberofBibmeasurementsperstudyperido[numberofBibmeasurementsperstudyperido!=0])


# Now I can estimate age

VB.TLandM.age$age <- VB.TLandM.age$eventSW - VB.TLandM.age$Cohort


# WITHIN- and BETWEEN-INDIVIDUAL effects

AveByInd <- function(x) mean(x)

VB.TLandM.age.2 <- do.call("rbind", as.list(
  by(VB.TLandM.age, VB.TLandM.age["BirdID"], transform, meanage=AveByInd(age))))  


WithinIndCentr <- function(x) x-mean(x)

VB.TLandM.age.2 <- do.call("rbind", as.list(  
  by(VB.TLandM.age.2, VB.TLandM.age.2["BirdID"], transform, agewithin=WithinIndCentr(age))))


#########################################################################################################
# # # 10.2.2. Adding fitness
#########################################################################################################

# First we add fledglings

VB.TLandM.age.fledg <- merge(VB.TLandM.age.2, male.social.annualfledglings,
                             by="BirdID_eventSW",all.x=TRUE)


# Second we add recruits

VB.TLandM.age.fledg.recruits <- merge(VB.TLandM.age.fledg, male.social.annualrecruits,
                                      by="BirdID_eventSW",all.x=TRUE)


# I'm saving this file for the following scripts

write.csv(VB.TLandM.age.fledg.recruits,"VB.TLandM.age.fledg.recruits.csv",row.names=FALSE)



#########################################################################################################
# # 10.3. AGE and FITNESS database containing all the neccessary information
#########################################################################################################

# We first combine male.social.annualfledglings with male.social.annualrecruits

fledg.recruits <- merge(male.social.annualfledglings,male.social.annualrecruits,
                        by="BirdID_eventSW",all.x=TRUE)


# Getting BirdID and eventSW as separate variables

fledg.recruits$BirdID <- factor(substr(fledg.recruits$BirdID_eventSW, 1, 4)) 
fledg.recruits$eventSW <- as.numeric(substr(fledg.recruits$BirdID_eventSW, 6, 9))


# adding cohort to estimate age

age.fledg.recruits <- merge(fledg.recruits,ccbirdidcohort3,
                            by="BirdID",all.x=TRUE)


age.fledg.recruits$age <- age.fledg.recruits$eventSW - age.fledg.recruits$Cohort


# I'm saving this file for the following scripts

write.csv(age.fledg.recruits,"age.fledg.recruits.csv",row.names=FALSE)
