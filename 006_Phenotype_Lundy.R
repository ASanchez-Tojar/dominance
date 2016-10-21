
# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 3rd of March, 2016

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
elo_scores_all_events_sim <- read.table("Elo-Rating_robustness/elo_scores_all_events_sim.csv",header=TRUE,sep=",")
birdsex.1 <- read.table("birdsex.1.csv",header=TRUE,sep=",")
morethan8pereventSW <- read.table("morethan8pereventSW.csv",header=TRUE,sep=",")
#fitness.full <- read.table("fledglings12/fitness.full.csv",header=TRUE,sep=",")
fitness.full.both <- read.table("fledglings12/fitness.full.both.csv",header=TRUE,sep=",")

# # to avoid making changes to the code, let's call elo_scores_all_events_sim
# # as elo_scores_all_events
# 
# elo_scores_all_events <- elo_scores_all_events_sim #simulated values


#########################################################################################################
# 10. Getting all databases ready
#########################################################################################################

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


# adding that information to the elo_scores_all_events.

elo_scores_all_events_age <- merge(elo_scores_all_events,ccbirdidcohort,
                                   by.x="individual",by.y="Code",
                                   all.x=TRUE)


# estimating age as 0 (fledgling in summer), 0.5 (first winter), 1 (first summer), 1.5 (2nd winter)...

elo_scores_all_events_age$age <- elo_scores_all_events_age$eventSW - elo_scores_all_events_age$Cohort


# individual identifier per event

elo_scores_all_events_age$BirdID_eventSW <- factor(paste(elo_scores_all_events_age$BirdID,
                                                         elo_scores_all_events_age$eventSW,sep="_"))

 
# # the mean value of StElos per individual for the whole study period: 
# 
# numberofStEloperbird <- 
#   as.numeric(table(as.factor(elo_scores_all_events_age$BirdID)))
# 
# summary(numberofStEloperbird[numberofStEloperbird!=0])


#########################################################################################################
# # # 10.1.3. Adding tarsus length, mass and visible badge
#########################################################################################################

# loading the database with tarsus length and mass (Stage=3, so it doesn't include measures in nest)

tarsuslengthandMass <- read.table("BirdID-Tarsus-Mass.csv",header=TRUE,sep=",")


# # the mean value of tarsus measurements per individual is: 
# 
# numberofTLmeasurements <- as.numeric(table(as.factor(tarsuslengthandMass$BirdID)))
# 
# summary(numberofTLmeasurements[numberofTLmeasurements!=0])


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

# # the mean value of bib measurements per individual is: 
# 
# numberofBibmeasurements <- as.numeric(table(as.factor(visibleBadge.AST$BirdID)))
# 
# summary(numberofBibmeasurements[numberofBibmeasurements!=0])


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

visibleBadge.AST$BirdID_eventSW <- factor(paste(visibleBadge.AST$BirdID,
                                                visibleBadge.AST$eventSW,sep="_"))


# Now we can finally estimate a mean visible badge per summer/winter

ind.event.VB<-summaryBy(AvgOfEstimate ~ BirdID_eventSW, data = visibleBadge.AST, 
                        FUN = list(mean),na.rm=TRUE)


# # the mean value of bib measurements per individual per event is: 
# 
# numberofBibmeasurementsperevent <- as.numeric(table(as.factor(visibleBadge.AST$BirdID_eventSW)))
# 
# summary(numberofBibmeasurementsperevent[numberofBibmeasurementsperevent!=0])


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

# reducing fitness database before merging

fitness.full.both.2 <- fitness.full.both[,c("BirdID_eventSW","gen.fledg.12d",
                                            "gen.recruits","soc.fledg.12d",
                                            "soc.recruits")]

rank.TLandM.VB.fitness <- merge(rank.TLandM.VB,fitness.full.both.2,
                                by="BirdID_eventSW",all.x=TRUE)

names(rank.TLandM.VB.fitness) <- c("BirdID_eventSW","BirdID","colourrings",
                                   "StElo","eventSW","sex","cohort",
                                   "cohortEstimated","age","tarsus",
                                   "mass","bib","season","meanage",
                                   "agewithin","bib.centred",
                                   "gen.fledg.12d","gen.recruits",
                                   "soc.fledg.12d","soc.recruits")


# I'm saving this file for the following scripts

write.csv(rank.TLandM.VB.fitness,
          "finaldatabases/rank.TLandM.VB.fitness.csv",
          #"finaldatabases/rank.TLandM.VB.fitness_sim.csv",
          row.names=FALSE)


#########################################################################################################
# # # 10.1.6. Final RANK database but only based on those individuals interacting more than 8 times
#########################################################################################################

# Subsetting the database to only those that interacted more than 8 times per event

intpereventSW <- morethan8pereventSW[,c("indevent","freqppereventSW")]

names(intpereventSW)<-c("ind_eventSW","freqppereventSW")

rank.TLandM.VB.fitness$ind_eventSW <- factor(paste(rank.TLandM.VB.fitness$colourrings,
                                                   rank.TLandM.VB.fitness$eventSW,sep="_"))

rank.TLandM.VB.fitness.9int <- merge(rank.TLandM.VB.fitness,intpereventSW,
                                     by="ind_eventSW",all.y=TRUE)


# I'm saving this file for the following scripts

write.csv(rank.TLandM.VB.fitness.9int,
          "finaldatabases/rank.TLandM.VB.fitness.9int.csv",
          #"finaldatabases/rank.TLandM.VB.fitness.9int_sim.csv",
          row.names=FALSE)


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

ccbirdidcohort2 <- unique(ccbirdidcohort[,c("BirdID","Cohort","CohortEstimated")])


VB.TLandM.age <- merge(VB.TLandM,ccbirdidcohort2,by="BirdID",all.x=TRUE)


# the bird with missing Cohort is due to duplicated colour rings. It got its colour
# rings back before the breeding season. Therefore, there is no reason to exclude it
# from THIS database. I'll put back the information with this code.

VB.TLandM.age$Cohort <- ifelse(is.na(VB.TLandM.age$Cohort),
                               2013,
                               VB.TLandM.age$Cohort)


# # the mean value of bib measurements per individual for the whole study period: 
# 
# numberofBibmeasurementsperstudyperido <- 
#   as.numeric(table(VB.TLandM.age$BirdID))
# 
# summary(numberofBibmeasurementsperstudyperido[numberofBibmeasurementsperstudyperido!=0])


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

VB.TLandM.age.fitness <- merge(VB.TLandM.age.2, fitness.full.both.2,
                               by="BirdID_eventSW",all.x=TRUE)


names(VB.TLandM.age.fitness) <- c("BirdID_eventSW","BirdID","bib",
                                  "eventSW","tarsus","mass",
                                  "cohort","cohortEstimated",
                                  "age","meanage","agewithin",
                                  "gen.fledg.12d","gen.recruits",
                                  "soc.fledg.12d","soc.recruits")


# I'm saving this file for the following scripts

write.csv(VB.TLandM.age.fitness,
          "finaldatabases/VB.TLandM.age.fitness.csv",
          row.names=FALSE)



#########################################################################################################
# # 10.3. AGE and FITNESS database containing all the neccessary information
#########################################################################################################

age.fitness <- merge(fitness.full.both,ccbirdidcohort2,
                     by="BirdID",all.x=TRUE)

# the 2 birds with missing Cohort is due to duplicated colour rings in the first place
# and a new bird not included in ccbirdidcohort2 yet. I'll give them back their cohorts.

age.fitness$Cohort <- ifelse(age.fitness$BirdID==7145,
                               2013,
                             age.fitness$Cohort)

age.fitness$Cohort <- ifelse(age.fitness$BirdID==9038,
                               2015,
                             age.fitness$Cohort)


age.fitness$age <- age.fitness$year - age.fitness$Cohort


names(age.fitness) <- c("BirdID","BirdID_eventSW","year",
                        "gen.fledg.12d","gen.recruits",
                        "soc.fledg.12d", "soc.recruits",
                        "cohort","cohortEstimated","age")



# I'm saving this file for the following scripts

write.csv(age.fitness,"finaldatabases/age.fitness.csv",row.names=FALSE)
