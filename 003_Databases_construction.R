
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
# The section 9.A: 


########################################################################################################
# Packages needed
########################################################################################################

# packages needed to be loaded for this script (a couple of them might be only needed in the following
# script)

library(EloRating)
library(statnet)
library(rptR)
library(ggplot2)
library(doBy)
library(lme4)
library(blmeco)
library(arm)


# Clear memory and get to know where you are
rm(list=ls())
#getwd()


# Little function for length(unique()), I do it all the time so it makes sense to do this:

lunique <- function(x){length(unique(x))}

# loading the clean database to estimate the ratings

elo_scores_all_events <- read.table("elo_scores_all_events.csv",header=TRUE,sep=",")
birdsex.1 <- read.table("birdsex.1.csv",header=TRUE,sep=",")


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


#########################################################################################################
# # # 10.1.3. Adding tarsus length, mass and visible badge
#########################################################################################################

# loading the database with tarsus length and mass (Stage=3, so it doesn't include measures in nest)

tarsuslengthandMass <- read.table("BirdID-Tarsus-Mass.csv",header=TRUE,sep=",")


# loading the database with Visible Badge

visibleBadge <-  read.table("BirdID-VisibleBadge.csv",header=TRUE,sep=",")


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


rank.TLandM.VB.2


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


#########################################################################################################
# # 10.2. BIB, AGE and FITNESS database containing all the neccessary information
#########################################################################################################

#########################################################################################################
# # # 10.2.1. Adding Tarsus, Mass and Age
#########################################################################################################

# First generating BirdID and eventSW in the VB dataset version 2

ind.event.VB2 <- ind.event.VB

ind.event.VB2$BirdID <- factor(substr(ind.event.VB2$BirdID_eventSW, 1, 4)) 
ind.event.VB2$eventSW <- factor(substr(ind.event.VB2$BirdID_eventSW, 6, 11)) 


# Now we can add tarsus length and mass

VB.TLandM <- merge(ind.event.VB2,ind.TLandM,by="BirdID",all.x=TRUE)


# Now we can add Cohort to estimate age. But first we have to delete duplicated BirdIDs in ccbirdidcohort

ccbirdidcohort2 <- ccbirdidcohort[,c("BirdID","Cohort","CohortEstimated")]
ccbirdidcohort3 <- unique(ccbirdidcohort2)


VB.TLandM.age <- merge(VB.TLandM,ccbirdidcohort3,by="BirdID",all.x=TRUE)


#########################################################################################################
# # # 10.2.2. Adding fitness
#########################################################################################################

# First we add fledglings

VB.TLandM.age.fledg <- merge(VB.TLandM.age, male.social.annualfledglings,
                             by="BirdID_eventSW",all.x=TRUE)


# Second we add recruits

VB.TLandM.age.fledg.recruits <- merge(VB.TLandM.age.fledg, male.social.annualrecruits,
                                      by="BirdID_eventSW",all.x=TRUE)


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


#########################################################################################################
# 11. Plotting all sort of plots for talks and manuscript
#########################################################################################################

#########################################################################################################
# # 11.1. Histograms
#########################################################################################################

#########################################################################################################
# # # 11.1.1. Visible Badge
#########################################################################################################

hist(ind.event.VB$AvgOfEstimate.mean,freq=TRUE,breaks=18, # I could have used database=w, but it is pretty much the same w=291 observations
     main = "",
     xlab = "Bib length (mm)",
     ylim = c(0,80),
     xlim = c(34,62),
     ylab = "",
     col="grey75",
     axes=FALSE,
     cex.lab=1.75,
     right=FALSE)
title(ylab="Number of observations", line=2.2, cex.lab=1.75)
axis(1,at = seq(34,62,by=2),lwd=1)
axis(2,at = seq(0,80,by=20),lwd=1,line=-0.75, las=2)


hist(z$AvgOfEstimate.mean,freq=TRUE,breaks=16,
     main = "",
     xlab = "Bib length (mm)",
     ylim = c(0,21),
     xlim = c(34,60),
     ylab = "",
     col="grey50",
     axes=FALSE,
     cex.lab=1.75,
     right=FALSE,
     add=TRUE)


#########################################################################################################
# # # 11.1.1. Standardized Elo-rating
#########################################################################################################

hist(full.lifehistory.2$StElo,freq=TRUE,breaks=10,
     main = "",
     xlab = "Standardized Elo-rating",
     ylim = c(0,165),
     xlim = c(0,1),
     ylab = "",
     col="grey75",
     axes=FALSE,
     cex.lab=1.75,
     right=FALSE)
title(ylab="Number of observations", line=2.2, cex.lab=1.75)
axis(1,at = seq(0,1,by=0.1),lwd=0.1)
axis(2,at = seq(0,165,by=15),lwd=1,line=-0.75, las=2)


# hist(full.lifehistory.2.9int$StElo,freq=TRUE,breaks=10,
#      main = "",
#      xlab = "Standardized Elo-rating",
#      ylim = c(0,165),
#      xlim = c(0,1),
#      ylab = "",
#      col="grey25",
#      axes=FALSE,
#      cex.lab=1.75,
#      right=FALSE,
#      add=TRUE)
# 
# legend("topright", c("Full data", ">8 int/event"), fill=c("grey75", "grey25"))

#########################################################################################################
# # # 11.2. Plots
#########################################################################################################

#########################################################################################################
# # # 11.2.1. Standardized Elo-rank (y-axis) and age (x-axis)
#########################################################################################################

# yuhu_no0 <- subset(yuhu,yuhu$age>0)
# yuhu_males <- subset(yuhu,yuhu$SexEstimate==1)
# yuhu_males_no0 <- subset(yuhu_no0,yuhu_no0$SexEstimate==1)
# yuhu3_males <- subset(yuhu3,yuhu3$SexEstimate==1)

ggplot(yuhu, aes(age, StElo)) + 
  
  geom_point(alpha = 0.60,
             size=5,
             colour="grey50") +
  
  geom_line(aes(group=individual), #if repeated values/individual, not needed for captivity
            alpha = 0.15,
            col="grey50") +
  
  scale_y_continuous("Standardized Elo-rank",
                     breaks=seq(0,1,0.2)) + 
  
  scale_x_continuous("Age",
                     limits=c(0,6),
                     breaks=seq(0,6,0.5),
                     labels=c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6)) + # change if older individuals are
  # included
  
  geom_smooth(method="lm",colour="darkorange",fill="darkorange") + #aes(fill = factor(SexEstimate))) # comment/uncomment depending on whether
  # you want the regression line or not
  
  theme(panel.background = element_rect(fill = "white",colour="grey70"),
        axis.text.x = element_text(color="black", size=20),
        axis.text.y = element_text(color="black", size=20),
        axis.title.x=element_text(size=25,color="black",margin=margin(30,0,20,0)),
        axis.title.y=element_text(size=25,color="black",margin=margin(30,30,20,20)),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())


#########################################################################################################
# # # 11.2.2. Standardized Elo-rank (y-axis) and visible badge (x-axis)
#########################################################################################################

# getting rid off that outlier with 36 of visible badge

full.lifehistory.3 <- subset(full.lifehistory.2,full.lifehistory.2$AvgOfEstimate.mean>36)
#full.lifehistory.3.9int <- subset(full.lifehistory.2.9int,full.lifehistory.2.9int$AvgOfEstimate.mean>36)


# full.lifehistory.4 <- full.lifehistory.3
# full.lifehistory.4$indevent <- paste(full.lifehistory.4$individual,
#                                      full.lifehistory.4$eventSW,sep="_")
# full.lifehistory.5 <- merge(morethan8pereventSW,full.lifehistory.4,
#                             by="indevent")


ggplot(full.lifehistory.3.9int, aes(AvgOfEstimate.mean, StElo)) + 
  
  geom_point(alpha = 0.60,
             size=5,
             colour="grey50") +
  
  geom_line(aes(group=individual),
            alpha = 0.15,
            col="grey50") +
  
  scale_y_continuous("Standardized Elo-rank",
                     breaks=seq(0,1,0.2)) + 
  
  scale_x_continuous("Bib length (mm)",
                     limits=c(42,60),
                     breaks=seq(42,60,2)) +
  
  geom_smooth(method="lm",colour="darkorange",fill="darkorange") +
  
  theme(panel.background = element_rect(fill = "white",colour="grey70"),
        axis.text.x = element_text(color="black", size=20),
        axis.text.y = element_text(color="black", size=20),
        axis.title.x=element_text(size=25,color="black",margin=margin(30,0,20,0)),
        axis.title.y=element_text(size=25,color="black",margin=margin(30,30,20,20)),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())


#########################################################################################################
# # # 11.2.3. Standardized Elo-rank (y-axis) and tarsus length (x-axis)
#########################################################################################################

# excluding those with no value for tarsus length

y <- subset(full.lifehistory.2,!(is.na(full.lifehistory.2$Tarsus.mean)))

ggplot(y, aes(Tarsus.mean, StElo)) + 
  
  geom_point(alpha = 0.60,
             size=5,
             colour="grey50") +
  
  scale_y_continuous("Standardized Elo-rank",
                     breaks=seq(0,1,0.2)) + 
  
  scale_x_continuous("Tarsus length (mm)",
                     limits=c(15,21),
                     breaks=seq(15,21,1)) +
  
  geom_smooth(method="lm",colour="darkorange",fill="darkorange") +
  
  theme(panel.background = element_rect(fill = "white",colour="grey70"),
        axis.text.x = element_text(color="black", size=20),
        axis.text.y = element_text(color="black", size=20),
        axis.title.x=element_text(size=25,color="black",margin=margin(30,0,20,0)),
        axis.title.y=element_text(size=25,color="black",margin=margin(30,30,20,20)),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())


#########################################################################################################
# # # 11.2.4. Standardized Elo-rank (y-axis) and mass (x-axis)
#########################################################################################################

# excluding those with no value for mass for counting

q <- subset(full.lifehistory.2,!(is.na(full.lifehistory.2$Mass.mean)))

ggplot(q, aes(Mass.mean, StElo)) + 
  
  geom_point(alpha = 0.60,
             size=5,
             colour="grey50") +
  
  scale_y_continuous("Standardized Elo-rank",
                     breaks=seq(0,1,0.2)) + 
  
  scale_x_continuous("Mass (g)",
                     limits=c(19,34),
                     breaks=seq(19,34,1)) +
  
  geom_smooth(method="lm",colour="darkorange",fill="darkorange") +
  
  theme(panel.background = element_rect(fill = "white",colour="grey70"),
        axis.text.x = element_text(color="black", size=20),
        axis.text.y = element_text(color="black", size=20),
        axis.title.x=element_text(size=25,color="black",margin=margin(30,0,20,0)),
        axis.title.y=element_text(size=25,color="black",margin=margin(30,30,20,20)),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())


#########################################################################################################
# # # 11.2.5. Visible badge (y-axis) and age (x-axis)
#########################################################################################################

# creating a database for this plot, I need age but also average value per event per bird for visible badge

# sorting for lines and excluding those with no value for VB and the outlier

lifehistory.VB <- lifehistory.VB[order(lifehistory.VB$BirdID, lifehistory.VB$eventSW),]

#ind.event.VB2

i <- lifehistory.VB[,c(1,23,24)]
i.2 <- unique(i)

e <- merge(ind.event.VB2,i.2,by="BirdID_eventSW",all.x=TRUE)

# e <- subset(lifehistory.VB,!(is.na(lifehistory.VB$AvgOfEstimate)))
e <- subset(e,e$AvgOfEstimate.mean>36)

ggplot(e, aes(age,AvgOfEstimate.mean)) + 
  
  geom_point(alpha = 0.60,
             size=5,
             colour="grey50") +
  
  geom_line(aes(group=BirdID),
            alpha = 0.15,
            col="grey50") +
  
  scale_y_continuous("Bib length (mm)",
                     limits=c(42,60),
                     breaks=seq(42,60,2)) +
  
  scale_x_continuous("Age",
                     limits=c(0,6),
                     breaks=seq(0,6,0.5),
                     labels=c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6)) +
  
  geom_smooth(method="lm",colour="darkorange",fill="darkorange") +
  
  theme(panel.background = element_rect(fill = "white",colour="grey70"),
        axis.text.x = element_text(color="black", size=20),
        axis.text.y = element_text(color="black", size=20),
        axis.title.x=element_text(size=25,color="black",margin=margin(30,0,20,0)),
        axis.title.y=element_text(size=25,color="black",margin=margin(30,30,20,20)),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())


#########################################################################################################
# 12. STATISTICAL ANALYSES
#########################################################################################################

#########################################################################################################
# # 12.1. Collinearity
#########################################################################################################

# First, the predictors, are they correalted to each other?

collinearity <- full.lifehistory.2[,c("age","Tarsus.mean","Mass.mean","AvgOfEstimate.mean")]

cor(collinearity,use="complete.obs")

pairs(collinearity)


#########################################################################################################
# # 12.2. Rank and age,tarus,sex (BOTH SEXES)
#########################################################################################################

# General model with the whole dataset

# mod1 <- lmer(StElo~scale(age,scale=FALSE)*as.factor(SexEstimate)+
#                scale(Tarsus.mean,scale=FALSE)+
#                #as.factor(SexEstimate)+
#                scale(season,scale=FALSE)+
#                (1|individual),data=full.lifehistory.2)
# 
# summary(mod1)

mod1 <- lmer(StElo~scale(age)*as.factor(SexEstimate)+
               scale(Tarsus.mean)+
               scale(season)+
               (1|individual),data=full.lifehistory.2)

summary(mod1)

#getting mean and CrI from the bayesian model
smod1<-sim(mod1,1000)

apply(smod1@fixef,2, mean)
apply(smod1@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod1@ranef$individual,1, mean))
mean(apply(smod1@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod1@ranef$individual,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod1))
compareqqnorm(mod1) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod1))
qqline(resid(mod1))

#qq plot of random effects
qqnorm(unlist(ranef(mod1)$individual))
qqline(unlist(ranef(mod1)$individual))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod1),resid(mod1))


# Same model but only including individuals interacting 9 times or more

mod1.9int <- lmer(StElo~scale(age)*as.factor(SexEstimate)+
                    scale(Tarsus.mean)+
                    scale(season)+
                    (1|individual),data=full.lifehistory.2.9int)

summary(mod1.9int)


#getting mean and CrI from the bayesian model
smod1.9int<-sim(mod1.9int,1000)
str(smod1.9int)

apply(smod1.9int@fixef,2, mean)
apply(smod1.9int@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod1.9int@ranef$individual,1, mean))
mean(apply(smod1.9int@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod1.9int@ranef$individual,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod1.9int))
compareqqnorm(mod1.9int) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod1.9int))
qqline(resid(mod1.9int))

#qq plot of random effects
qqnorm(unlist(ranef(mod1.9int)$individual))
qqline(unlist(ranef(mod1.9int)$individual))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod1.9int),resid(mod1.9int))


# within- and between- age effects

mod1.within <- lmer(StElo~agewithin+
                      scale(meanage,scale=FALSE)+
                      scale(Tarsus.mean,scale=FALSE)+
                      as.factor(SexEstimate)+
                      scale(season,scale=FALSE)+
                      (1|individual),data=full.lifehistory.2)

summary(mod1.within)


#########################################################################################################
# # 12.2. Rank and age,tarus (ONLY  MALES, NO BIB)
#########################################################################################################

# males subset

full.lifehistory.2.males <- subset(full.lifehistory.2,full.lifehistory.2$SexEstimate==1)


mod1.males <- lmer(StElo~scale(age)+
                     #I(scale(age,scale=FALSE)^2)+
                     scale(Tarsus.mean)+
                     scale(season)+
                     (1|individual),data=full.lifehistory.2.males)


summary(mod1.males)


#getting mean and CrI from the bayesian model
smod1.males<-sim(mod1.males,1000)
str(smod1.males)

apply(smod1.males@fixef,2, mean)
apply(smod1.males@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod1.males@ranef$individual,1, mean))
mean(apply(smod1.males@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod1.males@ranef$individual,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod1.males))
compareqqnorm(mod1.males) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod1.males))
qqline(resid(mod1.males))

#qq plot of random effects
qqnorm(unlist(ranef(mod1.males)$individual))
qqline(unlist(ranef(mod1.males)$individual))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod1.males),resid(mod1.males))



# same for 9 int or more

full.lifehistory.2.males.9int <- subset(full.lifehistory.2.9int,full.lifehistory.2.9int$SexEstimate==1)

mod1.males.9int <- lmer(StElo~scale(age)+
                          #I(scale(age)^2)+
                          scale(Tarsus.mean)+
                          scale(season)+
                          (1|individual),data=full.lifehistory.2.males.9int)


summary(mod1.males.9int)


#getting mean and CrI from the bayesian model
smod1.males.9int<-sim(mod1.males.9int,1000)
str(smod1.males.9int)

apply(smod1.males.9int@fixef,2, mean)
apply(smod1.males.9int@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod1.males.9int@ranef$individual,1, mean))
mean(apply(smod1.males.9int@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod1.males.9int@ranef$individual,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod1.males.9int))
compareqqnorm(mod1.males.9int) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod1.males.9int))
qqline(resid(mod1.males.9int))

#qq plot of random effects
qqnorm(unlist(ranef(mod1.males.9int)$individual))
qqline(unlist(ranef(mod1.males.9int)$individual))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod1.males.9int),resid(mod1.males.9int))


# withing and between

mod1.within.males <- lmer(StElo~scale(agewithin)+
                            scale(meanage)+
                            scale(Tarsus.mean)+
                            scale(season)+
                            (1|individual),data=full.lifehistory.2.males)


summary(mod1.within.males)


#getting mean and CrI from the bayesian model
smod1.within.males<-sim(mod1.within.males,1000)
str(smod1.within.males)

apply(smod1.within.males@fixef,2, mean)
apply(smod1.within.males@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod1.within.males@ranef$individual,1, mean))
mean(apply(smod1.within.males@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod1.within.males@ranef$individual,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod1.within.males))
compareqqnorm(mod1.within.males) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod1.within.males))
qqline(resid(mod1.within.males))

#qq plot of random effects
qqnorm(unlist(ranef(mod1.within.males)$individual))
qqline(unlist(ranef(mod1.within.males)$individual))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod1.within.males),resid(mod1.within.males))


#########################################################################################################
# # 12.4. Rank and age and bib
#########################################################################################################

mod10 <- lmer(StElo~scale(age)+
                #I(scale(age)^2)+
                scale(AvgOfEstimate.mean)+
                #I(scale(AvgOfEstimate.mean)^2)+
                scale(Tarsus.mean)+
                scale(season)+
                (1|individual)+(1|eventSW),data=full.lifehistory.2)

summary(mod10)


#getting mean and CrI from the bayesian model
smod10<-sim(mod10,1000)
str(smod10)

apply(smod10@fixef,2, mean)
apply(smod10@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod10@ranef$individual,1, mean))
mean(apply(smod10@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod10@ranef$individual,1, quantile, c(0.975)))

mean(apply(smod10@ranef$eventSW,1, mean))
mean(apply(smod10@ranef$eventSW,1, quantile, c(0.025)))
mean(apply(smod10@ranef$eventSW,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod10))
compareqqnorm(mod10) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod10))
qqline(resid(mod10))

#qq plot of random effects
qqnorm(unlist(ranef(mod10)$individual))
qqline(unlist(ranef(mod10)$individual))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod10),resid(mod10))


# only 9 or more int

mod10.9int <- lmer(StElo~scale(age)+
                     #I(scale(age)^2)+
                     scale(AvgOfEstimate.mean)+
                     #I(scale(AvgOfEstimate.mean)^2)+
                     scale(Tarsus.mean)+
                     scale(season)+
                     (1|individual)+(1|eventSW),data=full.lifehistory.2.9int)

summary(mod10.9int)


#getting mean and CrI from the bayesian model
smod10.9int<-sim(mod10.9int,1000)
str(smod10.9int)

apply(smod10.9int@fixef,2, mean)
apply(smod10.9int@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod10.9int@ranef$individual,1, mean))
mean(apply(smod10.9int@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod10.9int@ranef$individual,1, quantile, c(0.975)))

mean(apply(smod10.9int@ranef$eventSW,1, mean))
mean(apply(smod10.9int@ranef$eventSW,1, quantile, c(0.025)))
mean(apply(smod10.9int@ranef$eventSW,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod10.9int))
compareqqnorm(mod10.9int) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod10.9int))
qqline(resid(mod10.9int))

#qq plot of random effects
qqnorm(unlist(ranef(mod10.9int)$individual))
qqline(unlist(ranef(mod10.9int)$individual))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod10.9int),resid(mod10.9int))


# withing and between age

mod10.within <- lmer(StElo~scale(agewithin)+
                       scale(meanage)+
                       scale(AvgOfEstimate.mean)+
                       scale(Tarsus.mean)+
                       scale(season)+
                       (1|individual)+(1|eventSW),data=full.lifehistory.2)


summary(mod10.within)

#getting mean and CrI from the bayesian model
smod10.within<-sim(mod10.within,1000)
str(smod10.within)

apply(smod10.within@fixef,2, mean)
apply(smod10.within@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod10.within@ranef$individual,1, mean))
mean(apply(smod10.within@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod10.within@ranef$individual,1, quantile, c(0.975)))

mean(apply(smod10.within@ranef$eventSW,1, mean))
mean(apply(smod10.within@ranef$eventSW,1, quantile, c(0.025)))
mean(apply(smod10.within@ranef$eventSW,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod10.within))
compareqqnorm(mod10.within) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod10.within))
qqline(resid(mod10.within))

#qq plot of random effects
qqnorm(unlist(ranef(mod10.within)$individual))
qqline(unlist(ranef(mod10.within)$individual))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod10.within),resid(.within))


#########################################################################################################
# # 12.5. Bib and age
#########################################################################################################

lifehistory.VB.subset <- lifehistory.VB[,c("BirdID",
                                           "eventSW","BirdID_eventSW")]


lifehistory.VB.subset.2 <- unique(lifehistory.VB.subset)


birdIDcohort <- read.table("BirdID-Cohort-Cohort_estimated-Sex.csv",header=TRUE,sep=",")


# adding age to lifehistory.VB.subset

lifehistory.VB.subset.2 <- merge(lifehistory.VB.subset,birdIDcohort,by="BirdID",all.x=TRUE)

lifehistory.VB.subset.2$age <- lifehistory.VB.subset.2$eventSW - lifehistory.VB.subset.2$Cohort


meanVBandlifehistory <- merge(ind.event.VB,lifehistory.VB.subset.2,by="BirdID_eventSW",all.x=TRUE)

meanVBandlifehistory.2 <- merge(meanVBandlifehistory,ind.TLandM,by="BirdID",all.x=TRUE)

mod11 <- lmer(AvgOfEstimate.mean~
                scale(age)+
                #I(scale(age,scale=FALSE)^2)+
                scale(Tarsus.mean)+
                (1|BirdID)+(1|eventSW),data=meanVBandlifehistory.2)

summary(mod11)


#getting mean and CrI from the bayesian model
smod11<-sim(mod11,1000)
str(smod11)

apply(smod11@fixef,2, mean)
apply(smod11@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod11@ranef$individual,1, mean))
mean(apply(smod11@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod11@ranef$individual,1, quantile, c(0.975)))

mean(apply(smod11@ranef$eventSW,1, mean))
mean(apply(smod11@ranef$eventSW,1, quantile, c(0.025)))
mean(apply(smod11@ranef$eventSW,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod11))
compareqqnorm(mod11) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod11))
qqline(resid(mod11))

#qq plot of random effects
qqnorm(unlist(ranef(mod11)$BirdID))
qqline(unlist(ranef(mod11)$BirdID))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod11),resid(mod11))


# WITHIN- and BETWEEN-INDIVIDUAL effects

AveByInd <- function(x) mean(x)

d2 <- do.call("rbind", as.list(  
  by(meanVBandlifehistory.2, meanVBandlifehistory.2["BirdID"], transform, meanage=AveByInd(age))))  

par(mfrow=c(1,1))	
#hist(d2$AveAge, xlab="Average female age", ylab="Frequency", main="")	


WithinIndCentr <- function(x) x-mean(x)

d2 <- do.call("rbind", as.list(	
  by(d2, d2["BirdID"], transform, agewithin=WithinIndCentr(age))))	


mod11.within <- lmer(AvgOfEstimate.mean~
                       agewithin+
                       scale(meanage,scale=FALSE)+
                       #I(AveAge^2)+
                       #scale(age,scale=FALSE)+ 
                       #I(scale(age,scale=FALSE)^2)+
                       scale(Tarsus.mean,scale=FALSE)+
                       (1|BirdID)+(1|eventSW),data=d2)

summary(mod11.within)






#########################################################################################################
# # # 12.6.1. Rank and fitness
#########################################################################################################

# # Including social fitness in the database (OLD WAY)
# 
# social.fitness<-read.table("socialfitness_2014-2015.csv",sep=",",header=TRUE)
# 
# # creating a year identifier, I'm going to start by using only the elo ratins of summer 2014 and 2015
# 
# social.fitness<-subset(social.fitness,!(is.na(social.fitness$BirdID)))
# 
# social.fitness$BirdID_eventSW <- as.factor(paste(social.fitness$BirdID,social.fitness$year,sep="_"))
# 
# 
# # Know I will create a single value per year per bird by summing up FledgeNperOriginalClutch and perRearingNest
# 
# social.fitness2<-summaryBy(OffspringRINGED ~ BirdID_eventSW, 
#                            data = social.fitness, 
#                            FUN = list(sum),na.rm=TRUE)


# merging full.lifehistory.2 with fitness

male.social.fitnessperyear4 <- male.social.fitnessperyear3[,c("IDyear","ChickCoun3.sum")]

full.lifehistory.3 <- subset(full.lifehistory.2,full.lifehistory.2$eventSW==2014 | 
                               full.lifehistory.2$eventSW==2015 |
                               full.lifehistory.2$eventSW==2016)

full.lifehistory.3 <- subset(full.lifehistory.3,full.lifehistory.3$SexEstimate==1)

full.lifehistory.fitnes <- merge(full.lifehistory.3,male.social.fitnessperyear4,
                                 by.x="BirdID_eventSW",by.y="IDyear",
                                 all.x=TRUE)


full.lifehistory.fitnes <- subset(full.lifehistory.fitnes,
                                  !(is.na(full.lifehistory.fitnes$ChickCoun3.sum)))

full.lifehistory.fitnes <- full.lifehistory.fitnes[order(full.lifehistory.fitnes$BirdID, 
                                                         full.lifehistory.fitnes$eventSW),]

# analysis

mod.male.rank <- glmer(ChickCoun3.sum~scale(StElo)+
                         #I(scale(StElo,scale=FALSE)^2)+
                         scale(age)+
                         I(scale(age)^2)+
                         #scale(AvgOfEstimate.mean)+
                         scale(Tarsus.mean)+
                         scale(eventSW)+
                         (1|BirdID),
                       data=full.lifehistory.fitnes,
                       family=poisson)

summary(mod.male.rank)


#getting mean and CrI from the bayesian model
smod.male.rank<-sim(mod.male.rank,1000)

apply(smod.male.rank@fixef,2, mean)
apply(smod.male.rank@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod.male.rank@ranef$individual,1, mean))
mean(apply(smod.male.rank@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod.male.rank@ranef$individual,1, quantile, c(0.975)))

mean(apply(smod.male.rank@ranef$eventSW,1, mean))
mean(apply(smod.male.rank@ranef$eventSW,1, quantile, c(0.025)))
mean(apply(smod.male.rank@ranef$eventSW,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod.male.rank))
compareqqnorm(mod.male.rank) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod.male.rank))
qqline(resid(mod.male.rank))

#qq plot of random effects
qqnorm(unlist(ranef(mod.male.rank)$BirdID))
qqline(unlist(ranef(mod.male.rank)$BirdID))





####



full.lifehistory.fitnes$year <- substr(full.lifehistory.fitnes$BirdID_eventSW, 6, 9)



# Histogram on the number of social fledglings

hist(full.lifehistory.fitnes$OffspringRINGED.sum,freq=TRUE,breaks=12, # I could have used database=w, but it is pretty much the same w=291 observations
     main = "",
     xlab = "Number of social fledglings/year",
     ylim = c(0,20),
     xlim = c(0,12),
     ylab = "",
     col="grey75",
     axes=FALSE,
     cex.lab=1.75,
     right=FALSE)
title(ylab="Number of individuals", line=2.2, cex.lab=1.75)
axis(1,at = seq(0,12,by=1),lwd=1)
axis(2,at = seq(0,20,by=5),lwd=1,line=-0.75, las=2)


full.lifehistory.fitnes.females <- subset(full.lifehistory.fitnes,
                                          full.lifehistory.fitnes$SexEstimate==0)

full.lifehistory.fitnes.males <- subset(full.lifehistory.fitnes,
                                        full.lifehistory.fitnes$SexEstimate==1)




mod.fem.1 <- lmer(OffspringRINGED.sum~StElo+age+Tarsus.mean+year+(1|BirdID),data=full.lifehistory.fitnes.females)
summary(mod.fem.1)

mod.fem.2 <- lmer(OffspringRINGED.sum~StElo+I(StElo^2)+age+Tarsus.mean+year+(1|BirdID),data=full.lifehistory.fitnes.females)
summary(mod.fem.2)

mod.fem.3 <- lmer(OffspringRINGED.sum~StElo+I(StElo^2)+age+Mass.mean+year+(1|BirdID),data=full.lifehistory.fitnes.females)
summary(mod.fem.3)

mod.fem.4 <- lmer(OffspringRINGED.sum~StElo+I(StElo^2)+age+year+(1|BirdID),data=full.lifehistory.fitnes.females)
summary(mod.fem.4)

mod.fem.5 <- lmer(OffspringRINGED.sum~StElo+I(StElo^2)+age+I(age^2)+year+(1|BirdID),data=full.lifehistory.fitnes.females)
summary(mod.fem.5)

mod.fem.6 <- lmer(OffspringRINGED.sum~StElo+I(StElo^2)+age+I(age^2)+(1|BirdID),data=full.lifehistory.fitnes.females)
summary(mod.fem.6)

mod.fem.7 <- lmer(OffspringRINGED.sum~scale(StElo,scale=FALSE)+
                    #I(scale(StElo,scale=FALSE)^2)+
                    scale(age,scale=FALSE)+
                    I(scale(age,scale=FALSE)^2)+
                    scale(Tarsus.mean,scale=FALSE)+
                    year+
                    (1|BirdID),data=full.lifehistory.fitnes.females)
summary(mod.fem.7)





mod.male.1 <- lmer(OffspringRINGED.sum~StElo+age+Tarsus.mean+year+(1|BirdID),data=full.lifehistory.fitnes.males)
summary(mod.male.1)

mod.male.2 <- lmer(OffspringRINGED.sum~StElo+age+year+(1|BirdID),data=full.lifehistory.fitnes.males)
summary(mod.male.2)

mod.male.3 <- lmer(OffspringRINGED.sum~StElo+age+AvgOfEstimate.mean+year+(1|BirdID),data=full.lifehistory.fitnes.males)
summary(mod.male.3)

mod.male.4 <- lmer(OffspringRINGED.sum~StElo+AvgOfEstimate.mean+year+(1|BirdID),data=full.lifehistory.fitnes.males)
summary(mod.male.4)

mod.male.5 <- lmer(OffspringRINGED.sum~StElo+I(StElo^2)+age+I(age^2)+year+(1|BirdID),data=full.lifehistory.fitnes.males)
summary(mod.male.5)

mod.male.6 <- lmer(OffspringRINGED.sum~StElo+I(StElo^2)+age+I(age^2)+year+(1|BirdID),data=full.lifehistory.fitnes.males)
summary(mod.male.6)

mod.male.7 <- lmer(OffspringRINGED.sum~StElo+age+I(age^2)+year+(1|BirdID),data=full.lifehistory.fitnes.males)
summary(mod.male.7)


mod.male.8 <- lmer(OffspringRINGED.sum~scale(StElo)+
                     #I(scale(StElo,scale=FALSE)^2)+
                     #scale(age)+
                     #I(scale(age)^2)+
                     #scale(AvgOfEstimate.mean,scale=FALSE)+
                     scale(Tarsus.mean)+
                     year+
                     (1|BirdID),data=full.lifehistory.fitnes.males)
summary(mod.male.8)


#getting mean and CrI from the bayesian model
smod.male.8<-sim(mod.male.8,1000)
str(smod.male.8)

apply(smod.male.8@fixef,2, mean)
apply(smod.male.8@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod.male.8@ranef$individual,1, mean))
mean(apply(smod.male.8@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod.male.8@ranef$individual,1, quantile, c(0.975)))

mean(apply(smod.male.8@ranef$eventSW,1, mean))
mean(apply(smod.male.8@ranef$eventSW,1, quantile, c(0.025)))
mean(apply(smod.male.8@ranef$eventSW,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod.male.8))
compareqqnorm(mod.male.8) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod.male.8))
qqline(resid(mod.male.8))

#qq plot of random effects
qqnorm(unlist(ranef(mod.male.8)$BirdID))
qqline(unlist(ranef(mod.male.8)$BirdID))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod.male.8),resid(mod.male.8))



mod.male.9 <- lmer(OffspringRINGED.sum~StElo+AvgOfEstimate.mean+year+(1|BirdID),data=full.lifehistory.fitnes.males)
summary(mod.male.9)



# Plot for fledglings and rank in females

ggplot(full.lifehistory.fitnes.females, aes(StElo, OffspringRINGED.sum)) + 
  geom_point(alpha = 0.60,
             size=5,
             colour="grey50")+
  geom_line(aes(group=BirdID),
            alpha = 0.15,
            col="grey50")+
  #stat_summary(fun.data = "mean_cl_boot",
  #             size=1.5,
  #             aes(group= as.factor(age)),
  #             colour="darkorange",
  #             alpha=0.80)+
  scale_y_continuous("Number of social fledglings per year",
                     limits=c(0,12),
                     breaks=seq(0,12,1)) + 
  scale_x_continuous("Standardized Elo-rank",
                     limits=c(0,1),
                     breaks=seq(0,1,0.2))+
  geom_smooth(method="lm",colour="darkorange",fill="darkorange")+
  theme(panel.background = element_rect(fill = "white",colour="grey70"),
        #axis.line = element_line(colour = "grey20",
        #                        size = 0.5, linetype = "solid"),
        axis.text.x = element_text(color="black", size=20),
        axis.text.y = element_text(color="black", size=20),
        axis.title.x=element_text(size=25,color="black",margin=margin(30,0,20,0)),
        axis.title.y=element_text(size=25,color="black",margin=margin(30,30,20,20)),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())



# Plot for fledglings and rank in males

ggplot(full.lifehistory.fitnes.males, aes(StElo, OffspringRINGED.sum)) + 
  geom_point(alpha = 0.60,
             size=5,
             colour="grey50")+
  geom_line(aes(group=BirdID),
            alpha = 0.15,
            col="grey50")+
  #stat_summary(fun.data = "mean_cl_boot",
  #             size=1.5,
  #             aes(group= as.factor(age)),
  #             colour="darkorange",
  #             alpha=0.80)+
  scale_y_continuous("Number of social fledglings per year",
                     limits=c(0,12),
                     breaks=seq(0,12,1)) + 
  scale_x_continuous("Standardized Elo-rank",
                     limits=c(0,1),
                     breaks=seq(0,1,0.2))+
  geom_smooth(method="lm",colour="darkorange",fill="darkorange")+
  theme(panel.background = element_rect(fill = "white",colour="grey70"),
        #axis.line = element_line(colour = "grey20",
        #                        size = 0.5, linetype = "solid"),
        axis.text.x = element_text(color="black", size=20),
        axis.text.y = element_text(color="black", size=20),
        axis.title.x=element_text(size=25,color="black",margin=margin(30,0,20,0)),
        axis.title.y=element_text(size=25,color="black",margin=margin(30,30,20,20)),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())


#########################################################################################################
# # 12.7. Age and fitness
#########################################################################################################

# analysis

mod.male.9 <- glmer(ChickCoun3.sum~#scale(StElo)+
                      #I(scale(StElo,scale=FALSE)^2)+
                      scale(age)+
                      I(scale(age)^2)+
                      #scale(AvgOfEstimate.mean,scale=FALSE)+
                      scale(Tarsus.mean)+
                      scale(year)+
                      (1|BirdID),
                    data=male.social.fitnessperyear3,
                    family=poisson)

summary(mod.male.9)


#getting mean and CrI from the bayesian model
smod.male.9<-sim(mod.male.9,1000)
str(smod.male.9)

apply(smod.male.9@fixef,2, mean)
apply(smod.male.9@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod.male.9@ranef$individual,1, mean))
mean(apply(smod.male.9@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod.male.9@ranef$individual,1, quantile, c(0.975)))

mean(apply(smod.male.9@ranef$eventSW,1, mean))
mean(apply(smod.male.9@ranef$eventSW,1, quantile, c(0.025)))
mean(apply(smod.male.9@ranef$eventSW,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod.male.9))
compareqqnorm(mod.male.9) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod.male.9))
qqline(resid(mod.male.9))

#qq plot of random effects
qqnorm(unlist(ranef(mod.male.9)$BirdID))
qqline(unlist(ranef(mod.male.9)$BirdID))


#########################################################################################################
# # 12.8. Badge and fitness
#########################################################################################################

#########################################################################################################
# # # 12.8.1. Badge and anual fledglings
#########################################################################################################

# This database contains all my measurements of badge

male.social.fitnessperyear3.badge <- merge(ind.event.VB,
                                           male.social.fitnessperyear3,
                                           by.x="BirdID_eventSW",
                                           by.y="IDyear",
                                           all.x=TRUE)


mod.male.bib <- glmer(ChickCoun3.sum~
                        scale(age)+
                        I(scale(age)^2)+
                        scale(AvgOfEstimate.mean)+
                        scale(Tarsus.mean)+
                        scale(year)+
                        (1|BirdID),
                      data=male.social.fitnessperyear3.badge,
                      family=poisson)


summary(mod.male.bib)


#getting mean and CrI from the bayesian model
smod.male.bib<-sim(mod.male.bib,1000)
str(smod.male.bib)

apply(smod.male.bib@fixef,2, mean)
apply(smod.male.bib@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod.male.bib@ranef$individual,1, mean))
mean(apply(smod.male.bib@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod.male.bib@ranef$individual,1, quantile, c(0.975)))

mean(apply(smod.male.bib@ranef$eventSW,1, mean))
mean(apply(smod.male.bib@ranef$eventSW,1, quantile, c(0.025)))
mean(apply(smod.male.bib@ranef$eventSW,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod.male.bib))
compareqqnorm(mod.male.bib) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod.male.bib))
qqline(resid(mod.male.bib))

#qq plot of random effects
qqnorm(unlist(ranef(mod.male.bib)$BirdID))
qqline(unlist(ranef(mod.male.bib)$BirdID))


#########################################################################################################
# # # 12.8.1. Badge and anual recruits
#########################################################################################################


# This is the list of birds that breed from 2014 to 2016, I want to reduce and modify a bit

male.breeding3 <- male.breeding2

# getting the year for the estimates

male.breeding3$yearCode <- factor(substr(male.breeding3$BroodName, 1, 1))

male.breeding3$year <- ifelse(male.breeding3$yearCode=="N",
                              2014,
                              ifelse(male.breeding3$yearCode=="O",
                                     2015,2016))

male.breeding4 <- male.breeding3[,c("SocialDadID","year")]

male.breeding4$SocialDadID_year <- factor(paste(male.breeding4$SocialDadID,
                                                male.breeding4$year,
                                                sep="_"))


male.breeding5 <- unique(male.breeding4)

male.breeding6 <- subset(male.breeding5,male.breeding5$year<2016)


# Now we merge those two databases

social.recruits <- merge(male.breeding6,recruit.per.Dad2,by="SocialDadID_year",all.x=TRUE)


# convert the NA's in 0 as that's what they are (exceptions for the last layed broods from this year)

social.recruits$recruits2 <- ifelse(is.na(social.recruits$freq),
                                    0,social.recruits$freq)


# merging with Bib measurements

male.social.recruits.badge <- merge(ind.event.VB2,
                                    social.recruits,
                                    by.x="BirdID_eventSW",
                                    by.y="SocialDadID_year",
                                    all.x=TRUE)


mod.male.recruits.bib <- glmer(recruits2~
                                 #scale(age)+
                                 #I(scale(age)^2)+
                                 scale(AvgOfEstimate.mean)+
                                 #scale(Tarsus.mean)+
                                 scale(year)+
                                 (1|SocialDadID),
                               data=male.social.recruits.badge,
                               family=poisson)



summary(mod.male.recruits.bib)


#getting mean and CrI from the bayesian model
smod.male.bib<-sim(mod.male.bib,1000)
str(smod.male.bib)

apply(smod.male.bib@fixef,2, mean)
apply(smod.male.bib@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod.male.bib@ranef$individual,1, mean))
mean(apply(smod.male.bib@ranef$individual,1, quantile, c(0.025)))
mean(apply(smod.male.bib@ranef$individual,1, quantile, c(0.975)))

mean(apply(smod.male.bib@ranef$eventSW,1, mean))
mean(apply(smod.male.bib@ranef$eventSW,1, quantile, c(0.025)))
mean(apply(smod.male.bib@ranef$eventSW,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod.male.bib))
compareqqnorm(mod.male.bib) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod.male.bib))
qqline(resid(mod.male.bib))

#qq plot of random effects
qqnorm(unlist(ranef(mod.male.bib)$BirdID))
qqline(unlist(ranef(mod.male.bib)$BirdID))


KEEEEEP WORKING FROM HERE







# # I should get the same list by doing it this way. AND, YES I DO
# 
# birdsborn2014and2015 <- read.table("born2014-2015.csv",header=TRUE,sep=",")
# 
# socialdads <- read.table("SocialDads.csv",header=TRUE,sep=",")
# 
# 
# # limit the birdsborn to the ones that recruited later on
# 
# birdsborn2014and2015andrecruited <- merge(birdsborn2014and2015,socialdads,
#                                           by.x="BirdID",by.y="SocialDadID",
#                                           all.x=TRUE)
# 
# birdsborn2014and2015andrecruited2 <- subset(birdsborn2014and2015andrecruited,
#                                             !(is.na(birdsborn2014and2015andrecruited$BroodName)))
# 
# birdsborn2014and2015andrecruited3 <- birdsborn2014and2015andrecruited2[,c(1:3)]
# 
# birdsborn2014and2015andrecruited4 <- unique(birdsborn2014and2015andrecruited3)
# 
# 
# # Now I add the socialdad of these recruits so that I can count the number of recruits per male
# 
# birdsborn2014and2015andrecruited5<- rename(birdsborn2014and2015andrecruited4, 
#                                             c(BirdID="RecruitID",
#                                               BroodRef.x="BroodRef"))
# 
# 
# SocialDadsandrecruits <-merge(birdsborn2014and2015andrecruited5,
#                               socialdads,
#                               by="BroodRef",
#                               all.x=TRUE)
# 
# 
# recruit.per.Dad2 <- count(SocialDadsandrecruits,c("SocialDadID","Cohort"))
# 
# recruit.per.Dad3 <- subset(recruit.per.Dad2,!(is.na(recruit.per.Dad2$SocialDadID)))







#########################################################################################################
# # 12.9. Extra, sensitive analyses
#########################################################################################################

###

mod1 <- lmer(StElo~age+Tarsus.mean+as.factor(SexEstimate)+(1|individual),data=full.lifehistory.2)
summary(mod1)

mod2 <- lmer(StElo~age+Mass.mean+as.factor(SexEstimate)+(1|individual),data=full.lifehistory.2)
summary(mod2)

mod3 <- lmer(StElo~age+AvgOfEstimate.mean+Tarsus.mean+(1|individual)+(1|eventSW),data=full.lifehistory.2)
summary(mod3)

mod4 <- lmer(StElo~age+AvgOfEstimate.mean+(1|individual)+(1|eventSW),data=full.lifehistory.2)
summary(mod4)

mod5 <- lmer(StElo~AvgOfEstimate.mean+(1|individual),data=full.lifehistory.2)
summary(mod5)

mod6 <- lmer(StElo~AvgOfEstimate.mean+Tarsus.mean+(1|individual)+(1|eventSW),data=full.lifehistory.2)
summary(mod6)



full.lifehistory.2.males <- subset(full.lifehistory.2,full.lifehistory.2$SexEstimate==1)
full.lifehistory.2.males <- subset(full.lifehistory.2.males,full.lifehistory.2.males$AvgOfEstimate.mean>36)


mod7 <- lmer(StElo~age+Tarsus.mean+(1|individual),data=full.lifehistory.2.males)
summary(mod7)

mod8 <- lmer(StElo~age+AvgOfEstimate.mean+Tarsus.mean+(1|individual)+(1|eventSW),data=full.lifehistory.2)
summary(mod8)

mod9 <- lmer(StElo~age+AvgOfEstimate.mean+Tarsus.mean+(1|individual)+(1|eventSW),data=full.lifehistory.2.males)
summary(mod9)

###


rpt.St.MCMC <- rpt(elo_scores_all_events$StElo,
                   elo_scores_all_events$individual,
                   datatype="Gaussian",
                   method="MCMC",
                   nboot=1000,
                   npermut=1000)


# # Limiting analyses to those individuals that interacted more than 8 times
# 
# 
# for(i in 1:nrow(full.lifehistory.fitnes.females)){
#   
#   if((full.lifehistory.fitnes.females$individual[i] %in% list_morethan8)){
#     
#     full.lifehistory.fitnes.females$morethan8 <- 1
#     
#   } else {
#     
#     full.lifehistory.fitnes.females$morethan8 <- 0
#     
#   }   
#   
# }
# 
# full.lifehistory.fitnes.females.8 <- subset(full.lifehistory.fitnes.females,
#                                             full.lifehistory.fitnes.females$morethan8==1)
# 
# 
# 
# for(i in 1:nrow(full.lifehistory.fitnes.males)){
#   
#   if((full.lifehistory.fitnes.males$individual[i] %in% list_morethan8)){
#     
#     full.lifehistory.fitnes.males$morethan8 <- 1
#     
#   } else {
#     
#     full.lifehistory.fitnes.males$morethan8 <- 0
#     
#   }   
#   
# }
# 
# full.lifehistory.fitnes.males.8 <- subset(full.lifehistory.fitnes.males,
#                                             full.lifehistory.fitnes.males$morethan8==1)



#########################################################################################################
# 13. CAPTIVE ANALYSES
#########################################################################################################


dom.cap <- read.table("captive-dom-levels-v1.csv",header=TRUE,sep=",")

dom.cap.2 <- subset(dom.cap,dom.cap$Level!=0)

dom.cap.3 <- subset(dom.cap.2,dom.cap.2$Level!=1)

dom.cap.3$Loser_Aviary <- factor(paste(dom.cap.3$Loser,dom.cap.3$Aviary,sep="_"))

dom.cap.3$Winner_Aviary <- factor(paste(dom.cap.3$Winner,dom.cap.3$Aviary,sep="_"))


# data set with age and ID!

ID <- read.table("captivesparrowsage2014-v2.csv",header=TRUE,sep=",")

ID$newcc_NewAv <- factor(paste(ID$newcc,ID$NewAv,sep="_"))

ID.2 <- ID[,c("newcc_NewAv","BTO")]


# Merging both databases

dom.cap.ID <- merge(dom.cap.3,ID.2,
                    by.x="Loser_Aviary",by.y="newcc_NewAv",
                    all.x=TRUE)

dom.cap.ID.2 <- subset(dom.cap.ID,!(is.na(dom.cap.ID$BTO)))

library(reshape)
dom.cap.ID.3 <- rename(dom.cap.ID.2, c(BTO="Loser2"))

dom.cap.ID.4 <- merge(dom.cap.ID.3,ID.2,
                      by.x="Winner_Aviary",by.y="newcc_NewAv",
                      all.x=TRUE)

dom.cap.ID.5 <- rename(dom.cap.ID.4, c(BTO="Winner2"))

# final database

final.dom.cap <- dom.cap.ID.5[,c("Date","Aviary","Loser2","Winner2","Level","Draw")]

final.dom.cap.2 <- rename(final.dom.cap, c(Loser2="Loser",Winner2="Winner"))

final.dom.cap.2$Date <- factor(final.dom.cap.2$Date)

levels(final.dom.cap.2$Date)

final.dom.cap.3 <- subset(final.dom.cap.2,final.dom.cap.2$Date!="2015-02-16")
final.dom.cap.3 <- subset(final.dom.cap.3,final.dom.cap.3$Date!="2015-02-23")

final.dom.cap.3$Date <- factor(final.dom.cap.3$Date)

#levels(final.dom.cap.3$Date)


# getting the elo-ratings per individual, for that I first divide the database in 1 per aviary

########################################################################################################
# # 13.C.2. Obtaining the elo-scores for each individual
########################################################################################################

# First, spliting the database by event


#     Creating a different database per event with this for loop, and at the same time, creating a 
# database per event with elo_scores

seqcheck(winner=final.dom.cap.3$Winner, 
         loser=final.dom.cap.3$Loser, 
         Date=final.dom.cap.3$Date, 
         draw=final.dom.cap.3$Draw)

# there are some typos because loser ID equals winner ID, I'm going to remove them from the database

final.dom.cap.3<-final.dom.cap.3[final.dom.cap.3$Loser!=final.dom.cap.3$Winner,]

final.dom.cap.3$Aviary <- as.factor(final.dom.cap.3$Aviary)


#####
# Counting
#####

########################################################################################################
# # # 13.9.1. Generating a list of the individuals included in the database
########################################################################################################

#       This generates a list of the individiuals by combining the columns Loser and Winner
# This way we can count interactions by counting number of times that each individual shows up in the
# list.


# List with all individuals interacting as Loser and converting it as data.frame

list.Loser <- final.dom.cap.3$Loser

list.Loser <- as.data.frame(list.Loser)


# List with all individuals interacting as Winner and converting it as dataframe

list.Winner <- final.dom.cap.3$Winner

list.Winner <- as.data.frame(list.Winner)


#       Here I rename the variable in list.Winner and list.Loser so that they have the same names, otherwise, rbind 
# does not work the way I want it to work

names(list.Loser) <- c("BirdID")
names(list.Winner) <- c("BirdID")


# Now I construct a variable by rbinding together the 2 dataframes

list.x <- rbind(list.Loser,list.Winner)


########################################################################################################
# # # 13.9.2. Counting number of interactions per individual for the whole database
########################################################################################################

# Generating a database with the individuals and their number of total interactions in the whole dataset

ind.counts <- count(list.x,"BirdID")

names(ind.counts)<-c("BirdID","totalfreq")


#number of dates interacting per individual
hist(ind.counts$totalfreq,freq=TRUE,breaks=306,
     main = "",
     xlab = "Number of interactions",
     ylim = c(0,4),
     xlim = c(0,325),
     ylab = "",
     col="grey75",
     axes=FALSE,
     cex.lab=1.75,
     right=FALSE)
title(ylab="Number of individuals", line=2.2, cex.lab=1.75)
axis(1,at = seq(0,325,by=25),lwd=1)
axis(2,at = seq(0,4,by=1),lwd=1,line=-0.75, las=2)


########################################################################################################
# # # 13.9.3. Counting number of interactions per individual per date
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


########################################################################################################
# # # 3.9.4. Counting number of dates each individual showed up in
########################################################################################################

# This allows me to count the number of dates each individual showed up in

onlyind <- ind.counts.date$BirdID

onlyind <- as.data.frame(onlyind)


# Counting number of dates per individual

superlist.num.date <- count(onlyind,"onlyind")

names(superlist.num.date) <- c("BirdID","freqofdates")


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




#####

#####

counter <- 1

for(i in levels(final.dom.cap.3$Aviary)){
  
  x<-subset(final.dom.cap.3, final.dom.cap.3$Aviary==i)
  assign(paste0("final.com.cap",counter),x)
  y<-elo.seq(winner=x$Winner, 
             loser=x$Loser, 
             Date=x$Date, 
             draw=x$Draw)
  print(summary(y))
  assign(paste0("cap_elo_scores.",counter),y)
  counter <- counter + 1
}


########################################################################################################
# # 9.C.3. Stability coefficient
########################################################################################################

stab.elo(cap_elo_scores.1)
stab.elo(cap_elo_scores.2)
stab.elo(cap_elo_scores.3)
stab.elo(cap_elo_scores.4)


########################################################################################################
# # 9.C.5. Extracting elo-ratings per individual
########################################################################################################

# this can be probably included in the for loop of line 2758

cap.elo_scores_ind.1 <- extract.elo(cap_elo_scores.1,standardize = TRUE)
cap.elo_scores_ind.2 <- extract.elo(cap_elo_scores.2,standardize = TRUE)
cap.elo_scores_ind.3 <- extract.elo(cap_elo_scores.3,standardize = TRUE)
cap.elo_scores_ind.4 <- extract.elo(cap_elo_scores.4,standardize = TRUE)


#     This code is to make a dataframe out of those individuals scores, please, change the number for the
# event you want!

# creating the name of the rows, basically from 1 to total number of individuals

# Creating a database with elo_scores per event

counter <- 1

for(i in levels(final.dom.cap.3$Aviary)){
  
  x<-subset(final.dom.cap.3, final.dom.cap.3$Aviary==i)
  y<-elo.seq(winner=x$Winner, 
             loser=x$Loser, 
             Date=x$Date, 
             draw=x$Draw)
  
  w<-extract.elo(y,standardize = TRUE)
  
  rownames <- seq(1,length(w),
                  1)
  
  # making a data.frame with the elo-ratings
  scores <- as.data.frame(w,
                          row.names = as.character(rownames))
  
  z <- cbind(attributes(w),
             scores)
  
  z$Aviary <- counter
  
  names(z) <- c("individual","StElo","Aviary")
  
  assign(paste0("cap_elo_scores_ind.db.",counter),z)
  
  counter <- counter + 1
  
}


# creating a database with all these observations
cap_elo_scores_all_events <- rbind(cap_elo_scores_ind.db.1,
                                   cap_elo_scores_ind.db.2,
                                   cap_elo_scores_ind.db.3,
                                   cap_elo_scores_ind.db.4)



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

ID.age$age2014 <- ID.age$age2014+0.5

db.noNA.age<-merge(db.noNA,ID.age,by.x="Ring.ID",by.y="BTO",all.x=TRUE)


# reducing to my own measurements

db.noNA.age.AST<-subset(db.noNA.age,db.noNA.age$Obs=="AST")


# mean VB and age, and then adding tarsus

id.meanVB.age <-summaryBy(meanVB + age2014 ~ Ring.ID, data = db.noNA.age.AST, 
                          FUN = list(mean))


id.tarsus <- subset(db.noNA.age.AST,!(is.na(db.noNA.age.AST$TarsusLength)))

id.tarsus2 <- id.tarsus[,c("Ring.ID","TarsusLength")]


id.meanVB.age.TL <- merge(id.meanVB.age,id.tarsus2,by="Ring.ID",all.x=TRUE)


# Final database to run the analyses

final.cap.db <- merge(cap_elo_scores_all_events,id.meanVB.age.TL,
                      by.x="individual",by.y="Ring.ID",all.x=TRUE)

####
# ANALYSES
####



# First, the predictors, are they correalted to each other?

collinearity <- final.cap.db[,c("StElo","meanVB.mean","age2014.mean","TarsusLength")]

cor(collinearity,use="complete.obs")

pairs(collinearity)


# Bib size and rank


mod.cap1 <- lmer(StElo~#scale(age2014.mean)+
                   scale(meanVB.mean)+
                   #I(scale(meanVB.mean)^2)+
                   scale(TarsusLength)+
                   (1|Aviary),data=final.cap.db)


summary(mod.cap1)


#getting mean and CrI from the bayesian model
smod.cap1<-sim(mod.cap1,1000)
str(smod.cap1)

apply(smod.cap1@fixef,2, mean)
apply(smod.cap1@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod.cap1@ranef$Aviary,1, mean))
mean(apply(smod.cap1@ranef$Aviary,1, quantile, c(0.025)))
mean(apply(smod.cap1@ranef$Aviary,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod.cap1))
compareqqnorm(mod.cap1) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod.cap1))
qqline(resid(mod.cap1))

#qq plot of random effects
qqnorm(unlist(ranef(mod.cap1)$Aviary))
qqline(unlist(ranef(mod.cap1)$Aviary))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod.cap1),resid(mod.cap1))


# Age and rank


mod.cap2 <- lmer(StElo~scale(age2014.mean)+
                   #I(scale(age2014.mean)^2)+
                   scale(TarsusLength)+
                   (1|Aviary),data=final.cap.db)


summary(mod.cap2)


#getting mean and CrI from the bayesian model
smod.cap2<-sim(mod.cap2,1000)
str(smod.cap2)

apply(smod.cap2@fixef,2, mean)
apply(smod.cap2@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod.cap2@ranef$Aviary,1, mean))
mean(apply(smod.cap2@ranef$Aviary,1, quantile, c(0.025)))
mean(apply(smod.cap2@ranef$Aviary,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod.cap2))
compareqqnorm(mod.cap2) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod.cap2))
qqline(resid(mod.cap2))

#qq plot of random effects
qqnorm(unlist(ranef(mod.cap2)$Aviary))
qqline(unlist(ranef(mod.cap2)$Aviary))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod.cap2),resid(mod.cap2))


# bib and age


mod.cap3 <- lmer(meanVB.mean~scale(age2014.mean)+
                   #I(scale(age2014.mean)^2)+
                   scale(TarsusLength)+
                   (1|Aviary),data=final.cap.db)


summary(mod.cap3)


#getting mean and CrI from the bayesian model
smod.cap3<-sim(mod.cap3,1000)
str(smod.cap3)

apply(smod.cap3@fixef,2, mean)
apply(smod.cap3@fixef,2, quantile, c(0.025, 0.975))

mean(apply(smod.cap3@ranef$Aviary,1, mean))
mean(apply(smod.cap3@ranef$Aviary,1, quantile, c(0.025)))
mean(apply(smod.cap3@ranef$Aviary,1, quantile, c(0.975)))

#residual anaylisis, qq plot of residuals, etc
#library(blmeco)
hist(fitted(mod.cap3))
compareqqnorm(mod.cap3) #to check if I can visually identify the plot of my model

#model qqplot
qqnorm(resid(mod.cap3))
qqline(resid(mod.cap3))

#qq plot of random effects
qqnorm(unlist(ranef(mod.cap3)$Aviary))
qqline(unlist(ranef(mod.cap3)$Aviary))

#fitted versus residuals
par(mfrow=c(1,1))
scatter.smooth(fitted(mod.cap3),resid(mod.cap3))


#3333333