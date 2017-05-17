# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 20th October, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to test what is the effect of randomizing the order in which the
# interactions take place. For that, I simulate 1000 StElo values per individual per event,
# each time randomizing the order


########################################################################################################
# Packages needed
########################################################################################################

# packages needed to be loaded for this script (a couple of them might be only needed in the following
# script)

library(EloRating)
#require(plyr)
#library(arm)
library(rptR)
library(EloChoice)
library(aniDom)

# Clear memory and get to know where you are
rm(list=ls())
#getwd()


# loading the clean database to estimate the ratings. Same as the one
# used in 003_Elo-rating.R

dom.final.v2 <- read.table("dom.final.v2.csv",header=TRUE,sep=",")


# # Creating date as require by EloRating
# 
# dom.final.v2$date.ELO2.2 <- as.factor(paste(dom.final.v2$year,
#                                             dom.final.v2$month,
#                                             dom.final.v2$day,
#                                             sep="-"))

dom.final.v2$eventSW <- as.factor(dom.final.v2$eventSW)

########################################################################################################
# 1. Obtaining 1000 elo-scores for each individual (using EloChoice())
########################################################################################################

#using elochoice() to quicly estimate randomized Elo-ratings

counter <- 1

for(i in levels(dom.final.v2$eventSW)){
  
  x<-subset(dom.final.v2, dom.final.v2$eventSW==i)
  assign(paste0("dom.eventSW",counter),x)
  y<-elo_scores(as.character(x$Winner),
                as.character(x$Loser),
                identities = unique(c(as.character(x$Winner),as.character(x$Loser))),
                randomise=TRUE)
  mean.scores <- rowMeans(y)
  assign(paste0("elo_scores_ind.",counter),mean.scores)
  rownames <- seq(1,length(mean.scores),
                  1)
  scores <- as.data.frame(mean.scores,
                          row.names = as.character(rownames))
  
  z <- cbind(attributes(mean.scores),
             scores)
  
  z$eventSW <- counter
  
  names(z) <- c("individual","StElo","eventSW")
  
  assign(paste0("elo_scores_ind.db.",counter),z)
  
  
  #estimating repeatability for uncertainty
  
  r <- estimate_uncertainty_by_repeatability(as.character(x$Winner),
                                             as.character(x$Loser),
                                             identities = unique(c(as.character(x$Winner),as.character(x$Loser))))
  if(counter==1){
    domUncertainty <- rbind(c(counter,r))
  } else {
    domUncertainty <- rbind(domUncertainty,c(counter,r))
  }
  
  counter <- counter + 1
  
  
}


# creating a database with all these observations
elo_scores_all_events <- rbind(elo_scores_ind.db.1,
                               elo_scores_ind.db.2,
                               elo_scores_ind.db.3,
                               elo_scores_ind.db.4,
                               elo_scores_ind.db.5,
                               elo_scores_ind.db.6,
                               elo_scores_ind.db.7)


# Estimating repeatability of dominance rank. I'm using MCMC method, see above for ANOVA and REML.

sink("summaries/simple_repeatability_Lundy_sim.txt")

rpt.St.MCMC <- rpt(StElo~(1|individual),
                   grname="individual",
                   datatype="Gaussian",
                   data=elo_scores_all_events,
                   nboot=10000,
                   npermut=1)

rpt.St.MCMC

sink()


sink("summaries/domRepUncertainty.txt")

domUncertainty

sink()


# NOw is the same format as the original one. Save it!

write.csv(elo_scores_all_events,
          "elo_scores_all_events_aniDom.csv",row.names=FALSE)

