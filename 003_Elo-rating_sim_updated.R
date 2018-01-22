# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 20th October, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to estimate dominance status using the randomized Elo-rating
# from R package "aniDom" (Farine & Sanchez-Tojar 2017, https://cran.r-project.org/web/packages/aniDom/index.html)


########################################################################################################
# Packages needed
########################################################################################################

# packages needed
library(EloRating)
#require(plyr)
#library(arm)
library(rptR)
#library(EloChoice)
library(aniDom)
library(compete)

# Clear memory
rm(list=ls())

##############################################################
# Function needed
##############################################################

#this update will be soon in aniDom 1.1.3. It improves the function
elo_scores_2 <- function (winners, losers, identities = NULL, sigmoid.param = 1/100, 
                          K = 200, init.score = 0, randomise = FALSE, n.rands = 1000, 
                          return.as.ranks = FALSE, return.trajectories = FALSE) 
{
  if (is.null(identities)) {
    identities <- unique(c(winners, losers))
  }
  n.inds <- length(identities)
  if (randomise == FALSE) {
    n.rands <- 1
  }
  if (sum(c(winners, losers) %in% identities) < length(c(winners, 
                                                         losers))) {
    stop("Not all winners and/or losers are contained in identities")
  }
  T <- length(winners)
  if (return.trajectories) {
    if (randomise == TRUE) {
      all.scores <- array(init.score, c(n.inds, T + 1, 
                                        n.rands))
    }
    else {
      all.scores <- array(init.score, c(n.inds, T + 1))
    }
  }
  else {
    all.scores <- array(init.score, c(n.inds, n.rands))
  }
  rownames(all.scores) <- identities
  if (length(K) == 1) {
    K <- rep(K, T)
  }
  for (r in 1:n.rands) {
    if (randomise == FALSE) {
      ord <- 1:T
    }
    else {
      ord <- sample(1:T, T, replace = F)
    }
    winners.perm <- winners[ord]
    losers.perm <- losers[ord]
    scores <- array(NA, c(n.inds, T + 1))
    scores[, 1] <- init.score
    for (i in 1:T) {
      scores[, i + 1] <- scores[, i]
      winner <- which(identities == winners.perm[i])
      loser <- which(identities == losers.perm[i])
      p <- 1/(1 + exp(-sigmoid.param * abs((scores[winner, 
                                                   i] - scores[loser, i]))))
      if (scores[winner, i] >= scores[loser, i]) {
        scores[winner, i + 1] <- scores[winner, i] + 
          (1 - p) * K[i]
        scores[loser, i + 1] <- scores[loser, i] - (1 - 
                                                      p) * K[i]
      }
      else {
        scores[winner, i + 1] <- scores[winner, i] + 
          p * K[i]
        scores[loser, i + 1] <- scores[loser, i] - 
          p * K[i]
      }
    }
    if (return.trajectories) {
      if (randomise == TRUE) {
        all.scores[, , r] <- scores
      }
      else {
        all.scores <- scores
      }
    }
    else {
      all.scores[, r] <- scores[, T + 1]
    }
  }
  freq <- table(factor(c(winners, losers), levels = identities))
  all.scores[which(identities %in% names(freq)[which(freq == 
                                                       0)]), ] <- NA
  if (return.as.ranks == TRUE) {
    all.ranks <- apply(all.scores, 2, function(x) {
      rank(-x)
    })
    all.ranks[is.na(all.scores)] <- NA
    all.scores <- all.ranks
  }
  invisible(all.scores)
}


#################
# Database needed
#################
# loading the clean database to estimate the ratings.
dom.final.v2 <- read.table("dom.final.v2.csv",header=TRUE,sep=",")


# # Creating date as require by EloRating
# dom.final.v2$date.ELO2.2 <- as.factor(paste(dom.final.v2$year,
#                                             dom.final.v2$month,
#                                             dom.final.v2$day,
#                                             sep="-"))

dom.final.v2$eventSW <- as.factor(dom.final.v2$eventSW)


########################################################################################################
# 1. Obtaining dominance status
########################################################################################################

#using aniDom to estimate randomized Elo-ratings
counter <- 1

for(i in levels(dom.final.v2$eventSW)){
  
  x<-subset(dom.final.v2, dom.final.v2$eventSW==i)
  assign(paste0("dom.eventSW",counter),x)
  
  matrix <- get_wl_matrix(x[,c("Winner","Loser")])
  ttri<-ttri_test(matrix)$ttri
  Ttripval<-ttri_test(matrix)$pval
  
  y<-elo_scores_2(as.character(x$Winner),
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
  
  s <- estimate_uncertainty_by_splitting(as.character(x$Winner),
                                             as.character(x$Loser),
                                             identities = unique(c(as.character(x$Winner),as.character(x$Loser))),
                                         randomise = TRUE)
  
  #saving data
  if(counter==1){
    domUncertainty <- rbind(c(counter,r))
    domSplit <- rbind(c(counter,s))
    Ttris <- ttri
    Ttrips <- Ttripval
  } else {
    domUncertainty <- rbind(domUncertainty,c(counter,r))
    domSplit <- rbind(domSplit,c(counter,s))
    Ttris <- c(Ttris,ttri)
    Ttrips <- c(Ttrips,Ttripval)
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

