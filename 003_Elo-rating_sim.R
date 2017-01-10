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
require(plyr)
library(arm)
library(rptR)

# Clear memory and get to know where you are
rm(list=ls())
#getwd()


# loading the clean database to estimate the ratings. Same as the one
# used in 003_Elo-rating.R

dom.final.v2 <- read.table("dom.final.v2.csv",header=TRUE,sep=",")


# # sorting by date and time
# 
# dom.final.v2 <- dom.final.v2[order(dom.final.v2$date,
#                                    dom.final.v2$realtime2),]


# Creating date as require by EloRating

dom.final.v2$date.ELO2.2 <- as.factor(paste(dom.final.v2$year,
                                            dom.final.v2$month,
                                            dom.final.v2$day,
                                            sep="-"))

dom.final.v2$eventSW <- as.factor(dom.final.v2$eventSW)


########################################################################################################
# 1. Obtaining 1000 elo-scores for each individual (manual way)
########################################################################################################
# 
# # This for loop does the thing, it takes about 1 hour, so I saved the
# # results and I can import then from now on (unless you wanna run it
# # again)
# 
# listofdb <- list()
# 
# for (sim in 1:1000){
#   
#   counter <- 1
#   
#   for(i in levels(dom.final.v2$eventSW)){
#     
#     x<-subset(dom.final.v2, dom.final.v2$eventSW==i)
#     assign(paste0("dom.eventSW",counter),x)
#     
#     rand <- sample(nrow(x))
#     x<-x[rand, ]
#     
#     y<-elo.seq(winner=x$Winner, 
#                loser=x$Loser, 
#                Date=x$date.ELO2.2, 
#                draw=x$Draw,
#                progressbar=FALSE)
#     w<-extract.elo(y,standardize = TRUE)
#     assign(paste0("elo_scores.",counter),y)
#     assign(paste0("elo_scores_ind.",counter),w)
#     rownames <- seq(1,length(w),
#                     1)
#     scores <- as.data.frame(w,
#                             row.names = as.character(rownames))
#     
#     z <- cbind(attributes(w),
#                scores)
#     
#     z$eventSW <- counter
#     
#     names(z) <- c("individual","StElo","eventSW")
#     
#     assign(paste0("elo_scores_ind.db.",counter),z)
#     counter <- counter + 1
#   }
#   
#   # creating a database with all these observations
#   elo_scores_all_events <- rbind(elo_scores_ind.db.1,
#                                  elo_scores_ind.db.2,
#                                  elo_scores_ind.db.3,
#                                  elo_scores_ind.db.4,
#                                  elo_scores_ind.db.5,
#                                  elo_scores_ind.db.6)
#   
#   elo_scores_all_events$ind_event <- as.factor(paste(elo_scores_all_events$individual,
#                                                      elo_scores_all_events$eventSW,
#                                                      sep="_"))
#   
#   assign(paste0("elo_scores_all_events.",sim),
#          elo_scores_all_events)
#   
#   listofdb[[sim]] <- elo_scores_all_events[,c("ind_event","StElo")]
#   
# }
# 
# 
# 
# elo_ratings_sim <- join_all(listofdb,
#                             by="ind_event")
# 
# write.csv(elo_ratings_sim,
#           "elo_ratings_sim.csv",row.names=FALSE)

# importing the result of the simulations, no need to run it again,
# it takes about 1 h

elo_ratings_sim <- read.table("Elo-Rating_robustness/elo_ratings_sim.csv",header=TRUE,sep=",")


########################################################################################################
# 2. Obtaining mean and 95% CI elo-score for each individual
########################################################################################################

# Estimating mean of all simulations per individual

sim <- 1000 #it has to be the same as simulations were run in the for loop

RowM <- as.data.frame(cbind(as.character(elo_ratings_sim[,1]),
                            rowMeans(elo_ratings_sim[,c(2:(sim+1))]),
                            apply(elo_ratings_sim[,c(2:(sim+1))], 1, 
                                  quantile, probs = c(0.025)),
                            apply(elo_ratings_sim[,c(2:(sim+1))], 1, 
                                  quantile, probs = c(0.975))),
                      stringsAsFactors=FALSE)

RowM$V1 <- as.factor(RowM$V1)
RowM$V2 <- as.numeric(RowM$V2)
RowM$V3 <- as.numeric(RowM$V3)
RowM$V4 <- as.numeric(RowM$V4)


# changing the name of variables to more informative ones

names(RowM) <- c("ind_event","meanStElo",
                 "lower95CI","upper95CI")


# Estimating the lenght of the 95% CI per individual

RowM$CI.length <- RowM$upper95CI-RowM$lower95CI
#mean(RowM$CI.length)


########################################################################################################
# 3. Checking if error is different depending on Elo
########################################################################################################

Stdependence<-lm(CI.length~meanStElo,data=RowM)


#simulating a posterior distribution with 5000 draws

sStdependence<-sim(Stdependence,5000)


# Generating a database with what is run in the model.

newdat<-expand.grid(meanStElo=seq(min(RowM$meanStElo,na.rm = TRUE),
                                  max(RowM$meanStElo,na.rm = TRUE),
                                  0.001))

xmat<-model.matrix(~meanStElo, 
                   data=newdat) 

fitmatboth <- matrix(NA, 
                     ncol = nrow(sStdependence@coef),
                     nrow = nrow(newdat))


for(i in 1:nrow(sStdependence@coef)) {
  fitmatboth[,i] <- xmat%*%sStdependence@coef[i,]
}


# finally estimating the mean and the credible intervals for each
# value of bib length. This is what I will plot later on.

newdat$fit<-apply(fitmatboth, 1, mean) # 1= row , 2 = colum
newdat$lower<-apply(fitmatboth, 1, quantile, prob= 0.025)
newdat$upper<-apply(fitmatboth, 1, quantile, prob= 0.975)


########################################################################################################
# 4. Plotting if error is different depending on Elo
########################################################################################################

rgbing <- c(255,255,255)
chocolate1 <- c(255,127,36)/rgbing
darkblue <- c(31,120,180)/rgbing

plot(RowM$meanStElo,
     RowM$CI.length, 
     xlab="StElo-rating",
     ylab="Length of 95% CI",
     cex.lab=1.6,
     xlim=c(0,1),ylim=c(0,1),
     family="serif",
     type="n")

points(RowM$meanStElo,
       RowM$CI.length, 
       pch = 19, col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.4),
       cex = 1)

polygon(c(newdat$meanStElo,rev(newdat$meanStElo)),
        c(newdat$lower,rev(newdat$upper)),
        border=NA,col=rgb(chocolate1[1],chocolate1[2],chocolate1[3], 0.15))

lines(newdat$meanStElo, newdat$fit, lwd=3.5,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8)) 

lines(newdat$meanStElo, newdat$lower, lty=2, lwd=2,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8))

lines(newdat$meanStElo, newdat$upper, lty=2, lwd=2,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8))

text(0.9,0.5,"Adjusted R-squared:  0.20\nEstimate=0.28") # from summary(Stdependence)


# adding the mean of the population mean and the 95% CI

# Estimating pop.mean of all simulations

pop.means<-colMeans(elo_ratings_sim[,c(2:(sim+1))])
CI <- quantile(pop.means,prob=c(0.025,0.975))
#mean(pop.means)

rect(CI[1],0,CI[2],1, col=rgb(darkblue[1], darkblue[2], darkblue[3],0.1), border = "white")

arrows(CI[1],0,CI[1],1,angle=90,code=3,
       col=rgb(darkblue[1], darkblue[2], darkblue[3],1),length = 0,lwd=1, lty=3)

arrows(CI[2],0,CI[2],1,angle=90,code=3,
       col=rgb(darkblue[1], darkblue[2], darkblue[3],1),length = 0,lwd=1, lty=3)

arrows(mean(pop.means),0,mean(pop.means),1,angle=90,code=1,
       col=rgb(darkblue[1], darkblue[2], darkblue[3],1),length = 0,lwd=2, lty=1)

text(0.51,0.95,"Mean = 0.43\n5.24% variation") # from mean(pop.means) and its 95%CI


########################################################################################################
# 5. Plotting all individuals' mean and 95%CI
########################################################################################################

# sorted from subordinates to dominants

RowM <- RowM[order(RowM$meanStElo),]

plot(RowM$meanStElo,
     type="n",
     xlab="individual",
     ylab="StElo-rating",
     cex.lab=1.6,
     xlim=c(0,length(RowM$ind_event)),ylim=c(0,1),
     family="serif")

for(i in 1:nrow(RowM)){
  
  points(i,RowM$meanStElo[i])
  lines(x = c(i,i),
        y = c(RowM$lower95CI[i], RowM$upper95CI[i]))
}

text(100,0.8,"Mean 95%CI = 0.26\n26% variation then")


########################################################################################################
# 6. Checking correlation between original StElo and simulated one
########################################################################################################

# importing original StElos

elo_scores_all_events <- read.table("elo_scores_all_events.csv",header=TRUE,sep=",")

elo_scores_all_events$ind_event <- as.factor(paste(elo_scores_all_events$individual,
                                                   elo_scores_all_events$eventSW, 
                                                   sep="_"))

elo_scores_all_events.2 <- elo_scores_all_events[,c("ind_event","StElo")]

names(elo_scores_all_events.2) <- c("ind_event","originalStElo")


# Now merging it with simulation and testing correaltion

elo_comp <- merge(RowM,
                  elo_scores_all_events.2,
                  by="ind_event")


# actual model

Elo.robust <- lm(originalStElo~meanStElo,data=elo_comp)
#summary(Elo.robust)


# plotting residuals
par(mfrow=c(2,2))
plot(Elo.robust)
par(mfrow=c(1,1))


#simulating a posterior distribution with 5000 draws
sElo.robust<-sim(Elo.robust,5000)


newdat<-expand.grid(meanStElo=seq(min(elo_comp$meanStElo,na.rm = TRUE),
                                  max(elo_comp$meanStElo,na.rm = TRUE),
                                  0.001))

xmat<-model.matrix(~meanStElo, 
                   data=newdat) 

fitmatboth <- matrix(NA, 
                     ncol = nrow(sElo.robust@coef),
                     nrow = nrow(newdat))


for(i in 1:nrow(sElo.robust@coef)) {
  fitmatboth[,i] <- xmat%*%sElo.robust@coef[i,]
}


# finally estimating the mean and the credible intervals for each
# value of bib length. This is what I will plot later on.

newdat$fit<-apply(fitmatboth, 1, mean) # 1= row , 2 = colum
newdat$lower<-apply(fitmatboth, 1, quantile, prob= 0.025)
newdat$upper<-apply(fitmatboth, 1, quantile, prob= 0.975)


########################################################################################################
# 7. Plotting correlation between original StElo and simulated one
########################################################################################################

rgbing <- c(255,255,255)
chocolate1 <- c(255,127,36)/rgbing
darkblue <- c(31,120,180)/rgbing

plot(elo_comp$meanStElo,
     elo_comp$originalStElo, 
     xlab="simulated StElo-rating",
     ylab="original StElo-rating",
     cex.lab=1.6,
     xlim=c(0,1),ylim=c(0,1),
     family="serif",
     type="n")

points(elo_comp$meanStElo,
       elo_comp$originalStElo,
       pch = 19, col=rgb(darkblue[1], darkblue[2], darkblue[3],0.4),
       cex = 1)

polygon(c(newdat$meanStElo,rev(newdat$meanStElo)),
        c(newdat$lower,rev(newdat$upper)),
        border=NA,col=rgb(darkblue[1],darkblue[2],darkblue[3], 0.15))

lines(newdat$meanStElo, newdat$fit, lwd=3.5,
      col=rgb(darkblue[1], darkblue[2], darkblue[3],0.8)) 

abline(a = 0, b = 1,lty=2)

text(0.1,0.9,"Adjusted R-squared:  0.81\nEstimate=0.95") # from summary(Stdependence)


########################################################################################################
# 8. Saving the simulated Elo-ratings to re-run my analyses
########################################################################################################

# first formatting it as the original database

elo_scores_all_events_sim <- RowM[,c("ind_event","meanStElo")]

elo_scores_all_events_sim$individual <- as.factor(substr(elo_scores_all_events_sim$ind_event,
                                                         1, 4))

elo_scores_all_events_sim$eventSW <- as.factor(substr(elo_scores_all_events_sim$ind_event,
                                                      6,6))

elo_scores_all_events_sim <- elo_scores_all_events_sim[,c("individual",
                                                          "meanStElo",
                                                          "eventSW")]

names(elo_scores_all_events_sim) <- c("individual",
                                      "StElo",
                                      "eventSW")


# NOw is the same format as the original one. Save it!

write.csv(elo_scores_all_events_sim,
          "elo_scores_all_events_sim.csv",row.names=FALSE)


# # Estimating repeatability of dominance rank. I'm using MCMC method, see above for ANOVA and REML.
# 
# sink("../summaries/simple_repeatability_Lundy_sim.txt")
# 
# rpt.St.MCMC <- rpt(elo_scores_all_events_sim$StElo,
#                    elo_scores_all_events_sim$individual,
#                    datatype="Gaussian",
#                    method="MCMC",
#                    nboot=1000,
#                    npermut=1000)
# 
# rpt.St.MCMC
# 
# sink()
