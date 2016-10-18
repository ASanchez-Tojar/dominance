# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 10th October, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to plot the results on survival of dominance status


########################################################################################################
# Packages needed
########################################################################################################

# packages needed to be loaded for this script

library(lme4)
library(arm)
library(blmeco)


# Clear memory and get to know where you are
rm(list=ls())
#getwd()


# loading the clean databases from Lundy with all the data needed

survival <- read.table("survival/forsurv_jj.csv",header=TRUE,sep=",")



# Subsetting the database to only those that interacted more than 8 times per event

morethan8pereventSW <- read.table("morethan8pereventSW.csv",header=TRUE,sep=",")

intpereventSW <- morethan8pereventSW[,c("indevent","freqppereventSW")]

names(intpereventSW)<-c("ind_eventSW","freqppereventSW")

survival$ind_eventSW <- factor(paste(survival$individual,
                                     survival$eventSW,sep="_"))

survival.9int <- merge(survival,intpereventSW,
                       by="ind_eventSW",all.y=TRUE)

survival <- survival.9int[!(is.na(survival.9int$BirdID)),]


################################################################
# MODEL
################################################################

survival$SexEstimate <- as.factor(survival$SexEstimate)

survive <- glmer(Alive ~ 
                   StElo*
                   SexEstimate+
                   (1|eventSW),
                 data=survival,
                 family = binomial) 

#simulating a posterior distribution with 5000 draws
ssurvive<-sim(survive,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus and age (from this database), and a mean fictious season of 0.5
# (remember that season was coded as 0: non-breeding, and 1: breeding)

newdat<-expand.grid(StElo=seq(min(survival$StElo,na.rm = TRUE),
                              max(survival$StElo,na.rm = TRUE),
                              0.001),
                    SexEstimate=levels(survival$SexEstimate))


xmat<-model.matrix(~StElo*
                     SexEstimate, 
                   data=newdat) 

fitmatboth <- matrix(NA, 
                     ncol = nrow(ssurvive@fixef),
                     nrow = nrow(newdat))


for(i in 1:nrow(ssurvive@fixef)) {
  fitmatboth[,i] <- plogis(xmat%*%ssurvive@fixef[i,])
}


# finally estimating the mean and the credible intervals for each
# value of bib length. This is what I will plot later on.

newdat$fit<-apply(fitmatboth, 1, mean) # 1= row , 2 = colum
newdat$lower<-apply(fitmatboth, 1, quantile, prob= 0.025)
newdat$upper<-apply(fitmatboth, 1, quantile, prob= 0.975)



################################################################
# PLOTTING
################################################################


# vector needed to obtain the final rgb colours

rgbing <- c(255,255,255)


# few colours in rb

darkblue <- c(31,120,180)/rgbing
chocolate1 <- c(255,127,36)/rgbing

survival.m <- survival[survival$SexEstimate==1,]
survival.f <- survival[survival$SexEstimate==0,]


# PLOT saved as .tiff

# tiff("plots/talks/Figure13_Status_and_Survival.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)

tiff("plots/talks/9interactions/Figure13_Status_and_Survival_9int.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)

#par(mar=c(5, 5, 1, 1))
par(mar=c(6, 7, 1, 1))


plot(survival$StElo, 
     survival$Alive,
     type="n",
     #      xlab="Bib length (mm)",
     #      ylab= "Standardized Elo-rating",
     xlab="",
     ylab="",
     #cex.lab=1.7,
     cex.lab=2.4,
     xaxt="n",yaxt="n",
     xlim=c(0,1),ylim=c(0,1.1),
     family="serif",
     frame.plot = FALSE)

title(xlab="Standardized Elo-rating", line=4, cex.lab=3.0, family="serif")
title(ylab="survival", line=4.5, cex.lab=3.0, family="serif")


axis(1,at=seq(0,1,by=0.2),
     las=1,
     cex.axis=1.8,
     family="serif") 

axis(2,at=seq(0,1,by=0.25),
     cex.axis=1.8,
     las=2,
     family="serif")

# points(rep(1.3,each=length(data.plot1.m$StElo)), 
#        data.plot1.m$StElo, 
#        pch = 19, col=rgb(darkblue[1],darkblue[2],darkblue[3],0.02),
#        cex = 1.2)
# 
# points(rep(2,each=length(data.plot1.f$StElo)), 
#        data.plot1.f$StElo, 
#        pch = 19, col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.02),
#        cex = 1.2)

points(survival.m$StElo, 
       survival.m$Alive, 
       pch = 19, col=rgb(darkblue[1],darkblue[2],darkblue[3],0.4),
       cex = 2.0)

points(survival.f$StElo, 
       survival.f$Alive+0.03, 
       pch = 19, col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.4),
       cex = 2.0)

index.m<-newdat$SexEstimate=="1"
index.f<-newdat$SexEstimate=="0"


polygon(c(newdat$StElo[index.m],rev(newdat$StElo[index.m])),
        c(newdat$lower[index.m],rev(newdat$upper[index.m])),
        border=NA,col=rgb(darkblue[1],darkblue[2],darkblue[3], 0.15))

polygon(c(newdat$StElo[index.f],rev(newdat$StElo[index.f])),
        c(newdat$lower[index.f],rev(newdat$upper[index.f])),
        border=NA,col=rgb(chocolate1[1], chocolate1[2], chocolate1[3], 0.15))

lines(newdat$StElo[index.m], newdat$fit[index.m], lwd=3.5,
      col=rgb(darkblue[1],darkblue[2],darkblue[3],0.65)) 

lines(newdat$StElo[index.m], newdat$lower[index.m], lty=2, lwd=2,
      col=rgb(darkblue[1],darkblue[2],darkblue[3],0.65))

lines(newdat$StElo[index.m], newdat$upper[index.m], lty=2, lwd=2,
      col=rgb(darkblue[1],darkblue[2],darkblue[3],0.65))


lines(newdat$StElo[index.f], newdat$fit[index.f], lwd=3.5,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8)) 

lines(newdat$StElo[index.f], newdat$lower[index.f], lty=2, lwd=2,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8))

lines(newdat$StElo[index.f], newdat$upper[index.f], lty=2, lwd=2,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8))



dev.off()
