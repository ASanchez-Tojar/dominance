# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 6th October, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to plot the results on dominance status and social and genetic
# recruits in both sexes


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

rank.TLandM.VB.fitness <- read.table("finaldatabases/rank.TLandM.VB.fitness.csv",header=TRUE,sep=",")
#rank.TLandM.VB.fitness <- read.table("finaldatabases/rank.TLandM.VB.fitness.9int.csv",header=TRUE,sep=",")
#rank.TLandM.VB.fitness <- read.table("finaldatabases/rank.TLandM.VB.fitness_sim.csv",header=TRUE,sep=",")
#rank.TLandM.VB.fitness <- read.table("finaldatabases/rank.TLandM.VB.fitness.9int_sim.csv",header=TRUE,sep=",")

# subset (unknown excluded)

rank.TLandM.VB.fitness.2 <- rank.TLandM.VB.fitness[!(is.na(rank.TLandM.VB.fitness$sex)),]


# Males

rank.TLandM.VB.fitness.m <- rank.TLandM.VB.fitness.2[rank.TLandM.VB.fitness.2$sex==1,]


# Females

rank.TLandM.VB.fitness.f <- rank.TLandM.VB.fitness.2[rank.TLandM.VB.fitness.2$sex==0,]


################################################################
# MALES
################################################################

################################################################
# GENETIC RECRUITS MODEL
################################################################

rank.TLandM.VB.fitness.m.2 <- rank.TLandM.VB.fitness.m[!(is.na(rank.TLandM.VB.fitness.m$gen.recruits)),]

rank.TLandM.VB.fitness.m.2$eventSW <- as.factor(rank.TLandM.VB.fitness.m.2$eventSW)

mod.gen.recruits.m <- glmer(gen.recruits~
                              StElo+
                              age+
                              I(age^2)+
                              tarsus+
                              eventSW+
                              (1|BirdID),
                            data=rank.TLandM.VB.fitness.m.2,
                            family=poisson)


#simulating a posterior distribution with 5000 draws

smod.gen.recruits.m<-sim(mod.gen.recruits.m,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus (from this database)

newdat.m<-expand.grid(StElo=seq(min(rank.TLandM.VB.fitness.m.2$StElo,na.rm = TRUE),
                                 max(rank.TLandM.VB.fitness.m.2$StElo,na.rm = TRUE),
                                 0.001), 
                       age = mean(rank.TLandM.VB.fitness.m.2$age,na.rm = TRUE),
                       tarsus = mean(rank.TLandM.VB.fitness.m.2$tarsus,na.rm = TRUE),
                       eventSW = levels(rank.TLandM.VB.fitness.m.2$eventSW))


xmat.m<-model.matrix(~StElo+
                       age+
                       I(age^2)+
                       tarsus+
                       eventSW, 
                     data=newdat.m) 

fitmatboth.m <- matrix(NA,
                        ncol = nrow(smod.gen.recruits.m@fixef),
                        nrow = nrow(newdat.m))


for(i in 1:nrow(smod.gen.recruits.m@fixef)) {
  fitmatboth.m[,i] <- exp(xmat.m%*%smod.gen.recruits.m@fixef[i,])
}


# finally estimating the mean and the credible intervals for each
# value of bib length. This is what I will plot later on.

newdat.m$fit<-apply(fitmatboth.m, 1, mean) 
newdat.m$lower<-apply(fitmatboth.m, 1, quantile, prob= 0.025)
newdat.m$upper<-apply(fitmatboth.m, 1, quantile, prob= 0.975)



################################################################
# FEMALES
################################################################

################################################################
# GENETIC RECRUITS MODEL
################################################################

rank.TLandM.VB.fitness.f.2 <- rank.TLandM.VB.fitness.f[!(is.na(rank.TLandM.VB.fitness.f$gen.recruits)),]

rank.TLandM.VB.fitness.f.2$eventSW <- as.factor(rank.TLandM.VB.fitness.f.2$eventSW)

mod.gen.recruits.f <- glmer(gen.recruits~
                              StElo+
                              age+
                              I(age^2)+
                              tarsus+
                              eventSW+
                              (1|BirdID),
                            data=rank.TLandM.VB.fitness.f.2,
                            family=poisson)


#simulating a posterior distribution with 5000 draws

smod.gen.recruits.f<-sim(mod.gen.recruits.f,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus (from this database)

newdat.f<-expand.grid(StElo=seq(min(rank.TLandM.VB.fitness.f.2$StElo,na.rm = TRUE),
                                max(rank.TLandM.VB.fitness.f.2$StElo,na.rm = TRUE),
                                0.001), 
                      age = mean(rank.TLandM.VB.fitness.f.2$age,na.rm = TRUE),
                      tarsus = mean(rank.TLandM.VB.fitness.f.2$tarsus,na.rm = TRUE),
                      eventSW = levels(rank.TLandM.VB.fitness.f.2$eventSW))


xmat.f<-model.matrix(~StElo+
                       age+
                       I(age^2)+
                       tarsus+
                       eventSW, 
                     data=newdat.f) 

fitmatboth.f <- matrix(NA,
                       ncol = nrow(smod.gen.recruits.f@fixef),
                       nrow = nrow(newdat.f))


for(i in 1:nrow(smod.gen.recruits.f@fixef)) {
  fitmatboth.f[,i] <- exp(xmat.f%*%smod.gen.recruits.f@fixef[i,])
}


# finally estimating the mean and the credible intervals for each
# value of bib length. This is what I will plot later on.

newdat.f$fit<-apply(fitmatboth.f, 1, mean) 
newdat.f$lower<-apply(fitmatboth.f, 1, quantile, prob= 0.025)
newdat.f$upper<-apply(fitmatboth.f, 1, quantile, prob= 0.975)




################################################################
# PLOTTING
################################################################


# vector needed to obtain the final rgb colours

rgbing <- c(255,255,255)


# few colours in rb

darkblue <- c(31,120,180)/rgbing
chocolate1 <- c(255,127,36)/rgbing


# PLOT saved as .tiff

# tiff("plots/talks/Figure9_Status_and_recruits_both_sexes_2015.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)

# tiff("plots/talks/9interactions/Figure9_Status_and_recruits_both_sexes_2014_9int.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)

tiff("plots/talks/Figure9_Status_and_recruits_both_sexes_2014_sim_upd.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)

# tiff("plots/talks/9interactions/Figure9_Status_and_recruits_both_sexes_2015_9int_sim.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)


#par(mar=c(5, 5, 1, 1))
par(mar=c(6, 7, 1, 1))

plot(rank.TLandM.VB.fitness.m.2$StElo, 
     rank.TLandM.VB.fitness.m.2$gen.recruits, 
     type="n",
     #xlab="Standardized Elo-rating",
     #ylab= "Annual number of fledglings",
     xlab="",
     ylab="",
     cex.lab=1.7,
     xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,4),
     family="serif",
     frame.plot = FALSE)

title(xlab="Standardized Elo-rating", line=4, cex.lab=3.2, family="serif")
title(ylab="genetic recruits", line=4, cex.lab=3.2, family="serif")


axis(1,at=seq(0,1,by=0.2),
     las=1,
     #cex.axis=1.3,
     cex.axis=1.8,
     family="serif") 

axis(2,at=seq(0,4,by=1),
     #cex.axis=1.3,
     cex.axis=1.8,
     las=2,
     family="serif")


points(rank.TLandM.VB.fitness.m.2$StElo, 
       jitter(rank.TLandM.VB.fitness.m.2$gen.recruits,0.65), 
       pch = 19, col=rgb(darkblue[1], darkblue[2], darkblue[3],0.4),
       cex = 2.0)

points(rank.TLandM.VB.fitness.f.2$StElo, 
       jitter(rank.TLandM.VB.fitness.f.2$gen.recruits,0.65),
       pch = 19, col=rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.4),       
       cex = 2.0)

index.1<-newdat.m$eventSW=="2014" # only calls the plot but not the points yet

polygon(c(newdat.m$StElo[index.1],rev(newdat.m$StElo[index.1])),
        c(newdat.m$lower[index.1],rev(newdat.m$upper[index.1])),
        border=NA,col=rgb(darkblue[1], darkblue[2], darkblue[3], 0.15))

index.2<-newdat.f$eventSW=="2015" # only calls the plot but not the points yet

polygon(c(newdat.f$StElo[index.2],rev(newdat.f$StElo[index.2])),
        c(newdat.f$lower[index.2],rev(newdat.f$upper[index.2])),
        border=NA,col=rgb(chocolate1[1],chocolate1[2],chocolate1[3], 0.15))

lines(newdat.m$StElo[index.1], newdat.m$fit[index.1], lwd=3.5,
      col=rgb(darkblue[1], darkblue[2], darkblue[3],0.8))      

lines(newdat.m$StElo[index.1], newdat.m$lower[index.1], lty=2, lwd=2,
      col=rgb(darkblue[1], darkblue[2], darkblue[3],0.65))

lines(newdat.m$StElo[index.1], newdat.m$upper[index.1], lty=2, lwd=2,
      col=rgb(darkblue[1], darkblue[2], darkblue[3],0.65))

lines(newdat.f$StElo[index.2], newdat.f$fit[index.2], lwd=3.5,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8))      

lines(newdat.f$StElo[index.2], newdat.f$lower[index.2], lty=2, lwd=2,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.65))

lines(newdat.f$StElo[index.2], newdat.f$upper[index.2], lty=2, lwd=2,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.65))

# op <- par(family = "serif")
# #par(op)
# 
# legend(0,12,
#        legend=c("social","genetic"),
#        pch=19,
#        col=c(rgb(darkblue[1], darkblue[2], darkblue[3],0.8),
#              rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.8)),
#        pt.cex=1.9,
#        cex=1.1)


dev.off()
