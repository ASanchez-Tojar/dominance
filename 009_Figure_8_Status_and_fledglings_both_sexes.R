# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 5th October, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to plot the results on dominance status and social and genetic
# fledglings for females and males


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
# GENETIC FLEDGLINGS MODEL
################################################################

data.plot6 <- rank.TLandM.VB.fitness.m[!(is.na(rank.TLandM.VB.fitness.m$age)) &
                                         !(is.na(rank.TLandM.VB.fitness.m$gen.fledg.12d)) &
                                         !(is.na(rank.TLandM.VB.fitness.m$tarsus)),]

data.plot6$eventSW <- as.factor(data.plot6$eventSW)

mod.gen.fledg.m <- lmer(gen.fledg.12d~
                              StElo+
                              age+
                              I(age^2)+
                              tarsus+
                              eventSW+
                              (1|BirdID),
                            data=data.plot6)


#simulating a posterior distribution with 5000 draws

smod.gen.fledg.m<-sim(mod.gen.fledg.m,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus (from this database)

newdat.6<-expand.grid(StElo=seq(min(data.plot6$StElo,na.rm = TRUE),
                                max(data.plot6$StElo,na.rm = TRUE),
                                1),
                                #0.001), 
                      age = mean(data.plot6$age,na.rm = TRUE),
                      tarsus = mean(data.plot6$tarsus,na.rm = TRUE),
                      eventSW = levels(data.plot6$eventSW))


xmat.6<-model.matrix(~StElo+
                       age+
                       I(age^2)+
                       tarsus+
                       eventSW, 
                     data=newdat.6) 

fitmatboth.6 <- matrix(NA,
                       ncol = nrow(smod.gen.fledg.m@fixef),
                       nrow = nrow(newdat.6))


for(i in 1:nrow(smod.gen.fledg.m@fixef)) {
  fitmatboth.6[,i] <- xmat.6%*%smod.gen.fledg.m@fixef[i,]
}


# finally estimating the mean and the credible intervals for each
# value of bib length. This is what I will plot later on.

newdat.6$fit<-apply(fitmatboth.6, 1, mean) 
newdat.6$lower<-apply(fitmatboth.6, 1, quantile, prob= 0.025)
newdat.6$upper<-apply(fitmatboth.6, 1, quantile, prob= 0.975)




################################################################
# FEMALES
################################################################

################################################################
# GENETIC FLEDGLINGS MODEL
################################################################

data.plotf <- rank.TLandM.VB.fitness.f[!(is.na(rank.TLandM.VB.fitness.f$age)) &
                                         !(is.na(rank.TLandM.VB.fitness.f$gen.fledg.12d)) &
                                         !(is.na(rank.TLandM.VB.fitness.f$tarsus)),]

data.plotf$eventSW <- as.factor(data.plotf$eventSW)


mod.gen.fledg.f <- lmer(gen.fledg.12d~
                          StElo+
                          age+
                          I(age^2)+
                          tarsus+
                          eventSW+
                          (1|BirdID),
                        data=data.plotf)


#simulating a posterior distribution with 5000 draws

smod.gen.fledg.f<-sim(mod.gen.fledg.f,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus (from this database)

newdat.f<-expand.grid(StElo=seq(min(data.plotf$StElo,na.rm = TRUE),
                                max(data.plotf$StElo,na.rm = TRUE),
                                1), 
                                #0.001), 
                      age = mean(data.plotf$age,na.rm = TRUE),
                      tarsus = mean(data.plotf$tarsus,na.rm = TRUE),
                      eventSW = levels(data.plotf$eventSW))


xmat.f<-model.matrix(~StElo+
                       age+
                       I(age^2)+
                       tarsus+
                       eventSW, 
                     data=newdat.f) 

fitmatboth.f <- matrix(NA,
                       ncol = nrow(smod.gen.fledg.f@fixef),
                       nrow = nrow(newdat.f))


for(i in 1:nrow(smod.gen.fledg.f@fixef)) {
  fitmatboth.f[,i] <- xmat.f%*%smod.gen.fledg.f@fixef[i,]
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

# tiff("plots/talks/Figure8_Status_and_fledglings_both_sexes_2015.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)

# tiff("plots/talks/9interactions/Figure8_Status_and_fledglings_both_sexes_2015_9int.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)

# tiff("plots/talks/Figure9_Status_and_fledglings_both_sexes_2014_upd.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)

# tiff("plots/talks/9interactions/Figure9_Status_and_fledglings_both_sexes_2015_9int_sim.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)

tiff("plots/talks/Figure8_Status_and_fledglings_both_sexes_2014_notStElo.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)


#par(mar=c(5, 5, 1, 1))
par(mar=c(6, 7, 1, 1))

plot(data.plot6$StElo, 
     data.plot6$gen.fledg.12d, 
     type="n",
     #xlab="Standardized Elo-rating",
     #ylab= "Annual number of fledglings",
     xlab="",
     ylab="",
     cex.lab=1.7,
     xaxt="n",yaxt="n",
     #xlim=c(0,1),
     xlim=c(-500,900),
     ylim=c(0,12),
     family="serif",
     frame.plot = FALSE)

title(xlab="randomized Elo-rating", line=4, cex.lab=3.2, family="serif")
#title(xlab="Standardized Elo-rating", line=4, cex.lab=3.2, family="serif")
title(ylab="genetic fledglings", line=4, cex.lab=3.2, family="serif")


axis(#1,at=seq(0,1,by=0.2),
     1,at=seq(-500,900,by=200),
     las=1,
     #cex.axis=1.3,
     cex.axis=1.8,
     family="serif") 

axis(2,at=seq(0,12,by=1),
     #cex.axis=1.3,
     cex.axis=1.8,
     las=2,
     family="serif")


points(data.plot6$StElo, 
       jitter(data.plot6$gen.fledg.12d,0.65), 
       pch = 19, col=rgb(darkblue[1], darkblue[2], darkblue[3],0.4),
       cex = 1.5)
       #cex = 2.0)

points(data.plotf$StElo, 
       jitter(data.plotf$gen.fledg.12d,0.65),
       pch = 19, col=rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.4),
       cex = 1.5)
       #cex = 2.0)

index.1<-newdat.6$eventSW=="2014" # only calls the plot but not the points yet

polygon(c(newdat.6$StElo[index.1],rev(newdat.6$StElo[index.1])),
        c(newdat.6$lower[index.1],rev(newdat.6$upper[index.1])),
        border=NA,col=rgb(darkblue[1], darkblue[2], darkblue[3], 0.15))

index.2<-newdat.f$eventSW=="2014" # only calls the plot but not the points yet

polygon(c(newdat.f$StElo[index.2],rev(newdat.f$StElo[index.2])),
        c(newdat.f$lower[index.2],rev(newdat.f$upper[index.2])),
        border=NA,col=rgb(chocolate1[1],chocolate1[2],chocolate1[3], 0.15))

lines(newdat.6$StElo[index.1], newdat.6$fit[index.1], lwd=3.5,
      col=rgb(darkblue[1], darkblue[2], darkblue[3],0.8))      

lines(newdat.6$StElo[index.1], newdat.6$lower[index.1], lty=2, lwd=2,
      col=rgb(darkblue[1], darkblue[2], darkblue[3],0.65))

lines(newdat.6$StElo[index.1], newdat.6$upper[index.1], lty=2, lwd=2,
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
