# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 6th October, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to plot the results on dominance status and tarsus


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

#rank.TLandM.VB.fitness <- read.table("finaldatabases/rank.TLandM.VB.fitness.csv",header=TRUE,sep=",")
rank.TLandM.VB.fitness <- read.table("finaldatabases/rank.TLandM.VB.fitness.9int.csv",header=TRUE,sep=",")

VB.TLandM.age.fitness <- read.table("finaldatabases/VB.TLandM.age.fitness.csv",header=TRUE,sep=",")


# males subset (females and unknown excluded)

rank.TLandM.VB.fitness.2 <- rank.TLandM.VB.fitness[!(is.na(rank.TLandM.VB.fitness$sex)),]




################################################################
# WILD MODEL
################################################################

# subsetting only the necessary for the plotting of each model. 

data.plot1 <- rank.TLandM.VB.fitness.2[!(is.na(rank.TLandM.VB.fitness.2$age)) &
                                         rank.TLandM.VB.fitness.2$age>0 &
                                         !(is.na(rank.TLandM.VB.fitness.2$tarsus)) &
                                         !(is.na(rank.TLandM.VB.fitness.2$season)),]

data.plot1$sex <- as.factor(data.plot1$sex)
#data.plot1$season <- as.factor(data.plot1$season)


mod.rank.age <- lmer(StElo~age*
                       sex+
                       tarsus+
                       season+
                       (1|BirdID)+
                       (1|eventSW),
                     data=data.plot1)


#simulating a posterior distribution with 5000 draws
smod.rank.age<-sim(mod.rank.age,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus and age (from this database), and a mean fictious season of 0.5
# (remember that season was coded as 0: non-breeding, and 1: breeding)

newdat<-expand.grid(age=mean(data.plot1$age,na.rm = TRUE),
                    sex=levels(data.plot1$sex),
                    tarsus = seq(min(data.plot1$tarsus,na.rm = TRUE),
                                 max(data.plot1$tarsus,na.rm = TRUE),
                                 0.001),
                    season=0.5)


xmat<-model.matrix(~age*
                     sex+
                     tarsus+
                     season, 
                   data=newdat) 

fitmatboth <- matrix(NA, 
                     ncol = nrow(smod.rank.age@fixef),
                     nrow = nrow(newdat))


for(i in 1:nrow(smod.rank.age@fixef)) {
  fitmatboth[,i] <- xmat%*%smod.rank.age@fixef[i,]
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

data.plot1.m <- data.plot1[data.plot1$sex==1,]
data.plot1.f <- data.plot1[data.plot1$sex==0,]


# PLOT saved as .tiff

# tiff("plots/talks/Figure11_Status_and_tarsus_both_sexes.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)

tiff("plots/talks/9interactions/Figure11_Status_and_tarsus_both_sexes_9int.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)

#par(mar=c(5, 5, 1, 1))
par(mar=c(6, 7, 1, 1))


plot(data.plot1$tarsus, 
     data.plot1$StElo, 
     type="n",
     #      xlab="Bib length (mm)",
     #      ylab= "Standardized Elo-rating",
     xlab="",
     ylab="",
     #cex.lab=1.7,
     cex.lab=2.4,
     xaxt="n",yaxt="n",xlim=c(15.5,20.5),ylim=c(0,1),
     family="serif",
     frame.plot = FALSE)

title(xlab="tarsus length (mm)", line=4, cex.lab=3.0, family="serif")
title(ylab="Standardized Elo-rating", line=4.5, cex.lab=3.0, family="serif")

axis(1,at=seq(15.5,20.5,by=1),
     #cex.axis=1.3,
     cex.axis=1.8,
     family="serif")

axis(2,at=seq(0,1,by=0.2),
     las=2,
     #cex.axis=1.3,
     cex.axis=1.8,
     family="serif") 

points(data.plot1.m$tarsus, 
       data.plot1.m$StElo, 
       pch = 19, col=rgb(darkblue[1],darkblue[2],darkblue[3],0.4),
       cex = 2.0)

points(data.plot1.f$tarsus, 
       data.plot1.f$StElo, 
       pch = 19, col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.4),
       cex = 2.0)

index.m<-newdat$sex=="1" # only calls the plot but not the points yet

polygon(c(newdat$tarsus[index.m],rev(newdat$tarsus[index.m])),
        c(newdat$lower[index.m],rev(newdat$upper[index.m])),
        border=NA,col=rgb(darkblue[1],darkblue[2],darkblue[3], 0.15))

#index.f<-newdat$sex=="0"
#summary(data.plot1.f$tarsus)


newdat.f6 <- newdat[newdat$tarsus<19.89 & newdat$tarsus>16.31,]

index.f6<-newdat.f6$sex=="0"

polygon(c(newdat.f6$tarsus[index.f6],rev(newdat.f6$tarsus[index.f6])),
        c(newdat.f6$lower[index.f6],rev(newdat.f6$upper[index.f6])),
        border=NA,col=rgb(chocolate1[1], chocolate1[2], chocolate1[3], 0.15))

lines(newdat$tarsus[index.m], newdat$fit[index.m], lwd=3.5,
      col=rgb(darkblue[1],darkblue[2],darkblue[3],0.65)) 

lines(newdat$tarsus[index.m], newdat$lower[index.m], lty=2, lwd=2,
      col=rgb(darkblue[1],darkblue[2],darkblue[3],0.65))

lines(newdat$tarsus[index.m], newdat$upper[index.m], lty=2, lwd=2,
      col=rgb(darkblue[1],darkblue[2],darkblue[3],0.65))


lines(newdat.f6$tarsus[index.f6], newdat.f6$fit[index.f6], lwd=3.5,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8)) 

lines(newdat.f6$tarsus[index.f6], newdat.f6$lower[index.f6], lty=2, lwd=2,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8))

lines(newdat.f6$tarsus[index.f6], newdat.f6$upper[index.f6], lty=2, lwd=2,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8))

op <- par(family = "serif")
#par(op)

# legend(57,1.02,
#        legend=c("captive","wild"),
#        pch=19,
#        col=c(rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8),
#              rgb(darkblue[1],darkblue[2],darkblue[3],0.8)),
#        pt.cex=1.9,
#        cex=1.1)


dev.off()
