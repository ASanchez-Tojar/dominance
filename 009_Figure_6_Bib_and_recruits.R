# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 16th September, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to plot the results on bib length and social and genetic
# recruits


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

VB.TLandM.age.fitness <- read.table("finaldatabases/VB.TLandM.age.fitness.csv",header=TRUE,sep=",")

VB.TLandM.age.fitness.2 <- VB.TLandM.age.fitness[VB.TLandM.age.fitness$soc.fledg.12d<13 &
                                                   !(is.na(VB.TLandM.age.fitness$soc.fledg.12d)),]


################################################################
# SOCIAL RECRUITS MODEL
################################################################

VB.TLandM.age.fitness.6 <- VB.TLandM.age.fitness[!(is.na(VB.TLandM.age.fitness$soc.recruits)),]

VB.TLandM.age.fitness.6$eventSW <- as.factor(VB.TLandM.age.fitness.6$eventSW)
VB.TLandM.age.fitness.6$bib.s <- scale(VB.TLandM.age.fitness.6$bib)

mod.soc.recruits.bib <- glmer(soc.recruits~bib.s+
                                age+
                                I(age^2)+
                                tarsus+
                                eventSW+
                                (1|BirdID),
                              data=VB.TLandM.age.fitness.6,
                              family=poisson)


#simulating a posterior distribution with 5000 draws

smod.soc.recruits.bib<-sim(mod.soc.recruits.bib,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of age and tarsus (from this database), and use the year 2014 as reference.

newdat.11<-expand.grid(bib=seq(min(VB.TLandM.age.fitness.6$bib,na.rm = TRUE),
                                max(VB.TLandM.age.fitness.6$bib,na.rm = TRUE),
                                0.01), 
                      age = mean(VB.TLandM.age.fitness.6$age,na.rm = TRUE),
                      tarsus = mean(VB.TLandM.age.fitness.6$tarsus,na.rm = TRUE),
                      eventSW = levels(VB.TLandM.age.fitness.6$eventSW))

newdat.11$bib.s <- scale(newdat.11$bib)


xmat.11<-model.matrix(~bib.s+
                       age+
                       I(age^2)+
                       tarsus+
                       eventSW, 
                     data=newdat.11) 

fitmatboth.11 <- matrix(NA,
                       ncol = nrow(smod.soc.recruits.bib@fixef),
                       nrow = nrow(newdat.11))


for(i in 1:nrow(smod.soc.recruits.bib@fixef)) {
  fitmatboth.11[,i] <- exp(xmat.11%*%smod.soc.recruits.bib@fixef[i,]) #exp because of log link
}


# finally estimating the mean and the credible intervals for each
# value of dominance status. This is what I will plot later on.

newdat.11$fit<-apply(fitmatboth.11, 1, mean) 
newdat.11$lower<-apply(fitmatboth.11, 1, quantile, prob= 0.025)
newdat.11$upper<-apply(fitmatboth.11, 1, quantile, prob= 0.975)


################################################################
# GENETIC RECRUITS MODEL
################################################################

VB.TLandM.age.fitness.7 <- VB.TLandM.age.fitness[!(is.na(VB.TLandM.age.fitness$gen.recruits)),]

VB.TLandM.age.fitness.7$eventSW <- as.factor(VB.TLandM.age.fitness.7$eventSW)
VB.TLandM.age.fitness.7$bib.s <- scale(VB.TLandM.age.fitness.7$bib)

mod.gen.recruits.bib <- glmer(gen.recruits~bib.s+
                                age+
                                I(age^2)+
                                tarsus+
                                eventSW+
                                (1|BirdID),
                              data=VB.TLandM.age.fitness.7,
                              family=poisson)


#simulating a posterior distribution with 5000 draws

smod.gen.recruits.bib<-sim(mod.gen.recruits.bib,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus (from this database)

newdat.12<-expand.grid(bib=seq(min(VB.TLandM.age.fitness.7$bib,na.rm = TRUE),
                                 max(VB.TLandM.age.fitness.7$bib,na.rm = TRUE),
                                 0.01), 
                       age = mean(VB.TLandM.age.fitness.7$age,na.rm = TRUE),
                       tarsus = mean(VB.TLandM.age.fitness.7$tarsus,na.rm = TRUE),
                       eventSW = levels(VB.TLandM.age.fitness.7$eventSW))

newdat.12$bib.s <- scale(newdat.12$bib)


xmat.12<-model.matrix(~bib.s+
                        age+
                        I(age^2)+
                        tarsus+
                        eventSW, 
                      data=newdat.12) 

fitmatboth.12 <- matrix(NA,
                        ncol = nrow(smod.gen.recruits.bib@fixef),
                        nrow = nrow(newdat.12))


for(i in 1:nrow(smod.gen.recruits.bib@fixef)) {
  fitmatboth.12[,i] <- exp(xmat.12%*%smod.gen.recruits.bib@fixef[i,])
}


# finally estimating the mean and the credible intervals for each
# value of bib length. This is what I will plot later on.

newdat.12$fit<-apply(fitmatboth.12, 1, mean) 
newdat.12$lower<-apply(fitmatboth.12, 1, quantile, prob= 0.025)
newdat.12$upper<-apply(fitmatboth.12, 1, quantile, prob= 0.975)


################################################################
# PLOTTING
################################################################


# vector needed to obtain the final rgb colours

rgbing <- c(255,255,255)


# few colours in rb

turquoise <- c(49,163,84)/rgbing
blue <- c(44,127,184)/rgbing


# PLOT saved as .tiff

tiff("plots/Figure6_Bib_and_recruits.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)

par(mar=c(5, 5, 1, 1))

plot(VB.TLandM.age.fitness.6$bib, 
     VB.TLandM.age.fitness.6$soc.recruits, 
     type="n",
     xlab="Bib length (mm)",
     ylab= "Annual number of recruits",
     cex.lab=1.7,
     xaxt="n",yaxt="n",xlim=c(44,62),ylim=c(0,4),
     family="serif",
     frame.plot = FALSE)


axis(1,at=seq(44,62,by=2),
     las=1,
     cex.axis=1.3,
     family="serif") 

axis(2,at=seq(0,4,by=1),
     cex.axis=1.3,
     las=2,
     family="serif")


points(VB.TLandM.age.fitness.6$bib, 
       jitter(VB.TLandM.age.fitness.6$soc.recruits,0.50), 
       pch = 19, col=rgb(turquoise[1], turquoise[2], turquoise[3],0.4),
       cex = 2.0)

points(VB.TLandM.age.fitness.7$bib, 
       jitter(VB.TLandM.age.fitness.7$gen.recruits,0.50),
       pch = 19, col=rgb(blue[1],blue[2],blue[3],0.4),       
       cex = 2.0)

index.1<-newdat.11$eventSW=="2014" # only calls the plot but not the points yet

polygon(c(newdat.11$bib[index.1],rev(newdat.11$bib[index.1])),
        c(newdat.11$lower[index.1],rev(newdat.11$upper[index.1])),
        border=NA,col=rgb(turquoise[1], turquoise[2], turquoise[3], 0.15))

index.2<-newdat.12$eventSW=="2014" # only calls the plot but not the points yet

polygon(c(newdat.12$bib[index.2],rev(newdat.12$bib[index.2])),
        c(newdat.12$lower[index.2],rev(newdat.12$upper[index.2])),
        border=NA,col=rgb(blue[1],blue[2],blue[3], 0.15))

lines(newdat.11$bib[index.1], newdat.11$fit[index.1], lwd=3.5,
      col=rgb(turquoise[1], turquoise[2], turquoise[3],0.8))      

lines(newdat.11$bib[index.1], newdat.11$lower[index.1], lty=2, lwd=2,
      col=rgb(turquoise[1], turquoise[2], turquoise[3],0.65))

lines(newdat.11$bib[index.1], newdat.11$upper[index.1], lty=2, lwd=2,
      col=rgb(turquoise[1], turquoise[2], turquoise[3],0.65))

lines(newdat.12$bib[index.2], newdat.12$fit[index.2], lwd=3.5,
      col=rgb(blue[1],blue[2],blue[3],0.8))

lines(newdat.12$bib[index.2], newdat.12$lower[index.2], lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

lines(newdat.12$bib[index.2], newdat.12$upper[index.2], lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

op <- par(family = "serif")
#par(op)

legend(44,4,
       legend=c("social","genetic"),
       pch=19,
       col=c(rgb(turquoise[1], turquoise[2], turquoise[3],0.8),
             rgb(blue[1],blue[2],blue[3],0.8)),
       pt.cex=1.9,
       cex=1.1)


dev.off()



################################################################
# PLOTTING ONLY GENETIC
################################################################


# vector needed to obtain the final rgb colours

rgbing <- c(255,255,255)


# few colours in rb

turquoise <- c(49,163,84)/rgbing
blue <- c(44,127,184)/rgbing


# PLOT saved as .tiff

# tiff("plots/Figure6_Bib_and_geneticrecruits.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)

tiff("plots/talks/Figure6_Bib_and_geneticrecruits_talk.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)

#par(mar=c(5, 5, 1, 1))
par(mar=c(6, 7, 1, 1))

plot(VB.TLandM.age.fitness.7$bib, 
     VB.TLandM.age.fitness.7$gen.recruits, 
     type="n",
#      xlab="Bib length (mm)",
#      ylab= "Annual number of recruits",
     xlab="",
     ylab="",
     cex.lab=1.7,
     xaxt="n",yaxt="n",xlim=c(44,62),ylim=c(0,4),
     family="serif",
     frame.plot = FALSE)

#title(xlab="Standardized Elo-rating", line=4, cex.lab=2.5, family="serif")
title(ylab="recruits", line=4, cex.lab=3.2, family="serif")


axis(1,at=seq(44,62,by=2),
     las=1,
     cex.axis=1.3,
     family="serif") 

axis(2,at=seq(0,4,by=1),
     cex.axis=1.3,
     las=2,
     family="serif")

points(VB.TLandM.age.fitness.7$bib, 
       jitter(VB.TLandM.age.fitness.7$gen.recruits,0.50),
       pch = 19, col=rgb(blue[1],blue[2],blue[3],0.4),       
       cex = 2.0)

index.2<-newdat.12$eventSW=="2014" # only calls the plot but not the points yet

polygon(c(newdat.12$bib[index.2],rev(newdat.12$bib[index.2])),
        c(newdat.12$lower[index.2],rev(newdat.12$upper[index.2])),
        border=NA,col=rgb(blue[1],blue[2],blue[3], 0.15))

lines(newdat.12$bib[index.2], newdat.12$fit[index.2], lwd=3.5,
      col=rgb(blue[1],blue[2],blue[3],0.8))

lines(newdat.12$bib[index.2], newdat.12$lower[index.2], lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

lines(newdat.12$bib[index.2], newdat.12$upper[index.2], lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

dev.off()
