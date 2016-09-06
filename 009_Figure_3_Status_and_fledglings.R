# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 6th September, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to plot the results on dominance status and social and genetic
# fledglings


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

# males subset (females and unknown excluded)

rank.TLandM.VB.fitness.m <- rank.TLandM.VB.fitness[rank.TLandM.VB.fitness$sex==1 &
                                                     !(is.na(rank.TLandM.VB.fitness$sex)),]


################################################################
# SOCIAL FLEDGLINGS MODEL
################################################################

mod.soc.fledg.nobib <- lmer(soc.fledg.12d~
                              StElo+
                              age+
                              I(age^2)+
                              tarsus+
                              eventSW+
                              (1|BirdID),
                            data=rank.TLandM.VB.fitness.m)


# subsetting only the necessary for the plotting of each model. 

data.plot5 <- rank.TLandM.VB.fitness.m[!(is.na(rank.TLandM.VB.fitness.m$age)) &
                                         !(is.na(rank.TLandM.VB.fitness.m$tarsus)),]


#simulating a posterior distribution with 5000 draws

smod.soc.fledg.nobib<-sim(mod.soc.fledg.nobib,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of age and tarsus (from this database), and use the year 2014 as reference.

newdat.5<-expand.grid(StElo=seq(min(data.plot5$StElo,na.rm = TRUE),
                              max(data.plot5$StElo,na.rm = TRUE),
                              0.001), 
                      age = mean(data.plot5$age,na.rm = TRUE),
                      tarsus = mean(data.plot5$tarsus,na.rm = TRUE),
                      eventSW = 2014)


xmat.5<-model.matrix(~StElo+
                       age+
                       I(age^2)+
                       tarsus+
                       eventSW, 
                     data=newdat.5) 

fitmatboth.5 <- matrix(NA,
                       ncol = nrow(smod.soc.fledg.nobib@fixef),
                       nrow = nrow(newdat.5))


for(i in 1:nrow(smod.soc.fledg.nobib@fixef)) {
  fitmatboth.5[,i] <- xmat.5%*%smod.soc.fledg.nobib@fixef[i,]
}


# finally estimating the mean and the credible intervals for each
# value of dominance status. This is what I will plot later on.

newdat.5$fit<-apply(fitmatboth.5, 1, mean) 
newdat.5$lower<-apply(fitmatboth.5, 1, quantile, prob= 0.025)
newdat.5$upper<-apply(fitmatboth.5, 1, quantile, prob= 0.975)




################################################################
# GENETIC FLEDGLINGS MODEL
################################################################

mod.gen.fledg.nobib <- lmer(gen.fledg.12d~
                              StElo+
                              age+
                              I(age^2)+
                              tarsus+
                              eventSW+
                              (1|BirdID),
                            data=rank.TLandM.VB.fitness.m)


#simulating a posterior distribution with 5000 draws

smod.gen.fledg.nobib<-sim(mod.gen.fledg.nobib,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus (from this database)

newdat.6<-expand.grid(StElo=seq(min(data.plot5$StElo,na.rm = TRUE),
                                max(data.plot5$StElo,na.rm = TRUE),
                                0.001), 
                      age = mean(data.plot5$age,na.rm = TRUE),
                      tarsus = mean(data.plot5$tarsus,na.rm = TRUE),
                      eventSW = 2014)


xmat.6<-model.matrix(~StElo+
                       age+
                       I(age^2)+
                       tarsus+
                       eventSW, 
                     data=newdat.6) 

fitmatboth.6 <- matrix(NA,
                       ncol = nrow(smod.gen.fledg.nobib@fixef),
                       nrow = nrow(newdat.6))


for(i in 1:nrow(smod.gen.fledg.nobib@fixef)) {
  fitmatboth.6[,i] <- xmat.6%*%smod.gen.fledg.nobib@fixef[i,]
}


# finally estimating the mean and the credible intervals for each
# value of bib length. This is what I will plot later on.

newdat.6$fit<-apply(fitmatboth.6, 1, mean) 
newdat.6$lower<-apply(fitmatboth.6, 1, quantile, prob= 0.025)
newdat.6$upper<-apply(fitmatboth.6, 1, quantile, prob= 0.975)





################################################################
# PLOTTING
################################################################


# vector needed to obtain the final rgb colours

rgbing <- c(255,255,255)


# few colours in rb

turquoise <- c(49,163,84)/rgbing
blue <- c(44,127,184)/rgbing


# PLOT saved as .tiff

tiff("plots/Figure3_Status_and_fledglings.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)

par(mar=c(5, 5, 1, 1))

plot(data.plot5$StElo, 
     data.plot5$soc.fledg.12d, 
     type="n",
     xlab="Standardized Elo-rating",
     ylab= "Annual number of fledglings",
     cex.lab=1.7,
     xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,12),
     family="serif",
     frame.plot = FALSE)


axis(1,at=seq(0,1,by=0.2),
     las=1,
     cex.axis=1.3,
     family="serif") 

axis(2,at=seq(0,12,by=1),
     cex.axis=1.3,
     las=2,
     family="serif")


points(data.plot5$StElo, 
       jitter(data.plot5$soc.fledg.12d,0.65), 
       pch = 19, col=rgb(turquoise[1], turquoise[2], turquoise[3],0.4),
       cex = 2.0)

points(data.plot5$StElo, 
       jitter(data.plot5$gen.fledg.12d,0.65),
       pch = 19, col=rgb(blue[1],blue[2],blue[3],0.4),       
       cex = 2.0)

polygon(c(newdat.5$StElo,rev(newdat.5$StElo)),
        c(newdat.5$lower,rev(newdat.5$upper)),
        border=NA,col=rgb(turquoise[1], turquoise[2], turquoise[3], 0.15))

polygon(c(newdat.6$StElo,rev(newdat.6$StElo)),
        c(newdat.6$lower,rev(newdat.6$upper)),
        border=NA,col=rgb(blue[1],blue[2],blue[3], 0.15))

lines(newdat.5$StElo, newdat.5$fit, lwd=3.5,
      col=rgb(turquoise[1], turquoise[2], turquoise[3],0.8))      

lines(newdat.5$StElo, newdat.5$lower, lty=2, lwd=2,
      col=rgb(turquoise[1], turquoise[2], turquoise[3],0.65))

lines(newdat.5$StElo, newdat.5$upper, lty=2, lwd=2,
      col=rgb(turquoise[1], turquoise[2], turquoise[3],0.65))

lines(newdat.6$StElo, newdat.6$fit, lwd=3.5,
      col=rgb(blue[1],blue[2],blue[3],0.8))

lines(newdat.6$StElo, newdat.6$lower, lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

lines(newdat.6$StElo, newdat.6$upper, lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

op <- par(family = "serif")
#par(op)

legend(0.8,10.2,
       legend=c("social","genetic"),
       pch=19,
       col=c(rgb(turquoise[1], turquoise[2], turquoise[3],0.8),
             rgb(blue[1],blue[2],blue[3],0.8)),
       pt.cex=1.9,
       cex=1.1)


dev.off()
