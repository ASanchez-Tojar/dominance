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

#rank.TLandM.VB.fitness <- read.table("finaldatabases/rank.TLandM.VB.fitness.csv",header=TRUE,sep=",")
rank.TLandM.VB.fitness <- read.table("finaldatabases/rank.TLandM.VB.fitness.sex.csv",header=TRUE,sep=",")

# males subset (females and unknown excluded)

rank.TLandM.VB.fitness.m <- rank.TLandM.VB.fitness[rank.TLandM.VB.fitness$sex==1 &
                                                     !(is.na(rank.TLandM.VB.fitness$sex)),]


################################################################
# SOCIAL FLEDGLINGS MODEL
################################################################

rank.TLandM.VB.fitness.m$eventSW <- as.factor(rank.TLandM.VB.fitness.m$eventSW)

mod.soc.fledg.nobib <- lmer(soc.fledg.12d~
                              elo.z.event+
                              age+
                              I(age^2)+
                              tarsus+
                              eventSW+
                              (1|BirdID),
                            data=rank.TLandM.VB.fitness.m)


# subsetting only the necessary for the plotting of each model. 

data.plot5 <- rank.TLandM.VB.fitness.m[!(is.na(rank.TLandM.VB.fitness.m$age)) &
                                         !(is.na(rank.TLandM.VB.fitness.m$soc.fledg.12d)) &
                                         !(is.na(rank.TLandM.VB.fitness.m$tarsus)),]

data.plot5$eventSW <- factor(data.plot5$eventSW)

#simulating a posterior distribution with 5000 draws

smod.soc.fledg.nobib<-sim(mod.soc.fledg.nobib,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of age and tarsus (from this database), and use the year 2014 as reference.

newdat.5<-expand.grid(elo.z.event=seq(min(data.plot5$elo.z.event,na.rm = TRUE),
                              max(data.plot5$elo.z.event,na.rm = TRUE),
                              0.01), 
                      age = mean(data.plot5$age,na.rm = TRUE),
                      tarsus = mean(data.plot5$tarsus,na.rm = TRUE),
                      eventSW = levels(data.plot5$eventSW))


xmat.5<-model.matrix(~elo.z.event+
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

data.plot6 <- rank.TLandM.VB.fitness.m[!(is.na(rank.TLandM.VB.fitness.m$age)) &
                                         !(is.na(rank.TLandM.VB.fitness.m$gen.fledg.12d)) &
                                         !(is.na(rank.TLandM.VB.fitness.m$tarsus)),]

#data.plot6 <- data.plot6[data.plot6$gen.fledg.12d<15,]

data.plot6$eventSW <- factor(data.plot6$eventSW)

mod.gen.fledg.nobib <- lmer(gen.fledg.12d~
                              elo.z.event+
                              age+
                              I(age^2)+
                              tarsus+
                              eventSW+
                              (1|BirdID),
                            data=data.plot6)


#simulating a posterior distribution with 5000 draws

smod.gen.fledg.nobib<-sim(mod.gen.fledg.nobib,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus (from this database)

newdat.6<-expand.grid(elo.z.event=seq(min(data.plot6$elo.z.event,na.rm = TRUE),
                                max(data.plot6$elo.z.event,na.rm = TRUE),
                                0.01), 
                      age = mean(data.plot6$age,na.rm = TRUE),
                      tarsus = mean(data.plot6$tarsus,na.rm = TRUE),
                      eventSW = levels(data.plot6$eventSW))


xmat.6<-model.matrix(~elo.z.event+
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
green <- c(0,100,0)/rgbing
pink <- c(255,0,255)/rgbing


# PLOT saved as .tiff

tiff(#"plots/Figure3_Status_and_fledglings.tiff", 
     "plots/talks/Figure3_Status_and_fledglings_SilwoodJune2017.tiff", 
     height=20, width=20,
     units='cm', compression="lzw", res=300)

#par(mar=c(5, 5, 1, 1))
par(mar=c(6, 7, 1, 1))

plot(data.plot5$elo.z.event, 
     data.plot5$soc.fledg.12d, 
     type="n",
     xlab="",
     ylab= "",
     cex.lab=1.7,
     xaxt="n",yaxt="n",xlim=c(-2.5,3),ylim=c(0,20),
     family="serif",
     frame.plot = FALSE)

title(xlab="randomized Elo-rating", line=4, cex.lab=3, family="serif")
#title(xlab="Standardized Elo-rating", line=4, cex.lab=3.2, family="serif")
title(ylab="annual fledglings", line=4, cex.lab=3, family="serif")


axis(1,at=seq(-2.5,3,by=0.5),
     las=1,
     cex.axis=1.3,
     family="serif") 

axis(2,at=seq(0,20,by=2),
     cex.axis=1.5,
     #cex.axis=1.3,
     las=2,
     family="serif")


# points(data.plot5$elo.z.event, 
#        jitter(data.plot5$soc.fledg.12d,0.65), 
#        pch = 19, col=rgb(turquoise[1], turquoise[2], turquoise[3],0.4),
#        cex = 2.0)

# index.2014<-data.plot6$eventSW=="2014"
# index.2015<-data.plot6$eventSW=="2015"
# index.2016<-data.plot6$eventSW=="2016"

points(data.plot6$elo.z.event, 
       jitter(data.plot6$gen.fledg.12d,0.65),
       pch = 19, col=rgb(blue[1],blue[2],blue[3],0.4),       
       cex = 1.25)

# colour by year
# points(data.plot6$elo.z.event[index.2014], 
#        jitter(data.plot6$gen.fledg.12d[index.2014],0.65),
#        pch = 19, col=rgb(blue[1],blue[2],blue[3],0.4),       
#        cex = 1.25)
# 
# points(data.plot6$elo.z.event[index.2015], 
#        jitter(data.plot6$gen.fledg.12d[index.2015],0.65),
#        pch = 19, col=rgb(green[1],green[2],green[3],0.4),       
#        cex = 1.25)
# 
# points(data.plot6$elo.z.event[index.2016], 
#        jitter(data.plot6$gen.fledg.12d[index.2016],0.65),
#        pch = 19, col=rgb(pink[1],pink[2],pink[3],0.4),       
#        cex = 1.25)

# index.1<-newdat.5$eventSW=="2014"
# 
# polygon(c(newdat.5$elo.z.event[index.1],rev(newdat.5$elo.z.event[index.1])),
#         c(newdat.5$lower[index.1],rev(newdat.5$upper[index.1])),
#         border=NA,col=rgb(turquoise[1], turquoise[2], turquoise[3], 0.15))

index.2<-newdat.5$eventSW=="2016"

polygon(c(newdat.6$elo.z.event[index.2],rev(newdat.6$elo.z.event[index.2])),
        c(newdat.6$lower[index.2],rev(newdat.6$upper[index.2])),
        border=NA,col=rgb(blue[1],blue[2],blue[3], 0.15))

# lines(newdat.5$elo.z.event[index.1], newdat.5$fit[index.1], lwd=3.5,
#       col=rgb(turquoise[1], turquoise[2], turquoise[3],0.8))      
# 
# lines(newdat.5$elo.z.event[index.1], newdat.5$lower[index.1], lty=2, lwd=2,
#       col=rgb(turquoise[1], turquoise[2], turquoise[3],0.65))
# 
# lines(newdat.5$elo.z.event[index.1], newdat.5$upper[index.1], lty=2, lwd=2,
#       col=rgb(turquoise[1], turquoise[2], turquoise[3],0.65))

lines(newdat.6$elo.z.event[index.2], newdat.6$fit[index.2], lwd=3.5,
      col=rgb(blue[1],blue[2],blue[3],0.8))

lines(newdat.6$elo.z.event[index.2], newdat.6$lower[index.2], lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

lines(newdat.6$elo.z.event[index.2], newdat.6$upper[index.2], lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

# op <- par(family = "serif")
# #par(op)
# 
# legend(0.8,10.2,
#        legend=c("social","genetic"),
#        pch=19,
#        col=c(rgb(turquoise[1], turquoise[2], turquoise[3],0.8),
#              rgb(blue[1],blue[2],blue[3],0.8)),
#        pt.cex=1.9,
#        cex=1.1)


dev.off()





################################################################
# PLOTTING ONLY GENETICS
################################################################


# vector needed to obtain the final rgb colours

rgbing <- c(255,255,255)


# few colours in rb

turquoise <- c(49,163,84)/rgbing
blue <- c(44,127,184)/rgbing


# PLOT saved as .tiff

# tiff("plots/Figure3_Status_and_geneticfledglings.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)

tiff("plots/talks/Figure3_Status_and_geneticfledglings_talk.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)

#par(mar=c(5, 5, 1, 1))
par(mar=c(6, 7, 1, 1))

plot(data.plot6$StElo, 
     data.plot6$gen.fledg.12d, 
     type="n",
#      xlab="Standardized Elo-rating",
#      ylab= "Annual number of fledglings",
     xlab="",
     ylab="",
     cex.lab=1.7,
     xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,14),
     family="serif",
     frame.plot = FALSE)

#title(xlab="Standardized Elo-rating", line=4, cex.lab=2.5, family="serif")
title(ylab="fledglings", line=4.5, cex.lab=3.2, family="serif")

axis(1,at=seq(0,1,by=0.2),
     las=1,
     #cex.axis=1.3,
     cex.axis=1.8,
     family="serif") 

axis(2,at=seq(0,14,by=2),
     #cex.axis=1.3,
     cex.axis=1.8,
     las=2,
     family="serif")

points(data.plot6$StElo, 
       jitter(data.plot6$gen.fledg.12d,0.65),
       pch = 19, col=rgb(blue[1],blue[2],blue[3],0.4),       
       cex = 2.0)


polygon(c(newdat.6$StElo,rev(newdat.6$StElo)),
        c(newdat.6$lower,rev(newdat.6$upper)),
        border=NA,col=rgb(blue[1],blue[2],blue[3], 0.15))

lines(newdat.6$StElo, newdat.6$fit, lwd=3.5,
      col=rgb(blue[1],blue[2],blue[3],0.8))

lines(newdat.6$StElo, newdat.6$lower, lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

lines(newdat.6$StElo, newdat.6$upper, lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))


dev.off()

