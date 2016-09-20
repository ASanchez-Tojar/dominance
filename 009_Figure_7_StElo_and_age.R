# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 20th September, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to plot the results on dominance status and age.


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
VB.TLandM.age.fitness <- read.table("finaldatabases/VB.TLandM.age.fitness.csv",header=TRUE,sep=",")


# males subset (females and unknown excluded)

rank.TLandM.VB.fitness.m <- rank.TLandM.VB.fitness[rank.TLandM.VB.fitness$sex==1 &
                                                     !(is.na(rank.TLandM.VB.fitness$sex)),]

# the Seewiesen database

final.cap.db.3 <- read.table("finaldatabases/final.cap.db.3.csv",header=TRUE,sep=",")




################################################################
# WILD MODEL
################################################################

# subsetting only the necessary for the plotting of each model. 

data.plot1 <- rank.TLandM.VB.fitness.m[!(is.na(rank.TLandM.VB.fitness.m$age)) &
                                         rank.TLandM.VB.fitness.m$age>0 &
                                         !(is.na(rank.TLandM.VB.fitness.m$tarsus)) &
                                         #!(is.na(rank.TLandM.VB.fitness.m$bib)) &
                                         !(is.na(rank.TLandM.VB.fitness.m$season)),]

mod.rank.age <- lmer(StElo~age+
                       tarsus+
                       #bib+
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

newdat<-expand.grid(age=seq(min(data.plot1$age,na.rm = TRUE),
                            max(data.plot1$age,na.rm = TRUE),
                            0.01), 
                    tarsus = mean(data.plot1$tarsus,na.rm = TRUE),
                    season=0.5)


xmat<-model.matrix(~age+
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
# CAPTIVE MODEL
################################################################

# great outlier excluded. Results don't change much any way, but
# the negative effects is, of course, less strong due to its
# absence

data.plot2 <- final.cap.db.3[!(is.na(final.cap.db.3$age2014.mean)) &
                               !(is.na(final.cap.db.3$TarsusLength)),
                               #!(is.na(final.cap.db.3$meanVB.mean10)) &
                               #final.cap.db.3$age2014.mean<5&
                               #final.cap.db.3$meanVB.mean10>41
                             ]

mod.rank.age.capt <- lmer(StElo~age2014.mean+
                            TarsusLength+
                            (1|Aviary),
                          data=data.plot2)

smod.rank.age.capt<-sim(mod.rank.age.capt,5000)



# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus and age (from this database)

newdat.2<-expand.grid(age2014.mean = seq(min(data.plot2$age2014.mean,na.rm = TRUE),
                                         max(data.plot2$age2014.mean,na.rm = TRUE),
                                         0.01),
                      TarsusLength = mean(data.plot2$TarsusLength,na.rm = TRUE))


xmat.2<-model.matrix(~~age2014.mean+
                       TarsusLength, 
                     data=newdat.2) 

fitmatboth.2 <- matrix(NA, 
                       ncol = nrow(smod.rank.age.capt@fixef),
                       nrow = nrow(newdat.2))


for(i in 1:nrow(smod.rank.age.capt@fixef)) {
  fitmatboth.2[,i] <- xmat.2%*%smod.rank.age.capt@fixef[i,]
}


# finally estimating the mean and the credible intervals for each
# value of bib length. This is what I will plot later on.

newdat.2$fit<-apply(fitmatboth.2, 1, mean) # 1= row , 2 = colum
newdat.2$lower<-apply(fitmatboth.2, 1, quantile, prob= 0.025)
newdat.2$upper<-apply(fitmatboth.2, 1, quantile, prob= 0.975)





################################################################
# PLOTTING
################################################################


# vector needed to obtain the final rgb colours

rgbing <- c(255,255,255)


# few colours in rb

lightblue <- c(166,206,227)/rgbing
darkblue <- c(31,120,180)/rgbing
chocolate1 <- c(255,127,36)/rgbing



# PLOT saved as .tiff

# tiff("plots/Figure1_Status_and_age.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)

tiff("plots/talks/Figure1_Status_and_age_talk.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)

# tiff("plots/talks/Figure1_Status_and_Bib_talk_noelderly.tiff", height=20, width=20,
#      units='cm', compression="lzw", res=300)

#par(mar=c(5, 5, 1, 1))
par(mar=c(6, 7, 1, 1))


plot(data.plot1$age, 
     data.plot1$StElo, 
     type="n",
     #      xlab="Bib length (mm)",
     #      ylab= "Standardized Elo-rating",
     xlab="",
     ylab="",
     #cex.lab=1.7,
     cex.lab=2.4,
     xaxt="n",yaxt="n",xlim=c(0.5,9.5),ylim=c(0,1),
     family="serif",
     frame.plot = FALSE)

#title(xlab="Bib length (mm)", line=4, cex.lab=3.0, family="serif")
title(ylab="Standardized Elo-rating", line=4.5, cex.lab=3.0, family="serif")

axis(1,at=seq(0.5,9.5,by=1),
     #cex.axis=1.3,
     cex.axis=1.8,
     family="serif")

axis(2,at=seq(0,1,by=0.2),
     las=2,
     #cex.axis=1.3,
     cex.axis=1.8,
     family="serif") 

points(data.plot2$age2014.mean, 
       data.plot2$StElo, 
       pch = 19, col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.4),
       cex = 2.0)

points(data.plot1$age, 
       data.plot1$StElo, 
       pch = 19, col=rgb(darkblue[1],darkblue[2],darkblue[3],0.4),
       cex = 2.0)

polygon(c(newdat.2$age2014.mean,rev(newdat.2$age2014.mean)),
        c(newdat.2$lower,rev(newdat.2$upper)),
        border=NA,col=rgb(chocolate1[1], chocolate1[2], chocolate1[3], 0.15))

polygon(c(newdat$age,rev(newdat$age)),
        c(newdat$lower,rev(newdat$upper)),
        border=NA,col=rgb(darkblue[1],darkblue[2],darkblue[3], 0.15))

lines(newdat.2$age2014.mean, newdat.2$fit, lwd=3.5,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8)) 

lines(newdat.2$age2014.mean, newdat.2$lower, lty=2, lwd=2,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.65))

lines(newdat.2$age2014.mean, newdat.2$upper, lty=2, lwd=2,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.65))


lines(newdat$age, newdat$fit, lwd=3.5,
      col=rgb(darkblue[1],darkblue[2],darkblue[3],0.8)) 

lines(newdat$age, newdat$lower, lty=2, lwd=2,
      col=rgb(darkblue[1],darkblue[2],darkblue[3],0.65))

lines(newdat$age, newdat$upper, lty=2, lwd=2,
      col=rgb(darkblue[1],darkblue[2],darkblue[3],0.65))

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
