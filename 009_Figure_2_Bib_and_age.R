# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 5th September, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to plot the results on bib and age


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


mod.bib.age <- lmer(bib~
                      age+
                      tarsus+
                      (1|BirdID)+
                      (1|eventSW),
                    data=VB.TLandM.age.fitness)


# subsetting only the necessary for the plotting of each model. 

data.plot3 <- VB.TLandM.age.fitness[!(is.na(VB.TLandM.age.fitness$age)) &
                                         !(is.na(VB.TLandM.age.fitness$tarsus)),]


#simulating a posterior distribution with 5000 draws

smod.bib.age<-sim(mod.bib.age,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus (from this database)

newdat.3<-expand.grid(age=seq(min(data.plot3$age,na.rm = TRUE),
                              max(data.plot3$age,na.rm = TRUE),
                              0.01), 
                      tarsus = mean(data.plot3$tarsus,na.rm = TRUE))


xmat.3<-model.matrix(~~age+
                     tarsus, 
                   data=newdat.3) 

fitmatboth.3 <- matrix(NA,
                       ncol = nrow(smod.bib.age@fixef),
                       nrow = nrow(newdat.3))


for(i in 1:nrow(smod.bib.age@fixef)) {
  fitmatboth.3[,i] <- xmat.3%*%smod.bib.age@fixef[i,]
}


# finally estimating the mean and the credible intervals for each
# value of bib length. This is what I will plot later on.

newdat.3$fit<-apply(fitmatboth.3, 1, mean) 
newdat.3$lower<-apply(fitmatboth.3, 1, quantile, prob= 0.025)
newdat.3$upper<-apply(fitmatboth.3, 1, quantile, prob= 0.975)




################################################################
# CAPTIVE MODEL
################################################################

# great outlier excluded. Results don't change much any way

data.plot4 <- final.cap.db.3[!(is.na(final.cap.db.3$age2014.mean)) &
                               !(is.na(final.cap.db.3$TarsusLength)) &
                               !(is.na(final.cap.db.3$meanVB.mean10)) &
                               final.cap.db.3$meanVB.mean10>41
                             ,]

mod.bib.age.capt <- lm(meanVB.mean10~age2014.mean+
                         TarsusLength+
                         Aviary,
                       data=data.plot4)

smod.bib.age.capt<-sim(mod.bib.age.capt,5000)



# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus (from this database). A fictious mean Aviary is chosen for
# plotting purposes.

newdat.4<-expand.grid(age2014.mean=seq(min(data.plot4$age2014.mean,na.rm = TRUE),
                                       max(data.plot4$age2014.mean,na.rm = TRUE),
                                       0.01), 
                      TarsusLength = mean(data.plot4$TarsusLength,na.rm = TRUE),
                      Aviary = 2.5) # ficticious middle aviary


xmat.4<-model.matrix(~~age2014.mean+
                       TarsusLength+
                       Aviary, 
                     data=newdat.4) 

fitmatboth.4 <- matrix(NA,
                       ncol = nrow(smod.bib.age.capt@coef),
                       nrow = nrow(newdat.4))


for(i in 1:nrow(smod.bib.age.capt@coef)) {
  fitmatboth.4[,i] <- xmat.4%*%smod.bib.age.capt@coef[i,]
}


# finally estimating the mean and the credible intervals for each
# value of bib length. This is what I will plot later on.

newdat.4$fit<-apply(fitmatboth.4, 1, mean)
newdat.4$lower<-apply(fitmatboth.4, 1, quantile, prob= 0.025)
newdat.4$upper<-apply(fitmatboth.4, 1, quantile, prob= 0.975)



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

tiff("plots/Figure2_Bib_and_age.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)

par(mar=c(5, 5, 1, 1))

plot(data.plot3$age, 
     data.plot3$bib, 
     type="n",
     xlab="Age",
     ylab= "Bib length (mm)",
     cex.lab=1.7,
     xaxt="n",yaxt="n",xlim=c(0.5,9.5),ylim=c(40,60),
     family="serif",
     frame.plot = FALSE)


axis(1,at=seq(0.5,9.5,by=1),
     las=1,
     cex.axis=1.3,
     family="serif") 

axis(2,at=seq(40,60,by=2),
     cex.axis=1.3,
     las=2,
     family="serif")


points(jitter(data.plot4$age2014.mean,0.55), 
       data.plot4$meanVB.mean10, 
       pch = 19, col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.4),
       cex = 2.0)

points(jitter(data.plot3$age,0.55), 
       data.plot3$bib,
       pch = 19, col=rgb(darkblue[1],darkblue[2],darkblue[3],0.4),       
       cex = 2.0)

polygon(c(newdat.4$age2014.mean,rev(newdat.4$age2014.mean)),
        c(newdat.4$lower,rev(newdat.4$upper)),
        border=NA,col=rgb(chocolate1[1], chocolate1[2], chocolate1[3], 0.15))

polygon(c(newdat.3$age,rev(newdat.3$age)),
        c(newdat.3$lower,rev(newdat.3$upper)),
        border=NA,col=rgb(darkblue[1],darkblue[2],darkblue[3], 0.15))

lines(newdat.4$age2014.mean, newdat.4$fit, lwd=3.5,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8))      

lines(newdat.4$age2014.mean, newdat.4$lower, lty=2, lwd=2,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.65))

lines(newdat.4$age2014.mean, newdat.4$upper, lty=2, lwd=2,
      col=rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.65))

lines(newdat.3$age, newdat.3$fit, lwd=3.5,
      col=rgb(darkblue[1],darkblue[2],darkblue[3],0.8))

lines(newdat.3$age, newdat.3$lower, lty=2, lwd=2,
      col=rgb(darkblue[1],darkblue[2],darkblue[3],0.65))

lines(newdat.3$age, newdat.3$upper, lty=2, lwd=2,
      col=rgb(darkblue[1],darkblue[2],darkblue[3],0.65))

op <- par(family = "serif")
#par(op)

legend(7.5,44,
       legend=c("captive","wild"),
       pch=19,
       col=c(rgb(chocolate1[1], chocolate1[2], chocolate1[3],0.8),
             rgb(darkblue[1],darkblue[2],darkblue[3],0.8)),
       pt.cex=1.9,
       cex=1.1)


dev.off()
