# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 15th September, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to plot the results on dominance status and social and genetic
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

rank.TLandM.VB.fitness <- read.table("finaldatabases/rank.TLandM.VB.fitness.csv",header=TRUE,sep=",")

# males subset (females and unknown excluded)

rank.TLandM.VB.fitness.m <- rank.TLandM.VB.fitness[rank.TLandM.VB.fitness$sex==1 &
                                                     !(is.na(rank.TLandM.VB.fitness$sex)),]


################################################################
# SOCIAL RECRUITS MODEL
################################################################

rank.TLandM.VB.fitness.m.2 <- rank.TLandM.VB.fitness.m[!(is.na(rank.TLandM.VB.fitness.m$soc.recruits)),]

# The list of birds with obseravtions to be randomly excluded is:

#sort(table(rank.TLandM.VB.fitness.m.2$BirdID))

repeatedBirds.soc.rec <- c(4900,5195,6687,6786,6924,7074)


# # choosing 2014 or 2015 randomly for each bird
# 
# randomselection.soc.rec <- sample(c(2014,2015),
#                                   length(repeatedBirds.soc.rec),
#                                   replace=TRUE)

# The first time run was:
randomselection.soc.rec <- c(2015,2015,2015,2015,2014,2014)


# cbinding them together

randomyear.soc.rec <- as.data.frame(cbind(repeatedBirds.soc.rec,
                                          randomselection.soc.rec))


randomyear.soc.rec$BirdID_eventSW <- factor(paste(randomyear.soc.rec$repeatedBirds.soc.rec,
                                                  randomyear.soc.rec$randomselection.soc.rec,
                                                  sep="_"))


randomyear.soc.rec.2 <- randomyear.soc.rec[,c("BirdID_eventSW")]


# Now I can exclude these observations creating a new database

rank.TLandM.VB.fitness.m.3 <- rank.TLandM.VB.fitness.m.2

rank.TLandM.VB.fitness.m.3$soc.recruits <- ifelse((rank.TLandM.VB.fitness.m.3$BirdID_eventSW %in% randomyear.soc.rec.2),
                                                  NA,
                                                  rank.TLandM.VB.fitness.m.3$soc.recruits)

rank.TLandM.VB.fitness.m.4 <- rank.TLandM.VB.fitness.m.3[!(is.na(rank.TLandM.VB.fitness.m.3$soc.recruits)),]

rank.TLandM.VB.fitness.m.4$eventSW <- as.factor(rank.TLandM.VB.fitness.m.4$eventSW)

mod.soc.recruits.nobib <- glm(soc.recruits~
                                StElo+
                                age+
                                I(age^2)+
                                tarsus+
                                eventSW,
                              data=rank.TLandM.VB.fitness.m.4,
                              family=poisson)



#simulating a posterior distribution with 5000 draws

smod.soc.recruits.nobib<-sim(mod.soc.recruits.nobib,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of age and tarsus (from this database), and use the year 2014 as reference.

newdat.9<-expand.grid(StElo=seq(min(rank.TLandM.VB.fitness.m.4$StElo,na.rm = TRUE),
                                max(rank.TLandM.VB.fitness.m.4$StElo,na.rm = TRUE),
                                0.001), 
                      age = mean(rank.TLandM.VB.fitness.m.4$age,na.rm = TRUE),
                      tarsus = mean(rank.TLandM.VB.fitness.m.4$tarsus,na.rm = TRUE),
                      eventSW = levels(rank.TLandM.VB.fitness.m.4$eventSW))


xmat.9<-model.matrix(~StElo+
                       age+
                       I(age^2)+
                       tarsus+
                       eventSW, 
                     data=newdat.9) 

fitmatboth.9 <- matrix(NA,
                       ncol = nrow(smod.soc.recruits.nobib@coef),
                       nrow = nrow(newdat.9))


for(i in 1:nrow(smod.soc.recruits.nobib@coef)) {
  fitmatboth.9[,i] <- exp(xmat.9%*%smod.soc.recruits.nobib@coef[i,]) #exp because of log link
}


# finally estimating the mean and the credible intervals for each
# value of dominance status. This is what I will plot later on.

newdat.9$fit<-apply(fitmatboth.9, 1, mean) 
newdat.9$lower<-apply(fitmatboth.9, 1, quantile, prob= 0.025)
newdat.9$upper<-apply(fitmatboth.9, 1, quantile, prob= 0.975)




################################################################
# GENETIC RECRUITS MODEL
################################################################

rank.TLandM.VB.fitness.m.5 <- rank.TLandM.VB.fitness.m[!(is.na(rank.TLandM.VB.fitness.m$gen.recruits)),]

rank.TLandM.VB.fitness.m.5$eventSW <- as.factor(rank.TLandM.VB.fitness.m.5$eventSW)

mod.gen.recruits.nobib <- glmer(gen.recruits~
                                  StElo+
                                  age+
                                  I(age^2)+
                                  tarsus+
                                  eventSW+
                                  (1|BirdID),
                                data=rank.TLandM.VB.fitness.m.5,
                                family=poisson)


#simulating a posterior distribution with 5000 draws

smod.gen.recruits.nobib<-sim(mod.gen.recruits.nobib,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of tarsus (from this database)

newdat.10<-expand.grid(StElo=seq(min(rank.TLandM.VB.fitness.m.5$StElo,na.rm = TRUE),
                                max(rank.TLandM.VB.fitness.m.5$StElo,na.rm = TRUE),
                                0.001), 
                      age = mean(rank.TLandM.VB.fitness.m.5$age,na.rm = TRUE),
                      tarsus = mean(rank.TLandM.VB.fitness.m.5$tarsus,na.rm = TRUE),
                      eventSW = levels(rank.TLandM.VB.fitness.m.5$eventSW))


xmat.10<-model.matrix(~StElo+
                       age+
                       I(age^2)+
                       tarsus+
                       eventSW, 
                     data=newdat.10) 

fitmatboth.10 <- matrix(NA,
                       ncol = nrow(smod.gen.recruits.nobib@fixef),
                       nrow = nrow(newdat.10))


for(i in 1:nrow(smod.gen.recruits.nobib@fixef)) {
  fitmatboth.10[,i] <- exp(xmat.10%*%smod.gen.recruits.nobib@fixef[i,])
}


# finally estimating the mean and the credible intervals for each
# value of bib length. This is what I will plot later on.

newdat.10$fit<-apply(fitmatboth.10, 1, mean) 
newdat.10$lower<-apply(fitmatboth.10, 1, quantile, prob= 0.025)
newdat.10$upper<-apply(fitmatboth.10, 1, quantile, prob= 0.975)





################################################################
# PLOTTING
################################################################


# vector needed to obtain the final rgb colours

rgbing <- c(255,255,255)


# few colours in rb

turquoise <- c(49,163,84)/rgbing
blue <- c(44,127,184)/rgbing


# PLOT saved as .tiff

tiff("plots/Figure5_Status_and_recruits.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)

par(mar=c(5, 5, 1, 1))

plot(rank.TLandM.VB.fitness.m.4$StElo, 
     rank.TLandM.VB.fitness.m.4$soc.recruits, 
     type="n",
     xlab="Standardized Elo-rating",
     ylab= "Annual number of recruits",
     cex.lab=1.7,
     xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,4),
     family="serif",
     frame.plot = FALSE)


axis(1,at=seq(0,1,by=0.2),
     las=1,
     cex.axis=1.3,
     family="serif") 

axis(2,at=seq(0,4,by=1),
     cex.axis=1.3,
     las=2,
     family="serif")


points(rank.TLandM.VB.fitness.m.4$StElo, 
       jitter(rank.TLandM.VB.fitness.m.4$soc.recruits,0.50), 
       pch = 19, col=rgb(turquoise[1], turquoise[2], turquoise[3],0.4),
       cex = 2.0)

points(rank.TLandM.VB.fitness.m.5$StElo, 
       jitter(rank.TLandM.VB.fitness.m.5$gen.recruits,0.50),
       pch = 19, col=rgb(blue[1],blue[2],blue[3],0.4),       
       cex = 2.0)

index.1<-newdat.9$eventSW=="2014" # only calls the plot but not the points yet

polygon(c(newdat.9$StElo[index.1],rev(newdat.9$StElo[index.1])),
        c(newdat.9$lower[index.1],rev(newdat.9$upper[index.1])),
        border=NA,col=rgb(turquoise[1], turquoise[2], turquoise[3], 0.15))

index.2<-newdat.10$eventSW=="2014" # only calls the plot but not the points yet

polygon(c(newdat.10$StElo[index.2],rev(newdat.10$StElo[index.2])),
        c(newdat.10$lower[index.2],rev(newdat.10$upper[index.2])),
        border=NA,col=rgb(blue[1],blue[2],blue[3], 0.15))

lines(newdat.9$StElo[index.1], newdat.9$fit[index.1], lwd=3.5,
      col=rgb(turquoise[1], turquoise[2], turquoise[3],0.8))      

lines(newdat.9$StElo[index.1], newdat.9$lower[index.1], lty=2, lwd=2,
      col=rgb(turquoise[1], turquoise[2], turquoise[3],0.65))

lines(newdat.9$StElo[index.1], newdat.9$upper[index.1], lty=2, lwd=2,
      col=rgb(turquoise[1], turquoise[2], turquoise[3],0.65))

lines(newdat.10$StElo[index.2], newdat.10$fit[index.2], lwd=3.5,
      col=rgb(blue[1],blue[2],blue[3],0.8))

lines(newdat.10$StElo[index.2], newdat.10$lower[index.2], lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

lines(newdat.10$StElo[index.2], newdat.10$upper[index.2], lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

op <- par(family = "serif")
#par(op)

legend(0.8,4,
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

tiff("plots/Figure5_Status_and_geneticrecruits.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)

par(mar=c(5, 5, 1, 1))

plot(rank.TLandM.VB.fitness.m.5$StElo, 
     rank.TLandM.VB.fitness.m.5$gen.recruits, 
     type="n",
     xlab="Standardized Elo-rating",
     ylab= "Annual number of recruits",
     cex.lab=1.7,
     xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,4),
     family="serif",
     frame.plot = FALSE)


axis(1,at=seq(0,1,by=0.2),
     las=1,
     cex.axis=1.3,
     family="serif") 

axis(2,at=seq(0,4,by=1),
     cex.axis=1.3,
     las=2,
     family="serif")


points(rank.TLandM.VB.fitness.m.5$StElo, 
       jitter(rank.TLandM.VB.fitness.m.5$gen.recruits,0.50),
       pch = 19, col=rgb(blue[1],blue[2],blue[3],0.4),       
       cex = 2.0)

index.2<-newdat.10$eventSW=="2014" # only calls the plot but not the points yet

polygon(c(newdat.10$StElo[index.2],rev(newdat.10$StElo[index.2])),
        c(newdat.10$lower[index.2],rev(newdat.10$upper[index.2])),
        border=NA,col=rgb(blue[1],blue[2],blue[3], 0.15))

lines(newdat.10$StElo[index.2], newdat.10$fit[index.2], lwd=3.5,
      col=rgb(blue[1],blue[2],blue[3],0.8))

lines(newdat.10$StElo[index.2], newdat.10$lower[index.2], lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

lines(newdat.10$StElo[index.2], newdat.10$upper[index.2], lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))


dev.off()
