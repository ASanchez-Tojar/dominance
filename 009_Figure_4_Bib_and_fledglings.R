# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 6th September, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to plot the results on bib size and social and genetic
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

VB.TLandM.age.fitness <- read.table("finaldatabases/VB.TLandM.age.fitness.csv",header=TRUE,sep=",")

VB.TLandM.age.fitness.2 <- VB.TLandM.age.fitness[VB.TLandM.age.fitness$soc.fledg.12d<13 &
                                                   !(is.na(VB.TLandM.age.fitness$soc.fledg.12d)),]


################################################################
# SOCIAL FLEDGLINGS MODEL
################################################################

mod.soc.fledg.bib <- lmer(soc.fledg.12d~
                            bib+
                            age+
                            I(age^2)+
                            tarsus+
                            eventSW+
                            (1|BirdID),
                          data=VB.TLandM.age.fitness.2)


# subsetting only the necessary for the plotting of each model. 

data.plot6 <- VB.TLandM.age.fitness.2[!(is.na(VB.TLandM.age.fitness.2$age)) &
                                        !(is.na(VB.TLandM.age.fitness.2$tarsus)) &
                                        !(is.na(VB.TLandM.age.fitness.2$bib)),]


#simulating a posterior distribution with 5000 draws

smod.soc.fledg.bib<-sim(mod.soc.fledg.bib,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of age and tarsus (from this database), and use the year 2014 as reference.

newdat.7<-expand.grid(bib=seq(min(data.plot6$bib,na.rm = TRUE),
                              max(data.plot6$bib,na.rm = TRUE),
                              0.01), 
                      age = mean(data.plot6$age,na.rm = TRUE),
                      tarsus = mean(data.plot6$tarsus,na.rm = TRUE),
                      eventSW = 2014)


xmat.7<-model.matrix(~bib+
                       age+
                       I(age^2)+
                       tarsus+
                       eventSW, 
                     data=newdat.7) 

fitmatboth.7 <- matrix(NA,
                       ncol = nrow(smod.soc.fledg.bib@fixef),
                       nrow = nrow(newdat.7))


for(i in 1:nrow(smod.soc.fledg.bib@fixef)) {
  fitmatboth.7[,i] <- xmat.7%*%smod.soc.fledg.bib@fixef[i,]
}


# finally estimating the mean and the credible intervals for each
# value of dominance status. This is what I will plot later on.

newdat.7$fit<-apply(fitmatboth.7, 1, mean) 
newdat.7$lower<-apply(fitmatboth.7, 1, quantile, prob= 0.025)
newdat.7$upper<-apply(fitmatboth.7, 1, quantile, prob= 0.975)



################################################################
# GENETIC FLEDGLINGS MODEL
################################################################

# Running extra analyses excluding a big outlier

VB.TLandM.age.fitness.3 <- VB.TLandM.age.fitness[VB.TLandM.age.fitness$gen.fledg.12d<15&
                                                   !(is.na(VB.TLandM.age.fitness$gen.fledg.12d)),]

# Running an lmer() with BirdID as a random effects shows that the
# variance associated to BirdID cannot be computed. Therefore, I decided
# to run a lm() excluding 1 of the observations for those 13 individuals
# with two fitness estimates.


# The list of birds with obseravtions to be randomly excluded is:

# sort(table(VB.TLandM.age.fitness.3$BirdID))

repeatedBirds <- c(4943,5139,5195,5424,6786,6808,6921,6924,
                   6927,7074,7160,7267,7275)


# # choosing 2014 or 2015 randomly for each bird
# 
# randomselection <- sample(c(2014,2015),length(repeatedBirds),replace=TRUE)

# The first time run was:
randomselection <- c(2015,2014,2015,2015,2014,2015,2014,2015,2014,2014,2015,2015,2014)


# cbinding them together

randomyear <- as.data.frame(cbind(repeatedBirds,randomselection))


randomyear$BirdID_eventSW <- factor(paste(randomyear$repeatedBirds,
                                          randomyear$randomselection,
                                          sep="_"))


randomyear.2 <- randomyear[,c("BirdID_eventSW")]


# Now I can exclude these observations creating a new database

VB.TLandM.age.fitness.4 <- VB.TLandM.age.fitness.3

VB.TLandM.age.fitness.4$gen.fledg.12d <- ifelse((VB.TLandM.age.fitness.4$BirdID_eventSW %in% randomyear.2),
                                                NA,
                                                VB.TLandM.age.fitness.4$gen.fledg.12d)

VB.TLandM.age.fitness.5 <- VB.TLandM.age.fitness.4[!(is.na(VB.TLandM.age.fitness.4$gen.fledg.12d)),]


# model

mod.gen.fledg.bib <- lm(gen.fledg.12d~
                          bib+
                          age+
                          I(age^2)+
                          tarsus+
                          eventSW,                        
                        data=VB.TLandM.age.fitness.5)


# subsetting only the necessary for the plotting of each model. 

data.plot7 <- VB.TLandM.age.fitness.5[!(is.na(VB.TLandM.age.fitness.5$age)) &
                                        !(is.na(VB.TLandM.age.fitness.5$tarsus)) &
                                        !(is.na(VB.TLandM.age.fitness.5$bib)),]



#simulating a posterior distribution with 5000 draws

smod.gen.fledg.bib<-sim(mod.gen.fledg.bib,5000)


# Generating a database with what is run in the model. The model estimates
# calculated and presented in the plot correspond to those for a mean value
# of age and tarsus (from this database), and use the year 2014 as reference.

newdat.8<-expand.grid(bib=seq(min(data.plot7$bib,na.rm = TRUE),
                              max(data.plot7$bib,na.rm = TRUE),
                              0.01), 
                      age = mean(data.plot7$age,na.rm = TRUE),
                      tarsus = mean(data.plot7$tarsus,na.rm = TRUE),
                      eventSW = 2014)


xmat.8<-model.matrix(~bib+
                       age+
                       I(age^2)+
                       tarsus+
                       eventSW, 
                     data=newdat.8) 

fitmatboth.8 <- matrix(NA,
                       ncol = nrow(smod.gen.fledg.bib@coef),
                       nrow = nrow(newdat.8))


for(i in 1:nrow(smod.gen.fledg.bib@coef)) {
  fitmatboth.8[,i] <- xmat.8%*%smod.gen.fledg.bib@coef[i,]
}


# finally estimating the mean and the credible intervals for each
# value of dominance status. This is what I will plot later on.

newdat.8$fit<-apply(fitmatboth.8, 1, mean) 
newdat.8$lower<-apply(fitmatboth.8, 1, quantile, prob= 0.025)
newdat.8$upper<-apply(fitmatboth.8, 1, quantile, prob= 0.975)



################################################################
# PLOTTING
################################################################

# vector needed to obtain the final rgb colours

rgbing <- c(255,255,255)


# few colours in rgb

turquoise <- c(49,163,84)/rgbing
blue <- c(44,127,184)/rgbing


# PLOT saved as .tiff

tiff("plots/Figure4_Bib_and_fledglings.tiff", height=20, width=20,
     units='cm', compression="lzw", res=300)

par(mar=c(5, 5, 1, 1))

plot(data.plot6$bib, 
     data.plot6$soc.fledg.12d, 
     type="n",
     xlab="Bib length (mm)",
     ylab= "Annual number of fledglings",
     cex.lab=1.7,
     xaxt="n",yaxt="n",xlim=c(44,60),ylim=c(0,14),
     family="serif",
     frame.plot = FALSE)


axis(1,at=seq(44,60,by=2),
     las=1,
     cex.axis=1.3,
     family="serif") 

axis(2,at=seq(0,14,by=1),
     cex.axis=1.3,
     las=2,
     family="serif")


points(data.plot6$bib, 
       jitter(data.plot6$soc.fledg.12d,0.65), 
       pch = 19, col=rgb(turquoise[1], turquoise[2], turquoise[3],0.4),
       cex = 2.0)

points(data.plot7$bib, 
       jitter(data.plot7$gen.fledg.12d,0.65),
       pch = 19, col=rgb(blue[1],blue[2],blue[3],0.4),       
       cex = 2.0)

polygon(c(newdat.7$bib,rev(newdat.7$bib)),
        c(newdat.7$lower,rev(newdat.7$upper)),
        border=NA,col=rgb(turquoise[1], turquoise[2], turquoise[3], 0.15))

polygon(c(newdat.8$bib,rev(newdat.8$bib)),
        c(newdat.8$lower,rev(newdat.8$upper)),
        border=NA,col=rgb(blue[1],blue[2],blue[3], 0.15))

lines(newdat.7$bib, newdat.7$fit, lwd=3.5,
      col=rgb(turquoise[1], turquoise[2], turquoise[3],0.8))      

lines(newdat.7$bib, newdat.7$lower, lty=2, lwd=2,
      col=rgb(turquoise[1], turquoise[2], turquoise[3],0.65))

lines(newdat.7$bib, newdat.7$upper, lty=2, lwd=2,
      col=rgb(turquoise[1], turquoise[2], turquoise[3],0.65))

lines(newdat.8$bib, newdat.8$fit, lwd=3.5,
      col=rgb(blue[1],blue[2],blue[3],0.8))

lines(newdat.8$bib, newdat.8$lower, lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

lines(newdat.8$bib, newdat.8$upper, lty=2, lwd=2,
      col=rgb(blue[1],blue[2],blue[3],0.65))

op <- par(family = "serif")
#par(op)

legend(57.5,14,
       legend=c("social","genetic"),
       pch=19,
       col=c(rgb(turquoise[1], turquoise[2], turquoise[3],0.8),
             rgb(blue[1],blue[2],blue[3],0.8)),
       pt.cex=1.9,
       cex=1.1)


dev.off()
