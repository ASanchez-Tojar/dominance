# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 11th of August, 2017

############################################################################
# Description of script and Instructions
############################################################################

# This script analyze survival in relation to dominance


############################################################################
# Packages needed
############################################################################

# packages needed

library(lme4)
library(arm)
library(MCMCglmm)

sessionInfo()


# Clear memory
rm(list=ls())


# survival <- read.table("survival/data/survival_noRFID_including2017summeralmostcompletely.csv",
#                        header=TRUE,sep=",")
survival <- read.table("survival/data/survival_withRFID_including2017summercompletely.csv",
                       header=TRUE,sep=",")


# Estimating whether birds survive to next season or not (0=no, 1=yes)
# for a binary GLMM

survival <- survival[order(survival$BirdID,survival$DetectionDate),]

survival$BirdID <- as.factor(survival$BirdID)

survival.final <- data.frame(BirdID=integer(),
                             DetectionDate=numeric(),
                             method=factor(),
                             DetectionDate.2=character(),
                             Cohort=integer(),
                             AliveNextTime=integer(),
                             stringsAsFactors=FALSE)

for (i in levels(survival$BirdID)){
  
  x <- survival[survival$BirdID==i,]
  maxDate <- max(x$DetectionDate)
  
  x$AliveNextTime <- ifelse(x$DetectionDate==maxDate,
                    0,
                    1)
  
  survival.final <- rbind(survival.final,x)
  
}

survival.final$BirdID_eventSW <- paste(survival.final$BirdID,
                                       survival.final$DetectionDate,
                                       sep="_")

survival.final.reduced <- survival.final[,c("BirdID_eventSW","AliveNextTime","method")]


# dominance data
rank.fit <- read.table("finaldatabases/rank.TLandM.VB.fitness_sim.csv",header=TRUE,sep=",")

rank.fit.reduced <- rank.fit[,c("BirdID","BirdID_eventSW","StElo","eventSW",
                                "cohort","age","elo.z.event","sex")]


# add survival data to dominance data

rank.and.survival <- merge(rank.fit.reduced,survival.final.reduced,
                           by="BirdID_eventSW",all.x=TRUE)

#removing BirdID_eventSW
rank.and.survival <- rank.and.survival[order(rank.and.survival$BirdID,
                                             rank.and.survival$eventSW),c(-1)]



###############
#model MCMCglmm
###############

rank.and.survival$sex <- as.factor(rank.and.survival$sex)

rank.and.survival$StElo.z <- scale(rank.and.survival$StElo)
rank.and.survival$age.z <- scale(rank.and.survival$age)

prior<-list(R = list(V = diag(1), fix = 1),
            G =list(G1 = list(V = diag(1), nu = 0.002),
                    G2 = list(V = diag(1), nu = 0.002)))

MCMC.survival<-MCMCglmm(AliveNextTime~StElo.z*age.z+
                          StElo.z*sex+
                          I(age.z^2),
                        random = ~ BirdID + eventSW,
                        data = rank.and.survival,
                        prior=prior,
                        verbose= FALSE,
                        nitt=4000000, burnin=400000,thin=4000,
                        family="categorical")

# save(MCMC.survival,
#      file="survival/models_MCMCglmm/MCMC.survival.RData")
save(MCMC.survival,
     file="survival/models_MCMCglmm/MCMC.survival_age2.RData")

load("survival/models_MCMCglmm/MCMC.survival.RData")


plot(MCMC.survival)
summary(MCMC.survival)

#check chain autocorrelation
autocorr(MCMC.survival$Sol)
autocorr(MCMC.survival$VCV[,c(1,2)])

#ICC for random factors
mean(MCMC.survival$VCV[,1]/(rowSums(MCMC.survival$VCV) + pi^2/3))
mean(MCMC.survival$VCV[,2]/(rowSums(MCMC.survival$VCV) + pi^2/3))

#estimates and 95% CrI
for(i in 1:ncol(MCMC.survival$Sol)){
  print(round(mean(MCMC.survival$Sol[,i]),2))
}

round(HPDinterval(MCMC.survival$Sol[,c(1:7)]),2)

round(mean(MCMC.survival$VCV[,1]),2)
round(mean(MCMC.survival$VCV[,2]),2)
round(HPDinterval(MCMC.survival$VCV[,c(1,2)]),2)



#########################
#generating data for plot
#########################
newdat<-expand.grid(StElo=seq(min(rank.and.survival$StElo,na.rm = TRUE),
                              max(rank.and.survival$StElo,na.rm = TRUE),
                              1), 
                    age.z = mean(rank.and.survival$age.z,na.rm = TRUE),
                    sex = levels(rank.and.survival$sex))

newdat$StElo.z <- (newdat$StElo - mean(rank.and.survival$StElo,na.rm = TRUE))/sd(rank.and.survival$StElo,na.rm = TRUE)

xmat<-model.matrix(~StElo.z*age.z+
                     StElo.z*sex+
                     I(age.z^2), 
                   data=newdat) 

fitmatboth <- matrix(NA,
                     ncol = nrow(MCMC.survival$Sol),
                     nrow = nrow(newdat))


for(i in 1:nrow(MCMC.survival$Sol)) {
  fitmatboth[,i] <- plogis(xmat%*%MCMC.survival$Sol[i,c(1:7)]) #plogis because of logit link
}

newdat$fit<-apply(fitmatboth, 1, mean) 
newdat$lower<-apply(fitmatboth, 1, quantile, prob= 0.025)
newdat$upper<-apply(fitmatboth, 1, quantile, prob= 0.975)



# ACTUAL PLOTTING
par(mfrow=c(1,1))
rgbing <- c(255,255,255)
darkblue <- c(31,120,180)/rgbing
chocolate1 <- c(255,127,36)/rgbing

sexcolours<-c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.45),
              rgb(darkblue[1],darkblue[2],darkblue[3],0.45))

sexcolours.poly<-c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.25),
                   rgb(darkblue[1],darkblue[2],darkblue[3],0.25))

sexcolours.line<-c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.8),
                   rgb(darkblue[1],darkblue[2],darkblue[3],0.8))


tiff("plots/manuscript/status_and_survival.tiff", 
     height=21, width=21,
     units='cm', compression="lzw", res=600)

par(mar=c(6,6,1,1))

plot(rank.and.survival$AliveNextTime, 
     rank.and.survival$StElo, 
     type="n",
     xlab="",
     ylab= "",
     xaxt="n",yaxt="n",xlim=c(-400,600),ylim=c(-0.015,1.01),
     #frame.plot = FALSE,
     family="serif")


axis(1,at=seq(-400,600,by=200),
     las=1,
     cex.axis=1.4,
     #lwd=3,
     #mgp=c(10, 2, 0),
     family="serif") 

axis(2,at=seq(0,1,0.25),
     labels=c("0","0.25","0.5","0.75","1"),
     cex.axis=1.4,
     #line=1,
     las=2,
     #lwd=3,
     family="serif")

title(ylab="survival probability",
      xlab="randomized Elo-rating",
      line=3.75,
      cex.lab=2)

# mtext("low\nstatus",side=1, adj=0, line=2, cex=1,font=3)
# mtext(" high\nstatus",side=1, adj=1, line=2, cex=1,font=3)

rank.and.survival.m <- rank.and.survival[rank.and.survival$sex==1,]
rank.and.survival.f <- rank.and.survival[rank.and.survival$sex==0,]

points(rank.and.survival.m$StElo, 
       #jitter(rank.fit.m.noNA$gen.recruits,0.15), 
       rank.and.survival.m$AliveNextTime-0.012, 
       pch = 19, col=sexcolours[2],
       cex = 1.75)

points(rank.and.survival.f$StElo, 
       #jitter(rank.fit.f.noNA$gen.recruits,0.15)+0.2,
       rank.and.survival.f$AliveNextTime+0.012,
       pch = 19, col=sexcolours[1],       
       cex = 1.75)

index.m<-newdat$sex=="1"

polygon(c(newdat$StElo[index.m],rev(newdat$StElo[index.m])),
        c(newdat$lower[index.m],rev(newdat$upper[index.m])),
        border=NA,col=sexcolours.poly[2])

lines(newdat$StElo[index.m], newdat$fit[index.m], 
      lwd=3.5,col=sexcolours.line[2],lty=3)

index.f<-newdat$sex=="0"

polygon(c(newdat$StElo[index.f],rev(newdat$StElo[index.f])),
        c(newdat$lower[index.f],rev(newdat$upper[index.f])),
        border=NA,col=sexcolours.poly[1])

lines(newdat$StElo[index.f], newdat$fit[index.f], 
      lwd=3.5,col=sexcolours.line[1],lty=3)

# lines(newdat.m$StElo[index.1], newdat.m$fit[index.1], lwd=3.5,
#       col=sexcolours.line[2])      
# 
# lines(newdat.f$StElo[index.2], newdat.f$fit[index.2], lwd=3.5,
#       col=sexcolours.line[1])


op <- par(family = "serif")
#par(op)

legend(350,0.25,
       #legend=c("female = 215 ids","male = 238 ids"),
       legend=c("female","male"),
       pch=19,
       col=sexcolours.line,
       pt.cex=1.75,
       bty='n',
       cex=1.5)

text(380,0.25,"sex:",adj = 0,cex=1.7)

dev.off()





###############################
#generating data for plot: AGE
###############################
newdat<-expand.grid(age=seq(min(rank.and.survival$age,na.rm = TRUE),
                              max(rank.and.survival$age,na.rm = TRUE),
                              0.01), 
                    StElo.z = mean(rank.and.survival$StElo.z,na.rm = TRUE),
                    sex = levels(rank.and.survival$sex))

newdat$age.z <- (newdat$age - mean(rank.and.survival$age,na.rm = TRUE))/sd(rank.and.survival$age,na.rm = TRUE)

xmat<-model.matrix(~StElo.z*age.z+
                     StElo.z*sex+
                     I(age.z^2), 
                   data=newdat) 

fitmatboth <- matrix(NA,
                     ncol = nrow(MCMC.survival$Sol),
                     nrow = nrow(newdat))


for(i in 1:nrow(MCMC.survival$Sol)) {
  fitmatboth[,i] <- plogis(xmat%*%MCMC.survival$Sol[i,c(1:7)]) #plogis because of logit link
}

newdat$fit<-apply(fitmatboth, 1, mean) 
newdat$lower<-apply(fitmatboth, 1, quantile, prob= 0.025)
newdat$upper<-apply(fitmatboth, 1, quantile, prob= 0.975)



# ACTUAL PLOTTING
par(mfrow=c(1,1))
rgbing <- c(255,255,255)
darkblue <- c(31,120,180)/rgbing
chocolate1 <- c(255,127,36)/rgbing

sexcolours<-c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.45),
              rgb(darkblue[1],darkblue[2],darkblue[3],0.45))

sexcolours.poly<-c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.25),
                   rgb(darkblue[1],darkblue[2],darkblue[3],0.25))

sexcolours.line<-c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.8),
                   rgb(darkblue[1],darkblue[2],darkblue[3],0.8))


tiff("plots/manuscript/age_and_survival.tiff", 
     height=21, width=21,
     units='cm', compression="lzw", res=600)

par(mar=c(6,6,1,1))

plot(rank.and.survival$AliveNextTime, 
     rank.and.survival$age, 
     type="n",
     xlab="",
     ylab= "",
     xaxt="n",yaxt="n",xlim=c(0,6.5),ylim=c(-0.015,1.01),
     #frame.plot = FALSE,
     family="serif")


axis(1,at=seq(0,6.5,by=0.5),
     las=1,
     cex.axis=1.4,
     #lwd=3,
     #mgp=c(10, 2, 0),
     family="serif") 

axis(2,at=seq(0,1,0.25),
     labels=c("0","0.25","0.5","0.75","1"),
     cex.axis=1.4,
     #line=1,
     las=2,
     #lwd=3,
     family="serif")

title(ylab="survival probability",
      xlab="age",
      line=3.75,
      cex.lab=2)

# mtext("low\nstatus",side=1, adj=0, line=2, cex=1,font=3)
# mtext(" high\nstatus",side=1, adj=1, line=2, cex=1,font=3)

rank.and.survival.m <- rank.and.survival[rank.and.survival$sex==1,]
rank.and.survival.f <- rank.and.survival[rank.and.survival$sex==0,]

points(jitter(rank.and.survival.m$age,1.25), 
       #jitter(rank.fit.m.noNA$gen.recruits,0.15), 
       rank.and.survival.m$AliveNextTime-0.012, 
       pch = 19, col=sexcolours[2],
       cex = 1.75)

points(jitter(rank.and.survival.f$age,1.25), 
       #jitter(rank.fit.f.noNA$gen.recruits,0.15)+0.2,
       rank.and.survival.f$AliveNextTime+0.012,
       pch = 19, col=sexcolours[1],       
       cex = 1.75)

index.m<-newdat$sex=="1"

polygon(c(newdat$age[index.m],rev(newdat$age[index.m])),
        c(newdat$lower[index.m],rev(newdat$upper[index.m])),
        border=NA,col=sexcolours.poly[2])

lines(newdat$age[index.m], newdat$fit[index.m], 
      lwd=3.5,col=sexcolours.line[2],lty=1)

index.f<-newdat$sex=="0"

polygon(c(newdat$age[index.f],rev(newdat$age[index.f])),
        c(newdat$lower[index.f],rev(newdat$upper[index.f])),
        border=NA,col=sexcolours.poly[1])

lines(newdat$age[index.f], newdat$fit[index.f], 
      lwd=3.5,col=sexcolours.line[1],lty=1)

# lines(newdat.m$StElo[index.1], newdat.m$fit[index.1], lwd=3.5,
#       col=sexcolours.line[2])      
# 
# lines(newdat.f$StElo[index.2], newdat.f$fit[index.2], lwd=3.5,
#       col=sexcolours.line[1])


op <- par(family = "serif")
#par(op)

legend(4.9,0.25,
       #legend=c("female = 215 ids","male = 238 ids"),
       legend=c("female","male"),
       pch=19,
       col=sexcolours.line,
       pt.cex=1.75,
       bty='n',
       cex=1.5)

text(5,0.25,"sex:",adj = 0,cex=1.7)

dev.off()





# ############
# #model GLMER
# ############
# 
# # rank.and.survival$sex <- as.factor(rank.and.survival$sex)
# # 
# # rank.and.survival$StElo.z <- scale(rank.and.survival$StElo)
# # rank.and.survival$age.z <- scale(rank.and.survival$age)
# 
# survive <- glmer(AliveNextTime ~ 
#                    StElo.z*age.z+
#                    StElo.z*sex+
#                    (1|BirdID)+
#                    (1|eventSW),
#                  data=rank.and.survival,
#                  #data=rank.and.survival[rank.and.survival$age>0,],
#                  family = binomial) 
# 
# # 
# # rank.and.survival.1styear <- rank.and.survival[rank.and.survival$age==0.5,]
# # 
# # survive.1styear <- glmer(AliveNextTime ~ 
# #                            scale(StElo)*
# #                            sex+
# #                            (1|eventSW),
# #                          data=rank.and.survival.1styear,
# #                          family = binomial) 
# 
# 
# # #age first year vs rest
# # 
# # rank.and.survival$age2 <- as.factor(ifelse(rank.and.survival$age==0.5,0,1))
# # rank.and.survival.2 <- rank.and.survival[rank.and.survival$age!=0,]
# # 
# # survive <- glmer(AliveNextTime ~ 
# #                    scale(StElo)*age2+
# #                    #scale(age)+
# #                    sex+
# #                    (1|BirdID)+
# #                    (1|eventSW),
# #                  data=rank.and.survival.2,
# #                  family = binomial) 
# 
# # posterior distribution
# 
# ssurvive<-sim(survive,5000)
# 
# #obtaining estimates and 95% CrI
# round(apply(ssurvive@fixef,2, mean),2)
# round(apply(ssurvive@fixef,2, quantile, c(0.025, 0.975)),2)
# 
# round(mean(apply(ssurvive@ranef$BirdID,1, var)),2)
# round(quantile(apply(ssurvive@ranef$BirdID,1, var),
#                c(0.025, 0.975)),2)
# 
# round(mean(apply(ssurvive@ranef$eventSW,1, var)),2)
# round(quantile(apply(ssurvive@ranef$eventSW,1, var),
#                c(0.025, 0.975)),2)
# 
# 
# 
# #########################
# #generating data for plot
# #########################
# newdat<-expand.grid(StElo=seq(min(rank.and.survival$StElo,na.rm = TRUE),
#                                 max(rank.and.survival$StElo,na.rm = TRUE),
#                                 1), 
#                       age.z = mean(rank.and.survival$age.z,na.rm = TRUE),
#                       sex = levels(rank.and.survival$sex))
# 
# newdat$StElo.z <- (newdat$StElo - mean(rank.and.survival$StElo,na.rm = TRUE))/sd(rank.and.survival$StElo,na.rm = TRUE)
# 
# xmat<-model.matrix(~StElo.z*age.z+
#                        StElo.z*sex, 
#                      data=newdat) 
# 
# fitmatboth <- matrix(NA,
#                        ncol = nrow(ssurvive@fixef),
#                        nrow = nrow(newdat))
# 
# 
# for(i in 1:nrow(ssurvive@fixef)) {
#   fitmatboth[,i] <- plogis(xmat%*%ssurvive@fixef[i,]) #exp because of log link
# }
# 
# newdat$fit<-apply(fitmatboth, 1, mean) 
# newdat$lower<-apply(fitmatboth, 1, quantile, prob= 0.025)
# newdat$upper<-apply(fitmatboth, 1, quantile, prob= 0.975)
# 
# 
# 
# # ACTUAL PLOTTING
# par(mfrow=c(1,1))
# rgbing <- c(255,255,255)
# darkblue <- c(31,120,180)/rgbing
# chocolate1 <- c(255,127,36)/rgbing
# 
# sexcolours<-c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.5),
#               rgb(darkblue[1],darkblue[2],darkblue[3],0.45))
# 
# sexcolours.poly<-c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.25),
#                    rgb(darkblue[1],darkblue[2],darkblue[3],0.25))
# 
# sexcolours.line<-c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.75),
#                    rgb(darkblue[1],darkblue[2],darkblue[3],0.75))
# 
# 
# tiff("Figure_Status_and_survival.tiff", height=40, width=40,
#      units='cm', compression="lzw", res=600)
# 
# par(mar=c(10, 10, 0, 2))
# 
# plot(rank.and.survival$AliveNextTime, 
#      rank.and.survival$StElo, 
#      type="n",
#      xlab="",
#      ylab= "",
#      cex.lab=4,
#      xaxt="n",yaxt="n",xlim=c(-600,1000),ylim=c(-0.025,1.025),
#      family="serif",
#      frame.plot = FALSE)
# 
# 
# axis(1,at=seq(-600,1000,by=200),
#      las=1,
#      cex.axis=2.75,
#      lwd=3,
#      mgp=c(10, 2, 0),
#      family="serif") 
# 
# axis(2,at=seq(0,1,0.5),
#      cex.axis=3.25,
#      line=1,
#      las=2,
#      lwd=3,
#      family="serif")
# 
# title(ylab="prob. survival to following season",
#       xlab="dominance score",
#       line=6.5,
#       cex.lab=4.5)
# 
# mtext("low\nstatus",side=1, adj=0, line=8.5, cex=3,font=3)
# mtext(" high\nstatus",side=1, adj=1, line=8.5, cex=3,font=3)
# 
# rank.and.survival.m <- rank.and.survival[rank.and.survival$sex==1,]
# rank.and.survival.f <- rank.and.survival[rank.and.survival$sex==0,]
# 
# points(rank.and.survival.m$StElo, 
#        #jitter(rank.fit.m.noNA$gen.recruits,0.15), 
#        rank.and.survival.m$AliveNextTime-0.01, 
#        pch = 19, col=sexcolours[2],
#        cex = 3.5)
# 
# points(rank.and.survival.f$StElo, 
#        #jitter(rank.fit.f.noNA$gen.recruits,0.15)+0.2,
#        rank.and.survival.f$AliveNextTime+0.01,
#        pch = 19, col=sexcolours[1],       
#        cex = 3.5)
# 
# index.m<-newdat$sex=="1"
# 
# polygon(c(newdat$StElo[index.m],rev(newdat$StElo[index.m])),
#         c(newdat$lower[index.m],rev(newdat$upper[index.m])),
#         border=NA,col=sexcolours.poly[2])
# 
# index.f<-newdat$sex=="0"
# 
# polygon(c(newdat$StElo[index.f],rev(newdat$StElo[index.f])),
#         c(newdat$lower[index.f],rev(newdat$upper[index.f])),
#         border=NA,col=sexcolours.poly[1])
# 
# # lines(newdat.m$StElo[index.1], newdat.m$fit[index.1], lwd=3.5,
# #       col=sexcolours.line[2])      
# # 
# # lines(newdat.f$StElo[index.2], newdat.f$fit[index.2], lwd=3.5,
# #       col=sexcolours.line[1])
# 
# 
# op <- par(family = "serif")
# #par(op)
# 
# legend(400,0.3,
#        legend=c("female = 215","male = 238"),
#        pch=19,
#        col=sexcolours.line,
#        pt.cex=3.25,
#        bty='n',
#        cex=3)
# 
# text(435,0.3,"sex:",adj = 0,cex=3.75)
# 
# dev.off()
