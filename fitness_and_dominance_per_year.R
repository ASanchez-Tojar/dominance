# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 16th of Jan, 2018

############################################################################
# Description of script and Instructions
############################################################################

# This script analyze reproductive success in relation to dominance for each
# year separately.


############################################################################
# Packages needed
############################################################################

# packages needed

library(MCMCglmm)

#sessionInfo()


# Clear memory
rm(list=ls())


rank.fit <- read.table("finaldatabases/rank.TLandM.VB.fitness_sim.csv",
                       header=TRUE,sep=",")

rank.fit.m <- rank.fit[rank.fit$sex==1 &
                         !(is.na(rank.fit$sex)),]

rank.fit.f <- rank.fit[rank.fit$sex==0 &
                         !(is.na(rank.fit$sex)),]



#########################################################################################################
# # MALES: fledglings
#########################################################################################################

rank.fit.m.noNA <- rank.fit.m[!(is.na(rank.fit.m$gen.fledg.12d)),]

rank.fit.m.noNA$eventSW <- as.factor(rank.fit.m.noNA$eventSW)

prior<-list(R = list(V = diag(1), nu = 0.002))#,
            #G =list(G1 = list(V = diag(1), nu = 0.002)))

db.overall <- data.frame(test=factor(),
                         year=integer(),
                         sex=integer(),
                         estimate=numeric(),
                         lower=numeric(),
                         upper=numeric(),
                         N=integer(),
                         stringsAsFactors=FALSE)

db.age.int <- data.frame(test=factor(),
                         year=numeric(),
                         estimate=numeric(),
                         lower=numeric(),
                         upper=numeric(),
                         N=integer(),
                         stringsAsFactors=FALSE)

for(year in levels(rank.fit.m.noNA$eventSW)){
  
  data.year <- rank.fit.m.noNA[rank.fit.m.noNA$eventSW==year,]
  
  data.year$StElo.z <- scale(data.year$StElo)
  data.year$age.z <- scale(data.year$age)
  
  MCMC.fled.m<-MCMCglmm(gen.fledg.12d~StElo.z*
                          age.z+
                          I(age.z^2),
                        data = data.year,
                        prior=prior,
                        verbose= FALSE,
                        nitt=1000000, burnin=100000,thin=1000,
                        family="gaussian")
  
  estimate <- round(mean(MCMC.fled.m$Sol[,2]),2)
  HPD <- round(HPDinterval(MCMC.fled.m$Sol[,2]),2)
  year <- as.numeric(as.character(data.year$eventSW[1]))

  db.overall<-rbind(db.overall,c(1,year,1,estimate,HPD[1],HPD[2],nrow(data.year)))

  estimate.age <- round(mean(MCMC.fled.m$Sol[,5]),2)
  HPD.age <- round(HPDinterval(MCMC.fled.m$Sol[,5]),2)

  db.age.int<-rbind(db.age.int,c(1,year,1,estimate.age,HPD.age[1],HPD.age[2],nrow(data.year)))
  
}

# plot(MCMC.fled.m)
# summary(MCMC.fled.m)
# 
# #check chain autocorrelation
# autocorr(MCMC.fled.m$Sol)


#########################################################################################################
# # FEMALES: fledglings
#########################################################################################################

rank.fit.f.noNA <- rank.fit.f[!(is.na(rank.fit.f$gen.fledg.12d)),]

rank.fit.f.noNA$StElo.z <- scale(rank.fit.f.noNA$StElo)
rank.fit.f.noNA$age.z <- scale(rank.fit.f.noNA$age)
rank.fit.f.noNA$eventSW <- as.factor(rank.fit.f.noNA$eventSW)

for(year in levels(rank.fit.f.noNA$eventSW)){
  
  data.year <- rank.fit.f.noNA[rank.fit.f.noNA$eventSW==year,]
  
  data.year$StElo.z <- scale(data.year$StElo)
  data.year$age.z <- scale(data.year$age)
  
  MCMC.fled.f<-MCMCglmm(gen.fledg.12d~StElo.z*
                          age.z+
                          I(age.z^2),
                        data = data.year,
                        prior=prior,
                        verbose= FALSE,
                        nitt=1000000, burnin=100000,thin=1000,
                        family="gaussian")
  
  estimate <- round(mean(MCMC.fled.f$Sol[,2]),2)
  HPD <- round(HPDinterval(MCMC.fled.f$Sol[,2]),2)
  year <- as.numeric(as.character(data.year$eventSW[1]))
  
  db.overall<-rbind(db.overall,c(2,year,0,estimate,HPD[1],HPD[2],nrow(data.year)))
  
  estimate.age <- round(mean(MCMC.fled.f$Sol[,5]),2)
  HPD.age <- round(HPDinterval(MCMC.fled.f$Sol[,5]),2)
  
  db.age.int<-rbind(db.age.int,c(2,year,0,estimate.age,HPD.age[1],HPD.age[2],nrow(data.year)))
  
}

# plot(MCMC.fled.m)
# summary(MCMC.fled.m)
# 
# #check chain autocorrelation
# autocorr(MCMC.fled.m$Sol)


#########################################################################################################
# # MALES: recruits
#########################################################################################################


rank.fit.m.noNA <- rank.fit.m[!(is.na(rank.fit.m$gen.recruits)),]

rank.fit.m.noNA$StElo.z <- scale(rank.fit.m.noNA$StElo)
rank.fit.m.noNA$age.z <- scale(rank.fit.m.noNA$age)
rank.fit.m.noNA$eventSW <- as.factor(rank.fit.m.noNA$eventSW)

for(year in levels(rank.fit.m.noNA$eventSW)){
  
  data.year <- rank.fit.m.noNA[rank.fit.m.noNA$eventSW==year,]
  
  data.year$StElo.z <- scale(data.year$StElo)
  data.year$age.z <- scale(data.year$age)
  
  MCMC.rec.m<-MCMCglmm(gen.recruits~StElo.z*
                         age.z+
                         I(age.z^2),
                       data = data.year,
                       prior=prior,
                       verbose= FALSE,
                       nitt=1000000, burnin=100000,thin=1000,
                       family="poisson")
  
  estimate <- round(mean(MCMC.rec.m$Sol[,2]),2)
  HPD <- round(HPDinterval(MCMC.rec.m$Sol[,2]),2)
  year <- as.numeric(as.character(data.year$eventSW[1]))
  
  db.overall<-rbind(db.overall,c(3,year,1,estimate,HPD[1],HPD[2],nrow(data.year)))
  
  estimate.age <- round(mean(MCMC.rec.m$Sol[,5]),2)
  HPD.age <- round(HPDinterval(MCMC.rec.m$Sol[,5]),2)
  
  db.age.int<-rbind(db.age.int,c(3,year,1,estimate.age,HPD.age[1],HPD.age[2],nrow(data.year)))
  
}



#########################################################################################################
# # FEMALES: recruits
#########################################################################################################


rank.fit.f.noNA <- rank.fit.f[!(is.na(rank.fit.f$gen.recruits)),]

rank.fit.f.noNA$StElo.z <- scale(rank.fit.f.noNA$StElo)
rank.fit.f.noNA$age.z <- scale(rank.fit.f.noNA$age)
rank.fit.f.noNA$eventSW <- as.factor(rank.fit.f.noNA$eventSW)

for(year in levels(rank.fit.f.noNA$eventSW)){
  
  data.year <- rank.fit.f.noNA[rank.fit.f.noNA$eventSW==year,]
  
  data.year$StElo.z <- scale(data.year$StElo)
  data.year$age.z <- scale(data.year$age)
  
  MCMC.rec.f<-MCMCglmm(gen.recruits~StElo.z*
                          age.z+
                          I(age.z^2),
                        data = data.year,
                        prior=prior,
                        verbose= FALSE,
                        nitt=1000000, burnin=100000,thin=1000,
                        family="poisson")
  
  estimate <- round(mean(MCMC.rec.f$Sol[,2]),2)
  HPD <- round(HPDinterval(MCMC.rec.f$Sol[,2]),2)
  year <- as.numeric(as.character(data.year$eventSW[1]))
  
  db.overall<-rbind(db.overall,c(4,year,0,estimate,HPD[1],HPD[2],nrow(data.year)))
  
  estimate.age <- round(mean(MCMC.rec.f$Sol[,5]),2)
  HPD.age <- round(HPDinterval(MCMC.rec.f$Sol[,5]),2)
  
  db.age.int<-rbind(db.age.int,c(4,year,0,estimate.age,HPD.age[1],HPD.age[2],nrow(data.year)))
  
}


names(db.overall) <- c("test","year","sex","estimate","lower","upper","N")
names(db.age.int) <- c("test","year","sex","estimate","lower","upper","N")



########################
# PLOT
########################
rgbing <- c(255,255,255)
darkblue <- c(31,120,180)/rgbing
chocolate1 <- c(255,127,36)/rgbing

# db.overall$year.plus <- ifelse(db.overall$test==1,
#                                db.overall$year-0.3,
#                                ifelse(db.overall$test==2,
#                                       db.overall$year-0.1,
#                                       ifelse(db.overall$test==3,
#                                              db.overall$year+0.1,db.overall$year+0.3)))

db.overall$year.plus <- ifelse(db.overall$test==1,
                               db.overall$year-0.15,
                               ifelse(db.overall$test==2,
                                      db.overall$year+0.15,
                                      ifelse(db.overall$test==3,
                                             db.overall$year-0.15,db.overall$year+0.15)))

db.overall$col.sex <- ifelse(db.overall$sex==0,
                             rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.9),
                             rgb(darkblue[1],darkblue[2],darkblue[3],0.9))

fledglings <- db.overall[db.overall$test==1 |
                           db.overall$test==2,]

recruits <- db.overall[db.overall$test==3 |
                         db.overall$test==4,]

tiff("plots/supplements/status_and_reproductive_success_per_year.tiff", 
     height=15, width=30,
     units='cm', compression="lzw", res=600)


m <- rbind(c(1,2))

layout(m)

op <- par(oma = c(3,4,0.5,0.5) + 0.1,
          mar = c(1.5,3,1,0) + 0.1)

plot(fledglings$estimate, 
     fledglings$test, 
     type="n",
     xlab="",
     ylab= "",
     xaxt="n",yaxt="n",xlim=c(2013.5,2016.5),ylim=c(-2,2.5),
     family="serif")

axis(1,at=seq(2014,2016,by=1),
     las=1,
     cex.axis=1.6,
     #labels=FALSE,
     family="serif") 

axis(2,at=seq(-2,2.5,0.5),
     cex.axis=1.4,
     las=2,
     family="serif")

points(fledglings$year.plus,
       fledglings$estimate,
       pch = 19, col=fledglings$col.sex,       
       cex = 2.5)

lines(c(2013,2017), c(0,0), 
      lwd=1,col="black",lty=1)

arrows(fledglings$year.plus,
       fledglings$lower,
       fledglings$year.plus,
       fledglings$upper,
       angle=90,code=3,col=fledglings$col.sex,
       length = 0,lwd=4)

text(2015,2.4,"fledglings",cex=1.75)

text(fledglings$year.plus,
     fledglings$lower-0.15,
     as.character(fledglings$N),cex=0.7)


plot(recruits$estimate, 
     recruits$test, 
     type="n",
     xlab="",
     ylab= "",
     xaxt="n",yaxt="n",xlim=c(2013.5,2016.5),ylim=c(-2,2.5),
     family="serif")

axis(1,at=seq(2014,2016,by=1),
     las=1,
     cex.axis=1.6,
     #labels=FALSE,
     family="serif") 

axis(2,at=seq(-2,2.5,0.5),
     cex.axis=1.4,
     las=2,
     family="serif")

points(recruits$year.plus,
       recruits$estimate,
       pch = 19, col=recruits$col.sex,       
       cex = 2.5)

lines(c(2013,2017), c(0,0), 
      lwd=1,col="black",lty=1)

arrows(recruits$year.plus,
       recruits$lower,
       recruits$year.plus,
       recruits$upper,
       angle=90,code=3,col=recruits$col.sex,
       length = 0,lwd=4)

text(2015,2.4,"recruits",cex=1.75)

text(recruits$year.plus,
     recruits$lower-0.15,
     as.character(recruits$N),cex=0.7)

op <- par(family = "serif")
#par(op)

legend(2015.85,-1.3,
       legend=c("female","male"),
       pch=19,
       col=c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.9),
             rgb(darkblue[1],darkblue[2],darkblue[3],0.9)),
       pt.cex=1.4,
       bty='n',
       cex=1.25)

text(2015.85,-1.3,"sex:",adj = 0,cex=1.4)


title(ylab="rand. Elo-rating estimate",
      xlab = "breeding season",
      outer = TRUE, 
      cex.lab=2.5,
      line=1.5)

dev.off()




#########################################################################################################
# SURVIVAL
#########################################################################################################

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
rank.fit.reduced <- rank.fit[,c("BirdID","BirdID_eventSW","StElo","eventSW",
                                "cohort","age","elo.z.event","sex")]


# add survival data to dominance data

rank.and.survival <- merge(rank.fit.reduced,survival.final.reduced,
                           by="BirdID_eventSW",all.x=TRUE)

#removing BirdID_eventSW
rank.and.survival <- rank.and.survival[order(rank.and.survival$BirdID,
                                             rank.and.survival$eventSW),c(-1)]


rank.and.survival$eventSW <- as.factor(rank.and.survival$eventSW)


rank.and.survival.f <- rank.and.survival[rank.and.survival$sex==0,]
rank.and.survival.m <- rank.and.survival[rank.and.survival$sex==1,]


# ###############
# #model MALES
# ###############
# 
# 
# prior<-list(R = list(V = diag(1), nu = 0.002))
# 
# db.2.overall <- data.frame(year=integer(),
#                            sex=integer(),
#                            estimate=numeric(),
#                            lower=numeric(),
#                            upper=numeric(),
#                            N=integer(),
#                            mean.survival=numeric(),
#                            stringsAsFactors=FALSE)
# 
# db.2.age.int <- data.frame(year=numeric(),
#                            estimate=numeric(),
#                            lower=numeric(),
#                            upper=numeric(),
#                            N=integer(),
#                            mean.survival=numeric(),
#                            stringsAsFactors=FALSE)
# 
# 
# for(year in levels(rank.and.survival.m$eventSW)){
#   
#   data.year <- rank.and.survival.m[rank.and.survival.m$eventSW==year,]
#   
#   data.year$StElo.z <- scale(data.year$StElo)
#   data.year$age.z <- scale(data.year$age)
#   
#   MCMC.survival.m<-MCMCglmm(AliveNextTime~StElo.z*
#                               age.z+
#                               I(age.z^2),
#                             data = data.year,
#                             prior=prior,
#                             verbose= FALSE,
#                             nitt=1000000, burnin=100000,thin=1000,
#                             family="categorical")
#                          
#   
#   estimate <- round(mean(MCMC.survival.m$Sol[,2]),2)
#   HPD <- round(HPDinterval(MCMC.survival.m$Sol[,2]),2)
#   year <- as.numeric(as.character(data.year$eventSW[1]))
#   
#   db.2.overall<-rbind(db.2.overall,c(year,1,estimate,HPD[1],HPD[2],nrow(data.year),mean(data.year$AliveNextTime)))
#   
#   estimate.age <- round(mean(MCMC.survival.m$Sol[,5]),2)
#   HPD.age <- round(HPDinterval(MCMC.survival.m$Sol[,5]),2)
#   
#   db.2.age.int<-rbind(db.2.age.int,c(year,1,estimate.age,HPD.age[1],HPD.age[2],nrow(data.year),mean(data.year$AliveNextTime)))
#   
# }
# 
# 
# ###############
# #model FEMALES
# ###############
# 
# prior<-list(R = list(V = diag(1), nu = 0.002))
# 
# for(year in levels(rank.and.survival.f$eventSW)){
#   
#   data.year <- rank.and.survival.f[rank.and.survival.f$eventSW==year,]
#   
#   data.year$StElo.z <- scale(data.year$StElo)
#   data.year$age.z <- scale(data.year$age)
#   
#   MCMC.survival.f<-MCMCglmm(AliveNextTime~StElo.z*
#                               age.z+
#                               I(age.z^2),
#                             data = data.year,
#                             prior=prior,
#                             verbose= FALSE,
#                             nitt=1000000, burnin=100000,thin=1000,
#                             family="categorical")
#   
#   
#   estimate <- round(mean(MCMC.survival.f$Sol[,2]),2)
#   HPD <- round(HPDinterval(MCMC.survival.f$Sol[,2]),2)
#   year <- as.numeric(as.character(data.year$eventSW[1]))
#   
#   db.2.overall<-rbind(db.2.overall,c(year,0,estimate,HPD[1],HPD[2],nrow(data.year),mean(data.year$AliveNextTime)))
#   
#   estimate.age <- round(mean(MCMC.survival.f$Sol[,5]),2)
#   HPD.age <- round(HPDinterval(MCMC.survival.f$Sol[,5]),2)
#   
#   db.2.age.int<-rbind(db.2.age.int,c(year,0,estimate.age,HPD.age[1],HPD.age[2],nrow(data.year),mean(data.year$AliveNextTime)))
#   
# }
# 
# 
# names(db.2.overall) <- c("year","sex","estimate","lower","upper","N","mean.survival")
# names(db.2.age.int) <- c("year","sex","estimate","lower","upper","N","mean.survival")



###############
#model MALES and FEMALES
###############

db.2.overall <- data.frame(year=integer(),
                           sex=integer(),
                           estimate=numeric(),
                           lower=numeric(),
                           upper=numeric(),
                           N=integer(),
                           mean.survival=numeric(),
                           stringsAsFactors=FALSE)

db.2.age.int <- data.frame(year=numeric(),
                           estimate=numeric(),
                           lower=numeric(),
                           upper=numeric(),
                           N=integer(),
                           mean.survival=numeric(),
                           stringsAsFactors=FALSE)

db.2.sex.int <- data.frame(year=numeric(),
                           estimate=numeric(),
                           lower=numeric(),
                           upper=numeric(),
                           N=integer(),
                           mean.survival=numeric(),
                           stringsAsFactors=FALSE)

prior<-list(R = list(V = diag(1), nu = 0.002))

for(year in levels(rank.and.survival$eventSW)){
#for(year in levels(as.factor(c("2013.5")))){
    
  data.year <- rank.and.survival[rank.and.survival$eventSW==year,]
  
  data.year$StElo.z <- scale(data.year$StElo)
  data.year$age.z <- scale(data.year$age)
  data.year$sex <- as.factor(data.year$sex)
  
  MCMC.survival.both<-MCMCglmm(AliveNextTime~StElo.z*
                              age.z+
                              I(age.z^2)+
                            StElo.z*sex,
                            data = data.year,
                            prior=prior,
                            verbose= FALSE,
                            nitt=1000000, burnin=100000,thin=1000,
                            family="categorical")
  
  
  estimate <- round(mean(MCMC.survival.both$Sol[,2]),2)
  HPD <- round(HPDinterval(MCMC.survival.both$Sol[,2]),2)
  year <- as.numeric(as.character(data.year$eventSW[1]))

  db.2.overall<-rbind(db.2.overall,c(year,2,estimate,HPD[1],HPD[2],nrow(data.year),mean(data.year$AliveNextTime)))

  estimate.age <- round(mean(MCMC.survival.both$Sol[,6]),2)
  HPD.age <- round(HPDinterval(MCMC.survival.both$Sol[,6]),2)

  db.2.age.int<-rbind(db.2.age.int,c(year,2,estimate.age,HPD.age[1],HPD.age[2],nrow(data.year),mean(data.year$AliveNextTime)))
  
  estimate.sex <- round(mean(MCMC.survival.both$Sol[,7]),2)
  HPD.sex <- round(HPDinterval(MCMC.survival.both$Sol[,7]),2)
  
  db.2.sex.int<-rbind(db.2.sex.int,c(year,2,estimate.sex,HPD.sex[1],HPD.sex[2],nrow(data.year),mean(data.year$AliveNextTime)))
  
}

names(db.2.overall) <- c("year","sex","estimate","lower","upper","N","mean.survival")
names(db.2.age.int) <- c("year","sex","estimate","lower","upper","N","mean.survival")
names(db.2.sex.int) <- c("year","sex","estimate","lower","upper","N","mean.survival")



########################
# PLOT
########################
rgbing <- c(255,255,255)
darkblue <- c(31,120,180)/rgbing
chocolate1 <- c(255,127,36)/rgbing

# db.overall$year.plus <- ifelse(db.overall$test==1,
#                                db.overall$year-0.3,
#                                ifelse(db.overall$test==2,
#                                       db.overall$year-0.1,
#                                       ifelse(db.overall$test==3,
#                                              db.overall$year+0.1,db.overall$year+0.3)))

db.overall$year.plus <- ifelse(db.overall$test==1,
                               db.overall$year-0.15,
                               ifelse(db.overall$test==2,
                                      db.overall$year+0.15,
                                      ifelse(db.overall$test==3,
                                             db.overall$year-0.15,db.overall$year+0.15)))

db.overall$col.sex <- ifelse(db.overall$sex==0,
                             rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.9),
                             rgb(darkblue[1],darkblue[2],darkblue[3],0.9))


tiff("plots/supplements/status_and_survival_per_year.tiff", 
     height=15, width=21,
     units='cm', compression="lzw", res=600)


par(mar=c(6,6,1,1))

plot(db.2.overall$estimate, 
     db.2.overall$year, 
     type="n",
     xlab="",
     ylab= "",
     xaxt="n",yaxt="n",xlim=c(2013.3,2016.7),ylim=c(-140,180),
     family="serif")

axis(1,at=seq(2013.5,2016.5,by=0.5),
     las=1,
     cex.axis=1.3,
     #labels=FALSE,
     family="serif") 

axis(2,at=seq(-140,170,70),
     cex.axis=1.3,
     las=2,
     family="serif")

points(db.2.overall$year,
       db.2.overall$estimate,
       pch = 19, col="black",       
       cex = 2.5)

lines(c(2012,2018), c(0,0), 
      lwd=1,col="black",lty=1)

arrows(db.2.overall$year,
       db.2.overall$lower,
       db.2.overall$year,
       db.2.overall$upper,
       angle=90,code=3,col="black",
       length = 0,lwd=4)

text(db.2.overall$year,
     db.2.overall$lower-10,
     as.character(db.2.overall$N),cex=0.7)

title(ylab="rand. Elo-rating estimate",
      xlab = "event",
      #outer = TRUE, 
      cex.lab=2.25,
      line=4)

dev.off()