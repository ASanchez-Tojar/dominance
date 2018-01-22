# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 11th of Jan, 2018

############################################################################
# Description of script and Instructions
############################################################################

# This script analyze reproductive success in relation to dominance


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


# #########################################################################################################
# # # 1. Rank and genetic fledglings
# #########################################################################################################
# 
# par(mfrow=c(1,2))
# hist(rank.fit.m$gen.fledg.12d,breaks=30)
# hist(rank.fit.f$gen.fledg.12d,breaks=30)
# par(mfrow=c(1,1))


#########################################################################################################
# # MALES: fledglings
#########################################################################################################

rank.fit.m$StElo.z <- scale(rank.fit.m$StElo)
rank.fit.m$age.z <- scale(rank.fit.m$age)
rank.fit.m$eventSW.z <- scale(rank.fit.m$eventSW)

# prior<-list(R = list(V = diag(1), nu = 0.002),
#             G =list(G1 = list(V = diag(1), nu = 0.002)))
# 
# MCMC.fled.m<-MCMCglmm(gen.fledg.12d~StElo.z*
#                         age.z+
#                         I(age.z^2)+
#                         eventSW.z,
#                       random = ~ BirdID,
#                       data = rank.fit.m,
#                       prior=prior,
#                       verbose= FALSE,
#                       nitt=4000000, burnin=400000,thin=4000,
#                       #nitt=100000, burnin=10000,thin=100,
#                       family="gaussian")
# 
# # save(MCMC.fled.m,
# #      file="survival/models_MCMCglmm/MCMC.fledglings.males.RData")
# save(MCMC.fled.m,
#      file="survival/models_MCMCglmm/MCMC.fledglings.males_age2.RData")


load("survival/models_MCMCglmm/MCMC.fledglings.males_age2.RData")


# plot(MCMC.fled.m)
# summary(MCMC.fled.m)
# 
# #check chain autocorrelation
# autocorr(MCMC.fled.m$Sol)
# autocorr(MCMC.fled.m$VCV[,c(1,2)])
# 
# 
# #estimates and 95% CrI
# for(i in 1:ncol(MCMC.fled.m$Sol)){
#   print(round(mean(MCMC.fled.m$Sol[,i]),2))
# }
# 
# round(HPDinterval(MCMC.fled.m$Sol[,c(1:6)]),2)
# 
# round(mean(MCMC.fled.m$VCV[,1]),2)
# round(mean(MCMC.fled.m$VCV[,2]),2)
# round(HPDinterval(MCMC.fled.m$VCV[,c(1,2)]),2)


#########################
#generating data for plot
#########################

rank.fit.f.m.noNA <- rank.fit.m[!(is.na(rank.fit.m$gen.fledg.12d)),]

new.dat.f.m<-expand.grid(StElo=seq(min(rank.fit.f.m.noNA$StElo,na.rm = TRUE),
                              max(rank.fit.f.m.noNA$StElo,na.rm = TRUE),
                              1),
                    age.z = mean(rank.fit.f.m.noNA$age.z,na.rm = TRUE),
                    eventSW.z = mean(rank.fit.f.m.noNA$eventSW.z,na.rm = TRUE))

new.dat.f.m$StElo.z <- (new.dat.f.m$StElo - mean(rank.fit.f.m.noNA$StElo,na.rm = TRUE))/sd(rank.fit.f.m.noNA$StElo,na.rm = TRUE)

xmat.f.m<-model.matrix(~StElo.z*
                         age.z+
                         I(age.z^2)+
                         eventSW.z,
                       data=new.dat.f.m)

fitmatboth.f.m <- matrix(NA,
                         ncol = nrow(MCMC.fled.m$Sol),
                         nrow = nrow(new.dat.f.m))


for(i in 1:nrow(MCMC.fled.m$Sol)) {
  fitmatboth.f.m[,i] <- xmat.f.m%*%MCMC.fled.m$Sol[i,c(1:6)]
}

new.dat.f.m$fit<-apply(fitmatboth.f.m, 1, mean)
new.dat.f.m$lower<-apply(fitmatboth.f.m, 1, quantile, prob= 0.025)
new.dat.f.m$upper<-apply(fitmatboth.f.m, 1, quantile, prob= 0.975)



#########################################################################################################
# # MALES: recruits
#########################################################################################################

# prior<-list(R = list(V = diag(1), nu = 0.002),
#             G =list(G1 = list(V = diag(1), nu = 0.002)))
# 
# MCMC.rec.m<-MCMCglmm(gen.recruits~StElo.z*
#                         age.z+
#                         I(age.z^2)+
#                         eventSW.z,
#                       random = ~ BirdID,
#                       data = rank.fit.m,
#                       prior=prior,
#                       verbose= FALSE,
#                       nitt=4000000, burnin=400000,thin=4000,
#                       #nitt=100000, burnin=10000,thin=100,
#                       family="poisson")
# 
# # save(MCMC.rec.m,
# #      file="survival/models_MCMCglmm/MCMC.recruits.males.RData")
# save(MCMC.rec.m,
#      file="survival/models_MCMCglmm/MCMC.recruits.males_age2.RData")

load("survival/models_MCMCglmm/MCMC.recruits.males_age2.RData")


# plot(MCMC.rec.m)
# summary(MCMC.rec.m)
# 
# #check chain autocorrelation
# autocorr(MCMC.rec.m$Sol)
# autocorr(MCMC.rec.m$VCV[,c(1,2)])
# 
# 
# #estimates and 95% CrI
# for(i in 1:ncol(MCMC.rec.m$Sol)){
#   print(round(mean(MCMC.rec.m$Sol[,i]),2))
# }
# 
# round(HPDinterval(MCMC.rec.m$Sol[,c(1:6)]),2)
# 
# round(mean(MCMC.rec.m$VCV[,1]),2)
# round(mean(MCMC.rec.m$VCV[,2]),2)
# round(HPDinterval(MCMC.rec.m$VCV[,c(1,2)]),2)


#########################
#generating data for plot
#########################

rank.fit.r.m.noNA <- rank.fit.m[!(is.na(rank.fit.m$gen.recruits)),]

new.dat.r.m<-expand.grid(StElo=seq(min(rank.fit.r.m.noNA$StElo,na.rm = TRUE),
                                   max(rank.fit.r.m.noNA$StElo,na.rm = TRUE),
                                   1),
                         age.z = mean(rank.fit.r.m.noNA$age.z,na.rm = TRUE),
                         eventSW.z = mean(rank.fit.r.m.noNA$eventSW.z,na.rm = TRUE))

new.dat.r.m$StElo.z <- (new.dat.r.m$StElo - mean(rank.fit.r.m.noNA$StElo,na.rm = TRUE))/sd(rank.fit.r.m.noNA$StElo,na.rm = TRUE)

xmat.r.m<-model.matrix(~StElo.z*
                         age.z+
                         I(age.z^2)+
                         eventSW.z,
                       data=new.dat.r.m)

fitmatboth.r.m <- matrix(NA,
                         ncol = nrow(MCMC.rec.m$Sol),
                         nrow = nrow(new.dat.r.m))


for(i in 1:nrow(MCMC.rec.m$Sol)) {
  fitmatboth.r.m[,i] <- exp(xmat.r.m%*%MCMC.rec.m$Sol[i,c(1:6)])
}

new.dat.r.m$fit<-apply(fitmatboth.r.m, 1, mean)
new.dat.r.m$lower<-apply(fitmatboth.r.m, 1, quantile, prob= 0.025)
new.dat.r.m$upper<-apply(fitmatboth.r.m, 1, quantile, prob= 0.975)



#########################################################################################################
# # FEMALES: fledglings
#########################################################################################################

rank.fit.f$StElo.z <- scale(rank.fit.f$StElo)
rank.fit.f$age.z <- scale(rank.fit.f$age)
rank.fit.f$eventSW.z <- scale(rank.fit.f$eventSW)

# prior<-list(R = list(V = diag(1), nu = 0.002),
#             G =list(G1 = list(V = diag(1), nu = 0.002)))
# 
# MCMC.fled.f<-MCMCglmm(gen.fledg.12d~StElo.z*
#                         age.z+
#                         I(age.z^2)+
#                         eventSW.z,
#                       random = ~ BirdID,
#                       data = rank.fit.f,
#                       prior=prior,
#                       verbose= FALSE,
#                       nitt=4000000, burnin=400000,thin=4000,
#                       #nitt=100000, burnin=10000,thin=100,
#                       family="gaussian")
# 
# # save(MCMC.fled.f,
# #      file="survival/models_MCMCglmm/MCMC.fledglings.females.RData")
# save(MCMC.fled.f,
#      file="survival/models_MCMCglmm/MCMC.fledglings.females_age2.RData")

load("survival/models_MCMCglmm/MCMC.fledglings.females_age2.RData")


# plot(MCMC.fled.f)
# summary(MCMC.fled.f)
# 
# #check chain autocorrelation
# autocorr(MCMC.fled.f$Sol)
# autocorr(MCMC.fled.f$VCV[,c(1,2)])
# 
# 
# #estimates and 95% CrI
# for(i in 1:ncol(MCMC.fled.f$Sol)){
#   print(round(mean(MCMC.fled.f$Sol[,i]),2))
# }
# 
# round(HPDinterval(MCMC.fled.f$Sol[,c(1:6)]),2)
# 
# round(mean(MCMC.fled.f$VCV[,1]),2)
# round(mean(MCMC.fled.f$VCV[,2]),2)
# round(HPDinterval(MCMC.fled.f$VCV[,c(1,2)]),2)


#########################
#generating data for plot
#########################

rank.fit.f.f.noNA <- rank.fit.f[!(is.na(rank.fit.f$gen.fledg.12d)),]

new.dat.f.f<-expand.grid(StElo=seq(min(rank.fit.f.f.noNA$StElo,na.rm = TRUE),
                                   max(rank.fit.f.f.noNA$StElo,na.rm = TRUE),
                                   1),
                         age.z = mean(rank.fit.f.f.noNA$age.z,na.rm = TRUE),
                         eventSW.z = mean(rank.fit.f.f.noNA$eventSW.z,na.rm = TRUE))

new.dat.f.f$StElo.z <- (new.dat.f.f$StElo - mean(rank.fit.f.f.noNA$StElo,na.rm = TRUE))/sd(rank.fit.f.f.noNA$StElo,na.rm = TRUE)

xmat.f.f<-model.matrix(~StElo.z*
                         age.z+
                         I(age.z^2)+
                         eventSW.z,
                       data=new.dat.f.f)

fitmatboth.f.f <- matrix(NA,
                         ncol = nrow(MCMC.fled.f$Sol),
                         nrow = nrow(new.dat.f.f))


for(i in 1:nrow(MCMC.fled.f$Sol)) {
  fitmatboth.f.f[,i] <- xmat.f.f%*%MCMC.fled.f$Sol[i,c(1:6)]
}

new.dat.f.f$fit<-apply(fitmatboth.f.f, 1, mean)
new.dat.f.f$lower<-apply(fitmatboth.f.f, 1, quantile, prob= 0.025)
new.dat.f.f$upper<-apply(fitmatboth.f.f, 1, quantile, prob= 0.975)


#########################################################################################################
# # FEMALES: recruits
#########################################################################################################

# prior<-list(R = list(V = diag(1), nu = 0.002),
#             G =list(G1 = list(V = diag(1), nu = 0.002)))
# 
# MCMC.rec.f<-MCMCglmm(gen.recruits~StElo.z*
#                         age.z+
#                         I(age.z^2)+
#                         eventSW.z,
#                       random = ~ BirdID,
#                       data = rank.fit.f,
#                       prior=prior,
#                       verbose= FALSE,
#                       nitt=4000000, burnin=400000,thin=4000,
#                       #nitt=100000, burnin=10000,thin=100,
#                       family="poisson")
# 
# # save(MCMC.rec.f,
# #      file="survival/models_MCMCglmm/MCMC.recruits.females.RData")
# save(MCMC.rec.f,
#      file="survival/models_MCMCglmm/MCMC.recruits.females_age2.RData")

load("survival/models_MCMCglmm/MCMC.recruits.females_age2.RData")


# plot(MCMC.rec.f)
# summary(MCMC.rec.f)
# 
# #check chain autocorrelation
# autocorr(MCMC.rec.f$Sol)
# autocorr(MCMC.rec.f$VCV[,c(1,2)])
# 
# 
# #estimates and 95% CrI
# for(i in 1:ncol(MCMC.rec.f$Sol)){
#   print(round(mean(MCMC.rec.f$Sol[,i]),2))
# }
# 
# round(HPDinterval(MCMC.rec.f$Sol[,c(1:6)]),2)
# 
# round(mean(MCMC.rec.f$VCV[,1]),2)
# round(mean(MCMC.rec.f$VCV[,2]),2)
# round(HPDinterval(MCMC.rec.f$VCV[,c(1,2)]),2)


#########################
#generating data for plot
#########################

rank.fit.r.f.noNA <- rank.fit.f[!(is.na(rank.fit.f$gen.recruits)),]

new.dat.r.f<-expand.grid(StElo=seq(min(rank.fit.r.f.noNA$StElo,na.rm = TRUE),
                                   max(rank.fit.r.f.noNA$StElo,na.rm = TRUE),
                                   1),
                         age.z = mean(rank.fit.r.f.noNA$age.z,na.rm = TRUE),
                         eventSW.z = mean(rank.fit.r.f.noNA$eventSW.z,na.rm = TRUE))

new.dat.r.f$StElo.z <- (new.dat.r.f$StElo - mean(rank.fit.r.f.noNA$StElo,na.rm = TRUE))/sd(rank.fit.r.f.noNA$StElo,na.rm = TRUE)

xmat.r.f<-model.matrix(~StElo.z*
                         age.z+
                         I(age.z^2)+
                         eventSW.z,
                       data=new.dat.r.f)

fitmatboth.r.f <- matrix(NA,
                         ncol = nrow(MCMC.rec.f$Sol),
                         nrow = nrow(new.dat.r.f))


for(i in 1:nrow(MCMC.rec.f$Sol)) {
  fitmatboth.r.f[,i] <- exp(xmat.r.f%*%MCMC.rec.f$Sol[i,c(1:6)])
}

new.dat.r.f$fit<-apply(fitmatboth.r.f, 1, mean)
new.dat.r.f$lower<-apply(fitmatboth.r.f, 1, quantile, prob= 0.025)
new.dat.r.f$upper<-apply(fitmatboth.r.f, 1, quantile, prob= 0.975)




#########################################################################################################
# # MULTI-PANNEL PLOT
#########################################################################################################

rgbing <- c(255,255,255)
darkblue <- c(31,120,180)/rgbing
chocolate1 <- c(255,127,36)/rgbing

sexcolours<-c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.45),
              rgb(darkblue[1],darkblue[2],darkblue[3],0.45))

sexcolours.poly<-c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.25),
                   rgb(darkblue[1],darkblue[2],darkblue[3],0.25))

sexcolours.line<-c(rgb(chocolate1[1],chocolate1[2],chocolate1[3],0.8),
                   rgb(darkblue[1],darkblue[2],darkblue[3],0.8))


tiff("plots/manuscript/status_and_reproductive_success.tiff", 
     height=30, width=30,
     units='cm', compression="lzw", res=600)

m <- rbind(c(1,2),
           c(3,4))


layout(m)

op <- par(oma = c(6,3,4,1) + 0.1,
          mar = c(1.5,3,1,0) + 0.1)


######
# males fledglings
######

plot(rank.fit.m$gen.fledg.12d, 
     rank.fit.m$StElo, 
     type="n",
     xlab="",
     ylab= "",
     xaxt="n",yaxt="n",xlim=c(-400,400),ylim=c(0,20),
     family="serif")

axis(1,at=seq(-400,400,by=200),
     las=1,
     cex.axis=1.5,
     family="serif") 

axis(2,at=seq(0,20,2),
     cex.axis=1.5,
     las=2,
     family="serif")

# title(ylab="annual number of genetic fledglings",
#       line=3.75,
#       outer=TRUE,
#       cex.lab=2)


mtext("      genetic fledglings",
      side=2, adj=0, line=3, cex=2.5)

mtext("males             ",
      side=3, adj=1, line=1, cex=3)

points(rank.fit.m$StElo, 
       rank.fit.m$gen.fledg.12d, 
       pch = 19, col=sexcolours[2],
       cex = 1.85)


polygon(c(new.dat.f.m$StElo,rev(new.dat.f.m$StElo)),
        c(new.dat.f.m$lower,rev(new.dat.f.m$upper)),
        border=NA,col=sexcolours.poly[2])

lines(new.dat.f.m$StElo, new.dat.f.m$fit, 
      lwd=3.5,col=sexcolours.line[2],lty=3)



######
# female fledglings
######

plot(rank.fit.f$gen.fledg.12d, 
     rank.fit.f$StElo, 
     type="n",
     xlab="",
     ylab= "",
     xaxt="n",yaxt="n",xlim=c(-400,400),ylim=c(0,20),
     family="serif")

axis(1,at=seq(-400,400,by=200),
     las=1,
     cex.axis=1.5,
     family="serif") 

axis(2,at=seq(0,20,2),
     cex.axis=1.5,
     las=2,
     family="serif")

mtext("females            ",
      side=3, adj=1, line=1, cex=3)

points(rank.fit.f$StElo, 
       rank.fit.f$gen.fledg.12d, 
       pch = 19, col=sexcolours[1],
       cex = 1.85)

polygon(c(new.dat.f.f$StElo,rev(new.dat.f.f$StElo)),
        c(new.dat.f.f$lower,rev(new.dat.f.f$upper)),
        border=NA,col=sexcolours.poly[1])

lines(new.dat.f.f$StElo, new.dat.f.f$fit, 
      lwd=3.5,col=sexcolours.line[1],lty=3)



######
# males recruits
######

plot(rank.fit.m$gen.recruits, 
     rank.fit.m$StElo, 
     type="n",
     xlab="",
     ylab= "",
     xaxt="n",yaxt="n",xlim=c(-400,400),ylim=c(0,6),
     family="serif")

axis(1,at=seq(-400,400,by=200),
     las=1,
     cex.axis=1.5,
     family="serif") 

axis(2,at=seq(0,6,1),
     cex.axis=1.5,
     las=2,
     family="serif")

mtext("        genetic recruits",
      side=2, adj=0, line=3, cex=2.5)


points(rank.fit.m$StElo, 
       rank.fit.m$gen.recruits, 
       pch = 19, col=sexcolours[2],
       cex = 1.85)


polygon(c(new.dat.r.m$StElo,rev(new.dat.r.m$StElo)),
        c(new.dat.r.m$lower,rev(new.dat.r.m$upper)),
        border=NA,col=sexcolours.poly[2])

lines(new.dat.r.m$StElo, new.dat.r.m$fit, 
      lwd=3.5,col=sexcolours.line[2],lty=3)



######
# female recruits
######

plot(rank.fit.f$gen.recruits, 
     rank.fit.f$StElo, 
     type="n",
     xlab="",
     ylab= "",
     xaxt="n",yaxt="n",xlim=c(-400,400),ylim=c(0,6),
     family="serif")

axis(1,at=seq(-400,400,by=200),
     las=1,
     cex.axis=1.5,
     family="serif") 

axis(2,at=seq(0,6,1),
     cex.axis=1.5,
     las=2,
     family="serif")

points(rank.fit.f$StElo, 
       rank.fit.f$gen.recruits, 
       pch = 19, col=sexcolours[1],
       cex = 1.85)

polygon(c(new.dat.r.f$StElo,rev(new.dat.r.f$StElo)),
        c(new.dat.r.f$lower,rev(new.dat.r.f$upper)),
        border=NA,col=sexcolours.poly[1])

lines(new.dat.r.f$StElo, new.dat.r.f$fit, 
      lwd=3.5,col=sexcolours.line[1],lty=3)


title(xlab = "randomized Elo-rating",
      outer = TRUE, cex.lab=3.5)


dev.off()