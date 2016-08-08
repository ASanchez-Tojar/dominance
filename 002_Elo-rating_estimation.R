
# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Acknowledgements: great part of this code is modified from what Moises Sanchez-Fortun, a master
# student of as did for his master thesis. 

# References: Neumann et al. 2011. Assessing dominance hierarchies: validation and advantages of
# progressive evaluation with Elo-rating. Animal Behaviour, 82(4): 911-921

# Script created on the 3rd of March, 2016
# Script last updated on the 8th of August, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to estimate elo-ratings per individual. This script needs data from the previous script
# (001_Dominance_databases_cleaning_and_summary).
#
# The section 0:


########################################################################################################
# Packages needed
########################################################################################################

# packages needed to be loaded for this script (a couple of them might be only needed in the following
# script)

library(EloRating)
library(statnet)
library(rptR)
library(ggplot2)
library(doBy)
library(lme4)
library(blmeco)
library(arm)

# Clear memory and get to know where you are
rm(list=ls())
#getwd()


########################################################################################################
# 9.A. Estimating ELO-RANKS per winter/summer
########################################################################################################

# This repeats the sections 9.B. and 9.C. (below) but for each of the events. 
# Some explanations might be given below.


########################################################################################################
# # 9.A.2. Obtaining the elo-scores for each individual
########################################################################################################

# First, spliting the database by event


#     Creating a different database per event with this for loop, and at the same time, creating a 
# database per event with elo_scores

dom.final.v2$eventSW <- as.factor(dom.final.v2$eventSW)

counter <- 1

for(i in levels(dom.final.v2$eventSW)){
  
  x<-subset(dom.final.v2, dom.final.v2$eventSW==i)
  assign(paste0("dom.eventSW",counter),x)
  y<-elo.seq(winner=x$Winner, 
             loser=x$Loser, 
             Date=x$date.ELO2.2, 
             draw=x$Draw)
  print(summary(y))
  assign(paste0("elo_scores.",counter),y)
  counter <- counter + 1
}


########################################################################################################
# # 9.A.3. Stability coefficient
########################################################################################################

stab.elo(elo_scores.1)
stab.elo(elo_scores.2)
stab.elo(elo_scores.3)
stab.elo(elo_scores.4)
stab.elo(elo_scores.5)
stab.elo(elo_scores.6)


########################################################################################################
# # 9.A.4. Proportion of unknown dyads
########################################################################################################

prunk(elo_scores.1)
prunk(elo_scores.2)
prunk(elo_scores.3)
prunk(elo_scores.4)
prunk(elo_scores.5)
prunk(elo_scores.6)


########################################################################################################
# # 9.A.5. Extracting elo-ratings per individual
########################################################################################################

# this can be probably included in the for loop of line 2758

elo_scores_ind.1 <- extract.elo(elo_scores.1,standardize = TRUE)
elo_scores_ind.2 <- extract.elo(elo_scores.2,standardize = TRUE)
elo_scores_ind.3 <- extract.elo(elo_scores.3,standardize = TRUE)
elo_scores_ind.4 <- extract.elo(elo_scores.4,standardize = TRUE)
elo_scores_ind.5 <- extract.elo(elo_scores.5,standardize = TRUE)
elo_scores_ind.6 <- extract.elo(elo_scores.6,standardize = TRUE)


#     This code is to make a dataframe out of those individuals scores, please, change the number for the
# event you want!

# creating the name of the rows, basically from 1 to total number of individuals


# Creating a database with elo_scores per event

counter <- 1

for(i in levels(dom.final.v2$eventSW)){
  
  x<-subset(dom.final.v2, dom.final.v2$eventSW==i)
  y<-elo.seq(winner=x$Winner, 
             loser=x$Loser, 
             Date=x$date.ELO2.2, 
             draw=x$Draw)
  
  w<-extract.elo(y,standardize = TRUE)
  
  rownames <- seq(1,length(w),
                  1)
  
  # making a data.frame with the elo-ratings
  scores <- as.data.frame(w,
                          row.names = as.character(rownames))
  
  z <- cbind(attributes(w),
             scores)
  
  z$eventSW <- counter
  
  names(z) <- c("individual","StElo","eventSW")
  
  assign(paste0("elo_scores_ind.db.",counter),z)
  
  counter <- counter + 1
  
}


# creating a database with all these observations
elo_scores_all_events <- rbind(elo_scores_ind.db.1,
                               elo_scores_ind.db.2,
                               elo_scores_ind.db.3,
                               elo_scores_ind.db.4,
                               elo_scores_ind.db.5,
                               elo_scores_ind.db.6)

# I'm saving this file so that I don't have to run it again unless new data is added

write.csv(elo_scores_all_events,"elo_scores_all_events.csv",row.names=FALSE)


########################################################################################################
# # 9.A.5.1. Estimating repeatability of StElo-rank using Shinichi's package
########################################################################################################

# Estimating repeatability of dominance rank. I'm using MCMC method, see above for ANOVA and REML.

rpt.St.MCMC <- rpt(elo_scores_all_events$StElo,
                   elo_scores_all_events$individual,
                   datatype="Gaussian",
                   method="MCMC",
                   nboot=1000,
                   npermut=1000)


# saving it as a csv file
# write.csv(as.data.frame(EloR1.all.ext.scores),file="StElos2015.csv",sep=",",na="",dec = ".")


########################################################################################################
# # 9.A.6. Extracting individual trajectories (slopes), number of observations and period
########################################################################################################

# I'm not going into this, not for now at least


########################################################################################################
# # 9.A.7. Plotting those individuals showing up in more than one date within each event
########################################################################################################

# # It doesn't really work now, don't now why. But, anyway, I don't like it, I'll get my way around
# 
# eloplot(elo_scores.1,ids="random.20")
# eloplot(elo_scores_ind.2,ids="random.20")
# eloplot(elo_scores_ind.3,ids="random.20")
# eloplot(elo_scores_ind.4,ids="random.20")
# eloplot(elo_scores_ind.5,ids="random.20")
# eloplot(elo_scores_ind.6,ids="random.20")
# 
# # plotting all plots at once
# 
# par(mfrow=c(1,1))
# 
# for(i in 1:9){
#   
#   x <- subset(elo_scores_all_events,elo_scores_all_events$event == i)
#   eloplot(x,ids="random.20")
#   
# }


########################################################################################################
# # 9.A.8. Creating a dyadic matrix per event
########################################################################################################

dyad_matrix.1 <- creatematrix(elo_scores.1,
                              drawmethod="0.5",
                              daterange=c("2013-11-13","2013-11-14")) 

dyad_matrix.2 <- creatematrix(elo_scores.2, 
                              drawmethod="0.5",
                              daterange=c("2014-3-15","2014-7-9")) 

dyad_matrix.3 <- creatematrix(elo_scores.3, 
                              drawmethod="0.5",
                              daterange=c("2015-2-15","2015-2-17")) 

dyad_matrix.4 <- creatematrix(elo_scores.4, 
                              drawmethod="0.5",
                              daterange=c("2015-5-5","2015-6-11")) 

dyad_matrix.5 <- creatematrix(elo_scores.5, 
                              drawmethod="0.5",
                              daterange=c("2016-2-17","2016-2-18")) 

dyad_matrix.6 <- creatematrix(elo_scores.6, 
                              drawmethod="0.5",
                              daterange=c("2016-5-2","2016-6-1"))

########################################################################################################
# # 9.A.9. Dominance hierarchy characteristics: Tri-transitivity
########################################################################################################

int.to.dom=function(x){((x>t(x)) & (x+t(x)>0))+0}


# depending on the event you want to analyze, you've got to change m. I'll make a for loop late ron
m<-int.to.dom(as.matrix(dyad_matrix.6))

g<-network(m,directed=TRUE)

tri<-triad.census(g) 
tri

w=as.vector(c(0,0,0,0,0,0,0,0,1,0,0,1,1,0.5,0.75,0.75))

N.triangle=sum(tri*as.vector(c(0,0,0,0,0,0,0,0,1,1,0,1,1,1,1,1)))

Pt=sum(tri*w)/N.triangle  
Pt 

t.tri=4*(Pt-0.75)
t.tri

dyads=dyad.census(g) 
dyads

r.p.t=vector(length=1000)

j=1

while(j<1001){
  
  r=rguman(1,nv=nrow(m),mut=dyads[1],asym=dyads[2],null=dyads[3])
  
  r.triad=triad.census(r)
  
  r.p.t[j]=r.triad[9]/(r.triad[10]+r.triad[9])
  
  if (is.na(r.p.t[j])) next else j=j+1
  
}

p=length(r.p.t[r.p.t>=Pt])/1000
p



# ########################################################################################################
# # 9.B. Estimating ELO-RANKS FOR THE WHOLE DATASET! (NOT VERY INFORMATIVE, THEREFORE COMMENTED)
# ########################################################################################################
# 
# ########################################################################################################
# # # 9.B.1. First, checking that the data is in the right format
# ########################################################################################################
# 
# #     The function will whow "WARNINGS" for those individuals that only showed up in one date. From
# # ?seqcheck: "while not per se a problem for the calculation of Elo ratings, individuals that were
# # observed only on one day (irrespective of the number of interactions on that day) cannot be plotted. 
# # eloplot will give a warning in such cases, too."
# 
# seqcheck(winner=dom.final.v2$Winner, 
#          loser=dom.final.v2$Loser, 
#          Date=dom.final.v2$date.ELO2.2, 
#          draw=dom.final.v2$Draw)
# 
# 
# ########################################################################################################
# # # 9.B.2. Obtaining the elo-scores for each individual
# ########################################################################################################
# 
# elo_scores <- elo.seq(winner=dom.final.v2$Winner, 
#                       loser=dom.final.v2$Loser, 
#                       Date=dom.final.v2$date.ELO2.2, 
#                       draw=dom.final.v2$Draw)
# 
# # To see a summary of the database
# #summary(elo_scores)
# 
# 
# # I leave all these commented as I'm not too interested in it for now
# 
#
# ########################################################################################################
# # # 9.B.3. Stability coefficient
# ########################################################################################################
# 
# # It checks how stable the hierarchy is. It seems to be incredibly stable
# 
# stab.elo(elo_scores)
# 
# 
# ########################################################################################################
# # # 9.B.4. Proportion of unknown dyads
# ########################################################################################################
# 
# prunk(elo_scores)
# 
# 
# # ########################################################################################################
# # # # 9.B.5. Extracting elo-ratings per individual
# # ########################################################################################################
# # 
# # # One can even extract specific elo-ratings at specific dates, I'll this later on, e.g.:
# # # extract.elo(elo_scores, "2013-11-13", IDs=c("s", "a", "c", "k"))
# # 
# #     For now, I don't want to do it for the whole database (i.e. the whole period), as it doesn't probably
# # really make sense.
# #
# # elo_scores_ind <- extract.elo(elo_scores,standardize = TRUE) #,standardize = TRUE)
# # 
# # # saving it as a csv file
# # # write.csv(as.data.frame(EloR1.all.ext.scores),file="StElos2015.csv",sep=",",na="",dec = ".")
# # 
# # #sort(names(elo_scores_ind))
# # 
# # #str(elo_scores_ind)
# # #names<-as.character(names(elo_scores_ind))
# # 
# # 
# # ########################################################################################################
# # # # 9.B.6. Extracting individual trajectories (slopes), number of observations and period
# # ########################################################################################################
# # 
# # #     I'm using the list dataframe with colour code and number of dates appearing, but I will exclude
# # # from it birds that showed up only once
# # 
# # int.birds <- superlist.num.date[superlist.num.date$freqofdates>1,1]
# # 
# # 
# # int.birds <- as.character(int.birds)
# # 
# # 
# # traj.elo(elo_scores,int.birds,from="2013-11-13",
# #          to="2016-5-31")
# 
# 
# ########################################################################################################
# # # 9.B.7. Plotting those individuals showing up in more than one date
# ########################################################################################################
# 
# # # This is what Moises did to the function, he changed a few things
# # 
# # eloplot2<-function (eloobject, ids = "all", interpolate = "yes", from = "start", 
# #                     to = "end", color = TRUE) 
# # {
# #   res <- eloobject
# #   if (interpolate == "yes") {
# #     plotdata = res$cmat
# #   }
# #   else {
# #     plotdata = res$lmat
# #   }
# #   temp <- rbind(rowSums(table(res$logtable$winner, res$logtable$Date) > 
# #                           0)[res$allids], rowSums(table(res$logtable$loser, res$logtable$Date) > 
# #                                                     0)[res$allids])
# #   colnames(temp) <- res$allids
# #   if (1 %in% colSums(temp, na.rm = T)) {
# #     good <- colnames(temp)[colSums(temp, na.rm = T) > 1]
# #     bad <- colnames(temp)[colSums(temp, na.rm = T) == 1]
# #     plotdata <- plotdata[, good]
# #   }
# #   if (ids[1] %in% c("random.20", "first.20", "all")) {
# #     if (ids[1] == "random.20") {
# #       if (length(colnames(plotdata)) > 20) {
# #         ids = sample(colnames(plotdata), 20)
# #       }
# #       else {
# #         ids = colnames(plotdata)
# #       }
# #     }
# #     if (ids[1] == "first.20") {
# #       if (length(colnames(plotdata)) > 20) {
# #         ids = colnames(plotdata)[1:20]
# #       }
# #       else {
# #         ids = colnames(plotdata)
# #       }
# #     }
# #     if (ids[1] == "all") {
# #       ids = colnames(plotdata)
# #       if (exists("bad")) {
# #         warning("IDs for which interactions were observed on only one day were excluded", 
# #                 call. = FALSE)
# #         rm(bad)
# #       }
# #     }
# #   }
# #   else {
# #     ids = ids
# #   }
# #   if (exists("bad")) {
# #     if (length(intersect(ids, bad)) >= 1) {
# #       warning("IDs for which interactions were observed on only one day were excluded", 
# #               call. = FALSE)
# #       ids <- intersect(ids, good)
# #     }
# #   }
# #   plotdata = plotdata[, ids]
# #   if (from == "start" & to == "end") {
# #     dates = seq(min(res$truedates), max(res$truedates), 
# #                 "day")
# #     ids.wo = ""
# #   }
# #   if (from != "start" | to != "end") {
# #     if (from != "start" & to == "end") {
# #       dates = seq(as.Date(from), max(res$truedates), "day")
# #     }
# #     if (from == "start" & to != "end") {
# #       dates = seq(min(res$truedates), as.Date(to), "day")
# #     }
# #     if (from != "start" & to != "end") {
# #       dates = seq(as.Date(from), as.Date(to), "day")
# #     }
# #     plotdata = plotdata[which(res$truedates %in% dates), 
# #                         ]
# #     xx = apply(plotdata, 2, function(x) {
# #       sum(is.na(x))
# #     })
# #     plotdata = plotdata[, which(xx < nrow(plotdata))]
# #     ids = colnames(plotdata)
# #     ids.wo = names(xx)[xx == nrow(plotdata)]
# #   }
# #   fst.month = unique(as.Date(as.yearmon(dates)))
# #   if (dates[1] > fst.month[1]) {
# #     fst.month = fst.month[-1]
# #   }
# #   if (tail(dates, 1) < tail(fst.month, 1)) {
# #     fst.month = fst.month[-(length(fst.month))]
# #   }
# #   labs = c(min(dates), fst.month, max(dates))
# #   if (labs[1] == labs[2]) {
# #     labs <- labs[-1]
# #   }
# #   ats = which(dates %in% labs)
# #   colo = colors()[c(552, 254, 652, 26, 33, 259, 32, 610, 51, 
# #                     148, 31, 47, 128, 7, 8, 12, 24, 53, 56, 68, 547, 116, 
# #                     142, 30, 204, 498, 22, 62, 146)][1:length(ids)]
# #   if (color) {
# #     layout(matrix(c(1, 2), ncol = 2), heights = c(5, 5), 
# #            widths = c(4, 1))
# #     par(mar = c(5, 4, 4, 0))
# #     plot(1:length(dates), 1:length(dates), ylim = range(plotdata, 
# #                                                         na.rm = T), ylab = "Elo-rating", xlab = "date", 
# #          type = "n", xaxt = "n", las = 1, cex.axis = 1, 
# #          cex.lab = 1)
# #     axis(1, at = ats, labels = labs, cex.axis = 1, las = 1)
# #     mtext(c("first day", "last day"), side = 1, line = 2, 
# #           at = c(1, tail(ats, 1)), cex = 1)
# #     for (i in 1:length(ids)) {
# #       lines(plotdata[, ids[i]], lty = 1, type = "l", col = colo[i],lwd=2)
# #     }
# #     par(mar = c(5, 1, 3.8, 1))
# #     plot(1:2, 1:2, xaxt = "n", yaxt = "n", type = "n", bty = "n", 
# #          ylab = "", xlab = "")
# #     
# #   }
# #   else {
# #     p.dates = which(dates %in% pretty(dates, 15))
# #     pointsdata = plotdata[p.dates, ]
# #     if (length(c(ids, ids.wo))/25 > 1) {
# #       p.times = rep(1:25, round(length(c(ids, ids.wo))/25))
# #     }
# #     else {
# #       p.times = 1:25
# #     }
# #     layout(matrix(c(1, 2), ncol = 2), heights = c(5, 5), 
# #            widths = c(4, 1))
# #     par(mar = c(5, 4, 4, 0))
# #     plot(1:length(dates), 1:length(dates), ylim = range(plotdata, 
# #                                                         na.rm = T), ylab = "Elo-rating", xlab = "date", 
# #          type = "n", xaxt = "n", las = 1, cex.axis = 1, 
# #          cex.lab = 1)
# #     axis(1, at = ats, labels = labs, cex.axis = 1, las = 1)
# #     mtext(c("first day", "last day"), side = 1, line = 2, 
# #           at = c(1, tail(ats, 1)), cex = 1)
# #     for (i in 1:length(ids)) {
# #       lines(plotdata[, ids[i]], lty = i)
# #       points(p.dates, pointsdata[, ids[i]], pch = p.times[i], 
# #              cex = 0.8, bg = "grey")
# #     }
# #     par(mar = c(5, 1, 3.8, 1))
# #     plot(1:2, 1:2, xaxt = "n", yaxt = "n", type = "n", bty = "n", 
# #          ylab = "", xlab = "")
# #   }
# # }
# 
# #     This is the original package function, not very pretty. The function can only plot max.
# # 20 individuals. ids="random.20" plots 20 random ids so that you can explore it, otherwise it
# # does it alphabetically
# 
# eloplot(elo_scores,ids="random.20")
# 
# 
# # ########################################################################################################
# # # # 9.B.8. Creating a dyadic matrix
# # ########################################################################################################
# # 
# # It doesn't make sense to do so for the whole database as there will be plenty of null dyads
# #
# # dyad_matrix<-creatematrix(elo_scores, 
# #                           drawmethod="0.5",
# #                           daterange=c("2013-11-13","2016-05-31")) 
# # 
# # 
# # 
# # #if onlyinteracting=TRUE included in the previous function,
# # #only individuals that interacted are included. In my case, 
# # #all interacted, otherwise they wouldn't be in this database
# # 
# # 
# # ########################################################################################################
# # # # 9.B.9. Dominance hierarchy characteristics: Tri-transitivity
# # ########################################################################################################
# # 
# # #     This does not make sense for the whole dataset as the matrix contains too many 0s, so I will leave
# # # it commented.
# # 
# # #     First, we are going to transform the raw matrices into a **binary (dominant = 1, subordinate = 0)
# # # matrix** using this code lines:
# # 
# # head(dyad_matrix)
# # 
# # int.to.dom=function(x){((x>t(x)) & (x+t(x)>0))+0}
# # 
# # m<-int.to.dom(as.matrix(dyad_matrix))
# # 
# # 
# # #     Now, we are going to transform this binary interaction matrix into a *network object*, to be able
# # # to work with the statnet-package functions:
# # 
# # g<-network(m,directed=TRUE)
# # 
# # 
# # #     The next step consists in perform the triad census for our empirical matrix. To do so, we are
# # # going to pass through the function **triad.census()** the former created *network object* (i.e. our
# # # dominance binary matrix).This function will create for us a 16-element vector of the counts for each
# # # type of possible dominance triad.
# # 
# # # (see the manual for the **sna** package: http://cran.r-project.org/web/packages/sna/sna.pdf).
# # 
# # tri<-triad.census(g) 
# # tri
# # 
# # 
# # #     With the following script line, we are defining the **weighting vector for transitivity** for the
# # # triad census performed before (see the Word document/Appendix 2 from Shizuka and McDonald 2012):
# # 
# # w=as.vector(c(0,0,0,0,0,0,0,0,1,0,0,1,1,0.5,0.75,0.75))
# # 
# # 
# # #     Once this is done, we are going to **count and sum the number of triangles** (i.e. triads in which
# # # all dominance realations are stablished and thus can potentially be transitive):
# # 
# # N.triangle=sum(tri*as.vector(c(0,0,0,0,0,0,0,0,1,1,0,1,1,1,1,1)))
# # 
# # 
# # #     Then, we are going to calculate the $P_t$ and $t_{tri}$ using the following code (see the paper by
# # # McDonald and Shizuka 2011 for a mathematical definition of $P_t$ and $t_{tri}$):
# # 
# # Pt=sum(tri*w)/N.triangle  
# # 
# # Pt 
# # 
# # t.tri=4*(Pt-0.75)
# # 
# # t.tri
# # 
# # #     The last code chunk, will perform a *conditional uniform graph test* (i.e. think of it as a Monte 
# # # Carlo procedure for testing network metrics to test whether the given dominance set is more orderly
# # # than expected by chance.To do it, we conduct **1000 simulations** of random networks with the same
# # # number of mutual, asymmetric and null dyads as the empirical dominance matrix. We calculate P~t~ 
# # # from each of these simulated networks (r.P.t). The code uses a while() loop because randomizations of
# # # some sparse matrices will produce networks with no triangles. For simplicity, we just throw these
# # # no-triangle networks out. 
# # 
# # dyads=dyad.census(g) 
# # dyads
# # 
# # r.p.t=vector(length=1000)
# # 
# # j=1
# # 
# # while(j<1001){
# #   
# #   r=rguman(1,nv=nrow(m),mut=dyads[1],asym=dyads[2],null=dyads[3])
# #   
# #   r.triad=triad.census(r)
# #   
# #   r.p.t[j]=r.triad[9]/(r.triad[10]+r.triad[9])
# #   
# #   if (is.na(r.p.t[j])) next else j=j+1
# #   
# # }
# # 
# # p=length(r.p.t[r.p.t>=Pt])/1000
# # 
# # 
# # #this p is a p-value that should be <0.05 for the transitivity value estimated
# # #above to have any meaning
# # p
# 
# 
# 
# 
# ########################################################################################################
# # 9.C. Estimating ELO-RANKS per event (not separating winter from summer, I.E. 9 events)
# ########################################################################################################
# 
# #     This repeats the previous section 9.A. but for each of the events. Explanations are given above,
# # I won't repeat them in this section.
# 
# 
# ########################################################################################################
# # # 9.C.2. Obtaining the elo-scores for each individual
# ########################################################################################################
# 
# # First, spliting the database by event
# 
# 
# # Creating a different database per event with this for loop (can be put together with following loop)
# 
# counter <- 1
# 
# for(i in levels(dom.final.v2$event)){
#   
#   x<-subset(dom.final.v2, dom.final.v2$event==i)
#   assign(paste0("dom.event",counter),x)
#   counter <- counter + 1
# }
# 
# 
# # Creating a database with elo_scores per event
# 
# counter <- 1
# 
# for(i in levels(dom.final.v2$event)){
#   
#   x<-subset(dom.final.v2, dom.final.v2$event==i)
#   y<-elo.seq(winner=x$Winner, 
#              loser=x$Loser, 
#              Date=x$date.ELO2.2, 
#              draw=x$Draw)
#   print(summary(y))
#   assign(paste0("elo_scores.",counter),y)
#   counter <- counter + 1
#   
# }
# 
# 
# ########################################################################################################
# # # 9.C.3. Stability coefficient
# ########################################################################################################
# 
# stab.elo(elo_scores.1)
# stab.elo(elo_scores.2)
# stab.elo(elo_scores.3)
# stab.elo(elo_scores.4) # as expected, it is crazy and non-sense, displacements where very hard to score
#                        # funny thing is that it is also crazy when displacements are taken out. 
# stab.elo(elo_scores.5)
# stab.elo(elo_scores.6)
# stab.elo(elo_scores.7)
# stab.elo(elo_scores.8)
# stab.elo(elo_scores.9)
# 
# 
# ########################################################################################################
# # # 9.C.4. Proportion of unknown dyads
# ########################################################################################################
# 
# prunk(elo_scores.1)
# prunk(elo_scores.2)
# prunk(elo_scores.3)
# prunk(elo_scores.4)
# prunk(elo_scores.5)
# prunk(elo_scores.6)
# prunk(elo_scores.7)
# prunk(elo_scores.8)
# prunk(elo_scores.9)
# 
# 
# ########################################################################################################
# # # 9.C.5. Extracting elo-ratings per individual
# ########################################################################################################
# 
# elo_scores_ind.1 <- extract.elo(elo_scores.1,standardize = TRUE)
# elo_scores_ind.2 <- extract.elo(elo_scores.2,standardize = TRUE)
# elo_scores_ind.3 <- extract.elo(elo_scores.3,standardize = TRUE)
# elo_scores_ind.4 <- extract.elo(elo_scores.4,standardize = TRUE)
# elo_scores_ind.5 <- extract.elo(elo_scores.5,standardize = TRUE)
# elo_scores_ind.6 <- extract.elo(elo_scores.6,standardize = TRUE)
# elo_scores_ind.7 <- extract.elo(elo_scores.7,standardize = TRUE)
# elo_scores_ind.8 <- extract.elo(elo_scores.8,standardize = TRUE)
# elo_scores_ind.9 <- extract.elo(elo_scores.9,standardize = TRUE)
# 
# 
# #     This code is to make a dataframe out of those individuals scores, please, change the number for the
# # event you want!
# 
# # creating the name of the rows, basically from 1 to total number of individuals
# 
# 
# # Creating a database with elo_scores per event
# 
# counter <- 1
# 
# for(i in levels(dom.final.v2$event)){
#   
#   x<-subset(dom.final.v2, dom.final.v2$event==i)
#   y<-elo.seq(winner=x$Winner, 
#              loser=x$Loser, 
#              Date=x$date.ELO2.2, 
#              draw=x$Draw)
#   
#   w<-extract.elo(y,standardize = TRUE)
#   
#   rownames <- seq(1,length(w),
#                   1)
#   
#   # making a data.frame with the elo-ratings
#   scores <- as.data.frame(w,
#                           row.names = as.character(rownames))
#   
#   z <- cbind(attributes(w),
#               scores)
#   
#   z$event <- counter
#   
#   names(z) <- c("individual","StElo","event")
#   
#   assign(paste0("elo_scores_ind.db.",counter),z)
#   
#   counter <- counter + 1
#   
# }
# 
# 
# # creating a database with all these observations
# elo_scores_all_events <- rbind(elo_scores_ind.db.1,
#                                elo_scores_ind.db.2,
#                                elo_scores_ind.db.3,
#                                elo_scores_ind.db.4,
#                                elo_scores_ind.db.5,
#                                elo_scores_ind.db.6,
#                                elo_scores_ind.db.7,
#                                elo_scores_ind.db.8,
#                                elo_scores_ind.db.9)
# 
# 
# ########################################################################################################
# # # 9.C.5.1. Estimating repeatability of StElo-rank using Shinichi's package
# ########################################################################################################
# 
# # Estimating repeatability of dominance rank
# 
# # rpt.St.ANOVA <- rpt(elo_scores_all_events$StElo,
# #               elo_scores_all_events$individual,
# #               datatype="Gaussian",
# #               method="ANOVA",
# #               npermut=1000)
# # 
# # rpt.St.REML <- rpt(elo_scores_all_events$StElo,
# #               elo_scores_all_events$individual,
# #               datatype="Gaussian",
# #               method="REML",
# #               nboot=1000,
# #               npermut=1000)
# 
# rpt.St.MCMC <- rpt(elo_scores_all_events$StElo,
#                    elo_scores_all_events$individual,
#                    datatype="Gaussian",
#                    method="MCMC",
#                    nboot=1000,
#                    npermut=1000)
# 
# 
# 
# # saving it as a csv file
# # write.csv(as.data.frame(EloR1.all.ext.scores),file="StElos2015.csv",sep=",",na="",dec = ".")
# 
# 
# ########################################################################################################
# # # 9.C.6. Extracting individual trajectories (slopes), number of observations and period
# ########################################################################################################
# 
# # I'm not going into this, not for now at least
# 
# 
# ########################################################################################################
# # # 9.C.7. Plotting those individuals showing up in more than one date within each event
# ########################################################################################################
# 
# eloplot(elo_scores_ind.1,ids="random.20")
# eloplot(elo_scores_ind.2,ids="random.20")
# eloplot(elo_scores_ind.3,ids="random.20")
# eloplot(elo_scores_ind.4,ids="random.20")
# eloplot(elo_scores_ind.5,ids="random.20")
# eloplot(elo_scores_ind.6,ids="random.20")
# eloplot(elo_scores_ind.7,ids="random.20")
# eloplot(elo_scores_ind.8,ids="random.20")
# eloplot(elo_scores_ind.9,ids="random.20")
# 
# 
# # plotting all plots at once
# 
# par(mfrow=c(1,1))
# 
# for(i in 1:9){
#   
#   x <- subset(elo_scores_all_events,elo_scores_all_events$event == i)
#   eloplot(x,ids="random.20")
#   
# }
# 
# 
# ########################################################################################################
# # # 9.C.8. Creating a dyadic matrix per event
# ########################################################################################################
# 
# dyad_matrix.1 <- creatematrix(elo_scores.1,
#                               drawmethod="0.5",
#                               daterange=c("2013-11-13","2013-11-14")) 
# 
# dyad_matrix.2 <- creatematrix(elo_scores.2, 
#                               drawmethod="0.5",
#                               daterange=c("2014-3-15","2014-3-16")) 
# 
# dyad_matrix.3 <- creatematrix(elo_scores.3, 
#                               drawmethod="0.5",
#                               daterange=c("2014-7-8","2014-7-9")) 
# 
# dyad_matrix.4 <- creatematrix(elo_scores.4, 
#                               drawmethod="0.5",
#                               daterange=c("2015-2-15","2015-2-17")) 
# 
# dyad_matrix.5 <- creatematrix(elo_scores.5, 
#                               drawmethod="0.5",
#                               daterange=c("2015-5-5","2015-5-6")) 
# 
# dyad_matrix.6 <- creatematrix(elo_scores.6, 
#                               drawmethod="0.5",
#                               daterange=c("2015-6-10","2015-6-11")) 
# 
# dyad_matrix.7 <- creatematrix(elo_scores.7, 
#                               drawmethod="0.5",
#                               daterange=c("2016-2-17","2016-2-18")) 
# 
# dyad_matrix.8 <- creatematrix(elo_scores.8, 
#                               drawmethod="0.5",
#                               daterange=c("2016-5-2","2016-5-3")) 
# 
# dyad_matrix.9 <- creatematrix(elo_scores.9, 
#                               drawmethod="0.5",
#                               daterange=c("2016-5-31","2016-6-1")) 
# 
# 
# ########################################################################################################
# # # 9.C.9. Dominance hierarchy characteristics: Tri-transitivity
# ########################################################################################################
# 
# int.to.dom=function(x){((x>t(x)) & (x+t(x)>0))+0}
# 
# 
# # depending on the event you want to analyze
# m<-int.to.dom(as.matrix(dyad_matrix.9))
# 
# g<-network(m,directed=TRUE)
# 
# tri<-triad.census(g) 
# tri
# 
# w=as.vector(c(0,0,0,0,0,0,0,0,1,0,0,1,1,0.5,0.75,0.75))
# 
# N.triangle=sum(tri*as.vector(c(0,0,0,0,0,0,0,0,1,1,0,1,1,1,1,1)))
# 
# Pt=sum(tri*w)/N.triangle  
# Pt 
# 
# t.tri=4*(Pt-0.75)
# t.tri
# 
# dyads=dyad.census(g) 
# dyads
# 
# r.p.t=vector(length=1000)
# 
# j=1
# 
# while(j<1001){
#   
#   r=rguman(1,nv=nrow(m),mut=dyads[1],asym=dyads[2],null=dyads[3])
#   
#   r.triad=triad.census(r)
#   
#   r.p.t[j]=r.triad[9]/(r.triad[10]+r.triad[9])
#   
#   if (is.na(r.p.t[j])) next else j=j+1
#   
# }
# 
# p=length(r.p.t[r.p.t>=Pt])/1000
# p
