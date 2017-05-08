
# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 16th of August, 2016
# Script last updated on the 18th of August, 2016


########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to estimate how much time per day (and in total) we analyzed

# Clear memory and get to know where you are
rm(list=ls())
#getwd()


########################################################################################################
# Packages needed
########################################################################################################

library(doBy)


########################################################################################################
# Loading database
########################################################################################################

db <- read.table("summaries/video_summary.csv",header=TRUE,sep=',')

db$length.s <- ((as.numeric(substr(db$length,4,5)))*60)+
  (as.numeric(substr(db$length,7,8)))


time.per.date <- summaryBy(length.s~date,data=db,FUN=function(x) {m=sum(x)})
names(time.per.date) <- c("date","time.s")

time.per.date$time.h <- time.per.date$time.s/3600


# # This version of the database is sorted by date,video,realtime (realtime from ealier to later)
# 
# dom <- read.table("MegaDataBase-v136-201311-201611-FY-Dominance_Lundy_20170508.csv",header=TRUE,sep=',')
# 
# # x<-summaryBy(level~video,data=dom,FUN=function(x) {m=min(x)})
# # x[x$`level.function(x) {     m = min(x) }`==2,]
# 

# # reducing database variables to what I need for this
# 
# dom.2 <- dom[,c("date","date.ELO2","video","realtime")]
# 
# # creating new times with the date included
# dom.2$realtime2 <- strptime(paste(dom.2$date.ELO2,dom.2$realtime,sep=" "),
#                                   format = "%m/%d/%Y %H:%M:%S", tz = "CET")
# 
# 
# # Creating a database with the earliest entry for each video
# 
# dom.earliest <- dom.2[!duplicated(dom.2$video),]
# 
# #write.csv(dom.earliest[,c("date","starting_time","video")],"summaries/video_summary.csv",row.names=FALSE)
# 
# 
# 
# # Now I need to create a database with the latest entry for each video. For this, I load a version
# # of the database that is sorted by date,video,realtime (realtime from later to earlier)
# 
# dom.decrease <- read.table("MegaDataBase-v136-201311-201611-FY-Dominance_Lundy_20170508-decreasingtime.csv",header=TRUE,sep=',')
# 
# # reducing database variables to what I need for this
# 
# dom.decrease <- dom.decrease[,c("date","date.ELO2","video","realtime")]
# 
# # creating new times with the date included
# dom.decrease$realtime2 <- strptime(paste(dom.decrease$date.ELO2,dom.decrease$realtime,sep=" "),
#                                    format = "%m/%d/%Y %H:%M:%S", tz = "CET")
# 
# # Creating a database with the earliest entry for each video
# 
# dom.latest <- dom.decrease[!duplicated(dom.decrease$video),]
# 
# 
# # Now I just cbind the realtime from latest to earliest
# 
# latest <- dom.latest$realtime2
# 
# dom.dif <- cbind(dom.earliest,latest)
# 
# dom.dif$videolength <- abs(as.numeric(difftime(dom.dif$realtime2, dom.dif$latest)))
# 
# 
# # Now I can calculate how much effective time per event we analyzed
# 
# eff.time.event <-summaryBy(videolength ~ date, data = dom.dif, 
#                           FUN = list(sum))
# 
# eff.time.event$videolength_h <- eff.time.event$videolength.sum/3600
# 
