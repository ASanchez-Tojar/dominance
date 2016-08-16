
# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 16th of August, 2016
# Script last updated on the 16th of August, 2016


########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to estimate how much time per event we analyzed


# Clear memory and get to know where you are
rm(list=ls())
#getwd()


########################################################################################################
# Packages needed
########################################################################################################

library(doBy)


########################################################################################################
# Loading dominance database
########################################################################################################

# This version of the database is sorted by date,video,realtime (realtime from ealier to later)

dom <- read.table("MegaDataBase-v97-201311-201606-FY-Dominance_Lundy_20160816b.csv",header=TRUE,sep=',')


# reducing database variables to what I need for this

dom.2 <- dom[,c("date","date.ELO2","video","realtime")]

# creating new times with the date included
dom.2$realtime2 <- strptime(paste(dom.2$date.ELO2,dom.2$realtime,sep=" "),
                                  format = "%m/%d/%Y %H:%M:%S", tz = "CET")


# Creating a database with the earliest entry for each video

dom.earliest <- dom.2[!duplicated(dom.2$video),]


# Now I need to create a database with the latest entry for each video. For this, I load a version
# of the database that is sorted by date,video,realtime (realtime from later to earlier)

dom.decrease <- read.table("MegaDataBase-v97-201311-201606-FY-Dominance_Lundy_20160816b-decreasingtime.csv",header=TRUE,sep=',')

# reducing database variables to what I need for this

dom.decrease <- dom.decrease[,c("date","date.ELO2","video","realtime")]

# creating new times with the date included
dom.decrease$realtime2 <- strptime(paste(dom.decrease$date.ELO2,dom.decrease$realtime,sep=" "),
                                   format = "%m/%d/%Y %H:%M:%S", tz = "CET")

# Creating a database with the earliest entry for each video

dom.latest <- dom.decrease[!duplicated(dom.decrease$video),]


# Now I just cbind the realtime from latest to earliest

latest <- dom.latest$realtime2

dom.dif <- cbind(dom.earliest,latest)

dom.dif$videolength <- abs(as.numeric(difftime(dom.dif$realtime2, dom.dif$latest)))


# Now I can calculate how much effective time per event we analyzed

eff.time.event <-summaryBy(videolength ~ date, data = dom.dif, 
                          FUN = list(sum))

eff.time.event$videolength_h <- eff.time.event$videolength.sum/60
