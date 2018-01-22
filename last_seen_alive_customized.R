# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 8th of Jan, 2018

############################################################################
# Description of script and Instructions
############################################################################

# This script estimates LastSeenAlive from all sources possible for the 
# period between Jan 2013 to Dec 2017


############################################################################
# Packages needed
############################################################################

# packages needed

library(doBy)

# Clear memory
rm(list=ls())


##########################
# DOMINANCE DATA
##########################

dom <- read.table("dom.final.v2.csv",header=TRUE,sep=",")
#making a database with one row per individual
dominance.a <- dom[,c("date","Winner")]
names(dominance.a) <- c("date","cc")
dominance.b <- dom[,c("date","Loser")]
names(dominance.b) <- c("date","cc")
dom.2 <- unique(rbind(dominance.a,dominance.b))

#substituting the colour code by the real BirdID
cctobirdID <- read.table("birdsex.1.csv",header=TRUE,sep=",")
cctobirdID.2 <- cctobirdID[,c("Code","BirdID")]
dom.3 <- merge(dom.2,cctobirdID.2,by.x="cc",by.y="Code",all.x=TRUE)

dom.3$DetectionDate <- factor(paste0(substr(dom.3$date,7,8),"/",
                                   substr(dom.3$date,5,6),"/",
                                   substr(dom.3$date,1,4)))

dominance <- dom.3[!(is.na(dom.3$BirdID)),c("BirdID","DetectionDate")]
dominance$method <- "DOM"


##########################
# CAPTURE DATA
##########################
# SQL code: last update 06/01/2018
# if dead captures are included
# #SELECT tblCaptures.BirdID, tblCaptures.CaptureDate
# #FROM tblCaptures;
# if dead captures are excluded
# SELECT tblCaptures.BirdID, tblCaptures.CaptureDate, tblBirdID.DeathCaptureRef
# FROM tblCaptures LEFT JOIN tblBirdID ON tblCaptures.CaptureRef = tblBirdID.DeathCaptureRef
# WHERE (((tblBirdID.DeathCaptureRef) Is Null));

# #dead captures included
# cap <- read.table("survival/data/allcaptures_allstages_2000-2017.csv",header=TRUE,sep=",")
#dead captures excluded
cap <- read.table("survival/data/allcaptures_allstages_nodeadcaptures_2000-2017.csv",header=TRUE,sep=",")
captures <- unique(cap[,c("BirdID","CaptureDate")])
names(captures) <- c("BirdID","DetectionDate")
captures$method <- "CAP"


##########################
# SIGHTING DATA
##########################
# SQL code: last update 06/01/2018
#SELECT tblSightings.BirdID, tblSightings.SightingDate, tblSightings.IDCertain
#FROM tblSightings
#WHERE (((tblSightings.IDCertain)=True));
sig <- read.table("survival/data/allsightings2000-2017.csv",header=TRUE,sep=",")
sightings <- unique(sig[,c("BirdID","SightingDate")])
names(sightings) <- c("BirdID","DetectionDate")
sightings$method <- "SIG"


##########################
# PEDIGREE DATA
##########################
#includes all birds genotyped up to winter 2016
#this pedigree contains some birds that misteriously dissapeared since last(s) versions
ped <- read.table("survival/data/PedigreeUpToIncl2016-mysterydissapear_versionwithNA.txt",header=TRUE,sep="\t")
dams <- ped[,c("dam","Cohort")]
names(dams) <- c("BirdID","date")
sires <- ped[,c("sire","Cohort")]
names(sires) <- c("BirdID","date")

ped.2 <- unique(rbind(dams,sires))

#assuming that if bird identified as parent one summer, then it survived until the
#end of the summer (to standardize all the pedigree records)
ped.2$DetectionDate <- factor(paste0("31/08/",
                                     substr(ped.2$date,1,4)))

pedigree <- unique(ped.2[!(is.na(ped.2$BirdID)),c("BirdID","DetectionDate")])
pedigree$method <- "PED"


##########################
# RFID DATA
##########################
# RFID data from nest-boxes. Recorded from 1st Oct 2013 to 28th Feb 2014,
# and 1st Oct 2014 to 9th Feb 2015
RFID_nestbox <- read.table("survival/data/RFID_nestbox.csv",header=TRUE,sep=",")

# RFID data from an automatic feeder. Recorded from 29th Jan 2013 to 12th Feb 2016
RFID_feeder <- read.table("survival/data/RFID_feeder.csv",header=TRUE,sep=",")



##########################
# LAST SEEN ALIVE
##########################
survival <- rbind(dominance,captures,sightings,
                  pedigree,RFID_nestbox,RFID_feeder)
# 
# 
# survival <- survival[order(survival$BirdID,
#                            survival$DetectionDate,
#                            survival$method),]

survival$method <- as.factor(survival$method)



#choosing the last observation for each bird
survival$date.ord <- as.numeric(paste0(substr(survival$DetectionDate,7,10),
                                         substr(survival$DetectionDate,4,5),
                                         substr(survival$DetectionDate,1,2)))


survival <- survival[order(survival$BirdID,-survival$date.ord,survival$method),]


survival$BirdID <- as.factor(survival$BirdID)

survival.2 <- data.frame(BirdID=integer(),
                         DetectionDate=numeric(),
                         method=factor(),
                         date.ord=integer(),
                         lastseen=integer(),
                         stringsAsFactors=FALSE)

for (i in levels(survival$BirdID)){
  
  x <- survival[survival$BirdID==i,]

  x$lastseen <- ifelse(x$date.ord==max(x$date.ord),1,0)
  
  survival.2 <- rbind(survival.2,x)
  
}

survival.3 <- survival.2[survival.2$lastseen==1,c(-4,-5)]


# leaving a single entry per bird ID per date. For the duplicates, method alphabetically
# is chosen alphabetically

survival.3$BirdID_Date <- paste(survival.3$BirdID,survival.3$DetectionDate,sep="_")
survival.3 <- survival.3[order(survival.3$BirdID,survival.3$method),]

birdDate.list <- c()
survival.4 <- data.frame(BirdID=integer(),
                         DetectionDate=numeric(),
                         method=factor(),
                         stringsAsFactors=FALSE)


for (i in 1:nrow(survival.3)){
  if(!(survival.3$BirdID_Date[i] %in% birdDate.list)){
    birdDate.list <- c(birdDate.list,survival.3$BirdID_Date[i])
    survival.4 <- rbind(survival.4,survival.3[i,
                                            c("BirdID","DetectionDate","method")])
  }
}
#survival.2 <- survival

rownames(survival.4) <- NULL


#re-formatting date for script

survival.4$LastLiveRecord <- paste0(substr(survival.4$DetectionDate,4,6),
                                    substr(survival.4$DetectionDate,1,3),
                                    substr(survival.4$DetectionDate,7,10))


survival.final <- survival.4[,c("BirdID","LastLiveRecord","method")]
names(survival.final) <- c("BirdID","LastLiveRecord","Source")


write.csv(survival.final,"fledglings12/BirdID-LastSeenAlive-Source.csv",row.names=FALSE)
