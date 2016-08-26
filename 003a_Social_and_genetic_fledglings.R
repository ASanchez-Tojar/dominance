# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 19th of August, 2016
# Script last updated on the 26th of August, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to estimate the number of chicks reaching the age of 12 days for each 
# social and genetic father in each year. It does this by extracting all BirdIDs except
# unhatched and chigg BirdIDs (done in AccessDatabase query). Then it assigns to each bird 
# the hatching date (if known) and the date of lastseenalive, which is then used to estimate
# those BirdIDs that make it, at least, to their 12 day of life (what we call 12 days old).
# It also adds the fosterBroodRef for those birds that were crossfostered. This is used
# to assing to each BirdID the DadID that took care of it from at least day 2 (when we
# crossfoster) to day 12. After that, the pedigree is brought in to assign a genetic sire
# to each BirdID. Notice that there are some individuals (N=110?), for which brood is unkown
# this is because we caught them as fledglings or 1st winter individuals.

# During this script, I also looked at those Birds for which we did not have SocialDadID
# available (mostly because of Dads missing rings). By comparing the information we had
# on their remaining rings and the pedigree, I could decrease the number of missing
# SocialDadIDs by a bit more than half (only ca. 4% missing, instead of ca. 9%)


########################################################################################################
# Packages needed
########################################################################################################

# packages needed to be loaded for this script (a couple of them might be only needed in the following
# script)

library(plyr)


# Clear memory and get to know where you are
rm(list=ls())
#getwd()


##########################################################################
# Different datasets from sparrow main database
##########################################################################

# I'll do the same but withouth using the query designer
# database 1
# SELECT tblBirdID.BirdID, tblBirdID.Cohort, tblBirdID.BroodRef, tblBirdID.DeathDate
# FROM tblBirdID
# WHERE (((tblBirdID.Cohort)>2013) AND ((tblBirdID.LastStage)>1));
# Check that Date is in the right format, i.e. 4/25/2011

db1 <- read.table("fledglings12/BirdID-Cohort-DeathDate-BroodRef.csv",
                  header=TRUE,sep=",")


# database 2
# SELECT sys_LastSeenAlive.BirdID, sys_LastSeenAlive.LastLiveRecord, sys_LastSeenAlive.Source
# FROM sys_LastSeenAlive;
# Check that Date is in the right format, i.e. 4/25/2011

db2 <- read.table("fledglings12/BirdID-LastSeenAlive-Source.csv",
                  header=TRUE,sep=",")


# database 3
# SELECT sys_EggAndHatchDates.BirdID, sys_EggAndHatchDates.HatchDate
# FROM sys_EggAndHatchDates;
# Check that Date is in the right format, i.e. 4/25/2011

db3 <- read.table("fledglings12/BirdID_HatchingDate_builtinquery.csv",
                  header=TRUE,sep=",")


# database 5
# SELECT tblFosterBroods.BirdID, tblFosterBroods.FosterBrood, tblFosterBroods.FosterDate
# FROM tblFosterBroods;

db4 <- read.table("fledglings12/BirdID_FosterBroodRef.csv",
                  header=TRUE,sep=",")


##########################################################################
# Generating a database with all information needed per BirdID
##########################################################################

# Let's start merging them to get the final one

fledglings1 <- merge(db1,db2,by="BirdID",all.x=TRUE)

fledglings2 <- merge(fledglings1,db3,by="BirdID",all.x=TRUE)

fledglings3 <- merge(fledglings2,db4,by="BirdID",all.x=TRUE)


# Changing the names of some variables

names(fledglings3) <- c("BirdID","Cohort","DeathDate",
                        "OriginalBroodRef","LastLiveRecord","Source",
                        "HatchDate","FosterBroodRef")


# getting rid off those that we don't know the hatching date from #not active now

fledglings4 <- fledglings3#[!(is.na(fledglings3$HatchDate)),]


# Estimating last known age
# "+1" gives as the age that we expect since, for example, 4/10/2016 - 4/1/2016 = 9,
# instead of 10 days old :)

fledglings4$age.days <- 
  as.numeric(as.Date(fledglings4$LastLiveRecord, format="%m/%d/%Y")-
               as.Date(fledglings4$HatchDate, format="%m/%d/%Y")) + 1


# There are some NA due to some LastLiveRecord missing. Those missing values are of birds
# that died when we first saw them, i.e. we found the dead to start with

fledglings5 <- fledglings4#[!(is.na(fledglings4$age.days)),] #not active now


# Now I want to create a variable with the BroodRef where the individual lived from
# day 2 (i.e. day of crossfostering) on.

fledglings5$twodaysonBroodRef<-fledglings5$FosterBroodRef

fledglings5$twodaysonBroodRef<-ifelse(is.na(fledglings5$twodaysonBroodRef),
                                      fledglings5$OriginalBroodRef,
                                      fledglings5$twodaysonBroodRef)


##########################################################################
# Assigning social dads to each BirdID
##########################################################################

# Now, I will add the ID of the Social Dad that took care of the individual from 
# day 2 on. Thus, this takes into account crossfostering and assumes that the social
# influence of the dad during the first 24h is negligible. This is indeed very likely
# in our population as nest mortality mainly occurs from day 2 to day 5, but it is
# true that this also assumess that carry-over effects are negligible

# First, load the database with BroodRef and SocialDadID (updated from script 003aa)

SocialDads <- read.table("BroodRef_SocialDadID_IDCertain_2014-2016_Updated.csv",
                         header=TRUE,sep=",")


# merging it in relation to twodaysonBroodRef

fledglings6 <- merge(fledglings5,SocialDads,
                     by.x="twodaysonBroodRef",
                     by.y="BroodRef",
                     all.x=TRUE)


##########################################################################
# Assigning genetic dads to each BirdID
##########################################################################

# Now I'm going to add the genetic dad from the pedigree

pedigree <- read.table("fledglings12/PedigreeUpToIncl2015-versionwithNA.txt",
                       header=TRUE,sep="\t")


pedigree.red <- pedigree[,c("id","sire")]
names(pedigree.red) <- c("BirdID","GeneticDadID")


fledglings6.ped <- merge(fledglings6,pedigree.red,
                           by="BirdID",
                           all.x=TRUE)


##########################################################################
# Generating a list of breeders from 2014 to 2016
##########################################################################

##########################################################################
# MALES
##########################################################################

# But I can also see if there are some dads that pop up as genetic fathers
# but were not recorded as social, and add them to the list of social dads

# List of genetic fathers from 2014-2016 extracted from pedigree:
# Notice that the genetic pedigree isn't available for 2016 or birds caught
# unringed in February 2016. (*that meant 2 more birds than before in the final list)

genetic.males.breeding <- unique(pedigree[pedigree$Cohort>2013 & 
                                            !(is.na(pedigree$sire)),
                                   c("sire")])


# How many pop up as genetic but not social parents? 21 

SocialDads.list <- unique(SocialDads[!(is.na(SocialDads$SocialDadID)) & 
                                       SocialDads$SocialDadCertain==TRUE,
                                     c("SocialDadID")])


all.males.breeding <- c(SocialDads.list,
                        setdiff(genetic.males.breeding,SocialDads.list))


##########################################################################
# FEMALES
##########################################################################

# First, load the updated database with all social females. From script 003aa

SocialMums <- read.table("BroodRef_SocialMumID_IDCertain_2014-2016_Updated.csv",
                         header=TRUE,sep=",")


# But I can also see if there are some dads that pop up as genetic fathers
# but were not recorded as social, and add them to the list of social dads

# List of genetic fathers from 2014-2016 extracted from pedigree:
# Notice that the genetic pedigree isn't available for 2016 or birds caught
# unringed in February 2016. (*that meant 2 more birds than before in the final list)

genetic.females.breeding <- unique(pedigree[pedigree$Cohort>2013 & 
                                              !(is.na(pedigree$dam)),
                                            c("dam")])


# How many pop up as genetic but not social parents? 9 

SocialMums.list <- unique(SocialMums[!(is.na(SocialMums$SocialMumID)) & 
                                       SocialMums$SocialMumCertain==TRUE,
                                     c("SocialMumID")])


all.females.breeding <- c(SocialMums.list, 
                          setdiff(genetic.females.breeding,SocialMums.list))


##########################################################################
# Final breeder list (males and females)
##########################################################################

all.birds.breeding <- c(all.males.breeding,all.females.breeding)
                                               

##########################################################################
# Generating a variable for recruits: 0 = didn't recruit, 1 = recruited
##########################################################################

fledglings6.ped$recruited <- ifelse(fledglings6.ped$Cohort==2016,
                                      NA,
                                      ifelse(!(fledglings6.ped$BirdID %in% 
                                                 all.birds.breeding),
                                                0,1))

# Note: there are no individuals that did not reach 12 days but recruited,
# which is a good sign
# fledglings6.ped[fledglings6.ped$age.days<12 & 
#                   !(is.na(fledglings6.ped$age.days)) & 
#                   fledglings6.ped$recruited==1,
#                 c("BirdID")]


##########################################################################
# 12days old individuals
##########################################################################

# Subsetting the database according to those that survived, at least, to day 12
# This way we can estimate the number of fledglings per individual. I leave NAs as
# I also want to estimate the number of genetic fledglings. But I remove those age.days = NA
# for which HatchDate is known, this is because these ones refer to chicks that were first
# recorded as dead individuals, i.e. they did not make it to 12 days old

offspring.12d.ped <- subset(fledglings6.ped,
                            fledglings6.ped$age.days>11 | 
                              (is.na(fledglings6.ped$age.days) & 
                                 is.na(fledglings6.ped$HatchDate))
)



##########################################################################
# Final fitness databases
##########################################################################

##########################################################################
# Annual number of social fledglings reaching day 12 after hatching
##########################################################################

# Now I can count the annual number of social offspring reaching the age
# of 12 days per social and genetic dad respectively. I just need to count
# the number of times each SocialDadID2 and GeneticDadID show up in each
# year. 

social.per.Dad <- count(offspring.12d.ped,c("SocialDadID","Cohort"))
social.per.Dad.2 <- social.per.Dad[!(is.na(social.per.Dad$SocialDadID)),]


# To include 0 reproduction, I need to check all social breeders each year
# to assigned them 0 if they are not in social.per.Dad.2

# First, load a database with BroodRef and BroodName to get year for each
# BroodRef in SocialDads

# SELECT tblBroods.BroodRef, tblBroods.BroodName
# FROM tblBroods;

BroodName <- read.table("BroodRef_BroodName.csv",
                        header=TRUE,sep=",")


# Merge with SocialDads to get the Name and, subsequently, the year

SocialDads.year <- merge(SocialDads,BroodName,
                         by="BroodRef",all.x=TRUE)


# getting the year Code and then the year

SocialDads.year$yearCode <- factor(substr(SocialDads.year$BroodName, 1, 1))

SocialDads.year$year <- ifelse(SocialDads.year$yearCode=="N",
                              2014,
                              ifelse(SocialDads.year$yearCode=="O",
                                     2015,2016))

# Creating an identifier bird_year to subset later on

SocialDads.year$BirdID_eventSW <- as.factor(ifelse(!(is.na(SocialDads.year$SocialDadID)),
                                                   paste(SocialDads.year$SocialDadID,
                                                         SocialDads.year$year,
                                                         sep="_"),
                                                   NA))


# I also create an identifier for social.per.Dad.2

social.per.Dad.2$BirdID_eventSW <- factor(paste(social.per.Dad.2$SocialDadID,
                                                social.per.Dad.2$Cohort,
                                                sep="_"))


# sort(setdiff(SocialDads.year$BirdID_eventSW,
#         social.per.Dad.2$BirdID_eventSW))


# This is the list of supposedly 0 fitness birds, but most are birds for which
# we cannot be sure. I've checked them in the database:

# This is the list for which fitness=0 is uncertain

# 4682_2015: 2 broods that failed, but one after catching female (i.e mortality human causes?)
# 4745_2014: 1 inaccessible wild nest
# 4896_2015: 1 endoscope counting on day 9 = 4 chicks, 1 abandoned, 1 inaccessible
# 5004_2014: 1 inaccessible wild nest with chicks ca. 10 days
# 5048_2014: 1 inaccessible wild nest with chicks ca. 10 days
# 5107_2014: 2 inaccessible wild nests
# 5110_2014: 1 inaccessible wild nest with chicks ca. 10 days
# 5253_2014: 1 wild nest that failed at the end of the season, probably more before
# 6386_2015: 1 inaccessible wild nest
# 6517_2015: NOT EXCLUDED, only nestbox nest failed 
# 6768_2015: 1 inaccessible wild nest with chicks ca. 10 days
# 6778_2014: NOT EXCLUDED, 1 wild nest that failed mid-season
# 6789_2014: 1 inaccessible wild nest
# 6789_2015: 3 inaccessible wild nests
# 6793_2014: NOT EXCLUDED, 1 wild nest that failed beginning of May
# 6807_2014: 1 inaccessible wild nest with chicks ca. 10 days
# 6832_2015: 1 inaccessible wild nest, and one ringed before day 12
# 6862_2014: 1 wild nest failed but the other was ringed on day 10!
# 6921_2014: 2 inaccessible wild nests
# 6971_2014: NOT EXCLUDED, 1 wild nest that failed mid-May (although bird ID uncertain, exclude afterwards)
# 6978_2014: 1 inaccessible wild nest
# 7017_2015: 1 inaccessible wild nest
# 7019_2014: NOT EXCLUDED, 1 wild nest that failed mid-season
# 7060_2014: 1 wild nest failed, but another inaccessible
# 7266_2014: 1 nestbox nest at the end of the season that was ringed on day 10!
# 7273_2014: NOT EXCLUDED, only nestbox nest failed
# 7275_2015: 3 inaccessible wild nests
# 7570_2015: 1 inaccessible wild nest
# 7793_2015: 1 nestbox nest at the end of the season that was ringed on day 7!
# 7984_2015: NOT EXCLUDED, 2 nestbox nests failed
# 8307_2015: 1 inaccessible wild nest with chicks ca. 11 days

# For the 2016, wait for the database update before continuing, a few are:
# 6270_2016: 1 inaccessible wild nest, but update after summer
# 6648_2016: 1 inaccessible wild nest?, but update after summer
# 6688_2016: NOT EXCLUDED, only nestbox nest failed, but update after summer
# 6768_2016: 1 inaccessible wild nest with chicks ca. 6 days, but update after summer
# 6789_2016: 1 inaccessible wild nest, but update after summer
# 8307_2016
# 8390_2016
# 8414_2016
# 8486_2016
# 8499_2016
# 8642_2016
# 8652_2016
# 8697_2016
# 8715_2016

# The list to be included with 0 social fitness for the specific year:

certain <- c("6517_2015","6778_2014","6793_2014",
             "7019_2014","7273_2014","7984_2015")

# subsetting those that we can assign a 0 fitness

social.fitness.0 <-unique(SocialDads.year[!(is.na(SocialDads.year$SocialDadID)) &
                                            SocialDads.year$SocialDadCertain==TRUE &
                                            (SocialDads.year$BirdID_eventSW %in% certain),
                                          c("SocialDadID","year","BirdID_eventSW")])

# Setting their fitness to 0

social.fitness.0$freq <- 0

social.fitness.0 <- social.fitness.0[,c("SocialDadID","year","freq","BirdID_eventSW")]


# FINAL DATABASE

# rbinding both database

names(social.per.Dad.2) <- c("SocialDadID","year","freq","BirdID_eventSW")

social.per.Dad.3 <- rbind(social.per.Dad.2,social.fitness.0)

names(social.per.Dad.3) <- c("SocialDadID","year","soc.fledg.12d","BirdID_eventSW")


##########################################################################
# Annual number of social recruits
##########################################################################

# I can just count the number of recruits per SocialDad when recruited=1
# after that, I'll put it together with the previous database and assign
# 0 to those in social.per.Dad.3 that did not leave any

social.recruits.per.Dad <- count(offspring.12d.ped[offspring.12d.ped$recruited==1 & 
                                                     (!(is.na(offspring.12d.ped$recruited))),],
                                 c("SocialDadID","Cohort"))

social.recruits.per.Dad.2 <- social.recruits.per.Dad[!(is.na(social.recruits.per.Dad$SocialDadID)),]


# identifier to merge both databases

social.recruits.per.Dad.2$BirdID_eventSW <- factor(paste(social.recruits.per.Dad.2$SocialDadID,
                                                         social.recruits.per.Dad.2$Cohort,
                                                         sep="_"))


social.recruits.per.Dad.3 <- social.recruits.per.Dad.2[,c("BirdID_eventSW","freq")]

names(social.recruits.per.Dad.3) <- c("BirdID_eventSW","soc.recruits")


##########################################################################
# Final social fitness database
##########################################################################

# merging fledglings and recruits

social.fitness <- merge(social.per.Dad.3,
                        social.recruits.per.Dad.3,
                        by="BirdID_eventSW",
                        all.x=TRUE)

# assigning 0 recruits to those social that breed in 2014 and 2015

social.fitness$soc.recruits <- ifelse(social.fitness$year!=2016,
                                      ifelse(is.na(social.fitness$soc.recruits),
                                             0,
                                             social.fitness$soc.recruits),
                                      social.fitness$soc.recruits)



##########################################################################
# Annual number of genetic fledglings reaching day 12 after hatching
##########################################################################

genetic.per.Dad <- count(offspring.12d.ped,c("GeneticDadID","Cohort"))
genetic.per.Dad.2 <- genetic.per.Dad[!(is.na(genetic.per.Dad$GeneticDadID)),]

genetic.per.Dad.2$BirdID_eventSW <- factor(paste(genetic.per.Dad.2$GeneticDadID,
                                                 genetic.per.Dad.2$Cohort,
                                                 sep="_"))


# Now I need to add those that had 0 genetic fledglings, so I need the full
# list of breeders









##########################################################################
# Annual number of genetic recruits
##########################################################################
