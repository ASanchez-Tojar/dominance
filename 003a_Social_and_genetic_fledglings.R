# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 19th of August, 2016
# Script last updated on the 29th of August, 2016

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


# database 4
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

fledglings3 <- fledglings3[,c("BirdID","Cohort","DeathDate",
                              "BroodRef","LastLiveRecord","Source",
                              "HatchDate","FosterBrood")]

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
# 6270_2016: 1 inaccessible wild nest
# 6648_2016: 2 inaccessible wild nests
# 6688_2016: NOT EXCLUDED, only nestbox nest failed
# 6768_2016: 1 inaccessible wild nest with chicks ca. 6 days
# 6789_2016: 1 inaccessible wild nest
# 8307_2016: 1 inaccessible wild nest
# 8486_2016: 1 inaccessible wild nest
# 8642_2016: 2 accessible wild nests, but in the second chicks were ringed on day 10! So, I'll exclude this one
# 8652_2016: 1 inaccessible wild nest
# 8697_2016: NOT EXCLUDED, 1 accessible wild nest that failed
# 8706_2016: 1 inaccessible wild nest

# The list to be included with 0 social fitness for the specific year:

certain <- c("6517_2015","6778_2014","6793_2014",
             "7019_2014","7273_2014","7984_2015",
             "6688_2016","8697_2016")

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

# hist(social.per.Dad.3$soc.fledg.12d,breaks=22)

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

# hist(social.fitness$soc.recruits,breaks=3,right=FALSE)

##########################################################################
# Annual number of genetic fledglings reaching day 12 after hatching
##########################################################################

genetic.per.Dad <- count(offspring.12d.ped,c("GeneticDadID","Cohort"))
genetic.per.Dad.2 <- genetic.per.Dad[!(is.na(genetic.per.Dad$GeneticDadID)),]

genetic.per.Dad.2$BirdID_eventSW <- factor(paste(genetic.per.Dad.2$GeneticDadID,
                                                 genetic.per.Dad.2$Cohort,
                                                 sep="_"))


# Now I need to add those that had 0 genetic fledglings. For this, I'm going
# to use the full list of breeders, i.e. social and genetic. I know there will
# be some bias in the estimates for the parents of those birds that we caught
# us unringed because we obviously caught only the ones that survived long
# enough. However, there will always be some bias in the fitness estimates,
# and in this population that bias shouldn't be very big. I think it is a
# better solution that excluding birds that we catch as unringed.

# I need to create a list of all the breeders and the year

# I will start with the genetic ones as this is very easy

genetic.males.breeding.year <- unique(pedigree[pedigree$Cohort>2013 & 
                                                 !(is.na(pedigree$sire)),
                                               c("sire","Cohort")])

genetic.males.breeding.year$BirdID_eventSW <- factor(paste(genetic.males.breeding.year$sire,
                                                           genetic.males.breeding.year$Cohort,
                                                           sep="_"))


names(genetic.males.breeding.year) <- c("Dad","year","BirdID_eventSW")

# Now I need the same for the social list cleaned above. I exclude 2016 because we
# don't have the pedigree yet

social.males.breeding.year <- unique(SocialDads.year[!(is.na(SocialDads.year$SocialDadID)) &
                                                       SocialDads.year$SocialDadCertain==TRUE &                                                       
                                                       SocialDads.year$year!=2016,
                                                     c("SocialDadID",
                                                       "year","BirdID_eventSW")])


names(social.males.breeding.year) <- c("Dad","year","BirdID_eventSW")


# Now I can put both together to generate the full list of breeders per year

males.breeding.year <- unique(rbind(genetic.males.breeding.year,
                                    social.males.breeding.year))


# Now I can check which ones from that list aren't in the genetic.per.Dad.2
# database and assign them a 0 fitness

males.breeding.year$freq <- ifelse((males.breeding.year$BirdID_eventSW %in%
                                     setdiff(males.breeding.year$BirdID_eventSW,
                                             genetic.per.Dad.2$BirdID_eventSW)),
                                   0,
                                   NA)


males.breeding.year.0 <- males.breeding.year[!(is.na(males.breeding.year$freq)),
                                             c("Dad","year","freq","BirdID_eventSW")]


names(males.breeding.year.0) <- c("GeneticDadID","Cohort","freq","BirdID_eventSW")


# FINAL DATABASE

# rbinding both database

genetic.per.Dad.3 <- rbind(genetic.per.Dad.2,males.breeding.year.0)

names(genetic.per.Dad.3) <- c("GeneticDadID","year","gen.fledg.12d","BirdID_eventSW")

# hist(genetic.per.Dad.3$freq,breaks=24,right=FALSE)


##########################################################################
# Annual number of genetic recruits
##########################################################################

# I can just count the number of recruits per GeneticDad when recruited=1
# after that, I'll put it together with the previous database and assign
# 0 to those in genetic.per.Dad.3 that did not leave any

genetic.recruits.per.Dad <- count(offspring.12d.ped[offspring.12d.ped$recruited==1 & 
                                                      (!(is.na(offspring.12d.ped$recruited))),],
                                  c("GeneticDadID","Cohort"))

genetic.recruits.per.Dad.2 <- genetic.recruits.per.Dad[!(is.na(genetic.recruits.per.Dad$GeneticDadID)),]


# identifier to merge both databases

genetic.recruits.per.Dad.2$BirdID_eventSW <- factor(paste(genetic.recruits.per.Dad.2$GeneticDadID,
                                                          genetic.recruits.per.Dad.2$Cohort,
                                                          sep="_"))


genetic.recruits.per.Dad.3 <- genetic.recruits.per.Dad.2[,c("BirdID_eventSW","freq")]

names(genetic.recruits.per.Dad.3) <- c("BirdID_eventSW","gen.recruits")


##########################################################################
# Final genetic fitness database
##########################################################################

# merging fledglings and recruits

genetic.fitness <- merge(genetic.per.Dad.3,
                         genetic.recruits.per.Dad.3,
                         by="BirdID_eventSW",
                         all.x=TRUE)

# assigning 0 recruits to those social that breed in 2014 and 2015

genetic.fitness$gen.recruits <- ifelse(genetic.fitness$Cohort!=2016,
                                       ifelse(is.na(genetic.fitness$gen.recruits),
                                              0,
                                              genetic.fitness$gen.recruits),
                                       genetic.fitness$gen.recruits)


names(genetic.fitness) <- c("BirdID_eventSW","GeneticDadID",
                            "year","gen.fledg.12d","gen.recruits")

# hist(genetic.fitness$gen.recruits,breaks=4,right=FALSE)


##########################################################################
# FINAL FITNESS DATABASE
##########################################################################

# # There are quite a few individuals_year in the genetic.fitness database
# # that don't show up in the social.fitness
# 
# setdiff(genetic.fitness$BirdID_eventSW,
#         social.fitness$BirdID_eventSW)
# 
# # All the ones showing up in social.fitness but not in genetic.fitness
# # correspond to 2016, when there is no pedigree available.
# 
# setdiff(social.fitness$BirdID_eventSW,
#         genetic.fitness$BirdID_eventSW)

social.fitness.red <- social.fitness[,c("BirdID_eventSW",
                                        "soc.fledg.12d",
                                        "soc.recruits")]


fitness <- merge(genetic.fitness,
                 social.fitness.red,
                 by="BirdID_eventSW",
                 all.x=TRUE)

# Now I have to add the ones from 2016, which are still missing after that merge()

social.fitness.2016 <- social.fitness[social.fitness$year==2016,]

social.fitness.2016$gen.fledg.12d <- NA

social.fitness.2016$gen.recruits <- NA

social.fitness.2016 <- social.fitness.2016[,c("BirdID_eventSW","SocialDadID",
                                              "year","gen.fledg.12d",
                                              "gen.recruits","soc.fledg.12d",
                                              "soc.recruits")]

names(social.fitness.2016) <- c("BirdID_eventSW","GeneticDadID",
                                "year","gen.fledg.12d",
                                "gen.recruits","soc.fledg.12d",
                                "soc.recruits")



##########################################################################
# FINAL FITNESS DATABASE (for real)
##########################################################################

fitness.full <- rbind(fitness,social.fitness.2016)


names(fitness.full) <- c("BirdID_eventSW","BirdID",
                         "year","gen.fledg.12d",
                         "gen.recruits","soc.fledg.12d",
                         "soc.recruits")

fitness.full <- fitness.full[order(fitness.full$BirdID,
                                   fitness.full$year),]

row.names(fitness.full) <- NULL


