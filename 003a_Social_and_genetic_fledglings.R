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

offspring.12d.ped$recruited <- ifelse(offspring.12d.ped$Cohort==2016,
                                      NA,
                                      ifelse(!(offspring.12d.ped$BirdID %in% 
                                                 all.birds.breeding),
                                                0,1))


##########################################################################
# Final fitness databases
##########################################################################

##########################################################################
# Annual number of social fledglings reaching day 12 after hatching
##########################################################################

# Now I can count the annual number of social and genetic offspring reaching the age  
# of 12 days per social and genetic dad respectively. I just need to count the number 
# of times each SocialDadID2 and GeneticDadID show up in each year. 

social.per.Dad <- count(offspring.12d.ped,c("SocialDadID2","Cohort"))
social.per.Dad.2 <- social.per.Dad[!(is.na(social.per.Dad$SocialDadID2)),]



##########################################################################
# Annual number of genetic fledglings reaching day 12 after hatching
##########################################################################

genetic.per.Dad <- count(offspring.12d.ped,c("GeneticDadID","Cohort"))
genetic.per.Dad.2 <- genetic.per.Dad[!(is.na(genetic.per.Dad$GeneticDadID)),]



##########################################################################
# Annual number of social recruits
##########################################################################


##########################################################################
# Annual number of genetic recruits
##########################################################################
