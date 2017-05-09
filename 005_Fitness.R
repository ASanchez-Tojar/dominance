# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 19th of August, 2016

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to estimate four measures of annual fitness for all breeding birds
# on Lundy from 2014 to 2016. The four measures are:

# (1) Annual number of social offspring surviving to day 12 after hatching: in short,
# the age of each bird born from 2014 to 2016 is estimated using the hatching date of
# the bird and the date of last seen alive. For each bird, I assigned a social dad ID.
# Since we crossfoster great part of the broods each year, I decided that the social
# dad ID assigned for each offspring would be the one of the dad that took care of it
# from day 2 after hatching (when we crossfoster) on. Most post-hatching mortality
# takes place from day 2 to 5 in our population (unpublished data). This measure
# of fitness allows us to look at social effects of the Dad's phenotype on nestling
# survival to independence. Importantly, this measure of fitness is the most common
# measure used in previous studies looking at bib size and fitness in house sparrows,
# which allows us to directly compare our results to what's been previously published.
# As all measures of fitness, this measure has some biases. For example, some bias
# comes from those broods for which there was no access, i.e. some of the broods
# happening outside nestboxes. To account for this, I've excluded from the list of
# social breeders all birds observed breeding in inaccessible nests and for which 
# this measure of fitness was recorded as NA. Practically, this means that the 
# estimate of this measure of fitness is NA instead of 0 for this birds. Thus, 0 
# fitness was only assigned to those birds that were observed breeding in accessible 
# nests and whose breeding failed before day 12 after hatching. Further biases come 
# from birds that bred but were not observed doing so. These is probably negligible 
# in our population as we do a lot of effor in monitoring all breeding occuring on 
# Lundy, but also, it would be accounted for in the genetic measures of fitness (2)
# and (4).

# (2) Annual number of genetic offspring surviving to day 12 after hatching: in short,
# this measure of fitness is estimated similarly to the one above. The main difference
# being that the dad assigned is the genetic dad according to the genetic pedigree of
# the population (only available for 2014 and 2015). This allows to look at genetic
# effects of the Dad's phenoype on nestling survival to independence. Importantly, 
# this measure of fitness takes into account the occurrence of extra-pair paternity. 
# There would be some bias specific to this estimate, specifically, fitness would be
# underestimated for those birds breeding in inaccessible wild nests, since we obtain 
# great part of their genetic fitness (i.e. their within-pair fitness) after their 
# offspring fledges (day 15-16 usually)

# (3) Annual number of social recruits: in short, it is estimated using the same list
# of breeders than (1). A bird is considered to be a recruit if it attempted breeding 
# at least once, this involves all sort of breeding, social and genetic.This allows to 
# look at social effects of the Dad's phenoype on nestling survival to breeding, i.e.
# recruitment. It has been argued that bib size, which is supposedly a proxy of
# dominance status, depends on the social dad (Griffith et al. 1999). We can test this
# using this measure of fitness. Recruitment in our population is a very precise
# measure due to the absent of migration and our effort which involves social and 
# genetic information.

# (4) Annual number of genetic recruits: in short, it is estimated using the same list
# of breeders than (2), and in the same way as (3). This is probably the best mesure
# of fitness as it is not affected by any bias in data collection in the previous
# season. This allows to look at genetic effects of the Dad's phenoype on nestling
# survival to breeding, for example, due to good genes.


########################################################################################################
# Packages needed
########################################################################################################

# packages needed to be loaded for this script

library(plyr)
library(ggplot2)

# Clear memory and get to know where you are
rm(list=ls())
#getwd()


##########################################################################
# Loading different datasets from sparrow main database
##########################################################################

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

# Let's start merging them to get the final one:


# Adding the LastSeeAlive date for each BirdID born between 2014 and 2016

fledglings1 <- merge(db1,db2,by="BirdID",all.x=TRUE)


# Adding the Hatching date for each BirdID born between 2014 and 2016

fledglings2 <- merge(fledglings1,db3,by="BirdID",all.x=TRUE)


# Adding the FosterBroodRef for each crossfostered BirdID born between 2014
# and 2016. Those with NA here will correspond to the non-crossfostered ones.

fledglings3 <- merge(fledglings2,db4,by="BirdID",all.x=TRUE)


# Changing the order and then the names of some variables

fledglings3 <- fledglings3[,c("BirdID","Cohort","DeathDate",
                              "BroodRef","LastLiveRecord","Source",
                              "HatchDate","FosterBrood")]

names(fledglings3) <- c("BirdID","Cohort","DeathDate",
                        "OriginalBroodRef","LastLiveRecord","Source",
                        "HatchDate","FosterBroodRef")


# getting rid off those that we don't know the hatching date from
### this step is no longer needed. Left for future changes.

fledglings4 <- fledglings3#[!(is.na(fledglings3$HatchDate)),] #no longer active


# Estimating last known age
# "+1" gives age as I expect it. This is because 4/10/2016 - 4/1/2016 = 9,
# instead of 10 days old. But for me it means 10 days alive, brain cheating :)

fledglings4$age.days <- 
  as.numeric(as.Date(fledglings4$LastLiveRecord, format="%m/%d/%Y")-
               as.Date(fledglings4$HatchDate, format="%m/%d/%Y")) + 1


# There are some NA due to some LastLiveRecord missing. Those missing values
# are of birds that died when we first saw them, i.e. we found them dead to
# start with
### this step is no longer needed. Left for future changes.

fledglings5 <- fledglings4#[!(is.na(fledglings4$age.days)),] #no longer active


# Now I want to create a variable with the BroodRef where the individual lived from
# day 2 (i.e. day of crossfostering) on. See description above. Therefore, if a bird
# has a fosterbrood, that's the reference it's assigned to it, otherwise, the
# original one it's assigned. For birds caught as fledglings on, BroodRef is uknown

fledglings5$twodaysonBroodRef<-fledglings5$FosterBroodRef

fledglings5$twodaysonBroodRef<-ifelse(is.na(fledglings5$twodaysonBroodRef),
                                      fledglings5$OriginalBroodRef,
                                      fledglings5$twodaysonBroodRef)


##########################################################################
# Assigning social dads to each BirdID
##########################################################################

# Now, I will add the ID of the Social Dad that took care of the individual 
# from day 2 on. 


# First, load the database with BroodRef and SocialDadID (updated from script 003aa)

SocialDads <- read.table("BroodRef_SocialDadID_IDCertain_2014-2016_Updated.csv",
                         header=TRUE,sep=",")


# merging it in relation to twodaysonBroodRef so that I assign to each bird
# the dad that took care of him at least from day 2 on.

fledglings6 <- merge(fledglings5,SocialDads,
                     by.x="twodaysonBroodRef",
                     by.y="BroodRef",
                     all.x=TRUE)


##########################################################################
# Assigning social mums to each BirdID
##########################################################################

# Now, I will add the ID of the Social Mum that took care of the individual 
# from day 2 on. 


# First, load the database with BroodRef and SocialMumID (updated from script 003aa)

SocialMums <- read.table("BroodRef_SocialMumID_IDCertain_2014-2016_Updated.csv",
                         header=TRUE,sep=",")


# merging it in relation to twodaysonBroodRef so that I assign to each bird
# the mum that took care of him at least from day 2 on.

fledglings6 <- merge(fledglings6,SocialMums,
                     by.x="twodaysonBroodRef",
                     by.y="BroodRef",
                     all.x=TRUE)


##########################################################################
# Assigning genetic dads to each BirdID
##########################################################################

# Now I'm going to add the genetic dad from the pedigree. The pedigree
# is the sum of three different files. This is because I'm adding the 
# 2016 pedigree as well as the vanished individuals.

pedigree.a <- read.table("fledglings12/PedigreeUpToIncl2015-versionwithNA.txt",
                       header=TRUE,sep="\t")

pedigree.b <- read.table("fledglings12/mystery_disappear_JP.csv",
                         header=TRUE,sep=",")

pedigree.c <- read.table("fledglings12/2016_Pedigree_NdR.csv",
                         header=TRUE,sep=",")

pedigree.c.2 <- pedigree.c[pedigree.c$BirdID!="BPS0861",]


# preparing final pedigree database

pedigree.a.2 <- pedigree.a[,c("id","dam","sire","Cohort")]


pedigree.b.2 <- pedigree.b[,c("BirdID","MotherID","FatherID","Cohort")]
names(pedigree.b.2) <- c("id","dam","sire","Cohort")


pedigree.c.3 <- pedigree.c.2[,c("BirdID","GeneticMother","GeneticFather")]
names(pedigree.c.3) <- c("id","dam","sire")


cohort <- read.table("fledglings12/BirdID_Cohort_FULLdb_20170509.csv",
                         header=TRUE,sep=",")

pedigree.c.4 <- merge(pedigree.c.3,cohort,
                      by.x="id",by.y="BirdID",all.x=TRUE)


pedigree <- rbind(pedigree.a.2,pedigree.b.2,pedigree.c.4)
pedigree$id <- as.numeric(pedigree$id)
pedigree <- pedigree[order(pedigree$id),]


# reducing database before merging

pedigree.red <- pedigree[,c("id","sire")]
names(pedigree.red) <- c("BirdID","GeneticDadID")


fledglings6.ped <- merge(fledglings6,pedigree.red,
                           by="BirdID",
                           all.x=TRUE)


##########################################################################
# Assigning genetic mums to each BirdID
##########################################################################

# reducing database before merging

pedigree.f.red <- pedigree[,c("id","dam")]
names(pedigree.f.red) <- c("BirdID","GeneticMumID")


fledglings6.ped <- merge(fledglings6.ped,pedigree.f.red,
                         by="BirdID",
                         all.x=TRUE)


##########################################################################
# Generating a list of breeders from 2014 to 2016
##########################################################################

##########################################################################
# MALES
##########################################################################

# List of genetic fathers from 2014-2016 extracted from pedigree:
# Notice that the genetic pedigree isn't available birds caught
# unringed in February 2016.

genetic.males.breeding <- unique(pedigree[pedigree$Cohort>2013 & 
                                            !(is.na(pedigree$sire)),
                                   c("sire")])


# Adding those that pop up as genetic but not social parents

# First making a clean list of social dads

SocialDads.list <- unique(SocialDads[!(is.na(SocialDads$SocialDadID)) & 
                                       SocialDads$SocialDadCertain==TRUE,
                                     c("SocialDadID")])

# Then adding those that pop up as genetic but not as social to make
# a full list of breeders.

all.males.breeding <- c(SocialDads.list,
                        setdiff(genetic.males.breeding,SocialDads.list))


##########################################################################
# FEMALES
##########################################################################

# First, load the updated database with all social females. From script 003aa

SocialMums <- read.table("BroodRef_SocialMumID_IDCertain_2014-2016_Updated.csv",
                         header=TRUE,sep=",")


# List of genetic mums from 2014-2016 extracted from pedigree:
# Notice that the genetic pedigree isn't available for 2016 or birds caught
# unringed in February 2016.

genetic.females.breeding <- unique(pedigree[pedigree$Cohort>2013 & 
                                              !(is.na(pedigree$dam)),
                                            c("dam")])

# Adding those that pop up as genetic but not social parents

# First making a clean list of social mums

SocialMums.list <- unique(SocialMums[!(is.na(SocialMums$SocialMumID)) & 
                                       SocialMums$SocialMumCertain==TRUE,
                                     c("SocialMumID")])


# Then adding those that pop up as genetic but not as social to make
# a full list of breeders.

all.females.breeding <- c(SocialMums.list, 
                          setdiff(genetic.females.breeding,SocialMums.list))


##########################################################################
# Final breeder list (males and females)
##########################################################################

all.birds.breeding <- c(all.males.breeding,all.females.breeding)
                                               

##########################################################################
# Generating a variable for recruits: 0 = didn't recruit, 1 = recruited
##########################################################################

# This is, if BirdID showed up in all.birds.breeding, it means that it
# recruited. Note that 2016 is excluded as we don't have data for the 
# following season, i.e. no data for 2017 yet.

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

# Subsetting the database according to those that survived, at least, to 
# day 12. This way we can estimate the number of fledglings per individual. 
# I leave NAs because I also want to estimate the number of genetic fledglings. 
# But I remove those age.days = NA for which HatchDate is known, this is 
# because these ones refer to chicks that were first recorded as dead 
# individuals, i.e. they did not make it to 12 days old

offspring.12d.ped <- subset(fledglings6.ped,
                            fledglings6.ped$age.days>11 | 
                              (is.na(fledglings6.ped$age.days) & 
                                 is.na(fledglings6.ped$HatchDate))
)



##########################################################################
# Final fitness databases
##########################################################################

##########################################################################
# MALES
##########################################################################

##########################################################################
# Annual number of social fledglings reaching day 12 after hatching
##########################################################################

# Now I can count the annual number of social offspring reaching the age
# of 12 days per social dad. I just need to count the number of times each 
# SocialDadID shows up in each year. 

social.per.Dad <- count(offspring.12d.ped,c("SocialDadID","Cohort"))

# Getting ridd off the counts for SocialDadID==NA
social.per.Dad.2 <- social.per.Dad[!(is.na(social.per.Dad$SocialDadID)),]


# To include birds with 0 fitness, I need to check all social breeders of 
# each year to assigned them 0 only if they are not in social.per.Dad.2


# First, load a database with BroodRef and BroodName to get year for each
# BroodRef in SocialDads, which is the full list of social breeders

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


# I also create an identifier for social.per.Dad.2, the social fledgling 
# fitness database that I need to add 0's to

social.per.Dad.2$BirdID_eventSW <- factor(paste(social.per.Dad.2$SocialDadID,
                                                social.per.Dad.2$Cohort,
                                                sep="_"))


# I need to check all those socialdads that could potentially be assigned 
# with 0 fitness. This is because I need to check if I can really say that.

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

# After all, the list of birds for which I'm confident enough that they
# have 0 social fitness is:

certain <- c("6517_2015","6778_2014","6793_2014",
             "7019_2014","7273_2014","7984_2015",
             "6688_2016","8697_2016")

# subsetting those that we can assign a 0 fitness from the SocialDads.year database

social.fitness.0 <-unique(SocialDads.year[!(is.na(SocialDads.year$SocialDadID)) &
                                            SocialDads.year$SocialDadCertain==TRUE &
                                            (SocialDads.year$BirdID_eventSW %in% certain),
                                          c("SocialDadID","year","BirdID_eventSW")])

# Setting their fitness to 0

social.fitness.0$freq <- 0


# Changing the order of the variables for rbinding later on

social.fitness.0 <- social.fitness.0[,c("SocialDadID","year","freq","BirdID_eventSW")]


# FINAL DATABASE

# rbinding both database

names(social.per.Dad.2) <- c("SocialDadID","year","freq","BirdID_eventSW")

social.per.Dad.3 <- rbind(social.per.Dad.2,social.fitness.0)

names(social.per.Dad.3) <- c("SocialDadID","year","soc.fledg.12d","BirdID_eventSW")

# hist(social.per.Dad.3$soc.fledg.12d,breaks=22,right=FALSE)



##########################################################################
# Annual number of social recruits
##########################################################################

# I can just count the number of recruits per SocialDad when recruited=1
# after that, I'll put it together with the previous database and assign
# 0 to those in social.per.Dad.3 that did not leave any

social.recruits.per.Dad <- count(offspring.12d.ped[offspring.12d.ped$recruited==1 & 
                                                     (!(is.na(offspring.12d.ped$recruited))),],
                                 c("SocialDadID","Cohort"))


# Getting ridd off the counts for SocialDadID==NA

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

# assigning 0 recruits to those social dads with soc.recruits==NA that 
# breed in 2014 and 2015. 2016 excluded because we don't know yet.

social.fitness$soc.recruits <- ifelse(social.fitness$year!=2016,
                                      ifelse(is.na(social.fitness$soc.recruits),
                                             0,
                                             social.fitness$soc.recruits),
                                      social.fitness$soc.recruits)

# hist(social.fitness$soc.recruits,breaks=3,right=FALSE)



##########################################################################
# Annual number of genetic fledglings reaching day 12 after hatching
##########################################################################

# Now I can count the annual number of genetic offspring reaching the age
# of 12 days per genetic dad. I just need to count the number of times each 
# GeneticDadID shows up in each year. 

genetic.per.Dad <- count(offspring.12d.ped,c("GeneticDadID","Cohort"))


# Getting ridd off the counts for GeneticDadID==NA

genetic.per.Dad.2 <- genetic.per.Dad[!(is.na(genetic.per.Dad$GeneticDadID)),]


# creating a BirdID_year identifier for later on

genetic.per.Dad.2$BirdID_eventSW <- factor(paste(genetic.per.Dad.2$GeneticDadID,
                                                 genetic.per.Dad.2$Cohort,
                                                 sep="_"))


# Now I need to add those that had 0 genetic fledglings. For this, I'm going
# to use the full list of breeders, i.e. social and genetic. 

# I need to create a list of all the breeders and the year, I will start with 
# the genetic ones as that is very easy

genetic.males.breeding.year <- unique(pedigree[pedigree$Cohort>2013 & 
                                                 !(is.na(pedigree$sire)),
                                               c("sire","Cohort")])


# creating a BirdID_year identifier for later on

genetic.males.breeding.year$BirdID_eventSW <- factor(paste(genetic.males.breeding.year$sire,
                                                           genetic.males.breeding.year$Cohort,
                                                           sep="_"))


names(genetic.males.breeding.year) <- c("Dad","year","BirdID_eventSW")


# Now I need the same for the social list cleaned above. I don't exclude 2016 
# because we now have the pedigree for 2016

social.males.breeding.year <- unique(SocialDads.year[!(is.na(SocialDads.year$SocialDadID)) &
                                                       SocialDads.year$SocialDadCertain==TRUE, #&                                                       
                                                       #SocialDads.year$year!=2016,
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


# selecting only those few with 0 fitness

males.breeding.year.0 <- males.breeding.year[!(is.na(males.breeding.year$freq)),
                                             c("Dad","year","freq","BirdID_eventSW")]


names(males.breeding.year.0) <- c("GeneticDadID","Cohort","freq","BirdID_eventSW")


# FINAL DATABASE

# rbinding both database

genetic.per.Dad.3 <- rbind(genetic.per.Dad.2,males.breeding.year.0)

names(genetic.per.Dad.3) <- c("GeneticDadID","year","gen.fledg.12d","BirdID_eventSW")

# hist(genetic.per.Dad.3$gen.fledg.12d,breaks=24,right=FALSE)



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

# assigning 0 recruits to those that breed in 2014 and 2015

genetic.fitness$gen.recruits <- ifelse(genetic.fitness$year!=2016,
                                       ifelse(is.na(genetic.fitness$gen.recruits),
                                              0,
                                              genetic.fitness$gen.recruits),
                                       genetic.fitness$gen.recruits)


# names(genetic.fitness) <- c("BirdID_eventSW","GeneticDadID",
#                             "year","gen.fledg.12d","gen.recruits")

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
# # However, All the ones showing up in social.fitness but not in 
# # genetic.fitness correspond to 2016, when there is no pedigree available.
# # In fact, since we've added 2016, there are no longer individuals that 
# # show up in social.fitness but not in genetic.fitness
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

# # This does not longer applies as the pedigree of 2016 is now available
# # Now I have to add the ones from 2016, which are still missing after that merge()
# 
# social.fitness.2016 <- social.fitness[social.fitness$year==2016,]
# 
# social.fitness.2016$gen.fledg.12d <- NA
# 
# social.fitness.2016$gen.recruits <- NA
# 
# social.fitness.2016 <- social.fitness.2016[,c("BirdID_eventSW","SocialDadID",
#                                               "year","gen.fledg.12d",
#                                               "gen.recruits","soc.fledg.12d",
#                                               "soc.recruits")]
# 
# names(social.fitness.2016) <- c("BirdID_eventSW","GeneticDadID",
#                                 "year","gen.fledg.12d",
#                                 "gen.recruits","soc.fledg.12d",
#                                 "soc.recruits")



##########################################################################
# FINAL FITNESS DATABASE (for real)
##########################################################################

fitness.full <- fitness

# fitness.full <- rbind(fitness,social.fitness.2016)
# 
# 
names(fitness.full) <- c("BirdID_eventSW","BirdID",
                         "year","gen.fledg.12d",
                         "gen.recruits","soc.fledg.12d",
                         "soc.recruits")
# 
# fitness.full <- fitness.full[order(fitness.full$BirdID,
#                                    fitness.full$year),]
# 
# row.names(fitness.full) <- NULL
# 
# cor(fitness.full[,c("gen.fledg.12d",
#                     "gen.recruits","soc.fledg.12d",
#                     "soc.recruits")],use="complete.obs")



##########################################################################
# FEMALES
##########################################################################

##########################################################################
# Annual number of social fledglings reaching day 12 after hatching
##########################################################################

# Now I can count the annual number of social offspring reaching the age
# of 12 days per social mum. I just need to count the number of times each 
# SocialMumID shows up in each year. 

social.per.Mum <- count(offspring.12d.ped,c("SocialMumID","Cohort"))

# Getting ridd off the counts for SocialMumID==NA
social.per.Mum.2 <- social.per.Mum[!(is.na(social.per.Mum$SocialMumID)),]


# To include birds with 0 fitness, I need to check all social breeders each 
# year to assigned them 0 if they are not in social.per.Mum.2


# First, load a database with BroodRef and BroodName to get year for each
# BroodRef in SocialMums, which is the full list of social breeders. DONE ABOVE
# 
# # SELECT tblBroods.BroodRef, tblBroods.BroodName
# # FROM tblBroods;
# 
# BroodName <- read.table("BroodRef_BroodName.csv",
#                         header=TRUE,sep=",")


# Merge with SocialDads to get the Name and, subsequently, the year

SocialMums.year <- merge(SocialMums,BroodName,
                         by="BroodRef",all.x=TRUE)


# getting the year Code and then the year

SocialMums.year$yearCode <- factor(substr(SocialMums.year$BroodName, 1, 1))

SocialMums.year$year <- ifelse(SocialMums.year$yearCode=="N",
                               2014,
                               ifelse(SocialMums.year$yearCode=="O",
                                      2015,2016))


# Creating an identifier bird_year to subset later on

SocialMums.year$BirdID_eventSW <- as.factor(ifelse(!(is.na(SocialMums.year$SocialMumID)),
                                                   paste(SocialMums.year$SocialMumID,
                                                         SocialMums.year$year,
                                                         sep="_"),
                                                   NA))


# I also create an identifier for social.per.Mum.2, the social fledgling 
# fitness database that I need to add 0's to

social.per.Mum.2$BirdID_eventSW <- factor(paste(social.per.Mum.2$SocialMumID,
                                                social.per.Mum.2$Cohort,
                                                sep="_"))


# I need to check all those socialmums that could potentially be assigned 
# with 0 fitness. This is because I need to check if I can really say that.

# sort(setdiff(SocialMums.year$BirdID_eventSW,
#         social.per.Mum.2$BirdID_eventSW))


# This is the list of supposedly 0 fitness birds, but most are birds for which
# we cannot be sure. I've checked them in the database:

# This is the list for which fitness=0 is uncertain

# 4898_2014: 1 inaccessible wild nest
# 5136_2014: 1 inaccessible wild nest
# 5589_2015: NOT EXCLUDED, only nestbox nest failed
# 6245_2014: 1 inaccessible wild nest
# 6267_2015: 1 inaccessible wild nest
# 6269_2015: 3 inaccessible wild nests
# 6392_2015: 1 inaccessible wild nest
# 6415_2014: 1 inaccessible wild nest
# 6421_2014: 1 inaccessible wild nest
# 6472_2015: 2 inaccessible wild nests
# 6508_2014: NOT EXCLUDED, only nestbox nest failed
# 6526_2014: NOT EXCLUDED, only nestbox nests that failed
# 6544_2014: 2 inaccessible wild nests
# 6640_2014: it had 2 genetic offspring in 2014
# 6724_2014: NOT EXCLUDED, only nestbox nest failed
# 6755_2015: 1 inaccessible wild nest
# 6772_2014: it had 4 genetic offspring in 2014
# 6848_2015: 1 endoscope counting on day ca. 10 = 2 chicks
# 6884_2014: NOT EXCLUDED, only wild nest failed
# 6925_2014: it had 5 genetic offspring in 2014
# 6931_2014: 1 inaccessible wild nest
# 6952_2014: NOT EXCLUDED, 2 wild nests that failed
# 6952_2016: NOT EXCLUDED, 2 nestbox nests that failed
# 6985_2014: 1 inaccessible wild nest
# 7011_2015: 1 endoscope counting on day ca. 12 = 1 chicks
# 7114_2014: NOT EXCLUDED, only wild nest failed
# 7276_2015: 1 nestbox nest abandoned because of human reasons?
# 7894_2015: it had 3 genetic offspring in 2015
# 8354_2016: 1 inaccessible wild nest
# 8384_2016: 1 inaccessible wild nest
# 8413_2016: chicks ringed before day 12
# 8555_2016: chicks ringed before day 12
# 8627_2016: NOT EXCLUDED, only wild nest failed


# After all, the list of birds for which I'm confident enough that they
# have 0 social fitness is:

certain.f <- c("5589_2015","6508_2014","6526_2014",
             "6724_2014","6884_2014","6952_2014",
             "6952_2016","7114_2014","8627_2016")

# subsetting those that we can assign a 0 fitness from the SocialDads.year database

social.fitness.f.0 <-unique(SocialMums.year[!(is.na(SocialMums.year$SocialMumID)) &
                                              SocialMums.year$SocialMumCertain==TRUE &
                                            (SocialMums.year$BirdID_eventSW %in% certain.f),
                                          c("SocialMumID","year","BirdID_eventSW")])

# Setting their fitness to 0

social.fitness.f.0$freq <- 0


# Changing the order of the variables for rbinding later on

social.fitness.f.0 <- social.fitness.f.0[,c("SocialMumID","year","freq","BirdID_eventSW")]


# FINAL DATABASE

# rbinding both database

names(social.per.Mum.2) <- c("SocialMumID","year","freq","BirdID_eventSW")

social.per.Mum.3 <- rbind(social.per.Mum.2,social.fitness.f.0)

names(social.per.Mum.3) <- c("SocialMumID","year","soc.fledg.12d","BirdID_eventSW")

# hist(social.per.Mum.3$soc.fledg.12d,breaks=12)



##########################################################################
# Annual number of social recruits
##########################################################################

# I can just count the number of recruits per SocialMum when recruited=1
# after that, I'll put it together with the previous database and assign
# 0 to those in social.per.Mum.3 that did not leave any

social.recruits.per.Mum <- count(offspring.12d.ped[offspring.12d.ped$recruited==1 & 
                                                     (!(is.na(offspring.12d.ped$recruited))),],
                                 c("SocialMumID","Cohort"))


# Getting ridd off the counts for SocialMumID==NA

social.recruits.per.Mum.2 <- social.recruits.per.Mum[!(is.na(social.recruits.per.Mum$SocialMumID)),]


# identifier to merge both databases

social.recruits.per.Mum.2$BirdID_eventSW <- factor(paste(social.recruits.per.Mum.2$SocialMumID,
                                                         social.recruits.per.Mum.2$Cohort,
                                                         sep="_"))


social.recruits.per.Mum.3 <- social.recruits.per.Mum.2[,c("BirdID_eventSW","freq")]

names(social.recruits.per.Mum.3) <- c("BirdID_eventSW","soc.recruits")


##########################################################################
# Final social fitness database
##########################################################################

# merging fledglings and recruits

social.fitness.f <- merge(social.per.Mum.3,
                          social.recruits.per.Mum.3,
                          by="BirdID_eventSW",
                          all.x=TRUE)

# assigning 0 recruits to those social mums with soc.recruits==NA that 
# bred in 2014 and 2015. 2016 excluded because we don't know yet.

social.fitness.f$soc.recruits <- ifelse(social.fitness.f$year!=2016,
                                      ifelse(is.na(social.fitness.f$soc.recruits),
                                             0,
                                             social.fitness.f$soc.recruits),
                                      social.fitness.f$soc.recruits)

# hist(social.fitness.f$soc.recruits,breaks=3,right=FALSE)



##########################################################################
# Annual number of genetic fledglings reaching day 12 after hatching
##########################################################################

# Now I can count the annual number of genetic offspring reaching the age
# of 12 days per genetic mum. I just need to count the number of times each 
# GeneticMumID shows up in each year. 

genetic.per.Mum <- count(offspring.12d.ped,c("GeneticMumID","Cohort"))


# Getting ridd off the counts for GeneticMumID==NA

genetic.per.Mum.2 <- genetic.per.Mum[!(is.na(genetic.per.Mum$GeneticMumID)),]


# creating a BirdID_year identifier for later on

genetic.per.Mum.2$BirdID_eventSW <- factor(paste(genetic.per.Mum.2$GeneticMumID,
                                                 genetic.per.Mum.2$Cohort,
                                                 sep="_"))


# Now I need to add those that had 0 genetic fledglings. For this, I'm going
# to use the full list of breeders, i.e. social and genetic. 

# I need to create a list of all the breeders and the year, I will start with 
# the genetic ones as that is very easy

genetic.females.breeding.year <- unique(pedigree[pedigree$Cohort>2013 & 
                                                 !(is.na(pedigree$dam)),
                                               c("dam","Cohort")])


# creating a BirdID_year identifier for later on

genetic.females.breeding.year$BirdID_eventSW <- factor(paste(genetic.females.breeding.year$dam,
                                                             genetic.females.breeding.year$Cohort,
                                                           sep="_"))


names(genetic.females.breeding.year) <- c("Mum","year","BirdID_eventSW")


# Now I need the same for the social list cleaned above. I don't exclude 2016 
# anymore because we have the pedigree now

social.females.breeding.year <- unique(SocialMums.year[!(is.na(SocialMums.year$SocialMumID)) &
                                                         SocialMums.year$SocialMumCertain==TRUE,# &                                                       
                                                         #SocialMums.year$year!=2016,
                                                     c("SocialMumID",
                                                       "year","BirdID_eventSW")])


names(social.females.breeding.year) <- c("Mum","year","BirdID_eventSW")


# Now I can put both together to generate the full list of breeders per year

females.breeding.year <- unique(rbind(genetic.females.breeding.year,
                                    social.females.breeding.year))


# Now I can check which ones from that list aren't in the genetic.per.Mum.2
# database and assign them a 0 fitness

females.breeding.year$freq <- ifelse((females.breeding.year$BirdID_eventSW %in%
                                      setdiff(females.breeding.year$BirdID_eventSW,
                                              genetic.per.Mum.2$BirdID_eventSW)),
                                     0,
                                     NA)


# selecting only those few with 0 fitness

females.breeding.year.0 <- females.breeding.year[!(is.na(females.breeding.year$freq)),
                                                 c("Mum","year","freq","BirdID_eventSW")]


names(females.breeding.year.0) <- c("GeneticMumID","Cohort","freq","BirdID_eventSW")


# FINAL DATABASE

# rbinding both database

genetic.per.Mum.3 <- rbind(genetic.per.Mum.2,females.breeding.year.0)

names(genetic.per.Mum.3) <- c("GeneticMumID","year","gen.fledg.12d","BirdID_eventSW")

# hist(genetic.per.Mum.3$gen.fledg.12d,breaks=12,right=FALSE)



##########################################################################
# Annual number of genetic recruits
##########################################################################

# I can just count the number of recruits per GeneticMum when recruited=1
# after that, I'll put it together with the previous database and assign
# 0 to those in genetic.per.Mum.3 that did not leave any

genetic.recruits.per.Mum <- count(offspring.12d.ped[offspring.12d.ped$recruited==1 & 
                                                      (!(is.na(offspring.12d.ped$recruited))),],
                                  c("GeneticMumID","Cohort"))

genetic.recruits.per.Mum.2 <- genetic.recruits.per.Mum[!(is.na(genetic.recruits.per.Mum$GeneticMumID)),]


# identifier to merge both databases

genetic.recruits.per.Mum.2$BirdID_eventSW <- factor(paste(genetic.recruits.per.Mum.2$GeneticMumID,
                                                          genetic.recruits.per.Mum.2$Cohort,
                                                          sep="_"))


genetic.recruits.per.Mum.3 <- genetic.recruits.per.Mum.2[,c("BirdID_eventSW","freq")]

names(genetic.recruits.per.Mum.3) <- c("BirdID_eventSW","gen.recruits")



##########################################################################
# Final genetic fitness database
##########################################################################

# merging fledglings and recruits

genetic.fitness.f <- merge(genetic.per.Mum.3,
                           genetic.recruits.per.Mum.3,
                         by="BirdID_eventSW",
                         all.x=TRUE)

# assigning 0 recruits to those social that breed in 2014 and 2015,
# we don't know yet for 2016 as 2017 breeding season isn't over yet!

genetic.fitness.f$gen.recruits <- ifelse(genetic.fitness.f$year!=2016,
                                       ifelse(is.na(genetic.fitness.f$gen.recruits),
                                              0,
                                              genetic.fitness.f$gen.recruits),
                                       genetic.fitness.f$gen.recruits)


# names(genetic.fitness) <- c("BirdID_eventSW","GeneticDadID",
#                             "year","gen.fledg.12d","gen.recruits")

# hist(genetic.fitness$gen.recruits,breaks=4,right=FALSE)


##########################################################################
# FINAL FITNESS DATABASE
##########################################################################

# # There are quite a few individuals_year in the genetic.fitness database
# # that don't show up in the social.fitness
# 
# setdiff(genetic.fitness.f$BirdID_eventSW,
#         social.fitness.f$BirdID_eventSW)
# 
# # However, All the ones showing up in social.fitness but not in 
# # genetic.fitness correspond to 2016, when there is no pedigree available.
# # In fact, since we've added 2016, there are no longer individuals that 
# # show up in social.fitness but not in genetic.fitness
# 
# setdiff(social.fitness.f$BirdID_eventSW,
#         genetic.fitness.f$BirdID_eventSW)

social.fitness.f.red <- social.fitness.f[,c("BirdID_eventSW",
                                            "soc.fledg.12d",
                                            "soc.recruits")]


fitness.f <- merge(genetic.fitness.f,
                 social.fitness.f.red,
                 by="BirdID_eventSW",
                 all.x=TRUE)


# # This does not longer applies as the pedigree of 2016 is now available
# # Now I have to add the ones from 2016, which are still missing after that merge()
# 
# social.fitness.f.2016 <- social.fitness.f[social.fitness.f$year==2016,]
# 
# social.fitness.f.2016$gen.fledg.12d <- NA
# 
# social.fitness.f.2016$gen.recruits <- NA
# 
# social.fitness.f.2016 <- social.fitness.f.2016[,c("BirdID_eventSW","SocialMumID",
#                                                   "year","gen.fledg.12d",
#                                                   "gen.recruits","soc.fledg.12d",
#                                                   "soc.recruits")]
# 
# names(social.fitness.f.2016) <- c("BirdID_eventSW","GeneticMumID",
#                                   "year","gen.fledg.12d",
#                                   "gen.recruits","soc.fledg.12d",
#                                   "soc.recruits")



##########################################################################
# FINAL FITNESS DATABASE (for real)
##########################################################################

fitness.f.full <- fitness.f

# fitness.f.full <- rbind(fitness.f,social.fitness.f.2016)


names(fitness.f.full) <- c("BirdID_eventSW","BirdID",
                           "year","gen.fledg.12d",
                           "gen.recruits","soc.fledg.12d",
                           "soc.recruits")

fitness.f.full <- fitness.f.full[order(fitness.f.full$BirdID,
                                       fitness.f.full$year),]

# row.names(fitness.f.full) <- NULL

# cor(fitness.f.full[,c("gen.fledg.12d",
#                     "gen.recruits","soc.fledg.12d",
#                     "soc.recruits")],use="complete.obs")


##########################################################################
# FINAL DATABASE with BOTH SEXES
##########################################################################

# rbinding both databases

fitness.full.both <- rbind(fitness.full,fitness.f.full)


write.csv(fitness.full.both,"fledglings12/fitness.full.both.csv",row.names=FALSE)




# # Saving some histograms
# 
# p1 <- ggplot(fitness.full, aes(x=gen.fledg.12d, fill=as.factor(year))) +
#   geom_histogram(binwidth=1, alpha=.5, position="identity")
# 
# p2 <- ggplot(fitness.full, aes(x=gen.recruits, fill=as.factor(year))) +
#   geom_histogram(binwidth=1, alpha=.5, position="identity")
# 
# p3 <- ggplot(fitness.full, aes(x=soc.fledg.12d, fill=as.factor(year))) +
#   geom_histogram(binwidth=1, alpha=.5, position="identity")
# 
# p4 <- ggplot(fitness.full, aes(x=soc.recruits, fill=as.factor(year))) +
#   geom_histogram(binwidth=1, alpha=.5, position="identity")
# 
# multiplot(p1,p3,p2,p4, cols=2)
# 
# 
# # Multiple plot function, from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# #
# # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# # - cols:   Number of columns in layout
# # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# #
# # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# # then plot 1 will go in the upper left, 2 will go in the upper right, and
# # 3 will go all the way across the bottom.
# #
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     print(plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
