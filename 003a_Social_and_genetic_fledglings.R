# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 19th of August, 2016
# Script last updated on the 23rd of August, 2016

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

# First, load the database with BroodRef and SocialDadID
# SELECT tblBroods.BroodRef, tblBroods.SocialDadID, tblBroods.SocialDadCertain
# FROM tblBroods;

SocialDads <- read.table("fledglings12/BroodRef_SocialDadID_IDCertain.csv",
                         header=TRUE,sep=",")


# merging it in relation to twodaysonBroodRef

fledglings6 <- merge(fledglings5,SocialDads,
                     by.x="twodaysonBroodRef",
                     by.y="BroodRef",
                     all.x=TRUE)


# Generating a list with birds whose socialDadID is missing, I'd like to look at the
# specific cases as there are not too many. I can probably get the ID of some of the
# Social Dads

fledglings6.noDad <- fledglings6[is.na(fledglings6$SocialDadID),]


# I've checked the database, the book and the pedigree for the following broods:
# unique(fledglings6.noDad$twodaysonBroodRef)
# Based on partial ring sightings with max. two missing rings and the pedigree information,
# This is what I've decided:

##########################################################################################

# *BroodRef 1639: N001: Social Dad's rings = RR/ZM (book). 
#  Genetic father of all 3 offspring in brood = RR/MW.
#  Therefore, SocialDadID2 <- 4895

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1639,
                                   4895,
                                   fledglings6$SocialDadID)


# *BroodRef 1647: N009: Social Dad's rings = BN/ZM (book). 
#  Genetic father of all 2 offspring in brood = BN/MD.
#  Therefore, SocialDadID2 <- 7267

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1647,
                                   7267,
                                   fledglings6$SocialDadID2)


# *BroodRef 1648: N010: Dad's rings = ZB/MD (book).
#  Genetic father of all 4 offspring in brood = BR/MD
#  Therefore, SocialDadID2 <- 7268

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1648,
                                   7268,
                                   fledglings6$SocialDadID2)


# *BroodRef 1655: N017: Social Dad's rings = ZM/DN (book). 
#  Genetic father of the only offspring in brood = MO/DN
#  Therefore, SocialDadID2 <- 6391

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1655,
                                   6391,
                                   fledglings6$SocialDadID2)


# *BroodRef 1665: N027: Dad's rings = ZZ/MO (book).
#  Genetic father of all 4 offspring in brood = DR/MO
#  Therefore, SocialDadID2 <- 6532

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1665,
                                   6532,
                                   fledglings6$SocialDadID2)


# *BroodRef 1675: N037: Dad's rings = ZM/RW (book).
#  Genetic father of all 4 offspring in brood = MO/RW
#  Therefore, SocialDadID2 <- 6280

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1675,
                                   6280,
                                   fledglings6$SocialDadID2)


# *BroodRef 1683: N045: Dad's rings = ZZ/MO (book).
#  Genetic father of all 4 offspring in brood = DY/MO
#  Therefore, SocialDadID2 <- 6527

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1683,
                                   6527,
                                   fledglings6$SocialDadID2)


# *BroodRef 1690: N052: Dad's rings = ZO/OM (book).
#  Genetic father of 4 out of 5 offspring in brood = OR/OM. The other is: CO/MC, so discarded
#  Therefore, SocialDadID2 <- 6581

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1690,
                                   6581,
                                   fledglings6$SocialDadID2)


# *BroodRef 1692: N054: Social Dad's rings = ZB/MD (book). 
#  Genetic father of 3 out of 5 offspring in brood = BN/MD. The other is: MC/OO, so discarded
#  Therefore, SocialDadID2 <- 7267

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1692,
                                   7267,
                                   fledglings6$SocialDadID2)


# *BroodRef 1696: N058: Dad's rings = MD/ZZ (book).
#  Genetic father of 2 out 3 offspring in brood = MD/DC. The other is OV/MO.
#  Therefore, SocialDadID2 <- 6847

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1696,
                                   6847,
                                   fledglings6$SocialDadID2)


# *BroodRef 1698: N060: Dad's rings = ZM/ZB (book).
#  Genetic father of all 3 offspring in brood = DM/OB.
#  Therefore, SocialDadID2 <- 6924

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1698,
                                   6924,
                                   fledglings6$SocialDadID2)


# *BroodRef 1709: N071: Dad's rings = ZM/DN (book).
#  Genetic father of all 4 offspring in brood = MO/DN
#  Therefore, SocialDadID2 <- 6391

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1709,
                                   6391,
                                   fledglings6$SocialDadID2)


# *BroodRef 1710: N072: Dad's rings = RR/ZM (book).
#  Genetic father of all 5 offspring in brood = RR/MW
#  Therefore, SocialDadID2 <- 4895

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1710,
                                   4895,
                                   fledglings6$SocialDadID2)


# *BroodRef 1723: N085: Dad's rings = ZM/YB (book).
#  Genetic father of all 3 offspring in brood = DM/YB
#  Therefore, SocialDadID2 <- 7274

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1723,
                                   7274,
                                   fledglings6$SocialDadID2)


# *BroodRef 1726: N088: Dad's rings = ZZ/MC (book). 
#  Genetic father of all 4 offspring in brood = RG/MC
#  Therefore, SocialDadID2 <- 5139

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1726,
                                   5139,
                                   fledglings6$SocialDadID2)


# *BroodRef 1738: N100: Dad's rings = ZM/ZW (book). 
#  Genetic father of all 4 offspring in brood = BM/WB
#  Therefore, SocialDadID2 <- 4757

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1738,
                                   4757,
                                   fledglings6$SocialDadID2)


# *BroodRef 1777: N139: Dad's rings = MD/ZZ (book). 
#  Genetic father of all 4 offspring in brood = MD/DC
#  Therefore, SocialDadID2 <- 6847

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1777,
                                   6847,
                                   fledglings6$SocialDadID2)


# *BroodRef 1892: O013: Social Dad's rings = ZR/MO (database).
#  Genetic father of 3 out 4 offspring in brood = BR/MO.The other is: OG/MC, so discarded
#  Therefore, SocialDadID2 <- 6492

fledglings6$SocialDadID2 <- ifelse(fledglings6$twodaysonBroodRef==1892,
                                   6492,
                                   fledglings6$SocialDadID2)

# The ones left undecided:

# BroodRef 1642: N004: Dad's rings = MC/YO (book). Genetic father of 2 out 4 offspring = RR/MW
# BroodRef 1674: N036: Dad's rings = ZZ/ZM (book). - not enough rings
# BroodRef 1704: N066: Dad's rings = ZM/ZZ (book). - not enough rings
# BroodRef 1716: N078: Dad's rings = ZZ/ZM (book). - not enough rings
# BroodRef 1724: N086: Dad's rings = unkown
# BroodRef 1731: N093: Dad's rings = unkown
# BroodRef 1732: N094: Dad's rings = ZZ/ZM (book). - not enough rings
# BroodRef 1770: N132: Dad's rings = unkown
# BroodRef 1813: N175: Dad's rings = ZZ/ZM (book). - not enough rings
# BroodRef 1822: N184: Dad's rings = unkown
# BroodRef 1849: N211: Dad's rings = unkown
# BroodRef 1852: N214: Dad's rings = unkown
# BroodRef 1860: N222: Dad's rings = ZZ/ZM (book). - not enough rings
# BroodRef 2028: P010: No pedigree for 2016 yet
# BroodRef 2054: P036: No pedigree for 2016 yet
# BroodRef 2055: P037: No pedigree for 2016 yet
# BroodRef 2070: P052: No pedigree for 2016 yet 
# BroodRef 2076: P058: No pedigree for 2016 yet 
# BroodRef 2081: P063: No pedigree for 2016 yet

##########################################################################################

##########################################################################
# Generating a database with the BroodRef and the SocialDadID after my changes
##########################################################################

# I can use this for other analyses

BroodRef_SocialDadID <- unique(fledglings6[,c("twodaysonBroodRef",
                                       "SocialDadID2")])

names(BroodRef_SocialDadID) <- c("BroodRef","SocialDadID2")


BroodRef_SocialDadID.3 <- BroodRef_SocialDadID[!(is.na(BroodRef_SocialDadID$BroodRef)),]

# write.csv(BroodRef_SocialDadID.3,
#           "BroodRef_SocialDadID_2014-2016_Updated.csv",
#           row.names=FALSE)


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


# I've checked the SocialDadCertain==FALSE. All, except one correspond to those IDs
# that I've just added by checking books, pedigree and database. I've also checked
# the exception, and the pedigree says that all 4 offspring of that nest were 
# sired by the SocialDadID, therefore, I'll change SocialDadCertain to TRUE!

offspring.12d.ped$SocialDadCertain <- ifelse(offspring.12d.ped$twodaysonBroodRef==1944,
                                         TRUE,
                                         offspring.12d.ped$SocialDadCertain)

# I, anyway, consider all the SocialIDs to be the right ones.


# # To check the BirdIDs caught as fledglings or more that don't have a genetic father
# # assigned:
# write.csv(offspring.12d.ped[is.na(offspring.12d.ped$age.days) & 
#                     is.na(offspring.12d.ped$GeneticDadID) & 
#                     offspring.12d.ped$Cohort!=2016,],
#           "fledglings12/BirdIDsmissingGeneticDad.csv",
#           row.names=FALSE)


##########################################################################
# Generating a list of breeders from 2014 to 2016
##########################################################################

##########################################################################
# MALES
##########################################################################

# First I need the updated list of all breeders from 2014 to 2016

# importing database with all those males that attempted breeding from 2014 to the 15th of July, 2016
# SELECT tblBroods.BroodRef, tblBroods.BroodName, tblNestboxes.NestboxName, tblBroodEvents.EventNumber, tblBroodEvents.EventDate, tblBroods.SocialDadID, tblBroods.SocialDadCertain
# FROM tblNestboxes INNER JOIN (tblBroods INNER JOIN tblBroodEvents ON tblBroods.BroodRef = tblBroodEvents.BroodRef) ON tblNestboxes.NestboxRef = tblBroods.NestboxRef
# WHERE (((tblBroodEvents.EventNumber)=0) AND ((tblBroodEvents.EventDate)>#1/1/2014#));

male.breeding <- read.table("allbreedingbirdsfrom2014-20160715-2.csv",header=TRUE,sep=",")


# Just to make sure there are not BroodRef in BroodRef_SocialDadID.3 (those leaving some 
# offspring) that don't exist in male.breeding

setdiff(BroodRef_SocialDadID.3$BroodRef,male.breeding$BroodRef)


# Mergin both datasets

male.breeding.1 <- merge(male.breeding,
                         BroodRef_SocialDadID.3,
                         by="BroodRef",
                         all.x=TRUE)


# Now I want to combine both SocialDadID to create the final one

male.breeding.1$SocialDadID.final<-male.breeding.1$SocialDadID2

male.breeding.1$SocialDadID.final<-ifelse(is.na(male.breeding.1$SocialDadID2),
                                          male.breeding.1$SocialDadID,
                                          male.breeding.1$SocialDadID2)


# Checking those IDCertain=FALSE for SocialDadID only (SocialDadID2 was checked
# in the previoius script):
# 1708: bird observed with binos, no probs.
# 1768: bird was also observed breeding later on, no probs.
# 1944: bird was the genetic sired of all 4 offspring, no probs.
# 1970: bird was also observed breeding later on, no probs.
# 2049: bird was also observed breeding later on, no probs.

# This means, I can trust SocialDadID.final and get rid off the rest, including
# missing social dads here. 

# Note: I only made the extra-effort of going through books and pedigree
# for those missing dads in fledglings6, which only includea BirdIDs that
# reached the stage of nestling, i.e. eggs and chiggs are not included. 
# Including those could increase the number of SocialDads recovered
# because it could include broods that were left out before. Althogh this
# SocialDadIDs are anyway in "all.males.breeding" (see below), I'm
# interested in having a full list of social dads to subsequently check if
# some of those actually had 0 fledglings reaching the age of 12 d.

# Therefore, I'm going to make the extra effort of checking those broods
# that I din't check before and see if I can recover more SocialDadIDs

# First thing is to check how many more BroodRef without SocialDad assigned
# are in the total list compare to the one I already checked: = 25!

setdiff(unique(male.breeding[is.na(male.breeding$SocialDadID),c("BroodRef")]),
        unique(fledglings6.noDad$twodaysonBroodRef))


##########################################################################################

# Only one more Social Dad could be recovered

# *BroodRef 1905: O026: Dad's rings = ZO/ZM (database)
#  Genetic father of all 3 offspring in brood = OV/MD (BirdID: 6927)

male.breeding.1$SocialDadID.final <- ifelse(male.breeding.1$BroodRef==1905,
                                            6927,
                                            male.breeding.1$SocialDadID.final)

# The ones left undecided:

# BroodRef 1867: N229: Dad's rings = unkown (database)
# BroodRef 1868: N230: Dad's rings = unkown (book)
# BroodRef 1869: N231: Dad's rings = unkown (database)
# BroodRef 1870: N232: Dad's rings = unkown (database)
# BroodRef 1871: N233: Dad's rings = unkown (database)
# BroodRef 1872: N234: Dad's rings = MO/ (database). Non of the 4 offspring were genotyped.
# BroodRef 1873: N235: Dad's rings = unkown (database)
# BroodRef 1899: O020: Dad's rings = unkown (database)
# BroodRef 1900: O021: Dad's rings = /WY (database). No offspring sampled.
# BroodRef 1903: O024: Dad's rings = unkown (database)
# BroodRef 1994: O115: Dad's rings = MO/B?C? (database). No offspring sampled.
# BroodRef 2008: N248: Dad's rings = MD/ZZ (database & book)
# BroodRef 2010: N250: Dad's rings = MD/WY or RW/MO, both showed up in videos (database & book)
# BroodRef 2013: N253: Dad's rings = GN/CM (database & book) or WN/CM (database)
# BroodRef 2016: N256: Dad's rings = ZZ/ZM (database) - not enough rings 
# BroodRef 2033: No pedigree for 2016 yet
# BroodRef 2052: No pedigree for 2016 yet
# BroodRef 2072: No pedigree for 2016 yet
# BroodRef 2073: No pedigree for 2016 yet
# BroodRef 2104: No pedigree for 2016 yet
# BroodRef 2105: No pedigree for 2016 yet
# BroodRef 2123: No pedigree for 2016 yet
# BroodRef 2129: No pedigree for 2016 yet
# BroodRef 2135: No pedigree for 2016 yet

##########################################################################################


# Finally, this is the list of social breeders from 2014 to 2016:

male.breeding2 <- unique(male.breeding.1[!(is.na(male.breeding.1$SocialDadID.final)),
                                         c("SocialDadID.final")])


# The remaining BroodRef correspond to 2016, for which no pedigree is
# available yet. Therefore, I cannot check anything.

# But I can also see if there are some dads that pop up as genetic fathers
# but were not recorded as social

# List of genetic fathers from 2014-2016 extracted from pedigree:
# Notice that the genetic pedigree isn't available for 2016 or birds caught
# unringed in February 2016. (*that meant 2 more birds than before in the final list)

genetic.males.breeding <- unique(pedigree[pedigree$Cohort>2013 & 
                                            !(is.na(pedigree$sire)),
                                   c("sire")])


# How many pop up as genetic but not social parents? 22 

setdiff(genetic.males.breeding,male.breeding2)

all.males.breeding <- c(male.breeding2,
                            setdiff(genetic.males.breeding,male.breeding2))


##########################################################################
# FEMALES
##########################################################################

# First I need the updated list of all breeders from 2014 to 2016

# SELECT tblBroods.BroodRef, tblBroods.BroodName, tblNestboxes.NestboxName, tblBroodEvents.EventNumber, tblBroodEvents.EventDate, tblBroods.SocialMumID, tblBroods.SocialMumCertain
# FROM tblNestboxes INNER JOIN (tblBroods INNER JOIN tblBroodEvents ON tblBroods.BroodRef = tblBroodEvents.BroodRef) ON tblNestboxes.NestboxRef = tblBroods.NestboxRef
# WHERE (((tblBroodEvents.EventNumber)=0) AND ((tblBroodEvents.EventDate)>#1/1/2014#));

# importing database with all those males that attempted breeding from 2014 to the 15th of July, 2016


female.breeding <- read.table("allbreedingfemalesfrom2014-20160715.csv",header=TRUE,sep=",")


# First, I'm going to check, as I did before, if I can recover some females
# by looking at the books and the pedigree.

# Note: I wouldn't need to do this for this project because by combining pedigree and
# social mum IDs I would any way get the full list. However, by doing this now, I can 
# increase the number of social mum IDs to look at maternal effects in the following
# project. So, let's do it and then it is done!

# Run:
female.breeding[is.na(female.breeding$SocialMumID),c("BroodRef")]

# Notice that for mums, we always assume that the genetic mother of the chicks is also
# the social mother of the chicks. This is because we have never found a confirmed case
# of egg dumping in the Lundy Island population, even though we have a genetic and social
# pedigree covering 16 years of data, so far...

# *BroodRef 1642: N004: Mum's rings = ZZ/ZM (database & book)
#  Genetic mother of all 4 offspring in brood = RB/MC (BirdID: 5140)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1642,
                                       5140,
                                       female.breeding$SocialMumID)

# *BroodRef 1696: N058: Mum's rings = ZZ/ZZ (book)
#  Genetic mother of all 3 offspring in brood = WN/RM (BirdID: 7868)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1696,
                                       7868,
                                       female.breeding$SocialMumID)

# *BroodRef 1708: N070: Mum's rings = MD/O?D (book)
#  Genetic mother of all 4 offspring in brood = MD/RD (BirdID: 6772)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1708,
                                       6772,
                                       female.breeding$SocialMumID)

# *BroodRef 1715: N077: Mum's rings = ZZ/ZM (book)
#  Genetic mother of all 3 (genotyped) offspring in brood = ON/MO (BirdID: 6463)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1715,
                                       6463,
                                       female.breeding$SocialMumID)

# *BroodRef 1724: N086: Mum's rings = unkown (book)
#  Genetic mother of 2 out of 3 offspring in brood, the other offspring has NA assigned = OD/MO (BirdID: 6473)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1724,
                                       6473,
                                       female.breeding$SocialMumID)

# *BroodRef 1744: N106: Mum's rings = ZZ/ZM (book)
#  Genetic mother of all 4 offspring in brood = WW/RM (BirdID: 6925)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1744,
                                       6925,
                                       female.breeding$SocialMumID)

# *BroodRef 1747: N109: Mum's rings = MD/ (book)
#  Genetic mother of all 2 offspring in brood = DM/VV (BirdID: 6919)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1747,
                                       6919,
                                       female.breeding$SocialMumID)

# *BroodRef 1751: N113: Mum's rings = NN/
#  Genetic mother of all 3 genotyped offspring in brood = NN/DM (BirdID: 7119)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1751,
                                       7119,
                                       female.breeding$SocialMumID)

# *BroodRef 1768: N130: Mum's rings = unknown (book)
#  Genetic mother of all 2 genotyped offspring in brood = WW/MD (BirdID: 6974)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1768,
                                       6974,
                                       female.breeding$SocialMumID)

# *BroodRef 1816: N178: Mum's rings = unkown (book)
#  Genetic mother of 1 out of 4 offspring in brood, the other offspring has NA assigned = OD/MO (BirdID: 6473)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1816,
                                       6473,
                                       female.breeding$SocialMumID)

# *BroodRef 1862: N224: Mum's rings = unkown (database)
#  Genetic mother of all 2 offspring in brood = WB/DM (BirdID: 7086)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1862,
                                       7086,
                                       female.breeding$SocialMumID)

# *BroodRef 1867: N229: Mum's rings = unkown (database)
#  Genetic mother of all 2 offspring in brood = CC/OM (BirdID: 6640)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1867,
                                       6640,
                                       female.breeding$SocialMumID)

# *BroodRef 1869: N231: Mum's rings = unkown (database)
#  Genetic mother of all 2 offspring in brood = DM/VV (BirdID: 6919)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1869,
                                       6919,
                                       female.breeding$SocialMumID)

# *BroodRef 1899: O020: Mum's rings = unkown (database)
#  Genetic mother of the only offspring in brood = MO/NN (BirdID: 6267)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1899,
                                       6267,
                                       female.breeding$SocialMumID)

# *BroodRef 1901: O022: Mum's rings = ZZ/ZM (database)
#  Genetic mother of all 3 offspring in brood = BN/MG (BirdID: 8636)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1901,
                                       8636,
                                       female.breeding$SocialMumID)

# *BroodRef 1903: O024: Mum's rings = unkown (database)
#  Genetic mother of the only offspring in brood = RM/GG (BirdID: 7894)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1903,
                                       7894,
                                       female.breeding$SocialMumID)

# *BroodRef 1938: O059: Mum's rings = ZZ/ZM (database)
#  Genetic mother of all 4 offspring in brood = BN/MG (BirdID: 8636)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1938,
                                       8636,
                                       female.breeding$SocialMumID)

# *BroodRef 1952: O073: Mum's rings = ZW/OM (database)
#  Genetic mother of all 4 offspring in brood = WN/OM (BirdID: 6536)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1952,
                                       6536,
                                       female.breeding$SocialMumID)

# BroodRef 1970: O091: Mum's rings = unkown (database)
#  Genetic mother of the only genotyped offspring in brood = MG/CC (BirdID: 8300)

female.breeding$SocialMumID2 <- ifelse(female.breeding$BroodRef==1970,
                                       8300,
                                       female.breeding$SocialMumID)

# The ones left undecided:

# BroodRef 1695: N057: Mum's rings = ZZ/ZM (book)
#  Genetic mother of all 4 offspring in brood = NA (unkown, surprisingly)
# BroodRef 1818: N180: Mum's rings = MD/ZC (database)
#  Genetic mother of 2 out of 5 offspring in brood = WN/MW (BirdID: 4854) Last seen alive August 2015
#  Genetic mother of 1 out of 5 offspring in brood = WB/MO (BirdID: 6499) Last seen alive July 2013
#  Genetic mother of 1 out of 5 offspring in brood = MD/NC (BirdID: 6767) Last seen alive June 2014
#  The other offspring left unassigned
# BroodRef 1861: N223: Mum's rings = ZO/MW (database). Offspring's genotype not available
# BroodRef 1870: N232: Mum's rings = unkown (database). No offspring genotyped.
# BroodRef 1871: N233: Mum's rings = unkown (database). No offspring genotyped.
# BroodRef 1873: N235: Mum's rings = unkown (database). No offspring genotyped.
# BroodRef 1890: O011: Mum's rings = YB/MD? (database). No offspring.
# BroodRef 1925: O046: Mum's rings = unkown (database). No offspring.
# BroodRef 1971: O092: Mum's rings = ZM/CG (database). No offspring.
# BroodRef 1981: O102: Mum's rings = ZM/CC (database). No offspring.
# BroodRef 1985: O106: Mum's rings = NY/ZM (database). No offspring.
# BroodRef 1986: O107: Mum's rings = NY/ZM (database). No offspring.
# BroodRef 1994: O115: Mum's rings = MO/N?B? (database). No offspring.
# BroodRef 1997: O118: Mum's rings = NY/ZM (database). No offspring.
# BroodRef 2005: N245: Mum's rings = ZM/ZZ (database). No offspring.
# BroodRef 2006: N246: Mum's rings = NY/ZM (database). No offspring.
# BroodRef 2007: N247: Mum's rings = ZZ/ZZ (database). No offspring.
# BroodRef 2012: N252: Mum's rings = OM/ZZ (database). No offspring.
# BroodRef 2023: No pedigree for 2016 yet
# BroodRef 2033: No pedigree for 2016 yet
# BroodRef 2048: No pedigree for 2016 yet
# BroodRef 2052: No pedigree for 2016 yet
# BroodRef 2055: No pedigree for 2016 yet
# BroodRef 2062: No pedigree for 2016 yet
# BroodRef 2063: No pedigree for 2016 yet
# BroodRef 2072: No pedigree for 2016 yet
# BroodRef 2104: No pedigree for 2016 yet
# BroodRef 2105: No pedigree for 2016 yet
# BroodRef 2137: No pedigree for 2016 yet








                                               

##########################################################################
# Generating a variable for recruits: 0 = didn't recruit, 1 = recruited
##########################################################################

offspring.12d.ped$recruited <- ifelse(!(offspring.12d.ped$BirdID %in% male.breeding2),
                                                  0,1)

setdiff(BroodRef_SocialDadID.3$SocialDadID2,male.breeding2)







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


# # saving it for further analyses
# 
# write.csv(social.per.Dad.2,
#           "fledglings12/annual_social_fledglings12d_per_Dad.csv",
#           row.names=FALSE)


##########################################################################
# Annual number of genetic fledglings reaching day 12 after hatching
##########################################################################

genetic.per.Dad <- count(offspring.12d.ped,c("GeneticDadID","Cohort"))
genetic.per.Dad.2 <- genetic.per.Dad[!(is.na(genetic.per.Dad$GeneticDadID)),]


# # saving it for further analyses
# 
# write.csv(genetic.per.Dad.2,
#           "fledglings12/annual_genetic_fledglings12d_per_Dad.csv",
#           row.names=FALSE)


# counting but without including birds that were caugth as unringed

offspring.12d.ped.noZ <- offspring.12d.ped[!(is.na(offspring.12d.ped$twodaysonBroodRef)),]
genetic.per.Dad.noZ <- count(offspring.12d.ped.noZ,c("GeneticDadID","Cohort"))
genetic.per.Dad.noZ.2 <- genetic.per.Dad.noZ[!(is.na(genetic.per.Dad.noZ$GeneticDadID)),]

write.csv(genetic.per.Dad.noZ.2,
          "fledglings12/annual_genetic_fledglings12d_per_Dad_noZ.csv",
          row.names=FALSE)


##########################################################################
# Annual number of social recruits
##########################################################################


##########################################################################
# Annual number of genetic recruits
##########################################################################
