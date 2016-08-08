
# Author: Alfredo Sanchez-Tojar, MPIO (Seewiesen) and ICL (Silwood Park), alfredo.tojar@gmail.com
# Github profile: https://github.com/ASanchez-Tojar

# Script created on the 3rd of March, 2016
# Script last updated on the 25th of July, 2016

# Excusing myself: Due to the great ammount of for loops used, this script is quite innefficient and
# slow. However, since it does what I needed to do, I decided to not spend extra-time making the script
# more efficient. Feel free to suggest changes though. Thank you very much. Alfredo.

########################################################################################################
# Description of script and Instructions
########################################################################################################

# This script is to clean and check the sparrow dominance data from Lundy Island
#
# The section 0: gives you a quick overview of the time spent analyzing videos and the number of 
#                interactions/hour spent watching videos. Just for you to have an idea. Also, it loads
#                the MegaDataBase.
#
# The section 1: it assigns the right format to the variable of interest. It also creates an identifier
#                of the event.
#
# The section 2: searches and shows you the typos that need correction on the database. It is IMPORTANT
#                to run the sub-sections step by step, i.e. run each for loop separately. This is
#                because after each for loop, if there is something that needs fixing, the line will be
#                printed on the screen.
#
# The section 3: excludes from the dominance database all those interactions that we cannot use for 
#                the dominance analyses due to doubts, missing rings, etc. Also, this section creates
#                a database containing counts on the number of interactions per individual per date
#                event, year, etc. The default is to exclude "displacements" from the database.
#
# The section 4: works on the data extracted from the sparrow database. It generates a list of the
#                colour codes with their birdID that can be definitely used with no doubts, but more
#                important, it generates a list of those colour codes that could be potentially
#                duplicated during the study period. That way, you can exclude them from the dominance
#                database to avoid miss-identifications.
#
# The section 5: it excludes those duplicated colour codes from the dominance and counting database. 
#             
# The section 6: checks the database in order to look for mistakes reagarding sex. It is IMPORTANT
#                to run the sub-sections step by step, i.e. run each for loop separately. This is
#                because after each for loop, if there is something that needs fixing, the line will be
#                printed on the screen.
#
# The section 7: checks the database in order to look for those colour codes that don't exist in the
#                database and shows them on the screen. Therefore, it is IMPORTANT to look at the screen
#                after running this section.
#
# The section 8: prints a summary of the database. It also plots some histograms. At the end of this
#                section, some of the databases created during this script will be saved for the following
#                script. 


# Clear memory and get to know where you are
rm(list=ls())
#getwd()


########################################################################################################
# Packages needed
########################################################################################################

# packages needed to be loaded for this script (a couple of them might be only needed in the following
# script)

library(plyr)
library(reshape)
library(stringr)
library(doBy)


########################################################################################################
# 0. Time spent analyzing videos - quick overview
########################################################################################################

#       This is a little section to have a look at how many interactions per time of work were obtained.
# This only contains information from the videos analyzed from the 25th of February, 2016. Keep in mind 
# that this is just a rough estimate.


# the database for this

time<-read.table("timeperhouranalyzed.txt",sep=",",header=TRUE)


# subsetting to obtain only the time when we were focus on recording only the interactions

time <- subset(time,time$method=="quicker")


# estimating how much time of video per time spent watching was used

time$timeperhofwork <- (time$timeanalyzed.s./3600)/(time$timespent.min./60)


# estimating how many interactions per time spent watching were recorded

time$intperh <- time$numberinteractions/(time$timespent.min./60)


# have a look at average and stuff per observer

summary(time)
# summary(time[time$Observer=="AST",])
# summary(time[time$Observer=="DM",])
# 
# sum(time$timespent.min.)/60


########################################################################################################
# Loading dominance database
########################################################################################################

dom <- read.table("MegaDataBase-v97-201311-201606-FY-Dominance_Lundy_20160713.csv",header=TRUE,sep=',')


########################################################################################################
# 1. Transforming variables
########################################################################################################

#     Let's start by transforming all these variables to characters.Otherwise I cannot do the comparison
# in the following for loop

dom$individual1 <- as.character(dom$individual1)
dom$individual2 <- as.character(dom$individual2)
dom$starter <- as.character(dom$starter)
dom$Winner <- as.character(dom$Winner)
dom$Loser <- as.character(dom$Loser)
dom$wind <- as.factor(dom$wind)
dom$level <- as.factor(dom$level)


# Creating an identifier of the event that will be needed later on

for(i in 1:nrow(dom)){
  if(dom$date[i] != 20160531){
    dom$event[i] <- substr(dom$date[i], 1, 6)
  } else{
    dom$event[i] <- "201606"
  }
}

dom$event <- as.factor(dom$event)


########################################################################################################
# 2. Checking that individual1 and individual2 are the ones in starter, Winner and Loser, and that 
#    Winner and Loser are not the same individual.
########################################################################################################

#     This is to check if I made any mistake writting the ID's in the wrong row, i.e. looking for typos
# while copying


########################################################################################################
# # 2.1. I check it for individual1
########################################################################################################

for (i in 1:nrow(dom)){
  if(dom$individual1[i]==dom$Winner[i]|
       dom$individual1[i]==dom$Loser[i]){
    
    if(dom$individual1[i]==dom$starter[i] | dom$individual2[i]==dom$starter[i]){
      
    } else {
      print("Check this row for problems with the starter") #it prints the line so that you can go and check yourself
      print(i+1)
      
    }
    
  } else {
    print("Check this row for individual1") #it prints the line so that you can go and check yourself
    print(i+1)
  }
}


########################################################################################################
# # 2.2. I check it for individual2
########################################################################################################

for (i in 1:nrow(dom)){
  if(dom$individual2[i]==dom$Winner[i]|
       dom$individual2[i]==dom$Loser[i]){
    
    if(dom$individual1[i]==dom$starter[i] | dom$individual2[i]==dom$starter[i]){
      
    } else {
      print("Check this row for problems with the starter") #it prints the line so that you can go and check yourself
      print(i+1)
      
    }
    
  } else {
    print("Check this row for individual2") #it prints the line so that you can go and check yourself
    print(i+1)
  }
}

#     After you run those two for loops you are supposed to go and fix in the database whatever problem
# popped up. Then, move on to the next step.


########################################################################################################
# # 2.3. Checking that Winner is different from Loser
########################################################################################################

#     This is something that elo.seq() will tell you, but it wouldn't tell you were exactly so we can
# prevent the error here

# This for loop does that

for (i in 1:nrow(dom)){
  if(dom$Winner[i]==dom$Loser[i] & dom$Winner[i]!="zz/zz" 
     & dom$Winner[i]!="zm/zz" & dom$Winner[i]!="zz/zm" & dom$Winner[i]!="notidentified"){
    print("Check this row for Winner=Loser") #it prints the line so that you can go and check yourself
    print(i+1)
  } 
}


########################################################################################################
# # 2.4. Checking that all colour codes have 5 characters (4 letter + the /)
########################################################################################################

#       Keep in search of typos. Excluding, of course, the notidentified which have 13 characters and 
# are of not interest.

for (i in 1:nrow(dom)){
  if(dom$individual1[i]!="notidentified" & dom$individual2[i]!="notidentified" &
       dom$starter[i]!="notidentified" & dom$Winner[i]!="notidentified" &
       dom$Loser[i]!="notidentified"){
    
    if(nchar(dom$individual1[i])!=5 | nchar(dom$individual2[i])!=5 |
         nchar(dom$starter[i])!=5 | nchar(dom$Winner[i])!=5 |
         nchar(dom$Loser[i])!=5){
      
      print("Check this row for Codes < or > than 5 expected characters") #it prints the line so that you can go and check yourself
      print(i+1)
    }
    
  } 
}


########################################################################################################
# # 2.5. Checking that any code contains two "m" (metal rings)
########################################################################################################

# Dominic comment: check if there is at least one m (i.e. one BTO ring), !(=1)

# Keep in search of typos. 

for (i in 1:nrow(dom)){
  
  if(str_count(dom$individual1[i],"m")>1 | str_count(dom$individual2[i],"m")>1 |
       str_count(dom$starter[i],"m")>1 | str_count(dom$Winner[i],"m")>1 |
       str_count(dom$Loser[i],"m")>1){
    
    print("Check this row for Codes for > 1 metal ring") #it prints the line so that you can go and check yourself
    print(i+1)
    
  }
}


########################################################################################################
# # 2.6. Checking that any code contains letters that aren't expected
########################################################################################################

#       I know this isn't the most efficient way of doing it but it works I didn't have the time to keep 
# working on this to make it pretty and efficient, I just needed to do what I wanted.

# letters: contains all possible letters in our colour codes plus the / and the - (for the missing tarsus)

letters <- c("b","c","d","g","m","n","o","r","v","w","y","z","/","-")


# first for loop for individual1

for (i in 1:nrow(dom)){
  
  if(dom$individual1[i]!="notidentified"){
    
    x <- unlist(strsplit(dom$individual1[i],""))
    
    for (j in 1:length(x)){
      
      if(!(x[j] %in% letters)){
        
        print("Check this row for Codes with wrong letters") #it prints the line so that you can go and check yourself
        print(i+1)
        
      }
    }
  }    
}


# second for loop for individual2

for (i in 1:nrow(dom)){
  
  if(dom$individual2[i]!="notidentified"){
    
    x <- unlist(strsplit(dom$individual2[i],""))
    
    for (j in 1:length(x)){
      
      if(!(x[j] %in% letters)){
        
        print("Check this row for Codes with wrong letters") #it prints the line so that you can go and check yourself
        print(i+1)
        
      }
    }
  }    
}


# third for loop for starter

for (i in 1:nrow(dom)){
  
  if(dom$starter[i]!="notidentified"){
    
    x <- unlist(strsplit(dom$starter[i],""))
    
    for (j in 1:length(x)){
      
      if(!(x[j] %in% letters)){
        
        print("Check this row for Codes with wrong letters") #it prints the line so that you can go and check yourself
        print(i+1)
        
      }
    }
  }    
}


# fourth for loop for Winner

for (i in 1:nrow(dom)){
  
  if(dom$Winner[i]!="notidentified"){
    
    x <- unlist(strsplit(dom$Winner[i],""))
    
    for (j in 1:length(x)){
      
      if(!(x[j] %in% letters)){
        
        print("Check this row for Codes with wrong letters") #it prints the line so that you can go and check yourself
        print(i+1)
        
      }
    }
  }    
}


# fifth for loop for Loser

for (i in 1:nrow(dom)){
  
  if(dom$Winner[i]!="notidentified"){
    
    x <- unlist(strsplit(dom$Winner[i],""))
    
    for (j in 1:length(x)){
      
      if(!(x[j] %in% letters)){
        
        print("Check this row for Codes with wrong letters") #it prints the line so that you can go and check yourself
        print(i+1)
        
      }
    }
  }    
}


########################################################################################################
# # 2.7. Checking that no displacements occurred out of the feeder
########################################################################################################

for (i in 1:nrow(dom)){
  
  if(dom$level[i]=="1" & dom$infeeder.[i]=="no" & dom$doubtswithinteraction[i]=="no"){
    
    print("Check this row for displacement out of the feeder")
    print(i+1)                 
    
  }
}


########################################################################################################
# 3. Identifying and counting the number of unusable interactions
########################################################################################################

#       This piece of coude subsets the data base but also prints the counts on the number of  
# interactions involving:

#     a. doubts with interaction
#     b. doubts with BirdID
#     c. notidentified birds
#     d. unringed individuals (zz/zz)
#     e. individuals missing rings (zm/zz, zz/zm or any individual containing z)

#       At the end, I generate a database that does not include any of those interactions becuase I
# cannot use them for estimating dominance rank (I need to do both individuals). This database will be:
# dom.final. Furthermore, I also generate a database that includes all of them but the doubst with 
# interactions (a.) and with BirdIDs (b.) to run analyses on the proportions of wins, as a simplified
# version of dominance rank. This database will be: dom.prop.wins


########################################################################################################
# # 3.1. Getting database in format
########################################################################################################

# First thing is to get rid off the "/" in the Codes as in can be problematic later on. I use regex

dom$individual1 <- as.factor(sub("/","",dom$individual1))
dom$individual2 <- as.factor(sub("/","",dom$individual2))
dom$starter <- as.factor(sub("/","",dom$starter))
dom$Winner <- as.factor(sub("/","",dom$Winner))
dom$Loser <- as.factor(sub("/","",dom$Loser))


########################################################################################################
# # # 3.1.1. Extra step to do everything the same but excluding displacements
########################################################################################################

# Here you can choose whether you want to include displacements or not.

dom <- subset(dom, dom$level!="1")


########################################################################################################
# # 3.2. Counting and excluding interactions with doubts
########################################################################################################

# This is for printing later on the number and % of interactions with doubts

doubts.with.int <- as.vector(table(dom$doubtswithinteraction))[2]
perc.doubts.with.int <- (doubts.with.int*100)/length(dom$doubtswithinteraction)

# Excluding those interactions with doubts about the interaction

dom.final <- subset(dom, dom$doubtswithinteraction=="no")


########################################################################################################
# # 3.3. Counting and excluding interactions with doubts about the id
########################################################################################################

# This is for printing later on the number of interactions with doubts regarding the ID of the ind.

doubts.with.ID <- sum(as.vector(table(dom.final$IDcertain1,dom.final$IDcertain2))[1:3])
perc.doubts.with.ID <- (doubts.with.ID*100)/length(dom.final$IDcertain1)


# Excluding those with doubts about the ID of any of the participants in the interaction

dom.final <- subset(dom.final, IDcertain1!="no" & IDcertain2!="no")


########################################################################################################
# # 3.4. Counting and excluding (although keeping in a dataset) interactions involving notidentified
########################################################################################################

# This is for printing later on the number of interactions with notidentified individuals

notidentfied.ind <- sum(as.vector(table(dom.final$individual1=="notidentified",
                                        dom.final$individual2=="notidentified"))[2:4])
perc.notidentfied.ind <- (notidentfied.ind*100)/length(dom.final$individual1)


# Keeping dom.final before keeping excluding birds for estimating the proportion of wins per individual
# which isn't affected by the identity of the individual in front.

dom.prop.wins <- dom.final


# Excluding those with doubts about the ID of any of the participants in the interaction

dom.final <- subset(dom.final, individual1!="notidentified" & individual2!="notidentified")


########################################################################################################
# # 3.5. Counting and excluding interactions involving unringed individuals
########################################################################################################

# Printing later on the number of interactions involving unringed individuals

unringed.ind <- sum(as.vector(table(dom.final$individual1=="zzzz",
                                    dom.final$individual2=="zzzz"))[2:4])
perc.unringed.ind <- (unringed.ind*100)/length(dom.final$IDcertain1)


# Excluding those with doubts about the ID of any of the participants in the interaction

dom.final <- subset(dom.final, individual1!="zzzz" & individual2!="zzzz")


########################################################################################################
# # 3.6. Counting and excluding interactions involving individuals missing 1 or more colour rings
########################################################################################################

# Printing later on the number of interactions involving individuals missing rings

miss.rings.ind <- sum(as.vector(table(grepl("z",dom.final$individual1),
                                      grepl("z",dom.final$individual2)))[2:4])

perc.miss.rings.ind <- (miss.rings.ind*100)/length(dom.final$IDcertain1)


# Excluding those with individuals missing 1 or more colour rings

dom.final <- subset(dom.final,grepl("z",dom.final$individual1)==FALSE & grepl("z",dom.final$individual2)==FALSE)


########################################################################################################
# # 3.7. Final clean database: dom.final and dom.prop.wins
########################################################################################################

#       Here I run factor in all the factors of the database in order to drop unused factor and get the
# database fully in shape.

dom.final[,c(5:27)] <- lapply(dom.final[,c(5:27)], factor)


# I then create date.ELO2.2 which is the date in the format that elo.seq() will require it.

dom.final$date.ELO2.2 <- as.factor(paste(dom.final$year,dom.final$month,dom.final$day,sep="-"))


# I also drop unused factor for the database contianing data for the proportions of wins

dom.prop.wins[,c(5:27)] <- lapply(dom.prop.wins[,c(5:27)], factor)


# and also create date.ELO2.2 to keep them equal, at least

dom.prop.wins$date.ELO2.2 <- as.factor(paste(dom.prop.wins$year,dom.prop.wins$month,dom.prop.wins$day,sep="-"))


########################################################################################################
# # 3.8. Counting number of interactions per individual (check counts.ind at the end of 3.9)
########################################################################################################

#     The database that I'm creating contains counts on the number of interactions per individual. It 
# counts interactions (1) for the all database, (2) for each date, (3) for each year, (4) for each
# sampling event


########################################################################################################
# # # 3.8.1. Generating a list of the individuals included in the database
########################################################################################################

#       This generates a list of the individiuals by combining the columns individual1 and individual2.
# This way we can count interactions by counting number of times that each individual shows up in the
# list.


# List with all individuals interacting as individual1 and converting it as data.frame

list.ind1 <- dom.final$individual1

list.ind1 <- as.data.frame(list.ind1)

# length(list.ind1$list.ind1)
# summary(list.ind1)
# length(unique(list.ind1$list.ind1))

# #checking that everything worked by comparing the new data.frame to the variable from
# #the main database
# library(compare)
# comparison1 <- compare(list$list,EloR1$individual1,allowAll=TRUE)
# summary(comparison1)
# summary(comparison1$tM)


# List with all individuals interacting as individual2 and converting it as dataframe

list.ind2 <- dom.final$individual2

list.ind2 <- as.data.frame(list.ind2)

# length(list.ind2$list.ind2)
# summary(list.ind2)
# length(unique(list.ind2$list.ind2))

# #checking that everything worked by comparing the new data.frame to the variable from
# #the main database
# library(compare)
# comparison2 <- compare(list2$list2,EloR1$individual2,allowAll=TRUE)
# summary(comparison2)
# summary(comparison2$tM)


#       Here I rename the variable in list.ind2 so that it is the same as in list.ind1, otherwise, rbind 
# does not work the way I want it to work

names(list.ind2) <- c("list.ind1")


# Now I construct a variable by rbinding together the 2 dataframes

list.x <- rbind(list.ind1,list.ind2)

# summary(list.x)
# length(unique(list.x$list))


########################################################################################################
# # # 3.8.2. Counting number of interactions per individual for the whole database
########################################################################################################

# Generating a database with the individuals and their number of total interactions in the whole dataset

ind.counts <- count(list.x,"list.ind1")

names(ind.counts)<-c("individual","totalfreq")


# Making a quick histogram of the number of interactions per individual

#hist(ind.counts$totalfreq,breaks=20)

# #saving it as a csv file
# write.csv(ind.counts,file="numberofinteractionsperindividual.csv",sep=",",na="",dec = ".")


########################################################################################################
# # # 3.8.3. Counting number of interactions per individual per date
########################################################################################################

#       Generating a database with the individuals and their number of interactions per date. This way I
# know which individuals are seen in more than one date

# Creating two data.frames: one with individual1 and the date, and another with indivual2 and the date

list.ind1.date<-dom.final[,c(13,4)]
list.ind2.date<-dom.final[,c(16,4)]


# Renaming the variables so that they have the same name for the rbind

names(list.ind1.date) <- c("individual","date")
names(list.ind2.date) <- c("individual","date")


# Pasting them together using rbind

superlist.date<-rbind(list.ind1.date,list.ind2.date)


#       Now I can count the number of times each individual showed up in each date by counting the
# number of dates each individual showed up in

ind.counts.date <- count(superlist.date,c("individual","date"))

names(ind.counts.date) <- c("individual","date","freqperdate")

#hist(ind.counts.date$freqperdate,breaks=20)


# Now I can merge this database with the previos one, creating the database that will contain all counts

counts.ind <- merge(ind.counts.date,ind.counts,by="individual")


########################################################################################################
# # # 3.8.4. Counting number of dates each individual showed up in
########################################################################################################

# This allows me to count the number of dates each individual showed up in

onlyind <- ind.counts.date$individual

onlyind <- as.data.frame(onlyind)


# Counting number of dates per individual

superlist.num.date <- count(onlyind,"onlyind")

names(superlist.num.date) <- c("individual","freqofdates")

#hist(superlist.num.date$freqofdates,breaks=9)

# dim(supercount[supercount$freq>1,])


# Now I merge again in the counting database

counts.ind <- merge(counts.ind,superlist.num.date,by="individual")


########################################################################################################
# # # 3.8.5. Counting number of interactions per individual per year
########################################################################################################


########################################################################################################
# # # # 3.8.5.1. First preparing the database
########################################################################################################

# Creating two data.frames with individual1 and the year, and indivual2 and the year

list.ind1.year <- dom.final[,c(13,3)]
list.ind2.year <- dom.final[,c(16,3)]


# renaming the variables so that they have the same name for the rbind

names(list.ind1.year) <- c("individual","year")
names(list.ind2.year) <- c("individual","year")


#pasting them together using rbind

superlist.year <- rbind(list.ind1.year,list.ind2.year)


# This is to collate together dates withing the same sparrow year (period = Oct-Sept), and because I 
# know the sampling events in this particular dataset, I'm going to rename 2013 by 2014

superlist.year$year <- ifelse(superlist.year$year==2013,2014,superlist.year$year)


########################################################################################################
# # # # 3.8.5.2. Second, counting
########################################################################################################

ind.counts.year <- count(superlist.year,c("individual","year"))

names(ind.counts.year) <- c("individual","year","freqperyear")


# This creates an individual year identifier to merge this database later on

ind.counts.year$indyear <- paste(ind.counts.year$individual,ind.counts.year$year,sep="_")


#       Generating the same individual year identifier in the general counting dataset so that I can 
# merge the ind.counts.year with it

counts.ind$year <- substr(counts.ind[,2], 1, 4)

counts.ind$year <- ifelse(counts.ind$year==2013,2014,counts.ind$year)

counts.ind$indyear <- paste(counts.ind$individual,counts.ind$year,sep="_")


# Adding the individual number of interactions per year to the general count database

ind.counts.year.2 <- ind.counts.year[,3:4]

counts.ind <- merge(counts.ind,ind.counts.year.2,by="indyear")

# #saving it as a csv file
# write.csv(ind.counts.year,file="numberofinteractionsperindividualperyear.csv",sep=",",na="",dec = ".")


########################################################################################################
# # # 3.8.6. Counting number of years each individual showed up in
########################################################################################################

onlyind2 <- ind.counts.year$individual

onlyind2 <- as.data.frame(onlyind2)

superlist.year.2<-count(onlyind2,"onlyind2")

names(superlist.year.2) <- c("individual","freqofyears")


# Now I merge again in the counting database

counts.ind <- merge(counts.ind,superlist.year.2,by="individual")


# # This one extract the year from the date.ELO2 and creates a new variable
# counts.ind3$year <- as.numeric(sapply(strsplit(counts.ind3$date.ELO2, "-"), "[[", 1))


########################################################################################################
# # # 3.8.7. Counting number of interactions per individual per sampling event
########################################################################################################

#     First, making a sampling event identifier in the general database (this works because it is always
# within a single month, except may/june 2016)

# MOVED ABOVE

# I'll need to have the same identifier (event) in the counts.ind database, so I'm going to create it

for(i in 1:nrow(counts.ind)){
  if(counts.ind$date[i] != 20160531){
    counts.ind$event[i] <- substr(counts.ind$date[i], 1, 6)
  } else{
    counts.ind$event[i] <- "201606"
  }
}

counts.ind$event <- as.factor(counts.ind$event)

#old way
#counts.ind$event <- as.factor(substr(counts.ind[,3], 1, 6))


#       Generating a database with the individuals and their number of interactions per sampling event.
# This way I know which individuals are seen in more than one trip

# Creating two data.frames: one with individual1 and the event, and another with indivual2 and the event

list.ind1.event<-dom.final[,c(13,27)]
list.ind2.event<-dom.final[,c(16,27)]


# Renaming the variables so that they have the same name for the rbind

names(list.ind1.event) <- c("individual","event")
names(list.ind2.event) <- c("individual","event")


# Pasting them together using rbind

superlist.event<-rbind(list.ind1.event,list.ind2.event)


#       Now I can count the number of times each individual showed up in each event by counting the
# number of event each individual showed up in

superlist.event <- count(superlist.event,c("individual","event"))

names(superlist.event) <- c("individual","event","freqpperevent")

#hist(superlist.event$freqpperevent,breaks=20)


# To merge them, I have to create an individual event identifier in both databases

superlist.event$indevent <- paste(superlist.event$individual,superlist.event$event,sep="_")

counts.ind$indevent <- paste(counts.ind$individual,counts.ind$event,sep="_")


#       Last but not least, I'm goint to reduce the number of columns in superlist.event so that they
# are not repeated in the final counts.ind

superlist.event1 <- superlist.event[,c(3,4)]


# Now I can merge this database with counts.ind, creating the database that will contain all counts

counts.ind <- merge(counts.ind,superlist.event1,by="indevent")


########################################################################################################
# # # 3.8.8. Counting number of events each individual showed up in
########################################################################################################

# This allows me to count the number of sampling events each individual showed up in

onlyind3 <- superlist.event$individual

onlyind3 <- as.data.frame(onlyind3)


# Counting number of sampling events per individual

superlist.num.event <- count(onlyind3,"onlyind3")

names(superlist.num.event) <- c("individual","freqofevents")

#hist(superlist.num.event$freqofevents,breaks=9)

# dim(supercount[supercount$freq>1,])


# Now I merge again in the counting database

counts.ind <- merge(counts.ind,superlist.num.event,by="individual")


########################################################################################################
# # # 3.8.9. The final counting database is: counts.ind
########################################################################################################

# reordering the columns in the way I want

counts.ind <- counts.ind[,c(1,6,4,5,7,11,2,12,13,8,3,9,10)]
names(counts.ind) <- c("individual","total.num.inter",
                       "date","intperdate","numberofdates",
                       "event","indevent","intperevent","numberofevents",
                       "year","indyear","intperyear","numberofyears")


# choosing the right type for each variable

counts.ind$date <- as.factor(counts.ind$date)
counts.ind$indevent <- as.factor(counts.ind$indevent)
counts.ind$year <- as.factor(counts.ind$year)
counts.ind$indyear <- as.factor(counts.ind$indyear)


# # Now I'm cleaning the work space by removing uninterest objects
# 
# rm(ind.counts,ind.counts.date,ind.counts.year,ind.counts.year.2,list.ind1,list.ind1.date,
#    list.ind1.event,list.ind1.year,list.ind2,list.ind2.date,list.ind2.event,list.ind2.year,
#    list.x,onlyind,onlyind2,onlyind3,superlist.date,superlist.event,superlist.event1,
#    superlist.num.date,superlist.num.event,superlist.year,superlist.year.2)


########################################################################################################
# 4. Obtaining a list with information from the database by colour code
########################################################################################################

#     Easier said than done. Basically, this imports the database that you've obtained from the sparrow
# database and that contains, among others, BirdID, Code, SexEstimate, Cohort, LastLiveRecord, 
# CaptureDate and DeathDate. Then, it gets rid off all old colour codes that were not present during
# the time where my dominance videos were taken. With the remaining, the trickiest thing is to deal with
# those colour codes that are duplicated due to those colour codes used to ring birds when they shouldn't
# have been used because (1) another bird had or potentially had it still, (2) another bird died and its
# colour code was then used due to an emergency (e.g. no other colour codes available), (3) birdID, for
# whatever reason is duplicated in the AllCodestbl in the database.

#rm(list=ls())


# Importing database containing BirdID, FieldRing (usually colour code), Sex, Cohort and Last seen alive

# old way
#birdsex <- read.table("BirdID-colour-rings_sex_cohort-last-time-seen-as-to-20160303.csv",
#                      header=TRUE,sep=',')

#birdsex <- read.table("BirdID-colour-rings_sex_cohort-last-time-seen-ringing-date-death-date-as-to-20160309.csv",
#                      header=TRUE,sep=',')


birdsex <- read.table("BirdID-colour-rings_sex_cohort-last-time-seen-ringing-date-death-date-as-to-20160606.csv",
                      header=TRUE,sep=',')

birdsex$BirdID <- as.factor(birdsex$BirdID)

# str(birdsex)
# head(birdsex)
# summary(birdsex)


########################################################################################################
# # 4.1. Getting the database in format
########################################################################################################

#     First thing is to create a new variable in the database containing m and f for the sex instead of 
# 0 and 1. This is because we coded it as m=male and f=female while analyzing the videos (it was easier
# and less error prone). Then, transform it to a factor

birdsex$SexEstimate2<-ifelse(birdsex$SexEstimate==0,"f",ifelse(birdsex$SexEstimate==1,"m",""))

birdsex$SexEstimate2<-as.factor(birdsex$SexEstimate2)


#     Now let's sort the database by cohort, being the latest cohort on top. This is because I'm going to 
# make a subset of this database including each colour combination only once, this corresponding to the
# most recent birdID using this combination. 
#
#     However, in order to acknowledge that some colour codes might have been used twice by mistake
# (duplicates, happening when a colour combination of a bird last seen less than 2 years is used as the
# new colour combination of a bird). I will include a specific colour combination twice if the two birds
# carrying it were last seen less than 2 years apart from each other. This way we can explore it in deep
# and decide if we want to include those birds (if present)


# Before anything I've got to tell R that the column LastLiveRecord, CaptureDate and DeathDate
# are dates
#
#     This lct (seeting the C locale) is to avoid as.Date failing to recognize some of the month
#abbreviations (e.g. Aug)

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

birdsex$LastLiveRecord2<-as.Date(birdsex$LastLiveRecord,format='%d-%b-%y')
birdsex$CaptureDate2<-as.Date(birdsex$CaptureDate,format='%d-%b-%y')
birdsex$DeathDate2<-as.Date(birdsex$DeathDate,format='%d-%b-%y')

#summary(birdsex)


#     This adds a new variable saying whether the Code is actually a colour ring or not. This is
# because before I use the query Fieldring and that gives the metal ring of those birds that were not
# colour ring. I've decide to keep this step because it might pick up mistakes.

birdsex$colourring<-grepl("/",birdsex$Code)


## Quickly checking what birds don't have a colour ring assigned in this table, they should!
#whyFALSE <- subset(birdsex,birdsex$colourring==FALSE)


# Now I can subset to keep only those that are colour rings as I'm not interested in the others

birdsex <- subset(birdsex,birdsex$colourring==TRUE)


# Furthermore, by creating another variable I can also exclude those colour combinations
# missing rings (i.e. containing Z). And then subset to keep only those with a full colour code

birdsex$missingcolourring<-grepl("Z",birdsex$Code)

birdsex <- subset(birdsex,birdsex$missingcolourring==FALSE)



# Now I'm going to get rid off the "/" in the Code as in can be problematic later on. I use regex

birdsex$Code <- as.factor(sub("/","",birdsex$Code))



# I also want to exclude those birds codes that don't have 4 colours, just to keep everything clean
birdsex <- subset(birdsex,nchar(as.character(birdsex$Code))==4)


########################################################################################################
# # 4.2. Subsetting by date and deathDate
########################################################################################################

#     Now it is time to go through this clean data base and make a subset of it containing each colour
# combination once (twice if last seen between them is less than 2.5 years appart).

#     First thing I'm going to do to simplify everything is to exclude all those birds not seen since
# the 1st of April 2007, this is more than 5.5 years before the first Dominance video recorded. So, 
# that would mean that they should definitely be dead by November 2013. I should probably be more
# restrictive and do it for 2010 (3.5 years before) but I've chosen to go back to 2008 because it does
# not complicate the code, plus, it would include individuals that we could have ve missed for very
# long and suddenly show up in the dominance videos

# Decide the subset that you consider makes more sense

birdsex <- subset(birdsex,as.Date("2010-04-01")-birdsex$LastLiveRecord2<0)


#     An extra thing I realized is that I can exclude from the database those birds that were found 
# dead before the 13th of November, 2013, i.e. when the dominance observations started. The reason 
# being that if two birds shared a colour combination for some time but one died before I started my
# observations, I can be sure of which birdID carried the colour combination I saw.

birdsex <- subset(birdsex,is.na(birdsex$DeathDate2)==TRUE|
                    birdsex$DeathDate2>as.Date("2013-11-13"))


########################################################################################################
# # 4.3. Counting number of times each Code and BirdID shows up to further subset
########################################################################################################


########################################################################################################
# # # 4.3.1. Counting codes
########################################################################################################

#     Now I'm going to add a new variable to the database that contains a count of how many times the
# colour code is included in the database. For that, I will count the number of times that each colour
# code appears in the database and include that informationas a new variable. This will allow me to 
# divide the database in two, and focus only in the subset with the duplicates.

#library(plyr)

# Generating a database with FieldRing and it counting

birdsex.count <- count(birdsex,"Code")


# Making freq as a factor to later on now how many colour codes have 2 entries in the database

birdsex.count$freq<-as.factor(birdsex.count$freq)


# Merging the counting information into the database

birdsex <- merge(birdsex,birdsex.count,by='Code')

# Renaming freq to a more specific name

birdsex <- rename(birdsex, c("freq"="freqCode"))


########################################################################################################
# # # 4.3.2. Subsetting
########################################################################################################

# Now I sort the database by colour ring, cohort oldest to most recent
# This is more for visual purposes as I will sort it later on again during the function

birdsex <- birdsex[order(birdsex$Code,birdsex$Cohort, birdsex$LastLiveRecord2),]


# Now I'm going to add a function to choose, from those individuals that show up twice, the most
# recent birdid, unless the oldest individual was last seen less than two years before the newest
# was ringed, that I will keep both individuals in the list. However, if the oldest died before the 
# newest was ringed, I will discard the oldest.


#     I'm going to split the database in two, one with the observations for Code frequency = 1 and
# another for the observations with Code frequency = 2. I will make the script faster and easier to
# handle. Then I can rbind() them together later on.

birdsex.1 <- subset(birdsex,birdsex$freqCode == 1)
birdsex.2 <- subset(birdsex,birdsex$freqCode == 2)


# Following Malika's advice, I make them dates with POSIXct instead of as.Date

birdsex$LastLiveRecord2 <- as.POSIXct(birdsex$LastLiveRecord2)
birdsex$CaptureDate2 <- as.POSIXct(birdsex$CaptureDate2)
birdsex$DeathDate2 <- as.POSIXct(birdsex$DeathDate2)


########################################################################################################
# # # 4.3.3. Counting birdIDs in the subset with double colour codes
########################################################################################################

#     Since I've found that some BirdIDs are repeated in the dataset, I'm gonna check which and why
# so that I can sort out that before the following steps. So, as I did before, I'm going to count
# how many times each birdID is and check those that show up more than once. I'm carrying the
# following steps:
#     1. Generating a database with FieldRing and it counting
#     2. Making freq as a factor to later on know how many colour codes have 2 entries in the database
#     3. Merging the counting information into the database

#    There is one individual whose colour code was change on the 14th of November, 2013. Therefore, I
# might have recorded with one colour ring on the 13th of November, 2013, and a different one later on.

birdsex.2.count <- count(birdsex.2,"BirdID")
birdsex.2.count$freq<-as.factor(birdsex.2.count$freq)
birdsex.2 <- merge(birdsex.2,birdsex.2.count,by='BirdID')
birdsex.2 <- rename(birdsex.2, c("freq"="freqBirdID"))


########################################################################################################
# # # 4.3.4. Subsetting those with repeated bird ID
########################################################################################################

# Now, I will subset this repeated BirdIDs to understand what's going on

birdsex.3 <- subset(birdsex.2,as.numeric(birdsex.2$freqBirdID)>1)

# This is the dataset with no repeated BirdID but codes

birdsex.5 <- subset(birdsex.2,as.numeric(birdsex.2$freqBirdID)==1)


# The thing is that some birdID have 2 or more entries with different ringing date, don't know why. Also, 
# some birds have changes in their colour combination. I'm only interested in the last CaptureDate2
# of each bird (there is one exception, 4119 which is fully repeated). So, eventhough I know it
# isn't elegant, what I'm going to do is just to order this subset by CaptureDate2 and keep the most
# recent ringing date of each of these birds. I will do that for all those cases where ringing date
# (i.e. CaptureDate) is before November 2013 (when my videos started), because this way I'm sure I can
# just discard them without any possibility of affecting my video identification. I'm adding them to
# birdsex.5 and carry out the counting again so that I know which colour codes are still problematic.


# First, sorting it by date

birdsex.3 <- birdsex.3[order(birdsex.3$BirdID,-as.numeric(birdsex.3$CaptureDate2)),]


# To make the next for loop simple

birdsex.3$BirdID <- as.numeric(levels(birdsex.3$BirdID))[birdsex.3$BirdID]


# Then noting those that I want to keep as '0' and those that I don't want as '1' so that I can subset
# later on.

id<-c()


for(i in 1:nrow(birdsex.3)){
  
  if(birdsex.3$CaptureDate2[i]<as.Date("2013-11-01")){
    
    if(birdsex.3$BirdID[i] %in% id == TRUE){
      #print(birdsex.3$delete[i])
      
    } else {
      
      id <- append(id,birdsex.3$BirdID[i])
      birdsex.5 <- rbind(birdsex.5,birdsex.3[i,])
      
    }
    
  } else {
    print("check i")
  }
  
}


########################################################################################################
# # # 4.3.5. Counting Codes again the subset with repeated codes and remaking birdsex.1 and birdsex.2
########################################################################################################

#     After removing those repeated BirdIDs, a put back the single entries together with the remaining 
# rows with repeated Codes. Now I want to count again to see what are the Codes that are still repeated
# i.e. the problematic ones. The ones that arent problematic anymore will be moved to birdsex.1, i.e. 
# the data frame that has the unique Codes.I'm carrying the
# following steps:

#     1. Generating a database with Code and it counting
#     2. Making freq as a factor to later on now how many colour codes have 2 entries in the database
#     3. Merging the counting information into the database

#    There is one individual whose colour code was change on the 14th of November, 2013. Therefore, I
# might have recorded with one colour ring on the 13th of November, 2013, and a different one later on.

birdsex.5$freqCode <- NULL # deleting it to add the new version of it
birdsex.5$freqBirdID <- NULL # deleting it because it doesn't make sense anymore


birdsex.3.count <- count(birdsex.5,"Code")
birdsex.3.count$freq<-as.factor(birdsex.3.count$freq)
birdsex.5 <- merge(birdsex.5,birdsex.3.count,by='Code')
birdsex.5 <- rename(birdsex.5, c("freq"="freqCode"))


#     The following is to put the single Code back to the database with no repeated Codes (i.e.
# birdsex.1) and to remake the database with repeated Codes (i.e. birdsex.2)


# First,subset and check the names of both

birdsex.6 <- subset(birdsex.5,birdsex.5$freqCode=="1")

#names(birdsex.1)
#names(birdsex.6)


#     Now, I'm going to put birdsex.1 and birdsex.6 together. I'm using rbind() because both the same
# columns but, if they had different number of columns, I would use rbind .fill() from package plyr,
# which fills the empty values with NA
# Remember, birdsex.1 is the super cool amazing dataset that does not contain any colour code duplicated

birdsex.1 <- rbind(birdsex.1, birdsex.6)


#     Now I'm going to remake the dataset with Colour code duplicates and try to work it out

birdsex.2 <- subset(birdsex.5,birdsex.5$freqCode=="2")

# ## TO SAVE IT AS A CSV
# write.csv(birdsex,file="blabla.csv",sep=",",na="",dec = ".")


########################################################################################################
# # # 4.3.6. Choosing the right BirdID of those repeated Colour codes (when possible, of course)
########################################################################################################

#     Before anything, I call factor to all factors to drop unused (old) levels in that factor.
# Otherwise, everything gets messed up.

birdsex.2$Code <- factor(birdsex.2$Code)
birdsex.2$LastLiveRecord <- factor(birdsex.2$LastLiveRecord)
birdsex.2$CaptureDate <- factor(birdsex.2$CaptureDate)
birdsex.2$DeathDate <- factor(birdsex.2$DeathDate)
birdsex.2$SexEstimate2 <- factor(birdsex.2$SexEstimate2)


#     Next step is to split the database per colour ring, i.e. I will make several subsets of my
# database, one per colour ring

birdsex.2.split <- split(birdsex.2,birdsex.2$Code)



#     The following is a function to do everything I want to do. This is: for those colour codes shared
# by two birds, I want to keep only the birdid of the most recent bird, except if the difference
# between the LastLiveRecord2 of the oldest birdid and the ringing date (CaptureDate2) of the most
# recent bird is < 2 years, and the oldest bird did not die before the ringing date of the most recent
# bird.

# if diffyears is more than 2 years, it is ok, we can keep the most recent bird.
# if diffyears is less than 0 years, i.e. negative, and one of them did not die before November 2013,
#       exclude this colour combination from analysis because you wouldn't now which individual it is.
#       (you could check if sex is different but it might be mistake prone if you assigned the wrong
#         sex while watching the videos)
# if diffyears is in between 0 and 2, and the older bird did not die before the most recent was ringed
#       then you should also


# This is the function (thanks Malika!)

birdsex.split_FUN <- function(x) {
  x <- x[order(x$CaptureDate2),]
  x$prevCaptureDate2 = c(as.character(x$CaptureDate2[nrow(x)]),"1900-01-01")
  x$diffyears <-as.numeric((as.Date(x$prevCaptureDate2)-x$LastLiveRecord2))/365
  #return()
}


# Now I apply the above function to all little subsets of birdsex.2.split
dtf_split_percolorring_out1 <- lapply(birdsex.2.split, FUN=birdsex.split_FUN) 


dtf_split_percolorring_out2 <- data.frame(rownames(do.call(rbind,dtf_split_percolorring_out1)),
                                          do.call(rbind, dtf_split_percolorring_out1))

#nrow(dtf_split_percolorring_out2)  
rownames(dtf_split_percolorring_out2) <- NULL
dtf_split_percolorring_out2$X2 <- NULL

colnames(dtf_split_percolorring_out2) <- c('Code','diffyears')


#     Now I have the ones that I can definitely keep, i.e. the ones which difference between the old
# individual's lastseenalive and the new individual's ringing date is more than 2 years. Before that
# I will add this information to the duplicates dataset, i.e birdsex.2

birdsex.2 <- merge(birdsex.2,dtf_split_percolorring_out2,by="Code")


########################################################################################################
# # # 4.3.7. Now it's time to subset those that we can an put them back to birdsex.1
########################################################################################################

# First, sorting it by date

birdsex.2 <- birdsex.2[order(birdsex.2$Code,-as.numeric(birdsex.2$CaptureDate2)),]


# To make the next for loop simple

birdsex.2$Code2 <- as.numeric(birdsex.2$Code)


#     With this for loop I'm adding to the cool awesome list (i.e. birdsex.1) the unique colour codes
# that follow the criterion to be there. With this, birdsex.1 is finished.

id2<-c()

for(i in 1:nrow(birdsex.2)){
  
  if(birdsex.2$diffyears[i]>=2 | birdsex.2$BirdID[i]==7570 | birdsex.2$BirdID[i]==7544 |
       birdsex.2$BirdID[i]==6530 | birdsex.2$BirdID[i]==4898 | birdsex.2$BirdID[i]==4768){ #fucking exceptions that I don't want to deal with in a more sofisticated way, Friday, tired!
    
    #     CCMO: BirdID 6530 was finally included because BirdID 4380 was last seen in Nov2011, and CCMO 
    #           wasn't seen until August2014 (i.e. > 2.5 years in between). Plus, BirdID 4380 was born in
    #           2004, so it highly unlikely that it was still alive in 2014!!!!
    #
    #     GRMW: BirdID 4898 was finally included because BirdID 4975 was re-ringed in June, 2012
    #
    #     OWMW: BirdID 4768 was finally included because BirdID 4902 isn't seen since the nest in 
    #           summer 2010, plus both IDs have different sex. So far, the code does not show up in 
    #           the videos.
    
    
    if(birdsex.2$Code2[i] %in% id2 == TRUE){
      #print(birdsex.3$delete[i])
      
    } else {
      
      id2 <- append(id2,birdsex.2$Code2[i])
      birdsex.1 <- rbind(birdsex.1,birdsex.2[i,c(1:15)])
      
    }
  } 
}


# The remaining unresolved, and so excluded from analyses duplicates are the ones in birdsex.2

birdsex.2 <- subset(birdsex.2, birdsex.2$diffyears<2 & birdsex.2$BirdID!=7570 &
                      birdsex.2$BirdID!=7439 & birdsex.2$BirdID!=7544 & birdsex.2$BirdID!=7478 &
                      birdsex.2$BirdID!=6530 & birdsex.2$BirdID!=4380 & birdsex.2$BirdID!=4975 &
                      birdsex.2$BirdID!=4898 & birdsex.2$BirdID!=4768 & birdsex.2$BirdID!=4902)

# I've checked what's the problem with this colour combinations in a attempt of trying to rescue some.
# This is what I found:
#
#     MCDW: Definitely discarded, but it didn't show up in the videos anyway. Not included.
#     NBRM: it is definitely discarded. The combination is seen in the videos taking part in 7
#           interactions, but we cannot say who is the one. Not included.
#     OCMV: Definitely discarded, but it didn't show up in the videos anyway. Not included.
#     ODDM: It is definitely discarded. The combination is seen in the videos taking part in 6
#           interactions, but we cannot say who is the one. Not included.
#     ONWM: Definitely discarded, but it didn't show up in the videos anyway. Not included.
#     OVDM: Definitely discarded, but it didn't show up in the videos anyway. Not included.
#     RGRM: Definitely discarded, but it didn't show up in the videos anyway. Not included.
#     YGMW: Definitely discarded, but it didn't show up in the videos anyway. Not included.
#
#   POSSIBILITY OF INCLUDE THEM IF THEY SHOW UP IN THE VIDEOS!!! CHECK THEM OUT!
#
#     BNCM: BirdID 5594 could have been rescued from March 2014 on but the Code wasn't recorded anyway. Not included.
#     BOCM: BirdID 5593 same as before. Not included.
#     BRCM: BirdID 5595 same as before, but actually this birdID was re-ringed, so no probs. Not included.
#     DDMN: BirdID 4059 same as BRCM. Not included.
#     MCDN: BirdID 5428 same as BOCM. Not included.
#     MCGB: BirdID 5461 same as BRCM. Not included.
#


########################################################################################################
# # # 4.3.8. And the list of colour rings to be used is: birdsex.1(.ID) and the one to exclude birdsex.2(.ID)
########################################################################################################

birdsex.1 <- birdsex.1[,c(1,2,3,9,4,5,6,10,7,11,8,12)]


# I've included this line to exclude an individual from the list. This individual was missringed so we
# couldn't tell who was it anyway.

birdsex.1 <- birdsex.1[!(birdsex.1$Code=="VWDM"),]


# And this is to drop unused levels from factors

birdsex.1[,c(1,2,4,7,9,11)] <- lapply(birdsex.1[,c(1,2,4,7,9,11)], factor)


# Converting codes to lowercase so that they correspond to what it's written in dom.final

birdsex.1$Code <- as.factor(tolower(birdsex.1$Code))


# Saving the file for the following script.

write.csv(birdsex.1,"birdsex.1.csv",row.names=FALSE)


# The list of unique colour codes that can be used is:

birdsex.1.ID <- unique(birdsex.1$Code)


# And the final list with duplicates is:

birdsex.2 <- birdsex.2[,c(1,2,3,9,4,5,6,10,7,11,8,12)]


# And this is to drop unused levels from factors

birdsex.2[,c(1,2,4,7,9,11)] <- lapply(birdsex.2[,c(1,2,4,7,9,11)], factor)


# Converting codes to lowercase so that they correspond to what it's written in dom.final

birdsex.2$Code <- as.factor(tolower(birdsex.2$Code))


# The list of unique colour codes that need to be excluded from the dominance database dom.final is:

birdsex.2.ID <- unique(birdsex.2$Code)


# I've included this line to exclude an individual from the list. This individual was missringed so we
# couldn't tell who was it anyway.

birdsex.2.ID <- factor(append(as.character(birdsex.2.ID),"vwdm"))


# Now I'm cleaning the work space by removing uninterest objects

rm(birdsex.2.count, birdsex.2.split, birdsex.3, birdsex.3.count, birdsex.5, birdsex.6, birdsex.count,
   dtf_split_percolorring_out1,dtf_split_percolorring_out2,i, id, id2)


# # TO SAVE IT AS A CSV
# write.csv(birdsex,file="blabla.csv",sep=",",na="",dec = ".")


########################################################################################################
# 5. Excluding duplicate colour codes from dominance database (i.e. from dom.final) and from 
#    counts.ind database.
########################################################################################################

########################################################################################################
# # 5.1. Excluding duplicates from dom.final
########################################################################################################

dom.final.v2 <- subset(dom.final,!(dom.final$individual1 %in% birdsex.2.ID) & 
                         !(dom.final$individual2 %in% birdsex.2.ID))


########################################################################################################
# # 5.2. Excluding duplicates from dom.prop.wins
########################################################################################################

dom.prop.wins <- subset(dom.prop.wins,!(dom.prop.wins$individual1 %in% birdsex.2.ID) & 
                          !(dom.prop.wins$individual2 %in% birdsex.2.ID))


########################################################################################################
# # 5.3. Excluding duplicates from counts.ind
########################################################################################################

counts.ind <- subset(counts.ind,!(counts.ind$individual %in% birdsex.2.ID))


########################################################################################################
# 6. MORE CHECKING: checking that colour code and sex matches what's in the database:
########################################################################################################

#     This is a way of checking for missidentifications. Of course, it doesn't pick up all of them but
# at least it picks up those colour combinations whose sex isn't what it was expected. Furthermore,
# if the bird colour combination wasn't seen during the year before, it tells you so that you can 
# decide if you want to double-check the colour combination in the video.


########################################################################################################
# # 6.1. Checking those individuals that were assigned with two sexes
########################################################################################################

# Creating two data.frames: one with individual1 and the sex1, and another with indivual2 and the sex2

list.ind1.sex <- dom.final.v2[,c(13,15)]
list.ind2.sex <- dom.final.v2[,c(16,18)]


# Renaming the variables so that they have the same name for the rbind

names(list.ind1.sex) <- c("individual","sex")
names(list.ind2.sex) <- c("individual","sex")


# Pasting them together using rbind

superlist.sex<-rbind(list.ind1.sex,list.ind2.sex)


# Now I can create and individual_sex identifier to search for those double-sexed

superlist.sex$indsex <- paste(superlist.sex$individual,superlist.sex$sex,sep="_")


# reducing to unique values

superlist.sex.2 <- unique(superlist.sex)


#       and excluding the juveniles sightings as they make everything more complicated. Also, females and
# juveniles are, sometimes, mixed up

superlist.sex.3 <- superlist.sex.2[superlist.sex.2$sex!="j",]

superlist.sex.3 <- superlist.sex.3[order(superlist.sex.3$individual,superlist.sex.3$sex),]


# searching for those that are duplicated

checksex <- factor(superlist.sex.3[duplicated(superlist.sex.3$individual),1])


#       Now we can check the real sex of this individual and go to the database and check which ones have
# been wrongly assigned and check why. For this, the following for loop prints the real sex of those
# birds that have been assigned with two sexes in the dominance database. Therefore, go to the dominance
# database, have a look at those colour combinations and correct whatever needs to be corrected after
# rewatching the corresponding videos.

for (i in 1:nrow(birdsex.1)){
  if(birdsex.1$Code[i] %in% checksex){
    cat(c("\n",as.character(birdsex.1$Code[i]),"-> sex should always be:",as.character(birdsex.1$SexEstimate2[i])))
  }
}

# GO AND CHECK IF NEEDED


########################################################################################################
# # 6.2. Checking if there are misssexed individuals
########################################################################################################

#     For that, I will compare if the sex observed during video watching is the same as the one present
# in the database


# First I reduced the database info to birdID and sex and rename it to be the same as the following db

comp.birdsex.1 <- birdsex.1[,c(1,4)]
names(comp.birdsex.1) <- c("individual","sex")


# The same for the unique list coming from the dominance database

comp.superlist.sex.3 <- superlist.sex.3[,c(1,2)]


# Now I can merge both databases and reduce it to only the birdIDs in the dominance database

comp.sex <- merge(comp.superlist.sex.3,
                  comp.birdsex.1,
                  by="individual",
                  all.x=TRUE)


# and then renaming the variables to what makes sense

names(comp.sex) <- c("individual","observedsex","dbsex")


# dropping unused factor levels
comp.sex$observedsex <- factor(comp.sex$observedsex)


# Printing those cases where both sexes don't match, but first exclude NA's and print them too

missing.sex <- subset(comp.sex,is.na(comp.sex$dbsex))


comp.sex.2 <- subset(comp.sex,is.na(comp.sex$dbsex)==FALSE)


# printing the ones missing

missing.sex

for(i in 1:nrow(comp.sex.2)){
  
  if(comp.sex.2$observedsex[i] != comp.sex.2$dbsex[i]){
    
    cat(paste0("\nBird: ",
               as.character(comp.sex.2[i,1]),
               " should be: ",
               as.character(comp.sex.2[i,3])
    ))
    
  }
  
}


########################################################################################################
# 7. Checking that all colour combinations seen in the videos are real, i.e. are in the database
########################################################################################################

# Generating the list of unique colour codes from the birdsex database, before I split it

birdsex.ID <- unique(as.factor(tolower(birdsex$Code)))


#       I know I could combine everything but I decided it to leave separated to make much easier to
# understand. The fact that I keep doing it for individual1, 2, starter, Winner, and Loser is just to
# make sure that nothing escaped,eventhough it is very unlikely by now

# For individual1

for(i in 1:nrow(dom.final.v2)){
  
  if(!(dom.final.v2$individual1[i] %in% birdsex.ID) & dom.final.v2$individual1[i]!="mr--"){
    
    print(dom.final.v2$individual1[i])
    
  }
  
}


# For individual2

for(i in 1:nrow(dom.final.v2)){
  
  if(!(dom.final.v2$individual2[i] %in% birdsex.ID) & dom.final.v2$individual2[i]!="mr--"){
    
    print(dom.final.v2$individual2[i])
    
  }
  
}


# For starter

for(i in 1:nrow(dom.final.v2)){
  
  if(!(dom.final.v2$starter[i] %in% birdsex.ID) & dom.final.v2$starter[i]!="mr--"){
    
    print(dom.final.v2$starter[i])
    
  }
  
}


# For Winner

for(i in 1:nrow(dom.final.v2)){
  
  if(!(dom.final.v2$Winner[i] %in% birdsex.ID) & dom.final.v2$Winner[i]!="mr--"){
    
    print(dom.final.v2$Winner[i])
    
  }
  
}


# For Loser

for(i in 1:nrow(dom.final.v2)){
  
  if(!(dom.final.v2$Loser[i] %in% birdsex.ID) & dom.final.v2$Loser[i]!="mr--"){
    
    print(dom.final.v2$Loser[i])
    
  }
  
}

# GO AND CHECK IF NEEDED


########################################################################################################
# 8 SUMMARY information
########################################################################################################


########################################################################################################
# # 8.1 Showing the number of unique individuals per event
########################################################################################################


########################################################################################################
# # # 8.1.1 Showing the number of unique individuals per event = 9 events
########################################################################################################

# Quickly checking number of unique individuals per event

indperevent <- count(counts.ind,c("individual","event"))


#     Creating a different database per event with this for loop and printing the number of unique
# individuals per event at the same time

counter <- 1

for(i in levels(indperevent$event)){
  
  x<-subset(indperevent, indperevent$event==i)
  cat(paste0("\nNumber of individuals in ",
             i,
             " = ",
             length(x$individual)))
  assign(paste0("event",counter),x)
  counter <- counter + 1
}


########################################################################################################
# # # 8.1.2 Showing the number of unique individuals per event = 6 events (winter/summer)
########################################################################################################

# Creating an identifier that separates the database in winter,summer and another such as: winter1, summer1...

for(i in 1:nrow(dom.final.v2)){
  if(dom.final.v2$year[i] == 2013){
    dom.final.v2$eventSW[i] <- as.numeric("2013.5")
    dom.final.v2$season[i] <- "winter"
  } else if(dom.final.v2$year[i] == 2014){
    dom.final.v2$eventSW[i] <- as.numeric("2014.0")
    dom.final.v2$season[i] <- "summer"
  } else if(dom.final.v2$year[i] == 2015 & dom.final.v2$month[i] < 3){
    dom.final.v2$eventSW[i] <- as.numeric("2014.5")
    dom.final.v2$season[i] <- "winter"
  } else if(dom.final.v2$year[i] == 2015 & dom.final.v2$month[i] > 3){
    dom.final.v2$eventSW[i] <- as.numeric("2015.0")
    dom.final.v2$season[i] <- "summer"
  } else if(dom.final.v2$year[i] == 2016 & dom.final.v2$month[i] < 3){
    dom.final.v2$eventSW[i] <- as.numeric("2015.5")
    dom.final.v2$season[i] <- "winter"
  } else if(dom.final.v2$year[i] == 2016 & dom.final.v2$month[i] > 3){
    dom.final.v2$eventSW[i] <- as.numeric("2016.0")
    dom.final.v2$season[i] <- "summer"
  }
}

dom.final.v2$eventSW <- as.numeric(dom.final.v2$eventSW)
dom.final.v2$season <- factor(dom.final.v2$season)

#       Generating a database with the individuals and their number of interactions per sampling event.
# This way I know which individuals are seen in more than one trip

# Creating two data.frames: one with individual1 and the event, and another with indivual2 and the event

list.ind1.eventSW<-dom.final.v2[,c("individual1","eventSW")]
list.ind2.eventSW<-dom.final.v2[,c("individual2","eventSW")]


# Renaming the variables so that they have the same name for the rbind

names(list.ind1.eventSW) <- c("individual","eventSW")
names(list.ind2.eventSW) <- c("individual","eventSW")


# Pasting them together using rbind

superlist.eventSW<-rbind(list.ind1.eventSW,list.ind2.eventSW)


#       Now I can count the number of times each individual showed up in each event by counting the
# number of event each individual showed up in

superlist.eventSW <- count(superlist.eventSW,c("individual","eventSW"))

names(superlist.eventSW) <- c("individual","eventSW","freqppereventSW")

#hist(superlist.event$freqpperevent,breaks=20)


# Saving the database as csv to be used in following scripts

write.csv(superlist.eventSW,"interactionsperevent.csv",row.names=FALSE)


# This allows me to count the number of sampling events each individual showed up in

onlyindSW <- superlist.eventSW$individual

onlyindSW <- as.data.frame(onlyindSW)


# Counting number of sampling events per individual

superlist.num.eventSW <- count(onlyindSW,"onlyindSW")

names(superlist.num.eventSW) <- c("individual","freqofeventsSW")


########################################################################################################
# # 8.2 Printing the reduction in number of interactions and the final sample size
########################################################################################################

# Now printing how many have been removed

int.dupl <- length(dom.final$individual1)-length(dom.final.v2$individual1)

per.int.dupl <- (int.dupl*100)/length(dom.final$individual1)


# Total excluded

int.excluded <- length(dom$individual1)-length(dom.final.v2$individual1)

perc.int.excluded <- (int.excluded*100)/length(dom$individual1)


# Before anything, I have to make sure that the duplicates are excluded from all databases.

ind.counts <- subset(ind.counts,!(ind.counts$individual %in% birdsex.2.ID))
ind.counts.year <- subset(ind.counts.year,!(ind.counts.year$individual %in% birdsex.2.ID))
superlist.event <- subset(superlist.event,!(superlist.event$individual %in% birdsex.2.ID))
ind.counts.date <- subset(ind.counts.date,!(ind.counts.date$individual %in% birdsex.2.ID))
superlist.year.2 <- subset(superlist.year.2,!(superlist.year.2$individual %in% birdsex.2.ID))
superlist.num.event <- subset(superlist.num.event,!(superlist.num.event$individual %in% birdsex.2.ID))
superlist.num.date <- subset(superlist.num.date,!(superlist.num.date$individual %in% birdsex.2.ID))


# Printing the number of individuals that have more than 8 interactions in total

morethan8 <- subset(ind.counts,ind.counts$totalfreq>8)
length(unique(morethan8$individual))
list_morethan8 <- factor(morethan8$individual)

# Printing the number of individuals that have more than 8 interactions in total
morethan8pereventSW <- subset(superlist.eventSW,superlist.eventSW$freqppereventSW>8)
morethan8pereventSW$indevent <- factor(paste(morethan8pereventSW$individual,
                                             morethan8pereventSW$eventSW,
                                             sep="_"))


# Saving the database as csv to be used in following scripts

write.csv(morethan8pereventSW,"morethan8pereventSW.csv",row.names=FALSE)

# list_morethan8pereventSW <- factor(paste(morethan8pereventSW$individual,
#                                          morethan8pereventSW$eventSW,
#                                          sep="_"))


# Printing the number of individuals that are present in more than 1 event

morethan1eventSW <- subset(superlist.num.eventSW,superlist.num.eventSW$freqofeventsSW>1)


# Printing in a text file

sink("summaries/summary_interactions.txt")

cat(paste0("\nThe number of interactions with doubts is: ",
           doubts.with.int,
           " (",
           round(perc.doubts.with.int,digits=1),
           "%).",
           "\nThe number of interactions involving uncertain IDs is: ",
           doubts.with.ID,
           " (",
           round(perc.doubts.with.ID,digits=1),
           "%).",
           "\nThe number of interactions involving notidentified individuals is: ",
           notidentfied.ind,
           " (",
           round(perc.notidentfied.ind,digits=1),
           "%).",
           "\nThe number of interactions involving unringed individuals is: ",
           unringed.ind,
           " (",
           round(perc.unringed.ind,digits=1),
           "%).",
           "\nThe number of interactions involving individuals missing 1 or more colour rings is: ",
           miss.rings.ind,
           " (",
           round(perc.miss.rings.ind,digits=1),
           "%).",
           
           "\nThe number of interactions involving duplicated colour codes is: ",
           int.dupl,
           " (",
           round(per.int.dupl,digits=1),
           "%).",
           "\n",
           "\nThe TOTAL number of interactions finally excluded is: ",
           int.excluded,
           " (",
           round(perc.int.excluded,digits=1),
           "%).",
           "\n\nThe FINAL sample sizes are: ",
           "\n\n\t\t\t\t --> \t",
           length(dom.final.v2$individual1),
           " interactions (",
           round((nrow(dom.final.v2[dom.final.v2$level=="1",])*100)/length(dom.final.v2$individual1),digits=1),
           "% displacements)",
           "\n\n\t\t\t\t --> \t",
           length(unique(counts.ind$individual)),
           " individuals",
           "\n\n\t\t\t\t --> \t",
           round(mean(superlist.eventSW$freqppereventSW),digits=1),
           " interactions/individual/eventSW",
           "\n\n\t\t\t\t --> \t",
           length(unique(morethan8pereventSW$individual)),
           " individuals with more than 8 interactions/eventSW (",
           length(morethan8pereventSW$individual),
           " observations)",
           "\n\n\t\t\t\t --> \t",
           length(morethan1eventSW$individual),
           " individuals present in more than 1 eventSW"))

sink()


######################################################################################################
# # 8.3 Plotting some informative histograms
########################################################################################################

par(mfrow=c(1,1))


#to save the figure as tiff
tiff("plots/hist_interacions_per_event.tiff",
     height=18, width=27,units='cm', compression="lzw", res=300) 

#number of total interactions per event per individual
hist(superlist.eventSW$freqppereventSW,breaks=max(superlist.eventSW$freqppereventSW),
     main = "",
     xlab = "Number of interactions per event (summer/winter)",
     ylab = "",
     ylim = c(0,70),
     xlim = c(0,400),
     col="grey75",
     axes=FALSE,
     cex.lab=1.75,
     right=FALSE)
#title(ylab="Number of individuals", line=2.2, cex.lab=1.75)
axis(1,at = seq(0,400,by=50),lwd=1)
axis(2,at = seq(0,70,by=10),lwd=1,line=-0.5, las=2)

dev.off()


#to save the figure as tiff
tiff("plots/hist_events_per_individual.tiff",
     height=18, width=27,units='cm', compression="lzw", res=300) 

#number of eventsSW interacting per individual
hist(superlist.num.eventSW$freqofevents,breaks=6,
     main = "",
     xlab = "Number of events (summer/winter)",
     ylim = c(0,240),
     xlim = c(1,6),
     ylab = "",
     col="grey75",
     axes=FALSE,
     cex.lab=1.75,
     right=FALSE)
#title(ylab="Number of individuals", line=2.2, cex.lab=1.75)
axis(1,at = seq(1,6,by=1),lwd=1)
axis(2,at = seq(0,240,by=40),lwd=1,line=-0.75, las=2)

dev.off()


#to save the figure as tiff
tiff("plots/hist_dates_per_individual.tiff",
     height=18, width=27,units='cm', compression="lzw", res=300) 

#number of dates interacting per individual
hist(superlist.num.date$freqofdates,freq=TRUE,breaks=18,
     main = "",
     xlab = "Number of days",
     ylim = c(0,125),
     xlim = c(1,18),
     ylab = "",
     col="grey75",
     axes=FALSE,
     cex.lab=1.75,
     right=FALSE)
title(ylab="Number of individuals", line=2.2, cex.lab=1.75)
axis(1,at = seq(1,18,by=1),lwd=1)
axis(2,at = seq(0,125,by=25),lwd=1,line=-0.75, las=2)

dev.off()


# Saving the file for the following script

write.csv(dom.final.v2,"dom.final.v2.csv",row.names=FALSE)