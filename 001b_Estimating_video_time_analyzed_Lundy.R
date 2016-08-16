
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
# Loading dominance database
########################################################################################################

dom <- read.table("MegaDataBase-v97-201311-201606-FY-Dominance_Lundy_20160816.csv",header=TRUE,sep=',')


