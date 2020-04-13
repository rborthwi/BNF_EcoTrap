#Source data for assessing Bankhead National Forest as an ecological trap for cerulean warblers.

#This script includes the  
#Created by R. Borthwick, 11/17/2019

library(tidyverse)
library(readxl)

##################################################
###EVERYTHING HERE IS EXPLORATORY MORPHOMETRICS (JONES ET AL. 2005 RESPONSE) AND MAPPING MY SITES##############
###################################################
cw_data <- getwd()%>%
  paste0("/data/CERW_data_BNFEcoTrap.xlsx")%>%
  read_excel()

#View(blood)
#str(blood)
##I want Age, Sex, Collection_Team, Geo, State, Location, and Region to all be factors:
cw_data$Age<-as.factor(cw_data$Age)
cw_data$Sex<-as.factor(cw_data$Sex)
cw_data$Collection_Team<-as.factor(cw_data$Collection_Team)
cw_data$Geo<-as.factor(cw_data$Geo)
cw_data$State<-as.factor(cw_data$State)
cw_data$Location<-as.factor(cw_data$Location)
cw_data$Region<-as.factor(cw_data$Region)

##I want Band ID and Name to be Character strings, so they're left. These are numbers:

cw_data$Lat<-as.numeric(cw_data$Lat)
cw_data$Long<-as.numeric(cw_data$Long)
cw_data$mass<-as.numeric(cw_data$mass)
cw_data$wg<-as.numeric(cw_data$wg)
cw_data$tl<-as.numeric(cw_data$tl)

#Save it
saveRDS(cw_data,"C:/Users/richard/Documents/PhD/Publications/BNF_EcoTrap/data/cw_data.rds")

