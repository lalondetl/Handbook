# R FILE FOR FORMATTING OF FIRE-CLIMATE INTERACTION DATA FOR ANALYSIS #




## LOAD RELEVANT PACKAGES ##

library(gdata)
library(ggplot2)
library(plyr)
library(car)




## SET THE WORKING DIRECTORY ##

setwd('PATH') 




## USE PACIFIC NORTHWEST (PNW), INTERIOR WEST (IW), SOUTHWEST (SW) ##

Fire_PNW = read.xls('trouet2010.xls',sheet=2)
Fire_IW = read.xls('trouet2010.xls',sheet=4)
Fire_SW = read.xls('trouet2010.xls',sheet=5)






## TURN 1'S INTO 0'S, 2'S INTO 1'S ##

Fire_PNW[Fire_PNW == 1] = 0
Fire_IW[Fire_IW == 1] = 0
Fire_SW[Fire_SW == 1] = 0

Fire_PNW[Fire_PNW == 2] = 1
Fire_IW[Fire_IW == 2] = 1
Fire_SW[Fire_SW == 2] = 1






## FIRST AGGREGATE DATA BY SITE ##

Fire_PNW$FrostySum = rowSums(Fire_PNW[,c(2:20)],na.rm=TRUE)
Fire_PNW$NileCreekSum = rowSums(Fire_PNW[,c(21:31)],na.rm=TRUE)
Fire_PNW$QuartziteSum = rowSums(Fire_PNW[,c(32:36)],na.rm=TRUE)
Fire_PNW$SouthDeepSum = Fire_PNW[,37]
Fire_PNW$TwentyMileSum = rowSums(Fire_PNW[,c(38:67)],na.rm=TRUE)

Fire_IW$CheesmanLakeSum = rowSums(Fire_IW[,c(10:18)],na.rm=TRUE)
Fire_IW$ManitouSum = rowSums(Fire_IW[,c(38:44)],na.rm=TRUE)
Fire_IW$OldTreeSum = rowSums(Fire_IW[,c(50:62)],na.rm=TRUE)
Fire_IW$AshenfelderSum = rowSums(Fire_IW[,c(72:77)],na.rm=TRUE)

Fire_SW$BlacksSum = rowSums(Fire_SW[,c(2,45:54)],na.rm=TRUE)
Fire_SW$RoundMountainSum = rowSums(Fire_SW[,c(38:42)],na.rm=TRUE)
Fire_SW$CerroHoyaMarchanitaSum = rowSums(Fire_SW[,c(5:8,11:14,18:21)],na.rm=TRUE)





## MERGE BY YEAR ##

PNW_IW = merge(Fire_PNW[,c(1,69,70,73)],Fire_IW[,c(1,83:86)],by.x="Year",by.y="Year",all=TRUE)
FireData = merge(PNW_IW,Fire_SW[,c(1,55:57)],by.x="Year",by.y="Year",all=TRUE)






## TURN 1'S INTO 0'S, 2'S INTO 1'S, ELIMINATE 0'S ##

Fire_PNW[Fire_PNW == 0] = NA
Fire_IW[Fire_IW == 0] = NA
Fire_SW[Fire_SW == 0] = NA

Fire_PNW[Fire_PNW == 1] = 0
Fire_IW[Fire_IW == 1] = 0
Fire_SW[Fire_SW == 1] = 0

Fire_PNW[Fire_PNW == 2] = 1
Fire_IW[Fire_IW == 2] = 1
Fire_SW[Fire_SW == 2] = 1







## NEXT AGGREGATE DATA BY DECADE ##



## CREATE DECADE VARIABLE ##

FireData$Decade = (FireData$Year %/% 10)*10





## RESHAPE THE DATA: 		##
##	DECADE, SITE, COUNT	##

FireData_Long = rbind(cbind(FireData$Decade,FireData$FrostySum,rep("Frosty",length(FireData$FrostySum))),cbind(FireData$Decade,FireData$NileCreekSum,rep("NileCreek",length(FireData$NileCreekSum))),cbind(FireData$Decade,FireData$TwentyMileSum,rep("TwentyMile",length(FireData$TwentyMileSum))),cbind(FireData$Decade,FireData$CheesmanLakeSum,rep("CheesmanLake",length(FireData$CheesmanLakeSum))),cbind(FireData$Decade,FireData$ManitouSum,rep("Manitou",length(FireData$ManitouSum))),cbind(FireData$Decade,FireData$OldTreeSum,rep("OldTree",length(FireData$OldTreeSum))),cbind(FireData$Decade,FireData$AshenfelderSum,rep("Ashenfelder",length(FireData$AshenfelderSum))),cbind(FireData$Decade,FireData$BlacksSum,rep("Blacks",length(FireData$BlacksSum))),cbind(FireData$Decade,FireData$RoundMountainSum,rep("RoundMountain",length(FireData$RoundMountainSum))),cbind(FireData$Decade,FireData$CerroHoyaMarchanitaSum,rep("CerroHoyaMarchanita",length(FireData$CerroHoyaMarchanitaSum))))

FireData_Long = as.data.frame(FireData_Long)
colnames(FireData_Long) = c("Decade","Count","Site")





## AGGREGATE COUNTS BY DECADE, SITE ##

Count_Sums = aggregate(as.numeric(FireData_Long$Count)~FireData_Long$Decade+FireData_Long$Site,FUN=function(x)sum(x,na.rm=TRUE))
Count_Means = aggregate(as.numeric(FireData_Long$Count)~FireData_Long$Decade+FireData_Long$Site,FUN=function(x)mean(x,na.rm=TRUE))
Count_Variances = aggregate(as.numeric(FireData_Long$Count)~FireData_Long$Decade+FireData_Long$Site,FUN=function(x)var(x,na.rm=TRUE))




## FINAL DATA SET FOR ANALYSIS ##

FireData_Aggregated = as.data.frame(cbind(Count_Sums,Count_Means[,3],Count_Variances[,3]))



## ADD REGION ##

FireData_Aggregated$Region = rep(NA,nrow(FireData_Aggregated))
for(i in 1:nrow(FireData_Aggregated))
{
	if(FireData_Aggregated[i,2]=="Frosty" | FireData_Aggregated[i,2]=="NileCreek" | FireData_Aggregated[i,2]=="TwentyMile"){FireData_Aggregated$Region[i]="PNW"}
	if(FireData_Aggregated[i,2]=="CheesmanLake" | FireData_Aggregated[i,2]=="Manitou" | FireData_Aggregated[i,2]=="OldTree" | FireData_Aggregated[i,2]=="Ashenfelder"){FireData_Aggregated$Region[i]="IW"}
	if(FireData_Aggregated[i,2]=="Blacks" | FireData_Aggregated[i,2]=="RoundMountain" | FireData_Aggregated[i,2]=="CerroHoyaMarchanita"){FireData_Aggregated$Region[i]="SW"}
}

colnames(FireData_Aggregated) = c("Decade","Site","Decade_Count","Decade_Mean","Decade_Var","Region")
FireData_Aggregated$Region = as.factor(FireData_Aggregated$Region)



## WRITE OUT FINAL FILE ##

write.csv(FireData_Aggregated, file="FireData_Aggregated.csv",quote=FALSE,row.names=FALSE)










