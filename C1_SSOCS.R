# R ANALYSIS FOR CHAPTER 1: EXPLORATORY DATA ANALYSIS #
##	SCHOOL SURVEY ON CRIME AND SAFETY DATA		##




## LOAD RELEVANT PACKAGES ##

library(sas7bdat)
library(plyr)
library(ggplot2)
library(dafs)





## SET WORKING DIRECTORY ##

setwd('PATH')





## READ IN THE DATA ##

SSOCS07 = read.sas7bdat('SSOCS07_Data.sas7bdat')




## RENAME VARIABLES USING MEANINGFUL LABELS ##

SSOCS07 = rename(SSOCS07,c("C0514"="suspensions","C0134"="uniforms","C0116"="metal_detectors","C0188"="tipline","C0178"="counseling","C0562"="crime","C0268"="discipline_training","C0276"="behavioral_training","C0508"="insubordination","C0510"="insubordination_removal","C0526"="percent_limited_English","C0532"="below_15th","C0376"="bullying"))





## CREATE INDICATOR VARIABLES FOR CHARACTERISTICS OF INTEREST ##

SSOCS07$uniforms = ifelse((SSOCS07$uniforms==2),0,1)
SSOCS07$metal_detectors = ifelse((SSOCS07$metal_detectors==2),0,1)
SSOCS07$tipline = ifelse((SSOCS07$tipline==2),0,1)
SSOCS07$counseling = ifelse((SSOCS07$counseling==2),0,1)
SSOCS07$crime = 4 - SSOCS07$crime
SSOCS07$discipline_training = ifelse((SSOCS07$discipline_training==2),0,1)
SSOCS07$behavioral_training = ifelse((SSOCS07$behavioral_training==2),0,1)





## SUMMARIES FOR CONTINUOUS VARIABLES ##

summary(cbind(SSOCS07$suspensions,SSOCS07$insubordination,SSOCS07$percent_limited_English,SSOCS07$below_15th))
var(cbind(SSOCS07$suspensions,SSOCS07$insubordination,SSOCS07$percent_limited_English,SSOCS07$below_15th))




## CREATE LOG-TRANSFORMED OUTCOMES ##

SSOCS07$log.suspensions = log(SSOCS07$suspensions+0.01)
SSOCS07$log.insubordination = log(SSOCS07$insubordination+0.01)





## SCATTERPLOT MATRIX ##

PlotData = as.data.frame(cbind(LogSuspensions=SSOCS07$log.suspensions,LogInsubordinates=SSOCS07$log.insubordination,LimitedEnglish=SSOCS07$percent_limited_English,PercentBelow=SSOCS07$below_15th,Crime=crime))


pairsDAFS(PlotData[,c(1,2,3,4)],main="Histograms, Scatter Plots, and Pairwise Correlations")





## BOX PLOTS OF (LOG-TRANSFORMED) SUSPENSIONS BY CRIME LEVEL ##

ggplot(PlotData, aes(x=as.factor(crime),y=log.suspensions)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plots of Log-Suspensions by Crime") + 
	xlab("Crime") + scale_x_discrete(labels=c("Low","Moderate","High")) +
	ylab("Log-Suspensions") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24),legend.position="none", panel.background = element_rect(fill = "grey92"))







## PROPORTIONS OF CATEGORICAL VARIABLES ##

table(SSOCS07$bullying)
table(SSOCS07$uniforms)
table(SSOCS07$metal_detectors)
table(SSOCS07$tipline)
table(SSOCS07$counseling)
table(SSOCS07$crime)
table(SSOCS07$behavioral_training)
table(SSOCS07$discipline_training)

prop.table(table(SSOCS07$bullying))
prop.table(table(SSOCS07$uniforms))
prop.table(table(SSOCS07$metal_detectors))
prop.table(table(SSOCS07$tipline))
prop.table(table(SSOCS07$counseling))
prop.table(table(SSOCS07$crime))
prop.table(table(SSOCS07$behavioral_training))
prop.table(table(SSOCS07$discipline_training))




## CONTINGENCY TABLES OF BULLYING VERSUS OTHER VARIABLES ##

table(SSOCS07$uniforms,SSOCS07$bullying)
table(SSOCS07$metal_detectors,SSOCS07$bullying)
table(SSOCS07$tipline,SSOCS07$bullying)
table(SSOCS07$counseling,SSOCS07$bullying)
table(SSOCS07$crime,SSOCS07$bullying)
table(SSOCS07$behavioral_training,SSOCS07$bullying)
table(SSOCS07$discipline_training,SSOCS07$bullying)






## SUMMARY OF CONTINUOUS VARIABLES BY BULLYING ##

(insubordination_Means = aggregate(SSOCS07$insubordination~as.factor(SSOCS07$bullying),FUN=function(x)mean(x,na.rm=TRUE)))
(insubordination_Variances = aggregate(SSOCS07$insubordination~as.factor(SSOCS07$bullying),FUN=function(x)var(x,na.rm=TRUE)))

(percent_limited_English_Means = aggregate(SSOCS07$percent_limited_English~as.factor(SSOCS07$bullying),FUN=function(x)mean(x,na.rm=TRUE)))
(percent_limited_English_Variances = aggregate(SSOCS07$percent_limited_English~as.factor(SSOCS07$bullying),FUN=function(x)var(x,na.rm=TRUE)))

(below_15th_Means = aggregate(SSOCS07$below_15th~as.factor(SSOCS07$bullying),FUN=function(x)mean(x,na.rm=TRUE)))
(below_15th_Variances = aggregate(SSOCS07$below_15th~as.factor(SSOCS07$bullying),FUN=function(x)var(x,na.rm=TRUE)))







## SUMMARY OF SUSPENSIONS BY CATEGORICAL VARIABLES ##

suspensions_Means = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$bullying),FUN=function(x)mean(x,na.rm=TRUE))
suspensions_Variances = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$bullying),FUN=function(x)var(x,na.rm=TRUE))


(suspensions_Means = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$uniforms),FUN=function(x)mean(x,na.rm=TRUE)))
(suspensions_Variances = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$uniforms),FUN=function(x)var(x,na.rm=TRUE)))

(suspensions_Means = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$metal_detectors),FUN=function(x)mean(x,na.rm=TRUE)))
(suspensions_Variances = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$metal_detectors),FUN=function(x)var(x,na.rm=TRUE)))

(suspensions_Means = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$tipline),FUN=function(x)mean(x,na.rm=TRUE)))
(suspensions_Variances = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$tipline),FUN=function(x)var(x,na.rm=TRUE)))

(suspensions_Means = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$counseling),FUN=function(x)mean(x,na.rm=TRUE)))
(suspensions_Variances = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$counseling),FUN=function(x)var(x,na.rm=TRUE)))

(suspensions_Means = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$crime),FUN=function(x)mean(x,na.rm=TRUE)))
(suspensions_Variances = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$crime),FUN=function(x)var(x,na.rm=TRUE)))

(suspensions_Means = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$behavioral_training),FUN=function(x)mean(x,na.rm=TRUE)))
(suspensions_Variances = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$behavioral_training),FUN=function(x)var(x,na.rm=TRUE)))

(suspensions_Means = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$discipline_training),FUN=function(x)mean(x,na.rm=TRUE)))
(suspensions_Variances = aggregate(SSOCS07$suspensions~as.factor(SSOCS07$discipline_training),FUN=function(x)var(x,na.rm=TRUE)))



## BOX PLOTS OF SUSPENSIONS BY CATEGORICAL VARIABLES ##

boxplot(SSOCS07$suspensions~SSOCS07$uniforms)
boxplot(SSOCS07$suspensions~SSOCS07$metal_detectors)
boxplot(SSOCS07$suspensions~SSOCS07$tipline)
boxplot(SSOCS07$suspensions~SSOCS07$counseling)
boxplot(SSOCS07$suspensions~SSOCS07$crime)
boxplot(SSOCS07$suspensions~SSOCS07$discipline_training)
boxplot(SSOCS07$suspensions~SSOCS07$behavioral_training)








