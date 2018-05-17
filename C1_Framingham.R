# R ANALYSIS FOR CHAPTER 1: EXPLORATORY DATA ANALYSIS #
##	FRAMINGHAM HEART STUDY DATA		##


## LOAD RELEVANT PACKAGES ##

library(ggplot2)
library(dafs)




## SET WORKING DIRECTORY ##

setwd('PATH')




## READ IN THE DATA ##

FData = read.csv('frmgham2.csv',header=TRUE)





## SUMMARIES FOR CONTINUOUS VARIABLES ##

summary(FData)




## SCATTERPLOT MATRIX ##

FData = FData[complete.cases(FData[,c("TIMEHYP","AGE","CIGPDAY","TOTCHOL")]),]
SPData = as.data.frame(cbind(Cholesterol=FData$TOTCHOL,Age=FData$AGE,Cigarettes=FData$CIGPDAY,Time=FData$TIMEHYP))

pairsDAFS(SPData, main="Histograms, Scatter Plots, and Pairwise Correlations")





## PROPORTIONS OF CATEGORICAL VARIABLES ##

summary(as.data.frame(FData[,c(2,10,19,31)]))

table(FData$SEX)
table(FData$DIABETES)
table(FData$PREVHYP)
table(FData$HYPERTEN)





## CONTINGENCY TABLES OF HYPERTENSION VERSUS OTHER VARIABLES ##

table(FData$SEX,FData$HYPERTEN)
table(FData$DIABETES,FData$HYPERTEN)





## SUMMARY OF CONTINUOUS VARIABLES BY HYPERTENSION ##

(TOTCHOL_Means = aggregate(FData$TOTCHOL~as.factor(FData$HYPERTEN),FUN=function(x)summary(x,na.rm=TRUE)))
(TOTCHOL_Variances = aggregate(FData$TOTCHOL~as.factor(FData$HYPERTEN),FUN=function(x)var(x,na.rm=TRUE)))

(AGE_Means = aggregate(FData$AGE~as.factor(FData$HYPERTEN),FUN=function(x)summary(x,na.rm=TRUE)))
(AGE_Variance = aggregate(FData$AGE~as.factor(FData$HYPERTEN),FUN=function(x)var(x,na.rm=TRUE)))

(CIGPDAY_Means = aggregate(FData$CIGPDAY~as.factor(FData$HYPERTEN),FUN=function(x)summary(x,na.rm=TRUE)))
(CIGPDAY_Variances = aggregate(FData$CIGPDAY~as.factor(FData$HYPERTEN),FUN=function(x)var(x,na.rm=TRUE)))

(TIMEHYP_Means = aggregate(FData$TIMEHYP~as.factor(FData$HYPERTEN),FUN=function(x)summary(x,na.rm=TRUE)))
(TIMEHYP_Variances = aggregate(FData$TIMEHYP~as.factor(FData$HYPERTEN),FUN=function(x)var(x,na.rm=TRUE)))






## SCATTER PLOTS OF HYPERTEN VERSUS EACH CONTINUOUS VARIABLE ##


## HYPERTENSIVE VERSUS AGE ##

ggplot(FData, aes(x=AGE,y=HYPERTEN)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Hypertensive Versus Age") + 
	xlab("Age") + 
	ylab("Hypertensive") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))



## HYPERTENSIVE VERSUS CIGARETTES PER DAY ##

qplot(FData$CIGPDAY,FData$HYPERTEN) + geom_smooth(method="loess")



## HYPERTENSIVE VERSUS TOTAL CHOLESTEROL ##

ggplot(FData, aes(x=TOTCHOL,y=HYPERTEN)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Hypertensive Versus Cholesterol") + 
	xlab("Cholesterol") + 
	ylab("Hypertensive") + ylim(0,1) + 
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))


