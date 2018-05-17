# R ANALYSIS FOR CHAPTER 8: LONGITUDINAL MODELING #
##	FIRE-CLIMATE INTERACTION DATA		##




## LOAD RELEVANT PACKAGES ##

library(ggplot2)
library(joineR)
library(nortest)
library(lme4)
library(gamlss.mx)




## SET THE WORKING DIRECTORY ##
setwd('PATH')




## READ IN THE DATA ##

FireData_Aggregated = read.csv("FireData_Aggregated.csv",header=TRUE)






## DESCRIPTIVES OF THE OUTCOME ONLY ##

qplot(FireData_Aggregated$Decade_Count)




## HISTOGRAM OF DECADE FIRE COUNTS ##

ggplot(FireData_Aggregated, aes(Decade_Count)) + 
	geom_histogram(aes(y=..density..),col='grey45') + 
	geom_density() +
	ggtitle("Histogram of Decade Fire Counts") + 
	xlab("Decade Fire Counts") + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))




## BOX PLOT OF DECADE FIRE COUNTS ##

ggplot(FireData_Aggregated, aes(x=1,y=Decade_Count)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plot of Decade Fire Counts") + 
	xlab("Decade Fire Count Box Plot") + 
	ylab("Decade Fire Counts") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"),legend.position="none")






## DESCRIPTIVES INCLUDING PREDICTORS ##

aggregate(FireData_Aggregated$Decade_Count~FireData_Aggregated$Site,FUN=function(x)mean(x,na.rm=TRUE))
aggregate(FireData_Aggregated$Decade_Count~FireData_Aggregated$Region,FUN=function(x)mean(x,na.rm=TRUE))

qplot(FireData_Aggregated$Site,FireData_Aggregated$Decade_Count)
qplot(FireData_Aggregated$Region,FireData_Aggregated$Decade_Count)





## BOX PLOTS OF DECADE FIRE COUNT BY SITE ##

ggplot(FireData_Aggregated, aes(x=as.factor(Site),y=Decade_Count)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plots of Decade Fire Count by Site") + 
	xlab("Site") + 
	ylab("Decade Fire Count") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), axis.text.x=element_text(angle=45), panel.background = element_rect(fill = "grey92"),legend.position="none")





## BOX PLOTS OF DECADE FIRE COUNT BY REGION ##

ggplot(FireData_Aggregated, aes(x=as.factor(Region),y=Decade_Count)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plots of Decade Fire Count by Region") + 
	xlab("Region") + 
	ylab("Decade Fire Count") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"),legend.position="none")





## SCATTER PLOT OF DECADE FIRE COUNT VERSUS TIME ##

ggplot(FireData_Aggregated, aes(x=Decade,y=Decade_Count)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Decade Fire Count Versus Decade") + 
	xlab("Decade") + 
	ylab("Decade Fire Count") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))



## SPAGHETTI PLOT OF DECADE FIRE COUNTS BY TIME ##

interaction.plot(x.factor=as.factor(Decade),trace.factor=as.factor(Site),response=Decade_Count,fun=function(x)mean(x,na.rm=TRUE))




## SCATTER PLOT OF DECADE FIRE COUNT MEANS BY REGION ##

interaction.plot(x.factor=as.factor(Decade),trace.factor=as.factor(Region),response=Decade_Count,fun=function(x)mean(x,na.rm=TRUE),main="Plot of Fire Count Means by Region",xlab="Decade",ylab="Decade Fire Count",trace.label="Region",cex.lab=1.4,cex.main=1.6)





## SCATTER PLOT OF DECADE FIRE COUNT MEANS BY REGION ##

plotData = FireData_Aggregated[complete.cases(FireData_Aggregated[,c("Decade","Decade_Count","Region")]),]

ggplot(plotData, aes(x=Decade, y=Decade_Count, group=Region)) +
	stat_summary(fun.y=mean, geom="point") +
	stat_summary(fun.y=mean, geom="line") +
	geom_line(aes(group=Region,linetype=as.factor(Region))) +
	scale_linetype_manual(values=c("dotted","longdash","solid")) +
	ggtitle("Plot of Fire Count Means by Region") + 
	xlab("Decade") + 
	ylab("Decade Fire Count") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))






## PRODUCE RESIDUALS FROM NON-CORRELATED MODEL ##

residuals = glm(Decade_Count~as.factor(Region)+Decade,family=poisson,DATA=FireData_Aggregated)$residuals



## SCATTERPLOT MATRIX ##

residualData = as.data.frame(cbind(residuals,FireData_Aggregated$Decade,FireData_Aggregated$Site))
rDataWide = reshape(residualData,timevar="Decade",idvar="Site",direction="wide")

pairs(rDataWide[,-1])



## RESIDUAL CORRELATION MATRIX ##

cor(rDataWide[,-1],use="complete")



## VARIOGRAM ##

CountVG = variogram(FireData_Aggregated$Site,FireData_Aggregated$Decade,FireData_Aggregated$residuals)

plot.vargm(CountVG,main="Variogram for Decade Fire Counts")





## DEFINE VARIABLES FOR MODELS ##

FireData_Aggregated$Region = as.factor(FireData_Aggregated$Region)
FireData_Aggregated$Decade2 = (as.numeric(FireData_Aggregated$Decade))^2





## RANDOM-INTERCEPT POISSON MODEL ##

RIPModel1 = glmer(Decade_Count~Region+as.numeric(Decade)+as.numeric(Decade)*Region+(1|Site%in%Region),family=poisson,data=FireData_Aggregated)



## SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES ##

glmerData1 = as.data.frame(cbind(fitted=fitted(RIPModel1),residuals=summary(RIPModel1)@resid))

ggplot(glmerData1, aes(x= fitted,y= residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Residual Plot for Linear Decade Model") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))






## RANDOM INTERCEPT POISSON MODEL ##
##	QUADTRATIC TERM INCLUDED		##
##	LINEAR INTERACTIONS WITH REGION INCLUDED	##

RIPModel = glmer(Decade_Count~Region+as.numeric(Decade)+Decade2+as.numeric(Decade)*Region+(1|Site%in%Region),family=poisson,data=FireData_Aggregated)



## MODEL SUMMARY ##

summary(summary(RIPModel)@resid)




## SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES ##

glmerData2 = as.data.frame(cbind(fitted=fitted(RIPModel),residuals=summary(RIPModel)@resid))

ggplot(glmerData2, aes(x= fitted,y= residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Residual Plot for Quadratic Decade Model") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))







## SCATTER PLOT OF RESIDUALS VERSUS TIME ##

glmerData3 = as.data.frame(cbind(Decade=FireData_Aggregated$Decade,residuals=summary(RIPModel)@resid))

ggplot(glmerData3, aes(x= Decade,y= residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Residuals Versus Decade") + 
	xlab("Decade") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))






## NORMAL PROBABILITY PLOT OF RESIDUALS ##

residData = as.data.frame(cbind(residuals=summary(RIPModel)@resid))

v = quantile(residData$residuals[!is.na(residData$residuals)], c(0.25, 0.75))
h = qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
int <- v[1L] - slope * h[1L]
ggplot(residData, aes(sample= residuals)) + 
	stat_qq(col='grey45') +
	geom_abline(slope = slope, intercept = int) + 
	ggtitle("Normal Q-Q Plot for Residuals") + 
	xlab("Theoretical Quantiles") + 
	ylab("Residual Quantiles") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))









## RANDOM SLOPES POISSON MODEL 	##
##	QUADTRATIC TERM INCLUDED		##
##	LINEAR INTERACTIONS WITH REGION INCLUDED	##


# INCLUDING A RANDOM SLOPE FOR THE LINEAR COMPONENT ONLY #
RSPModel = glmer(Decade_Count~Region+as.numeric(Decade)+Decade2+as.numeric(Decade)*Region+(1+as.numeric(Decade)|Site%in%Region),family=poisson,data=FireData_Aggregated)



# INCLUDING RANDOM SLOPES FOR BOTH LINEAR AND QUADRATIC COMPONENTS #
RSPModel = glmer(Decade_Count~Region+as.numeric(Decade)+Decade2+as.numeric(Decade)*Region+(1+as.numeric(Decade)+Decade2|Site%in%Region),family=poisson,data=FireData_Aggregated)


## MODEL SUMMARY ##

summary(summary(RSPModel)@resid)











## GAMLSSNP PREFERS NA'S REMOVED ##

FireData_NB = as.data.frame(na.omit(FireData_Aggregated))
FireData_NB$Decade2 = (as.numeric(FireData_NB$Decade)^2)




## RANDOM INTERCEPT NEGATIVE BINOMIAL MODEL ##
##	QUADTRATIC TERM INCLUDED		##

RINBModel = gamlssNP(Decade_Count~as.factor(Region)+as.numeric(Decade)+Decade2+as.numeric(Decade)*as.factor(Region),family=NBI,random=~1|Site,data=FireData_NB)


## MODEL SUMMARY ##

summary(RINBModel)







## RANDOM SLOPES NEGATIVE BINOMIAL MODEL ##
##	QUADTRATIC TERM INCLUDED		##


RSNBModel = gamlssNP(Decade_Count~as.factor(Region)+as.numeric(Decade)+Decade2,family=NBI,random=~1|Site+as.numeric(Decade)|Site,data=FireData_NB)



## MODEL SUMMARY ##

summary(RSNBModel)








## EFFECTS AND CONFIDENCE INTERVALS ##

(exp(summary(RIPModel)@coefs))



## PREDICTION: PREDICTED VALUES USING THE CURRENT SAMPLE ##

pred_mu = summary(RIPModel)@mu
summary(pred_mu)



## SCATTER PLOT OF PREDICTED VALUES VERSUS OBSERVED VALUES ##

qplot(FireData_Aggregated$Decade_Count,pred_mu)




## CONTINGENCY TABLE OF PREDICTED VALUES VERSUS REGION, TIME ##

predictedData = as.data.frame(cbind(predicted=pred_mu,Decade=FireData_Aggregated$Decade,Region=as.factor(FireData_Aggregated$Region)))

ptable = aggregate(predictedData$predicted~predictedData$Region+predictedData$Decade,FUN=function(x)mean(x,na.rm=TRUE))





## PLOT OF PREDICTED DECADE FIRE COUNTS BY TIME, REGION ##

ggplot(predictedData, aes(x=Decade, y=predicted, group=Region)) +
	geom_line(aes(group=Region,linetype=as.factor(Region))) +
	scale_linetype_manual(values=c("dotted","longdash","solid")) +
	annotate("text",x=1760,y=20,label="PNW") + 
	annotate("text",x=1625,y=16.75,label="SW") + 
	annotate("text",x=1575,y=14.35,label="IW") + 
	ggtitle("Predicted Decade Fire Counts by Region") + 
	xlab("Decade") + 
	ylab("Predicted Decade Fire Count") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"),legend.position="none")















