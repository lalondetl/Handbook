# R ANALYSIS FOR CHAPTER 4: HETEROSCEDASTIC REGRESSION #
##	FIRE-CLIMATE INTERACTION DATA		##




## LOAD RELEVANT PACKAGES ##

library(gdata)
library(ggplot2)
library(plyr)
library(car)





## SET THE WORKING DIRECTORY ##

setwd('/Users/trent.lalonde/Documents/Research/Textbooks/Textbook - Handbook/Data/Fire Data/') 





## READ IN THE DATA ##

FireData_Aggregated = read.csv("FireData_Aggregated.csv",header=TRUE)






## DESCRIPTIVES OF THE RESPONSE ##

summary(FireData_Aggregated$Decade_Mean)
var(FireData_Aggregated$Decade_Mean)

qplot(FireData_Aggregated$Decade_Mean)



## DECADE MEAN HISTOGRAM ##

ggplot(FireData_Aggregated, aes(Decade_Mean)) + 
	geom_histogram(aes(y=..density..),col='grey45') + 
	geom_density() +
	ggtitle("Histogram of Decade Fire Means") + 
	xlab("Decade Fire Indicator Mean") + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))




## DECADE MEAN BOX PLOT ##

ggplot(FireData_Aggregated, aes(x=1,y=Decade_Mean)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plot of Decade Fire Means") + 
	xlab("Decade Fire Mean Box Plot") + 
	ylab("Decade Fire Mean") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"),legend.position="none")





## DECADE MEAN VERSUS DECADE ##

ggplot(FireData_Aggregated, aes(x=Decade,y=Decade_Mean)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Decade Fire Mean Versus Decade") + 
	xlab("Decade") + 
	ylab("Decade Fire Mean ") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))






## DECADE MEAN BOX PLOTS BY REGION ##

ggplot(FireData_Aggregated, aes(x=as.factor(Region),y=Decade_Mean)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plots of Decade Fire Mean by Region") + 
	xlab("Region") + 
	ylab("Decade Fire Mean") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"),legend.position="none")






## DEFINE DECADE-SQUARED FOR MODEL ##
FireData_Aggregated$Decade2 = (FireData_Aggregated$Decade)^2


## NORMAL LINEAR MODEL ##

NormalModel = lm(Decade_Mean~as.factor(Region)+Decade+Decade2,data=FireData_Aggregated)



## CHECK FOR CONSTANT VARIANCE ##

# TEST #
ncvTest(NormalModel)


# SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES #

ggplot(NormalModel, aes(x=NormalModel$fitted.values,y=NormalModel$residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Residuals Versus Predicted Values") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))




# SCATTER PLOT OF RESIDUALS VERSUS DECADE #

ggplot(NormalModel, aes(x=Decade,y=NormalModel$residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Residuals Versus Decade") + 
	xlab("Decade") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))







## WEIGHTED LEAST SQUARES ESTIMATION ##

## EVALUATE RELATIONSHIP BETWEEN RESIDUALS AND DECADE VARIANCE ##

qplot(Decade_Var,NormalModel$residuals)





## USE TRANSFORMATION OF DECADE VARIATION AS WEIGHTS ##

WeightedModel1 = lm(Decade_Mean~as.factor(Region)+Decade+Decade2,weights=(1/(Decade_Var+0.01)^2),data=FireData_Aggregated)





## CHECK FOR CONSTANT VARIANCE ##

# TEST #
ncvTest(WeightedModel1)


# SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES #

ggplot(WeightedModel1, aes(x=WeightedModel1$fitted.values,y=WeightedModel1$residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Weighted Residuals Versus Predicted Values") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))





## MODEL SUMMARY ##

summary(WeightedModel1)





## EVALUATE PREDICTION USING NEW DATA ##



## PREDICTIONS USING WEIGHTED LEAST SQUARES ##

# AVERAGE DECADE VALUE #
newPredictors = as.data.frame(expand.grid(Region=c("PNW","IW","SW"),Decade=mean(Decade,na.rm=TRUE),Decade2=(mean(Decade,na.rm=TRUE))^2))
predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.lm(WeightedModel1,newdata=newPredictors,type="response")))



# DECADE = 1750 #
newPredictors = as.data.frame(expand.grid(Region=c("PNW","IW","SW"),Decade=1750,Decade2=(1750)^2))
predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.lm(WeightedModel1,newdata=newPredictors,type="response")))





## PREDICTIONS USING NORMAL ORDINARY LEAST SQUARES ##

# AVERAGE DECADE VALUE #
newPredictors = as.data.frame(expand.grid(Region=c("PNW","IW","SW"),Decade=mean(Decade,na.rm=TRUE),Decade2=(mean(Decade,na.rm=TRUE))^2))
predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.lm(NormalModel,newdata=newPredictors,type="response")))


# DECADE = 1750 #
newPredictors = as.data.frame(expand.grid(Region=c("PNW","IW","SW"),Decade=1750,Decade2=(1750)^2))
predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.lm(NormalModel,newdata=newPredictors,type="response")))





