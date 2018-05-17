# R ANALYSIS FOR CHAPTER 5: LOGISTIC REGRESSION #
##	WIKIPEDIA CLICKSTREAM DATA		##





## LOAD RELEVANT PACKAGES ##

library(ggplot2)
library(plotROC)
library(ResourceSelection)
library(pROC)
library(pscl)




## SET THE WORKING DIRECTORY ##

setwd('PATH')





## READ IN 65476 ROWS FROM THE FILE ##
## PREDICTION FOR 3-HOUR BLOCKS ##

clickData = read.table("2015_02_clickstream.tsv",header=TRUE,sep="\t",na.strings="",fill=TRUE,quote="\"",nrows=65476)






## SIMPLIFY PREVIOUS TITLE ##

previous_title = rep("other",nrow(clickData))
for(i in 1:nrow(clickData))
{
	if(is.na(clickData$prev_title[i])){previous_title[i]=NA}
	else{
		if(clickData$prev_title[i] == "other-google"){previous_title[i]="Google"}
		if(clickData$prev_title[i] == "other-empty"){previous_title[i]="empty"}
		if(clickData$prev_title[i] == "other-wikipedia"){previous_title[i]="Wikipedia"}
		if(clickData$prev_title[i] == "other-bing"){previous_title[i]="Bing"}
		if(clickData$prev_title[i] == "other-yahoo"){previous_title[i]="Yahoo"}
		if(clickData$prev_title[i] == "Main_Page"){previous_title[i]="Main_Page"}
	}
}

clickData$previous_title = as.factor(previous_title)




## DEFINE BINARY REDLINK INDICATOR ##

redlink = ifelse(clickData$type=="redlink",1,0)
clickData$redlink = redlink






## DESCRIPTIVES: CONTINGENCY TABLE OF REDLINK BY PREVIOUS TITLE ##

table(clickData$redlink,clickData$previous_title)





## LOGISTIC HISTOGRAM PLOT ##

ggplot(clickData, aes(x = previous_title, y = redlink)) + 
	geom_bin2d(bins = 10) +
	scale_fill_gradientn(limits=c(0,40000), breaks=seq(0,40000, by=8000), colours=c('grey80','grey10')) + 
	ggtitle("Logistic Histogram of Redlink by Previous Titles") + 
	xlab("Previous Title") + 
	ylab("Redlink") +
	theme(axis.text=element_text(size=16), axis.text.x=element_text(angle=45), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))








## LOGISTIC REGRESSION MODEL FOR PROBABILITY OF A REDLINK ##

clickData$previous_title = relevel(clickData$previous_title,ref="other")

redlinkModel = glm(redlink~previous_title,weights=n,family=binomial,data=clickData)


## MODEL SUMMARY ##

summary(redlinkModel)






## HOSMER-LEMESHOW TEST ##

hoslem.test(redlinkModel$y,redlinkModel$fitted.values)





## ROC CURVE AND AREA ##

roc(redlinkModel$y~redlinkModel$fitted.values,plot=TRUE)



## ROC CURVE: TRUE POSITIVE VERSUS FALSE POSITIVE ##

ggplot(redlinkModel,aes(d=redlinkModel$y, m=redlinkModel$fitted.values)) +
	geom_roc() + 
	style_roc() +
	ggtitle("ROC Curve for Logistic Model") + 
	xlab("False Positive") + 
	ylab("True Positive") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))





## STANDARDIZED DEVIANCE RESIDUALS ##

fivenum(summary(redlinkModel)$deviance.resid)




## SCATTER PLOT OF DEVIANCE RESIDUALS VERSUS FITTED VALUES ##

qplot(redlinkModel$fitted.values,summary(redlinkModel)$deviance.resid)




## SCATTER PLOT OF DEVIANCE RESIDUALS VERSUS PREVIOUS TITLE ##

ggplot(redlinkModel, aes(x=clickData[complete.cases(clickData[,c("redlink","previous_title")]),]$previous_title,y=summary(redlinkModel)$deviance.resid)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Residuals Versus Previous Title") + 
	xlab("Previous Title") + 
	ylab("Deviance Residuals") +
	theme(axis.text=element_text(size=16), axis.text.x=element_text(angle=45), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))






## ODDS RATIOS AND CONFIDENCE INTERVALS ##

(exp(redlinkModel$coefficients))

(exp(redlinkModel$coefficients+qnorm(0.025)*sqrt(diag(summary(redlinkModel)$cov.scaled))))
(exp(redlinkModel$coefficients+qnorm(0.975)*sqrt(diag(summary(redlinkModel)$cov.scaled))))






## PREDICTION: PREDICTED VALUES USING THE CURRENT SAMPLE ##

summary(redlinkModel$fitted.values)






## PREDICTION: PREDICTED VALUES USING NEW DATA ##

newPredictors = as.data.frame(expand.grid(previous_title=c("other","Bing","empty","Google","Main_Page","Wikipedia","Yahoo")))

predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.glm(redlinkModel,newdata=newPredictors,type="response")))






