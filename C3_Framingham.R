# R ANALYSIS FOR CHAPTER 3: NORMAL LINEAR REGRESSION ANALYSIS #
##	FRAMINGHAM HEART STUDY DATA		##



## LOAD RELEVANT PACKAGES ##

library(ggplot2)
library(car)
library(nortest)



## SET THE WORKING DIRECTORY ##

setwd('PATH')



## READ IN THE DATA ##
FData = read.csv('frmgham2.csv',header=TRUE)






## NORMAL LINEAR REGRESSION ##

normalModel = lm(HYPERTEN~TOTCHOL+AGE+CIGPDAY+as.factor(SEX)+as.factor(DIABETES)+as.factor(SEX)*as.factor(DIABETES)+as.factor(SEX)*CIGPDAY,data=FData)




## CHECK FOR CONSTANT VARIANCE ##

# TEST #
ncvTest(normalModel)

# SCATTER PLOTS #
qplot(normalModel$fitted.values,normalModel$residuals)
qplot(normalModel$model$TOTCHOL,normalModel$residuals)
qplot(normalModel$model$AGE,normalModel$residuals)


## SCATTER PLOT OF RESIDUALS VERSUS AGE ##

ggplot(normalModel, aes(x=normalModel$model$AGE,y=normalModel$residuals)) +
	geom_point(col='grey45') + 
	ggtitle("Scatter Plot of Normal Model Residuals vs Age") + 
	xlab("Age") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))




## CHECK FOR RESIDUAL NORMALITY ##

# ABDERSON-DARLING TEST #
ad.test(normalModel$residuals)


# NORMAL PROBABILITY PLOT #
qqnorm(normalModel$residuals)
qqline(normalModel$residuals)

v = quantile(normalModel$residuals[!is.na(normalModel$residuals)], c(0.25, 0.75))
h = qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
int <- v[1L] - slope * h[1L]
ggplot(normalModel, aes(sample=normalModel$residuals)) + 
	stat_qq(col='grey45') +
	geom_abline(slope = slope, intercept = int) + 
	ggtitle("Normal Q-Q Plot for Normal Model Residuals") + 
	xlab("Theoretical Quantiles") + 
	ylab("Residual Quantiles") + 
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))





## THE NORMAL LINEAR MODEL HAS POOR PREDICTION ##

# SUMMARIZE FITTED VALUES #
summary(normalModel$fitted.values)
invalid_predictions = normalModel$fitted.values[which(normalModel$fitted.values>1)]
summary(invalid_predictions)
length(invalid_predictions)

# PREDICT NEW VALUES #
newPredictors = as.data.frame(expand.grid(AGE=c(35),DIABETES=c(0),SEX=c(1),CIGPDAY=c(0),TOTCHOL=c(110)))
predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.lm(normalModel,newdata=newPredictors,type="response")))

# UNREASONABLE PREDICTED VALUES #
newPredictors = as.data.frame(expand.grid(AGE=c(80),DIABETES=c(1),SEX=c(2),CIGPDAY=c(80),TOTCHOL=c(650)))
predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.lm(normalModel,newdata=newPredictors,type="response")))

