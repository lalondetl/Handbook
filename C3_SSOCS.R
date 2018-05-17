# R ANALYSIS FOR CHAPTER 3: NORMAL LINEAR REGRESSION ANALYSIS #
##	SCHOOL SURVEY ON CRIME AND SAFETY DATA		##




## LOAD RELEVANT PACKAGES ##

library(sas7bdat)
library(plyr)
library(ggplot2)
library(car)
library(nortest)



## SET THE WORKING DIRECTORY ##

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






## NORMAL LINEAR REGRESSION ##

normalModel = lm(suspensions~uniforms+metal_detectors+tipline+counseling+as.factor(crime)+discipline_training+behavioral_training+insubordination+percent_limited_English+discipline_training*as.factor(crime)+behavioral_training*as.factor(crime),data=SSOCS07)




## CHECK FOR CONSTANT VARIANCE ##

# TEST #
ncvTest(normalModel)


# RESIDUAL SCATTER PLOT #
ggplot(normalModel, aes(x=normalModel$fitted.values,y=normalModel$residuals)) +
	geom_point(col='grey45') + 
	ggtitle("Scatter Plot of Residuals vs Predicted Values") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))




## CHECK FOR RESIDUAL NORMALITY ##

# ANDERSON-DARLING TEST #
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




## THE LINEAR MODEL HAS POOR PREDICTION ##

# SUMMARY OF FITTED VALUES #
summary(normalModel$fitted.values)
invalid_predictions = normalModel$fitted.values[which(normalModel$fitted.values<0)]
summary(invalid_predictions)
length(invalid_predictions)

# UNREASONABLE PREDICTIONS #
newPredictors = as.data.frame(expand.grid(uniforms=c(1),metal_detectors=c(0),tipline=c(0),counseling=c(0),crime=c(1),discipline_training=c(1),behavioral_training=c(0),insubordination=c(5),percent_limited_English=c(50)))
predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.lm(normalModel,newdata=newPredictors,type="response")))






