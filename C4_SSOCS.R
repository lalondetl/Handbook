# R ANALYSIS FOR CHAPTER 4: HETEROSCEDASTIC REGRESSION #
##	SCHOOL SURVEY ON CRIME AND SAFETY DATA		##




## LOAD RELEVANT PACKAGES ##

library(sas7bdat)
library(ggplot2)
library(plyr)
library(car)
library(nortest)




## SET THE WORKING DIRECTORY ##

setwd('PATH')




## READ IN THE DATA ##

SSOCS07 = read.sas7bdat('SSOCS07_Data.sas7bdat')






## RENAME VARIABLES USING MEANINGFUL LABELS ##

SSOCS07 = rename(SSOCS07,c("C0514"="suspensions","C0134"="uniforms","C0116"="metal_detectors","C0188"="tipline","C0178"="counseling","C0562"="crime","C0268"="discipline_training","C0276"="behavioral_training","C0508"="insubordination","C0510"="insubordination_removal","C0526"="percent_limited_English","C0532"="below_15th"))





## CREATE INDICATOR VARIABLES FOR CHARACTERISTICS OF INTEREST ##

SSOCS07$uniforms = ifelse((SSOCS07$uniforms==2),0,1)
SSOCS07$metal_detectors = ifelse((SSOCS07$metal_detectors==2),0,1)
SSOCS07$tipline = ifelse((SSOCS07$tipline==2),0,1)
SSOCS07$counseling = ifelse((SSOCS07$counseling==2),0,1)
SSOCS07$crime = 4 - SSOCS07$crime
SSOCS07$discipline_training = ifelse((SSOCS07$discipline_training==2),0,1)
SSOCS07$behavioral_training = ifelse((SSOCS07$behavioral_training==2),0,1)






## DESCRIPTIVES OF OUTCOME: SUSPENSIONS ##

summary(SSOCS07$suspensions)
var(SSOCS07$suspensions)



# VIEW SUSPENSIONS BOX PLOT WITHOUT UNUSUALLY LARGE VALUES #
qplot(SSOCS07[(SSOCS07$suspensions<=500),]$suspensions)







## HISTOGRAM OF SUSPENSIONS ##

ggplot(SSOCS07[(SSOCS07$suspensions<=500),], aes(SSOCS07[(SSOCS07$suspensions<=500),]$suspensions)) + 
	geom_histogram(aes(y=..density..),col='grey45') + 
	geom_density() +
	ggtitle("Histogram of Suspensions, Truncated at 500") + 
	xlab("Number of Suspensions") + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))






## BOX PLOT OF SUSPENSIONS ##

ggplot(SSOCS07[(SSOCS07$suspensions<=500),], aes(x=1,y=SSOCS07[(SSOCS07$suspensions<=500),]$suspensions)) + 
	geom_boxplot(col='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plot of Suspensions, Truncated at 500") + 
	xlab("Suspensions Box Plot") + 
	ylab("Number of Suspensions") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))







## SCATTER PLOT OF SUSPENSIONS VERSU INSUBORDINATION ##

ggplot(SSOCS07[(SSOCS07$suspensions<=500),], aes(x=SSOCS07[(SSOCS07$suspensions<=500),]$insubordination,y=SSOCS07[(SSOCS07$suspensions<=500),]$suspensions)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Suspensions vs Insubordinates") + 
	xlab("Insubordinate Students") + 
	ylab("Number of Suspensions") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))






## BOX PLOTS OF SUSPENSIONS BY CRIME LEVEL ##

ggplot(SSOCS07[(SSOCS07$suspensions<=500),], aes(x=as.factor(SSOCS07[(SSOCS07$suspensions<=500),]$crime),y=SSOCS07[(SSOCS07$suspensions<=500),]$suspensions)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plots of Suspensions by Crime") + 
	xlab("Crime") + scale_x_discrete(labels=c("Low","Moderate","High")) +
	ylab("Suspensions") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24),legend.position="none", panel.background = element_rect(fill = "grey92"))







## PLOTS OF SUSPENSIONS BY OTHER PREDICTORS ##

qplot(SSOCS07$percent_limited_English,SSOCS07$suspensions)

qplot(as.factor(SSOCS07$uniforms),SSOCS07$suspensions)
qplot(as.factor(SSOCS07$metal_detectors),SSOCS07$suspensions)
qplot(as.factor(SSOCS07$tipline),SSOCS07$suspensions)
qplot(as.factor(SSOCS07$counseling),SSOCS07$suspensions)
qplot(as.factor(SSOCS07$crime),SSOCS07$suspensions)
qplot(as.factor(SSOCS07$discipline_training),SSOCS07$suspensions)
qplot(as.factor(SSOCS07$behavioral_training),SSOCS07$suspensions)






## BOX PLOTS OF SUSPENSIONS VERSUS PREDICTORS, ELIMINATING UNUSUALLY LARGE VALUES ##

boxplotData = SSOCS07[(SSOCS07$suspensions<200),]

boxplot(boxplotData$suspensions~boxplotData$uniforms)
boxplot(boxplotData$suspensions~boxplotData$metal_detectors)
boxplot(boxplotData$suspensions~boxplotData$tipline)
boxplot(boxplotData$suspensions~boxplotData$counseling)
boxplot(boxplotData$suspensions~boxplotData$crime)
boxplot(boxplotData$suspensions~boxplotData$discipline_training)
boxplot(boxplotData$suspensions~boxplotData$behavioral_training)









## NORMAL LINEAR MODEL ##

NormalModel = lm(suspensions~uniforms+metal_detectors+tipline+counseling+as.factor(crime)+discipline_training+behavioral_training+insubordination+percent_limited_English+discipline_training*as.factor(crime)+behavioral_training*as.factor(crime),data=SSOCS07)




## CHECK FOR CONSTANT VARIANCE ##

# TEST #
ncvTest(NormalModel)



## SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES ##

ggplot(NormalModel, aes(x=NormalModel$fitted.values,y=NormalModel$residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Residuals vs Predicted Values") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))









## SQUARE ROOT TRANSFORMATION ##

sqrt_suspensions = sqrt(suspensions)




## TRY RESPONSE VARIABLE TRANSFORMATION: SQUARE ROOT OF SUSPENSIONS ##

SQRTModel = lm(sqrt_suspensions~uniforms+metal_detectors+tipline+counseling+as.factor(crime)+discipline_training+behavioral_training+insubordination+percent_limited_English+discipline_training*as.factor(crime)+behavioral_training*as.factor(crime),data=SSOCS07)




## CHECK FOR CONSTANT VARIANCE ##

# TEST #
ncvTest(SQRTModel)



## SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES ##

ggplot(SQRTModel, aes(x=SQRTModel$fitted.values,y=SQRTModel$residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Residual Scatter Plot, Square-Root Transformed") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))










## NATURAL LOGARITHM TRANSFORMATION ##

ln_suspensions = log(suspensions+0.01)




## TRY RESPONSE VARIABLE TRANSFORMATION: NATURAL LOGARITHM OF SUSPENSIONS ##

LNModel = lm(ln_suspensions~uniforms+metal_detectors+tipline+counseling+as.factor(crime)+discipline_training+behavioral_training+insubordination+percent_limited_English+discipline_training*as.factor(crime)+behavioral_training*as.factor(crime),data=SSOCS07)






## CHECK FOR CONSTANT VARIANCE ##

# TEST #
ncvTest(LNModel)




## SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES ##

ggplot(LNModel, aes(x=LNModel$fitted.values,y=LNModel$residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Residual Scatter Plot, Natural-Log Transformed") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))






## WEIGHTED LEAST SQUARES ESTIMATION ##





## WEIGHTED LEAST SQUARES: TRY PERCENT BELOW 15TH PERCENTILE ##



## SCATTER PLOT OF NORMAL MODEL RESIDUALS VERSUS PERCENT BELOW 15TH PERCENTILE ##

ggplot(SSOCS07, aes(x=below_15th,y=NormalModel$residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Normal Residuals vs Below 15th") + 
	xlab("Percent Below 15th Percentile") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))




## WEIGHTED LEAST SQUARES ESTIMATION: USING PERCENT BELOW 15TH PERCENTILE ##

WeightedModel1 = lm(suspensions~uniforms+metal_detectors+tipline+counseling+as.factor(crime)+discipline_training+behavioral_training+insubordination+percent_limited_English+discipline_training*as.factor(crime)+behavioral_training*as.factor(crime),weights=below_15th,data=SSOCS07)




## CHECK FOR CONSTANT VARIANCE ##

# TEST 3
ncvTest(WeightedModel1)



## SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES ##

WM1Data = as.data.frame(cbind(fitted=WeightedModel1$fitted.values,residuals=WeightedModel1$residuals))

ggplot(WM1Data, aes(x=fitted,y=residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Residual Scatter Plot, Weighted by Below 15th") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))









## WEIGHTED LEAST SQUARES: TRY INSUBORDINATES ##



## SCATTER PLOT OF NORMAL MODEL RESIDUALS VERSUS INSUBORDINATES ##

ggplot(SSOCS07, aes(x=insubordination,y=NormalModel$residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Residuals vs Insubordinates") + 
	xlab("Insubordinate Students") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))





## WEIGHTED LEAST SQUARES ESTIMATION: USING INVERSE OF INSUBORDINATES ##

WeightedModel2 = lm(suspensions~uniforms+metal_detectors+tipline+counseling+as.factor(crime)+discipline_training+behavioral_training+insubordination+percent_limited_English+discipline_training*as.factor(crime)+behavioral_training*as.factor(crime),weights=(1/((insubordination+0.01))),data=SSOCS07)




## CHECK FOR CONSTANT VARIANCE ##

# TEST #
ncvTest(WeightedModel2)




## SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES ##

WM2Data = as.data.frame(cbind(fitted=WeightedModel2$fitted.values,residuals=WeightedModel2$residuals))

ggplot(WM2Data, aes(x=fitted,y=residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Residual Scatter Plot, Weights 1/Insubordinates") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))









## COMBINED APPROACH: TRY NATURAL-LOG-TRANSFORMED OUTCOME, INSUBORDINATES AS WEIGHTS ##

ln_suspensions = log(suspensions+0.01)

WeightedLNModel = lm(ln_suspensions~uniforms+metal_detectors+tipline+counseling+as.factor(crime)+discipline_training+behavioral_training+insubordination+percent_limited_English+discipline_training*as.factor(crime)+behavioral_training*as.factor(crime),weights=(1/((insubordination+0.01))))




## CHECK FOR CONSTANT VARIANCE ##

# TEST #
ncvTest(WeightedLNModel)



## SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES ##

ggplot(WeightedLNModel, aes(x=WeightedLNModel$fitted.values,y=WeightedLNModel$residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Residual Scatter Plot, Transformed, Weighted") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))



## CHECK FOR NORMALITY OF RESIDUALS ##

qqnorm(WeightedLNModel$residuals)
qqline(WeightedLNModel$residuals)



## NORMAL PROBABILITY PLOT ##

v = quantile(WeightedLNModel$residuals[!is.na(WeightedLNModel$residuals)], c(0.25, 0.75))
h = qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
int <- v[1L] - slope * h[1L]
ggplot(WeightedLNModel, aes(sample=WeightedLNModel$residuals)) + 
	stat_qq(col='grey45') +
	geom_abline(slope = slope, intercept = int) + 
	ggtitle("Normal Q-Q Plot for Residuals") + 
	xlab("Theoretical Quantiles") + 
	ylab("Residual Quantiles") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))



## ANDERSON-DARLING TEST ##

ad.test(WeightedLNModel$residuals)


## MODEL SUMMARY ##

summary(WeightedLNModel)







## MODEL PREDICTION: NEW DATA ##

newPredictors = as.data.frame(expand.grid(insubordination=seq(0,500,50),uniforms=c(1), metal_detectors=c(0), tipline=c(0),counseling=c(1),crime=c(2),discipline_training=c(1),behavioral_training=c(1),percent_limited_English=c(mean(percent_limited_English,na.rm=TRUE))))

predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.lm(WeightedLNModel,newdata=newPredictors,type="response")))

predicted_values = as.data.frame(cbind(predicted_values,exp(predicted_values$predicted)))

predictions = predict(WeightedLNModel,newdata=newPredictors,type="response",se=TRUE)
newPredictors$pred.full = predictions$fit

newPredictors$ymin = newPredictors$pred.full - 2*predictions$se.fit
newPredictors$ymax = newPredictors$pred.full + 2*predictions$se.fit



## SCATTER PLOT OF PREDICTED VALUES WITH OBSERVED VALUES, VERSUS INSUBORDINATES ##

ggplot(data= SSOCS07,aes(x=insubordination,y=suspensions)) +
	xlim(0,500) +
	ylim(0,200) +
	geom_point(col='grey45') +
	geom_ribbon(data=newPredictors,aes(y=exp(pred.full),ymin=exp(ymin),ymax=exp(ymax)),alpha=0.25) +
	geom_line(data=newPredictors,aes(y=exp(pred.full)),col='grey45') +
	ggtitle("Predicted Suspensions Versus Insubordinates") + 
	xlab("Insubordinate Students") + 
	ylab("Predicted Suspensions") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))









