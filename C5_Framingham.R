# R ANALYSIS FOR CHAPTER 5: LOGISTIC REGRESSION #
##	FRAMINGHAM HEART STUDY DATA		##






## LOAD RELEVANT PACKAGES ##

library(ggplot2)
library(plotROC)
library(ResourceSelection)
library(pROC)
library(pscl)




## SET THE WORKING DIRECTORY ##

setwd('PATH')




## READ IN THE DATA ##

FData = read.csv('frmgham2.csv',header=TRUE)






## CONSIDER BASELINE OBSERVATIONS ONLY ##

FData_Baseline = FData[FData$PERIOD==1,]



## FILTER OUT INDIVIDUALS WITH PREVALENCE OF HYPERTENSION AT BASELINE ##

FData_Baseline_NOHYP = FData_Baseline[FData_Baseline$PREVHYP==0,]







## DESCRIPTIVES: TABLES OF PROPORTIONS ##

summary(FData_Baseline_NOHYP$HYPERTEN)
table(FData_Baseline_NOHYP$HYPERTEN)






## CONTINGENCY TABLES OF HYPERTENSION VERSUS PREDICTORS ##

table(FData_Baseline_NOHYP$HYPERTEN,FData_Baseline_NOHYP$DIABETES)
table(FData_Baseline_NOHYP$HYPERTEN,FData_Baseline_NOHYP$SEX)






## LOGISTIC HISTOGRAM PLOT OF HYPERTENSION VERSUS AGE ##

ggplot(FData_Baseline_NOHYP, aes(x=AGE, y=HYPERTEN)) + 
	geom_bin2d(bins = 10) +
	scale_fill_gradientn(limits=c(0,350), breaks=seq(0,350, by=50), colours=c('grey80','grey10')) + 
	geom_smooth(method = "glm",method.args = list(family = "binomial"),col='grey45') +
	ggtitle("Logistic Histogram of Hypertensive vs Age") + 
	xlab("Age") + 
	ylab("Hypertensive") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))






## LOGISTIC HISTOGRAM PLOT OF HYPERTENSION VERSUS CHOLESTEROL ##

colours=c('grey41','grey42','grey43','grey44','grey45','grey46','grey47','grey48')

ggplot(FData_Baseline_NOHYP, aes(x=TOTCHOL, y=HYPERTEN)) + 
	geom_bin2d(bins = 10) +
	scale_fill_gradientn(limits=c(0,350), breaks=seq(0,350, by=50), colours=c('grey80','grey10')) + 
	geom_smooth(method = "glm",method.args = list(family = "binomial"),col='grey45') +
	ggtitle("Logistic Hist of Hypertensive vs Cholesterol") + 
	xlab("Total Cholesterol") + 
	ylab("Hypertensive") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))






## EXPLORATORY PLOTS OF PREDICTORS VERSUS EACH OTHER ##

qplot(FData_Baseline_NOHYP$TOTCHOL,FData_Baseline_NOHYP$CIGPDAY,color=factor(FData_Baseline_NOHYP$SEX))

qplot(FData_Baseline_NOHYP$AGE,FData_Baseline_NOHYP$SEX)
qplot(FData_Baseline_NOHYP$CIGPDAY,FData_Baseline_NOHYP$SEX)
qplot(FData_Baseline_NOHYP$TOTCHOL,FData_Baseline_NOHYP$SEX)

qplot(FData_Baseline_NOHYP$AGE,FData_Baseline_NOHYP$CIGPDAY)
qplot(FData_Baseline_NOHYP$AGE,FData_Baseline_NOHYP$TOTCHOL)
qplot(FData_Baseline_NOHYP$CIGPDAY,FData_Baseline_NOHYP$TOTCHOL)




## EXPLORATORY PLOTS OF PREDICTORS, BY HYPERTENSION ##

qplot(as.factor(FData_Baseline_NOHYP$DIABETES),fill=as.factor(FData_Baseline_NOHYP$HYPERTEN),geom='bar')
qplot(as.factor(FData_Baseline_NOHYP$SEX),fill=as.factor(FData_Baseline_NOHYP$HYPERTEN),geom='bar')







## LOGISTIC REGRESSION MODEL FOR HYPERTEN ##
LRModel = glm(HYPERTEN~DIABETES+as.factor(SEX)+AGE+CIGPDAY+TOTCHOL+as.factor(SEX)*DIABETES+as.factor(SEX)*CIGPDAY,family=binomial,data=FData_Baseline_NOHYP)



## MODEL SUMMARY ##

summary(LRModel)






## HOSMER-LEMESHOW TEST ##

hoslem.test(LRModel$y,LRModel$fitted.values)






## ROC CURVE AND AREA ##

roc(LRModel$y~LRModel$fitted.values,plot=TRUE)




## ROC CURVE: TRUE POSITIVE VERSUS FALSE POSITIVE ##

ggplot(LRModel,aes(d=LRModel$y, m=LRModel$fitted.values)) +
	geom_roc() + 
	style_roc() +
	ggtitle("ROC Curve for Logistic Prediction") + 
	xlab("False Positive") + 
	ylab("Tue Positive") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))








## STANDARDIZED DEVIANCE RESIDUALS ##

fivenum(summary(LRModel)$deviance.resid)





## SCATTER PLOT OF DEVIANCE RESIDUALS VERSUS AGE ##

qplot(AGE,summary(LRModel)$deviance.resid)




## SCATTER PLOT OF DEVIANCE RESIDUALS VERSUS FITTED VALUES ##

ggplot(LRModel, aes(x=LRModel$fitted.values,y=summary(LRModel)$deviance.resid)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Residuals Versus Predicted Values") + 
	xlab("Predicted Values") + 
	ylab("Deviance Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))







## ODDS RATIOS AND CONFIDENCE INTERVALS ##

(exp(LRModel$coefficients))

(OddsRatios = exp(cbind(OddsRatios=coef(LRModel),confint(LRModel))))





## PREDICTION: PREDICTED VALUES USING THE CURRENT SAMPLE ##

summary(LRModel$fitted.values)





## PREDICTION: PREDICTED VALUES USING NEW DATA ##

newPredictors = as.data.frame(expand.grid(AGE=seq(30,70,5),DIABETES=c(0,1),SEX=c(1,2),CIGPDAY=c(mean(CIGPDAY,na.rm=TRUE)),TOTCHOL=c(mean(TOTCHOL,na.rm=TRUE))))

predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.glm(LRModel,newdata=newPredictors,type="response")))

newPredictors = as.data.frame(expand.grid(AGE=seq(40,70,5),DIABETES=c(0,1),SEX=c(1,2),CIGPDAY=c(mean(CIGPDAY,na.rm=TRUE)),TOTCHOL=c(mean(TOTCHOL,na.rm=TRUE))))
predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.glm(LRModel,newdata=newPredictors,type="response")))



## STANDARD ERRORS FOR PREDICTED VALUES ##

predictions = predict(LRModel,newdata=newPredictors,type="response",se=TRUE)
newPredictors$pred.full = predictions$fit

newPredictors$ymin = newPredictors$pred.full - 2*predictions$se.fit
newPredictors$ymax = newPredictors$pred.full + 2*predictions$se.fit




## RE-DEFINE DIABETES AND SEX FACTORS ##

newPredictors$diabsex = rep("n",length(newPredictors$SEX))
for(i in 1:length(newPredictors$SEX))
{
	if((newPredictors$SEX[i]==1) & (newPredictors$DIABETES[i]==0)){newPredictors$diabsex[i]="Male,Non-Diabetic"}
	if((newPredictors$SEX[i]==1) & (newPredictors$DIABETES[i]==1)){newPredictors$diabsex[i]="Male,Diabetic"}
	if((newPredictors$SEX[i]==2) & (newPredictors$DIABETES[i]==0)){newPredictors$diabsex[i]="Female,Non-Diabetic"}
	if((newPredictors$SEX[i]==2) & (newPredictors$DIABETES[i]==1)){newPredictors$diabsex[i]="Female,Diabetic"}
}

newPredictors$diabsex_factor = factor(newPredictors$diabsex,levels=c("Male,Non-Diabetic","Female,Non-Diabetic","Male,Diabetic","Female,Diabetic"))





## SCATTER PLOT OF HYPERTENSION VERSUS AGE, WITH PREDICTED VALUES, BY DIABETES AND SEX ##

ggplot(data=FData_Baseline_NOHYP,aes(x=AGE,y=HYPERTEN,group=diabsex_factor)) +
	facet_grid(~diabsex_factor) +
	geom_bin2d(bins = 10) +
	scale_fill_gradientn(limits=c(0,350), breaks=seq(0,350, by=50), colours=c('grey80','grey10')) + 
	geom_ribbon(data=newPredictors,aes(y=pred.full,ymin=ymin,ymax=ymax),alpha=0.25,col='grey45') +
	geom_line(data=newPredictors,aes(y=pred.full),col='grey45') +
	ggtitle("Predicted Probabilities by Diabetes and Sex") + 
	xlab("Age") + 
	ylab("Predicted Probability of Hypertension") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))





