# R ANALYSIS FOR CHAPTER 8: MARGINAL LONGITUDINAL REGRESSION #
##	FRAMINGHAM HEART STUDY DATA		##




## LOAD RELEVANT PACKAGES ##

library(ggplot2)
library(geepack)
library(s20x)
library(dafs)




## SET THE WORKING DIRECTORY ##

setwd('PATH')




## READ IN THE DATA ##

FData = read.csv('frmgham2.csv',header=TRUE)



## REMOVE RELEVANT NA'S ##

FData = FData[complete.cases(FData[,c("HYPERTEN","DIABETES","SEX","AGE","CIGPDAY","TOTCHOL")]),]






## CONTINGENCY TABLES OF HYPERTENSION VERSUS PREDICTORS ##

table(FData$PERIOD,FData$PREVHYP)

xtabs(FData$PREVHYP~FData$PERIOD+FData$SEX)
xtabs(1-FData$PREVHYP~FData$PERIOD+FData$SEX)

xtabs(FData$PREVHYP~FData$PERIOD+FData$DIABETES)
xtabs(1-FData$PREVHYP~FData$PERIOD+FData$DIABETES)






## STACKED BAR PLOT OF HYPERTENSION BY TIME, SEX ##

ggplot(FData, aes(as.factor(PERIOD), fill=as.factor(PREVHYP))) + 
	geom_bar() +
	scale_fill_grey() + 
	facet_wrap(~SEX) +
	ggtitle("Stacked Barplot of Hypertensive by Time, Sex") + 
	xlab("Time Period, for Males and Females") + 
	ylab("Frequency by Hypertension") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"),legend.position="none")




## BAR PLOTS OF DIABETES BY TIME, FILLED BY HYPERTENSION ##

plotDataDIABETES = as.data.frame(xtabs(~PREVHYP+DIABETES+PERIOD))
ggplot(plotDataDIABETES, aes(x=PERIOD, y=Freq, fill=PREVHYP)) + 
	geom_bar(position="stack", stat="identity") +
	facet_wrap(~DIABETES)




## STACKED BAR PLOT OF HYPERTENSION BY TIME, DIABETES ##

ggplot(FData, aes(as.factor(PERIOD), fill=as.factor(PREVHYP))) + 
	geom_bar() +
	scale_fill_grey() + 
	facet_wrap(~DIABETES) +
	ggtitle("Stacked Barplot, Hypertensive, Time, Diabetes") + 
	xlab("Time Period, for Non-Diabetics and Diabetics") + 
	ylab("Frequency by Hypertension") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"),legend.position="none")





## LOGISTIC HISTOGRAM PLOT OF HYPERTENSION VERSUS AGE ##

ggplot(FData, aes(x = AGE, y = PREVHYP)) + 
	geom_bin2d(bins = 10) +
	scale_fill_gradientn(limits=c(0,750), breaks=seq(0,750, by=150), colours=c('grey80','grey10')) + 
	geom_smooth(method = "glm",method.args = list(family = "binomial"),col='grey45') +
	facet_wrap(~PERIOD) +
	ggtitle("Logistic Histogram of Hypertensive vs Age") + 
	xlab("Age") + 
	ylab("Hypertensive") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))





## LOGISTIC HISTOGRAM PLOT OF HYPERTENSION VERSUS CIGARETTES PER DAY ##

ggplot(FData, aes(x = CIGPDAY, y = PREVHYP)) + 
	geom_bin2d(bins = 10) +
	scale_fill_gradientn(limits=c(0,750), breaks=seq(0,750, by=150), colours=c('grey80','grey10')) + 
	geom_smooth(method = "glm",method.args = list(family = "binomial"),col='grey45') +
	facet_wrap(~PERIOD) +
	ggtitle("Logistic Histogram of Hypertensive vs Cigarettes") + 
	xlab("Cigarettes per Day") + 
	ylab("Hypertensive") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))





## LOGISTIC HISTOGRAM PLOT OF HYPERTENSION VERSUS CHOLESTEROL ##

ggplot(FData, aes(x = TOTCHOL, y = PREVHYP)) + 
	geom_bin2d(bins = 10) +
	scale_fill_gradientn(limits=c(0,750), breaks=seq(0,750, by=150), colours=c('grey80','grey10')) + 
	geom_smooth(method = "glm",method.args = list(family = "binomial"),col='grey45') +
	facet_wrap(~PERIOD) +
	ggtitle("Logistic Histogram of Hypertensive vs Cholesterol") + 
	xlab("Cholesterol") + 
	ylab("Hypertensive") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))








## PRODUCE RESIDUALS FROM NON-CORRELATED MODEL ##

residuals = glm(PREVHYP~DIABETES+as.factor(SEX)+AGE+CIGPDAY+TOTCHOL+as.factor(SEX)*DIABETES+as.factor(SEX)*CIGPDAY,family=binomial,data=FData)$residuals



## RESIDUAL SCATTERPLOT MATRIX ##

residualData = as.data.frame(cbind(residuals,FData$PERIOD,FData$RANDID))
rDataWide = reshape(residualData,timevar="PERIOD",idvar="RANDID",direction="wide")

rDataOrdered = rDataWide[,c("residuals.1","residuals.2","residuals.3")]
colnames(rDataOrdered) = c("Period1","Period2","Period3")

rDataOrdered = rDataOrdered[complete.cases(rDataOrdered[,c("Period1","Period2","Period3")]),]

pairsDAFS(rDataOrdered,main="Histograms, Scatter Plots, and Pairwise Correlations of Residuals")




## RESIDUAL CORRELATION MATRIX ##

cor(rDataWide[,-1],use="complete")






## VARIOGRAM ##

## SUB-SAMPLE DUE TO FHS SIZE ##

set.seed(12345)
VGData = FData[sample(1:nrow(FData),1000,replace=FALSE),]
VGresid = glm(VGData$PREVHYP~VGData$DIABETES+as.factor(VGData$SEX)+VGData$AGE+VGData$CIGPDAY+VGData$TOTCHOL+as.factor(VGData$SEX)*VGData$DIABETES+as.factor(VGData$SEX)*VGData$CIGPDAY,family=binomial)$residuals

VG = variogram(VGData$RANDID,VGData$PERIOD,VGresid)

plot.vargm(VG)










## MARGINAL ANALYSIS: GEE ##

# SORT FOR GEEGLM #
FData = FData[order(FData$RANDID),]



## INDEPENDENT WORKING CORRELATION STRUCTURE ##

GEEFHSModel_IND = geeglm(PREVHYP~DIABETES+as.factor(SEX)+AGE+CIGPDAY+TOTCHOL+as.factor(SEX)*DIABETES+as.factor(SEX)*CIGPDAY+as.factor(PERIOD),data=FData,id=RANDID,family=binomial,corstr="independence",data=FData)
summary(GEEFHSModel_IND)




## EXCHANGEABLE WORKING CORRELATION STRUCTURE ##

GEEFHSModel_EXCH = geeglm(PREVHYP~DIABETES+as.factor(SEX)+AGE+CIGPDAY+TOTCHOL+as.factor(SEX)*DIABETES+as.factor(SEX)*CIGPDAY+as.factor(PERIOD),data=FData,id=RANDID,family=binomial,corstr="exchangeable",data=FData)
summary(GEEFHSModel_EXCH)




## AUTO-REGRESSIVE WORKING CORRELATION STRUCTURE ##

GEEFHSModel_AR1 = geeglm(PREVHYP~DIABETES+as.factor(SEX)+AGE+CIGPDAY+TOTCHOL+as.factor(SEX)*DIABETES+as.factor(SEX)*CIGPDAY+as.factor(PERIOD),data=FData,id=RANDID,family=binomial,corstr="ar1",data=FData)
summary(GEEFHSModel_AR1)





## UNSTRUCTURED WORKING CORRELATION STRUCTURE ##

GEEFHSModel_UN = geeglm(PREVHYP~DIABETES+as.factor(SEX)+AGE+CIGPDAY+TOTCHOL+as.factor(SEX)*DIABETES+as.factor(SEX)*CIGPDAY+as.factor(PERIOD),data=FData,id=RANDID,family=binomial,corstr="unstructured",data=FData)
summary(GEEFHSModel_UN)





## QIC VALUES ##

QIC(GEEFHSModel_IND)
QIC(GEEFHSModel_EXCH)
QIC(GEEFHSModel_AR1)
QIC(GEEFHSModel_UN)





## GEE MODEL RESIDUALS ##

summary(summary(GEEFHSModel_IND)$deviance.resid)
summary(summary(GEEFHSModel_EXCH)$deviance.resid)
summary(summary(GEEFHSModel_AR1)$deviance.resid)
summary(summary(GEEFHSModel_UN)$deviance.resid)



## SCATTER PLOTS OF RESIDUALS VERSUS FITTED VALUES ##

qplot(GEEFHSModel_IND$fitted.values,summary(GEEFHSModel_IND)$deviance.resid)
qplot(GEEFHSModel_EXCH$fitted.values,summary(GEEFHSModel_EXCH)$deviance.resid)
qplot(GEEFHSModel_AR1$fitted.values,summary(GEEFHSModel_AR1)$deviance.resid)
qplot(GEEFHSModel_UN$fitted.values,summary(GEEFHSModel_UN)$deviance.resid)





## NORMAL PROBABILITY PLOTS AND NORMALITY TESTS FOR RESIDUALS ##

# INDEPENDENT WCS #
qqnorm(summary(GEEFHSModel_IND)$deviance.resid)
qqline(summary(GEEFHSModel_IND)$deviance.resid)

ad.test(summary(GEEFHSModel_IND)$deviance.resid)
lillie.test(summary(GEEFHSModel_IND)$deviance.resid)


# EXCHANGEABLE WCS #
qqnorm(summary(GEEFHSModel_EXCH)$deviance.resid)
qqline(summary(GEEFHSModel_EXCH)$deviance.resid)

ad.test(summary(GEEFHSModel_EXCH)$deviance.resid)
lillie.test(summary(GEEFHSModel_EXCH)$deviance.resid)



# AUTO-REGRESSIVE (1) WCS #
qqnorm(summary(GEEFHSModel_AR1)$deviance.resid)
qqline(summary(GEEFHSModel_AR1)$deviance.resid)

ad.test(summary(GEEFHSModel_AR1)$deviance.resid)
lillie.test(summary(GEEFHSModel_AR1)$deviance.resid)



# UNSTRUCTURED WCS #
qqnorm(summary(GEEFHSModel_UN)$deviance.resid)
qqline(summary(GEEFHSModel_UN)$deviance.resid)

ad.test(summary(GEEFHSModel_UN)$deviance.resid)
lillie.test(summary(GEEFHSModel_UN)$deviance.resid)





## AR(1) MODEL RESULTS ##

summary(GEEFHSModel_AR1)




## ODDS RATIOS AND CONFIDENCE INTERVALS ##

(exp(GEEFHSModel_AR1$coefficients))

GEETable = coef(summary(GEEFHSModel_AR1))
GEETable_CI <- with(as.data.frame(GEETable),
     cbind(lwr=Estimate-qnorm(0.975)*Std.err,
           upr=Estimate+qnorm(0.975)*Std.err))
rownames(GEETable_CI) <- rownames(GEETable)

(OddsRatios = exp(cbind(OddsRatios=coef(GEEFHSModel_AR1),GEETable_CI)))






## PREDICTION: PREDICTED VALUES USING THE CURRENT SAMPLE ##

summary(GEEFHSModel_AR1$fitted.values)







## PREDICTION: PREDICTED VALUES USING NEW DATA ##


# AGES: 30 TO 70 #
newPredictors = as.data.frame(expand.grid(AGE=seq(30,70,5),DIABETES=c(0),SEX=c(1,2),PERIOD=c(1,2,3),CIGPDAY=c(mean(CIGPDAY,na.rm=TRUE)),TOTCHOL=c(mean(TOTCHOL,na.rm=TRUE))))
predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.glm(GEEFHSModel_AR1,newdata=newPredictors,type="response")))


# AGES: 40 TO 70 #
newPredictors = as.data.frame(expand.grid(AGE=seq(40,70,5),DIABETES=c(0),SEX=c(1,2),PERIOD=c(1,2,3),CIGPDAY=c(mean(CIGPDAY,na.rm=TRUE)),TOTCHOL=c(mean(TOTCHOL,na.rm=TRUE))))
predicted_values = as.data.frame(cbind(newPredictors,predicted=predict.glm(GEEFHSModel_AR1,newdata=newPredictors,type="response")))





## PREDICTED PROBABILITIES OF HYPERTENSION BY AGE, STRATIFIED BY TIME ##

ggplot(data= FData,aes(x=AGE,y=PREVHYP)) +
	facet_wrap(~PERIOD) +
	geom_point(col='grey45') +
	geom_line(data=predicted_values,aes(y=predicted),color='grey45') +
	ggtitle("Predicted Probability Hypertensive by Age, Time") + 
	xlab("Age") + 
	ylab("Predicted Probability of Hypertension") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))




