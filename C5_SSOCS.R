# R ANALYSIS FOR CHAPTER 5: ORDINAL MULTINOMIAL LOGISTIC REGRESSION #
##	SCHOOL SURVEY ON CRIME AND SAFETY DATA		##





## LOAD RELEVANT PACKAGES ##

library(sas7bdat)
library(ggplot2)
library(plyr)
library(ResourceSelection)
library(pROC)
library(pscl)
library(VGAM)




## SET THE WORKING DIRECTORY ##

setwd('/Users/trent.lalonde/Documents/Research/Textbooks/Textbook - Handbook/Data/SSOCS Data/2007/')




## READ IN THE DATA ##

SSOCS07 = read.sas7bdat('SSOCS07_Data.sas7bdat')





## RENAME VARIABLES USING MEANINGFUL LABELS ##

SSOCS07 = rename(SSOCS07,c("C0376"="bullying","C0514"="suspensions","C0134"="uniforms","C0116"="metal_detectors","C0188"="tipline","C0178"="counseling","C0562"="crime"))




## CREATE INDICATOR VARIABLES FOR CHARACTERISTICS OF INTEREST ##

SSOCS07$uniforms = ifelse((SSOCS07$uniforms==2),0,1)
SSOCS07$metal_detectors = ifelse((SSOCS07$metal_detectors==2),0,1)
SSOCS07$tipline = ifelse((SSOCS07$tipline==2),0,1)
SSOCS07$counseling = ifelse((SSOCS07$counseling==2),0,1)
SSOCS07$crime = 4 - SSOCS07$crime






## DESCRIPTIVES OF RESPONSE ##

summary(SSOCS07$bullying)
table(SSOCS07$bullying)





## CONTINGENCY TABLES OF RESPONSE VERSUS PREDICTORS #

table(SSOCS07$uniforms,SSOCS07$bullying)
table(SSOCS07$metal_detectors,SSOCS07$bullying)
table(SSOCS07$tipline,SSOCS07$bullying)
table(SSOCS07$counseling,SSOCS07$bullying)
table(SSOCS07$crime,SSOCS07$bullying)






## STACKED BAR PLOTS OF BULLYING BY COUNSELING ##

ggplot(SSOCS07, aes(as.factor(bullying),fill=as.factor(counseling))) + 
	geom_bar() +
	scale_fill_grey() + 
	ggtitle("Stacked Barplot of Bullying by Counseling") + 
	xlab("Level of Bullying") + 
	ylab("Frequency by Counseling") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"),legend.position="none")







## CONSTRUCT ODDS OF COUNSELING BY LEVEL OF BULLYING ##

counselingTable = table(counseling,bullying)

odds1 = counselingTable[2,5]/counselingTable[1,5]
odds2 = counselingTable[2,4]/counselingTable[1,4]
odds3 = counselingTable[2,3]/counselingTable[1,3]
odds4 = counselingTable[2,2]/counselingTable[1,2]
odds5 = counselingTable[2,1]/counselingTable[1,1]

counselingOdds = c(odds1,odds2,odds3,odds4,odds5)
bullying_levels = c(1,2,3,4,5)

counselingData = as.data.frame(cbind(counselingOdds,bullying_levels))



## LINE PLOT OF ODDS OF COUNSELING, BY LEVEL OF BULLYING ##

ggplot(counselingData, aes(x=bullying_levels, y=counselingOdds)) +
	geom_point() +
	geom_line(aes(group=1)) +
	scale_x_discrete(labels=c("Never","On Occasion","Monthly","Weekly","Daily")) +
	ggtitle("Odds of Counseling by Bullying Level") + 
	xlab("Level of Bullying") + 
	ylab("Odds of Counseling") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))







## CONSTRUCT PROPORTION BULLYING BY CRIME LEVEL ##

crimeTable = table(crime,bullying)

prop11 = crimeTable[1,5]/sum(crimeTable[1,])
prop12 = crimeTable[1,4]/sum(crimeTable[1,])
prop13 = crimeTable[1,3]/sum(crimeTable[1,])
prop14 = crimeTable[1,2]/sum(crimeTable[1,])
prop15 = crimeTable[1,1]/sum(crimeTable[1,])

propLow = c(prop11,prop12,prop13,prop14,prop15)

prop21 = crimeTable[2,5]/sum(crimeTable[2,])
prop22 = crimeTable[2,4]/sum(crimeTable[2,])
prop23 = crimeTable[2,3]/sum(crimeTable[2,])
prop24 = crimeTable[2,2]/sum(crimeTable[2,])
prop25 = crimeTable[2,1]/sum(crimeTable[2,])

propMed = c(prop21,prop22,prop23,prop24,prop25)

prop31 = crimeTable[3,5]/sum(crimeTable[3,])
prop32 = crimeTable[3,4]/sum(crimeTable[3,])
prop33 = crimeTable[3,3]/sum(crimeTable[3,])
prop34 = crimeTable[3,2]/sum(crimeTable[3,])
prop35 = crimeTable[3,1]/sum(crimeTable[3,])

propHigh = c(prop31,prop32,prop33,prop34,prop35)

proportion = c(propLow,propMed,propHigh)
crime_num=c(rep(1,5),rep(2,5),rep(3,5))
bully_levels = rep(bullying_levels,3)

crimeData = as.data.frame(cbind(proportion,crime_num,bully_levels))




## LINES PLOT OF PROPORTION OF EACH LEVEL OF BULLYING, BY LEVEL OF CRIME ##

ggplot(crimeData, aes(x=bully_levels, y=proportion)) +
	geom_point() +
	geom_line(aes(group=crime_num,linetype=as.factor(crime_num))) +
	scale_linetype_manual(values=c("solid","longdash","dotted")) +
	scale_x_discrete(labels=c("Never","On Occasion","Monthly","Weekly","Daily")) +
	annotate("text",x=5,y=0.22,label="High") + 
	annotate("text",x=5,y=0.13,label="Moderate") + 
	annotate("text",x=5,y=0.09,label="Low") + 
	ggtitle("Proportions of Bullying by Crime Level") + 
	xlab("Level of Bullying") + 
	ylab("Proportion of Schools by Crime Level") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"),legend.position="none")






## STACKED BAR PLOTS OF PREDICTORS, BY LEVEL OF BULLYING ##

qplot(as.factor(SSOCS07$metal_detectors),fill=as.factor(SSOCS07$bullying),geom='bar')
qplot(as.factor(SSOCS07$tipline),fill=as.factor(SSOCS07$bullying),geom='bar')
qplot(as.factor(SSOCS07$counseling),fill=as.factor(SSOCS07$bullying),geom='bar')
qplot(as.factor(SSOCS07$crime),fill=as.factor(SSOCS07$bullying),geom='bar')





## ADJACENT CATEGORIES MULTINOMIAL LOGISTIC REGRESSION MODEL FOR BULLYING ##

SSOCS07 = SSOCS07[order(SSOCS07$bullying),]

## 1 IS DAILY, 2 IS WEEKLY, ETC, SO REVERSE ORDER OF COMPARISONS TO HAVE GREATER / LESSER ##

ACModel = vglm(bullying~uniforms+metal_detectors+tipline+counseling+as.factor(crime)+suspensions+as.factor(crime)*suspensions,family=acat(parallel=TRUE,reverse=TRUE),data=SSOCS07)



## MODEL SUMMARY ##

summary(ACModel)





## PEARSON RESIDUALS ##

summary(summary(ACModel)@pearson.resid)



## DEVIANCE RESIDUALS ##

summary(summary(ACModel)@residuals)




## PROPORTIONAL ODDS TEST ##

ACModel_NPO = vglm(bullying~uniforms+metal_detectors+tipline+counseling+as.factor(crime)+suspensions+as.factor(crime)*suspensions,family=acat(parallel=FALSE,reverse=TRUE),data=SSOCS07)

pchisq(deviance(ACModel_NPO)-deviance(ACModel),df=df.residual(ACModel_NPO)-df.residual(ACModel),lower.tail=FALSE)




## ODDS RATIOS AND CONFIDENCE INTERVALS ##

(exp(coefficients(ACModel)))

(OddsRatios = exp(cbind(OddsRatios=coef(ACModel),confint(ACModel))))





## PREDICTION: PREDICTED VALUES USING THE CURRENT SAMPLE ##

summary(summary(ACModel)@fitted.values)






## PREDICTION: PREDICTED VALUES USING NEW DATA ##

newPredictors = as.data.frame(expand.grid(uniforms=c(1), metal_detectors=c(0), tipline=c(0), counseling=c(1), suspensions = c(mean(suspensions,na.rm=TRUE)), crime=c(1,2,3)))

predicted_values = as.data.frame(cbind(newPredictors,predicted=predict(ACModel,newdata=newPredictors,type="response")))



## CREATE MULTINOMIAL PREDICTED VALUE S##

MNpredictions = c(rev(predicted_values[1,(7:11)])$predicted.5,rev(predicted_values[1,(7:11)])$predicted.4,rev(predicted_values[1,(7:11)])$predicted.3,rev(predicted_values[1,(7:11)])$predicted.2,rev(predicted_values[1,(7:11)])$predicted.1,rev(predicted_values[2,(7:11)])$predicted.5,rev(predicted_values[2,(7:11)])$predicted.4,rev(predicted_values[2,(7:11)])$predicted.3,rev(predicted_values[2,(7:11)])$predicted.2,rev(predicted_values[2,(7:11)])$predicted.1,rev(predicted_values[3,(7:11)])$predicted.5,rev(predicted_values[3,(7:11)])$predicted.4,rev(predicted_values[3,(7:11)])$predicted.3,rev(predicted_values[3,(7:11)])$predicted.2,rev(predicted_values[3,(7:11)])$predicted.1)

MNpredictionData = as.data.frame(cbind(MNpredictions,crime_num=crimeData$crime_num,bully_levels=crimeData$bully_levels))





## LINES PLOT OF PREDICTED PROBABILITIES OF BULLYING, BY CRIME LEVEL ##

ggplot(MNpredictionData, aes(x=bully_levels, y=MNpredictions)) +
	geom_point() +
	geom_line(aes(group=crime_num,linetype=as.factor(crime_num))) +
	scale_linetype_manual(values=c("solid","longdash","dotted")) +
	scale_x_discrete(labels=c("Never","On Occasion","Monthly","Weekly","Daily")) +
	annotate("text",x=5,y=0.165,label="High") + 
	annotate("text",x=5,y=0.12,label="Moderate") + 
	annotate("text",x=5,y=0.07,label="Low") + 
	ggtitle("Predicted Probabilities of Bullying by Crime Level") + 
	xlab("Level of Bullying") + 
	ylab("Predicted Probabilities of Bullying") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"),legend.position="none")










