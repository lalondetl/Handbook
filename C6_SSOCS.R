# R ANALYSIS FOR CHAPTER 6: HURDLE REGRESSION OF NUMBER OF SUSPENSIONS #
##	SCHOOL SURVEY ON CRIME AND SAFETY DATA		##





## LOAD RELEVANT PACKAGES ##

library(sas7bdat)
library(ggplot2)
library(plyr)
library(ResourceSelection)
library(pscl)



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






## SHOW PROPORTION OF VALUES THAT ARE ZEROS ##

susp_bin = ifelse(suspensions>0,1,0)
table(susp_bin)




## HURDLE NEGATIVE BINOMIAL MODEL ##

hurdleModel = hurdle(suspensions~uniforms+metal_detectors+tipline+counseling+as.factor(crime)+discipline_training+behavioral_training+insubordination+percent_limited_English+discipline_training*as.factor(crime)+behavioral_training*as.factor(crime), dist="negbin", zero.dist="binomial", link="logit", data=SSOCS07)




## MODEL SUMMARY ##

summary(hurdleModel)




## FIT STATISTICS AND COEFFICIENT INTERPRETATIONS ##

summary(hurdleModel)$loglik
exp(coef(hurdleModel))





## SCATTER PLOT OF RESIDUALS VERSUS INSUBORDINATES ##

residuals = summary(hurdleModel)$residuals
qplot(insubordination,residuals)





