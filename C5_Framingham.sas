/* SAS ANALYSIS FOR CHAPTER 5: LOGISTIC REGRESSION */
/*	FRAMINGHAM HEART STUDY DATA		*/





/* READ IN THE DATA */

proc import datafile='PATH\frmgham2.csv'
	out=FData
	dbms=csv replace;
	getnames=yes;
run;





/* CONSIDER BASELINE OBSERVATIONS ONLY */

data FData_Baseline;
	set FData;
	if (PERIOD > 1) then delete;
run;




/* FILTER OUT INDIVIDUALS WITH PREVALENCE OF HYPERTENSION AT BASELINE */

data FData_Baseline_NOHYP;
	set FData_Baseline;
	if (PREVHYP=1) then delete;
run;






/* DESCRIPTIVES: TABLES OF PROPORTIONS */

proc freq data=FData_Baseline_NOHYP;
	tables HYPERTEN;
run;




/* CONTINGENCY TABLES OF HYPERTENSION VERSUS PREDICTORS */

proc freq data=FData_Baseline_NOHYP;
	tables HYPERTEN*(DIABETES SEX);
run;






/* SCATTER PLOT OF HYPERTENSION VERSUS AGE */

proc sgplot data=FData_Baseline_NOHYP;
	title "Scatter Plot of Hypertensive vs Age"
	scatter y=HYPERTEN x=AGE;
run;
title;





/* SCATTER PLOT OF HYPERTENSION VERSUS CHOLESTEROL */

proc sgplot data=FData_Baseline_NOHYP;
	title "Scatter Plot of Hypertensive vs Cholesterol"
	scatter y=HYPERTEN x=TOTCHOL;
run;
title;





/* EXPLORATORY PLOTS OF PREDICTORS VERSUS EACH OTHER */

proc sgplot data=FData_Baseline_NOHYP;
	scatter y=TOTCHOL x=CIGPDAY;
run;

proc sgplot data=FData_Baseline_NOHYP;
	scatter y=AGE x=CIGPDAY;
run;

proc sgplot data=FData_Baseline_NOHYP;
	scatter y=TOTCHOL x=AGE;
run;


proc boxplot data=FData_Baseline_NOHYP;
	plot AGE*SEX;
run;

proc boxplot data=FData_Baseline_NOHYP;
	plot CIGPDAY*SEX;
run;

proc boxplot data=FData_Baseline_NOHYP;
	plot TOTCHOL*SEX;
run;







/* LOGISTIC REGRESSION MODEL FOR PROBABILITY OF HYPERTENSTION */
/* HOSMER-LEMESHOW TEST */
/* ROC CURVE AND AREA */
/* ODDS RATIOS AND CONFIDENCE INTERVALS */

proc logistic data=FData_Baseline_NOHYP descending plots(only)=(roc(id=obs) effect);
	class DIABETES SEX;
	model HYPERTEN = DIABETES SEX AGE CIGPDAY TOTCHOL SEX*DIABETES SEX*CIGPDAY / lackfit;
	output out=LROut r=residual p=yhat;
run;





/* STANDARDIZED DEVIANCE RESIDUALS */

proc univariate data=LROut;
	var residual;
run;






## SCATTER PLOT OF DEVIANCE RESIDUALS VERSUS AGE ##

qplot(AGE,summary(LRModel)$deviance.resid)




/* SCATTER PLOT OF DEVIANCE RESIDUALS VERSUS FITTED VALUES */

proc sgplot data=LROut;
	title "Scatter Plot of Residuals Versus Predicted Values";
	scatter y=residual x=yhat;
run;
title;





/* PREDICTION: PREDICTED VALUES USING THE CURRENT SAMPLE */

proc univariate data=LROut;
	var yhat;
run;





