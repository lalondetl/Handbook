/* SAS ANALYSIS FOR CHAPTER 8: MARGINAL LONGITUDINAL REGRESSION */
/*	FRAMINGHAM HEART STUDY DATA		*/





/* READ IN THE DATA */

proc import datafile='PATH\frmgham2.csv'
	out=FData
	dbms=csv replace;
	getnames=yes;
run;





/* CONTINGENCY TABLES OF HYPERTENSION VERSUS PREDICTORS */

proc freq data=FData;
	tables PREVHYP*(PERIOD SEX DIABETES);
run;






/* STACKED BAR PLOT OF HYPERTENSION BY TIME, SEX */

proc gchart data=FData;
	title "Stacked Barplot of Hypertensive by Time, Sex";
	vbar PREVHYP / sumvar=SEX subgroup=PERIOD;
run;
title;





/* BAR PLOTS OF DIABETES BY TIME, FILLED BY HYPERTENSION */

proc gchart data=FData;
	vbar DIABETES / sumvar=PREVHYP subgroup=PERIOD;
run;





/* STACKED BAR PLOT OF HYPERTENSION BY TIME, DIABETES */

proc gchart data=FData;
	title "Stacked Barplot of Hypertensive by Time, Diabetes";
	vbar PREVHYP / sumvar=DIABETES subgroup=PERIOD;
run;
title;





/* PLOT OF HYPERTENSION VERSUS AGE */

proc sgplot data=FData;
	title "Plot of Hypertensive vs Age, by Time";
	scatter y=PREVHYP x=AGE / group=PERIOD;
run;
title;





/* PLOT OF HYPERTENSION VERSUS CIGARETTES PER DAY */

proc sgplot data=FData;
	title "Plot of Hypertensive vs Cigarettes per Day, by Time";
	scatter y=PREVHYP x=CIGPDAY / group=PERIOD;
run;
title;






/* PLOT OF HYPERTENSION VERSUS CHOLESTEROL */

proc sgplot data=FData;
	title "Plot of Hypertensive vs Cholesterol, by Time";
	scatter y=PREVHYP x=TOTCHOL / group=PERIOD;
run;
title;






/* PRODUCE RESIDUALS FROM NON-CORRELATED MODEL */

proc genmod data=FData noprint;
	class SEX;
	model PREVHYP = DIABETES SEX AGE CIGPDAY TOTCHOL SEX*DIABETES SEX*CIGPDAY / dist=binomial link=logit;
	output out=newFData resdev=resid;
run;


/* SCATTERPLOT MATRIX */

proc sort data =newFData;
	by RANDID;
run;

proc transpose data=newFData out=FDataWide prefix=resid;
    by RANDID;
    id PERIOD;
    var resid;  
run;

proc sgscatter data=FDataWide;
	title "Scatter Plots and Pairwise Correlations of Residuals";
	matrix resid1 resid2 resid3  diagonal=(HISTOGRAM);
run;
title;




/* RESIDUAL CORRELATION MATRIX */

proc corr data=FDataWide;
	var resid1 resid2 resid3;
run;





/* VARIOGRAM */

proc variogram data=newFData plots=(SEMIVAR(CLA));	
	compute lagd=1 maxlag=3;
	coordinates xc=PERIOD yc=resid;
	var resid;
run;







/* MARGINAL ANALYSIS: GEE */
/* QIC VALUES */
/* ODDS RATIOS AND CONFIDENCE INTERVALS */


/* INDEPENDENT WORKING CORRELATION STRUCTURE */

proc genmod data=FData descending plots=all;
	class SEX RANDID;
	model PREVHYP = DIABETES SEX AGE CIGPDAY TOTCHOL SEX*DIABETES SEX*CIGPDAY / dist=bin link=logit p cl;
	repeated subject=RANDID / type=ind corrw;
run;



/* EXCHANGEABLE WORKING CORRELATION STRUCTURE */

proc genmod data=FData descending plots=all;
	class SEX RANDID;
	model PREVHYP = DIABETES SEX AGE CIGPDAY TOTCHOL SEX*DIABETES SEX*CIGPDAY / dist=bin link=logit p cl;
	repeated subject=RANDID / type=cs corrw;
run;



/* AUTO-REGRESSIVE WORKING CORRELATION STRUCTURE */

proc genmod data=FData descending plots=all;
	class SEX RANDID;
	model PREVHYP = DIABETES SEX AGE CIGPDAY TOTCHOL SEX*DIABETES SEX*CIGPDAY / dist=bin link=logit p cl;
	repeated subject=RANDID / type=ar(1) corrw;
	output out=AROut resdev=residual predicted=yhat;
run;



/* UNSTRUCTURED WORKING CORRELATION STRUCTURE */

proc genmod data=FData descending plots=all;
	class SEX RANDID;
	model PREVHYP = DIABETES SEX AGE CIGPDAY TOTCHOL SEX*DIABETES SEX*CIGPDAY / dist=bin link=logit p cl;
	repeated subject=RANDID / type=unstr corrw;
run;







/* AR(1) MODEL RESIDUALS */

proc univariate data=AROut;
	var residual;
run;




/* SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES */

proc sgplot data=AROut;
	scatter y=residual x=yhat;
run;



/* SCATTER PLOT OF RESIDUALS VERSUS TIME */

proc sgplot data=AROut;
	scatter y=residual x=PERIOD;
run;






/* NORMAL PROBABILITY PLOTS AND NORMALITY TESTS FOR RESIDUALS */

proc univariate data=AROut normaltest;
	var residual;
	probplot;
run;






/* PREDICTION: PREDICTED VALUES USING THE CURRENT SAMPLE */

proc univariate data=AROut;
	var yhat;
run;







