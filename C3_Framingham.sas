/* SAS ANALYSIS FOR CHAPTER 3: NORMAL LINEAR REGRESSION ANALYSIS */
/*	FRAMINGHAM HEART STUDY DATA		*/






/* READ IN THE DATA */

proc import datafile='PATH\frmgham2.csv'
	out=FData
	dbms=csv replace;
	getnames=yes;
run;





/* NORMAL LINEAR REGRESSION */
/* CHECK FOR CONSTANT VARIANCE */

proc glm data=FData;
	class SEX DIABETES;
	model HYPERTEN = TOTCHOL AGE CIGPDAY SEX DIABETES SEX*DIABETES SEX*CIGPDAY;
	means SEX / hovtest;
	output out=RegOut r=residual p=yhat;
run;



/* SCATTER PLOT OF RESIDUALS VERSUS AGE */

proc sgplot data=RegOut;
	title "Scatter Plot of Normal Model Residuals vs Age"
	scatter y=residual x=AGE;
run;
title;





/* CHECK FOR RESIDUAL NORMALITY */
/* ANDERSON-DARLING TEST */
/* NORMAL PROBABILITY PLOT */

proc univariate data=RegOut normaltest;
	var residual;
	probplot;
run;





/* THE NORMAL LINEAR MODEL HAS POOR PREDICTION */

proc means data=RegOut;
	var yhat;
run;

