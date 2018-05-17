/* SAS ANALYSIS FOR CHAPTER 1: EXPLORATORY DATA ANALYSIS */
/*	FRAMINGHAM HEART STUDY DATA		*/





/* READ IN THE DATA */

proc import datafile='PATH\frmgham2.csv'
	out=FData
	dbms=csv replace;
	getnames=yes;
run;





/* SUMMARIES FOR CONTINUOUS VARIABLES */

proc means data=FData;
	var TOTCHOL AGE CIGPDAY TIMEHYP;
run;





/* SCATTERPLOT MATRIX */

proc sgscatter data=FData;
	title "Histograms, Scatter Plots, and Pairwise Correlations";
	matrix TOTCHOL AGE CIGPDAY TIMEHYP;
run;
title;






/* PROPORTIONS OF CATEGORICAL VARIABLES */

proc freq data=FData;
	tables SEX DIABETES PREVHYP HYPERTEN;
run;




/* CONTINGENCY TABLES OF HYPERTENSION VERSUS OTHER VARIABLES */

proc freq data=FData;
	tables HYPERTEN*SEX HYPERTEN*DIABETES;
run;




/* SUMMARY OF CONTINUOUS VARIABLES BY HYPERTENSION */

proc means data=FData nway;
	class HYPERTEN;
	var TOTCHOL AGE CIGPDAY TIMEHYP;
run;





/* SCATTER PLOTS OF HYPERTEN VERSUS EACH CONTINUOUS VARIABLE */


/* HYPERTENSIVE VERSUS AGE */

proc sgplot data=FData;
	scatter y=HYPERTEN x=AGE;
run;

proc loess data=FData;
	title "Scatter Plot of Hypertensive Versus Age";
	model HYPERTEN=AGE;
run;
title;





/* HYPERTENSIVE VERSUS CIGARETTES PER DAY */

proc loess data=FData;
	title "Scatter Plot of Hypertensive Versus Cigarettes";
	model HYPERTEN=CIGPDAY;
run;
title;


/* HYPERTENSIVE VERSUS TOTAL CHOLESTEROL */

proc loess data=FData;
	title "Scatter Plot of Hypertensive Versus Cholesterol";
	model HYPERTEN=TOTCHOL;
run;
title;


quit;

