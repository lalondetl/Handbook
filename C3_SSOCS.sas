/* SAS ANALYSIS FOR CHAPTER 3: NORMAL LINEAR REGRESSION ANALYSIS */
/*	SCHOOL SURVEY ON CRIME AND SAFETY DATA		*/




/* READ IN THE DATA */

data SSOCS07;
	set 'PATH/SSOCS07_Data.sas7bdat';
run;




/* RENAME VARIABLES USING MEANINGFUL LABELS */

data SSOCS07;
	set SSOCS07 (rename=(C0514=suspensions C0134=uniforms C0116=metal_detectors C0188=tipline C0178=counseling C0562=crime C0268=discipline_training C0276=behavioral_training C0508=insubordination C0510=insubordination_removal C0526=percent_limited_English C0532=below_15th C0376=bullying));
run;




/* CREATE INDICATOR VARIABLES FOR CHARACTERISTICS OF INTEREST */

data SSOCS07;
	set SSOCS07;
	uniforms = 1-(uniforms=2);
	metal_detectors = 1-(metal_detectors=2);
	tipline = 1-(tipline=2);
	counseling = 1-(counseling=2);
	crime = 4-crime;
	discipline_training = 1-(discipline_training=2);
	behavioral_training = 1-(behavioral_training=2);
run;







## NORMAL LINEAR REGRESSION ##

normalModel = lm(suspensions~,data=SSOCS07)


/* NORMAL LINEAR REGRESSION */
/* CHECK FOR CONSTANT VARIANCE */

proc glm data=FData;
	class uniforms metal_detectors tipline counseling crime discipline_training behavioral_training;
	model suspensions = uniforms metal_detectors tipline counseling crime discipline_training behavioral_training insubordination percent_limited_English discipline_training*crime behavioral_training*crime;
	means crime / hovtest;
	output out=RegOut r=residual p=yhat;
run;



/* SCATTER PLOT OF RESIDUALS VERSUS PREDICTED VALUES */

proc sgplot data=RegOut;
	title "Scatter Plot of Normal Model Residuals vs Predicted Values"
	scatter y=residual x=yhat;
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




