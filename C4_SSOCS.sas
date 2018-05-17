/* SAS ANALYSIS FOR CHAPTER 4: HETEROSCEDASTIC REGRESSION */
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






/* DESCRIPTIVES OF OUTCOME: SUSPENSIONS */

proc means data=SSOCS07;
	var suspensions;
run;





/* HISTOGRAM OF SUSPENSIONS */

proc univariate data=SSOCS07;
	title "Histogram of Suspensions";
	var suspensions;
	histogram;
run;
title;





/* BOX PLOT OF SUSPENSIONS */

proc boxplot data=SSOCS07;
	title "Box Plot of Suspensions";
	plot suspensions;
run;
title;






/* SCATTER PLOT OF SUSPENSIONS VERSUS INSUBORDINATION */

proc sgplot data=SSOCS07;
	title "Scatter Plot of Suspensions Versus Insubordinates";
	scatter y=suspensions x=insubordination;
run;
title;






/* BOX PLOTS OF SUSPENSIONS BY CRIME LEVEL */

proc boxplot data=SSOCS07;
	title "Box Plot of Suspensions";
	plot suspensions*crime;
run;
title;






/* PLOTS OF SUSPENSIONS BY OTHER PREDICTORS */

proc sgplot data=SSOCS07;
	scatter y=suspensions x=percent_limited_English;
run;

proc boxplot data=SSOCS07;
	plot suspensions*uniforms;
run;

proc boxplot data=SSOCS07;
	plot suspensions*metal_detectors;
run;

proc boxplot data=SSOCS07;
	plot suspensions*tipline;
run;

proc boxplot data=SSOCS07;
	plot suspensions*counseling;
run;

proc boxplot data=SSOCS07;
	plot suspensions*discipline_training;
run;

proc boxplot data=SSOCS07;
	plot suspensions*behavioral_training;
run;






/* NORMAL LINEAR REGRESSION */
/* CHECK FOR CONSTANT VARIANCE */

proc glm data=SSOCS07;
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







/* SQUARE ROOT TRANSFORMATION */

data SSOCS07;
	set SSOCS07;
	sqrt_suspensions = sqrt(suspensions);
run;




/* TRY RESPONSE VARIABLE TRANSFORMATION: SQUARE ROOT OF SUSPENSIONS */

proc reg data=SSOCS07;
	class uniforms metal_detectors tipline counseling crime discipline_training behavioral_training;
	model sqrt_suspensions = uniforms metal_detectors tipline counseling crime discipline_training behavioral_training insubordination percent_limited_English discipline_training*crime behavioral_training*crime;
	means crime / hovtest;
	output out=SqRegOut r=residual p=yhat;
run;



/* SCATTER PLOT OF RESIDUALS VERSUS PREDICTED VALUES */

proc sgplot data=SqRegOut;
	title "Scatter Plot of Normal Model Residuals vs Predicted Values, Square-Root Transformed"
	scatter y=residual x=yhat;
run;
title;









/* NATURAL LOGARITHM TRANSFORMATION */

data SSOCS07
	set SSOCS07;
	ln_suspensions = log(suspensions+0.01);
run;




/* TRY RESPONSE VARIABLE TRANSFORMATION: NATURAL LOGARITHM OF SUSPENSIONS */

proc glm data=SSOCS07;
	class uniforms metal_detectors tipline counseling crime discipline_training behavioral_training;
	model ln_suspensions = uniforms metal_detectors tipline counseling crime discipline_training behavioral_training insubordination percent_limited_English discipline_training*crime behavioral_training*crime;
	means crime / hovtest;
	output out=LnRegOut r=residual p=yhat;
run;



/* SCATTER PLOT OF RESIDUALS VERSUS PREDICTED VALUES */

proc sgplot data=LnRegOut;
	title "Scatter Plot of Normal Model Residuals vs Predicted Values, Logarithm Transformed"
	scatter y=residual x=yhat;
run;
title;






/* WEIGHTED LEAST SQUARES ESTIMATION */





/* WEIGHTED LEAST SQUARES: TRY PERCENT BELOW 15TH PERCENTILE */


/* SCATTER PLOT OF NORMAL MODEL RESIDUALS VERSUS PERCENT BELOW 15TH PERCENTILE */

proc sgplot data=RegOut;
	title "Scatter Plot of Normal Model Residuals vs Percent Below 15th Percentile"
	scatter y=residual x=below_15th;
run;
title;





/* WEIGHTED LEAST SQUARES ESTIMATION: USING PERCENT BELOW 15TH PERCENTILE */

proc glm data=SSOCS07;
	class uniforms metal_detectors tipline counseling crime discipline_training behavioral_training;
	model suspensions = uniforms metal_detectors tipline counseling crime discipline_training behavioral_training insubordination percent_limited_English discipline_training*crime behavioral_training*crime;
	weight below_15th;
	means crime / hovtest;
	output out=WRegOut1 r=residual p=yhat;
run;



/* SCATTER PLOT OF RESIDUALS VERSUS PREDICTED VALUES */

proc sgplot data=WRegOut1;
	title "Scatter Plot of Weighted Model Residuals vs Predicted Values"
	scatter y=residual x=yhat;
run;
title;







/* WEIGHTED LEAST SQUARES: TRY INSUBORDINATES */



/* SCATTER PLOT OF NORMAL MODEL RESIDUALS VERSUS INSUBORDINATES */

proc sgplot data=RegOut;
	title "Scatter Plot of Normal Model Residuals vs Insubordinates"
	scatter y=residual x=insubordination;
run;
title;





/* WEIGHTED LEAST SQUARES ESTIMATION: USING INVERSE OF INSUBORDINATES */

data SSOCS07;
	set SSOCS07;
	weights=(1/((insubordination+0.01)));
run;



proc glm data=SSOCS07;
	class uniforms metal_detectors tipline counseling crime discipline_training behavioral_training;
	model suspensions = uniforms metal_detectors tipline counseling crime discipline_training behavioral_training insubordination percent_limited_English discipline_training*crime behavioral_training*crime;
	weight weights;
	means crime / hovtest;
	output out=WRegOut2 r=residual p=yhat;
run;



/* SCATTER PLOT OF RESIDUALS VERSUS PREDICTED VALUES */

proc sgplot data=WRegOut2;
	title "Scatter Plot of Weighted Model Residuals vs Predicted Values"
	scatter y=residual x=yhat;
run;
title;






/* COMBINED APPROACH: TRY NATURAL-LOG-TRANSFORMED OUTCOME, INSUBORDINATES AS WEIGHTS */

proc glm data=SSOCS07;
	class uniforms metal_detectors tipline counseling crime discipline_training behavioral_training;
	model ln_suspensions = uniforms metal_detectors tipline counseling crime discipline_training behavioral_training insubordination percent_limited_English discipline_training*crime behavioral_training*crime;
	weight weights;
	means crime / hovtest;
	output out=WRegOut3 r=residual p=yhat;
run;



/* SCATTER PLOT OF RESIDUALS VERSUS PREDICTED VALUES */

proc sgplot data=WRegOut3;
	title "Residual Scatter Plot, Transformed, Weighted"
	scatter y=residual x=yhat;
run;
title;




/* CHECK FOR NORMALITY OF RESIDUALS */

proc univariate data=WRegOut3 normaltest;
	var residual;
	probplot;
run;







