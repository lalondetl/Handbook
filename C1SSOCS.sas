/* SAS ANALYSIS FOR CHAPTER 1: EXPLORATORY DATA ANALYSIS */
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





/* SUMMARIES FOR CONTINUOUS VARIABLES */

proc means data=SSOCS07;
	var suspensions insubordination percent_limited_English below_15th;
run;





/* CREATE LOG-TRANSFORMED OUTCOMES */

data SSOCS07;
	set SSOCS07;
	log.suspensions = log(suspensions+0.01);
	log.insubordination = log(insubordination+0.01);
run;





/* SCATTERPLOT MATRIX */

proc sgscatter data=FData;
	title "Histograms, Scatter Plots, and Pairwise Correlations";
	matrix log.suspensions log.insubordination percent_limited_English below_15th crime;
run;
title;





/* BOX PLOTS OF (LOG-TRANSFORMED) SUSPENSIONS BY CRIME LEVEL */

proc boxplot data=SSOCS07;
	title "Box Plots of Log-Suspensions by Crime";
	plot log.suspensions*crime;
run;
title;






/* PROPORTIONS OF CATEGORICAL VARIABLES */

proc freq data=SSOCS07;
	tables bullying uniforms metal_detectors tipline counseling crime behavioral_training discipline_training;
run;





/* CONTINGENCY TABLES OF BULLYING VERSUS OTHER VARIABLES */

proc freq data=SSOCS07;
	tables bullying*(uniforms metal_detectors tipline counseling crime behavioral_training discipline_training);
run;





/* SUMMARY OF CONTINUOUS VARIABLES BY BULLYING */

proc means data=SSOCS07 nway;
	class bullying;
	var insubordination percent_limited_English below_15th;
run;






/* SUMMARY OF SUSPENSIONS BY CATEGORICAL VARIABLES */

proc means data=SSOCS07 nway;
	class bullying;
	var suspensions;
run;

proc means data=SSOCS07 nway;
	class uniforms;
	var suspensions;
run;

proc means data=SSOCS07 nway;
	class metal_detectors;
	var suspensions;
run;

proc means data=SSOCS07 nway;
	class tipline;
	var suspensions;
run;

proc means data=SSOCS07 nway;
	class counseling;
	var suspensions;
run;

proc means data=SSOCS07 nway;
	class crime;
	var suspensions;
run;

proc means data=SSOCS07 nway;
	class behavioral_training;
	var suspensions;
run;

proc means data=SSOCS07 nway;
	class discipline_training;
	var suspensions;
run;





/* BOX PLOTS OF SUSPENSIONS BY CATEGORICAL VARIABLES */

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
	plot suspensions*crime;
run;

proc boxplot data=SSOCS07;
	plot suspensions*discipline_training;
run;

proc boxplot data=SSOCS07;
	plot suspensions*behavioral_training;
run;


quit;





