/* SAS ANALYSIS FOR CHAPTER 5: ORDINAL MULTINOMIAL LOGISTIC REGRESSION */
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





/* DESCRIPTIVES OF RESPONSE */

proc freq data=SSOCS07;
	tables bullying;
run;





/* CONTINGENCY TABLES OF RESPONSE VERSUS PREDICTORS */

proc freq data=SSOCS07;
	tables bullying*(uniforms metal_detectors tipline counseling crime);
run;





/* ADJACENT CATEGORIES MULTINOMIAL LOGISTIC REGRESSION MODEL FOR BULLYING */
/* ODDS RATIOS AND CONFIDENCE INTERVALS */

proc catmod data=SSOCS07;
	direct suspensions;
	response alogit / out=MNRegOut resid=residual pred=yhat;
	model bullying = uniforms metal_detectors tipline counseling crime suspensions crime*suspensions;
run;




/* RESIDUALS SUMMARY */
proc means data=MNRegOut;
	var residual;
run;





/* PROPORTIONAL ODDS TEST */

proc logistic data=SSOCS07;
	class uniforms metal_detectors tipline counseling crime;
	model bullying = uniforms metal_detectors tipline counseling crime suspensions crime*suspensions;
run;





/* PREDICTION: PREDICTED VALUES USING THE CURRENT SAMPLE */

proc means data=MNRegOut;
	var yhat;
run;








