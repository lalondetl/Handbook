/* ANALYSIS FOR CHAPTER 6: HURDLE REGRESSION OF NUMBER OF SUSPENSIONS */
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






/* SHOW PROPORTION OF VALUES THAT ARE ZEROS */

proc freq data=SSOCS07;
	tables suspensions;
run;




/* HURDLE NEGATIVE BINOMIAL MODEL */

proc fmm data=SSOCS07;
	class uniforms metal_detectors tipline counseling crime discipline_training behavioral_training;
	model suspensions = uniforms metal_detectors tipline counseling crime discipline_training behavioral_training insubordination percent_limited_English discipline_training*crime behavioral_training*crime / dist=truncnegbin;
	model + / dist=constant;
	output out=HNBOut residual=residual predicted=yhat;
run;




/* SCATTER PLOT OF RESIDUALS VERSUS INSUBORDINATES */

proc sgplot data=HNBOut;
	scatter y=residual x=insubordination;
run;







