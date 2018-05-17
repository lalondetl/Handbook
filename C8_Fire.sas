/* SAS ANALYSIS FOR CHAPTER 8: LONGITUDINAL MODELING */
/*	FIRE-CLIMATE INTERACTION DATA		*/






/* READ IN THE DATA */

proc import datafile='PATH\FireData_Aggregated.csv'
	out=FireData_Aggregated
	dbms=csv replace;
	getnames=yes;
run;






/* DESCRIPTIVES OF THE RESPONSE */

proc means data=FireData_Aggregated;
	var Decade_Count;
run;





/* DECADE COUNT HISTOGRAM */

proc univariate data=FireData_Aggregated;
	title "Histogram of Decade Fire Counts";
	var Decade_Count;
	histogram;
run;
title;





/* DECADE COUNT BOX PLOT */

proc boxplot data=FireData_Aggregated;
	title "Box Plot of Decade Fire Counts";
	plot Decade_Count;
run;
title;






/* DESCRIPTIVES INCLUDING PREDICTORS */

proc means data=FireData_Aggregated nway;
	class Site;
	var Decade_Count;
run;

proc means data=FireData_Aggregated nway;
	class Region;
	var Decade_Count;
run;





/* DECADE COUNT BOX PLOTS BY SITE */

proc boxplot data=FireData_Aggregated;
	title "Box Plots of Decade Fire Mean by Site";
	plot Decade_Count*Site;
run;
title;




/* DECADE COUNT BOX PLOTS BY REGION */

proc boxplot data=FireData_Aggregated;
	title "Box Plots of Decade Fire Mean by Region";
	plot Decade_Count*Region;
run;
title;





/* SCATTER PLOT OF DECADE FIRE COUNT VERSUS TIME */

proc sgplot data=FireData_Aggregated;
	title "Scatter Plot of Decade Fire Count Versus Decade";
	scatter y=Decade_Count x=Decade;
run;
title;







## SPAGHETTI PLOT OF DECADE FIRE COUNTS BY TIME ##

proc sgplot data=FireData_Aggregated;
	series y=Decade_Count x=Decade / group=Site;
run;




/* SCATTER PLOT OF DECADE FIRE COUNT MEANS BY REGION */

proc sort data=FireData_Aggregated;
  by Region Decade;
run;

proc univariate data=FireData_Aggregated noprint;
  by Region Decade;
  var Decade_Count;
  output out=means mean=countmean;
run;

proc sgplot data=means;
	title "Plot of Fire Count Means by Region";
	series y=countmean x=Decade / group=Region;
run;
title;





/* PRODUCE RESIDUALS FROM NON-CORRELATED MODEL */

proc genmod data=FireData_Aggregated noprint;
	class Region;
	model Decade_Count = Region Decade / dist=poisson link=log;
	output out=newFData resdev=resid;
run;



/* RESIDUAL CORRELATION MATRIX */

proc sort data =newFData;
	by Site;
run;

proc transpose data=newFData out=FDataWide prefix=resid;
    by Site;
    id Decade;
    var resid;  
run;

proc corr data=FDataWide;
	var resid1 resid2 resid3;
run;





/* VARIOGRAM */

proc variogram data=newFData plots=(SEMIVAR(CLA));	
	compute lagd=1 maxlag=20;
	coordinates xc=Decade yc=resid;
	var resid;
run;







/* DEFINE DECADE-SQUARED FOR MODELS */

data FireData_Aggregated;
	set FireData_Aggregated;
	Decade2 = Decade^2;
run;






/* RANDOM-INTERCEPT POISSON MODEL */

proc glimmix data=FireData_Aggregated method=mspl ic=pq;
      class Region Site;
      model Decade_Count = Region Decade Decade*Region / s dist=poisson link=log corrb cl;
      random Int | Site/ SUBJECT=Site;
      output out = RIPModel resid=residual pred = pred;   
run;





/* SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES */

proc sgplot data=RIPModel;
	title "Residual Plot for Linear Decade Model";
	scatter y=residual x=pred;
run;
title;






/* RANDOM INTERCEPT POISSON MODEL */
/*	QUADTRATIC TERM INCLUDED		*/
/*	LINEAR INTERACTIONS WITH REGION INCLUDED	*/

proc glimmix data=FireData_Aggregated method=mspl ic=pq;
      class Region Site;
      model Decade_Count = Region Decade Decade2 Decade*Region / s dist=poisson link=log corrb cl;
      random Int | Site/ SUBJECT=Site;
      output out = RIPOut resid=residual pred = pred;   
run;





/* SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES */

proc sgplot data=RIPOut;
	title "Residual Plot for Qudratic Decade Model";
	scatter y=residual x=pred;
run;
title;





/* SCATTER PLOT OF RESIDUALS VERSUS TIME */

proc sgplot data=RIPOut;
	title "Scatter Plot of Residuals Versus Decade";
	scatter y=residual x=Decade;
run;
title;





/* NORMAL PROBABILITY PLOT OF RESIDUALS */

proc univariate data=RIPOut normaltest;
	var residual;
	probplot;
run;







/* RANDOM SLOPES POISSON MODEL 	*/
/*	QUADTRATIC TERM INCLUDED		*/
/*	LINEAR INTERACTIONS WITH REGION INCLUDED	*/


/* INCLUDING A RANDOM SLOPE FOR THE LINEAR COMPONENT ONLY */
proc glimmix data=FireData_Aggregated method=mspl ic=pq;
      class Region Site;
      model Decade_Count = Region Decade Decade2 Decade*Region / s dist=poisson link=log corrb cl;
      random Int Decade | Site/ SUBJECT=Site;
      output out = RSPOut resid=residual pred = pred;   
run;



/* INCLUDING RANDOM SLOPES FOR BOTH LINEAR AND QUADRATIC COMPONENTS */
proc glimmix data=FireData_Aggregated method=mspl ic=pq;
      class Region Site;
      model Decade_Count = Region Decade Decade2 Decade*Region / s dist=poisson link=log corrb cl;
      random Int Decade Decade2 | Site/ SUBJECT=Site;
      output out = RSPOut resid=residual pred = pred;   
run;







/* RANDOM INTERCEPT NEGATIVE BINOMIAL MODEL */
/*	QUADTRATIC TERM INCLUDED		*/

proc glimmix data=FireData_Aggregated method=mspl ic=pq;
      class Region Site;
      model Decade_Count = Region Decade Decade2 Decade*Region / s dist=negbin link=log corrb cl;
      random Int | Site/ SUBJECT=Site;
      output out = RINBOut resid=residual pred = pred;   
run;







/* RANDOM SLOPE NEGATIVE BINOMIAL MODEL */
/*	QUADTRATIC TERM INCLUDED		*/

proc glimmix data=FireData_Aggregated method=mspl ic=pq;
      class Region Site;
      model Decade_Count = Region Decade Decade2 Decade*Region / s dist=negbin link=log corrb cl;
      random Int Decade | Site/ SUBJECT=Site;
      output out = RSNBOut resid=residual pred = pred;   
run;









/* PREDICTION: PREDICTED VALUES USING THE CURRENT SAMPLE */

proc univariate data=RIPOut;
	var pred;
run;




/* SCATTER PLOT OF PREDICTED VALUES VERSUS OBSERVED VALUES */

proc sgplot data=RIPOut;
	scatter y=pred x=Decade_Count;
run;





/* CONTINGENCY TABLE OF PREDICTED VALUES VERSUS REGION, TIME */

proc freq data=RIPOut;
	tables pred*Region*Decade;
run;








