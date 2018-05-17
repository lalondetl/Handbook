/* SAS ANALYSIS FOR CHAPTER 4: HETEROSCEDASTIC REGRESSION */
/*	FIRE-CLIMATE INTERACTION DATA		*/






/* READ IN THE DATA */

proc import datafile='PATH\FireData_Aggregated.csv'
	out=FireData_Aggregated
	dbms=csv replace;
	getnames=yes;
run;






/* DESCRIPTIVES OF THE RESPONSE */

proc means data=FireData_Aggregated;
	var Decade_Mean;
run;





/* DECADE MEAN HISTOGRAM */

proc univariate data=FireData_Aggregated;
	title "Histogram of Decade Fire Means";
	var Decade_Mean;
	histogram;
run;
title;





/* DECADE MEAN BOX PLOT */

proc boxplot data=FireData_Aggregated;
	title "Box Plot of Decade Fire Means";
	plot Decade_Mean;
run;
title;





/* DECADE MEAN VERSUS DECADE */

proc sgplot data=FireData_Aggregated;
	title "Scatter Plot of Decade Fire Mean Versus Decade";
	scatter y=Decade_Mean x=Decade;
run;
title;






/* DECADE MEAN BOX PLOTS BY REGION */

proc boxplot data=FireData_Aggregated;
	title "Box Plots of Decade Fire Mean by Region";
	plot Decade_Mean*Region;
run;
title;






/* DEFINE DECADE-SQUARED FOR MODEL */
data FireData_Aggregated;
	set FireData_Aggregated;
	Decade2 = Decade^2;
run;





/* NORMAL LINEAR REGRESSION */
/* CHECK FOR CONSTANT VARIANCE */

proc glm data=FireData_Aggregated;
	class Region;
	model Decade_Mean = Region Decade Decade2;
	means Region / hovtest;
	output out=RegOut r=residual p=yhat;
run;



/* SCATTER PLOT OF RESIDUALS VERSUS PREDICTED VALUES */

proc sgplot data=RegOut;
	title "Scatter Plot of Normal Model Residuals vs Predicted Values"
	scatter y=residual x=yhat;
run;
title;





/* SCATTER PLOT OF RESIDUALS VERSUS DECADE */

proc sgplot data=RegOut;
	title "Scatter Plot of Normal Model Residuals vs Decade"
	scatter y=residual x=Decade;
run;
title;







/* WEIGHTED LEAST SQUARES ESTIMATION */

/* EVALUATE RELATIONSHIP BETWEEN RESIDUALS AND DECADE VARIANCE */

proc sgplot data=RegOut;
	title "Scatter Plot of Normal Model Residuals vs Decade Variance"
	scatter y=residual x=Decade_Var;
run;
title;





/* USE TRANSFORMATION OF DECADE VARIATION AS WEIGHTS */

data FireData_Aggregated;
	set FireData_Aggregated;
	weights=(1/(Decade_Var+0.01)^2);
run;


proc glm data=FireData_Aggregated;
	class Region;
	model Decade_Mean = Region Decade Decade2;
	weight weights;
	means Region / hovtest;
	output out=WRegOut r=residual p=yhat;
run;



/* SCATTER PLOT OF RESIDUALS VERSUS FITTED VALUES */

proc sgplot data=WRegOut;
	title "Weighted Residuals vs Predicted Values"
	scatter y=residual x=yhat;
run;
title;






