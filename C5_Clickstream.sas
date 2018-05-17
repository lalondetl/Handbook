/* SAS ANALYSIS FOR CHAPTER 5: LOGISTIC REGRESSION */
/*	WIKIPEDIA CLICKSTREAM DATA		*/






/* READ IN 65476 ROWS FROM THE FILE */
/* PREDICTION FOR 3-HOUR BLOCKS */

data clickData;
	infile "PATH/2015_02_clickstream.tsv";
	input;
run;
	

clickData = read.table("",header=TRUE,sep="\t",na.strings="",fill=TRUE,quote="\"",nrows=65476)


data clickData;
	set clickData(obs=65476);
run;





/* SIMPLIFY PREVIOUS TITLE */

data clickData;
	set clickData;
	previous_title = prev_title;
	if (prev_title="other-google") then previous_title="Google";
	if (prev_title="other-empty") then previous_title="empty";
	if (prev_title="other-wikipedia") then previous_title="Wikipedia";
	if (prev_title="other-bing") then previous_title="Bing";
	if (prev_title="other-yahoo") then previous_title="Yahoo";
	if (prev_title="Main_Page") then previous_title="Main_Page";
run;





/* DEFINE BINARY REDLINK INDICATOR */

data clickData;
	set clickData;
	redlink = 0;
	if (type="redlink") then redlink=1;
run;






/* DESCRIPTIVES: CONTINGENCY TABLE OF REDLINK BY PREVIOUS TITLE */

proc freq data=clickData;
	tables redlink*previous_title;
run;






/* SCATTER PLOT OF REDLINK VERSUS PREVIOUS TITLE */

proc sgplot data=clickData;
	title "Scatter Plot of Redlink by Previous Titles";
	scatter y=redlink x=previous_title;
run;
title;







/* LOGISTIC REGRESSION MODEL FOR PROBABILITY OF A REDLINK */
/* HOSMER-LEMESHOW TEST */
/* ROC CURVE AND AREA */
/* ODDS RATIOS AND CONFIDENCE INTERVALS */

proc logistic data=clickData descending plots(only)=(roc(id=obs) effect);
	class previous_title(ref="other");
	model redlink = previous_title / lackfit clodds;
	weight n;
	output out=LROut r=residual p=yhat;
run;





/* STANDARDIZED DEVIANCE RESIDUALS */

proc univariate data=LROut;
	var residual;
run;




/* SCATTER PLOT OF DEVIANCE RESIDUALS VERSUS FITTED VALUES */

proc sgplot data=LROut;
	scatter y=residual x=yhat;
run;






/* SCATTER PLOT OF DEVIANCE RESIDUALS VERSUS PREVIOUS TITLE */

proc sgplot data=LROut;
	title "Scatter Plot of Residuals Versus Previous Titles";
	scatter y=residual x=previous_title;
run;
title;






/* PREDICTION: PREDICTED VALUES USING THE CURRENT SAMPLE */

proc univariate data=LROut;
	var yhat;
run;



