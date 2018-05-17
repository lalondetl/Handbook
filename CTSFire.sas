
* ########################################################## ;
*     Forrest Fire Data ;
*     Counts analysis ;

*     Jamie D. Riggs, 2017 ;

*     Data are loaded in each section for convenience ;


* ########################################################## ;
*     Initialization ;
* ########################################################## ;

Ex <- "CTS"
ver <- "Fire"

setwd(Path)    # string with the path name to your working data
WD <- getwd()


X = Fire ;


* ########################################################## ;
*    Summary ;
* ########################################################## ;

proc univariate data=X noprint;
	histogram ;
	run ;
	
	
	
* ########################################################## ;
     part = "Poisson"
* ########################################################## ;

proc genmod data=X;
	model dec.n = zone + dec.c / dist=poisson;
	run;



* ########################################################## ;
     part = "Quasi-Poisson"
* ########################################################## ;

proc glimmix data = X ;
	model majordrg = age acadmos minordrg ownrent / link = log solution;
	_variance_ = _mu_;
	run;



* ########################################################## ;
     part = "NB2"   # two-parameter negative binomial
* ########################################################## ;

/* Fitting the NB distribution now  */

/*  The scale adjustment works fine but are still not efficient */
/*  The NB model is another alternative */
/*  Here we add an error term to the poisson regression model to account for the subject to subject heterogeneity */
/*  The results are very similar to the ones obtained via the scale option */

proc genmod data=X;
	model arts = age mar doc ag und / d=nb;
	run;
proc genmod data=X;
	model cits = age mar doc ag und / d=nb;
	run;
