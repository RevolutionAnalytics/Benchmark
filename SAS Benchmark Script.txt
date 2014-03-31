/* SAS Benchmark Script */

/* Add library and environment references */
/* Script assumes library named BENCH /*
/* Analysis data in SAS data set named BENCH.ANALYSIS */
/* Prediction data in SAS data set named BENCH.PREDICT */

/* Run descriptive statistics */

PROC SURVEYMEANS DATA=BENCH.ANALYSIS NOBS MIN MAX MEAN STD;
	VAR F1;
RUN;

/* Compute median and deciles */

PROC SURVEYMEANS DATA=BENCH.ANALYSIS MEDIAN DECILES;
	VAR F1;
RUN;

/* Run frequency distribution */

PROC FREQ DATA=BENCH.ANALYSIS;
	TABLES T1;
RUN;

/* Linear regression 20 numeric variables */

PROC REG 
	DATA=BENCH.ANALYSIS 
	OUTEST=BENCH.MODEL1;
	MODEL N1=N2-N21;
RUN;
QUIT;

/* Linear regression 20 mixed variables */

PROC GLM
	DATA=BENCH.ANALYSIS;
	CLASS T1-T10;
	MODEL N1=N2-N11 T1-T10;
RUN;
QUIT;

/* Stepwise linear 100 numeric variables*/

PROC REG
	DATA=BENCH.ANALYSIS;
	MODEL N1=N2-N101/SELECTION=STEPWISE;
RUN;
QUIT;

/* Logistic regression */

PROC LOGISTIC
	DATA=BENCH.ANALYSIS;
	MODEL B1=N2-N21;
RUN;
QUIT;

/* Generalized linear model */

PROC GENMOD
	DATA=BENCH.ANALYSIS;
	MODEL N1=N2-N21/DIST=GAMMA;
RUN;

/* k-means clustering v=20 k=3 */

PROC FASTCLUS
	DATA=BENCH.ANALYSIS
	MAXCLUSTERS=3
	MAXITER=5;
	VAR N1-N20;
RUN;

/* k-means clustering v=100 k=5*/

PROC FASTCLUS
	DATA=BENCH.ANALYSIS
	MAXCLUSTERS=5
	MAXITER=5;
	VAR N1-N100;
RUN;

/* Score Prediction table and retain results */

PROC SCORE
	DATA = BENCH.PREDICT
	SCORE= BENCH.MODEL1
	OUT  = BENCH.SCORED 
	PREDICT nostd type=parms;
	VAR N1;
RUN;




