
##########################################################
#     Forrest Fire Data
#     Counts analysis

#     Jamie D. Riggs, 2017

#     Data are loaded in each section for convenience


##########################################################
#    Libraries
##########################################################

library('gdata')
library(dafs)
library('ggplot2')
library(nortest)
library(car)
library(gvlma)
library(COUNT)
library(pscl)
library(gamlss.tr)


(Path <- "~/Desktop/WD")    # replace with your desired path


##########################################################
#    Functions
##########################################################

FreqProp2 <- function(factor1, factor2, faclab1, faclab2, dump) {
	table.x <- as.data.frame(table(factor1,factor2),exclude=c(NA,dump))
	names(table.x)[1:2] <- c(faclab1,faclab2)
	prop <- table.x$Freq / sum(table.x$Freq)
	table.x <- data.frame(table.x, prop)
	sum.x <- colSums(table.x[,3:4])
	new.row <- table.x[1,]
	new.row[1:2] <- c("","Total")
	new.row[3:4] <- sum.x
	table.x <- rbind(table.x, new.row)
	}

##########################################################
#     Initialization 
##########################################################

Ex <- "CTS"
ver <- "Fire"

setwd(Path)    # string with the path name to your working data
WD <- getwd()

# read the fire data from the .xls file
 IW <- read.xls('trouet2010.xls',sheet=4)   # Inter-Mountain West
 NC <- read.xls('trouet2010.xls',sheet=3)   # Northern California
PNW <- read.xls('trouet2010.xls',sheet=2)   # Pacific Northwest
 SW <- read.xls('trouet2010.xls',sheet=5)   # Southwest


# set up data for analysis
  X <- IW
reg <- "IW"
A <- data.matrix(which( X > 1, arr.ind=T ))  # matrix of row and col with X=2 => ring=fire
B <- data.matrix((FreqProp2(A[,2],A[,1],"col","row"))[,1:3])
A <- data.matrix(B[B[,1]>1,])  # dump B[1,]=year

# Sum number of trees with rings = fire
Y <- data.matrix(aggregate(A[-nrow(A),3],list(r=A[-nrow(A),2]),sum)) # last row tot not wanted
A <- cbind(X[,1],Y[,2])
colnames(A) <- c("year","n")
z <- rep(reg,nrow(A))
Z <- data.frame(Region=reg,A)    # first time only


reg <- c("NC","PNW","SW")
for (reg in c("NC","PNW","SW")) {

switch(reg,
        NC = {X <- NC},
       PNW = {X <- PNW},
        SW = {X <- SW}
       )

A <- data.matrix(which( X > 1, arr.ind=T ))  # matrix of row and col with X=2 => ring=fire
B <- data.matrix((FreqProp2(A[,2],A[,1],"col","row"))[,1:3])
A <- data.matrix(B[B[,1]>1,])  # dump B[1,]=year

# Sum number of trees with rings = fire
Y <- data.matrix(aggregate(A[-nrow(A),3],list(r=A[-nrow(A),2]),sum)) # last row tot not wanted
A <- cbind(X[,1],Y[,2])
colnames(A) <- c("year","n")
z <- rep(reg,nrow(A))

Z <- rbind(Z,cbind(Region=z,A))    # use to append
}
summary(Z)
nrow(Z)


X <- data.frame(zone=Z[,1],year=as.numeric(Z[,2]),n=as.numeric(Z[,3]))
summary(X)
G <- as.data.frame(X)    # Hold original data

names(X)[2:3] <- c("yr","yr.n")
X$dec <- round(X$yr,-1)
summary(X)
nrow(X)
G <- as.data.frame(X)    # Hold original data

Y <- aggregate(X$yr.n,list(dec=X$dec),sum)
names(Y)[2] <- "dec.n"
summary(Y)
nrow(Y)
Z <- merge(X,Y,by="dec")
summary(Z)
nrow(Z)
X <- Z
rm(G,Y,Z)

outfile <- paste0(WD, "/", ver, ".RData")
save(X, file=outfile, ascii=FALSE)


##########################################################
#    Summary
##########################################################

 Ex <- "CTS"
ver <- "Fire";

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/",ver, ".RData"))
load(infile)

H <- X    # hold data
#X <- H    # restore data

summary(X)
nrow(X)
var(X)


##########################################################
#     Scatter plot and pairwise correlation
##########################################################

 Ex <- "CTS"
ver <- "Fire";

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)

A <- subset(X, select=c(dec,dec.n))
A$lndec.n <- log(A$dec.n+1)
summary(A)
var(A)

(main <- paste0("Fire Data Histograms, Scatter Plots, and Pairwise Correlations"))
	pairsDAFS(A,main=main,labels=c("Decade", "Decadal Counts", "log Decadal Counts"))


##########################################################
#     Histograms, Q-Q plots
##########################################################

  Ex <- "CTS"
 ver <- "Fire"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)


   y  <- log(X$dec.n)
(main <- paste0(ver, "-Climate log Count \n Normal Q-Q Plot"))
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
   gp <- ggplot(X, aes(sample=log(dec.n))) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

x <- X$dec.n
hist(x,breaks="FD",plot=F)
breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
bwidth <- breaks[2]-breaks[1]
    xl <- "Rings/Decade"
 (main <- paste0(ver, "-Climate \n Decadal Counts Histogram"))
    gp <- ggplot(X, aes(dec.n)) + 
          geom_histogram(aes(y=..density..),col='grey45',binwidth=bwidth) + 
          geom_density() +
          ggtitle(main) + 
          xlab(xl) + 
          ylab("Frequency") +
          theme(axis.text=element_text(size=16)) +
          theme(axis.title=element_text(size=20)) +
          theme(plot.title=element_text(size=24))
gp


##########################################################
#   Boxplots
##########################################################

 Ex <- "CTS"
ver <- "Fire"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)


  yl <- "Decadal Counts"
   x <- X$zone
  xl <- "Region"
(main <- paste("Fire-Climate \n",yl,"vs.",xl))
   gp <- ggplot(X, aes(x=as.factor(zone),y=dec.n)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ggtitle(main) + 
         xlab(xl) + #scale_x_discrete(labels=c("level1",. . . )) +
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

  yl <- "Log Decadal Counts"
   x <- X$zone
  xl <- "Region"
(main <- paste("Fire-Climate \n",yl,"vs.",xl))
   gp <- ggplot(X, aes(x=as.factor(zone),y=log(dec.n+1))) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ggtitle(main) + 
         xlab(xl) + #scale_x_discrete(labels=c("level1",. . . )) +
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


##########################################################
#    ZipTest: are the data zero-inflated?
##########################################################

 Ex <- "CTS"
ver <- "Fire"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)


(muhat <- mean(X$dec.n))                 # mean of dec.n distribution
(  Var <- var(X$dec.n))                  # variance of dec.n >> mean => overdispersion
(   N0 <- length(X$dec.n[X$dec.n==0]))   # number of zeros in dec.n
(   P0 <- ppois(0, muhat))               # probability of dec.n = 0
(  Nap <- P0 * nrow(X))                  # espected nummber of zeros, inflated if Nap << N0


##########################################################
     part <- "NLR"   # Normal Linear Regression (NLR) without Southwest region
##########################################################

  Ex <- "CTS"
 ver <- "Fire"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/Fire", ".RData"))
load(infile)

H <- X
#X<- H
X$dec.c <- X$dec - mean(X$dec)   # center decade
X$dec.c2 <- X$dec.c*X$dec.c
X <- X[!(X$zone %in% "SW"),]     # without SW
X$y <- log(X$dec.n+1)

# model names so information may be used for model comparisons
mn <- "m.gm"
m.gm <- lm(y ~ zone + dec.c + zone:dec.c + dec.c2, data=X)
m <- m.gm
ms <- "ms.gm"
(ms <- summary(m))

# Andersen-Darling test of residuals for normal distribution
ad.test(residuals(m))


# subset the data for plots
A <- data.frame(Residuals=resid(m),Fitted=m$fitted.values,Observed=X$dec.n)

    y <- A$Residuals
(main <- paste("Fire-Climate", part, "Residuals \n Normal Q-Q Plot"))
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
   gp <- ggplot(A, aes(sample=Residuals)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Predicted Values"
   yl <- "Residuals"
(main <- paste("Fire-Climate", part, "Model \n Residuals by Fitted Values"))
   gp <- ggplot(A, aes(x=Fitted,y=Residuals)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Observed Values"
   yl <- "Fitted"
(main <- paste("Fire-Climate", part, "Model \n Fitted by Observed Values"))
   gp <- ggplot(A, aes(x=Observed,y=Fitted)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


# Evaluate homoscedasticity
# non-constant error variance test
(ncv <- ncvTest(m))

# Global test of model assumptions
gv.m <- gvlma(m)
summary(gv.m)

summary(X)
# 3rd quartile values
(x <- 1140 - mean(X$dec))
(w <- c("IW","NC","PNW"))
newPredictors <- as.data.frame(expand.grid(zone=w,dec.c=x,dec.c2=(x)^2))
predicted_values <- as.data.frame(cbind(newPredictors,predicted=predict.lm(m,newdata=newPredictors,type="response")))
predicted_values
exp(predicted_values$predicted) - 1


##########################################################
     part <- "Poisson"
##########################################################

library(boot)

 Ex <- "CTS"
ver <- "Fire"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)


(var(X$dec.n) / mean(X$dec.n) )   # ratio = 1 => no overdispersion

X$dec.c <- X$dec - mean(X$dec)     # center decade

poi <- glm(dec.n ~ zone + dec.c, family=poisson, data=X)
  m <- poi
(ps <- summary(poi))
 ms <- ps
exp(coef(m))


# model diagnostics
(     D <- ms$null.deviance - ms$deviance)
(   dfr <- ms$df.null - ms$df.residual)
( Dstat <- D / dfr)
(chisq3 <- qchisq(0.95, dfr))
( pval3 <- 1 - pchisq(Dstat, dfr))

( PX2 <- sum(residuals(m, type="pearson")^2))
(disp <- PX2 / m$df.residual)


# subset the data for plots
A <- data.frame(Residuals=resid(m),Fitted=m$fitted.values,Observed=X$dec.n)

    y <- A$Residuals
(main <- paste("Fire-Climate", part, "Residuals \n Normal Q-Q Plot"))
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
   gp <- ggplot(A, aes(sample=Residuals)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Predicted Values"
   yl <- "Residuals"
(main <- paste("Fire-Climate", part, "Model \n Residuals by Fitted Values"))
   gp <- ggplot(A, aes(x=Fitted,y=Residuals)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Observed Values"
   yl <- "Fitted"
(main <- paste("Fire-Climate", part, "Model \n Fitted by Observed Values"))
   gp <- ggplot(A, aes(x=Observed,y=Fitted)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


# Global test of model assumptions
gv.m <- gvlma(m)
summary(gv.m)

# no predictions as model inqdequate


##########################################################
     part <- "Quasi-Poisson"
##########################################################

 Ex <- "CTS"
ver <- "Fire"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)


X$dec.c <- X$dec - mean(X$dec)     # center decade

poiq <- glm(dec.n ~ zone + dec.c, family=quasipoisson, data=X)
   m <- poiq
(pqs <- summary(poiq))
  ms <- pqs
exp(coef(m))


# model diagnostics
(     D <- ms$null.deviance - ms$deviance)
(   dfr <- ms$df.null - ms$df.residual)
( Dstat <- D / dfr)
(chisq3 <- qchisq(0.95, dfr))
( pval3 <- 1 - pchisq(Dstat, dfr))

( PX2 <- sum(residuals(m, type="pearson")^2))
(disp <- PX2 / m$df.residual)


# subset the data for plots
A <- data.frame(Residuals=resid(m),Fitted=m$fitted.values,Observed=X$dec.n)

    y <- A$Residuals
(main <- paste("Fire-Climate", part, "Residuals \n Normal Q-Q Plot"))
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
   gp <- ggplot(A, aes(sample=Residuals)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Predicted Values"
   yl <- "Residuals"
(main <- paste("Fire-Climate", part, "Model \n Residuals by Fitted Values"))
   gp <- ggplot(A, aes(x=Fitted,y=Residuals)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Observed Values"
   yl <- "Fitted"
(main <- paste("Fire-Climate", part, "Model \n Fitted by Observed Values"))
   gp <- ggplot(A, aes(x=Observed,y=Fitted)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


# Global test of model assumptions
gv.m <- gvlma(m)
summary(gv.m)

# no predictions as model inqdequate


##########################################################
     part <- "NB2"   # two-parameter negative binomial
##########################################################

 Ex <- "CTS"
ver <- "Fire"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)


X$dec.c <- X$dec - mean(X$dec)     # center decade

nb2 <- glm.nb(dec.n ~ zone + dec.c, data=X)
  m <- nb2
#nb2r <- nb2
(nb2s <- summary(m))
   ms <- nb2s
exp(coef(m))


# model diagnostics
(     D <- ms$null.deviance - ms$deviance)
(   dfr <- ms$df.null - ms$df.residual)
( Dstat <- D / dfr)
(chisq3 <- qchisq(0.95, dfr))
( pval3 <- 1 - pchisq(Dstat, dfr))

(disp <- 1 / m$theta)   # dispersion parameter, ~1 => equidispersion


# subset the data for plots
A <- data.frame(Residuals=resid(m),Fitted=m$fitted.values,Observed=X$dec.n)

    y <- A$Residuals
(main <- paste("Fire-Climate", part, "Residuals \n Normal Q-Q Plot"))
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
   gp <- ggplot(A, aes(sample=Residuals)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Predicted Values"
   yl <- "Residuals"
(main <- paste("Fire-Climate", part, "Model \n Residuals by Fitted Values"))
   gp <- ggplot(A, aes(x=Fitted,y=Residuals)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Observed Values"
   yl <- "Fitted"
(main <- paste("Fire-Climate", part, "Model \n Fitted by Observed Values"))
   gp <- ggplot(A, aes(x=Observed,y=Fitted)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


# no predictions as model inqdequate


##########################################################
     part <- "NBH"  # hetergeneous NB
##########################################################

 Ex <- "CTS"
ver <- "Fire"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)


X$dec.c <- X$dec - mean(X$dec)     # center decade

 nbh <- nbinomial(dec.n ~ zone + dec.c, formula2 =~ zone + dec.c, family = "negBinomial", scale.link = "log_s", data=X)
    m <- nbh
(nbhs <- summary(nbh))
   ms <- nbhs
exp(coef(m))


# model diagnostics
(     D <- ms$null.deviance - ms$deviance)
(   dfr <- ms$df.null - ms$df.residual)
( Dstat <- D / dfr)
(chisq3 <- qchisq(0.95, dfr))
( pval3 <- 1 - pchisq(Dstat, dfr))

(disp <- ms$dispersion)    # dispersion parameter


# subset the data for plots
A <- data.frame(Residuals=resid(m)[1:m$obs],Fitted=fitted(m)[1:m$obs],Observed=X$dec.n)

    y <- A$Residuals
(main <- paste("Fire-Climate", part, "Residuals \n Normal Q-Q Plot"))
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
   gp <- ggplot(A, aes(sample=Residuals)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Predicted Values"
   yl <- "Residuals"
(main <- paste("Fire-Climate", part, "Model \n Residuals by Fitted Values"))
   gp <- ggplot(A, aes(x=Fitted,y=Residuals)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Observed Values"
   yl <- "Fitted"
(main <- paste("Fire-Climate", part, "Model \n Fitted by Observed Values"))
   gp <- ggplot(A, aes(x=Observed,y=Fitted)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


# no predictions as model inqdequate


##########################################################
     part <- "ZINB"  # zero-inflated NB
##########################################################

 Ex <- "CTS"
ver <- "Fire"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)


X$dec.c <- X$dec - mean(X$dec)     # center decade

  zinb <- zeroinfl(dec.n ~ zone + dec.c | zone + dec.c, dist = "negbin", data=X)
     m <- zinb
(zinbs <- summary(zinb))
    ms <- zinbs
vuong(zinb,nb2)
exp(coef(zinb))


# model diagnostics
(     D <- ms$null.deviance - ms$deviance)
(   dfr <- ms$df.null - ms$df.residual)
( Dstat <- D / dfr)
(chisq3 <- qchisq(0.95, dfr))
( pval3 <- 1 - pchisq(Dstat, dfr))

(disp <- 1/ms$theta)   # dispersion parameter


# subset the data for plots
A <- data.frame(Residuals=resid(m),Fitted=m$fitted.values,Observed=X$dec.n)

    y <- A$Residuals
(main <- paste("Fire-Climate", part, "Residuals \n Normal Q-Q Plot"))
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
   gp <- ggplot(A, aes(sample=Residuals)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Predicted Values"
   yl <- "Residuals"
(main <- paste("Fire-Climate", part, "Model \n Residuals by Fitted Values"))
   gp <- ggplot(A, aes(x=Fitted,y=Residuals)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Observed Values"
   yl <- "Fitted"
(main <- paste("Fire-Climate", part, "Model \n Fitted by Observed Values"))
   gp <- ggplot(A, aes(x=Observed,y=Fitted)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


# no predictions as model inqdequate


##########################################################
     part <- "HNBL"   # NB hurdle model
##########################################################

 Ex <- "CTS"
ver <- "Fire"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)

X$dec.c <- X$dec - mean(X$dec)           # center decade
X$dec.n <- ifelse(X$dec.n<2,0,X$dec.n)   # hudle 0 and 1

  hnbl <- hurdle(dec.n ~ zone + dec.c, dist = "negbin", zero.dist="binomial", link="logit", data=X)
     m <- hnbl
(hnbls <- summary(hnbl))
vuong(hnbl,zinb)
exp(coef(hnbl))


# subset the data for plots
A <- data.frame(Residuals=resid(m),Fitted=m$fitted.values,Observed=X$dec.n)

    y <- A$Residuals
(main <- paste("Fire-Climate", part, "Residuals \n Normal Q-Q Plot"))
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
   gp <- ggplot(A, aes(sample=Residuals)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Predicted Values"
   yl <- "Residuals"
(main <- paste("Fire-Climate", part, "Model \n Residuals by Fitted Values"))
   gp <- ggplot(A, aes(x=Fitted,y=Residuals)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   xl <- "Observed Values"
   yl <- "Fitted"
(main <- paste("Fire-Climate", part, "Model \n Fitted by Observed Values"))
   gp <- ggplot(A, aes(x=Observed,y=Fitted)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


# no predictions as model inqdequate


##########################################################
