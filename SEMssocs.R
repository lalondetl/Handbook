
##########################################################
#     SSOCS07 data
#     SEM analysis

#     Jamie D. Riggs, 2017

#     Data are loaded in each section for convenience


##########################################################
#    Libraries
##########################################################

library('sas7bdat')
library(reshape)
library('dafs')
library(gamlss)
library(ggplot2)
library(lavaan)
library(semPlot)

(Path <- "~/Desktop/WD")    # replace with your desired path


##########################################################
#     Initialization 
##########################################################

 Ex <- "SEM"
ver <- "SSOCS"

setwd(Path)    # string with the path name to your working data
WD <- getwd()

X <- read.sas7bdat(paste0(WD,"/", ver, "07_Data.sas7bdat"))

X <- rename(X,c("C0514"="suspensions","C0134"="uniforms","C0116"="metal.detectors","C0188"="tipline","C0178"="counseling","C0562"="crime","C0268"="discipline_training","C0276"="behavioral_training","C0508"="insubordination","C0510"="insubordination_removal","C0526"="English","C0532"="below.15th"))

X$uniforms <- ifelse((X$uniforms==2),0,1)
X$metal.detectors <- ifelse((X$metal.detectors==2),0,1)
X$tipline <- ifelse((X$tipline==2),0,1)
X$counseling <- ifelse((X$counseling==2),0,1)
X$crime <- 4 - X$crime

X <- X[,c("metal.detectors", "uniforms", "counseling", "tipline", "suspensions", "English", "below.15th", "crime")]

(outfile <- paste0(WD, "/", ver, ".RData"))
save(X, file=outfile, ascii=FALSE)


##########################################################
#     Data summary
##########################################################

 Ex <- "SEM"
ver <- "SSOCS"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver ".RData"))
load(infile)

summary(X)
nrow(X)


##########################################################
#     Scatter plot and pairwise correlation
##########################################################

 Ex <- "SEM"
ver <- "SSOCS"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver ".RData"))
load(infile)

A <- subset(X, select=c(suspensions:below.15th))
summary(A)
(main <- paste0("Histograms, Scatter Plots, and Pairwise Correlations"))
pairsDAFS(A, main=main, labels=names(A))


#  Construct a ZINB or hurdle model (Vuon tests the same) for suspensions

  zinb <- gamlss(suspensions ~ 1, sigma.fo=~-1, family = "ZIPIG", data=X)
     m <- zinb
(zinbs <- summary(zinb))
    ms <- zinbs
exp(coef(zinb))

# Normality of Residuals
# qq plot for studentized resid
    y <- resid(m)
 part <- "ZIPIG"
    y <- resid(m)
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
(main <- paste(ver, part, "Model Residuals","\n Normal Q-Q Plot"))
   gp <- ggplot(X, aes(sample=y)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

X$susres <- resid(m)    # save better behaved suspensions as ZINB residuals

(outfile <- paste0(WD, "/", ver, ".RData"))
save(X, file=outfile, ascii=FALSE)


##########################################################
#     Q-Q plots
##########################################################

 Ex <- "SEM"
ver <- "SSOCS"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver ".RData"))
load(infile)

#=========================
 part <- "QQ"
 xvar <- "Suspensions"
    y <- X$suspensions
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
(main <- paste(Ex, ver, xvar, "\n Normal", part, "Plot"))
   gp <- ggplot(X, aes(sample=y)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

  ilk <- "ZIPIG"
    y <- X$susres
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
(main <- paste(Ex, ver, ilk, xvar, "\n Normal", part, "Plot"))
   gp <- ggplot(X, aes(sample=y)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

xvar <- "English"
 ilk <- paste0("log",xvar)
   y <- log(X$English+0.1)
   v <- quantile(y[!is.na(y)], c(0.25, 0.75))
   h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
(main <- paste(Ex, ver, ilk, "\n Normal", part, "Plot"))
   gp <- ggplot(X, aes(sample=y)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

xvar <- "Below15"
 ilk <- paste0("log", xvar)
   y <- log(X$below.15th+0.1)
   v <- quantile(y[!is.na(y)], c(0.25, 0.75))
   h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
(main <- paste(Ex, ver, ilk, "\n Normal", part, "Plot"))
   gp <- ggplot(X, aes(sample=y)) + 
         stat_qq(col='grey45') +
         geom_abline(slope = slope, intercept = int) + 
         ggtitle(main) + 
         xlab("Theoretical Quantiles") + 
         ylab("Observed Quantiles") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


##########################################################
#     LVM"
##########################################################

 Ex <- "SEM"
ver <- "SSOCS"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver ".RData"))
load(infile)

# set up data and then generate the raw data correlation matrix
M <- X[,c(1:4,6:9)]
summary(M)
nrow(M)
(A <- round(cor(M),4))
A[upper.tri(A)] <- NA
(A <- data.frame(A))
(B <- sapply(A, as.character))
B[is.na(B)] <- " "
(raw <- as.data.frame(B,row.names=rownames(A)))


#  Define latent variables
  
m0 <- '
C =~ a*metal.detectors + b*tipline + c*crime + d*susres
A =~ e*uniforms + f*counseling + g*English + h*below.15th
A ~ k*C
'
mm <- "m0"

# fit model
m0.fit <- cfa(model=m0, data=M, ordered=c("metal.detectors","tipline", "uniforms", "counseling", "crime"), std.lv=TRUE)

# examine parameter estimates
(sm <- summary(m0.fit,standardized=TRUE))
(pe <- parameterEstimates(m0.fit,standardized=TRUE))

# path diagram
(main <- paste(ver, mm, "Path Analysis."))
semPaths(m0.fit, title=F, edge.label.cex = 0.5, curvePivot = TRUE, intercepts=T)
title(main, line = 3)

# Communalities
(com <- pe$std.all[1:9]*pe$std.all[1:9])
(unq <- 1 - com)
(lds <- data.frame(IV=pe$rhs[1:9], Loading=pe$std.all[1:9], Communality=com, Uniqueness=unq))

# check model
# model-implied covariances
fitted(m0.fit)

# transform model-implied covariances to correlations
(m0Fit.cov <- fitted(m0.fit)$cov)
(A <- as.data.frame(round(cov2cor(m0Fit.cov),4)))
A[upper.tri(A)] <- NA
(A <- data.frame(A))
(B <- sapply(A, as.character))
B[is.na(B)] <- " "
(m0.cor <- as.data.frame(B,row.names=rownames(A)))

# residual correlations
(A <- as.data.frame(round(residuals(m0.fit,type="cor")$cor,4)))
A[upper.tri(A)] <- NA
(A <- data.frame(A))
(B <- sapply(A, as.character))
B[is.na(B)] <- " "
(res.cor <- as.data.frame(B,row.names=rownames(A)))

# measures of model fit (Chi-squared, RMSEA), CFI), and SRMR)
fM <- round(fitMeasures(m0.fit)[c(1:2,6:9,25,36,39)],4)
fMdf <-data.frame(Statistic=attr(fM[c(1:3,6:8)],"names"), Estimate=fM[c(1:3,6:8)], df=c(NA,NA,fM[4],NA,NA,NA), pvalue=c(NA,NA,fM[5],NA,NA,fM[9]), row.names=NULL )
fMdf <- sapply(fMdf, as.character)
fMdf[is.na(fMdf)] <- " "
(fMdf <- as.data.frame(fMdf))


##########################################################

