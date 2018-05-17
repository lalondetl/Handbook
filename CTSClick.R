
##########################################################
#     Clickstream data 
#     Counts analysis

#     Jamie D. Riggs, 2017

#     Data are loaded in each section for convenience


##########################################################
#    Libraries
##########################################################

library(ggplot2)
library(nortest)
library(car)
library(gvlma)
library(plyr)
library(gamlss.tr)
library(gamlss)


(Path <- "~/Desktop/WD")    # replace with your desired path


##########################################################
#    Functions
##########################################################

FreqProp <- function(factor, factorLabel, dump) {# factor <- X$type; factorLabel <- "Type"
	table.x <- as.data.frame(table(factor),exclude=c(NA,dump))
	names(table.x)[1] <- c(factorLabel)
	prop <- table.x$Freq / sum(table.x$Freq)
	table.x <- data.frame(table.x, prop)
	sum.x <- colSums(table.x[,2:3])
	new.row <- table.x[1,]
	new.row[1] <- "Total"
	new.row[2:3] <- sum.x
	table.x <- rbind(table.x, new.row)
	}
  
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

setwd(Path)    # set your working data
(WD <- getwd())

fn <- "ClickStream"

(infile <- paste0(WD, "/", fn, ".csv"))
      X <- data.frame(read.delim(infile, header=TRUE, sep="\t", na.strings="", fill=TRUE,quote="\"", nrows=65476))    # avg 3 hrs


names(X)
H <- as.data.frame(X)    # Hold original data
# X <- H    # restore original data if necessary

X <- H[,c(3:4,6)]  # use only the count, type of link, and previous title
Y <- na.omit(X)    # remove missing data records
names(Y)
X <- Y[Y$type %in% c("link","other","redlink"),]
X$type <- factor(X$type)
nrow(X)
table(X$type)
rm(Y)

# SIMPLIFY PREVIOUS TITLE 
previous_title <- rep("other", nrow(X))
for(i in 1:nrow(X))
{
	if(X$prev_title[i] == "other-google"){previous_title[i]="Google"}
	if(X$prev_title[i] == "other-empty"){previous_title[i]="empty"}
	if(X$prev_title[i] == "other-wikipedia"){previous_title[i]="Wikipedia"}
	if(X$prev_title[i] == "other-bing"){previous_title[i]="Bing"}
	if(X$prev_title[i] == "other-yahoo"){previous_title[i]="Yahoo"}
	if(X$prev_title[i] == "Main_Page"){previous_title[i]="Main_Page"}
}

X$prev.title <- as.factor(previous_title)
names(X)
X <- X[,-2]
str(X)
table(X$prev.title)

# save reduced data set
outfile <- paste0(WD, "/CS65k", ".RData")
save(X, file=outfile, ascii=FALSE)


##########################################################
#     Summary
##########################################################

 Ex <- "CTS"
ver <- "CS65k"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)

summary(X)
var(X$n)
nrow(X)


##########################################################
#     Histograms, Q-Q plots
##########################################################

 Ex <- "CTS"
ver <- "CS65k"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)


   y  <- X$n
(main <- paste("EW Clickstream Pairings Count \n Normal Q-Q Plot"))
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
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

    y <- log(X$n)
(main <- paste("EW Clickstream Pairings log Count \n Normal Q-Q Plot"))
    v <- quantile(y[!is.na(y)], c(0.25, 0.75))
    h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
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

       x <- X$n
       h <- hist(x,breaks="FD",plot=F)
h$breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
h$bwidth <- breaks[2]-breaks[1]
    xl <- "Pairings count (<1000)"
 (main <- paste0("EW Clickstream Pairings \n Count Histogram"))
    gp <- ggplot(X[X$n<1000,], aes(n)) + 
          geom_histogram(aes(y=..density..),col='grey45',binwidth=h$bwidth) + 
          geom_density() +
          ggtitle(main) + 
          xlab(xl) + 
          ylab("Frequency") +
          theme(axis.text=element_text(size=16)) +
          theme(axis.title=element_text(size=20)) +
          theme(plot.title=element_text(size=24))
gp

       x <- log(X$n)
       h <- hist(x,breaks="FD",plot=F)
h$breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
h$bwidth <- breaks[2]-breaks[1]
      xl <- "Pairings log count"
   (main <- paste0("EW Clickstream Pairings \n log Count Histogram"))
      gp <- ggplot(X, aes(log(n))) + 
            geom_histogram(aes(y=..density..),col='grey45',binwidth=h$bwidth) + 
            geom_density() +
            ggtitle(main) + 
            xlab(xl) + 
            ylab("Frequency") +
            theme(axis.text=element_text(size=16)) +
            theme(axis.title=element_text(size=20)) +
            theme(plot.title=element_text(size=24))
gp


##########################################################
#     Boxplots
##########################################################

Ex <- "CTS"
ver <- "CS65k"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)


   yl <- "Pairings Counts"
   xl <- "Previous Title"
(main <- paste(yl,"vs.",xl))
   gp <- ggplot(X, aes(x=as.factor(prev.title),y=log(n))) +
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

   yl <- "Log Pairings Counts"
   xl <- "Type \n"
(main <- paste(yl,"vs. Type"))
   gp <- ggplot(X, aes(x=as.factor(type),y=log(n))) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


##########################################################
#     Tables
##########################################################

Ex <- "CTS"
ver <- "CS65k"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/CS10k", ".RData"))
load(infile)

(tab1 <- FreqProp(X$type,"Type"))
(tab2 <- FreqProp(X$prev.title,"Previous Title"))
(tab12 <- FreqProp2(X$type,X$prev.title,"Type","PrevTitle"))


##########################################################
   part <- "NLR"  #   Normal Linear Regression (NLR)
##########################################################

Ex <- "CTS"
ver <- "CS65k"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)

y <- log(X$n)    # min count 10 so translation not necessary
m <- "m.gm"
m.gm <- lm(y ~ type + prev.title, data=X)
m <- m.gm
ms <- "ms.gm"
summary(m)

# Andersen-Darling test of residuals for normal distribution
ad.test(residuals(m))


# subset the data for plots
A <- data.frame(Residuals=resid(m),Fitted=m$fitted.values,Observed=log(X$n))

    y <- A$Residuals
(main <- paste("EW Clickstream", part, "Residuals \n Normal Q-Q Plot"))
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
(main <- paste("EW Clickstream", part, "\n Model Residuals by Fitted Values"))
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
(main <- paste("EW Clickstream", part, "\n Model Fitted by Observed Values"))
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


##########################################################
     part <- "Truncated"
##########################################################

Ex <- "CTS"
ver <- "CS65k"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)


# For each subset of n, type, prev.title, by number of rows, combine into data frame.
X <- ddply(X,.(n, type, prev.title), nrow)


##########################################################
# truncated Poisson model

ilk <- "Poisson"
gen.trun(9,family="PO","left")
con1 <- gamlss.control(c.crit=0.5)   # the convergence criterion
 tpo <- gamlss(n ~ type + prev.title, weights=V1, family=paste0(mn,"left"), data=X, control=con1)
   m <- tpo
summary(m)

# subset the data for plots
A <- data.frame(Residuals=m$residuals,Fitted=exp(lpred(m)),Observed=X$n)

    y <- A$Residuals
(main <- paste("EW Clickstreame", part, ilk, "Residuals \n Normal Q-Q Plot"))
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
         theme(plot.title=element_text(size=20))
gp

   xl <- "Predicted Values"
   yl <- "Residuals"
(main <- paste("EW Clickstream", part, "\n Model Residuals by Fitted Values"))
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
(main <- paste("EW Clickstream", part, "\n Model Fitted by Observed Values"))
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


# Examine residuals time series acf and pacf
y <- X$n
length(which(y<0))       # number of fitted counts below zero
length(which(fitted(m,"mu")<10)) / nrow(h)
acf(diff(resid(m)), lag.max=30, ci=0.99, plot=T)   # MA(1)
pacf(resid(m), lag.max=30, ci=0.99, plot=T)


##########################################################
# truncated Negative Binomial model

ilk <- "NBII"
gen.trun(9,"NBII","left")
con1 <- gamlss.control(c.crit=0.5)
 tnb <- gamlss(n ~ type + prev.title, weights=V1, family= paste0(mn,"left"), data=X, control=con1)
   m <- tnb
summary(m)


# subset the data for plots
A <- data.frame(Residuals=m$residuals,Fitted=exp(lpred(m)),Observed=X$n)

    y <- A$Residuals
(main <- paste("EW Clickstreame", part, ilk, "Residuals \n Normal Q-Q Plot"))
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
         theme(plot.title=element_text(size=20))
gp

   xl <- "Predicted Values"
   yl <- "Residuals"
(main <- paste("EW Clickstream", part, "\n Model Residuals by Fitted Values"))
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
(main <- paste("EW Clickstream", part, "\n Model Fitted by Observed Values"))
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


# Examine predicted values of the model to the original data
y <- X$n
length(which(y<0))       # number of fitted counts below zero
length(which(fitted(m,"mu")<10)) / nrow(h)
acf(diff(resid(m)), lag.max=30, ci=0.99, plot=T)   # MA(1)
pacf(resid(m), lag.max=30, ci=0.99, plot=T)


##########################################################
# truncated Delaporte model

ilk <- "Delaporte"
gen.trun(9,"DEL","left")
con1 <- gamlss.control(c.crit=0.5)
   m <- gamlss(n ~ type + prev.title, weights=V1, family= paste0(mn,"left"), data=X, control=con1)
summary(m)


# subset the data for plots
A <- data.frame(Residuals=m$residuals,Fitted=exp(lpred(m)),Observed=X$n)

    y <- A$Residuals
(main <- paste("EW Clickstreame", part, ilk, "Residuals \n Normal Q-Q Plot"))
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
         theme(plot.title=element_text(size=20))
gp

   xl <- "Predicted Values"
   yl <- "Residuals"
(main <- paste("EW Clickstream", part, ilk, "\n Model Residuals by Fitted Values"))
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
(main <- paste("EW Clickstream", part, ilk, "\n Model Fitted by Observed Values"))
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


# Examine predicted values of the model to the original data
y <- X$n
length(which(y<0))       # number of fitted counts below zero
length(which(fitted(m,"mu")<10)) / nrow(h)
acf(diff(resid(m)), lag.max=30, ci=0.99, plot=T)   # MA(1)
pacf(resid(m), lag.max=30, ci=0.99, plot=T)


##########################################################
# truncated Sichel 1 model
# May takes ~30 minutes or more to run (big data set).
# Watch for about 11 iterations (Sichel 1)

ilk <- "Weighted Sichel 1"
gen.trun(9,"SI","left")
con1 <- gamlss.control(c.crit=0.5)
 tsi <- gamlss(n ~ type + prev.title, weights=V1, family=paste0(mn,"left"), data=X, control=con1)
   m <- tsi
summary(m)


# subset the data for plots
A <- data.frame(Residuals=m$residuals,Fitted=exp(lpred(m)),Observed=X$n)

    y <- A$Residuals
(main <- paste("EW Clickstreame", part, ilk, "\n Residuals Normal Q-Q Plot"))
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
         theme(plot.title=element_text(size=20))
gp

   xl <- "Predicted Values"
   yl <- "Residuals"
(main <- paste("EW Clickstream", part, ilk, "\n Model Residuals by Fitted Values"))
   gp <- ggplot(A, aes(x=Fitted,y=Residuals)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20))
gp

   xl <- "Observed Values"
   yl <- "Fitted"
(main <- paste("EW Clickstream", part, ilk, "\n Model Fitted by Observed Values"))
   gp <- ggplot(A, aes(x=Observed,y=Fitted)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20))
gp


# Examine predicted values of the model to the original data
y <- X$n
length(which(y<0))       # number of fitted counts below zero
length(which(fitted(m,"mu")<10)) / nrow(h)
acf(diff(resid(m)), lag.max=30, ci=0.99, plot=T)   # MA(1)
pacf(resid(m), lag.max=30, ci=0.99, plot=T)


##########################################################
# truncated Sichel 2 model
# May takes ~30 minutes or more to run (big data set).
# Watch for about 11 iterations (Sichel 1)

ilk <- "Sichel 2"
gen.trun(9,"SICHEL","left")
   n <- X$n
con1 <- gamlss.control(c.crit=0.5)   # 6 iterations
tsic <- gamlss(n ~ type + prev.title, weights=V1, family=paste0(mn,"left"), data=X, control=con1)
    m <- tsic
summary(m)


# subset the data for plots
A <- data.frame(Residuals=m$residuals,Fitted=exp(lpred(m)),Observed=X$n)

    y <- A$Residuals
(main <- paste("EW Clickstreame", part, ilk, "\n Residuals Normal Q-Q Plot"))
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
         theme(plot.title=element_text(size=20))
gp

   xl <- "Predicted Values"
   yl <- "Residuals"
(main <- paste("EW Clickstream", part, ilk, "\n Model Residuals by Fitted Values"))
   gp <- ggplot(A, aes(x=Fitted,y=Residuals)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20))
gp

   xl <- "Observed Values"
   yl <- "Fitted"
(main <- paste("EW Clickstream", part, ilk, "\n Model Fitted by Observed Values"))
   gp <- ggplot(A, aes(x=Observed,y=Fitted)) +
         geom_point(col='grey45') + 
         geom_smooth(col='grey45') +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=20))
gp


# Examine predicted values of the model to the original data
y <- X$n
length(which(y<0))       # number of fitted counts below zero
length(which(fitted(m,"mu")<10)) / nrow(h)
acf(diff(resid(m)), lag.max=30, ci=0.99, plot=T)   # MA(1)
pacf(resid(m), lag.max=30, ci=0.99, plot=T)


# Likelihood ratio tests to compare models
LR.test(tpo,tnb)
LR.test(tnb,tsi)
LR.test(tsi,tsic)


##########################################################
