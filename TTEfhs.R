
##########################################################
#     Framingham Heart Study data 
#     Survival analysis

#     Jamie D. Riggs, 2017

#     Data are loaded in each section for convenience


##########################################################
#    Libraries
##########################################################

library(fBasics)
library(usdm)
library(ggplot2)
library(survival)
library(GGally)
library(nlme)
library(KMsurv)
library(km.ci)
library(reshape2)

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

Ex <- "TTE"
ver <- "FHS"

setwd(Path)    # set your working data
(WD <- getwd())

infile <- paste0(WD, "/", ver, ".csv")
     X <- data.frame(read.csv(infile, header=TRUE))


H <- as.data.frame(X)    # Hold original data
#X <- H

# All the necessary information for time to hypertension is in the period 1 record
# Exclude subjects with diagnosed hypertension prior to insertion into the study
# Remove cases with missing data
tte <- na.omit(X[X$PREVHYP=="0"&X$PERIOD==1,c(1:4,8,10,24,31,39)])
  X <- tte


##########################################################
#      Summmary
##########################################################

 Ex <- "TTE"
ver <- "FHS"

setwd(Path)
(WD <- getwd())

(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)

summary(X)
nrow(X)


summary(X[,c(3:5,9)])    # variables of interest

     A <- X[,3:5]
     n <- nrow(A)
 (mean <- colMeans(A))
 (std <- colSds(A))
 (skew <- skewness(A))
(tskew <- skew/(sqrt(6/n)))
   (ps <- 2 * (1 - pt(abs(skew/(sqrt(6/n))), n - 1)))
 (kurt <- kurtosis(A))
(tkurt <- kurt/(sqrt(24/n)))
   (pk <- 2 * (1 - pt(abs(kurt/(sqrt(24/n))), n - 1)))

(sM <- data.frame(Predictor=names(A), n=n, Mean=mean, sd=std, skew=skew, skew.z=tskew, skew.p=ps, kurtosis=kurt, kurt.z=tkurt, kurt.p=pk))


A[,1] <- log(A[,1])
names(A)[1] <- "lnTOTCHOL"
mahal <- mahalanobis(A, colMeans(A), var(A))    # squared Mahalanobis distance of all rows
   cM <- mahal[mahal>(qchisq(1-0.001, df=3))]   # rows with large Mahalanobis distance
(qchisq(1-0.001, df=3))                         # chi^2 discriminator
attr(cM, "names")
length(attr(cM,"names")) / nrow(A)              # fraction of large Mahalanobis distances
summary(A[attr(cM,"names"),])                   # values causing large Mahalanobis distances


#   iteratively test each excessive Mahalanobis case
for (i in 1:length(cM)) {# i <- 1  # test iteration
A$y <- 0
A$y[which(rownames(A)==attr(cM,"names")[i])] <- 1
mM <- lm(y ~ lnTOTCHOL + AGE + CIGPDAY, data=A)
cat("\n--------- Run ", attr(cM,"names")[i], "---------")
print(summary(mM))
}
#  Indicts log(TOTCHOL), age, cigs/day at alpha = 0.05 Opt to remove these 12 cases.

cases <- attr(cM,"names")
id <- as.numeric(cases)

Y <- X[!rownames(X) %in% cases,]    # remove cases causing large Mahalanobis distances
nrow(Y)
X <- Y
summary(X)
nrow(X)


# Check for multicolinearity
A <- X[,3:5]
A[,1] <- log(A[,1])
names(A)[1] <- "lnTOTCOL"
vif(A[,1:3])



outfile <- paste0(WD, "/", ver, ".RData")
save(X, file=outfile, ascii=FALSE)


##########################################################
#     Scatter plot and pairwise correlation
##########################################################

 Ex <- "TTE"
ver <- "FHS"

setwd(Path)
(WD <- getwd())

(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)
nrow(X)

A <- subset(X, select=c(TOTCHOL:CIGPDAY,TIMEHYP))
summary(A)
(main <- paste0("Histograms, Scatter Plots, and Pairwise Correlations"))
pairsDAFS(A, main=main, labels=names(A))


##########################################################
#     Normal Q-Q plots
##########################################################

 Ex <- "TTE"
ver <- "FHS"

setwd(Path)
(WD <- getwd())

(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)
nrow(X)

xvar <- "Cholesterol"
   y <- X$TOTCHOL
   v <- quantile(y[!is.na(y)], c(0.25, 0.75))
   h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
(main <- paste("FHS", xvar, "\n Normal Q-Q Plot"))
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

xvar <- "Cholesterol"
 ilk <- paste0("log", xvar)
   y <- log(X$TOTCHOL)
   v <- quantile(y[!is.na(y)], c(0.25, 0.75))
   h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
(main <- paste(Ex, ver, ilk, "\n Normal Q-Q Plot"))
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

xvar <- "Age"
   y <- X$AGE
   v <- quantile(y[!is.na(y)], c(0.25, 0.75))
   h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
(main <- paste(Ex, ver, xvar, "\n Normal Q-Q Plot"))
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

 ilk <- paste0("log", xvar)
   y <- log(X$AGE)
   v <- quantile(y[!is.na(y)], c(0.25, 0.75))
   h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
(main <- paste(Ex, ver, ilk, "\n Normal Q-Q Plot"))
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

xvar <- "Cigarettes"
 ilk <- xvar
   y <- X$CIGPDAY
   v <- quantile(y[!is.na(y)], c(0.25, 0.75))
   h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
(main <- paste(Ex, ver, xvar, "\n Normal Q-Q Plot"))
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

 ilk <- paste0("sqrt", xvar)
   y <- sqrt(X$CIGPDAY+0.1)
   v <- quantile(y[!is.na(y)], c(0.25, 0.75))
   h <- qnorm(c(0.25, 0.75))
slope <- diff(v)/diff(h)
  int <- v[1L] - slope * h[1L]
(main <- paste(Ex, ver, ilk, "\n Normal Q-Q Plot"))
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
#     Boxplots
##########################################################

 Ex <- "TTE"
ver <- "FHS"

setwd(Path)
(WD <- getwd())

(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)
nrow(X)

  y <- X$TIMEHYP
  x <- X$SEX
 xl <- "Sex"
 yl <- "Time to Hypertension"
ilk <-"T2HYP"
(main <- paste(yl,"vs",xl))
boxplot(y~x, notch=TRUE, main=, xlab=xl, ylab=yl, varwidth=F, col="coral")
grid(nx=NA,ny=NULL,col="black",lty="dotted")
(means <- by(y, x, base::mean))                      
points(1:length(means), means, pch = 23, cex = 1.5,bg = "blue")
text(1:length(means)+.2, means+500, 
     labels = formatC(means, format = "f", digits = 2),
     pos = 2, cex = 0.9, col = "blue4")

    y <- X$TIMEHYP
    x <- X$SEX
   xl <- "Sex"
   yl <- "Time to Hypertension"
(main <- paste(yl,"vs.",xl))
   gp <- ggplot(X, aes(x=as.factor(x),y=y)) + 
         geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
#         scale_y_continuous(trans=scales::log2_trans()) +
         stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


##########################################################
#     Tables
##########################################################

 Ex <- "TTE"
ver <- "FHS"

setwd(Path)
(WD <- getwd())

(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)
nrow(X)

(tSex <- FreqProp(X$SEX,"Sex"))
(tDia <- FreqProp(X$DIABETES,"Diabetes"))
(tHyp <- FreqProp(X$HYPERTEN,"Hypertension"))
(tDea <- FreqProp(X$DEATH,"Death"))

(tab12 <- FreqProp2(X$SEX,X$HYPERTEN,"Sex","Hypertension"))
(tab13 <- FreqProp2(X$SEX,X$DEATH,"Sex","Death"))
(tab14 <- FreqProp2(X$HYPERTEN,X$DEATH,"Hypertension","Death"))


##########################################################
#     Survival function
##########################################################

 Ex <- "TTE"
ver <- "FHS"

setwd(Path)
(WD <- getwd())

(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)
nrow(X)


(hyp.surv <- survfit(Surv(TIMEHYP, HYPERTEN) ~ 1, data=X))

(main <- "Hypertension Survival Function (95% CI)")
   xl <- "Time After Hypertensive Diagnosis (days)"
   yl <- "Survivial Proportion"
   gp <- ggsurv(hyp.surv,plot.cens=F,cens.col="black") +
         ylim(0.2,1) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

# survival funtion table
sum <- summary(hyp.surv)    # full table, very long
sdf <- data.frame(sum$time,sum$n.risk,sum$n.event,sum$n.censor,sum$surv,sum$std.err,sum$upper,sum$lower)
names(sdf) <- names(sum)[c(2:6,8:10)]

# first 3, mekkle 3, and last 3 entries in table
(A <- rbind(head(sdf,3),sdf[592:594,],tail(sdf,3)))
rm(sum)


##########################################################
#     Kaplan-Meier (KM)
##########################################################

 Ex <- "TTE"
ver <- "FHS"

setwd(Path)
(WD <- getwd())

(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)
nrow(X)


# construct KM table
     t6m <- floor(X$TIMEHYP/365)       # time in days to time in years
hyperten <- as.numeric(X$HYPERTEN)     # factor to numeric, 0 = no, 1 = yes
    tall <- data.frame(t6m, hyperten)  # matrix: time to hyptension (years) and have hypertension 
    tall <- tall[order(tall[,1]),]     # sort by time to hypertension
     die <- data.frame(nevent=table(tall)[,2])    # add with cases with hypertension by year
   total <- gsummary(tall, length, groups=t6m)    # number of cases by year
      (A <- cbind(die, total=total[,2]))
rm(t6m)

        t6m <- as.numeric(rownames(A))
         lt <- length(t6m)
  t6m[lt+1] <- NA
    A$nlost <- A$total - A$nevent
       m.lt <- lifetab(t6m, length(unique(X$RANDID)), A$nlost, A$nevent)
       m.lt <- cbind(year = rownames(m.lt), m.lt)
rownames(m.lt) <- NULL
(m.lt[,1:6])

   Y <- data.frame("x"=t6m[-(lt+1)], "y"=m.lt$surv)
  xl <- "Survival by Year"
  yl <- "Survival Proportion"
(main <- "Hypertension Survival Function \n from Life Table")
   gp <- ggplot() + 
        geom_step(data = Y, aes(x = x, y = y)) +
         ylim(0.2,1) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


##########################################################
#     KM ci
##########################################################

 Ex <- "TTE"
ver <- "FHS"

setwd(Path)
(WD <- getwd())

(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)
nrow(X)

    time <- X$TIMEHYP
  censor <- X$HYPERTEN
hyp.surv <- survfit(Surv(time, censor) ~ 1, conf.type="none")
       a <- km.ci(hyp.surv, tl=NA, tu=NA, method="hall-wellner")

y <- data.frame(time=a$time,surv=a$surv,lower=a$lower,upper=a$upper)
x <- melt(y, id.vars = "time", measure.vars = c("surv", "lower", "upper"), value.name = "surv", variable.name = "ltype")
levels(x$ltype) <- c("SF", "lcl95", "ucl95")
names(x)[3] <- "surv"
str(x)

   xl <- "Time After Hypertension (days)"
   yl <- "Survival Probability"
(main <- "Hypertension Survival Function \n Kaplan-Meier with 95% CI")
   gp <- ggplot() + 
         geom_step(data = x, aes(x = time, y = surv, linetype=ltype)) +
         ylim(0.2,1) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

    sex <- X$SEX
hyp.sex <- survfit(Surv(time, censor) ~ sex, conf.type="none")
     xl <- "Time After Hypertension (days)"
     yl <- "Survival Probability"
  (main <- "Survival Function for Hypertension by Sex")
     gp <- ggsurv(hyp.sex,plot.cens=F,surv.col="black",lty.est=1) +
           guides(linetype = F) +
          ylim(0.2,1) +
          ggtitle(main) + 
          xlab(xl) + 
          ylab(yl) +
          theme(axis.text=element_text(size=16)) +
          theme(axis.title=element_text(size=20)) +
          theme(plot.title=element_text(size=24))
gp

diabetes <- X$DIABETES
 hyp.dia <- survfit(Surv(time, censor) ~ diabetes, conf.type="none")
   (main <- "Hypertension Survival Probability by Diabetes")
      xl <- "Time After Hypertension (days)"
      yl <- "Survival Probability"
   (main <- "Survival Function for Hypertension \n by Diagnosis of Diabetes")
      gp <- ggsurv(hyp.dia,plot.cens=F,lty.est=1) +
            guides(linetype = F) +
            ylim(0.2,1) +
            ggtitle(main) + 
            xlab(xl) + 
            ylab(yl) +
            theme(axis.text=element_text(size=16)) +
            theme(axis.title=element_text(size=20)) +
            theme(plot.title=element_text(size=24))
gp

# cut age by decades
   time <- X$TIME
 censor <- X$HYPERTEN
age.cut <- cut(X$AGE, c(min(X$AGE), 39.4, 49.4, 59.4, max(X$AGE)))
hyp.age <- survfit(Surv(time, censor) ~ strata(age.cut), conf.type="log-log")
   brks <- na.omit(unique(age.cut)) factor(attr(hyp.age$strata,"names"))
     xl <- "Time After Hypertension (days)"
     yl <- "Survival Probability"
  (main <- "Survival Function for Hypertension \n by Age in Decades")
     gp <- ggsurv(hyp.age,plot.cens=F,lty.est=1) +
           guides(linetype = F) +
           ylim(0.2,1) +
           ggtitle(main) + 
           xlab(xl) + 
           ylab(yl) +
           theme(axis.text=element_text(size=16)) +
           theme(axis.title=element_text(size=20)) +
           theme(plot.title=element_text(size=24))
gp


##########################################################
#     Cox proportional hazard
##########################################################

 Ex <- "TTE"
ver <- "FHS"

setwd(Path)
(WD <- getwd())

(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)
nrow(X)


df <- with(X , aggregate(TIMEHYP, list(Sex=as.factor(SEX), Diabetes=as.factor(DIABETES)), mean))
names(df)[3] <- "Time"
   xl <- "Sex"
   yl <- "Time to Hypertension"
(main <- "Time to Hypertension by Sex and \n Diabetes Interactions")
   gp <- ggplot(df, aes(x=Sex, y=Time, group=Diabetes)) +
         geom_line(aes(linetype=Diabetes), size=.5) + 
         geom_point(aes(shape=Diabetes), size=1) + 
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

df <- with(X , aggregate(TIMEHYP, list(Sex=as.factor(SEX), Diabetes=as.factor(DIABETES)), mean))
names(df)[3] <- "Time"
   xl <- "Diabetes"
   yl <- "Time to Hypertension"
(main <- "Time to Hypertension by Sex and \n Diabetes Interactions")
   gp <- ggplot(df, aes(x=Diabetes, y=Time, group=Sex)) +
         geom_line(aes(linetype=Sex), size=.5) + 
         geom_point(aes(shape=Diabetes), size=1) + 
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

     Y <- X
Y$cigs <- ifelse(Y$CIGPDAY > 19,20,ifelse(Y$CIGPDAY > 0,10,0))
table(Y$cigs)
df <- with(Y , aggregate(TIMEHYP, list(Sex=as.factor(SEX), Cigarettes=as.factor(cigs)), mean))
names(df)[3] <- "Time"
   xl <- "Cigarettes"
   yl <- "Time to Hypertension"
(main <- "Time to Hypertension by Sex and \n Cigarettes Interactions")
   gp <- ggplot(df, aes(x=Cigarettes, y=Time, group=Sex)) +
         geom_line(aes(linetype=Sex), size=.5) + 
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


df <- with(Y , aggregate(TIMEHYP, list(Sex=as.factor(SEX), Cigarettes=as.factor(cigs)), mean))
names(df)[3] <- "Time"
   xl <- "Sex"
   yl <- "Time to Hypertension"
(main <- "Time to Hypertension by Sex and \n Cigarettes Interactions")
   gp <- ggplot(df, aes(x=Sex, y=Time, group=Cigarettes)) +
         geom_line(aes(linetype=Cigarettes), size=.5) + 
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


# Test for time dependent predictors
Y$lnTOTCHOL <- log(Y$TOTCHOL)
cox.T  <- coxph(Surv(time,HYPERTEN) ~ SEX + AGE + CIGPDAY + DIABETES + lnTOTCHOL, data=Y)
cox.Tz <- cox.zph(cox.T)
round(cox.Tz$table,4)

(main <- "Hypertension Predictor Parameter Estimates vs. Time")
	op <- par(mfrow=c(2,2), oma=c(0,0,2,0))
	plot(cox.Tz[1])
	abline(h=0, lty=3)
	grid(col="grey")
	plot(cox.Tz[2])
	abline(h=0, lty=3)
	grid(col="grey")
	plot(cox.Tz[4])
	abline(h=0, lty=3)
	grid(col="grey")
	plot(cox.Tz[5])
	abline(h=0, lty=3)
	grid(col="grey")
	title(main=main, outer=T)
	par(op)


#============================
# Sex and diabetes interaction plots
        sex <- X$SEX
   diabetes <- X$DIABETES
       cigs <- X$CIGPDAY
       time <- X$TIMEHYP
X$lnTOTCHOL <- log(X$TOTCHOL)
    X$years <- round(X$TIMEHYP / 365.25)
    yrs.cut <- sort(unique(X$years[X$HYPERTEN == 1]))
   hyp.surv <- Surv(time, X$HYPERTEN)~1
          Y <- relsurv::survsplit(data = X, cut = yrs.cut, end = "years", start = "time0", event = "HYPERTEN")
          Y <- Y[order(Y$RANDID),]


# Main effects only Cox model model
cox <- coxph(Surv(time0, years, HYPERTEN) ~ AGE + CIGPDAY + lnTOTCHOL + DIABETES + SEX + SEX:CIGPDAY + SEX:DIABETES, data=Y)
summary(cox)

   xl <- "Time After Hypertensive Diagnosis (years)"
   yl <- "Survivial Proportion"
(main <- "Cox Main Effects Baseline \n Hazard Curve (95% CI)")
   gp <- ggsurv(survfit(cox),plot.cens=F,cens.col="black") +
         ylim(0.2,1) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp

# Cox table
 covs <- data.frame(AGE=45, CIGPDAY=10, lnTOTCHOL=log(300), DIABETES=1, SEX=1)(scov <- summary(survfit(cox, newdata = covs, type = "aalen")))

   xl <- "Time After Hypertensive Diagnosis (years)"
   yl <- "Proportion survived"
    x <- scov$time
(main <- "Cox Main Effects Baseline (95% CI) \n Specific Case")
   gp <- ggsurv(scov,plot.cens=F,cens.col="black") +
         ylim(0.2,1) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         annotate("text", x = 15, y = .97, label = "Diabetic Male, Age 45 years") +
         annotate("text", x = 16.9, y = .94, label = "Cholesterol 300mg/dL, 10 Cigarettes/day") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


Y$lt.sex <- Y$SEX * log(Y$years)
Y$lt.age <- Y$AGE * log(Y$years)Y$lt.dia <- Y$DIABETES * log(Y$years)
Y$lt.tch <- Y$lnTOTCHOL * log(Y$years)
   cox.X <- coxph(Surv(time0, years, HYPERTEN) ~ SEX + lt.sex + AGE + lt.age + CIGPDAY + DIABETES + lt.dia + lnTOTCHOL + lt.tch + SEX:CIGPDAY + SEX:DIABETES, data=Y)
(coxXsum <- summary(cox.X))
AIC(cox.X)
cox.X


   xl <- "Time After Hypertensive Diagnosis (years)"
   yl <- "Survivial Proportion"
(main <- "Cox Main Time-Crossed Effects \n Baseline Hazard Curve (95% CI)")
   gp <- ggsurv(survfit(cox.X),plot.cens=F,cens.col="black") +
         ylim(0.2,1) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


# Note that the interaction between age and log(time) is specified using the end of the interval,
# years. To estimate survival for a 21 year old male we might be inclined to try the following:
     last <- Y$RANDID[which.max(Y$years)]intervals <- Y[Y$RANDID == last, c("time0", "years", "HYPERTEN")]
# We then add on the constant values of interest for age and gender and create the special interaction between our fixed age of interest and log(time).       covs <- data.frame(AGE=45, CIGPDAY=10, lnTOTCHOL=log(300), DIABETES=1, SEX=1, intervals)
covs$lt.sex <- covs$SEX * log(covs$years)
covs$lt.age <- covs$AGE * log(covs$years)
covs$lt.dia <- covs$DIABETES * log(covs$years)covs$lt.tch <- covs$lnTOTCHOL * log(covs$years)

# Next we call survfit using the newdata = covs and individual = TRUE options.
(scov <- summary(survfit(cox.X, newdata = covs, individual = TRUE)))

# Cox interaction model
V <- scov
(stab <- data.frame(time=V$time,n.risk=V$n.risk,n.event=V$n.event,survival=V$surv,std.err=V$std.err,lower0.95CI=V$lower,upper0.95CI=V$upper))

   xl <- "Time After Hypertensive Diagnosis (years)"
   yl <- "Survivial Proportion"
    x <- scov$time
(main <- "Cox Time-Crossed Effects \n Baseline (95% CI) Specific Case")
   gp <- ggsurv(scov,plot.cens=F,cens.col="black") +
         ylim(0.2,1) +
         ggtitle(main) + 
         xlab(xl) + 
         ylab(yl) +
         annotate("text", x = 15, y = .97, label = "Diabetic Male, Age 45 years") +
         annotate("text", x = 16.9, y = .94, label = "Cholesterol 300mg/dL, 10 Cigarettes/day") +
         theme(axis.text=element_text(size=16)) +
         theme(axis.title=element_text(size=20)) +
         theme(plot.title=element_text(size=24))
gp


##########################################################

