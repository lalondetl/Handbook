
##########################################################
#     Framingham data
#     SEM analysis

#     Jamie D. Riggs, 2017

#     Data are loaded in each section for convenience after data conditioning


##########################################################
#    Libraries
##########################################################

library(MASS)
library(usdm)
library(lavaan)
library(psych)
library(stats)
library(semPlot)
library(fBasics)
library('dafs')

(Path <- "~/Desktop/WD")  # replace with your desired path


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

Ex <- "SEM"
ver <- "FHS"

setwd(Path)    # string with the path name to your working data
WD <- getwd()

(infile <- paste0(WD, "/", ver, ".csv"))
     X <- data.frame(read.csv(infile, header=TRUE))

# All the necessary information for time to hypertension is in the period 1 record
# Exclude subjects with diagnosed hypertension prior to insertion into the study
# Remove cases with missing data
tte <- na.omit(X[X$PREVHYP=="0"&X$PERIOD==1,c(1:4,8,10,24,31,39)])
  X <- tte   # X is the working data matrix


##########################################################
#     Data summary
##########################################################

summary(X)

     A <- X[,3:5]
     n <- nrow(A)
 (mean <- colMeans(A))
  (std <- colStdevs(A))
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

# save scrubbed data
outfile <- paste0(WD, "/", ver, ".RData")
save(X, file=outfile, ascii=FALSE)


##########################################################
#     Scatter plot and pairwise correlation
##########################################################

Ex <- "SEM"
ver <- "FHS"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))  # load scrubbed data
load(infile)
summary(X)

A <- subset(X, select=c(TOTCHOL:CIGPDAY,TIMEHYP))
summary(A)
(main <- paste0("Histograms, Scatter Plots, and Pairwise Correlations"))
pairsDAFS(A, main=main, labels=names(A))


##########################################################
#     Normal Q-Q plots
##########################################################

Ex <- "SEM"
ver <- "FHS"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)

A <- subset(X, select=c(TOTCHOL:CIGPDAY,TIMEHYP))

# normal Q-Q plots of observed data
par(mfrow=c(2,2))
qqnorm(A[,1],main=paste(names(A[1]),"Normal Q-Q Plot"))
qqline(A[,1],lwd=2,col="red")
grid(col="black")
qqnorm(A[,2],main=paste(names(A[2]),"Normal Q-Q Plot"))
qqline(A[,2],lwd=2,col="red")
grid(col="black")
qqnorm(A[,3],main=paste(names(A[3]),"Normal Q-Q Plot"))
qqline(A[,3],lwd=2,col="red")
grid(col="black")
qqnorm(A[,4],main=paste(names(A[4]),"Normal Q-Q Plot"))
qqline(A[,4],lwd=2,col="red")
grid(col="black")


# normal Q-Q plots of transformed data
  A[,3] <- sqrt(B[,3])      # counts data
A[,1:2] <- log(B[,1:2])     # totchol and age
  A[,4] <- log(B[,4]+1)
par(mfrow=c(2,2))
qqnorm(A[,1],main=paste("Log",names(A[1]),"Normal Q-Q Plot"))
qqline(A[,1],lwd=2,col="red")
grid(col="black")
qqnorm(A[,2],main=paste("Log", names(A[2]),"Normal Q-Q Plot"))
qqline(A[,2],lwd=2,col="red")
grid(col="black")
qqnorm(A[,3],main=paste("Root", names(A[3]),"Normal Q-Q Plot"))
qqline(A[,3],lwd=2,col="red")
grid(col="black")
qqnorm(A[,4],main=paste("Root", names(A[4]),"Normal Q-Q Plot"))
qqline(A[,4],lwd=2,col="red")
grid(col="black")


##########################################################
#    Boxplots
##########################################################

Ex <- "SEM"
ver <- "FHS"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)

  y <- X$TIMEHYP
  x <- X$SEX
 xl <- "Sex"
 yl <- "Time to Hypertension"
boxplot(y~x, notch=TRUE, main=paste(yl,"vs",xl), xlab=xl, ylab=yl, varwidth=F, col="coral")
grid(nx=NA,ny=NULL,col="black",lty="dotted")
(means <- by(y, x, base::mean))                      
points(1:length(means), means, pch = 23, cex = 1.5,bg = "blue")
text(1:length(means)+.2, means+500, 
     labels = formatC(means, format = "f", digits = 2),
     pos = 2, cex = 0.9, col = "blue4")


##########################################################
#     Tables
##########################################################

Ex <- "SEM"
ver <- "FHS"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)

(tSex <- FreqProp(X$SEX,"Sex"))
(tDia <- FreqProp(X$DIABETES,"Diabetes"))
(tHyp <- FreqProp(X$HYPERTEN,"Hypertension"))
(tDea <- FreqProp(X$DEATH,"Death"))

(tab12 <- FreqProp2(X$SEX,X$HYPERTEN,"Sex","Hypertension"))
(tab13 <- FreqProp2(X$SEX,X$DEATH,"Sex","Death"))
(tab14 <- FreqProp2(X$HYPERTEN,X$DEATH,"Hypertension","Death"))


##########################################################
#     LVM
##########################################################

Ex <- "SEM"
ver <- "FHS"

setwd(Path)
(WD <- getwd())
(infile <- paste0(WD, "/", ver, ".RData"))
load(infile)
summary(X)


# condition data for latent variabble model
    M <- X[,c(3,5:6,8)]
M[,1] <- log(M[,1])
M[,2] <- sqrt(M[,2])
summary(M)
(A <- round(cor(M),4))    # correlation matrix
A[upper.tri(A)] <- NA
(A <- data.frame(A))
(B <- sapply(A, as.character))
B[is.na(B)] <- " "
(raw <- as.data.frame(B,row.names=rownames(A)))   # raw correlation matrix

m0 <- 'g =~ a*TOTCHOL + b*CIGPDAY + c*DIABETES + d*HYPERTEN'
mm <- "m0"
# fit model
m0.fit <- cfa(model=m0, data=M, ordered=c("HYPERTEN","DIABETES"), std.lv=TRUE)
# examine parameter estimates
(sm <- summary(m0.fit,standardized=TRUE))
(pe <- parameterEstimates(m0.fit,standardized=TRUE))

main <- paste(mm, "Path Analysis.")
semPaths(m0.fit, title=F, edge.label.cex = 0.5, curvePivot = TRUE, intercepts=T)
title(main, line = 3)

# Communalities
(com <- pe$std.all[1:4]*pe$std.all[1:4])
(unq <- 1 - com)
(lds <- data.frame(IV=pe$rhs[1:4], Loading=pe$std.all[1:4], Communality=com, Uniqueness=unq))

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
