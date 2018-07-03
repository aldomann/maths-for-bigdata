#Adapted fom Ramsay,Hooker, Grave' book chap9

################################################################################
################################################################################
#                             Functional Linear Models                                       #
################################################################################
################################################################################
install.packages("fda");
require(fda)
install.packages("fda.usc");
require(fda.usc)
# Functional linear models using the fRegress function (fda)
# Canadian Weather Data

data(CanadianWeather)
temp = CanadianWeather$dailyAv[,,1]
precip = CanadianWeather$dailyAv[,,2]

daytime = (1:365)-0.5
day5 = seq(0,365,5)

dayrng = c(0,365)

# Basis and parameter objects

fbasis =  create.fourier.basis(dayrng,65)

temp.fdPar = fdPar(fbasis,harmLfd,1e-2)

# And smooth

tempSmooth = smooth.basis(daytime,temp,temp.fdPar)

# And extract the functional data objects

tempfd = tempSmooth$fd



################################################################################
###                         SCALAR RESPONSE MODELS                          ####
################################################################################

## We examine predicting a scalar response from a functional covariate. 

# In this case, we'll predict log annual precipitation from the temperature
# profile.  This is one of the most over-used examples in FDA.

#### 1. Setup Data

# First we'll obtain log annual precipitation

annualprec = log10( apply(precip,2,mean))

##### Easiest way to do  it: low dimensional coefficient beta

conbasis=create.constant.basis(dayrng)
betabasis=create.fourier.basis(dayrng,5)
betalist = list(len=2)
betalist[[1]]=conbasis
betalist[[2]]=betabasis


# Now we need to set up a list of covariates.
# First co-variate is just the intercept: a vector of ones
# Second covariate is temperature
xlist = list(len=2)
xlist[[1]] = rep(1,35)
xlist[[2]] = tempfd

#### and Regression!

fRegressList=fRegress(annualprec,xlist,betalist)
prec.model$betaestlist[[1]]$fd$coef
# We can see the intercept as 

fRegressList$betaestlist[[1]]$fd$coef
# We can also plot the estimated regression function

plot(fRegressList$betaestlist[[2]])

annualprechat1=fRegressList$yhatfdobj
annualprecres1=annualprec-annualprechat1
SSE1=sum(annualprecres1^2)
SSE0=sum((annualprec-mean(annualprec))^2)
R2=(SSE0-SSE1)/SSE0 # not bad!!


#########  Now regression using Roughness penalty:
harmLfd = vec2Lfd(c(0,(2*pi/365)^2,0),rangeval=dayrng)  #makes a linear differential operator from a vector
## since data are periodic, we use an harmonic accelerator operator Lbeta=omega^2Dbeta+D^3beta
## simple differential doperators are created with int2Lfd
####  fdPar objects for coeffients

# We also need a list of functional parameter objects to define the coefficient
# functions. 
# First is a constant basis and add it to the fdPar object
bwtlist = list(len=2)
cbasis = create.constant.basis(dayrng)
bwtlist[[1]] = fdPar(cbasis)

# Now we need the coefficient of temperature, we'll use the same basis for it
# and add it as the second element in the list. 
# 12.5 is our choice for lambda
beta.fdPar = fdPar(fbasis,harmLfd,10^12.5)
bwtlist[[2]] = beta.fdPar

#### 3. fRegress and outcome

prec.model = fRegress(annualprec,xlist,bwtlist)

# Let's look at the outcome

names(prec.model)

# We can see the intercept as 

prec.model$betaestlist[[1]]$fd$coef

# We can also plot the estimated regression function

plot(prec.model$betaestlist[[2]])

annualprechat2=prec.model$yhatfdobj
annualprecres2=annualprec-annualprechat2
SSE1.2=sum(annualprecres2^2)
SSE0.2=sum((annualprec-mean(annualprec))^2)
R2.2=(SSE0.2-SSE1.2)/SSE0.2 # not bad either (but worse...)

### And what about having beta==0? 
betalist[[2]]=fdPar(conbasis)
fRegressList.con=fRegress(annualprec,xlist,betalist)
betaestlist=fRegressList.con$betaestlist

### compute R^2 in this case, it is lower..33 Ftests can be done, but beware because
# putting penalyzing coefficients would break normality.

#### 4. Cross-validation

### We should look at selecting lambda. We'll do this with OCV

lambdas = 10^(seq(5,15,0.5))

ocvs = rep(0,length(lambdas))

for(ilam in 1:length(lambdas)){
  bwtlisti = bwtlist                # define temporary beta.fdPar and bwtlist
  beta.fdPari = beta.fdPar
  
  beta.fdPari$lambda = lambdas[ilam]   # update lambda
  bwtlisti[[2]] = beta.fdPari
  
  prec.modeli = fRegress(annualprec,xlist,bwtlisti)
  
  ocvs[ilam] = prec.modeli$OCV        # record ocv
}

plot(lambdas,ocvs,type='b',log='x')

# It looks like our original choice of 12.5 was about right. 



#### 4. Statistics, Standard Errors and Tests 

# Degrees of freedom

prec.model$df 

# We'll plot y-by-yhat (in this case yhatfdobj is just a scalar despite 
# its name). 

yhat = prec.model$yhatfdobj

plot(yhat,annualprec)
abline(c(0,1))

# And we can caculate a residual variance

sigma = sum( (annualprec-yhat)^2 )/(35-prec.model$df)
sigma


# To obtain standard error estiamtes we can now call

sigmaE = sigma*diag(35)
prec.stderr = fRegress.stderr(prec.model,NULL,sigmaE)

# And we can obtain plots for beta from the estimated and standarderror

betahat = prec.model$betaestlist[[2]]
betastd = prec.stderr$betastderrlist[[2]]

plotbeta(betahat,betastd)


### EXERCISE: obtain a confidence interval for the intercept. 

### THOUGHT EXERCISE: beta1(t) integrates to zero; why?

# Finally, we can run a permutation test on this model; this will take some 
# time. 

par(mfrow=c(1,1),ask=FALSE)
Fresult = Fperm.fd(annualprec,xlist,bwtlist)

# The histogram gives the permutation distribution. Dashed lines are the 95 
# quantile and the solid line is the observed F value. 

### EXERCISE: plot residuals against predicted values. This may not indicate
### poor fit if there is a non-linear relationship only with one part of the
### covariate function. Try a 3-dimensional plot putting time on the 'x' axis,
### the covariate value on the 'y' axis and residuals on the 'z' axis. The #
### library 'rgl' is particularly good for this.

### EXERCISE: how sensitive are these results to the amount of smoothing of the
### temperature process? Try lambda at 1e-6 and 1e2.

################################################################################
###              FUNCTIONAL PRINCIPAL COMPONENTS REGRESSION                 ####
################################################################################

# Here we will continue the problem above, but we will tackle it from the 
# perspective of functional Principal Components Regression. 

#### 1. Obtaining an Estimate

# First we need to re-obtain fPCAs

tempPCA = pca.fd(tempfd,nharm=6)

# We'll continue to use the first three PCAs and extract their scores into 
# a predictor matrix

Xmat = tempPCA$scores[,1:3]

# Now perform linear regression

prec.lm = lm(annualprec~Xmat)

# We can already obtain some summary statistics

summary(prec.lm)

# and try the same trick

plot(prec.lm$fitted,annualprec)
abline(c(0,1))

# Now we want to reconstruct beta. First we'll make the code easy to read by 
# obtaining the PCs and the coefficients

Mcoefs = prec.lm$coef
PCs = tempPCA$harmonics[1:3]

# and put them together to get beta; remember, we still leave out the intercept. 

beta = Mcoefs[2]*PCs[1] + Mcoefs[3]*PCs[2] + Mcoefs[4]*PCs[3]

# Now we can plot the result

plot(beta)

### EXERCISE: this is pretty rough -- try cross-validating the amount of 
### smoothing in the PCA analysis based on its ability to predict log
### annual precipitation. 

##### 2. Standard Errors

# We will do this manually. First we will use the usual variance

varbeta = sigma*solve(t(Xmat)%*%Xmat)

# Now we'll obtain the values of the PCs at a set of plotting points

PCvals = eval.fd(day5,PCs)

# The covariance for beta is then 

PCbetacov = PCvals%*%varbeta%*%t(PCvals)

# We can actually have a look at the whole covariance surface

contour(day5,day5,PCbetacov)

# But largely we just want to extract the diagonal and then plot it.

# First we'll get values for beta

PCbetavals = eval.fd(day5,beta)

# Then standard errors

PCbetastd = sqrt(diag(PCbetacov))

# And we can form a plot

plot(day5,PCbetavals,type='l',lwd=2,ylim=c(-6e-4,1e-3))
lines(day5,PCbetavals+2*PCbetastd,type='l',lwd=2,lty=2)
lines(day5,PCbetavals-2*PCbetastd,type='l',lwd=2,lty=2)
abline(h=0)

# This is actually pretty similar to the fRegress version and will improve 
# with smoothing the PCA. 


################################################################################
###                      FUNCTIONAL RESPONSE MODELS                         ####
################################################################################

# We could predict total precipitation from temperature. How about the 
# annual precipitation profile?

# We can also look at constant predictors -- see the weather demo for an ANOVA
# between different weather regions. 

# First we'll create a smooth of the log precipitation

prec.fdPar = fdPar(fbasis,harmLfd,1e6)
precSmooth = smooth.basis(daytime,log(precip+0.5),prec.fdPar)
precfd = precSmooth$fd

# We can retain xlist from the scalar response model. 


#### 1. fdPar objects and estimation

bwtlist2 = list(len=2)

# The intercept is now a functional parameter as well as beta 1.   Since this
# is an identifiable model without smoothing, we'll set the smoothing parameter 
# very low. 

beta.fdPar2 = fdPar(fbasis,harmLfd,1e-5)

bwtlist2[[1]] = beta.fdPar2
bwtlist2[[2]] = beta.fdPar2 

# We can also call fRegress with this

prec.conc = fRegress(precfd,xlist,bwtlist2)

# Let's have a look at what we've got

par(mfrow=c(2,1))
plot(prec.conc$betaestlist[[1]])
plot(prec.conc$betaestlist[[2]])

# We can also look at a comparison between predicted and observed

yhatfd  = prec.conc$yhatfdobj$fd  # Fitted smooths. 

plot(yhatfd)
plot(precfd)

# And compare observed with residuals

plot(precfd-yhatfd)
plot(precfd)

# Doesn't look like we really got very much. 

#### 2. Confidence Intervals

## In order to obtain confidence intervals, we can include the smoothing of 
## precipitation as a source of error. In order to do this, we need two things

y2cmap = precSmooth$y2cMap

# This is the matrix that goes from the observations to the coefficients. 

# We now need a covariance matrix for the errors of the original observed 
# precipitation from the functional linear model

Errmat = precip - eval.fd(daytime,yhatfd) 

SigmaE2 = cov(t(Errmat))

# We can now run fRegress.stderr

conc.std = fRegress.stderr(prec.conc,y2cmap,SigmaE2)

# And plot the results

plotbeta(prec.conc$betaestlist,conc.std$betastderrlist)

# There really doesn't appear to be much going on. 


### EXERCISE: try predicting precipitation instead of log precipitation -- does
### this make a difference?

### EXERCISE: what diagnostics could be done to check goodness of fit? Try
### plotting residuals. Try plotting residuals against predicted values
### (this should give you a series of lines, you'll need to evaluate and use
### 'matplot').    How could you check for dependence of precipitation on
### non-concurrent times?


#### 3. Permutation Tests and Cross Validation

### The next two functions can take a very long while to run 

## Permutation test for fRegress

par(mfrow=c(1,1),ask=FALSE)
Fresult = Fperm.fd(precfd,xlist,bwtlist2)

# Here the dotted line gives the 95th percentile of the permutation distribution
# at each time t, the dashed line gives the 95th percentile of the permutation
# distribution of the maximum F value, and the solid line gives the observed 
# F value at each time t.  


## Cross validated integrated squared error. 

# This is a particularly long undertaking since the cross-validation is 
# done manually. There are some matrix identities that can speed this up
# (still to be implemented).  

# In this case we will only look at the same lambda for each coefficient 
# function

lambdas = 10^(c(-5,-2,1,3))
SISEs = rep(0,length(lambdas))

for(ilam  in 1:length(lambdas)){
  beta.fdPari = fdPar(fbasis,harmLfd,lambdas[ilam])   # Update lambda
  bwtlisti = list(beta.fdPari,beta.fdPari)
  
  CVres = fRegress.CV(precfd,xlist,bwtlisti)
  
  SISEs[ilam] = CVres$SSE.CV
  
  print(c(ilam,SISEs[ilam]))      # Just so we know where we are. 
}

plot(lambdas,SISEs,type='b',log='x')

##### Further data sets to try playing with:

#  gait -- provides the gait data demo'd in class. Predict knee angle from
#  hip angle, or try predicting a derivative. The data are in 'gait'

#  Try predicting temperature from region in the weather data. You can find
#  a region indicator in "CanadianWeather$Region".  BUT, you will need to set
#  up an appropriate design matrix.
#
#  This is a functional version of an ANOVA.


################################################################################
###                           OTHER UTILITIES                               ####
################################################################################

# There are a number of other utilities in the fda library, that are worth
# checking out. 

# 1. Constrained smooths, this includes
#
#  - smooth.pos   (positive smoothing -- see precipitation demo)
#  - smooth.mon   (monotone smoothing -- growth demo)
#  - density.fd   (log spline density estimation)
#
# These all have evaluation routines associated with them, too. 

# 2. Registration, in particular
#
#  - landmarkreg   (landmark registration)
#  - register.fd   (automatic registration)
#
#  Good demos are the pinchforce data and the growth data. 

# 3. Principal Differential Analysis
#
# The main engine for this is pda.fd which is just like a multivariate
# concurrent linear model with derivatives, but the signs can be different. 
#
# See, in particular, the lip demo and the handwriting demo. 
#
# plot.pda   provides some plotting  functions of coefficients
# eigen.pda  gives a stability analysis. 

# 4. Unconstrained functional response models
#
# linmod will estimate a bivariate functional parameter for this case. 
#
# See the weather data, or demo from Swedish mortality data.
#

###  Finally: code to produce all analyzes and figures in 
#
# Ramsay & Silverman, "Functional Data Analysis"
#
# and
# 
# Ramsay, Hooker and Graves, "Functional Data Analysis in R and Matlab", 
#
# can be found in the "scripts" subdirectory of the fda directory in your R
# library. 

## Introduction to R for Functional Data Analysis
#
# Author: Giles Hooker
#
# Last Modified: 10/10/2011
#
# This file presents code for an introduction to R and Functional Data
# Analysis (FDA) associated with a workshop on FDA run through the
# Statistical Consulting Unit
#
# To begin with, note that R has a text editing window (this one) in
# which you can write commands and programs. As will all programming
# languages, programs can be commented -- R will ignore anything after
# a '#' on a line. You should always comment liberally; for your own sake
# as well as those who read your code.
#
# A useful key to know is control-R, which will enter highlighted commands
# into the R process window.


## Obtaining functional data analysis tools:

# The fda tools are contained in a library in R, try

library('fda')

# if you get an error message at this point, you will need to install the library
# to do this

install.packages('fda')

# choose some server to download from, and at that point type in

library('fda')

## Obtaining some data

data(handwrit)

### Now to Functional Data Analysis

# First we'll observe that handwrit is a 3-d array

dim(handwrit)

# 1401 points, by 20 reps, by 2 dimensions.

# Let's separate the dimensions

xdata = handwrit[,,1]
ydata = handwrit[,,2]

# Time:

fdatime   = handwritTime/1000;

# plot a line

plot(fdatime,xdata[,1],type='l')

# take a closer look

plot(fdatime[1:100],xdata[1:100,1],pch='x')

# what about all the replications?

matplot(fdatime,ydata,type='l',xlab="time")

# How do I see the handwriting?

matplot(xdata,ydata,type='l')

## The FDA Package

# We need to turn our discrete measurements into smooth curves. To do this,
# we first define a system of basis functions

help(create.basis)

# All basis objects require a range to be specified:

basis.range = range(fdatime)

# Normally, you would use as many basis functions as there are data points.
# This is a bit large here, so I'll use 100:

bspline.obj = create.bspline.basis(basis.range,100);

# Now let's look at these functions. To do this you can simply

plot(bspline.obj)

# we can specify the points where we want to see it

basisvals = eval.basis(fdatime,bspline.obj);
matplot(fdatime,basisvals,type='l')

# these are different because the plot for basis objects evaluates only at
# 100 points.

# To see these up close

matplot(fdatime[1:100],basisvals[1:100,1:10],type='l')


# Having got a set of basis functions, we can turn them into a functional
# data object by adding coefficients. A very simple way to do this is

fd.obj = Data2fd(handwrit,fdatime,bspline.obj)

# Usually, however, we need to introduce some smoothing: see the book for
# more on that.

# We can now plot these objects

par(mfrow=c(2,1))  # divides the plotting window in 2.
plot(fd.obj)

# We can also look at only one variable

plot(fd.obj[,2])

# Or even one curve

plot(fd.obj[3,2])

# We can also evalutate the functional data objects directly

fdvals = eval.fd(fdatime,fd.obj);
matplot(fdvals[,,1],fdvals[,,2],type='l')

# And look at residuals

plot(fdatime,handwrit[,1,1]-fdvals[,1,1],pch='.')

# Since I've got a basis expansion, I can access derivatives

dfdvals = eval.fd(fdatime,fd.obj,1);    # Velocity
d2fdvals = eval.fd(fdatime,fd.obj,2);   # Acceleration

# And I can plot

matplot(fdatime,dfdvals[,,2],type='l')
matplot(fdatime,d2fdvals[,,2],type='l')

# And produce three dimensional plots

library(rgl)
plot3d(fdvals[,1:10,2],dfdvals[,1:10,2],d2fdvals[,1:10,2],type='l')


# You can also add and subtract fd objects

plot(fd.obj[,1]+fd.obj[,2])

# Not that this is overly useful.

# And we can take a mean

mfdvals = eval.fd(fdatime,mean(fd.obj[,2]));

plot(fd.obj[,2])
lines(fdatime,mfdvals,lwd=2)


# And we can look at residuals from the mean

cfd.obj = center.fd(fd.obj[,2]);
plot(cfd.obj)


# And we can look at variance

sfd.obj = sd.fd(fd.obj[,2]);
plot(sfd.obj)

# And the covariance:

vfd.obj = var.fd(fd.obj[,2]);

# This is a .bivariate. fd object for which plotting functions are not
# available. However, we can evaluate it

vfdvals = eval.bifd(fdatime,fdatime,vfd.obj);

# We can plot this with a contour plot:

contour(fdatime,fdatime,vfdvals);

# Or a surface plot, but for this we probably want to subset:

persp3d(fdatime[seq(1,501,5)],fdatime[seq(1,501,5)],vfdvals[seq(1,501,5),seq(1,501,5)],col='blue')

# And we can perform a PCA analysis:

pcafd.obj = pca.fd(fd.obj[,2],4);
plot(pcafd.obj$harmonics)       # Eigenfunctions
pcafd.obj$varprop               # Eigenvalues

# There are special plotting functions for this as well

pcafd.obj.eigvals = pcafd.obj$values;
plot(pcafd.obj)

# For further examples, see
#
# C:\Users\<This User>\Documents\R\win-library\<version>\fda
#
# or
#
# C:\Program Files\R\<R Version>\library\fda
#
# depending on your installation (different again on Mac of Linux)
#
# Also a longer lab associated with a short course is at
#
#  http://www.bscb.cornell.edu/~hooker/ShortCourseLab.R
