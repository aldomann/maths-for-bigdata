##### Adapted from Gilles Hooker
#####
install.packages("fda");
require(fda)
install.packages("fda.usc");
require(fda.usc)
## some plots
par(pty="s") # square plot

# handwritten data 
data("handwrit")
?handwrit
plot(handwrit[, 1, 1], handwrit[, 1, 2], type="l", col="maroon3",xlab="mm",ylab="mm", lwd=2)
matplot(handwrit[, 1:5, 1], handwrit[, 1:5, 2], type="l", col="maroon3",xlab="mm",ylab="mm", lwd=2)
## take a look at the difference between plot and matplot


# Canadian Weather Data.
?CanadianWeather
data(CanadianWeather)
names(CanadianWeather)
dim(CanadianWeather$dailyAv)
# Temperature and precipitation are contained in the dailyAV element
#1: day of the year
#2: Monitoring Station
#3:Temperature.C Precipitation.mm      log10precip 
temp = CanadianWeather$dailyAv[,,1]
precip = CanadianWeather$dailyAv[,,2]
# We need corresponding time points. Put them half-way through a day. This
# is because the period is over 0:365 and we'd like the 365 data points to be
# about symmetric in that period.

daytime = (1:365)-0.5

# This is a bit fine for plotting purposes, so we can also create a vector of
# points to use for plotting every 5 days.  Later to be used for b-spline basis.

day5 = seq(0,365,5)

### Some plots (for the course's notes)
par(mfrow=c(1,2)) 
par(pty="s")
matplot(daytime,temp,type='l', xlab="Day",ylab="temp", lty=1, main="Temperature",col=as.factor(CanadianWeather$region))
matplot(daytime,precip,type='l',xlab="Day",ylab="rain",lty=1, main= "Precipitation",col=as.factor(CanadianWeather$region))

par(mfrow=c(1,1));par(pty="m") #back to "normal" settings
matplot(daytime,temp,type='l', xlab="Day", col="darkblue")

# We can also plot by region; Atlantic, Pacific, Central and North. 
par(mfrow=c(1,2)) 
par(pty="s")
matplot(daytime,temp,type='l',col=as.factor(CanadianWeather$region))
matplot(daytime,precip,type='l',col=as.factor(CanadianWeather$region))

# Fdata objects.  Beware, packages fda and fda.usc have slightly different syntaxis.

dim(CanadianWeather$dailyAv)
######## The simplest Fdata object (with no smoothing)
tt=1:365
temp.fdata<-fdata(t(CanadianWeather$dailyAv[,,1]),tt) #from fda.usc , ask for help!
dim(temp.fdata) #  rows are functions as opposed to fda package
class(temp.fdata)
temp.fd=fdata2fd(temp.fdata)
depth.mode(temp.fdata,draw=T) # we'll talk about depths later
lines(mean.fd(temp.fd),col="darkblue",lwd=2)


################################################################################
###                          DEFINING A BASIS SYSTEM                        ####
################################################################################

# We'll always need the range (in fda.usc it is called rangeval)
# fda.usc mostly relyes on basis systems created by fda, see help
# for both libraries.

dayrng = c(0,365)

#### 1. Fourier Basis with 365 basis functions

fbasis = create.fourier.basis(dayrng,365) #from fda
par(mfrow=c(1,1))
par(pty="m")
# Plot a basis with just 5 components  nbasis must be odd, otherwise one more is added
plot(create.fourier.basis(dayrng,1),lwd=2,main="1 basis function")
plot(create.fourier.basis(dayrng,3),lwd=2,main="3 basis functions",type="l")
plot(create.fourier.basis(dayrng,5),lwd=2,main="5 basis functions")#  sin, cos sin2w, cos2w
plot(create.fourier.basis(dayrng,9),lwd=2,main="9 basis functions")
# 

# Let's try a simple linear regression of the first temperature record on the 
# first few basis functions

fb.values = eval.basis(day5,fbasis)

# the 74 by 365 matrix that results has rows as days, columns as bases

dim(fb.values)
plot(day5,fb.values[,1])
plot(day5,fb.values[,2])

# Extract the first temperature record

ex.temp = temp[,1] #St Johns' station

# Run a linear regression on temperature with the first 5 basis functions. In
# this case, evaluate the basis at the observation times

Xmat = eval.basis(daytime,fbasis)
Xmat = Xmat[,1:5]   # First 5 basis functions
Xmat=Xmat[,1:20]
Xmat=Xmat[,1:3] # First 3 basis functions
ex.temp.mod = lm(ex.temp~Xmat)

# Plot this; the fitted values returned by lm will represent the smooth
# well enough to plot. 

plot(daytime,ex.temp,col="grey", main="St Johns monitoring Station",ylab="temp")
lines(daytime,ex.temp.mod$fitted,col="darkblue",lwd=2)

# We can also look at residuals

plot(daytime,ex.temp.mod$resid)

# There's some clear autocorrelation; more basis functions may be warranted. 

###  EXERCISE: repeat the above with different numbers of basis functions (say
###  20, 50, 100, 200, 365. How many look like they give a reasonable smooth?

#### 2. B-spline bases with knots every 5 days

# First of all define a knot sequence; this will be the same as day5

knots = day5

# We'll use fourth-order B-splines (that is, a local polynomial of degree 3)

norder = 4

# this implies the number of basis functions

nbasis = length(knots) + norder - 2

# Now we can define the basis

bbasis = create.bspline.basis(dayrng,nbasis,norder,knots)

# If in doubt, we can obtain

bbasis$nbasis    # number of basis functions
bbasis$rangeval   # basis range
plot(bbasis)

# but we can look at a smaller number

plot(create.bspline.basis(dayrng,nbasis=12,norder))

# We can also look at the inner product of these (should be orthogonal)

in.mat = inprod(bbasis,bbasis)

par(mfrow=c(1,1))
image(in.mat)

# and see that it is zero outside of a diagonal band; this can help computation
# a great deal. 

### EXERCISE: try changing the order of the basis and observe how the width
### of the support of the basis changes and how its smoothness properties change. 

### EXERCISE: obtain a least squares smooth of these data with the Bspline basis
### how does this compare with a least squares smooth using a Fourier basis with
### the same number of basis functions?

##########  Wiener Process in 0,10000  ## from fda package
wiener=c(0,cumsum(rnorm(10^4-1))/100 )
plot.ts(wiener)
B25.basis=create.bspline.basis(rangeval=c(0,10^4),nbasis=25)
wiener.fd=smooth.basis(y=wiener,fdParobj=B25.basis)
lines(wiener.fd,col="maroon4",lwd=2)
#several curves
N=50
w.vec=rnorm(10^4*N)/100
w.mat=matrix(rnorm((10^4-1)*N)/100,ncol=N,nrow=10^4-1)
w.mat=rbind(rep(0,N),w.mat)
w.mat=apply(w.mat,2,cumsum)
w.fd=smooth.basis(y=w.mat,fdParobj=B25.basis)
plot(w.fd,col="gray",lty=1)
lines(mean(w.fd$fd),col="darkblue",lwd=2)
lines(std.fd(w.fd$fd),col="darkgreen",lwd=2)
## computing the covariance matrix
w.cov=var.fd(w.fd$fd)
## to plot it
grid=(1:100)*100
w.cov.mat=eval.bifd(grid,grid,w.cov)
persp(grid,grid,w.cov.mat, xlab="s",ylab="t",zlab="c(s,t)")
par(pty="s")
contour(grid,grid,w.cov.mat,lwd=2)
################################################################################
###                          SMOOTHING FUNCTIONS                            ####
################################################################################

#### 1. Lfd Objects

# Two common means of generating Lfd objects
# 1. int2Lfd -- just penalize some derivatives. 

curv.Lfd = int2Lfd(2)

# 2. vec2Lfd -- a (constant) linear combination of derivatives; for technical
# reasons this also requires the range of the basis. 

harmLfd = vec2Lfd(c(0,(2*pi/365)^2,0),rangeval=dayrng)

# looking inside these objects is not terribly enlightening. 

#### 2. fdPar objects

# We'll concentrate on B-splines and second-derivative penalties.

# First, a value of lambda  (purposefully large so that we can distinguish a fit
# from data below).

lambda = 1e6

# Now we can define the fdPar object 

curv.fdPar = fdPar(bbasis,curv.Lfd,lambda)

#### 3. Smoothing functions

# We're now in a position to smooth

tempSmooth1 = smooth.basis(daytime,temp,curv.fdPar)

# Let's look at the result

names(tempSmooth1)

# First of all, let's plot it

plot(tempSmooth1$fd)

# There is also a neat utility to go through each curve in turn and look at its
# fit to the data:

plotfit.fd(temp,daytime,tempSmooth1$fd)


# Let's examine some fit statistics

# degrees of freedom

tempSmooth1$df

# Just about equivalent to fitting 5 parameters

# We'll also look at GCV, this is given for each observation

tempSmooth1$gcv

# Let's change to a more realistic value of lambda

lambda = 1e1
curv.fdPar$lambda = lambda

tempSmooth = smooth.basis(daytime,temp,curv.fdPar)

# and repeat the previous steps

plotfit.fd(temp,daytime,tempSmooth$fd)
tempSmooth$df
tempSmooth$gcv

# Here the fit looks a lot better and the gcv values are much smaller. 

#### 4. Choosing smoothing parameters

# We can search through a collection of smoothing parameters to try and find
# an optimal parameter.

# We will record the average gcv and choose lambda to be the minimum of these. 

lambdas = 10^seq(-4,4,by=0.5)    # lambdas to look over

mean.gcv = rep(0,length(lambdas)) # store mean gcv


for(ilam in 1:length(lambdas)){
  # Set lambda
  curv.fdPari = curv.fdPar
  curv.fdPari$lambda = lambdas[ilam]
  
  # Smooth
  tempSmoothi = smooth.basis(daytime,temp,curv.fdPari)
  
  # Record average gcv
  mean.gcv[ilam] = mean(tempSmoothi$gcv)
}

# We can plot what we have

plot(lambdas,mean.gcv,type='b',log='x')

# Lets select the lowest of these and smooth

best = which.min(mean.gcv)
lambdabest = lambdas[best]

curv.fdPar$lambda = lambdabest
tempSmooth = smooth.basis(daytime,temp,curv.fdPar)

# And look at the same statistics

plotfit.fd(temp,daytime,tempSmooth$fd)
tempSmooth$df

# We'll also plot these

plot(tempSmooth)

### EXERCISE: try obtaining a smooth of the precipitation data

### EXERCISE: how much does the result change if the basis has a knot every day
### instead of every 5 days?


################################################################################
###       FUNCTIONAL DATA OBJECTS: MANIPULATION AND STATISTICS              ####
################################################################################

## Now that we have a functional data object we can manipulate them in various 
# ways.  First let's extract the fd object

tempfd = tempSmooth$fd

# if we look at what's in this we see

names(tempfd)

# We see a basis, plus coefficient matrix

dim(tempfd$coefs)

# and an array giving names

tempfd$fdnames

# With lists giving names for time points, replicates and dimensions. Each list
# also has a name that can be used in plotting.  Apart from plotting functions, 
# fdnames isn't used and you can generally ignore it. 

# We can also create fd objects by adding a basis and a coefficient array. Let's 
# make a random one, say

newcoefs = matrix(rgamma(nbasis*10,5,2),nbasis,10)
newfd = fd(newcoefs,bbasis)

# Notice that we haven't specified fdnames. 

# The plotting command nicely draws these. 

plot(newfd)

# Not that this looks very nice; we'll stick with the Canadian weather data. 


#### 1. Manipulation

# We can do a number of things with these functions, treating them as data. 
# These operations all result in new functional data objects, but we will plot
# them directly as an illustration. 

# Subset

plot(tempfd[1:10])

# We can add them together; the 'lines' function also works with them

newfd = tempfd[1] + tempfd[2]
plot(newfd)
lines(tempfd[1],col=2)
lines(tempfd[2],col=4)

# We can also multiply

plot(tempfd[1]*tempfd[2])

# And obtain powers

plot(tempfd[1]^2)

# We can also obtain derivatives

plot(deriv.fd(tempfd))

# These are pretty wild because of the roughness of the resulting curves
# instead let's have a look at the over-smoothed data:

plot(deriv.fd(tempSmooth1$fd))

# We can also look at second derivatives

plot(deriv.fd(tempSmooth1$fd,2))

# Note that it is a property of B-splines of order m that the (m-2)th derivative 
# is zero at the end of the interval. 

#### 2. Summary statistics

# The obvious thing to look at is the mean

mtempfd = mean(tempfd)
plot(tempfd,col=4)
lines(mtempfd,lwd=2,col=2)

# We can also examine a variance

temp.varbifd = var.fd(tempfd)

# temp.varbifd is a bivariate functional data object -- meaning it takes values
# on a rectangle. 

# To plot this, we need to evaluate it; here we'll use day5 -- 365 points is
# a bit overkill.

temp.var = eval.bifd(day5,day5,temp.varbifd)
contour(day5,day5,temp.var)

# Mostly high variance in the winter, low in summer. Let's have a look at 
# correlation. In this case, evaluation arguments go in with the function call

temp.cor = cor.fd(day5,tempfd)
filled.contour(day5,day5,temp.cor)

# Here we see high correlation between Summer and Winter temperatures, but 
# much less in the spring and fall (although spring to fall correlation is still
# high). 

### EXERCISE: obtain these for the precipitation data and look at the covariance
### and correlation between temperature and precipitation.  

### EXERCISE: try repeating the above with a Fourier basis and the harmonic 
### acceleration penalty. Does this make much difference?


################################################################################
###                   FUNCTIONAL PRINCIPAL COMPONENTS                       ####
################################################################################

#### 1. pca.fd

# We can conduct a fPCA through

tempPCA = pca.fd(tempfd,nharm=6)

# Here we can look at proportion of variance explained:

plot(tempPCA$varprop,type='b')

# Looks like we could have stopped at 3.

## Looking at the principal components:

plot(tempPCA$harmonics[1:3])

# 1 Looks like over-all temperature. 
# 2 Looks like Summer vs Winter
# 3 Is Spring vs Fall. 

## But let's plot these 

plot(tempPCA,harm=1:3)

# Which gives a much better picture of what's going on. 

##### 2. PCA and Smoothing

# The PCs above are fairly rough, we can also add a smoothing penalty (see
# the special topics slides). 

pca.fdPar = fdPar(bbasis,curv.Lfd,1e4)

tempPCAsmooth = pca.fd(tempfd,nharm=6,harmfdPar=pca.fdPar)

# Let's plot the PCs

plot(tempPCAsmooth$harmonics[1:3])

# We can see that these are considerably smoother but still have pretty much
# the same interpretation. 

plot(tempPCAsmooth)

##### 3. PCA and Reconstructing Data

# We can ask how well the PCs reconstruct the observed functions. We'll focus
# on the first observation and reconstructing using PC score.

# Let's extract the scores and PCs just to make the code easier to read

scores = tempPCAsmooth$scores
PCs = tempPCAsmooth$harmonics

# Firstly, just the mean + PC1

ex.temp.r1 = mtempfd + scores[1,1]*PCs[1]

# and plot these

plot(tempfd[1],ylim=c(-20,20))
lines(mtempfd,col=2)
lines(ex.temp.r1,col=3)

# Try adding the second PC

ex.temp.r2 = mtempfd + scores[1,1]*PCs[1] + scores[1,2]*PCs[2]
lines(ex.temp.r2,col=4)

# And the third

ex.temp.r3 = ex.temp.r2  + scores[1,3]*PCs[3]
lines(ex.temp.r3,col=6)

### THOUGHT EXERCISE: how would you use this to choose the level of smoothing
### in pca.fd by leave-one-curve-out cross validation? 



##### 3. PCA of Multivariate Functions

# To look at two dimensions, we'll re-smooth the temperature data and look at 
# it along with its derivative. To do that, let's consider a fourier basis
# and harmonic acceleration. 

# First an fdPar object; we'll use 65 Fourier basis functions; more than enough
# but this will cut down the computational time. We'll also over-smooth so our
# derivatives don't look so wild. 

fbasis =  create.fourier.basis(dayrng,65)

harm.fdPar = fdPar(fbasis,harmLfd,1e6)

# Do the smooth
tempSmooth2 = smooth.basis(daytime,temp,harm.fdPar)

# and take a derivative

temp.deriv = deriv.fd(tempSmooth2$fd)

# Now we need to duck under the hood to create a joine fd object for both
# temperature and precipitation at once. 

# This basically means I need an fd object that stacks the coefficients for 
# temperature and the coefficients for D temperature along a third dimension
# of a coefficient array. 

Dtempcoefs = array(0,c(65,35,2))

Dtempcoefs[,,1] = tempSmooth2$fd$coefs
Dtempcoefs[,,2] = temp.deriv$coefs

# Let's also deal with the dimension names 

Dtemp.fdnames = tempfd$fdnames
Dtemp.fdnames[[3]] = c('temperature','D temperature')

# Put it all together

Dtempfd = fd(Dtempcoefs,fbasis,Dtemp.fdnames)

# Now we can plot

par(mfrow=c(2,1))
plot(Dtempfd[,1])
plot(Dtempfd[,2])

# We can also look at covariance

Dtemp.varbifd = var.fd(Dtempfd)

# If we look at 

Dtemp.var = eval.bifd(day5,day5,Dtemp.varbifd)

# We have a 74 x 74 x 1 x 3 array.

par(mfrow=c(1,1))

# First temperature
contour(day5,day5,Dtemp.var[,,1,1])

# Then d temperature
contour(day5,day5,Dtemp.var[,,1,3])

# Then their cross-product
contour(day5,day5,Dtemp.var[,,1,2])

### EXERCISE: experiment with cor.fd on this multidimensional fd object.

## Now let's look at the PCA

Dtemp.pca = pca.fd(Dtempfd,nharm=4)

# The PCs are now two dimensional

par(mfrow=c(2,1))
plot(Dtemp.pca$harmonics[,1])
plot(Dtemp.pca$harmonics[,2])

# We can plot these in the usual manner

plot(Dtemp.pca,harm=1:3)

# But we can also plot the whole cycle

par(mfrow=c(1,1))
plot(Dtemp.pca,harm=1:3,cycle=TRUE,xlab='temperature',ylab='D temperature')

# PC1 = over-all temperature and little variation in derivatives. 
# PC2 = high summer-winter variation and also large variation in derivatives


