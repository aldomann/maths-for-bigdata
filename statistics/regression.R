##### Multiple Regression Chap 3 ISL

## Install packages
# install.packages("leaps")
# install.packages("car")
# install.packages("glmnet")
# install.packages("plotmo")

## Load libraries
library(MASS)
library(car)    # For vif()
library(glmnet) # Lasso and ridge
library(leaps)  # Subset selection, Cp, AIC, BIC

###### Data
data("Boston")
names(Boston)
# medv=median house value
attach(Boston)
pairs(Boston)
modelo1=lm(medv~.,data=Boston)
summary(modelo1)
vif(modelo1)
modelo2=update(modelo1,~.-tax)
summary(modelo2)

# Compare model1 and model2
anova(modelo1, modelo2)
# The p-value is small, therefore we cannot remove tax from the model; we have to stick with modelo1

# Models can be compared with:
# - F-test (of nested)
# - AIC = 1/(n sigma^2) * (RSS + 2 p sigma^2), where RSS: Res. Sum of Squares
# - CP = 1/n (RSS + 2 p sigma^2)
# - BIC = 1/n (RSS + log(n) p sigma^2)
# - R_{ADJ}^2 = (RSS/n - d - 1)/(\sum(y - \hat{y})^2/(n-1) )


modelo3=update(modelo1,~.-age)
summary(modelo3)
saic=stepAIC(modelo1)
plot(saic)

######### seleccion de modelos
fit.full=regsubsets(medv~.,data=Boston)
summary(fit.full) #max 8 predictors , change it with nvmax=...
summary.fit.full=summary(fit.full)
names(summary.fit.full)
summary.fit.full$cp # proportional to AIC
summary.fit.full$bic
summary.fit.full$adjr2
## All criteria choose model 8 (with optimal value)

fit.full.larger=regsubsets(medv~.,data=Boston,nvmax=13)
summary.fit.full.larger=summary(fit.full.larger)
summary.fit.full.larger$cp  #mod 11
summary.fit.full.larger$bic #mod 11
summary.fit.full.larger$adjr2 #mod 11

par(mfrow=c(2,2))
plot(summary.fit.full.larger$cp,xlab="Number of variables",ylab="Cp", type="b",col="darkblue",lwd=2)
plot(summary.fit.full.larger$bic,xlab="Number of variables",ylab="BIC", type="b",col="darkcyan",lwd=2)
plot(summary.fit.full.larger$adjr2,xlab="Number of variables",ylab="RSSadj", type="b",col="darkblue",lwd=2)

### a particular plot from regsubsets:
par(mfrow=c(1,1))
#squares indicate which variable is present
plot(fit.full.larger, scale="adjr2", col="darkcyan")
plot(fit.full.larger, scale="bic", col="maroon4")
plot(fit.full.larger, scale="Cp", col="dodgerblue2")

###### forward selection
forward=regsubsets(medv~.,data=Boston,nvmax=13,method="forward")
summary(forward)

######### how do we choose????? Cross-validation (later)






########## LASSO  lambda is chosen wirh CV as well...  train/validate
## in order to split the data, put iyt in vector Y matrix X
library(plotmo) # for plot_glmnet
### CAREFUL:
# alpha -> 1 = lasso, 0 = ridge, 2 = elastic net

set.seed(115)
x=model.matrix(medv~.,data=Boston)[,-1]
y=Boston$medv
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
lasso.mod=glmnet(x[train,], y[train],alpha=1,standardize=TRUE)
plot_glmnet(lasso.mod, label=TRUE,lwd=2)
plot_glmnet(lasso.mod, label=TRUE,lwd=2, xvar="norm")

cv.out =cv.glmnet (x[train ,],y[train],alpha =1)
plot(cv.out)
# bestlam =cv.out$lambda.min;bestlam
bestlam =cv.out$lambda.1se;bestlam
lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x[test ,])
mean(( lasso.pred -y.test)^2)

out=glmnet (x,y,alpha =1) #### with all data
lasso.coef=predict (out,type="coefficients",s=bestlam )[1:13,]
lasso.coef

## EXERCISE: try this using ridge

#### other training proportion
train2=sample(1:nrow(x),2*nrow(x)/3)
test2=(-train2)
y2.test=y[test2]
cv.out =cv.glmnet (x[train2,],y[train2],alpha =1)
plot(cv.out)
# bestlam =cv.out$lambda.min;bestlam
bestlam =cv.out$lambda.1se;bestlam
out=glmnet (x,y,alpha =1) #### with all data
lasso.coef=predict (out,type="coefficients",s=bestlam )[1:13,]
lasso.coef
############### mtcars  (far away from "big" data)
data(mtcars)
correlations=cor(mtcars)
round(correlations,2)
modelo=lm(mpg~., data=mtcars)
anova(modelo)
x=as.matrix(mtcars[-1])
y=mtcars[,1]
mod=glmnet(x,y,standardize=T,alpha=1)
plot.glmnet(mod,label=T,lwd=2)

mod=glmnet(as.matrix(x),y,standardize=T,alpha=1)
plot_glmnet(mod, label=T, lwd=2, xvar="norm")                             # default colors
plot_glmnet(mod, label=5)                    # label the 5 biggest final coefs

######choosing lambda

train=sample(1:nrow(x),nrow(x)*0.66)
test=(-train)
y.test=y[test]
cv.out =cv.glmnet (as.matrix(x[train,]),y[train],alpha =1,nfold=5)
plot(cv.out) # dotted line on the left min cv-error, right error within 1 stdev from min
# bestlam =cv.out$lambda.min;bestlam
bestlam =cv.out$lambda.1se;bestlam
out=glmnet(as.matrix(x),y,alpha =1) #### with all data, no standarization
lasso.coef=predict (out,type="coefficients",s=bestlam )[1:11,]
lasso.coef


####################### large p
set.seed(19874)
n <- 1000    # Number of observations
p <- 5000     # Number of predictors included in model
real_p <- 1500  # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

# Split data into train and test sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]

fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.lasso.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=1,family="gaussian")
bestlam =fit.lasso.cv$lambda.min;bestlam
out=glmnet (x,y,alpha =1) #### with all data
lasso.coef=predict (out,type="coefficients",s=bestlam )[1:5000,]
length(lasso.coef[lasso.coef!=0])

