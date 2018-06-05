# Author: Daniel Salgado Rojo
# Exercise 8 from the book: An Introduction to Statistical Learning

# In this exercise, we will generate simulated data, and will then use
# this data to perform best subset selection.
library(glmnet)
library(leaps)

#(a) Use the rnorm() function to generate a predictor X of length
    # n = 100, as well as a noise vector epsilon of length n = 100.
n=100
mean = 0
sd = 1

set.seed(8.1)
X = rnorm(n, mean, sd)
epsilon = rnorm(n, mean, sd)

#(b) Generate a response vector Y of length n = 100 according to
    #the model Y = β0 + β1 X + β2 X^2 + β3 X^3 + epsilon ,
    #where β0 , β1 , β2 , and β3 are constants of your choice

b0 = 1
b1 = 2
b2 = 3
b3 = 4   

Y = b0 + b1 * X + b2 * X^2 + b3 * X^3 + epsilon

# (c) Use the regsubsets() function to perform best subset selection
  # in order to choose the best model containing the predictors
  # X, X^2 , . . . , X^10 . What is the best model obtained according to
  # C p , BIC, and adjusted R^2 ? Show some plots to provide evidence
  # for your answer, and report the coefficients of the best model ob-
  #   tained. Note you will need to use the data.frame() function to
  # create a single data set containing both X and Y .

Data8 = data.frame(Y, X)
Data8.yx = Data8

for(i in 2:10){
  str = paste("X.",i, sep = "")
  Data8[,str] = X^i
}

head(Data8)

dataset = Data8

regfit.full=regsubsets(Y~.,data=dataset,nvmax=dim(dataset)[2]-1)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq

statistics.plots <- function(reg.summary){
  par(mfrow=c(2,2))
  plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
  
  #Adjusted R^2
  plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
  ar2.max = which.max(reg.summary$adjr2)
  points(ar2.max,reg.summary$adjr2[ar2.max], col="red",cex=2,pch=20)
  
  #Cp
  plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
  cp.min = which.min(reg.summary$cp)
  points(cp.min,reg.summary$cp[cp.min],col="red",cex=2,pch=20)
  
  # BIC
  bic.min = which.min(reg.summary$bic)
  plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
  points(bic.min,reg.summary$bic[bic.min],col="red",cex=2,pch=20)
  
  return(c(ar2.max, cp.min, bic.min))
}

best.selection.models <- statistics.plots(reg.summary)

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# We know the model has been generated with up to the 3rd power of X
# plus a random error. As expected the optimal model selected with all the
# 4 statistics is the one with 3 variables, X, x^2 and X^3. 
# We can output the coefficients of that model and compare it with the original
# coefficents
coef(regfit.full,3)
c(b0,b1,b2,b3)


# (d): Repeat (c), using forward stepwise selection and also using back-
  # wards stepwise selection. How does your answer compare to the
  # results in (c)?

# FORWARD STEPWISE SELECTION

regfit.fwd=regsubsets(Y~.,data=dataset,nvmax=dim(dataset)[2]-1
                      ,method="forward")
reg.summary.fwd = summary(regfit.fwd)

forward.selection.models <- statistics.plots(reg.summary.fwd)

# BACKWARDS STEPWISE SELECTION
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=dim(dataset)[2]-1
                      ,method="backward")
reg.summary.bwd = summary(regfit.bwd)

backward.selection.models <- statistics.plots(reg.summary.bwd)

# Comparison of the results
print("Best model according to: Adjusted R^2, Cp and BIC")
print("Best subset selection:")
print(best.selection.models)

print("Froward stepwise selection:")
print(forward.selection.models)

print("Backward stepwise selection:")
print(backward.selection.models)

# The results for the best subset selection and forward stepwise selection
# with the three performance metrics are the same ( 3 3 3), whereas for the
# backward stepwise selection case the results according to adjr2, cp and bic
# are 10, 10 and 8 respectively. We see that much more variables are retained.


##################################################################
##################################################################

# LASSO

#(e) Now fit a lasso model to the simulated data, again using X, X^2 ,
# . . . , X^10 as predictors. Use cross-validation to select the optimal
# value of λ. Create plots of the cross-validation error as a function
# of λ. Report the resulting coefficient estimates, and discuss the
# results obtained.

library(glmnet)

grid=10^seq(10,-2,length=100)

x = as.matrix(Data8[, -1])
y = Data8[, 1]

set.seed(8.2)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

par(mfrow=c(1,1))

lasso.mod = glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)


#PLOT OF MSE  vs lambda
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
names(cv.out)
plot(log(cv.out$lambda), cv.out$cvm, ylab = "MSE", 
     xlab = expression(log(lambda)))
plot(cv.out$lambda, cv.out$cvm, ylab = "MSE", xlab = expression(lambda) )

bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

bestlam.1se =cv.out$lambda.1se
lasso.pred.1se=predict(lasso.mod,s=bestlam.1se,newx=x[test,])
mean((lasso.pred.1se-y.test)^2)

# I this case, we obtain a lower MSE with the bestlambda.min than
#with the bestlambda.1se

#Model with best lambda min
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:11,]
lasso.coef
lasso.coef[lasso.coef!=0]

#Model with best lambda 1se

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam.1se)[1:11,]
lasso.coef
lasso.coef[lasso.coef!=0]


# We obtained a lower MSE with the bestlambda.min than
#with the bestlambda.1se. We also see that the model coefficients
# are closer to the real ones for the bestlambda.min model.

# Observe also that the LASSO regression models obtained 
# have selected directly the 3 predictors that in fact exists by construction
# of the data.

##############################################################################
##############################################################################
b7 = 8

Y2 = b0 + b7 * X^7 + epsilon

#Best subset selection

Data8f = data.frame(Y2, X)
Data8f.yx = Data8f

for(i in 2:10){
  str = paste("X.",i, sep = "")
  Data8f[,str] = X^i
}

head(Data8f.yx)

dataset = Data8f

regfit.full=regsubsets(Y2~.,data=dataset,nvmax=dim(dataset)[2]-1)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq

statistics.plots(reg.summary)

coef(regfit.full,1)
coef(regfit.full,2)

# We see that for Cp and BIC the best model is the one with one variable
# that is the predictor X^7, as expected.

# For the BIC, the number of predictors with best value is the one with
# 2 predictors. X^7 and X^10. This makes sense since the original data 
# comes from a normal with mean 0 and sd = 1, so there are small numbers 
# that become smaller for higher powers (in our case tha maximum power is 10)


# LASSO

grid=10^seq(10,-2,length=100)

x = as.matrix(Data8f[, -1])
y = Data8f[, 1]

set.seed(8.3)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

par(mfrow=c(1,1))

lasso.mod = glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)


#PLOT OF MSE  vs lambda
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
names(cv.out)
plot(log(cv.out$lambda), cv.out$cvm, ylab = "MSE", 
     xlab = expression(log(lambda)))
plot(cv.out$lambda, cv.out$cvm, ylab = "MSE", xlab = expression(lambda) )
plot(cv.out)

bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

bestlam.1se =cv.out$lambda.1se
lasso.pred.1se=predict(lasso.mod,s=bestlam.1se,newx=x[test,])
mean((lasso.pred.1se-y.test)^2)

# I this case, we obtain a lower MSE with the bestlambda.min than
#with the bestlambda.1se

#Model with best lambda min
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:11,]
lasso.coef
lasso.coef[lasso.coef!=0]

#Model with best lambda 1se

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam.1se)[1:11,]
lasso.coef
lasso.coef[lasso.coef!=0]

# DISCUSSION OF RESULTS

# With the lasso some nonzero predictors apart from X⁷ survive,
# so for this case it seems to be less effective, although note that
# the only high coefficient is the one in X^7. The others are of order 10^-1 or
# less.


