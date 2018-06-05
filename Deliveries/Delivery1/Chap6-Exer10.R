# Author: Daniel Salgado Rojo
# Exercise 10 from the book: An Introduction to Statistical Learning
library(glmnet)
library(leaps)

# We have seen that as the number of features used in a model increases,
# the training error will necessarily decrease, but the test error may not.
# We will now explore this in a simulated data set.


# (a) Generate a data set with p = 20 features, n = 1,000 observa-
  #   tions, and an associated quantitative response vector generated
  # according to the model
  # Y = Xβ + epsilon,
  # where β has some elements that are exactly equal to zero.

p = 20
n = 1000

#Exponentials, normals 

# lambdas = seq(0.1, 5.1, 0.5)
# mus = seq(0,10,1)
# sigmas = seq(1, 2,0.1)
set.seed(10.0)
lambdas = abs(runif(p/2,0, 300))
mus = abs(runif(p/2,0, 200))
sigmas = abs(runif(p/2,0, 40))

Data10 = data.frame(X1 = rep(0,n))

set.seed(10.1)

for(i in 1:(p/2)){
  str = paste("X",i, sep = "")
  Data10[,str] = rnorm(n, mus[i], sigmas[i])
}

for(i in (p/2+1):p){
  str = paste("X",i, sep = "")
  Data10[,str] =  rexp(n, lambdas[i-p/2])
}

betas = sample(c(0,0,0,0,0,0:p),p, replace = TRUE)*0.005
sd = 10
epsilon = rnorm(n, mean, sd)

x = Data10
y = as.matrix(x) %*% as.matrix(betas) + as.matrix(epsilon)


# (b) Split your data set into a training set containing 100 observations
# and a test set containing 900 observations.

ntrain = 900
ntest = n - ntrain

train = sample(1:nrow(x), ntrain)
test=(-train)
#y.test=y[test]

regfit.full=regsubsets(y~.,data=x,nvmax=dim(x)[2])
reg.summary=summary(regfit.full)

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

#We plot RSS instead of MSE since MSE = 1/n RSS
print(statistics.plots(reg.summary))



# (c) Perform best subset selection on the training set, and plot the
# training set MSE associated with the best model of each size.


regfit.train=regsubsets(y[train,]~.,data=x[train,],nvmax=dim(x)[2])
reg.summary=summary(regfit.train)
print(statistics.plots(reg.summary))





# (d) Plot the test set MSE associated with the best model of each
# size.

########
dataset = cbind(data.frame(Y = y),x)

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
# regfit.best=regsubsets(Y~.,data=dataset,nvmax=p)
# coef(regfit.best,10)

k=10
set.seed(1)
folds=sample(1:k,nrow(dataset),replace=TRUE)
cv.errors=matrix(NA,k,p, dimnames=list(NULL, paste(1:p)))

for(j in 1:k){
  best.fit=regsubsets(Y~.,data=dataset[folds!=j,],nvmax=p)
  for(i in 1:p){
    pred=predict.regsubsets(best.fit,dataset[folds==j,],id=i)
    cv.errors[j,i]=mean( (dataset$Y[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
best_number_of_parameters = which.min(mean.cv.errors)
print(best_number_of_parameters )

reg.best=regsubsets(Y~.,data=dataset, nvmax=p)
print(coef(reg.best,best_number_of_parameters ))





# (e) For which model size does the test set MSE take on its minimum
# value? Comment on your results. If it takes on its minimum value
# for a model containing only an intercept or a model containing
# all of the features, then play around with the way that you are
# generating the data in (a) until you come up with a scenario in
# which the test set MSE is minimized for an intermediate model
# size.

mean.cv.errors[mean.cv.errors == min(mean.cv.errors)]
# The model size where the test MSE takes its minima is 10


# (f) How does the model at which the test set MSE is minimized
# compare to the true model used to generate the data? Comment
# on the coefficient values.

print(round(as.vector(coef(reg.best,best_number_of_parameters)), 3)[-1]   )
print(betas[c(2,3,4,5,6,7,8,9,10,15)])

# estimated: [1] 0.015 0.060 0.080 0.090 0.100 0.095 0.070 0.055 0.080 0.025
# real ones: [1] 0.032 0.059 0.079 0.074 0.080 0.085 0.086 0.056 0.071 -13.167

#We can see that most of them are similar in value, but something strange happens
# with the 15th coefficient which is about three orders of magnitude greater than
# the real model value.

# (g) Create a plot displaying
# j=1 (β j − β̂ j )
#for a range of values
# of r, where β̂ j r is the jth coefficient estimate for the best model
# containing r coefficients. Comment on what you observe. How
# does this compare to the test MSE plot from (d)?

beta_indexs = list()
beta_coefs = list()
for(r in 1:p){
  coefs = coef(reg.best,r)
  beta_coefs[[r]] = coefs
  beta_index = list()
  for(i in 1:r){
    beta_index[[i]] = strtoi(substr(names(coefs)[i+1], 2,length(coefs)))
  }
  
  beta_indexs[[r]] = beta_index
}

beta_errors = numeric(length(betas))

for(i in 1:length(betas)){
  for(j in 1:i){
    beta_errors[i] = beta_errors[i] + (betas[ beta_indexs[[j]][[1]] ] - beta_coefs[[i]][[j+1]])^2
  }
  beta_errors[i] = sqrt(beta_errors[i])
}

plot(beta_errors)

# We can see that from 0 to 10 the beta errors are almost zero, and just
# between 9 and 10 they start to increase very quickly until they stabilize to some 
# value from the 15th index on

#The similarity with the mean crossvalidation errors is that in that case, between 9 and
#and 10 was where the minima of the errors was located.
