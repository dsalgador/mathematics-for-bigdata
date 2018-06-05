# Author: Daniel Salgado Rojo
# Exercise 9 from the book: An Introduction to Statistical Learning
library(glmnet)
library(leaps)
library(ISLR)

# In this exercise, we will predict the number of applications received
# using the other variables in the College data set.

attach(College)
names(College)
College = na.omit(College)
names(College)
# (a) Split the data set into a training set and a test set.
predictor_column = 2 
#We also remove the cathegorical 'Private' predictor
x = as.matrix(College[, -c(predictor_column,1)])
y = College[, predictor_column]
#colnames(y) = "Apps"
#colnames(x) = colnames(College)[3:length(colnames(College))]

set.seed(9.1)
#We do 80% train and 20% test
train=sample(1:nrow(x), round(8*nrow(x)/10) )
test=(-train)
y.test=y[test]

# (b) Fit a linear model using least squares on the training set, and
# report the test error obtained.


grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
y.pred = predict(ridge.mod,s=0,newx = x[test,])
mse.ls = mean( (y.test - y.pred)^2           )
mse.ls



# (c) Fit a ridge regression model on the training set, with λ chosen
# by cross-validation. Report the test error obtained.

par(mfrow=c(1,1))

set.seed(9.2)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)

bestlam=cv.out$lambda.min
ridge.pred=predict(ridge.mod,s=bestlam,newx=as.matrix(x[test,]))
mse.min.ridge = mean((ridge.pred-y.test)^2)

bestlam.1se =cv.out$lambda.1se
ridge.pred.1se=predict(ridge.mod,s=bestlam.1se,newx=x[test,])
mse.1se.ridge = mean((ridge.pred.1se-y.test)^2)

# (d) Fit a lasso model on the training set, with λ chosen by cross-
#   validation. Report the test error obtained, along with the num-
#   ber of non-zero coefficient estimates.

lasso.mod = glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
names(cv.out)
plot(log(cv.out$lambda), cv.out$cvm, ylab = "MSE", 
     xlab = expression(log(lambda)))
plot(cv.out$lambda, cv.out$cvm, ylab = "MSE", xlab = expression(lambda) )

bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mse.min.lasso = mean((lasso.pred-y.test)^2)

bestlam.1se =cv.out$lambda.1se
lasso.pred.1se=predict(lasso.mod,s=bestlam.1se,newx=x[test,])
mse.1se.lasso = mean((lasso.pred.1se-y.test)^2)

#Model with best lambda min
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:17,]
#lasso.coef
nonzerocoeffs.min = length(lasso.coef[lasso.coef!=0])

#Model with best lambda 1se

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam.1se)[1:17,]
#lasso.coef
nonzerocoeffs.1se = length(lasso.coef[lasso.coef!=0])


# (g) Comment on the results obtained. How accurately can we pre-
#   dict the number of college applications received? Is there much
# difference among the test errors resulting from these five ap-
#   proaches?

#Report
print("Least squares")
print(paste("MSE:", mse.ls))

print("Ridge")
print(paste("MSE.min:", mse.min.ridge))
print(paste("MSE.1se:", mse.1se.ridge))


print("Lasso")
print(paste("MSE.min:", mse.min.lasso, "Nº non-zero coefficents:",
            nonzerocoeffs.min))
print(paste("MSE.1se:", mse.1se.lasso, "Nº non-zero coefficents:",
            nonzerocoeffs.1se))

mean(Apps)
head(Apps)

#####
# We observe that the test errors are of the order of 10^6 in all the models
# and the mean value of the data is in the order of 10^3. Thus, this suggests that
# these linear models are not good to describe the data we have been working with.
#####


# [1] "Least squares"
# [1] "MSE: 1036993.07573663"
# 
# [1] "Ridge"
# [1] "MSE.min: 1126281.55533838"
# [1] "MSE.1se: 1412740.95831475"
# 
# [1] "Lasso"
# [1] "MSE.min: 1018854.40373143 Nº non-zero coefficents: 15"
# [1] "MSE.1se: 1263467.21447022 Nº non-zero coefficents: 4"
# 
# mean(Apps)
# [1] 3001.638

