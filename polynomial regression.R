library(ISLR)
library(leaps)

set.seed(1992)
X<-rnorm(100,2,2)
epsilon<-rnorm(100,0,1)
beta_0<-3
beta_1<--1.5
beta_2<-4.5
beta_3<-0.75
Y<-beta_0+beta_1*X+beta_2*(X^2)+beta_3*(X^3)+epsilon

data <- data.frame(X,Y)
head(data)

#regsubset feature selection:
regfit=regsubsets(Y~X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8)+I(X^9)+I(X^10), data=data, nvmax=11)
reg.summary<-summary(regfit)

reg.summary

png("info criteria plots.png")
par(mfrow=c(2,2))
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
dev.off()

#find number of variables associated with min of info criterions
which(reg.summary$cp==min(reg.summary$cp))
which(reg.summary$bic==min(reg.summary$bic))
which(reg.summary$adjr==max(reg.summary$adjr))

#best model incudes 3 variables. According to re.summary these would be X, X^2, X^3

coef(regfit,3) #fit best model

#the above can be repeated using the forward and backward selection methods by changing the method variable in the regsubsets function.
#eg regsubsets(.....,method="forward")



#Lasso regression for feature selection
install.packages("bestglm","glmnet")
require(bestglm)
require(glmnet)

X_matrix <- poly(as.matrix(data[,1]) ,degree=10, raw=TRUE)


set.seed(1)

lasso_cv <- cv.glmnet(X_matrix, Y, alpha=1)
plot(lasso_cv)
best<-lasso_cv$lambda.min
best

lasso_cv_new <- glmnet(X_matrix, Y, alpha=1, lambda=best)
coef(lasso_cv_new)


#fitting the new model
lm.fit<-lm(Y~I(X^2)+I(X^3)+I(X^4), data=data)
summary(lm.fit)

png("Scatter plot with regression line.png")
plot(X,Y, main="Scatter plot with polynomia regression line")
aRng = range(X)
a_predict = seq( from=aRng[1], to=aRng[2], length.out=100 )
w_predict = predict( lm.fit, newdata=list( X=a_predict ) )
lines( a_predict, w_predict, col='red' )
dev.off()