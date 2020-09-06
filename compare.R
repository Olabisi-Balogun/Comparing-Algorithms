library(ggplot2)
library(class)
#simulate 10 centers for each class 
set.seed(1)
p=2
csize = 10
train_num = 100
sd = 0.2
s=1
#simulate 20 centers; 10 for each class 
#simulate 10 centers for class 0
m0 = matrix(rnorm(csize*p), csize,p)*s + cbind(rep(1,csize), rep(0,csize))
m1 = matrix(rnorm(csize*p), csize,p)*s + cbind(rep(0,csize), rep(1,csize))
#simulate training data for class 0
#get random index for mean
#train.d0.size = 100
id0 = sample(1:csize,train_num, replace = TRUE)
id1 = sample(1:csize,train_num, replace = TRUE)
train_d0 = matrix(rnorm(train_num*p), train_num,p)*sd +m0[id0,]
Ytrain.d0 = (rep(0,train_num))
train_d0.Y = cbind(train_d0,Ytrain.d0)
#simululate data for class 1
train_d1 = matrix(rnorm(train_num*p), train_num,p)*sd +m1[id1,]
Ytrain.d1 = (rep(1,train_num))
train_d1.Y = cbind(train_d1,Ytrain.d1)
head(train_d1.Y)

data_train = rbind(train_d0.Y, train_d1.Y)
data_train.df = data.frame(X1=data_train[,1],X2=data_train[,2],Y=data_train[,"Ytrain.d0"])
head(data_train.df)

#generate test data
test.size = 5000
m0.test = matrix(rnorm(csize*p), csize,p)*s + cbind(rep(1,csize), rep(0,csize))
m1.test = matrix(rnorm(csize*p), csize,p)*s + cbind(rep(0,csize), rep(1,csize))
id0.test = sample(1:csize,test.size, replace = TRUE)
id1.test = sample(1:csize,test.size, replace = TRUE)
test.data = matrix(rnorm(2*test.size*p),2*test.size,p)*sd+
  rbind(m0.test[id0.test,],m1.test[id1.test,])
Ytest = c(rep(0,test.size),rep(1,test.size))
test.data.df = data.frame(X1 = test.data[,1],X2=test.data[,2],Y=Ytest)
#Least Square Method
lm.model = lm(Y~., data=data_train.df)
Y.hat=as.numeric(lm.model$fitted.values>0.5)
table(data_train.df$Y, Y.hat)
train.err.LS = sum(data_train.df$Y != Y.hat2)/(2*train_num)
train.err.LS

#calculate test error
Y.hat.test = lm.model$coefficients[1]+lm.model$coefficients[2]*test.data[,1]+lm.model$coefficients[3]*test.data[,2]
Y.hat.test = as.numeric(Y.hat.test>0.5)
table(test.data.df$Y, Y.hat.test)
test.err.LS = sum(test.data.df$Y != Y.hat.test)/(2*test.size)
test.err.LS

#Quadratric Regression
quadratic.model = lm(Y~X1+X2+X1^2+X2^2+X1*X2, data = data_train.df)
summary(quadratic.model)

#calculate train error
Ytrain.hat.quad = as.numeric(quadratic.model$fitted.values>0.5)
table(data_train.df$Y, Ytrain.hat.quad)
train.err.quad = sum(data_train.df$Y != Ytrain.hat.quad)/(2*train_num)
train.err.quad
#calculate the test error
Ytest.hat.quad = quadratic.model$coefficients[1]+quadratic.model$coefficients[2]*test.data[,1]+
  quadratic.model$coefficients[3]*test.data[,2]+quadratic.model$coefficients[4]*test.data[,1]*test.data[,2]
Ytest.hat.quad = as.numeric(Ytest.hat.quad>0.5)
table(test.data.df$Y, Ytest.hat.quad)
test.err.quad = sum(test.data.df$Y != Ytest.hat.quad)/(2*test.size)
test.err.quad
