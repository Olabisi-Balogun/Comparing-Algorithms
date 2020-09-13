library(ggplot2)
library(class)
p=2 #dimension of data
csize = 10 #number of centers of each class of data
#train_num = 100 #size of training data for each class
s=1 #standard deviation to generate mean centers
sd = sqrt(1/5) #standard deviation to generate data based on mean centers

#simulate 20 centers; 10 for each class 
m0 = matrix(rnorm(csize*p), csize,p)*s + cbind(rep(1,csize), rep(0,csize))
m1 = matrix(rnorm(csize*p), csize,p)*s + cbind(rep(0,csize), rep(1,csize))
generateTrainData = function(train_num=100){
    id0 = sample(1:csize,train_num, replace = TRUE)
    id1 = sample(1:csize,train_num, replace = TRUE)
    #simulate 10 centers for class 0
    train_d0 = matrix(rnorm(train_num*p), train_num,p)*sd +m0[id0,]
    Ytrain.d0 = (rep(0,train_num))
    train_d0.Y = cbind(train_d0,Ytrain.d0)
    #simululate data for class 1
    train_d1 = matrix(rnorm(train_num*p), train_num,p)*sd +m1[id1,]
    Ytrain.d1 = (rep(1,train_num))
    train_d1.Y = cbind(train_d1,Ytrain.d1)
    
    
    data_train = rbind(train_d0.Y, train_d1.Y)
    data_train.df = data.frame(X1=data_train[,1],X2=data_train[,2],Y=data_train[,"Ytrain.d0"])
    
    #reshuffle the train data
    rows = sample(nrow(data_train.df))
    data_train.df = data_train.df[rows,]
    nrow(data_train.df)
    return (data_train.df)
}

#generate test data
#test.size = 5000
generateTestData = function(test.size = 5000){
  id0.test = sample(1:csize,test.size, replace = TRUE)
  id1.test = sample(1:csize,test.size, replace = TRUE)
  # test.data = matrix(rnorm(2*test.size*p),2*test.size,p)*sd+
  #   rbind(m0.test[id0.test,],m1.test[id1.test,])
  test.data = matrix(rnorm(2*test.size*p),2*test.size,p)*sd+
    rbind(m0[id0.test,],m1[id1.test,])
  Ytest = c(rep(0,test.size),rep(1,test.size))
  test.data.df = data.frame(X1 = test.data[,1],X2=test.data[,2],Y=Ytest)
  head(test.data.df)
  #reshuffle test data
  test.rows = sample(nrow(test.data.df))
  test.data.df = test.data.df[test.rows,]
  return(test.data.df)
}


#Least Square Method
least.square = function(data_train.df, test.data.df, name="Linear"){
  lm.model = lm(Y~., data=data_train.df)
  Y.hat=as.numeric(lm.model$fitted.values>0.5)
  table(data_train.df$Y, Y.hat)
  train.err.LS = mean(data_train.df$Y != Y.hat)
  train.err.LS
  
  #calculate test error
  Y.hat.test = lm.model$coefficients[1]+lm.model$coefficients[2]*test.data.df$X1+lm.model$coefficients[3]*test.data.df$X2
  Y.hat.test = as.numeric(Y.hat.test>0.5)
  table(test.data.df$Y, Y.hat.test)
  test.err.LS = mean(test.data.df$Y != Y.hat.test)
  test.err.LS
  #data.frame(train_error = train.err.LS, test_error = test.err.LS, Algorithm = name) 
  d1 = data.frame(error = train.err.LS, type="train", algorithm=name)
  d2 = data.frame(error = test.err.LS, type="test", algorithm=name)
  
  df = rbind(d1, d2)
  return(df)
}

#Quadratic Regression 
quadratic = function(data_train.df, test.data.df, name="Quadratic"){
  #Quadratric Regression
  quadratic.model = lm(Y~X1+X2+X1^2+X2^2+X1*X2, data = data_train.df)
 
  #calculate train error
  Ytrain.hat.quad = as.numeric(quadratic.model$fitted.values>0.5)
  table(data_train.df$Y, Ytrain.hat.quad)
  train.err.quad = mean(data_train.df$Y != Ytrain.hat.quad)
  train.err.quad
  
  #calculate the test error
  Ytest.hat.quad = quadratic.model$coefficients[1]+quadratic.model$coefficients[2]*test.data.df$X1+
    quadratic.model$coefficients[3]*test.data.df$X2+quadratic.model$coefficients[4]*test.data.df$X1*test.data.df$X2
  Ytest.hat.quad = as.numeric(Ytest.hat.quad>0.5)
  table(test.data.df$Y, Ytest.hat.quad)
  test.err.quad = mean(test.data.df$Y != Ytest.hat.quad)
  test.err.quad
  
  d1 = data.frame(error = train.err.quad, type="train", algorithm=name)
  d2 = data.frame(error = test.err.quad, type="test", algorithm=name)
  
  df = rbind(d1, d2)
  return(df)
}

#KNN
#cross-validation for knn algorithm
num.fold = 10
fold= cut(seq(1, nrow(data_train.df)), breaks = 10, labels = FALSE)


#Cross Validation function
cross.validate = function(train_dataset,k){
  fold.num = 10
  #fold.size = floor(nrow(train_dataset)/fold.num)
  error = 0
  for (i in 1:fold.num) {
    #partition data by fold
    testIndexes = which(fold ==i, arr.ind = TRUE)
    
    te.data = train_dataset[testIndexes,]
    tr.data = train_dataset[-testIndexes,]
    
    #partition test data into predictor and response variables
    X.te.data = subset(te.data, select=-Y)
    y.te.data = as.factor(te.data$Y)
    
    #partition train data into predictor and response variables 
    X.tr.data = subset(tr.data, select=-Y)
    y.tr.data = as.factor(tr.data$Y)
    
    #predict training labels
    pred.ytrain = knn(train = X.tr.data, test = X.tr.data, cl=y.tr.data, k=k)
    
    #predict test labels
    pred.ytest = knn(train = X.tr.data, test = X.te.data, cl=y.tr.data, k=k)
    error = error +sum(pred.ytest != y.te.data)
  }
  error = error/nrow(data_train.df)
  data.frame(cvError = error)
}

get.best.k = function(data_train.df){
  #validation errors
  validation.error = NULL
  #generate random k that would be used for the knn algorithm
  k.neighbor = nrow(data_train.df)
  k.list = c(3, seq(5, k.neighbor, 6))
  
  #Another method
  for (j in k.list){
    temp = cross.validate(data_train.df, j)
    temp$neighbor = j
    validation.error = rbind(validation.error, temp)
  }
  #get minimum cv error
  min.cv.error = validation.error[which(validation.error$cvError == min(validation.error$cvError)),]
  #get K with minimum cv error
  best.k = max(min.cv.error$neighbor)
  return (best.k)
}

knn.error = function(data_train.df,test.data.df,best.k, name = "KNN"){
  #calculate training error bases on best K
  Y.train.label = as.factor(data_train.df$Y)

  pred.Y.train.knn = knn(train = subset(data_train.df, select = -Y), test = subset(data_train.df, select = -Y), cl = Y.train.label, k=best.k)
  #train.err.KNN = error.rate((as.factor(pred.Y.train.knn)),Y.train.label)
  train.err.KNN = mean(pred.Y.train.knn != Y.train.label)
  #calculate test error bases on best K
  Y.test.label = as.factor(test.data.df$Y)

  pred.Y.test.knn = knn(train = subset(data_train.df, select = -Y), test = subset(test.data.df, select = -Y), cl = Y.train.label, k=best.k)
  #test.err.KNN = error.rate((as.factor(pred.Y.test.knn)),Y.test.label)
  test.err.KNN = mean(pred.Y.test.knn != Y.test.label)
  
  d1 = data.frame(error = train.err.KNN, type="train", algorithm=name)
  d2 = data.frame(error = test.err.KNN, type="test", algorithm=name)
  
  df = rbind(d1, d2)
  return(df)
}

#Bayes Error
bayes = function(data_train.df, test.data.df, name="Bayes"){
    mixnorm = function(x){
    #return a density ration for a point x
    sum(exp(-apply((t(m1)-x)^2, 2, sum)*5/2)) /sum(exp(-apply((t(m0)-x)^2, 2, sum)*5/2))
  }
  #calculate test error
  Ytest.hat.Bayes = apply(subset(test.data.df, select=-Y), 1, mixnorm)
  Ytest.hat.Bayes = as.numeric(Ytest.hat.Bayes>1)
  table(test.data.df$Y, Ytest.hat.Bayes)
  test.err.Bayes = mean(test.data.df$Y != Ytest.hat.Bayes)
  test.err.Bayes
  
  #calculate train errror
  Ytrain.hat.Bayes = apply(subset(data_train.df, select=-Y), 1, mixnorm)
  Ytrain.hat.Bayes = as.numeric(Ytrain.hat.Bayes>1)
  table(data_train.df$Y, Ytrain.hat.Bayes)
  train.err.Bayes = mean(data_train.df$Y != Ytrain.hat.Bayes)
  train.err.Bayes
  
  d1 = data.frame(error = train.err.Bayes, type="train", algorithm=name)
  d2 = data.frame(error = test.err.Bayes, type="test", algorithm=name)
  
  df = rbind(d1, d2)
  return(df)
}

Error = NULL
#store best k-neighbor for 20 simulations
best.k.list = c()
#Simulate process 20 times
set.seed(1368)
for (i in 1:20) {
  #simulate the training data and the test data
  train.data = generateTrainData()
  test.data = generateTestData()
  tmp = least.square(train.data, test.data)
  tmp.quad = quadratic(train.data, test.data)
  #get the best k for knn
  best.k = get.best.k(train.data)
  best.k.list = append(best.k.list, best.k)
  tmp.knn = knn.error(train.data, test.data, best.k)
  tmp.bayes = bayes(train.data, test.data)
  Error = rbind(Error, tmp)
  Error = rbind(Error, tmp.quad)
  Error = rbind(Error, tmp.knn)
  Error = rbind(Error, tmp.bayes)
  }

head(Error,15)
ggplot(Error, aes(algorithm,error, color=type))+geom_boxplot()+labs(title="Test Error/Train Error Comparison by Algorithms", color="Error Type")
nrow(Error)
par(mfrow=c(1,1))
barplot(best.k.list, main="Best k selected by 10-fold Cross-Validation for KNN",xlab = "k")
