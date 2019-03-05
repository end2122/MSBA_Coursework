# Question 1 Setup
setwd("~/Columbia MSBA/Spring 2019/E4650 - Business Analytics/Assignments/Assignment 3")
ojfile <- read.csv("OrangeJuice.csv",header=TRUE,",")
ojfile = na.omit(ojfile)

# Question 1a
v1=rep(1,length(ojfile$Purchase))
v2=rep(0,length(ojfile$Purchase))
ojfile$Purchase=ifelse(ojfile$Purchase=="CH",v1,v2)

set.seed(1337)

library("caret")
ss = sample(1:3, size=nrow(ojfile),replace=TRUE,prob=c(0.5,0.25,0.25))
oj_train = ojfile[ss==1,]
oj_test = ojfile[ss==2,]
oj_validation = ojfile[ss==3,]

train = ss==1
test = ss==2
validation = ss==3
  
summary(oj_train)
names(oj_train)
summary(oj_test)
summary(oj_validation)

# Question 1b
glm.mod = glm(Purchase ~., data=oj_train, family=binomial)
summary(glm.mod)
coef(glm.mod)

# Question 1c
library("glmnet")
library("leaps")

x = model.matrix(Purchase~.,ojfile)
y = ojfile$Purchase

grid=10^(-3:3)
cv.out = cv.glmnet(x[train,],y[train], alpha=1, lambda=grid, family="binomial") 
bestlam = cv.out$lambda.min
bestlam

lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=bestlam, family="binomial")
lasso.mod
coef(lasso.mod)


# Question 1d
library("MASS")
library("ISLR")

lda.mod = lda(Purchase~.-X-WeekofPurchase-DiscCH-SpecialCH-SpecialMM-SalePriceMM-SalePriceCH-PriceDiff, data=ojfile, subset=train)

lda.pred = predict(lda.mod, oj_train)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class,oj_train$Purchase)

correct = mean(lda.class==oj_train$Purchase)
lda.error = 1-correct
lda.error

# Question 1e
library("class")

knn.error = array(0,dim=20)
print(as.data.frame(colnames(oj_train)))
oj_train = oj_train[-c(1,3,7,9,10,12,13,14)]
oj_train = na.omit(oj_train)
set.seed(1337)

for (i in 1:20){
  class_knn = knn.cv(oj_train[,-1],oj_train[,1], k=i)
  knn.error[i] = mean(class_knn!=oj_train[,1])
}

knn.best = which.min(knn.error)
knn.error
knn.best
knn.error[knn.best]

# Question 1f

#     log model
glm.probs = predict(glm.mod, newdata=oj_validation, type="response")
glm.pred = rep(1,nrow(oj_validation))
thresh = 0.61
glm.pred[glm.probs>thresh]=0
table(glm.pred,oj_validation$Purchase)
mean(glm.pred==oj_validation$Purchase)


#     lasso model
lasso.pred = predict(lasso.mod, x[validation,],type="class");
table(lasso.pred,y[validation])
lasso.error = sum(lasso.pred != y[validation])/length(lasso.pred)
lasso.error #0.1634

#     lda model
lda.pred = predict(lda.mod, newdata=oj_validation)
names(lda.pred)
lda.pred.class = lda.pred$class
table(lda.pred.class, oj_validation$Purchase)
lda.pred.error = mean(lda.pred.class!=oj_validation$Purchase)
lda.pred.error #0.1479

#     knn model
oj_train = ojfile[ss==1,]
oj_test = ojfile[ss==2,]
oj_validation = ojfile[ss==3,]

xTrain = oj_train[,-2]
xValidation = oj_validation[,-2]
yTrain = oj_train[,2]

KNNpred = knn(xTrain, xValidation, yTrain, k=knn.best)
knn.error2 = mean(KNNpred!=oj_validation$Purchase)
knn.error2 #0.2568


# Question 1g
oj_train = ojfile[ss==1,]
oj_test = ojfile[ss==2,]
oj_validation = ojfile[ss==3,]
oj_trainval = rbind(oj_train,oj_validation)
lda.mod2 = lda(Purchase~.-X-WeekofPurchase-DiscCH-SpecialCH-SpecialMM-SalePriceMM-SalePriceCH-PriceDiff, data=oj_trainval, subset=train)

lda.pred2 = predict(lda.mod2, newdata=oj_test)
names(lda.pred2)
lda.pred2.class = lda.pred2$class
table(lda.pred2.class, oj_test$Purchase)
lda.pred.error2 = mean(lda.pred2.class!=oj_test$Purchase)
lda.pred.error2 #0.1692

lda.mod2

# Question 1h
oj_train = ojfile[ss==1,]
oj_test = ojfile[ss==2,]
oj_validation = ojfile[ss==3,]
oj_trainval = rbind(oj_train,oj_validation)
oj_trainval = oj_trainval[,-1]
#     CH=1; MM=0

costMat = matrix(c(3.5, -0.5, 0, 0), 2, 2)
print(costMat)
 thres.vec = seq(0.02, 1, by = 0.01)

worst = sum(oj_train$Purchase==0)*-0.5
worst
best = sum(oj_train$Purchase==1)*3.5
best

glm.mod = glm(Purchase ~., data=oj_train, family=binomial)
glm.probs = predict(glm.mod, newdata=oj_validation, type="response")
glm.pred = rep(0,nrow(oj_validation))
glm.actual = oj_validation$Purchase

computeNetBenefit = function(prob, label, costMat, thres.vec) {
  cost.vec = rep(NA, length(thres.vec))
  StatusQuo = sum(table(label, numeric(length(label))) * costMat[,1])
  for (i in 1:length(thres.vec)) {
    thres = thres.vec[i]
    pred = ifelse(prob > thres, 1,0)
    classficationTable = table(truth=label, predict=pred )
    if (ncol(classficationTable) == 1) {
      newClassficationTable = matrix(0, 2, 2)
      if (sum(pred==0)==0) {
        newClassficationTable[,2] = classficationTable[,1]
      } else { newClassficationTable[,1] = classficationTable[,1] }
      classficationTable = newClassficationTable
    }
    cost.vec[i] = sum(as.matrix(classficationTable) * costMat)
  }
  netBenefit = cost.vec
  return(netBenefit)
}

netBenefit = computeNetBenefit(glm.probs, glm.actual, costMat, thres.vec)
best.thresh = thres.vec[which.max(netBenefit)]
best.thresh
netBenefit = computeNetBenefit(glm.probs, glm.actual, costMat, best.thresh)
netBenefit


