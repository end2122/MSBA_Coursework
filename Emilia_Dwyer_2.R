# Question 1 Setup
setwd("~/Columbia MSBA/Spring 2019/E4650 - Business Analytics/Assignments/Assignment 2")
gefile <- read.csv("GE.csv",header=TRUE,",")

# Question 1a
library("zoo")
ZooClose <- zoo(gefile$Close)
ZooOneDayLag <- lag(ZooClose, k=-1, na.pad=TRUE)
ZooFiveDayLag <- lag(ZooClose, k=-5, na.pad=TRUE)
ZooOneDayAhead <- lag(ZooClose, k=1, na.pad=TRUE)
ZooOneDayLag <- as.data.frame(ZooOneDayLag)
ZooFiveDayLag <- as.data.frame(ZooFiveDayLag)
ZooOneDayAhead <- as.data.frame(ZooOneDayAhead)
gefile <- cbind(gefile,ZooOneDayLag,ZooFiveDayLag,ZooOneDayAhead)

gefile$OneDayReturn =  (gefile$Close - gefile$ZooOneDayLag)/(gefile$ZooOneDayLag)
gefile$FiveDayReturn = (gefile$Close - gefile$ZooFiveDayLag)/(gefile$ZooFiveDayLag)
gefile$OneDayActualReturn = (gefile$ZooOneDayAhead - gefile$Close)/(gefile$Close)

mean(gefile$OneDayReturn, na.rm = TRUE)
mean(gefile$FiveDayReturn, na.rm = TRUE)
mean(gefile$OneDayActualReturn, na.rm = TRUE)

# Question 1b
gefile$Date = as.Date(gefile$Date)
ge_train <- with(gefile, gefile[(Date>='2016-01-01' & Date<='2016-12-31'),])
tail(ge_train)

ge_train_LM = lm(OneDayActualReturn ~OneDayReturn+FiveDayReturn, data=ge_train)
summary(ge_train_LM)

# Question 1c
ge_2017 <- with(gefile, gefile[(Date>='2017-01-01' & Date<='2017-12-31'),])

ge_predict = predict(ge_train_LM, newdata=ge_2017)
ge_predict = as.data.frame(ge_predict)
ge_2017 <- cbind(ge_2017,ge_predict)

v1=rep(1,length(ge_2017$ge_predict))
v2=rep(0,length(ge_2017$ge_predict))
ge_2017$Long1=ifelse(ge_2017$ge_predict>=0,v1,v2)

ge_2017$StratOneDayReturn = ifelse((ge_2017$Long1==1 & ge_2017$OneDayActualReturn>0 | ge_2017$Long1==0 & ge_2017$OneDayActualReturn<0),
                                   abs(ge_2017$OneDayActualReturn),-abs(ge_2017$OneDayActualReturn))
mean(ge_2017$StratOneDayReturn, na.rm = TRUE)


#######################################################################################################


# Question 2 Setup
setwd("~/Columbia MSBA/Spring 2019/E4650 - Business Analytics/Assignments/Assignment 2")
collegedatafile <- read.csv("CollegeData.csv",TRUE,",")
collegedatadictionaryfile <- read.csv("CollegeDataDictionary.csv",TRUE,",")
nrow(collegedatafile)

# Quesion 2a
collegedatafile = na.omit(collegedatafile,invert=FALSE)
nrow(collegedatafile)

# Question 2b
collegedatafile$sqrtCOSTT4_A = sqrt(collegedatafile$COSTT4_A)
collegedatafile$sqrtTUITIONFEE_OUT = sqrt(collegedatafile$TUITIONFEE_OUT)
collegedatafile$sqrtTUITFTE = sqrt(collegedatafile$TUITFTE)
collegedatafile$sqrtAVGFACSAL = sqrt(collegedatafile$AVGFACSAL)

#  Introduce 6 covariates
collegedatafile$mult1 = (collegedatafile$COSTT4_A)*(collegedatafile$TUITIONFEE_OUT)
collegedatafile$mult2 = (collegedatafile$COSTT4_A)*(collegedatafile$TUITFTE)
collegedatafile$mult3 = (collegedatafile$COSTT4_A)*(collegedatafile$AVGFACSAL)
collegedatafile$mult4 = (collegedatafile$TUITIONFEE_OUT)*(collegedatafile$TUITFTE)
collegedatafile$mult5 = (collegedatafile$TUITIONFEE_OUT)*(collegedatafile$AVGFACSAL)
collegedatafile$mult6 = (collegedatafile$TUITFTE)*(collegedatafile$AVGFACSAL)

mean(collegedatafile$mult1, na.rm=TRUE)
mean(collegedatafile$mult2, na.rm=TRUE)
mean(collegedatafile$mult3, na.rm=TRUE)
mean(collegedatafile$mult4, na.rm=TRUE)
mean(collegedatafile$mult5, na.rm=TRUE)
mean(collegedatafile$mult6, na.rm=TRUE)


# Question 2c
set.seed(4574)
collegedatafile = na.omit(collegedatafile)
train = sample(1:nrow(collegedatafile),0.75*nrow(collegedatafile))
test = -train

collegedata_train = collegedatafile[train,]
collegedata_test = collegedatafile[test,]

mean(collegedata_train$SAT_AVG)
mean(collegedata_test$SAT_AVG)


# Question 2d
###install.packages("leaps")
###install.packages("glmnet")
library("glmnet")
library("leaps")

collegedata_train = na.omit(collegedata_train)
collegedata_test = na.omit(collegedata_test)

p = 8
k = 5
regfit.fwd=regsubsets(SAT_AVG~.-INSTNM, data=collegedata_train, nvmax=p, method="forward")

predict.regsubsets=function(regfit.fwd,newdata,t){
  form=as.formula(regfit.fwd$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(regfit.fwd,id=t)
  xvars=names(coefi)
  pred = mat[,xvars]%*%coefi
  return(pred)
}

folds=sample(1:k,nrow(collegedata_train),replace=TRUE)
cv.errors=array(NA,dim=c(k,p)) 

for(j in 1:k){
  best.fit = regsubsets(SAT_AVG~.-INSTNM,data=collegedata_train[folds!=j,],nvmax=p,method="forward")
  for(t in 1:p){
    pred = predict.regsubsets(best.fit,collegedata_train[folds==j,],t)
    actual = collegedata_train$SAT_AVG[folds==j]
    cv.errors[j,t] = mean((actual-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors

best.model = which.min(mean.cv.errors)
best.model

regfit.full = regsubsets(SAT_AVG~.-INSTNM,data=collegedata_train, nvmax=p)

pred=predict.regsubsets(regfit.full, collegedata_test, best.model)
actual = collegedata_test$SAT_AVG
mean((actual - pred)^2) 

reg.summary=summary(regfit.full)
names(reg.summary)
coef(regfit.full,best.model)


# Question 2e
set.seed(4574)

collegedatafile = na.omit(collegedatafile)
train = sample(1:nrow(collegedatafile),0.75*nrow(collegedatafile))
test = -train

collegedata_train = collegedatafile[train,]
collegedata_test = collegedatafile[test,]

library("glmnet")
library("leaps")

collegedata_train = na.omit(collegedata_train)
collegedata_test = na.omit(collegedata_test)

x=model.matrix(SAT_AVG~.-INSTNM,collegedatafile)
y=collegedatafile$SAT_AVG

grid=10^(-4:2)
cv.out = cv.glmnet(x[train,],y[train], alpha=1, lambda=grid, nfolds=5) 
bestlam = cv.out$lambda.min
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=bestlam)
bestlam

coef(lasso.mod)



# Question 2f
pred = predict(lasso.mod, x[test,])
actual = y[test]
mean((actual-pred)^2) 


# Question 2g
#see pdf
