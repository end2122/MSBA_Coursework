# Question 1 Setup
getwd()
setwd("~/Columbia MSBA/Spring 2019/E4650 - Business Analytics/Assignments/Assignment 1")
getwd()
eggsfile <- read.csv("EggProduction.csv",TRUE,",")
class(eggsfile)
head(eggsfile)

# Question 1a
summary(eggsfile)

# Question 1b.1
EFregression = lm(eggs ~feed,data=eggsfile)
summary(EFregression)
attach(eggsfile)
plot(feed,eggs)
abline(EFregression)

# Question 1b.2
eggsunder25 = eggsfile[eggsfile$feed<25,]
attach(eggsunder25)
eggsunder25LM = lm(eggs ~feed, data=eggsunder25)
summary(eggsunder25LM)
plot(feed,eggs)
abline(eggsunder25LM)

# Question 1b.3
eggsover25 = eggsfile[eggsfile$feed>25,]
attach(eggsover25)
eggsover25LM = lm(eggs ~feed, data=eggsover25)
summary(eggsover25LM)
plot(feed,eggs)
abline(eggsover25LM)

# Question 1c
attach(eggsfile)
EFTregression = lm(eggs ~feed+temperature,data=eggsfile)
summary(EFTregression)
plot(feed+temperature,eggs)

# Question 1d
attach(eggsfile)
plot(temperature,feed)
str(eggsfile)
v1=rep(1,length(eggsfile$temperature))
v2=rep(0,length(eggsfile$temperature))
eggsfile$temperature1=ifelse(eggsfile$temperature<35 & eggsfile$temperature>0,v1,v2)
summary(eggsfile$temperature1)

# Question 1e
EFTTregression = lm(eggs ~feed+temperature+temperature1,data=eggsfile)
summary(EFTTregression)

# Question 1f
# see pdf
  
# Question 1g
confint(EFTTregression,level=.95)
predict(EFTTregression,data.frame(feed=c(26,26),temperature=c(-2,-2),temperature1=c(0)),interval="confidence",level=0.95)

# Question 1h
predict(EFTTregression,data.frame(feed=c(26),temperature=c(-2),temperature1=c(0)),interval="prediction",level=0.99)

# Question 1i
#see pdf

# Question 2 Setup
getwd()
setwd("~/Columbia MSBA/Spring 2019/E4650 - Business Analytics/Assignments/Assignment 1")
getwd()
carsfile <- read.csv("NomisB_e-Car_Data.csv",TRUE,",")
class(carsfile)
head(carsfile)

# Question 2a
carscut = carsfile[carsfile$Term==60,]
carscut = carscut[carscut$Car_Type=='N',]
carscut = carscut[carscut$FICO>=675 & carscut$FICO<=725,]
carscut = carscut[carscut$Amount>=30000 & carscut$Amount<=40000,]
summary(carscut)
carscutLM = lm(Outcome ~Rate,data=carscut)
summary(carscutLM)

# Question 2b
carscutGLM = glm(Outcome ~Rate,data=carscut,family=binomial)
summary(carscutGLM)

# Question 2c
carscutLM2 = lm(Outcome ~Rate+Competition_Rate,data=carscut)
summary(carscutLM2)

# Question 2d.1
carscut2 = carscut[carscut$Partner_Bin==1,]
summary(carscut2)
carscut2LM = lm(Outcome ~Rate+Competition_Rate,data=carscut2)
summary(carscut2LM)

# Question 2d.2
carscut2_pred = predict(carscutLM2,newdata=carscut2)
carscut2_actual = carscut2['Outcome']
length(carscut2_pred)
RSE = sqrt(sum((carscut2_pred - carscut2_actual)^2)/(length(carscut2_pred)-2-1))
RSE