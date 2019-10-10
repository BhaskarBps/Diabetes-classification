#prediction of whether a person is a diabetic or not

#importing libraries and dataset
install.packages('mlbench')
install.packages('MASS')
install.packages('pROC')
library(mlbench)
library(MASS)
library(pROC)
data("PimaIndiansDiabetes2")
head(PimaIndiansDiabetes2)

#Data Analysis
summary(PimaIndiansDiabetes2)
#remove missing values
newdata<-na.omit(PimaIndiansDiabetes2)
summary(newdata)
#Analyzing distribution
par(mfrow = c(4,2))
for( i in 1:8){
  hist(newdata[,i], main = colnames(newdata)[i],xlab=colnames(newdata)[i], col = 'yellow')
  }
#pregnant and age variables are higly skewed
newdata$age_bucket <- as.factor(ifelse(newdata$age<=30,"20-30",ifelse(newdata$age<=40,"31-40",ifelse(newdata$age<=50,"41-50","50+"))))
newdata$preg_bucket <- as.factor(ifelse(newdata$pregnant<=5,"0-5",ifelse(newdata$pregnant<=10,"6-10","10+")))
#analysing continuous independent variables
par(mfrow = c(3,2))
boxplot(glucose~diabetes, ylab="Glucose", xlab= "Diabetes", col="light blue",data = newdata)
boxplot(pressure~diabetes, ylab="Pressure", xlab= "Diabetes", col="light blue",data = newdata)
boxplot(triceps~diabetes, ylab="triceps", xlab= "Diabetes", col="light blue",data = newdata)
boxplot(insulin~diabetes, ylab="Insulin", xlab= "Diabetes", col="light blue",data = newdata)
boxplot(mass~diabetes, ylab="Mass", xlab= "Diabetes", col="light blue",data = newdata)
boxplot(pedigree~diabetes, ylab="Pedigree", xlab= "Diabetes", col="light blue",data = newdata)
#analysing categorical variables
xtabs(~diabetes + age_bucket, data = newdata)
xtabs(~diabetes + preg_bucket, data = newdata)

#creating a new dataset
newdata2 <- newdata[,c("diabetes","glucose","pressure","triceps","insulin","mass","pedigree","age_bucket","preg_bucket")]

#applying logistic regression model on whole data
logit_1 <- glm(diabetes~., family = binomial,data = newdata2)
summary(logit_1)

#variable selection
logit_2 <- stepAIC(logit_1)
summary(logit_2)
summary(logit_2$fitted.values)
hist(logit_2$fitted.values,main = " Histogram ",xlab = "Probability of 'pos' diabetes", col = 'light green')

#prediction
newdata2$Predict <- ifelse(logit_2$fitted.values >0.5,"pos","neg")

#comparing logit1 and logit2
logit_1$aic
logit_2$aic

#confusion matrix
mytable <- table(newdata2$diabetes,newdata2$Predict)
rownames(mytable) <- c("Obs. neg","Obs. pos")
colnames(mytable) <- c("Pred. neg","Pred. pos")
mytable

#efficiency
efficiency <- sum(diag(mytable))/sum(mytable)
efficiency

#ROC Curve
roc(diabetes~logit_2$fitted.values, data = newdata2, plot = TRUE, main = "ROC CURVE", col= "blue")

#Accuracy using area under curve (AUC)
auc(diabetes~logit_2$fitted.values, data = newdata2)
auc
