#case1
require(readxl)
cancer<-read_excel("F:/R_workbook/Programming and Predictive Modeling using R/3. Case Studies/6. Logistic Regression/Data/Smoking_and_Cancer.xlsx")
View(cancer)
train<-cancer[1:16,]
test<-cancer[17:25,]

model<-glm(`Lung Cancer (Y)`~`Smoking (X)`,data=train,family=binomial)
summary(model)

k<-ifelse(model$fitted.values>0.5,1,0)
pred<-predict(model,test,type="response")
pred1<-ifelse(pred>0.5,1,0)
t<-table(pred1,test$`Lung Cancer (Y)`)

odds<-exp(model$coefficients[2])
odds

install.packages("ResourceSelection")
library("ResourceSelection")
hoslem.test(train$`Lung Cancer (Y)`,fitted(model),g=10)

#case2
require(readxl)
skull<-read_excel("F:/R_workbook/Programming and Predictive Modeling using R/3. Case Studies/6. Logistic Regression/Data/Skull_Type_Prediction.xlsx")
View(skull)

model2<-glm(`Skull Type`~.,data=skull,family="binomial")
summary(model2)

hoslem.test(skull$`Skull Type`,fitted(model2),g=10)

p<-predict(model2,skull,type = "response")
p1<-ifelse(p>0.5,1,0)

table(p1,skull$`Skull Type`)

test<-read_excel("F:/R_workbook/Programming and Predictive Modeling using R/3. Case Studies/6. Logistic Regression/Data/Skull_Type_Prediction-Validation_Data.xlsx")
View(test)
ifelse(predict(model2,test,type = "response")>0.5,1,0)

#case3
require("readxl")
vote<-read_excel("F:/R_workbook/Programming and Predictive Modeling using R/3. Case Studies/6. Logistic Regression/Data/US_Presidential_Data.xlsx")
View(vote)

train<-vote[1:1200,-]
test<-vote[1201:nrow(vote),]

model3<-glm(`Win/Loss`~.,data=train,family="binomial")
summary(model3)

ResourceSelection::hoslem.test(train$`Win/Loss`,fitted(model3),g=10)

t<-predict(model3,test,type="response")
testoutcome<-ifelse(t>0.5,1,0)

tab<-table(testoutcome,test$`Win/Loss`)
accuracy<-sum(diag(tab))/sum(tab)
sensitivity<-tab[1,1]/tab[1,1]+tab[1,2]
specificity<-tab[1,1]/tab[1,1]+tab[2,1]

#case4

mydata<-read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
View(mydata)

model4<-glm(admit~.,mydata,family = "binomial")
summary(model4)

ResourceSelection::hoslem.test(mydata$admit,fitted(model4),g=10)

y<-1/(1+exp(3.449548-0.002294*720-0.777014*3.9+0.560031*1))
adm<-ifelse(y>0.5,1,0)
adm

#case5
library(readxl)
data<-read_excel("F:/R_workbook/Programming and Predictive Modeling using R/3. Case Studies/6. Logistic Regression/Data/Flower_Species.xlsx")
View(data)

#Model 1
model<-glm(y1 ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
           data = data, family = "binomial")
summary(model)
pred1<-model$fitted.values

#Model 2
model<-glm(y2 ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
           data = data, family = "binomial")
summary(model)
pred2<-model$fitted.values

#Model 3
model<-glm(y3 ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
           data = data, family = "binomial")
summary(model)
pred3=model$fitted.values
data<-cbind(data,pred1,pred2,pred3)

for(i in 1:nrow(data))
{
  if(max(data$pred1[i],data$pred2[i],data$pred3[i]) == (data$pred1[i]))
    
    data$pred[i]<-"setosa"
  
  if(max(data$pred1[i],data$pred2[i],data$pred3[i]) == (data$pred2[i]))
    
    data$pred[i]<-"versicolor"
  
  if(max(data$pred1[i],data$pred2[i],data$pred3[i]) == (data$pred3[i]))
    
    data$pred[i]<-"virginica"
}
a<-table(data$Species,data$pred)
accuracy<-sum(diag(a))/sum(a)

#case6

require(nnet)
model<-multinom(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                data = data)
summary(model)

data<-cbind(data,fitted(model))
library(plyr)
data<-rename(data, c("setosa"="pred1", "versicolor"="pred2", "virginica" = "pred3"))



for(i in 1:nrow(data))
{
  if(max(data$pred1[i],data$pred2[i],data$pred3[i]) == (data$pred1[i]))
    
    data$pred[i]<-"setosa"
  
  if(max(data$pred1[i],data$pred2[i],data$pred3[i]) == (data$pred2[i]))
    
    data$pred[i]<-"versicolor"
  
  if(max(data$pred1[i],data$pred2[i],data$pred3[i]) == (data$pred3[i]))
    
    data$pred[i]<-"virginica"
}

#confusion matrix
table(data$Species,data$pred)
