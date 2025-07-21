a=c(35,36,34,35,36,32,35,38,39,34,32,31,35,36,32)
e=c(1.5,1.2,2.1,2.3,2.5,1.6,1.5,1.4,2.3,2.6,2.6,1.2,1.3,1.8,1.9)
r=c(110,115,80,89,78,54,86,110,118,119,200,240,210,110,118)
s=c(0.63,0.59,0.56,0.55,0.54,0.59,0.56,0.46,0.63,0.60,0.63,0.58,0.55,0.57,0.62)
d=c(18.1,19.6,16.6,16.4,16.9,17.0,20.0,16.6,16.2,18.5,18.7,19.4,17.6,18.3,18.8)
MLR=lm(d~a+e+r+s)
summary(MLR)
plot(MLR)
res=resid(MLR)
res
pre=predict(MLR)
pre
plot(res,pre)
plot(MLR,2)
shapiro.test(res)

install.packages("caTools")
install.packages('car')
install.packages("quantmod")
install.packages("MASS")
install.packages("corrplot")
library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)
vif(MLR)


install.packages("datarium")
library(datarium)
data(marketing,package="datarium")
head(marketing,4)
View(marketing)
MLR=lm(sales~youtube+newspaper+facebook)
summary(MLR)
install.packages("packagename")
mydata=read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv"
summary(data)



install.packages("e1071")
library(e1071)
x=matrix(rnorm(60),30,2)
x
y=rep(c(-1,1),c(15,15))
y
x[y==1,]=x[y==1,]+1
d=data.frame(x, y = as.factor(y))
svm=svm(y~.,data=d,kernal="linear",cost=10)
summary(svm)

