Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_161') 
library(rJava)
library(xlsx)
library(readxl)
energy_df <- read_excel("E:\\ADS\\Assignment 4\\Energy Efficiency ENB2012_data.xlsx", 1)
str(energy_df)
summary(energy_df)

energy_df <- data.frame(energy_df)
#Y<-cbind(energy_df$Y1,energy_df$Y2)

pairs(energy_df[,1:10])
#Splitting the test set into train and test
set.seed(101)
new_df=sort(sample(nrow(energy_df),nrow(energy_df)*0.7))
energy_train<-energy_df[new_df,]
energy_test<-energy_df[-new_df,]

#Building a model but first we combine our two dependent variables
library(RColorBrewer)
library(rattle)
library(rpart)
energy_d1<-rpart(Y1~X1+X2+X3+X4+X5+X6+X7+X8,data=energy_train,method="anova")
printcp(energy_d1)
plot(energy_d1,uniform=TRUE)
text(energy_d1,use.n=TRUE,all=TRUE,cex=.7)

fancyRpartPlot(energy_d1,palettes = "Greens" )

pred_energy<-predict(energy_d1,energy_test,method="anova")

#Mean Squared Error in prediction of Y1
mean((energy_test$Y1- pred_energy)^2)

#Checking the r squared error value to find the accuracy of the model
numerator <- sum((energy_test$Y1- pred_energy)^2)
denominator <- sum((pred_energy-mean(energy_test$Y1))^2)
rsquare <- 1-(numerator/denominator)
rsquare

#Model for predicting y2
energy_d2<-rpart(Y2~X1+X2+X3+X4+X5+X6+X7+X8,data=energy_train,method="anova")
fancyRpartPlot(energy_d2,palettes = "Purples")
pred_energy2<-predict(energy_d2,energy_test,method="anova")
#Mean squared error for y2
mean((energy_test$Y2- pred_energy2)^2)

#r squared value
numerator2 <- sum((energy_test$Y2- pred_energy2)^2)
denominator <- sum((pred_energy2-mean(energy_test$Y2))^2)
rsquared <- 1-(numerator/denominator)
rsquared