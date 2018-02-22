library(rJava)
library(RColorBrewer)
library(rattle)

titanic_train<-read.csv("E:\\ADS\\Assignment 4\\Titanic train.csv",stringsAsFactors = FALSE)
titanic_test<-read.csv("E:\\ADS\\Assignment 4\\Titanic test.csv", stringsAsFactors = FALSE)

#to check the missing values
titanic_train[is.na(titanic_train)==TRUE]

#Data Cleaning
str(titanic_train)

#Changing to factors
titanic_train$Survived <- as.factor(titanic_train$Survived)
titanic_train$Sex<-as.factor(titanic_train$Sex)
titanic_train$Embarked<-as.factor(titanic_train$Embarked)
titanic_train$Pclass<-as.factor(titanic_train$Pclass)

#Checking the factors 
str(titanic_train)

titanic_train[!complete.cases(titanic_train),]
index_blankcabin<-which(titanic_train$Cabin=='')

#Removing cabin column
clean_train<-titanic_train[,-(c(11))]
is.na(titanic_train$Age)

str(titanic_train)

#Splitting the training data set in train and test

set.seed(101)
df=sort(sample(nrow(clean_train),nrow(clean_train)*0.7))
train<-clean_train[df,]
test<-clean_train[-df,]

#Making the first decision tree

library(rpart)

d1<-rpart(Survived~Pclass+Sex+Fare,data=train,method="class")
printcp(d1)
fancyRpartPlot(d1,palettes="YlGnBu")
plot(d1,uniform=TRUE)
text(d1,use.n=TRUE,all=TRUE,cex=.7)
pred<-predict(d1,train,type="class")
confMatrix1<-table(pred,train$Survived)
plotcp(d1)
accuracy<-sum(diag(confMatrix1))/sum(confMatrix1)

#Checking the accuracy of the test data set using the confusion matrix
pred1<-predict(d1,test,type="class")
confMatrix2<-table(pred1,test$Survived)
accuracy_test1<-sum(diag(confMatrix2))/sum(confMatrix2)

#Pruning the tree

pruned_d1<-prune(d1,cp=d1$cptable[which.min(d1$cptable[,"xerror"]),"CP"])
plot(pruned_d1,uniform=TRUE)
text(pruned_d1,use.n=TRUE,all=TRUE,cex=0.8)

#Checking the accuracy of the test dataset using the pruned decision tree

pruned_pred2<-predict(pruned_d1,test,type="class")
confMatrix3<-table(pruned_pred2,test$Survived)
sum(diag(confMatrix3))/sum(confMatrix3)

fancyRpartPlot(pruned_d1,palettes = "PuRd" )

master_df=rbind(test,train)
master_df$title=sapply(master_df$Name,FUN=function(str){strsplit(str,master_df$Name,split="[,.]",fixed = FALSE)}[[1]][2])
master_df$title[master_df$title %in% c("Don", "Jonkheer","Major","Sir","Capt")] <- "Mr"
master_df$title[master_df$title %in% c("Mlle", "Mme","the Countess","Lady")] <- "Mrs"
master_df$title<-as.factor(master_df$title)
master_df$title <- sub(" ", "",master_df$title)
#train$title<-sapply(train$Name,FUN=function(str){strsplit(str,train$Name,split="[,.]",fixed = FALSE)}[[1]][2])
#New test and training set

set.seed(101)
d=sort(sample(nrow(master_df),nrow(clean_train)*0.7))
master_train<-master_df[d,]
master_test<-master_df[-d,]

#Making a decision tree considering the title rather than the age.

d_new<-rpart(Survived~Pclass+Sex+Fare+title,data=master_train,method="class")
printcp(d_new)
plot(d_new,uniform=TRUE)
text(d_new,use.n=TRUE,cex=0.7)
fancyRpartPlot(d_new,palettes = "PuRd" )
#Checking the accuracy of this model with the test set.
#First we need to add the title column in the test data set

#test$title<-sapply(test$Name,FUN=function(str){strsplit(str,test$Name,split="[,.]",fixed = FALSE)}[[1]][2])


pred_new2<-predict(d_new,master_test,type="class")
confMatrix_master<-table(pred_new2,master_test$Survived)
sum(diag(confMatrix_master))/sum(confMatrix_master)

# Adding other variables 
d6<-rpart(Survived~Pclass+Fare+title+SibSp+Parch+Embarked,data=master_train,method="class")
fancyRpartPlot(d6,palettes = "Reds" )

# The model that we got without adding these variables is the same as the model we got after this adding these variables
pred_new3<-predict(d6,master_test,type="class")
confMatrix4<-table(pred_new3,master_test$Survived)
sum(diag(confMatrix4))/sum(confMatrix4)

# The accuracy we get is the same too.


#Plotting some basic trends that we observe in the data

library(ggplot2)

#The survival rate for people according to their title.
ggplot(master_df,aes(x=title , y=Survived))+geom_bar(stat="identity",aes(fill=Survived))+labs(title="Survival Rate of people based on their Title")+theme_bw()

#Checking if big families were safer compared to people travelling alone
ggplot(master_df,aes(x=Parch,fill=Survived))+geom_bar(stat="count",color="black",position=position_dodge())+scale_fill_manual(values=c('#4dffc3','#008000'))+coord_flip()+labs(title="Survival Rate of people based on the size of their family")+theme_bw()

#Checking if fare had an impact on Survival Rate
ggplot(master_df,aes(x=Fare,fill=Survived))+geom_density()+scale_fill_manual(values=c('#e60000','#000099'))+labs(title="Survival Rate of people based on their Fare")+theme_light()

#Finding the Survival Rate for the test dataset.
View(titanic_test)
titanic_test$Survived<-""

titanic_test$Survived <- as.factor(titanic_test$Survived)
titanic_test$Sex<-as.factor(titanic_test$Sex)
titanic_test$Embarked<-as.factor(titanic_test$Embarked)
titanic_test$Pclass<-as.factor(titanic_test$Pclass)

titanic_test$title=sapply(titanic_test$Name,FUN=function(str){strsplit(str,titanic_test$Name,split="[,.]",fixed = FALSE)}[[1]][2])
titanic_test$title<-as.factor(titanic_test$title)

titanic_test$Survived<-predict(d6,titanic_test,type="class")





