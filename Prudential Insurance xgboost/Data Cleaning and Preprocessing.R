prudent_train<-read.csv("E:\\ADS\\Midterm Project\\TrainingDataset\\train.csv")
prudent_test<-read.csv("E:\\ADS\\Midterm Project\\test\\test.csv")
str(prudent_train)

str(prudent_test)
library(ggplot2)

#is.na(prudent_train)

ggplot(prudent_train,aes(x=Response))+geom_histogram(stat="count",fill=I("Blue"),col=I("black"),binwidth = 5,main="Histogram for Response")+theme_bw()

#Continuous Variables
ggplot(prudent_train,aes(x=Ht,fill="hue"))+geom_density()

#Percentage of Blank Values
df1<-data.frame(sapply(prudent_train, function(y) sum(length(which(is.na(y))))))
colnames(df1)<-c("BlankRows")
df1$PercentBlanks<-(df1$BlankRows/nrow(prudent_train))*100

#Columns with more than 70 percent values blank
omit_columns<-which(df1$PercentBlanks>=70.00)
omit_colnames<-colnames(prudent_train[,omit_columns])

prudent_test[, !(names(prudent_test) %in% omit_colnames)]

#Removing these columns from prudent train and prudent test
prudent_train<- prudent_train[,-omit_columns]
prudent_test<-prudent_test[, !(names(prudent_test) %in% omit_colnames)]

#Substituting mean values for NA values in Employement Info 1
prudent_train$Employment_Info_1[is.na(prudent_train$Employment_Info_1)]<-mean(prudent_train$Employment_Info_1,na.rm=TRUE)
prudent_train$Employment_Info_4[is.na(prudent_train$Employment_Info_4)]<-mean(prudent_train$Employment_Info_4,na.rm=TRUE)
prudent_train$Employment_Info_6[is.na(prudent_train$Employment_Info_6)]<-mean(prudent_train$Employment_Info_6,na.rm=TRUE)
prudent_train$Insurance_History_5[is.na(prudent_train$Insurance_History_5)]<-mean(prudent_train$Insurance_History_5,na.rm=TRUE)
prudent_train$Family_Hist_2[is.na(prudent_train$Family_Hist_2)]<-mean(prudent_train$Family_Hist_2,na.rm=TRUE)
prudent_train$Family_Hist_3[is.na(prudent_train$Family_Hist_3)]<-mean(prudent_train$Family_Hist_3,na.rm=TRUE)
prudent_train$Family_Hist_4[is.na(prudent_train$Family_Hist_4)]<-mean(prudent_train$Family_Hist_4,na.rm=TRUE)
prudent_train$Medical_History_1[is.na(prudent_train$Medical_History_1)]<-median(prudent_train$Medical_History_1,na.rm=TRUE)

#Percentage of Blank Values in prudent_test
df2<-data.frame(sapply(prudent_test, function(y) sum(length(which(is.na(y))))))
colnames(df2)<-c("BlankRows")
df2$PercentBlanks<-(df2$BlankRows/nrow(prudent_test))*100

#Columns with same blank values in test dataset
blanks_cols<-which(df2$PercentBlanks<70.00 & df2$PercentBlanks>0.00)

#Substituing the mean and median for missing values
prudent_test$Employment_Info_1[is.na(prudent_test$Employment_Info_1)]<-mean(prudent_test$Employment_Info_1,na.rm=TRUE)
prudent_test$Employment_Info_4[is.na(prudent_test$Employment_Info_4)]<-mean(prudent_test$Employment_Info_4,na.rm=TRUE)
prudent_test$Employment_Info_6[is.na(prudent_test$Employment_Info_6)]<-mean(prudent_test$Employment_Info_6,na.rm=TRUE)
prudent_test$Insurance_History_5[is.na(prudent_test$Insurance_History_5)]<-mean(prudent_test$Insurance_History_5,na.rm=TRUE)
prudent_test$Family_Hist_2[is.na(prudent_test$Family_Hist_2)]<-mean(prudent_test$Family_Hist_2,na.rm=TRUE)
prudent_test$Family_Hist_3[is.na(prudent_test$Family_Hist_3)]<-mean(prudent_test$Family_Hist_3,na.rm=TRUE)
prudent_test$Family_Hist_4[is.na(prudent_test$Family_Hist_4)]<-mean(prudent_test$Family_Hist_4,na.rm=TRUE)
prudent_test$Medical_History_1[is.na(prudent_test$Medical_History_1)]<-median(prudent_test$Medical_History_1,na.rm=TRUE)

#Checking if there are any more NA values in train and test
which(is.na(prudent_test))
which(is.na(prudent_train))

#Converting categorical variables (1 to c encoding)
prudent_test$Source<-"Test"
prudent_train$Source<-"Train"

#Adding the response column in Test dataset
prudent_test$Response<-NA

#All the categorical variables in test and train
categorical_test <- prudent_test[c("Medical_History_1","Product_Info_1", "Product_Info_2", "Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", "Employment_Info_2", "Employment_Info_3", "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", "Medical_History_2", "Medical_History_3", "Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_7", "Medical_History_8", "Medical_History_9", "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", "Medical_History_17", "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", "Medical_History_25", "Medical_History_26", "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", "Medical_History_31", "Medical_History_33", "Medical_History_34", "Medical_History_35", "Medical_History_36", "Medical_History_37", "Medical_History_38", "Medical_History_39", "Medical_History_40", "Medical_History_41","Source")]
categorical_train<- prudent_train[c("Medical_History_1","Product_Info_1", "Product_Info_2", "Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", "Employment_Info_2", "Employment_Info_3", "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", "Medical_History_2", "Medical_History_3", "Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_7", "Medical_History_8", "Medical_History_9", "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", "Medical_History_17", "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", "Medical_History_25", "Medical_History_26", "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", "Medical_History_31", "Medical_History_33", "Medical_History_34", "Medical_History_35", "Medical_History_36", "Medical_History_37", "Medical_History_38", "Medical_History_39", "Medical_History_40", "Medical_History_41","Source")]

#Dataset with all the categorical variables from both test and train
cat_variables<-rbind(categorical_test,categorical_train)
colnames(cat_variables)

#For 1-C encoding, we need factors
cat_variables<-data.frame(lapply(cat_variables, as.factor))

#1- C encoding on all the categorical variables, using model.matrix

#Importing libraries

library(Matrix)
library(ade4)

#creating a dataframe with 1 to c conversion

test2<-data.frame(model.matrix(~ . + 0, data=cat_variables, contrasts.arg = lapply(cat_variables, contrasts, contrasts=FALSE)))

#Now, we remove the test columns from this dataset
training_df<-test2[which(test2$SourceTrain==1),]
testing_df<-test2[which(test2$SourceTrain==0),]

#Merging the rest of the remaining columns to the dataset

categorical_variables <- c("Medical_History_1","Product_Info_1", "Product_Info_2", "Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", "Employment_Info_2", "Employment_Info_3", "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", "Medical_History_2", "Medical_History_3", "Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_7", "Medical_History_8", "Medical_History_9", "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", "Medical_History_17", "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", "Medical_History_25", "Medical_History_26", "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", "Medical_History_31", "Medical_History_33", "Medical_History_34", "Medical_History_35", "Medical_History_36", "Medical_History_37", "Medical_History_38", "Medical_History_39", "Medical_History_40", "Medical_History_41")

remainingtest_cols<-prudent_test[, !(names(prudent_test) %in% categorical_variables)]
remainingtrain_cols<-prudent_train[, !(names(prudent_train) %in% categorical_variables)]

#Merging these with the categorical variables dataframe

training_df<-cbind(training_df,remainingtrain_cols)
testing_df<-cbind(testing_df,remainingtest_cols)

#Removing the columns identifying the source
drop<- c("Source","SourceTest","SourceTrain")

training_df<-training_df[, !(names(training_df) %in% drop)]
testing_df<-testing_df[, !(names(testing_df) %in% drop)]

write.csv(training_df,"Prudential_Edited_Training.csv")
write.csv(testing_df,"Prudentail_Edited_Testing.csv")