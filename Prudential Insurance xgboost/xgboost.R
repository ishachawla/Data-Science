library(xgboost)

#Removing ID column from both test and train
which(colnames(training_df)=='Id')  #1065 column
training_df<-training_df[,-1065]

which(colnames(testing_df)=='Id')
#testing_df<-testing_df[,-1187]
which(colnames(testing_df)=='Response')

#Creating test and train split
set.seed(101)
df=sort(sample(nrow(training_df),nrow(training_df)*0.7))
train<-training_df[df,]
test<-training_df[-df,]
y=train$Response
test_y=test$Response

#We don't need Response when we train, so removing Response from train and test datasets
which(colnames(train)=="Response") #1124
train<-train[,-1124]
#which(colnames(test)=='Response') #1125
#test<-test[,-1125]

xg=xgboost(data=data.matrix(train),
           label = y,
           eta=0.5,
           depth=15,
           nround=50, 
           subsample = 0.7,
           objective="reg:linear",
           eval_metric = "rmse",
           verbose=1
           )

pred_xg <- as.integer(round(predict(xg, data.matrix(test[,-1124]))))

#Checking the accuracy of the model
pred_xg
vec<-which((test$Response-pred_xg==0)|(test$Response-pred_xg==1))
length(vec)/length(test$Response)

#training_df$Response-pred_xg

names <- dimnames(data.matrix(train))[[2]]
imp<-xgb.importance(names, model=xg)

xgb.plot.importance(imp[1:10 ,])

#Cross Validation using 5 fold cross validation
cv<-xgb.cv(data = data.matrix(training_df[,-1125]),label=training_df$Response, nrounds = 10, nthread = 2, nfold = 5, metrics = list("rmse","mae"),
             max_depth = 3, eta = 0.25, objective = "reg:linear")

cv2<-xgb.cv(data = data.matrix(training_df[,-1125]),label=training_df$Response, nrounds = 7, nthread = 2, nfold = 5, metrics = list("rmse","mae"),
           max_depth = 3, eta = 0.5, objective = "reg:linear")

cv3<-xgb.cv(data = data.matrix(training_df[,-1125]),label=training_df$Response, nrounds = 5, nthread = 2, nfold = 5, metrics = list("rmse","mae"),
            max_depth = 3, eta = 0.75, objective = "reg:linear")
library(DiagrammeR)
#xgb.plot.tree(feature_names = names,model = xg)

#Plotting the ensemble tree
xgb.plot.multi.trees(model=xg,feature_names = names,features_keep = 2)

#export_graph(g, 'tree.pdf', width=3000, height=4000)
#Linear Booster method to predict values
xg_linear=xgboost(data=data.matrix(train),
           label = y,
           booster='gblinear',
           depth=15,
           nround=100, 
           subsample = 0.5,
           objective="reg:linear",
           eval_metric = "rmse",
           verbose=1
)




pred_xg_linear <- as.integer(round(predict(xg_linear, data.matrix(test[,-1125]))))

vec_linear<-which((test$Response-pred_xg_linear==0)|(test$Response-pred_xg_linear==1))
length(vec_linear)/length(test$Response)

#Finding response variable for test data set.
testing_df$Response<-as.integer(round(predict(xg, data.matrix(testing_df[,-1065]))))
row1<-which(testing_df$Response<1)
row8<-which(testing_df$Response>8)
testing_df[row1,"Response"]<-1
testing_df[row8,"Response"]<-8

submission<-data.frame(testing_df$Id, testing_df$Response)
colnames(submission)<-c("Id","Response")
write.csv(submission,"Submission_xgboost.csv")