
Train_data<- read.csv("D:/Document/r studio file/Network_Intrusion_Train_data.csv", sep = ',', header = FALSE)
Train_data
head(Train_data)
Test_data<-read.csv("D:/Document/r studio file/Network_Intrusion_Test_data.csv")
Test_data
str(Train_data)


set.seed(3033)

install.packages("caret")
library(caret)

install.packages("rpart.plot")
library(rpart.plot)

intrain <- createDataPartition(y = Train_data$V42,p=0.7, list = FALSE)
training <- Train_data[intrain,]
testing <- Test_data

#check dimensions of train & test set
dim(training); dim(testing);

anyNA(Train_data)

summary(Train_data)


#Training as criterion as INFORMATION GAIN
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(V42 ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)


testing[1,]

predict(dtree_fit, newdata = testing[1,])

test_pred <- predict(dtree_fit, newdata = testing)
confusionMatrix(test_pred, testing$V7 )  #check accuracy

#Training as criterion as GINI INDEX
set.seed(3333)
dtree_fit_gini <- train(V7 ~., data = training, method = "rpart",
                          parms = list(split = "gini"),
                          trControl=trctrl,
                          tuneLength = 10)
dtree_fit_gini
prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)


test_pred_gini <- predict(dtree_fit_gini, newdata = testing)
confusionMatrix(test_pred_gini, testing$V7 )  #check accuracy



