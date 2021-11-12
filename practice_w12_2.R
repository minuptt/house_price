install.packages('caret')
library('caret')

#read the ‘HousePrice.csv’,load it to a data frame.

setwd('/Users/ThiPhan1/Documents/UNT/1FALL2019/INFO3010/WEEK12')
house2 <- read.csv("HousePrice.csv")

#str(house)
head(house2, 5)

#convert continuous house price values to labels 
house2$Price <- factor(with(house2, ifelse((house2$Price<800000),'low','high')))

#check number of samples in each category
table(house2$Price)

# Divide as training and testing: 20% test 80% trainandget the training data size
sample_size<-floor(0.8*nrow(house2))

# check the training data size
sample_size

#get train data index
train_ind <- sample(seq_len(nrow(house2)), size = sample_size)

#generate training and test dataset
train <- house2[train_ind,]
test <- house2[-train_ind,]

#use caret package to build SVM models
#before training,remove samples with NA
train <- train[complete.cases(train),]
test <- test[complete.cases(test),]

#set cross validation method:
## 5-fold CV ,repeated two times

fitControl <- trainControl(
                            method = "repeatedcv",
                            number = 5,
                            repeats = 2)

#train the model Support Vector Machines with Polynomial Kernel
svmpoly <- train(Price ~ Sqft_Area + Age + Lot_Area + Crime,
                 data = train,
                 method = "svmPoly",
                 trControl = fitControl)

# check the model svmPoly
svmpoly

# apply model on test data
prediction_svmpoly <- predict(svmpoly, newdata = test)

# evaluate prediction results
confusionMatrix(prediction_svmpoly, test$Price)

# try to build svm models wit hother kernels, evaluate the results on test dataset to see if the performance increases or decreases
# svm with linear kernel 
svmlinear <- train(Price ~ Sqft_Area + Age + Lot_Area+Crime, data = train, 
                    method = "svmLinear", 
                    trControl = fitControl)
svmlinear

# svm with gaussian kernel
svmradial <- train(Price ~ Sqft_Area + Age + Lot_Area+Crime, 
                    data = train, 
                    method = "svmRadial", 
                    trControl= fitControl)
svmradial




