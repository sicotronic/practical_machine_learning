
#Load all necessary Libraries
library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)

#Set seed for research reproduceability
set.seed(101087)

# Downloading and getting the data

# set the strings with the train and test files URL:
url_test_set <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
url_train_set <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

# download.file(url = url_test_set, destfile = 'data_test_set.csv')
# download.file(url = url_train_set, destfile = 'data_train_set.csv')

complete_training_set <- read.csv(url(url_train_set), na.strings=c("NA","#DIV/0!",""))
complete_testing_set <- read.csv(url(url_test_set), na.strings=c("NA","#DIV/0!",""))

# Some variables are irrelevant to our current project: user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window (columns 1 to 7). We can delete these variables.
complete_training_set <- complete_training_set[, -c(1:7)]
complete_testing_set <- complete_testing_set[, -c(1:7)]

# Create the partitions of the training data set into two data sets, 60% for training, 40% for testing:
index_training <- createDataPartition(y=complete_training_set$classe, p=0.6, list=FALSE)
training <- complete_training_set[index_training, ]
testing <- complete_training_set[-index_training, ]

# Delete columns with all missing values
training<-training[, colSums(is.na(training)) == 0]
testing <-testing[, colSums(is.na(testing)) == 0]

dim(training)
head(training)
dim(testing)
head(testing)

# Removing first ID variable so that it does not interfer with ML Algorithms:
training <- training[c(-1)]

# clean1 <- colnames(training)
# clean2 <- colnames(training[, -58]) #already with classe column removed
# testing <- testing[clean1]
# testing <- testing[clean2]


## Using ML algorithms for prediction: Decision Tree

model_fit_decission_trees <- rpart(classe ~ ., data=training, method="class")
rpart.plot(model_fit_decission_trees, main="Classification Tree", extra=102, under=TRUE, faclen=0)

#Predicting:
predictions_decission_trees <- predict(model_fit_decission_trees, testing, type = "class")

#(Moment of truth) Using confusion Matrix to test results:
confusionMatrix(predictions_decission_trees, testing$classe)

#Overall Statistics
#Accuracy : 0.8653          
#95% CI : (0.8575, 0.8728)
#No Information Rate : 0.2845          
#P-Value [Acc > NIR] : < 2.2e-16       
#Kappa : 0.8296          
#Mcnemar's Test P-Value : NA         

## Using ML algorithms for prediction: Random Forests
model_fit_random_forest <- randomForest(classe ~. , data=training)

#Predicting:
predictions_random_forest <- predict(model_fit_random_forest, testing, type = "class")

#(Moment of truth) Using confusion Matrix to test results:
confusionMatrix(predictions_random_forest, testing$classe)

# Overall Statistics
#Accuracy : 0.9999     
#95% CI : (0.9993, 1)
#No Information Rate : 0.2845     
#P-Value [Acc > NIR] : < 2.2e-16  
#Kappa : 0.9998     
#Mcnemar's Test P-Value : NA      

# Random Forests yielded better Results, as expected!

# Generating Files to submit as answers for the Assignment:

##Finally, using the provided Test Set:
#For Decision Tree would be like this, but not going to use it:
#predictionsA2 <- predict(modFitA1, testing, type = "class")

#For Random Forests is:
# 53 is the "classe" column
final_testing <- complete_testing_set[,colnames(testing[ , -c(53)])]
predictions_complete_testing <- predict(model_fit_random_forest, final_testing, type = "class")

#Function to generate files with predictions to submit for assignment:

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
  }
}

pml_write_files(predictions_complete_testing)