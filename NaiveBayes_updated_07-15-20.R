install.packages("pacman")
library(pacman)
p_load("rsample","dplyr", "caTools","caret", "e1071", "modeldata") # 
# Earlier the dataset was available in rSample, now it's removed thus we will need 
# to read dataset from another package called modeldata


data("attrition", package = "modeldata") # if you are unable to read the dataset this way
# you may read the csv file also using the code below, please remove the hashtag before running 
# the below command

#attrition <- read.csv("attrition.csv", stringsAsFactors = TRUE)

dim(attrition)# There are 31 variable, we will use only 9 variables, 1 target and 8 input 
# we will selelct a few variables to predict attrition 
# BusinessTravel, Department, Education, EducationField,  Marital Status, TrainingTimesLastYear, StockOptionLevel 

# Subsetted dataframe 
attrition.df <- attrition[,c("Attrition","JobLevel","BusinessTravel", "Department", "Education", "EducationField",  "MaritalStatus", "TrainingTimesLastYear", "StockOptionLevel")]

set.seed(101) # to ensure replicability of the results
attrition.df <- attrition.df %>% # We can convert numeric variables into categorical/ factor as Naive Bayes can handle categories
  mutate(
    JobLevel = factor(JobLevel),
    StockOptionLevel = factor(StockOptionLevel),
    TrainingTimesLastYear = factor(TrainingTimesLastYear)
)


#################### Please remember that your outcome variable must be set as factor and not character##########
# In your peer review also don't forget to check. 

class(attrition.df$Attrition) # already a factor so not problem. 

# train test split
sample = sample.split(attrition.df, SplitRatio = 0.80)# select a random sample of rows- 80%
train <- subset(attrition.df, sample==TRUE) # training
test <- subset(attrition.df, sample==FALSE) # test


# distribution of Attrition rates across train & test set
table(train$Attrition) %>% prop.table()
table(test$Attrition) %>% prop.table()





# We perform Naive Bayes on "JobLevel" + "Department", "Education", "EducationField"
Naive_Bayes_Model_4=naiveBayes(train$Attrition ~ JobLevel  + Department + Education + EducationField, data=train)
Naive_Bayes_Model_4$tables

# Accuracy on training set 
train_predictions_4 = as.data.frame(predict(Naive_Bayes_Model_4, train[,-1], type="raw")) # removing target variable from dataframe for prediction

confusionMatrix(as.factor(ifelse(train_predictions_4$Yes>0.2, 'Yes', 'No')), 
                train$Attrition)
# Accuracy - 0.6859        

# Accuracy on the test set
test_predictions_4 = as.data.frame(predict(Naive_Bayes_Model_4, test[,-1], type="raw"))
confusionMatrix(as.factor(ifelse(test_predictions_4$Yes>0.2, 'Yes', 'No')), test$Attrition)
# Accuracy - 0.682   

# Can we improve the model performnace by adding more variables?
# Naive Bayes with all 8 input 

Naive_Bayes_Model_8=naiveBayes(train$Attrition ~., data=train)

# Accuracy on training set 
train_predictions_8 = as.data.frame(predict(Naive_Bayes_Model_8, train[,-1], type="raw")) # removing target variable from dataframe for prediction
confusionMatrix(as.factor(ifelse(train_predictions_8$Yes>0.2, 'Yes', 'No')), 
                train$Attrition)
# Accuracy - 0.727 

# Accuracy on the test set  
test_predictions_8 = as.data.frame(predict(Naive_Bayes_Model_8, test[,-1], type="raw"))
confusionMatrix(as.factor(ifelse(test_predictions_8$Yes>0.2, 'Yes', 'No')), test$Attrition)
# Accuracy - 0.7156


