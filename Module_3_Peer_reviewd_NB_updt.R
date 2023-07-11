###################################################---Begin---#############################################################
if (!require("pacman")) install.packages("pacman") # this package helps in installing(if not already) and load the packages
p_load(rsample, dplyr, caTools, caret, e1071)


# Read the data file
real_es <- read.csv("realEstate.csv")

# Define input variables
X = real_es[,2:9]
# Define target variable
y = real_es[,10]

# We will have to convert all the numeric variables into categoric/ factor variables: something like Low-Med-High

# For that we will use the cut function in R, there are other options also.

# Firts, let's see what all columns do we have in the data. 
colnames(X)
# "full_sq"    "life_sq"    "floor"      "max_floor"  "material"   "build_year" "num_room"   "kitch_sq"  

# The bins (cuts) should be made according to the following logic: low =   < mean - 1*sd, med = mean-1*sd - mean +1*sd, high = > mean+1*sd
# calculating these for each variable is not effcienct. Thus, we will scale all the variables and then use cut on all of them at a go

# normalizing all input variables 
# Normalize the inputs 
norm.values <- preProcess(X, method=c("center", "scale")) 
X.norm <- predict(norm.values, X) # Normalized input 

# we create a function that takes a variable and returns the same variables as "low", "med", "high" 
binning_func <- function(x){ 
  binx <- cut(x, c((min(x)), (mean(x)-1*sd(x)), (mean(x)+1*sd(x)), max(x)), labels=c("Low", "Med", "High"))
  return(binx)
}

# We applied the function to all the columns in the X.norm dataframe and saved all of them in X_binned dataframe
X_binned <- data.frame(apply(X.norm, 2, binning_func))

# Now the data is ready for Naive Bayes. If a student does this outisde of R also, that is absolutely fine. Also. the definition of low
# med and high are arbitary so any reasonable choice is fine and should be alloted full marks.

# let's merge the Y variable with the X_binned to get the finl dataset

df <- cbind(X_binned, y)

set.seed(101) # random seed for replicating the results
# train test split
sample = sample.split(df$y, SplitRatio = 0.80)# seelect a random sample of 80%
train <- df[sample==TRUE,] # training dataset
test <-  df[sample==FALSE,] # testing dataset


# Perform Naive Bayes on 8 predictors 
Naive_Bayes_Model=naiveBayes(y ~ ., data=train)
Naive_Bayes_Model$tables

# Accuracy on training set 
train_predictions = predict(Naive_Bayes_Model, train[,-9]) # we reomved the ninth column (target variable)
confusionMatrix(train_predictions, train$y)# comparing actual price classes and predicted price classes in the train dataset
# Accuracy - 0.71

# Accuracy on the test set
test_predictions = predict(Naive_Bayes_Model, test[,-9])# we reomved the ninth column (target variable)
confusionMatrix(test_predictions, test$y)# comparing actual price classes and predicted price classes in the test dataset
# Accuracy - 0.71

# You may comment on how test and train accuracies are comparable and thus, we do not see any overfitting (high variance) as such
# however, low accuracy might indicate underfitting. 

# Remember, we lost a lot of information by converting our nuemric variables into dummies. 
# Note, Naive Bayes is suitable when the input features are categorical. 

#######################################################---END---############################################################

