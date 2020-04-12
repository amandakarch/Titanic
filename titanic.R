library(readr)
source('C:/Users/akarch1/Desktop/Spring20/QTM3/Data/BabsonAnalytics.R')
df_train = read.csv('C:/Users/akarch1/Desktop/Analytics_Portfolio/Kaggle/titanic/train.csv')
df_test = read.csv('C:/Users/akarch1/Desktop/Analytics_Portfolio/Kaggle/titanic/test.csv')
df = merge(df_train, df_test, all=TRUE)

summary(df)
# 263 NAs for Age
# 2 blanks for Embarked
# 1 NA for Fare
# 1014 blanks for Cabin - shouldn't use (majority of dataset is blank in this column)
# 418 NAs for Survived - shows rows of test dataset

# replacing NAs for Age with mean age (does not change distribution very much)
mean_age = mean(df$Age, na.rm=TRUE)
df$Age[is.na(df$Age)] = mean_age

# replacing blanks for Embarked with mode 'S'
df$Embarked[df$Embarked==''] = 'S'
df$Embarked = factor(df$Embarked) # removed unused level

# removing NA from Fare
which(is.na(df$Fare)) # row 1044 in test data set
getmode <- function(x) {
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x,uniq)))]
}
mode = getmode(df$Fare)
df$Fare[is.na(df$Fare)] = mode

# re-classifying variable types
df$Survived = as.logical(df$Survived) # target
df$Pclass = as.factor(df$Pclass)
df$SibSp = as.numeric(df$SibSp)
df$Parch = as.numeric(df$Parch)

# partition - splitting on original test & training dfs (first 891 are training and last 418 are test)
building = df[c(1:891),]
submission = df[-c(1:891),] # use at end to test for submission

N = nrow(building)
trainingSize = round(N*0.7)
trainingCases = sample(N, trainingSize)
training = building[trainingCases,]
test = building[-trainingCases,]

# only going to use Pclass, Sex, Age, SibSp, Parch, Fare, and Embarked in model building

# model building
# bagging
library(randomForest)
rf = randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=training, ntree=500)
predictions_rf = predict(rf, test)
predictions_rf = (predictions_rf>0.5)
error_rf = sum(predictions_rf != test$Survived)/nrow(test)
importance(rf)

# making predictions on full test dataset
predicted_survival = predict(rf, newdata=submission)
predicted_survival = (predicted_survival>0.5)
df$Survived[892:1309] = predicted_survival

# subsetting df for submission
submitting = df[c(892:1309),c(1,12)]
submitting$Survived = as.numeric(submitting$Survived)
write.csv(submitting, 'C:/Users/akarch1/Desktop/Analytics_Portfolio/Kaggle/titanic/submission1.csv') # had to manually delete first column to keep format right