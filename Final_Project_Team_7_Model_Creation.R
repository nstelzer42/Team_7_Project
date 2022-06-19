#Team #7 R script Final Project. Nick Stelzer, Grant Shireman, Ricardo pignatta
#This script will be used to create the model using H2O auto function to run a model for 2 hours to determine the 
# best forecasting model based on the data that will then be used in the shiny app to predict a base salary 

library(h2o)
library(mice)
library(caret)
#imports the libraries that we will need to perform the machine learning model 

data = read.csv("C:\\Users\\Nick Stelzer\\Box Sync\\Team 7\\Levels_Fyi_Salary_Data.csv")


for (i in 1:nrow(data)){
  data$Country[i] = ifelse(is.na(unlist(strsplit(data[i,'location'], ", "))[3]),'United States',unlist(strsplit(data[i,'location'], ", "))[3]) 
  data$State[i] = unlist(strsplit(data[i,'location'], ", "))[2]
}
#This is a for loop that goes through the location column and creates two new columns of the country and the state based on how the data was loaded

rm(i)
#removes the variable

data$gender[data$gender=='Title: Senior Software Engineer']=NA
#one data set in the gender column has a title that is in the gender. This makes the value NA for the one off ball 

data[,c("basesalary","stockgrantvalue","bonus","company","level","location","tag","otherdetails","cityid","dmaid","rowNumber","Masters_Degree","Bachelors_Degree","Doctorate_Degree","Highschool","Some_College","Race_Asian","Race_White","Race_Two_Or_More","Race_Black","Race_Hispanic")]=NULL
#This removes all of the data that we will not be collecting from the user that will not determine the forecasted total 


data$timestamp = as.Date(strptime(data$timestamp, "%m/%d/%Y %H:%M:%S"))
#Converts the string of the timestamp of the data to a date format to what we can use in the model 

data$totalyearlycompensation = as.numeric(data$totalyearlycompensation)
#converts the total yearly compensation to a numeric field

a = c("title","yearsofexperience","yearsatcompany","gender","Race","Education","Country","State")

for (i in 1:length(a)){
  data[,a[i]] = as.factor(data[,a[i]])
}
#This loop coerces the data set on the variables that the user will enter in as a factor level before the data set runs

rm(i,a)
#removes the not needed variables

str(data)
#looks at the data to make sure that everything is coerced correctly 

#We are then going to use the mice package to then fill in the NA values in the data before we run the model. The
#type of model that we will use is a cart that stands for Classification and Regression Trees
imputedValues = mice(data=data, seed=99, method="cart", m=1, maxit = 1)
#We use the data set with a seed, the method is a cart, the number of imputations is 1 and the number of iterations is 1

data = mice::complete(imputedValues,1)
#This fills in the missing data with the imputed values that were calculated

rm(imputedValues)
#removes the imputed values that we no longer need

data = data[,c(3,1:2,4:10)]
#moves the target variable to the first column in the data set

h2o.init(nthreads=12, max_mem_size="64g")
# Imports the library for h2o into the model and sets the parameters to run the model based on data and cores

d = as.h2o(data)
#creates the data set that we just manipulated into a H2O object

y='totalyearlycompensation'
#This is the column that we are looking to learn from

x = setdiff(names(d), y)
#these are all of the other columns and features to learn from

parts = h2o.splitFrame(d, 0.8, seed=99)
#parts the data  split based on a seed of 99 and an 80/20 split on the data frame

train = parts[[1]]
test = parts[[2]]
#gets the train and test data sets based on a 80/20 split

rm(parts)
#removes unwanted variables

m1 = h2o.automl(x, y, train, max_runtime_secs=7200)

#This runs for 2 hours to find and create the best model for the data set given the information available

pred_train = as.data.frame(h2o.predict(m1,train))
pred_test = as.data.frame(h2o.predict(m1,test))
#finds the prediction for the train and test data sets


results_train = cbind(as.data.frame(train$totalyearlycompensation),as.data.frame(pred_train))
results_test = cbind(as.data.frame(test$totalyearlycompensation),as.data.frame(pred_test))
#creates a data frame for the test and train results on the actual annual compensation and prediction 


defaultSummary(data=data.frame(obs=results_train$totalyearlycompensation, pred=results_train$predict), model=m1)
#Summary statistic to find the r squared values between the prediction and actual in the train data set
# 
# RMSE            Rsquared           MAE 
# 9.075705e+04   0.5860965    5.400425e+04 



defaultSummary(data=data.frame(obs=results_test$totalyearlycompensation, pred=results_test$predict), model=m1)
#Summary statistic to find the r squared values between the prediction and actual in the test data set

# RMSE              Rsquared          MAE 
# 93357.471676     0.493891       58819.573603 


#Based on the Rsquared values between the test and train data sets, the values are not over fitted because the values are within 0.1 or 10% of one another

model_path = h2o.saveModel(object = m1@leader, path = "C:\\Users\\Nick Stelzer\\Box Sync\\Team 7", force = TRUE)
#This saves the model to a location within the box folder that we will use to then perform analysis on the data set that the
#user enters into their shiny application

#"C:\\Users\\Nick Stelzer\\Box Sync\\Team 7\\Model_Forecasted"
rm(list = ls())
#removes all variables

h2o.shutdown()
#shuts down H2o to preserve memory