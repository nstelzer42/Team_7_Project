
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

data[,c("level","location","tag","otherdetails","cityid","dmaid","rowNumber","Masters_Degree","Bachelors_Degree","Doctorate_Degree","Highschool","Some_College","Race_Asian","Race_White","Race_Two_Or_More","Race_Black","Race_Hispanic")]=NULL
#This removes all of the data that we will not be collecting from the user that will not determine the forecasted total 

data$timestamp = as.Date(strptime(data$timestamp, "%m/%d/%Y %H:%M:%S"))
#Converts the string of the timestamp of the data to a date format to what we can use in the model 

data$totalyearlycompensation = as.numeric(data$totalyearlycompensation)
data$basesalary = as.numeric(data$basesalary)
data$stockgrantvalue = as.numeric(data$stockgrantvalue)
data$bonus = as.numeric(data$bonus)

#converts the total yearly compensation to a numeric field

a = c("title","yearsofexperience","yearsatcompany","gender","Race","Education","Country","State","company")

for (i in 1:length(a)){
  data[,a[i]] = as.factor(data[,a[i]])
}
#This loop coerces the data set on the variables that the user will enter in as a factor level before the data set runs

rm(i,a)
#removes the variables

write.csv(data,'C:\\Users\\Nick Stelzer\\Box Sync\\Team 7\\app_data.csv')
rm(data)