# Code needed to read in the data for your final project

install.packages("jsonlite")   # Install the needed package to read a json file
library(jsonlite)  #library or check the installed jsonlite package in to use

mydata.list <- fromJSON("completeSurvey.json" ) #read in the json file called completeSurvey.json
survey <- data.frame(mydata.list) # Convert json file to a data.fram
str(survey) # start to get to know your data...many more get to know your data funcitons needed
