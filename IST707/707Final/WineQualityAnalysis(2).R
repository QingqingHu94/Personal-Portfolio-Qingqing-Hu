library(e1071)
library(caret)
library(dplyr)
library(ggplot2)  # data visualization
library(rsample)  # data splitting
library(car)
library(arules)
lSys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_281') # for 64-bit version
install.packages("partykit")
library(partykit)  
install.packages("rJava")
library(rJava)
# basic Rweka packages
install.packages("RWeka")
library(RWeka)       # Weka
install.packages("party")
library(party)  
library(rpart)       # direct engine for decision tree application


## Loading required package: lattice
install.packages("AmesHousing")
library(AmesHousing) # dataset

# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
install.packages("vip")
library(vip)         
library(RWeka)


#read the data
df = read.csv('redwinequality.csv',header=TRUE,sep = ",")

#remove rows that contain null value
df = na.omit(df)

#change the column name
colnames(df)[1] = 'type'
colnames(df)[13] <- "quality"


#classify wines quality as good and bad


temp = df %>% mutate(classified_quality = case_when(quality >= 7 ~ 1,
                                                       quality <= 6 ~ 0))
new_temp = df %>% mutate(classified_quality = case_when(quality >= 7 ~ "Good",
                                                    quality <= 6 ~ "Bad"))

#perform VIF test for collinearity 
new_df = df[,-1]
lm1 = lm(quality ~., data = new_df)
summary(lm1)

#if the vif > 10, it means there is a strong linearity between variables
vif(lm1,digits=3)

#################################Logistic Regression############################
#then we can safely perform logistic regression 
temp = temp[,c(-13,-1)]
new_temp$classified_quality = factor(new_temp$classified_quality)
str(temp)
logreg = glm(classified_quality ~., data = temp, family=binomial(link = "logit"))

summary(logreg)


###############################Decision Tree Model#############################
  #partition the data 
splitData = createDataPartition(y=new_temp$classified_quality,p=0.3,list=FALSE)
train = new_temp[splitData,]
test = new_temp[-splitData,]



C.values <- c(0.01,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.5)
M.values <- c(2,3,4,5,6,7,8,9,10)

## variable to record the best model
best_performance = 0.0
best_c <- 0.0
best_m <- 0.0

for (i in 1:length(C.values)) {
  
  for (j in 1:length(M.values)) {
    
    c_value = C.values[i]
    
    m_value = M.values[j]
    
    m = J48(classified_quality~.,data=train)
    
    e <- evaluate_Weka_classifier(m,
                                  numFolds = 3, complexity = TRUE,
                                  seed = 9, class = TRUE)
    
    if (e$details['pctCorrect'] > best_performance) {
      best_performance <- e$details['pctCorrect']
      
      best_c <- c_value
      best_m <- m_value
    }
    
  }
  
}

print(paste("best accuracy: ", best_performance))

print(paste("best m: ", best_m))

print(paste("best c: ", best_c))


m1=J48(classified_quality~., data = train, control=Weka_control(U=FALSE, M=best_m, C=best_c))


## evaluate model on testing data
e <- evaluate_Weka_classifier(m1, newdata = test,
                              cost = matrix(c(0,1,1,0), ncol = 2),
                              complexity = TRUE,class = TRUE)

e$detailsClass

rpart(classified_quality ~., data = train, method = 'class')


library(FSelector)
information.gain(classified_quality~., data = new_temp)

if(require("party",quietly=TRUE)) plot(m)

tc <- rpart.control(minsplit=20,minbucket=20,maxdepth=10,xval=5,cp=0.005)
rpart.mod=rpart(classified_quality ~.,data=train,method="class",
                parms = list(prior = c(0.65,0.35), split = "gini"),
                control=tc)

rpart.mod.pru<-prune(rpart.mod, cp= rpart.mod$cptable[which.min(rpart.mod$cptable[,"xerror"]),"CP"]) 
rpart.mod.pru$cp

rpart.plot(rpart.mod.pru,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="Decision Tree")



#############################Association Rule Model############################# 

#discretize numerical variables
new_temp1 = new_temp %>% mutate_if(is.numeric,funs(discretize))
quarules = apriori(data=new_temp1,parameter=list(supp=0.018,conf=0.05,minlen=2),
                   appearance=list(default='lhs',rhs=("classified_quality=Good")),
                   control = list(verbose=F))

quarules = sort(quarules, decreasing = TRUE, by ='confidence')
inspect(quarules[1:5])


q1 = apriori(data=new_temp1,parameter=list(supp=0.02,conf=0.02,minlen=2),
             appearance=list(default='lhs',rhs=("classified_quality=Bad")),
             control = list(verbose=F))
q1 = sort(q1, decreasing = TRUE, by ='support')
inspect(q1[1:10])




###############################Naive bayes Model#############################

# select 30% data randomly to index and split the data by the index
splitData1 = createDataPartition(y=new_temp$classified_quality,p=0.3,list=FALSE)
train1 = new_temp[splitData1,]
test1 = new_temp[-splitData1,]
# Build the classifier. 
nbTrain <- naiveBayes(as.factor(classified_quality) ~ ., data = train1)

# test the results on the train set 
nbTrainPred <- predict(nbTrain, train1, type = 'class')
confusionMatrix(nbTrainPred, as.factor(train1$classified_quality))
# The results are satisfactory - 99.58 percent accuracy.

# test the results on the test set and show the accuracy of Naive naive bayes algorithm
nbTestPred <- predict(nbTrain, test1, type = 'class')
confusionMatrix(nbTestPred, as.factor(test1$classified_quality))


# standardize the train data in the range of 0~1 
train1$fixed.acidity <- (train1$fixed.acidity-min(train1$fixed.acidity))/(max(train1$fixed.acidity)-min(train1$fixed.acidity))
train1$volatile.acidity <- (train1$volatile.acidity-min(train1$volatile.acidity))/(max(train1$volatile.acidity)-min(train1$volatile.acidity))
train1$citric.acid <- (train1$citric.acid-min(train1$citric.acid))/(max(train1$citric.acid)-min(train1$citric.acid))
train1$residual.sugar <- (train1$residual.sugar-min(train1$residual.sugar))/(max(train1$residual.sugar)-min(train1$residual.sugar))
train1$chlorides <- (train1$chlorides-min(train1$chlorides))/(max(train1$chlorides)-min(train1$chlorides))
train1$free.sulfur.dioxide <- (train1$free.sulfur.dioxide-min(train1$free.sulfur.dioxide))/(max(train1$free.sulfur.dioxide)-min(train1$free.sulfur.dioxide))
train1$total.sulfur.dioxide <- (train1$total.sulfur.dioxide-min(train1$total.sulfur.dioxide))/(max(train1$total.sulfur.dioxide)-min(train1$total.sulfur.dioxide))
train1$density <- (train1$density-min(train1$density))/(max(train1$density)-min(train1$density))
train1$pH <- (train1$pH-min(train1$pH))/(max(train1$pH)-min(train1$pH))
train1$sulphates <- (train1$sulphates-min(train1$sulphates))/(max(train1$sulphates)-min(train1$sulphates))
train1$alcohol <- (train1$alcohol-min(train1$alcohol))/(max(train1$alcohol)-min(train1$alcohol))

# standardize the test data in the range of 0~1 
test1$fixed.acidity <- (test1$fixed.acidity-min(test1$fixed.acidity))/(max(test1$fixed.acidity)-min(test1$fixed.acidity))
test1$volatile.acidity <- (test1$volatile.acidity-min(test1$volatile.acidity))/(max(test1$volatile.acidity)-min(test1$volatile.acidity))
test1$citric.acid <- (test1$citric.acid-min(test1$citric.acid))/(max(test1$citric.acid)-min(test1$citric.acid))
test1$residual.sugar <- (test1$residual.sugar-min(test1$residual.sugar))/(max(test1$residual.sugar)-min(test1$residual.sugar))
test1$chlorides <- (test1$chlorides-min(test1$chlorides))/(max(test1$chlorides)-min(test1$chlorides))
test1$free.sulfur.dioxide <- (test1$free.sulfur.dioxide-min(test1$free.sulfur.dioxide))/(max(test1$free.sulfur.dioxide)-min(test1$free.sulfur.dioxide))
test1$total.sulfur.dioxide <- (test1$total.sulfur.dioxide-min(test1$total.sulfur.dioxide))/(max(test1$total.sulfur.dioxide)-min(test1$total.sulfur.dioxide))
test1$density <- (test1$density-min(test1$density))/(max(test1$density)-min(test1$density))
test1$pH <- (test1$pH-min(test1$pH))/(max(test1$pH)-min(test1$pH))
test1$sulphates <- (test1$sulphates-min(test1$sulphates))/(max(test1$sulphates)-min(test1$sulphates))
test1$alcohol <- (test1$alcohol-min(test1$alcohol))/(max(test1$alcohol)-min(test1$alcohol))



nbTrain1 <- naiveBayes(as.factor(classified_quality) ~ ., data = train1)

# test the results on the train set 
nbTrainPred1 <- predict(nbTrain1, train1, type = 'class')
confusionMatrix(nbTrainPred1, as.factor(train1$classified_quality))

# The results are satisfactory,which is over 99 percent accuracy.

# test the results on the test set and show the accuracy of Naive Bayes algorithm
nbTestPred1 <- predict(nbTrain1, test1, type = 'class')
confusionMatrix(nbTestPred1, as.factor(test1$classified_quality))

#Compare the accuracy from the original data and standardized data, we found there is almost no difference. The both models have good performance with over 95% accuracy.

