
# Run these three functions to get a clean test of homework code
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear user objects from the environment




# Code needed to read in the data for your final project

install.packages("jsonlite")   # Install the needed package to read a json file
library(jsonlite)  #library or check the installed jsonlite package in to use
library("tidyverse")
mydata.list <- fromJSON("completeSurvey.json" ) #read in the json file called completeSurvey.json
survey <- data.frame(mydata.list) # Convert json file to a data.fram
str(survey) # start to get to know your data...many more get to know your data funcitons needed
survey$Scheduled.Departure.Hour
df<- survey
sum(is.na(df$Scheduled.Departure.Hour))
sum(is.na(df$Departure.Delay.in.Minutes))
sum(is.na(df$Arrival.Delay.in.Minutes))
sum(is.na(df$Flight.cancelled))
sum(is.na(df$Flight.time.in.minutes))
df %>%
  select(Origin.City,Destination.City,Flight.cancelled,Flight.time.in.minutes) %>%
  filter(is.na(Flight.time.in.minutes) & Flight.cancelled=="No")
averageFlightTime <- df %>%                                  ## Creating dataframe with average flight time in minutes at route and partner level
  group_by(Origin.City, Destination.City, Partner.Code)%>%
  summarise(mean(Flight.time.in.minutes, na.rm=TRUE))


indexOfNAList <- which(is.na(df$Flight.time.in.minutes) & df$Flight.cancelled=="No")

for(indexOfNA in indexOfNAList){
  df$Flight.time.in.minutes[indexOfNA] <-
    as.integer(averageFlightTime[averageFlightTime$Origin.City==df[indexOfNA,"Origin.City"] & 
                                   averageFlightTime$Destination.City==df[indexOfNA,"Destination.City"] & 
                                   averageFlightTime$Partner.Code==df[indexOfNA,"Partner.Code"],4])
}

## Setting Flight Time in minutes to 0 for all cancelled flights.
indexOfNAList <- which(is.na(df$Flight.time.in.minutes) & df$Flight.cancelled=="Yes")
for(indexOfNA in indexOfNAList){df$Flight.time.in.minutes[indexOfNA] <- 0}

indexOfNAList <- which(is.na(df$Flight.time.in.minutes) & df$Flight.cancelled=="No")
df$Flight.time.in.minutes[indexOfNAList[1]] <- 90
df$Flight.time.in.minutes[indexOfNAList[2]] <- 100
df$Flight.time.in.minutes[indexOfNAList[3]] <- 135
remove(indexOfNA)
remove(averageFlightTime)
indexOfNAList <- which(is.na(df$Flight.time.in.minutes))



averageArrivalDelay <- df %>%
  group_by(Origin.City, Destination.City, Partner.Code)%>%
  summarise(mean(Arrival.Delay.in.Minutes, na.rm=TRUE))

## Departure Delaay has Nas only when flight was cancelled

indexArrivalDelay <- which(is.na(df$Arrival.Delay.in.Minutes) & df$Flight.cancelled=="No")     ## Obtaining indexes where arrival delay is NA & flight was not cancelled

### Treating Nas for arrival delay column where the flight was not cancelled
for(i in indexArrivalDelay)
{
  df$Arrival.Delay.in.Minutes[i] <-
    as.integer(averageArrivalDelay[averageArrivalDelay$Origin.City==df[i,"Origin.City"] & 
                                     averageArrivalDelay$Destination.City==df[i,"Destination.City"] & 
                                     averageArrivalDelay$Partner.Code==df[i,"Partner.Code"],4])
  if(is.na(df$Arrival.Delay.in.Minutes[i]))
  {
    df$Arrival.Delay.in.Minutes[i] <-df$Departure.Delay.in.Minutes[i]
  }
  
} 

df$Arrival.Delay.in.Minutes[which(is.na(df$Arrival.Delay.in.Minutes) &
                                    df$Flight.cancelled=="Yes")] <- 0
df$Departure.Delay.in.Minutes[which(is.na(df$Departure.Delay.in.Minutes) &
                                      df$Flight.cancelled=="Yes")] <- 0


survey$Age_1<- ifelse(survey$Age<=30,1,0)
survey$Age_2<- ifelse(30<survey$Age & survey$Age<51,1,0)
survey$Age_3<- ifelse(50<survey$Age & survey$Age<71,1,0)
survey$Age_4<- ifelse(survey$Age>=71,1,0)

survey$New.Flyer<-ifelse(survey$Year.of.First.Flight>= 2012, TRUE, FALSE)

survey$Frequent.Flyer<-ifelse(survey$Flights.Per.Year>= 20, 'YES', 'NO')

install.packages("lubridate")
library(lubridate)
survey$Flight.month <- month(as.POSIXlt(survey$Flight.date, format="%m/%d/%Y"))
#Add a column called Flight.month.
table(survey$Flight.month)

survey$Flight.time.greater.two.hours <- survey$Flight.time.in.minutes > 120
table(survey$Flight.time.greater.two.hours)

df$longdelay <- cut(df$Arrival.Delay.in.Minutes, breaks = c(0,10, Inf), labels = c('No','Yes'), right = FALSE)




library(ggplot2)

plotTheme <- theme_classic()+theme(axis.title.x = element_text(size = 12), axis.title.y =
                                     element_text(size = 10),plot.title = element_text(size = 15, hjust = 0.5),
                                   panel.grid.major = element_line(color="#e6e6e6",linetype=1))

### Number of PROMOTERS, DETRACTORS and PASSIVE for southeast airines, across cancelled and non-cancelled flights
df$recommender_type <- cut(df$Likelihood.to.recommend, breaks = c(0,7,9, Inf),
                           labels = c('Detractors','Passive','Promoters'), right = FALSE) ## Creating Recommender Type categorical variable
partner_cancellation_nps <-
  data.frame(table(df$Flight.cancelled,df$recommender_type))
## Number of Detractors, Promoters & Passives for each partner & cancellation status
colnames(partner_cancellation_nps) <-
  c('FlightCancellationStatus','RecommenderType','Number')
partner_flight_status_no <-
  ggplot(partner_cancellation_nps[partner_cancellation_nps$FlightCancellationStatus ==
                                    'No',],aes(x=FlightCancellationStatus,y=Number,group=RecommenderType))+
  geom_col(aes(fill=RecommenderType))+
  xlab("Flight Cancellation Status")+ ylab("Number")+plotTheme
partner_flight_status_no

partner_flight_status_yes <-
  ggplot(partner_cancellation_nps[partner_cancellation_nps$FlightCancellationStatus ==
                                    'Yes',],aes(x=FlightCancellationStatus,y=Number,group=RecommenderType))+
  geom_col(aes(fill=RecommenderType))+
  xlab("Flight Cancellation Status")+ ylab("Number")+plotTheme
partner_flight_status_yes










library(arules)
library(arulesViz)
library(tidyverse)
View(df)
table(df$recommender_type)
df$AgeGroup <- cut(df$Age, breaks = c(0,30,50,70, Inf), labels = c('0-30','30-50','50-
70','>70'), right = FALSE)

df$recommender_type <- cut(df$Likelihood.to.recommend, breaks = c(0,7,9, Inf),
                           labels = c('Detractors','Passive','Promoters'), right = FALSE)
## Creating Recommender Type categorical variable
Associative_Df <- df %>% select (Airline.Status,Gender,
                                 Type.of.Travel,Class,AgeGroup,
                                 recommender_type)
Associative_Df$Airline.Status <- as.factor(Associative_Df$Airline.Status)
Associative_Df$AgeGroup <- as.factor(Associative_Df$AgeGroup)
Associative_Df$Gender <- as.factor(Associative_Df$Gender)
Associative_Df$Type.of.Travel <- as.factor(Associative_Df$Type.of.Travel)
Associative_Df$Class <- as.factor(Associative_Df$Class)
Associative_Df$recommender_type <- as.factor(Associative_Df$recommender_type)
Associative_DfX <- as(Associative_Df,"transactions")
ruleset <- apriori(Associative_DfX, ### RULES FOR DETRACTORS
                   parameter=list(support=0.005,confidence=0.5), # Setting support as 0.5% and confidence as 50%
                   appearance = list(default="lhs", rhs=("recommender_type=Detractors")))
inspect(ruleset)
inspectDT(ruleset)

save(ruleset,file='ruleset.data')
dir()
save(ruleset_p,file='ruleset_p.data')
dir()
ruleset_p <- apriori(Associative_DfX, ### RULES FOR PROMOTERS
                     parameter=list(support=0.005,confidence=0.5), # Setting support as 0.5% and confidence as 50%
                     appearance = list(default="lhs", rhs=("recommender_type=Promoters")))
inspect(ruleset_p)
inspectDT(ruleset_p)




