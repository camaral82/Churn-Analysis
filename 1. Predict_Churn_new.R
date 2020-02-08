'Version 2: Predict Customer Churn with R

https://towardsdatascience.com/predict-customer-churn-with-r-9e62357d47b4'

library(dplyr)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(MASS)
library(caTools) #ROC Curve
library(descr) #pseudo r2

setwd("~/1. Data Science for Management 2019/0. Thesis/Mkt Analytics R")
telco <- read.csv("~/1. Data Science for Management 2019/0. Thesis/Mkt Analytics R/telco.csv")

print("Data Structure - Telco Dataset"); str(telco)

#Churn Distribution
?geom_bar
ggplot(telco, aes(Churn, fill = Churn))+
  ggtitle("Churn Distribution") +
  geom_bar(stat = "count")

'We use sapply to check the number if missing values in each columns. 
We found that there are 11 missing values in "TotalCharges" columns. 
So, lets remove all rows with missing values.'

print("Missing Values"); sapply(telco, function(x) sum(is.na(x)))

telco <- telco[complete.cases(telco), ]


'######## ------ DATA WRANGLING ------########'
#####
'Look at the variables, we can see that we have some wrangling to do.

1. We will change "No internet service" to "No" for six columns, that are:
"OnlineSecurity", "OnlineBackup", "DeviceProtection", 
"TechSupport", "streamingTV", "streamingMovies".'

print("Recoding 'No Internet Service' "); sapply(telco[, 10:15], table)

cols_recode1 <- c(10:15)
for(i in 1:ncol(telco[,cols_recode1])) {
  telco[,cols_recode1][,i] <- as.factor(mapvalues
                                        (telco[,cols_recode1][,i], 
                                          from =c("No internet service"),
                                          to=c("No")))
}

print("After recoding"); sapply(telco[, 10:15], table)



'2. We will change "No phone service" to "No" for column "MultipleLines"'

print("Recoding 'No phone service' "); as.data.frame(table(telco$MultipleLines))

telco$MultipleLines <- as.factor(mapvalues(telco$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))


print("After recoding "); as.data.frame(table(telco$MultipleLines))



'3. Since the minimum tenure is 1 month and maximum tenure is 72 months,
we can group them into five tenure groups: 
"0-12 Month", "12-24 Month", "24-48 Months", 
"48-60 Month", "> 60 Month"'
min(telco$tenure); max(telco$tenure)

ggplot(telco, aes(tenure)) + 
  ggtitle("Tenure Distribution before grouping")+
  geom_histogram()

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

telco$tenure_group <- sapply(telco$tenure,group_tenure)
telco$tenure_group <- factor(telco$tenure_group, 
                                levels = c("0-12 Month", "12-24 Month",
                                           "24-48 Month", "48-60 Month",
                                           "> 60 Month"))
str(telco$tenure_group)



ggplot(telco, aes(tenure_group, fill = tenure_group))+
  ggtitle("Tenure Group") +
  geom_bar()


'4. Change the values in column "SeniorCitizen" 
from 0 or 1 to "No" or "Yes".'

print("Changing the SeniorCitizen variable:"); str(telco$SeniorCitizen)

telco$SeniorCitizen <- as.factor(mapvalues(telco$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))


print("From int to factor:"); str(telco$SeniorCitizen)


'5. Remove the columns we do not need for the analysis.'
telco$customerID <- NULL
telco$tenure <- NULL


#####

'##### -----Exploratory data analysis and feature selection
Correlation between numeric variables'
#####

p18 <- ggplot(telco, aes(x = MonthlyCharges)) +
  geom_histogram()
p181 <- ggplot(telco, aes(x = MonthlyCharges, fill = Churn)) +
  geom_density(alpha = .3)

p19 <- ggplot(telco, aes(x = TotalCharges)) +
  geom_histogram()
p191 <- ggplot(telco, aes(x = TotalCharges, fill = Churn)) +
  geom_density(alpha = .3)

grid.arrange(p18, p181, p19, p191, ncol = 2)


numeric.var <- sapply(telco, is.numeric)
corr.matrix <- cor(telco[,numeric.var])

corrplot(corr.matrix, 
         title = "\n\nCorrelation Matrix for Numerical Variables", 
         method="number",
         pch.col = "black",
         addgrid.col = "grey")



'The Monthly Charges and Total Charges are correlated. 
So one of them will be removed from the model. 
We remove Total Charges.'
telco$TotalCharges <- NULL

#####

'Bar plots of categorical variables'
#####


#Barplots for Customer Demographic Data
p1 <- ggplot(telco, aes(x=gender, fill = Churn)) +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()
p2 <- ggplot(telco, aes(x=SeniorCitizen, fill = Churn)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()
p3 <- ggplot(telco, aes(x=Partner, fill = Churn)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) +
  ylab("Percentage") + theme_minimal()
p4 <- ggplot(telco, aes(x=Dependents, fill = Churn)) + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol=2)

#Subscribed Services
p5 <- ggplot(telco, aes(x=PhoneService, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()

p6 <- ggplot(telco, aes(x=MultipleLines, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()

p7 <- ggplot(telco, aes(x=InternetService, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()

p8 <- ggplot(telco, aes(x=OnlineSecurity, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()

p9 <- ggplot(telco, aes(x=OnlineBackup, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()
  
p10 <- ggplot(telco, aes(x=DeviceProtection, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()
  
p11 <- ggplot(telco, aes(x=TechSupport, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()
  
p12 <- ggplot(telco, aes(x=StreamingTV, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()

p13 <- ggplot(telco, aes(x=StreamingMovies, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()

grid.arrange(p5, p6, p7, p8, p9, p10, p11, p12, p13, ncol=3)


#Customer Account Information
p14 <- ggplot(telco, aes(x=Contract, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()

p15 <- ggplot(telco, aes(x=PaperlessBilling, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()
  
p16 <- ggplot(telco, aes(x=PaymentMethod, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()
  
p17 <- ggplot(telco, aes(x=tenure_group, fill = Churn))  +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + theme_minimal()


grid.arrange(p14, p15, p16, p17, ncol=2)


'All of the categorical variables seem to have a reasonably broad distribution, 
therefore, all of them will be kept for the further analysis.
'

#####


print("Telco dataset after data wrangling"); str(telco)


'Spliting in Train and Test'
###First, we split the data into training and testing sets:'

set.seed(2020)
intrain <- createDataPartition(telco$Churn,p=0.7,list=FALSE)
training <- telco[intrain,]
testing <- telco[-intrain,]

###Confirm the splitting is correct:
dim(training); dim(testing)






