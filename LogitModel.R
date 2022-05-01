library(readr)
Churn_Modelling <- read_csv("Churn_Modelling.csv")
View(Churn_Modelling)

summary(Churn_Modelling)

# Convert Exited, Gender, HasCrcard, and is Active Member to factor variables
Churn_Modelling$Exited <- as.factor(Churn_Modelling$Exited)
Churn_Modelling$Gender <- as.factor(Churn_Modelling$Gender)
Churn_Modelling$HasCrCard <- as.factor(Churn_Modelling$HasCrCard)
Churn_Modelling$IsActiveMember <- as.factor(Churn_Modelling$IsActiveMember)

str(Churn_Modelling)
na.omit(Churn_Modelling)
#levels(Churn_Modelling$Gender) <- c(1,0)
summary(Churn_Modelling)

library(vtable)
# Drop RowNumber, CustomerId, and Surname
library(tidyverse)
Data1 <- Churn_Modelling %>% 
  select(-c(RowNumber, CustomerId, Surname, Geography))
View(Data1)
# Summary statistics and correlation analysis
summary(Data1)
# i) bar plot
ggplot(Data1, aes(x = Gender)) +
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Population by Gender")
# ii) histogram
ggplot(Data1, aes(x = Age)) +
  geom_histogram(fill = "cornflowerblue",
                 color = "white") +
  labs(title="Participants by age",
       x = "Age")
# Summary for the factor variables
library(gtsummary)
Data1 %>% tbl_summary()
categorical_vars <- Data1%>% select(Gender, Exited, HasCrCard, IsActiveMember)
categorical_vars %>% tbl_summary()

# Split the data to train and test set
set.seed(1234)
indexSet= sample(2,nrow(Data1),replace= T, prob=c(0.8,0.2))
train=Data1[indexSet==1,]
test=Data1[indexSet==2,]

# Create logistic model
model1 <- glm(Exited ~ ., family= 'binomial', data = train)
summary(model1)

#From model1 we realise some of the variables are not significant in predicting whether a customer exited or not, we'll drop to fit a new model
# Drop the insignificant variables
data2 <- Data1 %>% 
  select(-c(CreditScore, NumOfProducts, HasCrCard, EstimatedSalary, Tenure))
# Split the data to train and test sets
set.seed(1234)
indexSet= sample(2,nrow(data2),replace= T, prob=c(0.8,0.2))
train_set=data2[indexSet==1,]
test_set=data2[indexSet==2,]

# Build the second logistic model
model2 <- glm(Exited ~ ., family= 'binomial', data = train_set)
summary(model2)

# Now, we will make predictions using the test set
predicted <- predict(model2, test_set, type = "response")
head(predicted, n=8)
head(test_set, n=8)

#Confusion Matrix
pred<-ifelse(predicted>0.5,1,0)
tab1<- table(Predicted=pred, Actual=test_set$Exited)
tab1

#Misclassification error
1-sum(diag(tab1))/sum(tab1)

#Goodness of fit
with(model2, pchisq(null.deviance-deviance, df.null- df.residual, lower.tail = F))
