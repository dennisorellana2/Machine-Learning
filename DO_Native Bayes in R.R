
# Submitted By : Dennis Orellana

# Purpose: This script is part of an assignment
#     to use Native Bayes in R.

############################# Load Data and Packages #################################################################

# Arules, e1071, and lattice packages are already installed
# Load the folowing packages
library("arules")
library("e1071")
library("lattice")
library("ggplot2")
library("caret")

# Load and read the dataset from working directory
setwd("C:/Documents/Data 630/Module 3/week 5/Assignment 2")
Creditcard_clients <- read.csv("DefaultCreditCardClients.csv")


################################## Exploring the data ###############################################################

# show dataframe and dataset
str(Creditcard_clients)
View(Creditcard_clients)


# Find any missing values in the data
nrow(Creditcard_clients[!complete.cases(Creditcard_clients),])
Creditcard_clients[!complete.cases(Creditcard_clients),]
apply(Creditcard_clients, 2, function (Creditcard_clients) sum(is.na(Creditcard_clients)))


################################## Data Pre-processing ##############################################################

# show summary
summary(Creditcard_clients)


#remove the unique identifier
Creditcard_clients$ID<-NULL

#discretize the  LIMIT_BAL variable
Creditcard_clients$LIMIT_BAL<-discretize(Creditcard_clients$LIMIT_BAL, "frequency", categories=6)
summary(Creditcard_clients$LIMIT_BAL)

#discretize the income variable
Creditcard_clients$AGE<-discretize(Creditcard_clients$AGE, "frequency", categories=6)
summary(Creditcard_clients$AGE)


#discretize all the BILL_AMT1 through BILL_AMT6 variables
Creditcard_clients$BILL_AMT1<-discretize(Creditcard_clients$BILL_AMT1, "frequency", categories=6)
summary(Creditcard_clients$BILL_AMT1)

Creditcard_clients$BILL_AMT2<-discretize(Creditcard_clients$BILL_AMT2, "frequency", categories=6)
summary(Creditcard_clients$BILL_AMT2)

Creditcard_clients$BILL_AMT3<-discretize(Creditcard_clients$BILL_AMT3, "frequency", categories=6)
summary(Creditcard_clients$BILL_AMT3)

Creditcard_clients$BILL_AMT4<-discretize(Creditcard_clients$BILL_AMT4, "frequency", categories=6)
summary(Creditcard_clients$BILL_AMT4)

Creditcard_clients$BILL_AMT5<-discretize(Creditcard_clients$BILL_AMT5, "frequency", categories=6)
summary(Creditcard_clients$BILL_AMT5)

Creditcard_clients$BILL_AMT6<-discretize(Creditcard_clients$BILL_AMT6, "frequency", categories=6)
summary(Creditcard_clients$BILL_AMT6)

#discretize all the PAY_AMT1 through PAY_AMT6 variables
Creditcard_clients$PAY_AMT1<-discretize(Creditcard_clients$PAY_AMT1, "frequency", categories=4)
summary(Creditcard_clients$PAY_AMT1)

Creditcard_clients$PAY_AMT2<-discretize(Creditcard_clients$PAY_AMT2, "frequency", categories=4)
summary(Creditcard_clients$PAY_AMT2)

Creditcard_clients$PAY_AMT3<-discretize(Creditcard_clients$PAY_AMT3, "frequency", categories=4)
summary(Creditcard_clients$PAY_AMT3)

Creditcard_clients$PAY_AMT4<-discretize(Creditcard_clients$PAY_AMT4, "frequency", categories=4)
summary(Creditcard_clients$PAY_AMT4)

Creditcard_clients$PAY_AMT5<-discretize(Creditcard_clients$PAY_AMT5, "frequency", categories=4)
summary(Creditcard_clients$PAY_AMT5)

Creditcard_clients$PAY_AMT6<-discretize(Creditcard_clients$PAY_AMT6, "frequency", categories=4)
summary(Creditcard_clients$PAY_AMT6)


#convert sex, education, marriage, pay_0, pay_2 through pay_6, and default paymeny next month variables as factors
Creditcard_clients$SEX<-factor(Creditcard_clients$SEX)

Creditcard_clients$EDUCATION<-factor(Creditcard_clients$EDUCATION)

Creditcard_clients$MARRIAGE<-factor(Creditcard_clients$MARRIAGE)
 
Creditcard_clients$PAY_0<-factor(Creditcard_clients$PAY_0)

Creditcard_clients$PAY_2<-factor(Creditcard_clients$PAY_2)

Creditcard_clients$PAY_3<-factor(Creditcard_clients$PAY_3)

Creditcard_clients$PAY_4<-factor(Creditcard_clients$PAY_4)

Creditcard_clients$PAY_5<-factor(Creditcard_clients$PAY_5)

Creditcard_clients$PAY_6<-factor(Creditcard_clients$PAY_6)

Creditcard_clients$default.payment.next.month<-factor(Creditcard_clients$default.payment.next.month, levels = 0:1, labels=c("No", "Yes"))

# show summary and data after the data pre-processing 
summary(Creditcard_clients)
str(Creditcard_clients)

###################################### Naive Bayes Classifier ###########################################################

#make sure that the result is reproducible

set.seed(1234)

#split the data into a training and test set
ind <- sample(2, nrow(Creditcard_clients), replace = TRUE, prob = c(0.7, 0.3))
Train <- Creditcard_clients[ind == 1, ]
Test <- Creditcard_clients[ind == 2, ]

#build the model and store in a variable model
model_naive<-naiveBayes(default.payment.next.month~SEX+MARRIAGE+AGE+PAY_0+PAY_6+BILL_AMT1+BILL_AMT6+PAY_AMT1+PAY_AMT6, Train, laplace=1)

#output the model
model_naive

###################################### Visualization ################################################################

#confusion matrix for the training set; need to round the estimated values
table(predict(model_naive, Train), Train$default.payment.next.month)

#confusion matrix for the test data
table(predict(model_naive, Test), Test$default.payment.next.month)

pred_naive<-predict(model_naive, newdata = Test)
confusionMatrix(data=pred_naive, reference = Test$default.payment.next.month)

#mosaic plot
mosaicplot(table(predict(model_naive, Test), Test$default.payment.next.month), shade=TRUE, main="Predicted vs. Actual Default Next Month Payment")


# End of Script 
