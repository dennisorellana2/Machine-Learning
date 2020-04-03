
# Submitted By : Dennis Orellana

# Purpose: This script is part of an assignment 
#     to use Decision Tree in R.

############################# Load Data and Packages #################################################################

# load party package for decision tree

library("party")

# load the following packages
library("lattice")
library("ggplot2")
library("caret")
library('readr')
library("rpart")

# Set the work directory
setwd("C:")

# Load and read the dataset from working directory
bank_marketing <- read_csv("C:/Documents/R")


################################## Exploring the data ##############################################################

# show dataframe
str(bank_marketing)
summary(bank_marketing)

# Find any missing values in the data
apply(bank_marketing, 2, function (bank_marketing) sum(is.na(bank_marketing)))

#create a new table for suscribed-term-deposited
counts<- table(bank_marketing$`suscribed-term-deposited`)

# Shown the table dataframe
str(counts)

# Bar Plot on suscribed-term-deposited
barplot(counts, main="Yes vs No suscribed-term-deposited",
        col=c("red", "green"))

################################## Data Pre-processing #############################################################

# replace "?" with "NA"
bank_marketing$`passed-days`<- replace(bank_marketing$`passed-days`,bank_marketing$`passed-days`=="?", NA)

#Check for and handle missing values
bank_marketing$`yearly-balance`[is.na(bank_marketing$`yearly-balance`)]<-mean(bank_marketing$`yearly-balance`, na.rm=TRUE)
bank_marketing$`passed-days`[is.na(bank_marketing$`passed-days`)]<-NULL

# convert the dependent variable suscribed term deposited to a factor
bank_marketing$`suscribed-term-deposited`<-factor(bank_marketing$`suscribed-term-deposited`)

Summary(bank_marketing$age)

#convert all the character independent variables to factors 

bank_marketing$job<-factor(bank_marketing$job)

bank_marketing$`marital-status`<-factor(bank_marketing$`marital-status`)

bank_marketing$education<-factor(bank_marketing$education)

bank_marketing$`default-credit`<-factor(bank_marketing$`default-credit`)

bank_marketing$`housing-loan`<-factor(bank_marketing$`housing-loan`)

bank_marketing$`personal-loan`<-factor(bank_marketing$`personal-loan`)

bank_marketing$`last-contact-type`<-factor(bank_marketing$`last-contact-type`)

bank_marketing$`last-contact-month`<-factor(bank_marketing$`last-contact-month`)

bank_marketing$`passed-days`<-factor(bank_marketing$`passed-days`)

bank_marketing$`outcome-previous-campaign`<-factor(bank_marketing$`outcome-previous-campaign`)

# show class type for each variables
sapply(bank_marketing,class)


###################################### ctree Method ###################################################################

# split the data into a training and test set
set.seed(1234)
ind <- sample(2, nrow(bank_marketing), replace = TRUE, prob = c(0.7, 0.3))
train <- bank_marketing[ind == 1, ]
test <- bank_marketing[ind == 2, ]

# Run the method on a training data
myFormula<-`suscribed-term-deposited`~.
model<- ctree(myFormula, data = train)

#output the tree structure
print(model) 


###################################### Decision Trees Visualization ###################################################

# visualize the tree
nodes(model, 2)
plot(model)
plot(model, type="simple")

# Plot the model
par(xpd = TRUE)
plot(model, compress=TRUE)   # plot the tree skelleton
text(model, use.n = TRUE) 


# confusion matrix
table(predict(model), train$`suscribed-term-deposited`)
prop.table(table(predict(model), train$`suscribed-term-deposited`))

trainPred<-predict(model , newdata = train)

confusionMatrix(data=trainPred, reference = train$`suscribed-term-deposited`)


# Evaluate the model on a test data
testPred <- predict(model, newdata = test)
table (testPred, test$`suscribed-term-deposited`)

confusionMatrix(data=testPred, reference = test$`suscribed-term-deposited`)

model<-rpart(`suscribed-term-deposited`~., train, method="class")

# Look for an inflection point to determine the optimal number of splits
model$cptable

#an alternative way to print complexity table
printcp(model)

#plot complexity table.  
plotcp(model)

# The table shows the error reduction at each split (xerror)

# End of Script 