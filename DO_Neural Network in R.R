
# Submitted By : Dennis Orellana

# Script File Name: DOrellana_Assignment4.R

# Purpose: This script is part of an assignment 
#     to use Neural Network in R.

############################# Load Data and Packages ##########################################################################


# load the "neuralnet","NeuralNetTools" and "ggplot2" packages
library("neuralnet")
library("NeuralNetTools")
library("ggplot2")

# Set the work directory
setwd("C:/Documents/R")


# Load and read the dataset from working directory
pima_diabetes<-read.csv(file="pima_diabetes.csv", head=TRUE, sep=",")


################################## Exploring the data ###########################################################################

# Preview the first 6 rows
head(pima_diabetes, 6)

# show the data frame
str(pima_diabetes)

# show the summary  
summary(pima_diabetes)

# show class for each variable
sapply(pima_diabetes, class)

# show count in class variable
table(pima_diabetes$class)

# check any missing values
colSums(is.na(pima_diabetes))

#Count for Pima disbetes
table(pima_diabetes$class)

#plot the class variable
barplot(table(pima_diabetes$class), col = "pink", xlab = "negative vs positive", main = " Test Results for Pima diabetes")

################################## Data Pre-processing ###########################################################################

# convert class as character
pima_diabetes$class<-as.character(pima_diabetes$class)

# change tested_positive as 1 and tested_negative as 0
pima_diabetes$class[pima_diabetes$class=="tested_positive"]<-1
pima_diabetes$class[pima_diabetes$class=="tested_negative"]<-0


# convert class variable as numeric
pima_diabetes$class<-as.numeric(pima_diabetes$class)

# view the first 10 rows
head(pima_diabetes, 10)

#scale the first 8 variables
pima_diabetes[1:8]<-scale(pima_diabetes[1:8])

# show the scale summary
summary(pima_diabetes[1:8])


###################################### Training and Test Set ####################################################################

# Run set the seed value
set.seed(12345)

# split the data into a training and test set
ind <- sample(2, nrow(pima_diabetes), replace = TRUE, prob = c(0.7, 0.3))
train.data <- pima_diabetes[ind == 1, ]
test.data <- pima_diabetes[ind == 2, ]


# Build the model
nn<-neuralnet(formula = class~preg+plas+pres+skin+insu+mass+pedi+age , 
              data = train.data, hidden=c(2), err.fct="ce", linear.output = FALSE)

#names display the network properties
names(nn)


# show result.matrix model
nn$result.matrix

# show the net.result
nn$net.result[[1]][1:8] 

###################################### Network Visualization ####################################################################


# plot the network
plotnet(nn)

# plot for weights in the hidden layers
garson(nn)

# predicted probabilities model
mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)
mypredict[1:8]


# confusion matrix for the training set
table(mypredict, train.data$class, dnn =c("Predicted", "Actual"))
mean(mypredict==train.data$class)

confusionMatrix(table(mypredict, train.data$class))

# confusion matrix for the test set
testPred <- compute(nn, test.data[, 0:8])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test.data$class, dnn =c("Predicted", "Actual"))
mean(testPred==test.data$class)

# Confusion matrix implemenation in caret package
require(caret)
require(e1071)
cmtrx<-confusionMatrix(table(testPred, test.data$class))

print(cmtrx)

# plot the confusion Matrix
fourfoldplot(cmtrx$table)


# create an alternative plot for confusion matrix 

draw_confusion_matrix <- function(cmtrx) {
  
  total <- sum(cmtrx$table)
  
  res <- as.numeric(cmtrx$table)
  
  # Generate color gradients. Palettes come from RColorBrewer.
  
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  
  getColor <- function (greenOrRed = "green", amount = 0) {
    
    if (amount == 0)
      
      return("#FFFFFF")
    
    palette <- greenPalette
    
    if (greenOrRed == "red")
      
      palette <- redPalette
    
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
    
  }
  
  # set the basic layout
  
  layout(matrix(c(1,1,2)))
  
  par(mar=c(2,2,2,2))
  
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix
  
  classes = colnames(cmtrx$table)
  
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  
  text(195, 435, classes[1], cex=1.2)
  
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  
  text(295, 435, classes[2], cex=1.2)
  
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  
  text(245, 450, 'Actual', cex=1.3, font=2)
  
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  
  text(140, 400, classes[1], cex=1.2, srt=90)
  
  text(140, 335, classes[2], cex=1.2, srt=90)
  
  # add in the cmtrx results
  
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics
  
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  
  text(10, 85, names(cmtrx$byClass[1]), cex=1.2, font=2)
  
  text(10, 70, round(as.numeric(cmtrx$byClass[1]), 3), cex=1.2)
  
  text(30, 85, names(cmtrx$byClass[2]), cex=1.2, font=2)
  
  text(30, 70, round(as.numeric(cmtrx$byClass[2]), 3), cex=1.2)
  
  text(50, 85, names(cmtrx$byClass[5]), cex=1.2, font=2)
  
  text(50, 70, round(as.numeric(cmtrx$byClass[5]), 3), cex=1.2)
  
  text(70, 85, names(cmtrx$byClass[6]), cex=1.2, font=2)
  
  text(70, 70, round(as.numeric(cmtrx$byClass[6]), 3), cex=1.2)
  
  text(90, 85, names(cmtrx$byClass[7]), cex=1.2, font=2)
  
  text(90, 70, round(as.numeric(cmtrx$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information
  
  text(30, 35, names(cmtrx$overall[1]), cex=1.5, font=2)
  
  text(30, 20, round(as.numeric(cmtrx$overall[1]), 3), cex=1.4)
  
  text(70, 35, names(cmtrx$overall[2]), cex=1.5, font=2)
  
  text(70, 20, round(as.numeric(cmtrx$overall[2]), 3), cex=1.4)
  
}


# show the alternative confusion matrix plot
draw_confusion_matrix(cmtrx)


# End of Script 