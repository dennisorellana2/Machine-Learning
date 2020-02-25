
# Submitted By : Dennis O.

# Purpose: This script is part of an assignment 
#     to use Association Rules Analysis in R.

############################# Load Data and Packages #################################################################

# Arules and arulesviz packages are already installed
# Load the arules and arulesviz packages
library("arules")
library("arulesViz")

# Set the work directory
setwd("C:")

# Load and read the dataset from working directory
setwd("C:/Documents/Data 630/Module 2/week 3/Assignment 1")
wages <- read.csv("wages.csv")


################################## Exploring the data ################################################################

# show dataframe
str(wages)

# Display the descriptive statistics for the following Variables 
summary(wages$education)
summary(wages$experience)
summary(wages$age)

# Find any missing values in the data
nrow(wages[!complete.cases(wages),])
wages[!complete.cases(wages),]
apply(wages, 2, function (wages) sum(is.na(wages)))


################################## Data Pre-processing ###############################################################


# Discretization for education, experience, wage,and age
wages$education<-discretize(wages$education, method="interval", breaks=6)
summary(wages$education)

wages$experience<-discretize(wages$experience, method="interval", breaks=6)
summary(wages$experience)

wages$wage<-discretize(wages$wage, method="interval", breaks=6)
summary(wages$wage)

wages$age<-discretize(wages$age, method="interval", breaks=6)
summary(wages$age)

# factor for sex, union,race,occupation,sector, and marital status
wages$sex<-factor(wages$sex)
summary(wages$sex)

wages$union<-factor(wages$union)
summary(wages$union)

wages$race<-factor(wages$race)
summary(wages$race)

wages$occupation<-factor(wages$occupation)
summary(wages$occupation)

wages$sector<-factor(wages$sector)
summary(wages$sector)

wages$marital_status<-factor(wages$marital_status)
summary(wages$marital_status)

# After results show dataframe
str(wages)


###################################### Rule Method ###################################################################

# Apriori rule
rules<-apriori(wages)
rules

# Inspect the rule
inspect(rules)

# Remove the blanks 
rules <- apriori(wages, parameter= list(supp=0.4, conf=0.7, minlen=2))
inspect(rules)

# Generate the rules that have only the "sex=  0" or "sex= 1" 
rules<-apriori(wages, parameter= list(supp=0.1, conf=0.8, minlen=2), appearance=list(rhs=c("sex=0", "sex=1"), default="lhs"))
inspect(rules)

# Prune the returned rules
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules 
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# Rules remaining after the redundant rule has been removed
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
summary(rules.pruned)


###################################### Rules visualization ###########################################################

# Plot the pruned rules scatterplot
plot(rules.pruned)

# Pruned rules grouped plot
plot(rules.pruned, method = "grouped")

# Matrix plot
plot(rules.pruned, method="matrix", measure=c("lift", "confidence"))


# End of Script 