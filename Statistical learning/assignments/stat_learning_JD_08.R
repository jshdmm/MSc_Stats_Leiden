library(nFactors)
library(EFA.dimensions)
library(GPArotation)
install.packages("pls")
library("pls")


setwd('/Users/joshd/Library/Mobile Documents/com~apple~CloudDocs/statistics_DS/statistical_learning/assignments/07')

data <- read.csv('data_alexithymia2.csv', sep = ';')

#exclude ID, sex, age
TAS_20 = data[,-c(1,2,3)]



# Define response and predictor variables
x = model.matrix(CESD~., data=TAS_20)
y = TAS_20$CESD

# Create trainng and test sets
set.seed(789)
train = sample( 1:nrow(x) , nrow(x)/2)
test = (-train)
x.test = x[test,]
y.test = y[test]


#perform 10-fold cross-validation (on training data) to determine the number of components
#and estimate the test error (on validation data)
set.seed(20230315)
pcr_fit_train = pcr(CESD~., data=TAS_20, subset=train, scale=TRUE, validation="CV")
summary(pcr_fit_train)
