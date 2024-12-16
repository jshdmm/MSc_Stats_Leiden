library(nFactors)
library(EFA.dimensions)
library(GPArotation)
install.packages("pls")
library("pls")

# Clear the entire workspace
rm(list = ls())


setwd('/Users/joshd/Library/Mobile Documents/com~apple~CloudDocs/statistics_DS/statistical_learning/assignments/07')

data <- read.csv('data_alexithymia2.csv', sep = ';')

#exclude ID, sex, age
TAS_20 = data[,-c(1,2,3)]



# Define response and predictor variables
x = model.matrix(CESD~., data=TAS_20)[,-1] #remove intercept
y = TAS_20$CESD

# Create training and test sets
set.seed(789)
train = sample( 1:nrow(x) , nrow(x)/2) #indices
test = (-train) #indices
x.test = x[test,] #data, takes indeces from design matrix
y.test = y[test]


# run PCA with normalized variables, mean 0 and unit variance
pca_result_sc = prcomp(TAS_20, scale=TRUE)
summary(pca_result_sc)

#variance
pc.var <- pca_result_sc$sdev^2

# proportion of variance explained by each principal component
pve <- pc.var / sum(pc.var)
pve
cumsum(pve)

# plot variances explained by PC's
par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")


# Decicde on how many components to keep using Kaiser's rule and PVE
ev <- eigen(cor(TAS_20))
ev$values # Kaiser's rule, scree plot -> 6 components



#perform 10-fold cross-validation (on training data) to determine the number of components
#and estimate the test error (on validation data)
set.seed(20230315)
pcr_fit_train = pcr(CESD~., data=TAS_20, subset=train, scale=TRUE, validation="CV")
summary(pcr_fit_train)



validationplot(pcr_fit_train, val.type="MSEP") # MSEP -> lets keep 13 components, 91.67% variance explained on training data


# Get predictior error for fitted PCR model on training data
pcr_predict = predict(pcr_fit_train, x.test, ncomp=13)
pcr_mse = mean((pcr_predict-y.test)^2) # 115.7976 on 20 components, 106.1891 on 13 components
pcr_mse


#fit PCR with 13 components to full data
pcr_fit_full = pcr(y~x, scale=TRUE, ncomp=13)
summary(pcr_fit_full) #87.63% variance explained on full data



# coefficients
coef(pcr_fit_full)


# relationship predictors to PC's
pcr_fit_full$loadings

# relationship response to PC's
pcr_fit_full$Yloadings
