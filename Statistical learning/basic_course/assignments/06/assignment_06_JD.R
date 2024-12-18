setwd('/Users/joshd/Library/Mobile Documents/com~apple~CloudDocs/statistics_DS/statistical_learning/assignments/06')

CESD <- read.csv('data_alexithymia2.csv', sep = ';')

#checkup
str(CESD)
names(CESD)
dim(CESD)

#check for missing values
sum(is.na(CESD))


# convert from int to numeric data type
CESD[, 3:24] <- lapply(CESD[,3:24], as.numeric)


#exclude ID, sex, age, and CESD
TAS_20 = CESD[,-c(1,2,3, 24)]

apply(TAS_20, 2, mean, na.rm = TRUE)
apply(TAS_20, 2, var)


# run PCA with normalized variables, mean 0 and unit variance
pca_result_sc = prcomp(TAS_20, scale=TRUE)
summary(pca_result_sc)

# means before centering
pca_result_sc$center

# standard deviations before centering
pca_result_sc$scale

# Prinicipal component loadings
pca_result_sc$rotation

# dimensionality
dim(pca_result_sc$x)

# plot first 2 PC's
pca_result_sc$rotation = -pca_result_sc$rotation
pca_result_sc$x = -pca_result_sc$x
biplot(pca_result_sc, scale = 0, cex = 0.5)


# standard deviations of each principal component
pca_result_sc$sdev
 
#variance
pc.var <- pca_result_sc$sdev^2


# proportion of variance explained by each principal component
pve <- pc.var / sum(pc.var)
pve


# plot variances explained by PC's
par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")


# Decicde on how many components to keep using Kaiser's rule
ev <- eigen(cor(CESD))
ev$values # we keep 7 PC's
