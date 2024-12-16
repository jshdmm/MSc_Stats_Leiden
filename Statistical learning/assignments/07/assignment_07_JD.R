setwd('/Users/joshd/Library/Mobile Documents/com~apple~CloudDocs/statistics_DS/statistical_learning/assignments/07')

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


# Decicde on how many components to keep using Kaiser's rule and PVE
ev <- eigen(cor(CESD))
ev$values # we keep 3 PC's




#### Assignment 07 ####




'''
Run k-means clustering with up to 10 clusters and plot the results on
a scree plot. Select the two best k’s and run k-means clustering again with both. Plot the
results for each pair of variables for both solutions and also create a contingency table. Note the
differences/similarities and decide which solution to keep
'''

# I keep 3 PC's from the previous exercise

#a

## create function to color PCA score vectors
Cols <- function(vec) {
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# labels
TAS_labs <- names(TAS_20)

png("plot_3PC.png")
par(mfrow = c(1, 2))
plot(pca_result_sc$x[, 1:2], col = Cols(TAS_labs), pch = 19,
     xlab = "Z1", ylab = "Z2")
plot(pca_result_sc$x[, c(1, 3)], col = Cols(TAS_labs), pch = 19,
     xlab = "Z1", ylab = "Z3")
dev.off()



# PCA summary
pca_result_sc


# variance explained
plot(pca_result_sc)

# Scree plot
pve <- 100 * pr.out$sdev^2 / sum(pca_result_sc$sdev^2)
par(mfrow = c(1, 2))
plot(pve,  type = "o", ylab = "PVE",
     xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE",
     xlab = "Principal Component", col = "brown3")


#run k-means for first 7 PC's
km.out <- kmeans(pca_result_sc$x[,1:3], 2, nstart = 50)
km.out # lowest withinSS for k = 2 (between_SS / total_SS =  36.6 %), for k = 3 (between_SS / total_SS =  49.1 %)

# cluster assignment
cluster_assignments <- km.out$cluster 

# contigency table
contingency_table <- table(cluster_assignments)
print(contingency_table)


# Define the range of k values you want to test
k_values <- 2:10

# Initialize a list to store contingency tables
contingency_tables <- list()
k_withinSS <- list()

# Iterate over each value of k
for (k in k_values) {
  # Perform k-means clustering
  kmeans_result <- kmeans(pca_result_sc$x[,1:7], centers = k)
  
  # Get cluster assignments
  cluster_assignments <- kmeans_result$cluster
  
  # Create a contingency table
  contingency_table <- table(cluster_assignments)
  
  # Store the contingency table in the list
  contingency_tables[[as.character(k)]] <- contingency_table
  
  # Store withinSS for this k
  k_withinSS[[as.character(k)]] <- kmeans_result$betweenss / kmeans_result$tot.withinss
}

# Access a contingency table for a specific k value
print(contingency_tables[["3"]])  # Accessing contingency table for k = 2
print(k_withinSS)




# plot first 2 PC score vectors
png("kmeans2_PC2_PC3.png")
plot(pca_result_sc$x[, c(2,3)], col = (km.out$cluster + 1),
     main = "K-Means Clustering Results with K = 2",
     xlab = "PC2", ylab = "PC3", pch = 20, cex = 2)
dev.off()



'''
b) Select 20 cases randomly and perform hierarchical clustering with different linkage methods.
Compare the obtained results. Are there clusters that appear in all solutions?
'''

# no standardization


# sample 20 random observations
set.seed(123)

# Sample 20 random row indices
sampled_indices <- sample(nrow(TAS_20), 20)

# Sample 20 random observations from the data frame
TAS_sample <- TAS_20[sampled_indices, ]



# hierarchical clustering
par(mfrow = c(1, 3))
data.dist <- dist(TAS_sample)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "",
     labels = TAS_labs, main = "Complete Linkage")
plot(hclust(data.dist, method = "average"),
     labels = TAS_labs, main = "Average Linkage",
     xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"),
     labels = TAS_labs,  main = "Single Linkage",
     xlab = "", sub = "", ylab = "")


# cut Dendrogram
png("hier_clust_table_3.png")
hc.out <- hclust(dist(TAS_sample))
hc.clusters <- cutree(hc.out, 3)
table(hc.clusters, TAS_labs)
dev.off()
