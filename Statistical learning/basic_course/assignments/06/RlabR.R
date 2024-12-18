#packages
install.packages("ISLR2")
library("ISLR2")
install.packages("nFactors")
library("nFactors")
install.packages("GPArotation")
library("GPArotation")
install.packages("psych")
library("psych")
install.packages("EFA.dimensions")
library("EFA.dimensions")

# checkup
str(College)

# clear out variables
data_College = College[,-c(1,2)]


# centered (default) and normalised variables
pca_result_sc = prcomp(data_College, scale=TRUE)
# not normalised/scaled
pca_result_unsc = prcomp(data_College, scale=FALSE)

# Explained variance per component - normalised
summary(pca_result_sc) # gives output for the variance explained

# Explained variance per component - not normalised
summary(pca_result_unsc)



# Explained variance per component plotted
par(mfrow=c(1,2))
plot(pca_result_sc)
plot(pca_result_unsc)


ev <- eigen(cor(data_College))
ev$values


# We first save PVE and cumulative PVE from the summary
prop_var_expl_sc = summary(pca_result_sc)$importance[2,]
cumul_prop_var_expl_sc = summary(pca_result_sc)$importance[3,]
prop_var_expl_unsc = summary(pca_result_unsc)$importance[2,]
cumul_prop_var_expl_unsc = summary(pca_result_unsc)$importance[3,]
# scree plot
par(mfrow=c(2,2))
plot(prop_var_expl_sc, type="b", xlab="Component number (sc.)",
     ylab="Proportion variance explained", ylim=c(0,1) )
plot(cumul_prop_var_expl_sc, type="b", xlab="Number of components (sc.)",
     ylab="Cumulative prop. var. expl.", ylim=c(0,1) )
plot(prop_var_expl_unsc, type="b", xlab="Component number (unsc.)",
     ylab="Proportion variance explained", ylim=c(0,1) )
plot(cumul_prop_var_expl_unsc, type="b", xlab="Number of components (unsc.)",
     ylab="Cumulative prop. var. expl.", ylim=c(0,1) )


cumul_prop_var_expl <- cumul_prop_var_expl_sc
nElem=length(cumul_prop_var_expl_sc)
ratios=(cumul_prop_var_expl[2:(nElem-1)]-cumul_prop_var_expl[1:(nElem-2)])/
  (cumul_prop_var_expl[3:nElem]-cumul_prop_var_expl[2:(nElem-1)])
ratios

nComponents_selected = which.max( ratios ) + 1
nComponents_selected

