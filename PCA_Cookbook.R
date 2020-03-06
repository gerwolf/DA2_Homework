setwd('C:/Users/wolfg/Desktop/Uni/HU Master/Datenanalyse II/Hausarbeit')
z <- read.table('Matlab_input.csv', header = TRUE, sep = ",")
z <- scale(z)
eigen_cor <- eigen(cov(z))
E <- eigen_cov$vectors

pc_cor <- prcomp(z, center = F, scale = F)

E

E[,1] <- E[,1]*-1
E[,3] <- E[,3]*-1
E[,4] <- E[,4]*-1
E[,5] <- E[,5]*-1
E[,6] <- E[,6]*-1

scores <- z %*% E # Now scores and the ones from pc_cor$x are the same!

# alternative computation of scores: predict(pc_cor) = z %*% E

# Note: computation of scores is independent of number of components chosen, it's just multiplying every row with
# each eigenvector separately, e.g. score2 = z %*% E[,2]

# total variance of data: equal to the number of columns as their individual variance have been scaled to 1.

sum(diag(var(z)))

library("psych")

psych_pca <- psych::principal(z, nfactors = 9, rotate = "none")

psych_pca$scores[,1]*sqrt(psych_pca$values[1]) # == scores[,1]

# transform loadings of psych::principal to unit-length eigenvectors according to 1/sqrt(a^2 + b^2) * (a,b)

1/sqrt(sum(psych_pca$loadings[,1]^2))*psych_pca$loadings[,1] # is identical to pc_cor$rotation[,1] and E[,1]

# transforming unit-length eigenvectors to loadings like in psych::principal:

manual_loadings <- matrix(nrow = dim(z)[2], ncol = dim(z)[2])

for (i in 1:dim(z)[2]){
  
  manual_loadings[,i] <- E[,i] * sqrt(eigen_cor$values[i])
  
}

# Still signs are interchanged, affected components are 3 and 9, change signs manually

manual_loadings[,3] <- manual_loadings[,3]*-1
manual_loadings[,9] <- manual_loadings[,9]*-1

# Scores in psych are computed according to 

(z %*% E[,1]) / sqrt(eigen_cor$values[1]) #  == psych_pca$scores[,1]

# i.e. using the unit-length eigenvectors and dividing them by the square root of the corresponding eigenvalue

# Fitting != scores!

# Now fitting the reduced model, that is choosing an optimal number of components and projecting the original data to a new 
# feature space by using only the coefficients of the chosen components

RMSE = function(m, o){
  
  sqrt(mean((m - o)^2))
  
}

# 1) In terms of the not unit-length eigenvectors

# all three expressions are equivalent

scores <- z %*% E # can be divided by the square root of the corresponding eigenvalue: scores <- (z %*% E) / sqrt(eigen_cor$values[1])

#for (i in 1:dim(z)[2]){
  
#  scores[,i] <- scores[,i] / sqrt(eigen_cor$values[i])
  
#}

scores_from_psych <- (z %*% (1/sqrt(sum(psych_pca$loadings[,1]^2))*psych_pca$loadings[,1])) / sqrt(psych_pca$values[1]) # == psych_pca$scores[,1]
scores_from_princomp <- (z %*% pc_cor$rotation[,1]) / sqrt(eigen_cor$values[1])

num_comp <- 1

fit <- scores[, 1:num_comp] %*% t(E[, 1:num_comp]) # here specify the number of components to be projected
residuals <- z - fit
RMSE(z, fit)
1-(sum(diag(var(residuals)))/ sum(diag(var(z))))

fit_psych <- psych_pca$scores[, 1:num_comp] %*% t(psych_pca$loadings[, 1:num_comp])
residuals_psych <- z - fit_psych
RMSE(z, fit_psych)
1-(sum(diag(var(residuals_psych)))/ sum(diag(var(z))))

fit_princomp <- pc_cor$x[, 1:num_comp] %*% t(pc_cor$rotation[, 1:num_comp])
princomp_residuals <- z - fit_princomp
RMSE(z, fit_princomp)
1-(sum(diag(var(princomp_residuals)))/ sum(diag(var(z))))


# SPE_1

a = 1
Xhat <- pc_cor$x[,seq(1,a)] %*% t(pc_cor$rotation[,seq(1,a)])
E <- z - Xhat
E2 <- E * E
SPE_1 <- sqrt(apply(E2, 1, sum))

plot(SPE_1, type = 'l')
