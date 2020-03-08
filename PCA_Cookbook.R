setwd('C:/Users/wolfg/Desktop/Uni/HU Master/Datenanalyse II/Hausarbeit')
z <- read.table('Matlab_input.csv', header = TRUE, sep = ",")
z <- scale(z)
eigen_cor <- eigen(cov(z))
E <- eigen_cor$vectors

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


######################################

# Various ways to compute Hotelling's T^2:
library("matlib")
num_comp <- 2

inverse_cov <- diag(eigen_cor$values[1:num_comp], nrow = num_comp, ncol = num_comp)^-1
inverse_cov[is.infinite(inverse_cov)] <- 0

tsquared <- diag(z %*% E[,1:num_comp] %*% inverse_cov %*% t(z %*% E[,1:num_comp]))

# which is equivalent to the Mahalanobis distance: 

tsquared_mahal <- mahalanobis(scores[,1:num_comp], center = FALSE, cov(cbind(scores[,1], scores[,num_comp])), inverted = FALSE)

# Using mdatools package:

#library("mdatools")

#result <- pca(z, ncomp = 2, center = FALSE, scale = FALSE, alpha = 0.05)
#plotResiduals(result, show.labels = TRUE)
#result$res$cal$residuals # same as manual residuals
#result$res$cal$T2
#tsquared_mda <- result$res$cal$T2
#tsquared_lim95 <- result$T2lim[1]

tsquared_lim95 <- (((dim(z)[1] - 1) * (dim(z)[1] + 1) * num_comp) / (dim(z)[1] * (dim(z)[1] - num_comp))) * qf(p =.95, df1 = num_comp, df2 = dim(z)[1] - num_comp)


# result <- pca(z, ncomp = 2, center = FALSE, scale = FALSE, alpha = 0.01)
# tsquared_lim99 <- result$T2lim[1]
tsquared_lim99 <- (((dim(z)[1] - 1) * (dim(z)[1] + 1) * num_comp) / (dim(z)[1] * (dim(z)[1] - num_comp))) * qf(p =.99, df1 = num_comp, df2 = dim(z)[1] - num_comp)

plot(tsquared, type = 'l', ylim = c(0,tsquared_lim99*1.1))
abline(h=tsquared_lim99, col = 'red', lty = 2)
abline(h=tsquared_lim95, col = 'darkgreen', lty = 2)



simpleEllipse <- function(x, y, alfa=0.95, len=200) {
  N <- length(x)
  A <- 2
  mypi <- seq(0, 2 * pi, length=len)
  r1 <- sqrt(var(x) * qf(alfa, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))
  r2 <- sqrt(var(y) * qf(alfa, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))
  cbind(r1 * cos(mypi) + mean(x), r2 * sin(mypi) + mean(y))
}

confidence_ellipse95 <- simpleEllipse(scores[,1], scores[,2], alfa = 0.95, len=500)
confidence_ellipse99 <- simpleEllipse(scores[,1], scores[,2], alfa = 0.99, len=500)

plot(scores[,1], scores[,2], xlim = c(min(confidence_ellipse99[,1]), max(confidence_ellipse99[,1])), 
     ylim = c(min(confidence_ellipse99[,2]), max(confidence_ellipse99[,2])))
abline(h = 0, v = 0)
points(confidence_ellipse95, type = 'l', lty = 2, col = 'darkgreen')
points(confidence_ellipse99, type = 'l', lty = 2, col = 'red')

# SPE_1

a = 1
Xhat <- pc_cor$x[,seq(1,a)] %*% t(pc_cor$rotation[,seq(1,a)])
RMSE(z, Xhat)
E <- z - Xhat
E2 <- E * E
SPE_1 <- sqrt(apply(E2, 1, sum))
# pca(z, ncomp = a, center = FALSE, scale = FALSE, alpha = 0.05)$Qlim[1]

# Box (1954) methods: weighted Chi-squared

v <- var(SPE_1)
m <- mean(SPE_1)
h <- (2*m^2)/v
g <- v / (2*m)
lim <- g*qchisq(.95, df=h) 
lim

plot(SPE_1)
abline(h = lim, lty = 2, col = 'red')


a = 2
Xhat <- pc_cor$x[,seq(1,a)] %*% t(pc_cor$rotation[,seq(1,a)])
RMSE(z, Xhat)
E <- z - Xhat
E2 <- E * E
SPE_2 <- sqrt(apply(E2, 1, sum))
# pca(z, ncomp = a, center = FALSE, scale = FALSE, alpha = 0.05)$Qlim[1]

v <- var(SPE_2)
m <- mean(SPE_2)
h <- (2*m^2)/v
g <- v / (2*m)
lim <- g*qchisq(.95, df=h) 
lim

plot(SPE_2)
abline(h = lim, lty = 2, col = 'red')




a = 9
Xhat <- pc_cor$x[,seq(1,a)] %*% t(pc_cor$rotation[,seq(1,a)])
RMSE(z, Xhat)
E <- z - Xhat
E2 <- E * E
SPE_9 <- sqrt(apply(E2, 1, sum))
# pca(z, ncomp = a, center = FALSE, scale = FALSE, alpha = 0.05)$Qlim[1]


v <- var(SPE_1)
m <- mean(SPE_1)
h <- (2*m^2)/v
g <- v / (2*m)
lim <- g*qchisq(.95, df=h) 
lim

plot(SPE_9)
abline(h = lim, lty = 2, col = 'red')



par(mfrow=c(3,1))

plot(SPE_1, type = 'l', col = 'darkgreen')
plot(SPE_2, type = 'l', col = 'red')
plot(SPE_9, type = 'l', col = 'blue')

# 3D plot for t1, t2 (i.e. scores of variable 1, ..., k, just using the first, two, ... k principal loadings vectors) and SPEs

 #if (!requireNamespace("BiocManager", quietly = TRUE))
 # install.packages("BiocManager")

# BiocManager::install("pcaMethods")

# library("pcaMethods")


# pcaMethods::simpleEllipse(scores[,1], scores[,2], alfa = 0.95, len = 200)


library("rgl")
library("car")
scatter3d(x = scores[,1], y = scores[,2], z = SPE_2,
          point.col = "blue", surface=FALSE, 
          axis.scales = FALSE, ellipsoid = TRUE)
# This gives a fairly complete picture of the major dimensions in the model: the explained variation on-the-plane, 
# given by t1 and t2, and the residual distance off-the-plane, summarized by SPE




