#############################################################

# Data prep

setwd('C:/Users/wolfg/Desktop/Uni/HU Master/Datenanalyse II/Hausarbeit')

library("rio")
x <- import("https://docs.google.com/spreadsheets/d/1h7XhLd2Byp4OcXSdtxHly9iS7RA673RJ/export?format=csv&gid=1083477596")
x$commentCount <- as.integer(x$commentCount)
x$viewsCount <- as.numeric(x$viewsCount)
x$acousticness <- as.numeric(sub(",", ".", x$acousticness, fixed = TRUE))
x$danceability <- as.numeric(sub(",", ".", x$danceability, fixed = TRUE))
x$energy <- as.numeric(sub(",", ".", x$energy, fixed = TRUE))
x$instrumentalness <- as.numeric(sub(",", ".", x$instrumentalness, fixed = TRUE))
x$liveness <- as.numeric(sub(",", ".", x$liveness, fixed = TRUE))
x$loudness <- as.numeric(sub(",", ".", x$loudness, fixed = TRUE))
x$speechiness <- as.numeric(sub(",", ".", x$speechiness, fixed = TRUE))
x$tempo <- as.numeric(sub(",", ".", x$tempo, fixed = TRUE))
x$valence <- as.numeric(sub(",", ".", x$valence, fixed = TRUE))
x$key <- as.factor(x$key)
x$time_signature <- as.factor(x$time_signature)
min_max_normalize <- function(x)
{
  return( (1000-10)*((x- min(x)) /(max(x)-min(x))) + 10)
}
x$acousticness <- min_max_normalize(x$acousticness)
x$danceability <- min_max_normalize(x$danceability)
x$energy <- min_max_normalize(x$energy)
x$instrumentalness <- min_max_normalize(x$instrumentalness)
x$liveness <- min_max_normalize(x$liveness)
x$loudness <- min_max_normalize(x$loudness)
x$speechiness <- min_max_normalize(x$speechiness)
x$tempo <- min_max_normalize(x$tempo)
x$valence <- min_max_normalize(x$valence)
x$Genre <- as.factor(x$Genre)
x$key <- as.factor(x$key)
x$mode <- as.factor(x$mode)
x$Charts <- as.factor(x$Charts)

library("dplyr")

x <- dplyr::select(x, -one_of('Streams'))
x <- x[complete.cases(x), ]

audio_features <- c('acousticness' , 'danceability' ,'energy', 'instrumentalness', 'liveness', 'loudness', 'speechiness', 'tempo', 'valence')
x_features <- dplyr::select(x, audio_features)

#############################################################

# La grand final: PCA!

z <- scale(x_features)
eigen_cor <- eigen(cov(z))
E <- eigen_cor$vectors

pc_cor <- prcomp(z, center = F, scale = F)

par(mfrow=c(1,2))

plot(pc_cor, main="Scree plot as bar chart (cor)")
plot(pc_cor$sdev^2, type="b", main="Scree plot (cor)")

par(mfrow=c(1,1))
plot((pc_cor$sdev^2)/round(sum(pc_cor$sdev^2))*100, type="b", main="Scree plot (cor, percentage of variance)")

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

library(corrplot)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(pc_cor$rotation, method="color", col=col(200), 
        addCoef.col = "black", # Add coefficient of correlation
        tl.col="black", tl.srt=90, #Text label color and rotation
         diag=TRUE, mar = c(0,0,5,5), title = 'PCA - loadings matrix')

plot(scores[,1], scores[,2]) # both plots are identical
plot(pc_cor$x[,1], pc_cor$x[,2]) # both plots are identical

# total variance of data: equal to the number of columns as their individual variance have been scaled to 1.

sum(diag(var(z)))

library("psych")

psych_pca <- psych::principal(z, nfactors = 9, rotate = "none")

scree(z)



# plot.matrix::plot(psych_pca$loadings, las=2)

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

par(mfrow=c(1,2))
biplot(pc_cor) # loadings plot with loadings scaled to fit in same graph with scores
plot(pc_cor$rotation[, 1:2]) # loadings in original measures
text(pc_cor$rotation[, 1:2], as.character(colnames(z)), pos = 1, cex = 0.7, offset = 0.1)

palette <- rainbow(length(levels(as.factor(x$Genre))))
my_colors <- palette[as.numeric(x$Genre)]

par(mfrow=c(1,2))
plot(scores[, 1:2], col = my_colors)
#plot(pc_cor$rotation[, 1:2])
plot(E[, 1:2])
text(E[, 1:2], as.character(colnames(z)), pos = 1, cex = 0.7, offset = 0.1)

# How many components to choose?

# Kaiser criterion: two eigenvalues > 1

eigen_cor$values

cumsum(eigen_cor$values)/length(audio_features)

# identical (we can use length because the total variance of the standardized variables is the sum of the diagonal elements which is 1 in every variable)

# According to the 90% criterion there would be 5 components needed to appropriately retain the variance in the data by reducing the feature space from nine to five.

# Fitting != scores!

# Now fitting the reduced model, that is choosing an optimal number of components and projecting the original data to a new 
# feature space by using only the coefficients of the chosen components

RMSE = function(m, o){
  
  sqrt(mean((m - o)^2))
  
}

# 1) In terms of the unit-length eigenvectors

# all three expressions are equivalent

scores <- z %*% E # can be divided by the square root of the corresponding eigenvalue: scores <- (z %*% E) / sqrt(eigen_cor$values[1])

#for (i in 1:dim(z)[2]){
  
#  scores[,i] <- scores[,i] / sqrt(eigen_cor$values[i])
  
#}

scores_from_psych <- (z %*% (1/sqrt(sum(psych_pca$loadings[,1]^2))*psych_pca$loadings[,1])) / sqrt(psych_pca$values[1]) # == psych_pca$scores[,1]
scores_from_princomp <- (z %*% pc_cor$rotation[,1]) / sqrt(eigen_cor$values[1])

######################################

# Fitting the model 

fits <- matrix(nrow = dim(z)[2], ncol = 3)

for (i in 1:dim(z)[2]){
  
  num_comp <- i
  fit <- scores[, 1:num_comp] %*% t(E[, 1:num_comp])
  residuals <- z - fit
  rmse <- RMSE(z, fit)
  r2 <- 1-(sum(diag(var(residuals)))/ sum(diag(var(z))))
  
  fits[i,1] <- num_comp
  fits[i,2] <- rmse
  fits[i,3] <- r2
  
}

par(mfrow=c(1,1))
plot(fits[,1], fits[,2], type = "l", ylab = 'RMSE', xlab = 'Number of components')
par(new = TRUE)
plot(fits[,1], fits[,3], type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(fits[,3])))
mtext("R^2", side = 4, line = -1)
abline(v = 2, lty = 2, col = 'green')
mtext("Kaiser criterion", side = 1, line = 2, adj = 0.1)
abline(v=min(which(fits[,3]>=0.9)), lty = 2, col = 'red')
mtext("90% rule", side = 1, line = 1, adj = 0.5)


# the expressions below yield all equivalent results

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

# Squared prediction errors / Q-residuals

par(mfrow=c(1,1))

a <- 1
Xhat <- pc_cor$x[,seq(1,a)] %*% t(pc_cor$rotation[,seq(1,a)])
RMSE(z, Xhat)
res <- z - Xhat
E2 <- res * res
SPE_1 <- apply(E2, 1, sum) # not using sqrt(apply(E2, 1, sum))!
#pca(z, ncomp = a, center = FALSE, scale = FALSE, alpha = 0.05)$Qlim[1]

# Box (1954) method: weighted Chi-squared

v <- var(SPE_1)
m <- mean(SPE_1)
h <- (2*m^2)/v
g <- v / (2*m)
lim_k1_95 <- g*qchisq(.95, df=h) 
plot(SPE_1, ylab = paste('SPE after using', a, 'component'), xlab = 'index')
abline(h = lim_k1_95, lty = 2, col = 'darkgreen')
mtext(paste('RMSE =', round(sqrt(mean(SPE_1)), digits = 4)), side=1, line=3.5, at=9)
sqrt(mean(SPE_1))


a <- 2
Xhat <- pc_cor$x[,seq(1,a)] %*% t(pc_cor$rotation[,seq(1,a)])
RMSE(z, Xhat)
res <- z - Xhat
E2 <- res * res
#SPE_2 <- sqrt(apply(E2, 1, sum))
SPE_2 <- apply(E2, 1, sum)
# pca(z, ncomp = a, center = FALSE, scale = FALSE, alpha = 0.05)$Qlim[1]

v <- var(SPE_2)
m <- mean(SPE_2)
h <- (2*m^2)/v
g <- v / (2*m)
lim_k2_95 <- g*qchisq(.95, df=h) 
plot(SPE_2, ylab = paste('SPE after using', a, 'components'), xlab = 'index')
abline(h = lim_k2_95, lty = 2, col = 'darkgreen')
mtext(paste('RMSE =', round(sqrt(mean(SPE_2)), digits = 4)), side=1, line=3.5, at=9)
sqrt(mean(SPE_2))


a <- 9
Xhat <- pc_cor$x[,seq(1,a)] %*% t(pc_cor$rotation[,seq(1,a)])
RMSE(z, Xhat)
res <- z - Xhat
E2 <- res * res
# SPE_9 <- sqrt(apply(E2, 1, sum))
SPE_9 <- apply(E2, 1, sum)
# pca(z, ncomp = a, center = FALSE, scale = FALSE, alpha = 0.05)$Qlim[1]

v <- var(SPE_9)
m <- mean(SPE_9)
h <- (2*m^2)/v
g <- v / (2*m)
lim_k9_95 <- g*qchisq(.95, df=h) 
plot(SPE_9, ylab = paste('SPE after using', a, 'components'), xlab = 'index')
abline(h = lim_k9_95, lty = 2, col = 'darkgreen')
mtext(paste('RMSE =', round(sqrt(mean(SPE_9)), digits = 4)), side=1, line=3.5, at=9)
sqrt(mean(SPE_9))


par(mfrow=c(3,1))

plot(SPE_1, type = 'l', col = 'darkgreen')
abline(h = lim_k1_95, lty = 2, col = 'darkgreen')
plot(SPE_2, type = 'l', col = 'red')
abline(h = lim_k2_95, lty = 2, col = 'darkgreen')
plot(SPE_9, type = 'l', col = 'blue')
abline(h = lim_k9_95, lty = 2, col = 'darkgreen')

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
          axis.scales = FALSE, ellipsoid = FALSE)

# This gives a fairly complete picture of the major dimensions in the model: the explained variation on-the-plane, 
# given by t1 and t2, and the residual distance off-the-plane, summarized by SPE


######################################

# Various ways to compute Hotelling's T^2:
library("matlib")
num_comp <- 2

inverse_cov <- diag(eigen_cor$values[1:num_comp], nrow = num_comp, ncol = num_comp)^-1
inverse_cov[is.infinite(inverse_cov)] <- 0

tsquared <- diag(z %*% E[,1:num_comp] %*% inverse_cov %*% t(z %*% E[,1:num_comp]))

# which is equivalent to the Mahalanobis distance: (for k > 1)

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

par(mfrow=c(1,1))

plot(tsquared, type = 'l', ylim = c(0,tsquared_lim99*1.1))
abline(h=tsquared_lim99, col = 'red', lty = 2)
abline(h=tsquared_lim95, col = 'darkgreen', lty = 2)

# source: https://github.com/hredestig/pcaMethods/blob/master/R/pca.R

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

######################################

# Finally, combining residuals and scores (i.e. Hotelling's T^2)

# cumsum(pc_cor$sdev^2 / sum(pc_cor$sdev^2))[num_comp]

plot(tsquared, SPE_2, 
     xlab = paste("Hotelling's T^2 (", round(cumsum(pc_cor$sdev^2 / sum(pc_cor$sdev^2))[num_comp]*100, digits = 2), "%)"),
     ylab = paste("Q-residuals (", round((1-cumsum(pc_cor$sdev^2 / sum(pc_cor$sdev^2))[num_comp])*100, digits = 2), "%)"))
abline(h = lim_k2_95, lty = 2, col = 'darkgreen') # seven residual outliers
abline(v = tsquared_lim95, lty = 2, col = 'darkgreen') #  two score outliers

# Double check with adjOutlyingness

library("robustbase")
out <- adjOutlyingness(z, ndir=5000, clower=0, cupper=0)
hist(out$adjout)
rug(out$adjout)
z[!out$nonOut,] #  as we can confirm, the outlying cases differ strongly in their characteristics from their respective centers, that is
                # especially liveness and energy are inconsistent with the model
x[rownames(z[!out$nonOut,]),c("Track_Title", "Track_Artist", "Genre", "Track_Popularity")]

library("paran")

paran(z, centile=95, all=T, graph=T)

# Final result: use two components to describe music theoretical characteristics and extract the scores as new variables

x$pc_1 <- pc_cor$x[,1]
x$pc_2 <- pc_cor$x[,2]


library("xlsx")

write.xlsx(x,"C:\\Users\\wolfg\\Desktop\\Uni\\HU Master\\Datenanalyse II\\DA2_Homework\\pca_df.xlsx", row.names = TRUE)
