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

cluster_df <- import('https://docs.google.com/spreadsheets/d/1FFoxM7e7NXRCpw9mAVXYZiE1csYZB9HIwXnnVF9TxTY/export?format=csv&gid=1951789099')
cluster_df<- dplyr::select(cluster_df, c("cluster"))
x$cluster <- as.factor(cluster_df$cluster)

pca_df <- import('https://docs.google.com/spreadsheets/d/1i78eZ6tMqF7aV3JOvg4jZ7vJmkr7eejQ/export?format=csv&gid=734613762')
pca_df<- dplyr::select(pca_df, c("pc_1", "pc_2"))
pca_df$pc_1 <- as.numeric(sub(",", ".", pca_df$pc_1, fixed = TRUE))
pca_df$pc_2 <- as.numeric(sub(",", ".", pca_df$pc_2, fixed = TRUE))
x$pc_1 <- pca_df$pc_1
x$pc_2 <- pca_df$pc_2

fa_df <- import('https://docs.google.com/spreadsheets/d/13SFvna32wkP5-Q13FlM5O2HkVfdorAha/export?format=csv&gid=1394812963')
fa_df<- dplyr::select(fa_df, c("factor_1", "factor_2", "factor_3", "factor_4", "days_release"))
fa_df$factor_1 <- as.numeric(sub(",", ".", fa_df$factor_1, fixed = TRUE))
fa_df$factor_2 <- as.numeric(sub(",", ".", fa_df$factor_2, fixed = TRUE))
fa_df$factor_3 <- as.numeric(sub(",", ".", fa_df$factor_3, fixed = TRUE))
fa_df$factor_4 <- as.numeric(sub(",", ".", fa_df$factor_4, fixed = TRUE))
x$fa_1 <- fa_df$factor_1
x$fa_2 <- fa_df$factor_2
x$fa_3 <- fa_df$factor_3
x$fa_4 <- fa_df$factor_4
x$days_release <- fa_df$days_release

# fa_1: singles - one-hit wonders
# fa_2: "YouTube success" (ML_2)
# fa_3: "music supply" (ML_3)
# fa_4: appearances - cross-marketing

# pc_1 (between classic and Pop, Hip Hop, Techno): strong negative loadings: acousticness and instrumentalness, strong positive loadings: danceability, energy and loudness
# pc_2 (between Techno and Pop, Hip Hop): strong negative loadings: instrumentalness and tempo, strong positive loadings: speechiness and valence


x_scaled <- as.data.frame(scale(x[, c("fa_1", "fa_2", "fa_3", "fa_4", 
                                      "pc_1", "pc_2", "Artist_Follower", "Artist_Popularity", 
                                      "days_release", "Track_Popularity")]))

x_unscaled <- as.data.frame(x[, c("fa_1", "fa_2", "fa_3", "fa_4", 
                                       "pc_1", "pc_2", "Artist_Follower", "Artist_Popularity", 
                                       "days_release", "Track_Popularity")]) 

x_unscaled$Artist_Follower <- x_unscaled$Artist_Follower/1000000

par(mfrow=c(3,3))
nx <- c("fa_1", "fa_2", "fa_3", "fa_4", "pc_1", "pc_2", "Artist_Follower", "days_release", "Track_Popularity")
for (i in 1:length(nx)) {
  lmi <- lm(as.formula(paste('Artist_Popularity', nx[i], sep="~")), data=x_unscaled)
  summary(lmi)
  plot(x_unscaled[,nx[i]], x_unscaled[,'Artist_Popularity'], xlab=nx[i], ylab='Artist Popularity', main=sprintf("R^2=%.2f", summary(lmi)$r.squared))
  abline(lmi, col="red")
}

model1 <- lm(Artist_Popularity ~ fa_1 + fa_2 + fa_3 + fa_4 + pc_1 + pc_2 + Artist_Follower + days_release, data = x_unscaled)
summary(model1)
vif(model1) # fa_1, fa_3, fa_4, pc_1 mild collinearity
library("perturb") # condition index
colldiag(model1)

# Model without collinearity, appropriate to use standardized data / coefficients
# I remove some variables according to insignificance

model2 <- lm(Artist_Popularity ~ fa_2 + fa_3 + fa_4 + pc_2 + Artist_Follower, data = x_unscaled)
summary(model2)
vif(model2)
sum(model2$coefficients^2)
library("perturb") # condition index
colldiag(model2)
# No signs for multicollinearity, condition index corresponding to the smallest eigenvalue is 1.644

model2_z <- lm(Artist_Popularity ~ fa_2 + fa_3 + fa_4 + pc_2 + Artist_Follower, data = x_scaled)
sort(round((model2_z$coefficients)^2, digits = 4), decreasing = T)

library("boot")
lmboot <- Boot(model2)
summary(lmboot)
ci <- confint(model2, level = 0.95)
hist(lmboot)

plot.confint <- function (b, b.boot, ci, main) {
  hist(b.boot, main=main); rug(b.boot)
  abline(v=b, col="red"); abline(v=ci, col="blue")
  abline(v=quantile(b.boot, c(0.025, 0.975)), col="green")
}

betahat <- boot(model2$model, R=999, 
                function(x, index) { lm(Artist_Popularity ~ fa_2 + fa_3 + fa_4 + pc_2 + Artist_Follower, data = x_unscaled, subset=index)$coefficients })

par(mfcol=c(2,3))

plot.confint(model2$coefficients[1], betahat$t[,1], ci[1,], "Intercept")
plot.confint(model2$coefficients[2], betahat$t[,2], ci[2,], "fa_2")
plot.confint(model2$coefficients[3], betahat$t[,3], ci[3,], "fa_3")
plot.confint(model2$coefficients[4], betahat$t[,4], ci[4,], "fa_4")
plot.confint(model2$coefficients[5], betahat$t[,5], ci[5,], "pc_2")
plot.confint(model2$coefficients[6], betahat$t[,6], ci[6,], "Artist_Follower")

# some comments on R^2...




# Residuals normality

ks.test(model1$residuals, "pnorm", mean = mean(model1$residuals), sd=sd(model1$residuals))$statistic
1.3581 / sqrt(length(model1$residuals))

# KS-Test: Test statistic D = 0.06728437 < 0.09700714 = critical value, Null is not rejected and conclude that residuals
# are approximately normally distributed

ks.test(model2$residuals, "pnorm", mean = mean(model2$residuals), sd=sd(model2$residuals))$statistic
1.3581 / sqrt(length(model2$residuals))

par(mfcol=c(2,2))
plot(model2)
# standardized residuals plot and studentized residuals plot
#par(mfcol=c(1,2))
#plot(rstandard(model2), main = 'Standardized residuals')
#plot(rstudent(model2), main = 'Studentized residuals')

par(mfcol=c(2,3))
nx <- names(model2$model)
for (i in 2:length(model2$model)) {
  plot(model2$model[,i], residuals(model2), xlab=nx[i])
  lines(lowess(model2$model[,i], residuals(model2)), col = "red")
  abline(h = 0, col = "black", lty = 2)
}

plot(model2$fitted.values, residuals(model2), main = 'Residuals vs. fitted values')
lines(lowess(model2$fitted.values, residuals(model2)), col = "red")
abline(h = 0, col = "black", lty = 2)

# presence of outliers

par(mfcol=c(2,3))
nx <- names(model2$model)
for (i in 2:length(model2$model)) {
  plot(model2$model[,i], rstandard(model2), xlab=nx[i])
  lines(lowess(model2$model[,i], rstandard(model2)), col = "red")
  # wenn Linie von 0 abweicht: Residuen sind biased, sprich deren Mittelwert nicht 0
  abline(h = 0, col = "black", lty = 2)
}

# standardized residuals vs. fitted values

plot(model2$fitted.values, rstandard(model2), main = 'Standardized residuals vs. fitted values')
lines(lowess(model2$fitted.values, rstandard(model2)), col = "red")
abline(h = 0, col = "black", lty = 2)


# variables without and with quadratic terms (fa_2, pc_2, Artist_Follower)

par(mfcol=c(2,3))

lm_fa2 <- lm(Artist_Popularity ~ fa_2, data=x_unscaled)
plot(x_unscaled[,'fa_2'], x_unscaled[,'Artist_Popularity'], xlab='fa_2', main=sprintf("R^2=%.2f", summary(lm_fa2)$r.squared),
     ylim = c(min(min(fitted(lm_fa2)), min(x_unscaled$Artist_Popularity)), max(max(fitted(lm_fa2)), max(x_unscaled$Artist_Popularity))))
abline(lm_fa2, col="red")

lmq_fa2 <- lm(Artist_Popularity ~ poly(fa_2, 2), data=x_unscaled)
o <- order(x_unscaled$fa_2)
plot(x_unscaled[,'fa_2'], x_unscaled[,'Artist_Popularity'], xlab= 'fa_2 + fa_2^2', main=sprintf("R^2=%.2f", summary(lmq_fa2)$r.squared),
     ylim = c(min(min(fitted(lmq_fa2)), min(x_unscaled$Artist_Popularity)), max(max(fitted(lmq_fa2)), max(x_unscaled$Artist_Popularity))))
lines(x_unscaled$fa_2[o], fitted(lmq_fa2)[o], col="red")

lm_follower <- lm(Artist_Popularity ~ Artist_Follower, data=x_unscaled)
plot(x_unscaled[,'Artist_Follower'], x_unscaled[,'Artist_Popularity'], xlab='Artist_Follower', main=sprintf("R^2=%.2f", summary(lm_follower)$r.squared),
     ylim = c(min(min(fitted(lm_follower)), min(x_unscaled$Artist_Popularity)), max(max(fitted(lm_follower)), max(x_unscaled$Artist_Popularity))))
abline(lm_follower, col="red")

lmq_follower <- lm(Artist_Popularity ~ poly(Artist_Follower, 2), data=x_unscaled)
o <- order(x_unscaled$Artist_Follower)
plot(x_unscaled[,'Artist_Follower'], x_unscaled[,'Artist_Popularity'], xlab= 'Artist_Follower + Artist_Follower^2', main=sprintf("R^2=%.2f", summary(lmq_follower)$r.squared),
     ylim = c(min(min(fitted(lmq_follower)), min(x_unscaled$Artist_Popularity)), max(max(fitted(lmq_follower)), max(x_unscaled$Artist_Popularity))))
lines(x_unscaled$Artist_Follower[o], fitted(lmq_follower)[o], col="red")

lm_pc2 <- lm(Artist_Popularity ~ pc_2, data=x_unscaled)
plot(x_unscaled[,'pc_2'], x_unscaled[,'Artist_Popularity'], xlab='pc_2', main=sprintf("R^2=%.2f", summary(lm_pc2)$r.squared),
     ylim = c(min(min(fitted(lm_pc2)), min(x_unscaled$Artist_Popularity)), max(max(fitted(lm_pc2)), max(x_unscaled$Artist_Popularity))))
abline(lm_pc2, col="red")

lmq_pc2 <- lm(Artist_Popularity ~ poly(pc_2, 2), data=x_unscaled)
o <- order(x_unscaled$pc_2)
plot(x_unscaled[,'pc_2'], x_unscaled[,'Artist_Popularity'], xlab= 'pc_2 + pc_2^2', main=sprintf("R^2=%.2f", summary(lmq_pc2)$r.squared),
     ylim = c(min(min(fitted(lmq_pc2)), min(x_unscaled$Artist_Popularity)), max(max(fitted(lmq_pc2)), max(x_unscaled$Artist_Popularity))))
lines(x_unscaled$pc_2[o], fitted(lmq_pc2)[o], col="red")


# Breusch-Pagan Test + Durbin-Watson Test / spherical errors (i.e. iid)
# model2 uses unscaled data
auxiliary_reg <- lm(model2$residuals^2 ~ fa_2 + fa_3 + fa_4 + pc_2 + Artist_Follower, data = x_unscaled)
summary(auxiliary_reg)
# R^2: 0.1071
# N = 196, N = nrow(x_unscaled) 
# test stat: 0.1071 * 196 = 20.9916
p <- ncol(model2$model)-1
qchisq(.95, df = p) # df = number of parameters p (without the constant!), acc. to Klinke: q = 9*(9+3)/2 WTF?
# p_value = P(z > 11.0705) = 1-pchisq(20.9916,, p=5 ) =0.00081300742 < 0.05 ,  reject H0 of homoskedasticity and conclude there is heteroskedasticity
1-pchisq(20.9916, p)

library("lmtest")
bptest(model2)
# test statistic n*R^2_aux > X^2_crit = qchisq(.95, df = 9), hence reject H0 and conclude that there's
# heteroskedasticity and constancy of error variance does not hold

dwtest(model2)
# Durbin Watson test confirms that the error variance-covariance is diagonal, i.e. var(eps | X) =  \Omega * I
# fail to reject H0: rho = 0 (alternative hypothesis is that there is autocorrelation in the errors)

# outlier detection / leverage

par(mfcol=c(1,2))
plot(hatvalues(model2), pch=19, main="Leverage", cex=0.5)
n <- nrow(model2$model)
p <- ncol(model2$model)-1
abline(h=(1:3)*(p+1)/n, col=c("black", "darkred", "red"))

plot(cooks.distance(model2), pch=19, main="Cook's distances",
     cex=0.5)
n <- nrow(model2$model)
p <- ncol(model2$model)-1
abline(h=4/n, col="red")

# Rule of thumb leverage
outlier_index_leverage <- as.numeric(rownames(x_unscaled[hatvalues(model2) > 3*(p+1)/n, ]))
length(outlier_index_leverage)
# There are 8 outliers that might be influential on the regression
x[outlier_index_leverage, c("Track_Title","Track_Artist", "Artist_Popularity", "Artist_Follower", "viewsCount", "pc_1",
                            "pc_2")]

outlier_index_final <- outlier_index_leverage

# Rule of thumb Cook's distance
# movement of regression coefficients all together if ith observation is excluded
outlier_index_cooksd <- as.numeric(rownames(x_unscaled[cooks.distance(model2) > 3*(p+1)/n, ]))
length(outlier_index_cooksd)
x[outlier_index_cooksd, c("Track_Title","Track_Artist", "Artist_Popularity", "Artist_Follower", "viewsCount", "pc_1",
                            "pc_2")]


library('generics')
library('broom')
model.diag.metrics <- augment(model2)

# differences

# SDBETA

SDBETA <- dfbetas(model2)
n <- nrow(model2$model)
par(mfcol=c(2,3))
plot(SDBETA[, 'fa_2'], main="fa_2", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")
plot(SDBETA[, 'fa_3'], main="fa_3", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")
plot(SDBETA[, 'fa_4'], main="fa_4", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")
plot(SDBETA[, 'pc_2'], main="pc_2", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")
plot(SDBETA[, 'Artist_Follower'], main="Artist_Follower", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")

# SDFITS
par(mfcol=c(1,1))
SDFITS <- dffits(model2)
plot(SDFITS, pch=19)
abline(h=c(-1,1), col="red")
n <- nrow(model2$model)
p <- ncol(model2$model)-1
abline(h=c(-2,2)*sqrt(p/n), col="darkred")

# As this is a small data set, it is sufficient to analyze |\delta*y_i| > 1, in this case two observations are identified
# of exceeding this limit. They are

outlier_index_sdfits <- as.numeric(names(which(abs(SDFITS) > 1))) 

x[outlier_index_sdfits, c("Track_Title","Track_Artist", "Artist_Popularity", "Artist_Follower", "viewsCount", "pc_1",
                          "pc_2")]

# coincides with the outliers identified by Cook's distance

x_noout <- x_unscaled[-outlier_index_leverage, ]
x_noout$fa_2sq <- x_noout$fa_2^2
x_noout$pc_2sq <- x_noout$pc_2^2
x_noout$Artist_Followersq <- x_noout$Artist_Follower^2

# Model changes

model3 <- lm(Artist_Popularity ~ fa_2 + fa_2sq + fa_3 + fa_4 + pc_2 + pc_2sq + Artist_Follower + Artist_Followersq, data = x_noout)
summary(model3)
library("car")
vif(model3) # Obviously, now multicollinearity is severe and standardized coefficients wouldn't give a plausible interpretation


# Since multicollinearity is severe, bootstrapping confidence regions is sensible.
library("boot")
lmboot3 <- Boot(model3)
summary(lmboot3)
ci <- confint(model3, level = 0.95)
hist(lmboot3)


plot.confint <- function (b, b.boot, ci, main) {
  hist(b.boot, main=main); rug(b.boot)
  abline(v=b, col="red"); abline(v=ci, col="blue")
  abline(v=quantile(b.boot, c(0.025, 0.975)), col="green")
}

betahat <- boot(model3$model, R=999, 
                function(x, index) { lm(Artist_Popularity ~ fa_2 + fa_2sq + fa_3 + fa_4 + pc_2 + pc_2sq + Artist_Follower + Artist_Followersq, data = x_noout, subset=index)$coefficients })

par(mfcol=c(3,3))

plot.confint(model3$coefficients[1], betahat$t[,1], ci[1,], "Intercept")
plot.confint(model3$coefficients[2], betahat$t[,2], ci[2,], "fa_2")
plot.confint(model3$coefficients[3], betahat$t[,3], ci[3,], "fa_2sq")
plot.confint(model3$coefficients[4], betahat$t[,4], ci[4,], "fa_3")
plot.confint(model3$coefficients[5], betahat$t[,5], ci[5,], "fa_4")
plot.confint(model3$coefficients[6], betahat$t[,6], ci[6,], "pc_2")
plot.confint(model3$coefficients[7], betahat$t[,7], ci[7,], "pc_2sq")
plot.confint(model3$coefficients[8], betahat$t[,8], ci[8,], "Artist_Follower")
plot.confint(model3$coefficients[9], betahat$t[,9], ci[9,], "Artist_Followersq")

# Seems quite robust despite multicollinearity.

# Residuals normality

ks.test(model3$residuals, "pnorm", mean = mean(model3$residuals), sd=sd(model3$residuals))$statistic
1.3581 / sqrt(length(model3$residuals))

# residuals also approx. normally distributed

par(mfcol=c(2,2))
plot(model3)

par(mfcol=c(3,3))
nx <- names(model3$model)
for (i in 2:length(model3$model)) {
  plot(model3$model[,i], residuals(model3), xlab=nx[i])
  lines(lowess(model3$model[,i], residuals(model3)), col = "red")
  abline(h = 0, col = "black", lty = 2)
}

plot(model3$fitted.values, residuals(model3), main = 'Residuals vs. fitted values')
lines(lowess(model3$fitted.values, residuals(model3)), col = "red")
abline(h = 0, col = "black", lty = 2)

# presence of outliers

par(mfcol=c(3,3))
nx <- names(model3$model)
for (i in 2:length(model3$model)) {
  plot(model3$model[,i], rstandard(model3), xlab=nx[i])
  lines(lowess(model3$model[,i], rstandard(model3)), col = "red")
  abline(h = 0, col = "black", lty = 2)
}

# standardized residuals vs. fitted values

plot(model3$fitted.values, rstandard(model3), main = 'Standardized residuals vs. fitted values')
lines(lowess(model3$fitted.values, rstandard(model3)), col = "red")
abline(h = 0, col = "black", lty = 2)

# Breusch-Pagan Test + Durbin-Watson Test / spherical errors (i.e. iid)
# model3 uses unscaled data
auxiliary_reg <- lm(model3$residuals^2 ~ fa_2 + fa_2sq + fa_3 + fa_4 + pc_2 + pc_2sq + Artist_Follower + Artist_Followersq, data = x_noout)
summary(auxiliary_reg)
# R^2: 0.07354
# N = 188, N = nrow(x_noout) 
# test stat: 0.07354 * 188 = 13.82552,
p <- ncol(model3$model)-1
qchisq(.95, df = p) # df = number of parameters p (without the constant!), acc. to Klinke: q = 9*(9+3)/2 WTF?
# p_value = P(z > 15.50731) = 1-pchisq(13.82552,, p=8 )= 0.086428072> 0.05 , fail to reject H0 of homoskedasticity
1-pchisq(13.82552, p)

library("lmtest")
bptest(model3)
# test statistic n*R^2_aux < X^2_crit = qchisq(.95, df = 8), hence fail to reject H0 and conclude that there's no
# heteroskedasticity and constancy of error variance does hold

dwtest(model3)
# Durbin Watson test confirms that the error variance-covariance is diagonal, i.e. var(eps | X) =  \Omega * I
# fail to reject H0: rho = 0

# outlier detection / leverage

par(mfcol=c(1,2))
plot(hatvalues(model3), pch=19, main="Leverage", cex=0.5)
n <- nrow(model3$model)
p <- ncol(model3$model)-1
abline(h=(1:3)*(p+1)/n, col=c("black", "darkred", "red"))

plot(cooks.distance(model3), pch=19, main="Cook's distances",
     cex=0.5)
n <- nrow(model3$model)
p <- ncol(model3$model)-1
abline(h=4/n, col="red")

# Rule of thumb leverage
outlier_index_leverage <- as.numeric(rownames(x_unscaled[hatvalues(model3) > 3*(p+1)/n, ]))
length(outlier_index_leverage)
# There are 6 outliers that might be influential on the regression
x[outlier_index_leverage, c("Track_Title","Track_Artist", "Artist_Popularity", "Artist_Follower", "viewsCount", "pc_1",
                            "pc_2")]

# Rule of thumb Cook's distance
# movement of regression coefficients all together if ith observation is excluded
outlier_index_cooksd <- as.numeric(rownames(x_unscaled[cooks.distance(model3) > 3*(p+1)/n, ]))
length(outlier_index_cooksd)
x[outlier_index_cooksd, c("Track_Title","Track_Artist", "Artist_Popularity", "Artist_Follower", "viewsCount", "pc_1",
                          "pc_2")]
library('generics')
library('broom')
model.diag.metrics <- augment(model3)

# differences

# SDBETA

SDBETA <- dfbetas(model3)
n <- nrow(model3$model)
par(mfcol=c(2,4))
plot(SDBETA[, 'fa_2'], main="fa_2", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")
plot(SDBETA[, 'fa_2sq'], main="fa_2sq", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")
plot(SDBETA[, 'fa_3'], main="fa_3", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")
plot(SDBETA[, 'fa_4'], main="fa_4", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")
plot(SDBETA[, 'pc_2'], main="pc_2", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")
plot(SDBETA[, 'pc_2sq'], main="pc_2sq", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")
plot(SDBETA[, 'Artist_Follower'], main="Artist_Follower", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")
plot(SDBETA[, 'Artist_Followersq'], main="Artist_Followersq", pch=19)
abline(h=c(-2,2)/sqrt(n), col="red")

# SDFITS
par(mfcol=c(1,1))
SDFITS <- dffits(model3)
plot(SDFITS, pch=19)
abline(h=c(-1,1), col="red")
n <- nrow(model3$model)
p <- ncol(model3$model)-1
abline(h=c(-2,2)*sqrt(p/n), col="darkred")

# As this is a small data set, it is sufficient to analyze |\delta*y_i| > 1, in this case two observations are identified
# of exceeding this limit. They are

outlier_index_sdfits <- as.numeric(names(which(abs(SDFITS) > 1)))

x[outlier_index_sdfits, c("Track_Title","Track_Artist", "Artist_Popularity", "Artist_Follower", "viewsCount", "pc_1",
                          "pc_2")]

# Cooks distance

# biggest effect on overall regression if observation 29 excluded, effect here means the sum of squares between the
# fitted values from regression excluding observation i and the fitted values from full regression, normalized by
# (n-p) times the (estimated) variance of the residuals from the full regression



# model selection / stepwise models

# drop1(model3_noout, test="F") #  result: no variable can be deleted since for every variable removed/added the change
# in overall model fit (partial F-test) is significant



lms_add <- lm(Artist_Popularity ~ fa_2 + fa_2sq + fa_3 + fa_4 + pc_2 + pc_2sq + Artist_Follower + Artist_Followersq 
              + x[-outlier_index_final, 'fa_1'] + x[-outlier_index_final, 'Track_Duration_ms'] + + x[-outlier_index_final, 'days_release']
              + x[-outlier_index_final, 'Track_Popularity'], data = x_noout)

drop1(lms_add, test="F")
summary(lms_add)

add1(model3, ~. + x[-outlier_index_final, 'fa_1'] + x[-outlier_index_final, 'Track_Duration_ms'] + x[-outlier_index_final, 'days_release']
     + x[-outlier_index_final, 'Track_Popularity'], test = "F") # result: fa_1 must not be included but Track_Duration_ms, days_release  and Track_Popularity should be included!

# baseline model:
# forward

lmi <- lm(Artist_Popularity~1, data=model3$model)
lmf <- MASS::stepAIC(lmi, scope=~
                fa_2 
               + fa_2sq
               + fa_3 
               + fa_4 
               + pc_2
               + pc_2sq
               + Artist_Follower
               + Artist_Followersq
               + x[-outlier_index_final, 'fa_1']
               + x[-outlier_index_final, 'Track_Duration_ms']
               + x[-outlier_index_final, 'days_release']
               + x[-outlier_index_final, 'Track_Popularity']
               ,direction = "forward")

summary(lmf)

# backward:
lma <- lm(Artist_Popularity ~ fa_2 + fa_2sq + fa_3 + fa_4 + pc_2 + pc_2sq  + Artist_Follower 
          + Artist_Followersq + x[-outlier_index_final, 'fa_1'] + x[-outlier_index_final, 'Track_Duration_ms'] 
          + x[-outlier_index_final, 'days_release'] + x[-outlier_index_final, 'Track_Popularity'], 
          data= x_noout)

lmb <- MASS::stepAIC(lma)

summary(lmb)



# automatic backward, using Bayesian information criterion (true model rather than predictive model)
fm_bic <- step(lms_add, k = log(n)) # keeps only variables with significant coefficients
summary(fm_bic)
fm_aic <- step(lms_add, k = 2)
summary(fm_aic)

# both penalty methods yield the same result

# library("mlr")
# cluster_dummies <- mlr::createDummyFeatures(x[, 'cluster'], target = "1") # clusters relative to base category = classic

# x$clus_classic <- cluster_dummies$`1`
# x$clus_hiphop <- cluster_dummies$`2`
# x$clus_pop <- cluster_dummies$`3`
# x$clus_techno <- cluster_dummies$`4`

# noout_clus <- cluster_dummies[-outlier_index,]


x_noout$Track_Duration_ms <- x[-outlier_index_final, 'Track_Duration_ms']

lmfull <- lm(Artist_Popularity~., data=x_noout)
summary(lmfull)
lmback <- step(lmfull)
summary(lmback) # keeps days_release, although not significant

############### LASSO

library("glmnet")
xl <- as.matrix(x_noout[,-8])
yl <- x_noout[,8]
set.seed(42)
lmlasso <- cv.glmnet(xl, yl, alpha =1) # cross-validation finds the value of lambda which minimizes MSE, alpha = 1 for LASSO
plot(lmlasso) # displays the bias-variance trade-off (MSE = variance + bias^2)
best.lambda <- lmlasso$lambda.min
lmlasso$lambda.1se

cl <- list(coef(lmlasso, s="lambda.min")[,1], coef(lmlasso, s="lambda.1se")[,1], coef(lmback), coef(lmfull))
cf <- matrix(NA, nrow=dim(xl)[2]+1, ncol=length(cl))
row.names(cf) <- names(lmfull$coefficients)
i <- 1
for (ci in cl) {
  cf[names(ci),i] <- ci[names(ci)]
  i <- i+1
}
colnames(cf) <- c("min", "1se", "back", "full")
cf[cf==0] <- NA
cf


# variable importance
glmmod <- glmnet(xl, yl, lambda = best.lambda)
coefs = coef(glmmod)[,1]
coefs = sort(abs(coefs), decreasing = F)
coefs
coef(lmlasso, s="lambda.1se")


######################
# Performance metrics - full data

RMSE(predict(lmfull, newdata = x_noout), x_noout$Artist_Popularity)
R2(predict(lmfull, newdata = x_noout), x_noout$Artist_Popularity)

# x_model <- model.matrix(Artist_Popularity~., x_noout)

library("glmnet")
"Ridge regression"
set.seed(42)
best.lambda <- cv.glmnet(x = as.matrix(x_noout[,-8]), y = as.matrix(x_noout[,8]), alpha = 0)$lambda.min
ridge.cv <- glmnet(x = as.matrix(x_noout[,-8]), y = as.matrix(x_noout[,8]), alpha = 0, lambda = best.lambda)
# as.numeric(coef(ridge.cv)[,1]) %*% as.numeric(x_model[1,]) equivalent
# predict(ridge.cv, newx = as.matrix(x_noout[1,-8])) equivalent
RMSE(predict(ridge.cv, newx = as.matrix(x_noout[,-8])), as.matrix(x_noout[,8]))
R2(predict(ridge.cv, newx = as.matrix(x_noout[,-8])), as.matrix(x_noout[,8]))

"LASSO regression"
set.seed(42)
best.lambda <- cv.glmnet(x = as.matrix(x_noout[,-8]), y = as.matrix(x_noout[,8]), alpha = 1)$lambda.min
lasso.cv <- glmnet(x = as.matrix(x_noout[,-8]), y = as.matrix(x_noout[,8]), alpha = 1, lambda = best.lambda)
RMSE(predict(lasso.cv, newx = as.matrix(x_noout[,-8])), as.matrix(x_noout[,8]))
R2(predict(lasso.cv, newx = as.matrix(x_noout[,-8])), as.matrix(x_noout[,8]))

"Elasticnet regression"
set.seed(42)
best.lambda <- cv.glmnet(x = as.matrix(x_noout[,-8]), y = as.matrix(x_noout[,8]), alpha = 0.5)$lambda.min
elasticnet.cv <- glmnet(x = as.matrix(x_noout[,-8]), y = as.matrix(x_noout[,8]), alpha = 0.5, lambda = best.lambda)
RMSE(predict(elasticnet.cv, newx = as.matrix(x_noout[,-8])), as.matrix(x_noout[,8]))
R2(predict(elasticnet.cv, newx = as.matrix(x_noout[,-8])), as.matrix(x_noout[,8]))

x_full <- model.matrix(Artist_Popularity~., x_noout)[,-1]

ridge_res2 <- as.numeric(x_noout$Artist_Popularity - predict(ridge.cv, x_full))^2
lasso_res2 <- as.numeric(x_noout$Artist_Popularity - predict(lasso.cv, x_full))^2
elasticnet_res2 <- as.numeric(x_noout$Artist_Popularity - predict(elasticnet.cv, x_full))^2

par(mfcol=c(2,2))
barplot((x_noout$Artist_Popularity - predict(lmfull, x_noout))^2, labels = FALSE)
Axis(side=2)
title(main = paste("LM (full sample): RMSE =",round(RMSE(predict(lmfull, x_noout), x_noout$Artist_Popularity), digits = 2), ", R^2:", round(R2(predict(lmfull, x_noout), x_noout$Artist_Popularity), digits = 2)))
barplot(ridge_res2, main = paste("Ridge (full sample): RMSE =",round(RMSE(predict(ridge.cv, x_full), x_noout$Artist_Popularity), digits = 2), ", R^2:", round(R2(predict(ridge.cv, x_full), x_noout$Artist_Popularity), digits = 2)))
barplot(lasso_res2, main = paste("LASSO (full sample): RMSE =",round(RMSE(predict(lasso.cv, x_full), x_noout$Artist_Popularity), digits = 2), ", R^2:", round(R2(predict(lasso.cv, x_full), x_noout$Artist_Popularity), digits = 2)))
barplot(elasticnet_res2, main = paste("Elasticnet (full sample): RMSE =",round(RMSE(predict(elasticnet.cv, x_full), x_noout$Artist_Popularity), digits = 2), ", R^2:", round(R2(predict(elasticnet.cv, x_full), x_noout$Artist_Popularity), digits = 2)))


library("caret")

# Splitting in train and test data

set.seed(42)
idx.train <- createDataPartition(y = x_noout$Artist_Popularity, p = 0.75, list = FALSE)
train <- x_noout[idx.train, ]
test <- x_noout[-idx.train,]

lm_train <- lm(Artist_Popularity~., data=train)
RMSE(predict(lm_train, newdata = train), train$Artist_Popularity)
R2(predict(lm_train, newdata = train), train$Artist_Popularity)

RMSE(predict(lm_train, newdata = test), test$Artist_Popularity)
R2(predict(lm_train, newdata = test), test$Artist_Popularity)

"Ridge regression"
set.seed(42)
best.lambda <- cv.glmnet(x = as.matrix(train[,-8]), y = as.matrix(train[,8]), alpha = 0)$lambda.min
ridge.cv <- glmnet(x = as.matrix(train[,-8]), y = as.matrix(train[,8]), alpha = 0, lambda = best.lambda)
# as.numeric(coef(ridge.cv)[,1]) %*% as.numeric(x_model[1,]) equivalent
# predict(ridge.cv, newx = as.matrix(x_noout[1,-8])) equivalent
RMSE(predict(ridge.cv, newx = as.matrix(train[,-8])), as.matrix(train[,8]))
R2(predict(ridge.cv, newx = as.matrix(train[,-8])), as.matrix(train[,8]))

RMSE(predict(ridge.cv, newx = as.matrix(test[,-8])), as.matrix(test[,8]))
R2(predict(ridge.cv, newx = as.matrix(test[,-8])), as.matrix(test[,8]))


"LASSO regression"
set.seed(42)
best.lambda <- cv.glmnet(x = as.matrix(train[,-8]), y = as.matrix(train[,8]), alpha = 1)$lambda.min
lasso.cv <- glmnet(x = as.matrix(train[,-8]), y = as.matrix(train[,8]), alpha = 1, lambda = best.lambda)
RMSE(predict(lasso.cv, newx = as.matrix(train[,-8])), as.matrix(train[,8]))
R2(predict(lasso.cv, newx = as.matrix(train[,-8])), as.matrix(train[,8]))
RMSE(predict(lasso.cv, newx = as.matrix(test[,-8])), as.matrix(test[,8]))
R2(predict(lasso.cv, newx = as.matrix(test[,-8])), as.matrix(test[,8]))


"Elasticnet regression"
set.seed(42)
best.lambda <- cv.glmnet(x = as.matrix(train[,-8]), y = as.matrix(train[,8]), alpha = 0.5)$lambda.min
elasticnet.cv <- glmnet(x = as.matrix(train[,-8]), y = as.matrix(train[,8]), alpha = 0.5, lambda = best.lambda)
RMSE(predict(elasticnet.cv, newx = as.matrix(train[,-8])), as.matrix(train[,8]))
R2(predict(elasticnet.cv, newx = as.matrix(train[,-8])), as.matrix(train[,8]))
RMSE(predict(elasticnet.cv, newx = as.matrix(test[,-8])), as.matrix(test[,8]))
R2(predict(elasticnet.cv, newx = as.matrix(test[,-8])), as.matrix(test[,8]))

# Graphics

# Training data

x_train <- model.matrix(Artist_Popularity~., train)[,-1]

ridge_res2 <- as.numeric(train$Artist_Popularity - predict(ridge.cv, x_train))^2
lasso_res2 <- as.numeric(train$Artist_Popularity - predict(lasso.cv, x_train))^2
elasticnet_res2 <- as.numeric(train$Artist_Popularity - predict(elasticnet.cv, x_train))^2

par(mfcol=c(2,2))
barplot((train$Artist_Popularity - predict(lm_train, train))^2, main = paste("LM (train): RMSE =",round(RMSE(predict(lm_train, train), train$Artist_Popularity), digits = 2), ", R^2:", round(R2(predict(lm_train, train), train$Artist_Popularity), digits = 2)))
barplot(ridge_res2, main = paste("Ridge (train): RMSE =",round(RMSE(predict(ridge.cv, x_train), train$Artist_Popularity), digits = 2), ", R^2:", round(R2(predict(ridge.cv, x_train), train$Artist_Popularity), digits = 2)))
barplot(lasso_res2, main = paste("LASSO (train): RMSE =",round(RMSE(predict(lasso.cv, x_train), train$Artist_Popularity), digits = 2), ", R^2:", round(R2(predict(lasso.cv, x_train), train$Artist_Popularity), digits = 2)))
barplot(elasticnet_res2, main = paste("Elasticnet (train): RMSE =",round(RMSE(predict(elasticnet.cv, x_train), train$Artist_Popularity), digits = 2), ", R^2:", round(R2(predict(elasticnet.cv, x_train), train$Artist_Popularity), digits = 2)))

# Test data

x_test <- model.matrix(Artist_Popularity~., test)[,-1]

ridge_res2 <- as.numeric(test$Artist_Popularity - predict(ridge.cv, x_test))^2
lasso_res2 <- as.numeric(test$Artist_Popularity - predict(lasso.cv, x_test))^2
elasticnet_res2 <- as.numeric(test$Artist_Popularity - predict(elasticnet.cv, x_test))^2

par(mfcol=c(2,2))
barplot((test$Artist_Popularity - predict(lm_train, test))^2, main = paste("LM (test): RMSE =",round(RMSE(predict(lm_train, test), test$Artist_Popularity), digits = 2), ", R^2:", round(R2(predict(lm_train, test), test$Artist_Popularity), digits = 2)))
barplot(ridge_res2, main = paste("Ridge (test): RMSE =",round(RMSE(predict(ridge.cv, x_test), test$Artist_Popularity), digits = 2), ", R^2:", round(R2(predict(ridge.cv, x_test), test$Artist_Popularity), digits = 2)))
barplot(lasso_res2, main = paste("LASSO (test): RMSE =",round(RMSE(predict(lasso.cv, x_test), test$Artist_Popularity), digits = 2), ", R^2:", round(R2(predict(lasso.cv, x_test), test$Artist_Popularity), digits = 2)))
barplot(elasticnet_res2, main = paste("Elasticnet (test): RMSE =",round(RMSE(predict(elasticnet.cv, x_test), test$Artist_Popularity), digits = 2), ", R^2:", round(R2(predict(elasticnet.cv, x_test), test$Artist_Popularity), digits = 2)))


###############################################################################
# Interpretation of linear model 

# Final linear model (adopting the result from elasticnet, same remaining coefficients from CV and training on full data set):
# decision: remove fa_1 and fa_4, yields R^2 of 0.7787 on full data set

# final linear model: Artist_Popularity ~ fa_2 + pc_2 + Artist_Follower + fa_2sq + pc_2sq + Artist_Followersq

lmfinal <- lm(Artist_Popularity~.-fa_1-fa_4-days_release-Track_Duration_ms-Track_Popularity-pc_1-fa_3, data=x_noout, x=TRUE)
summary(lmfinal)

# interpretation of standardized regression coefficients:
x_noout_z <- as.data.frame(scale(x_noout))
x_noout_z$Artist_Follower <- x_noout$Artist_Follower # Note that Artist_Follower remains in units as it's not sensible to center it,
# given its highly right-skewed distribution
x_noout_z$Artist_Followersq <- x_noout$Artist_Followersq
lmfinal_z <- lm(Artist_Popularity~.-fa_1-fa_4-days_release-Track_Duration_ms-Track_Popularity-pc_1-fa_3, data=x_noout_z, x=TRUE)
summary(lmfinal_z)

# if fa_2 increases by 1 standard deviation (i.e. 0.39 units) then Artist_Popularity increases by 0.34 standard deviations
# (i.e. sd(x_noout[, 'Artist_Popularity'])*0.34 = 5.2 units) given the level of fa_2 = 0 which corresponds
# to an observation which is exactly at the mean value of fa_2 (0 in standardized case, or -0.08139358 in unstandardized
# case)

# (diminishing) marginal effect of fa_2: there's no simple linear relationship between a Youtube video's popularity and
# its artist's popularity on Spotify as Spotify track and user pool are more diverse than some of the most watched
# videos of all time on Youtube and do not only consist of these extreme cases

# I can manually evaluate the marginal effect at every given observation for fa_2 and compute the mean from this

mean(lmfinal_z$coefficients[2]+2*lmfinal_z$coefficients[5]*x_noout_z[,'fa_2'])

# 1) vertex: break-even point y = ax2 + bx + c is at x = -b/2a. Check that vertex is inside the data range of x

# 2) quadratic term coefficient: the rate of change of the coefficient of X2, per unit change in X2. The quadratic specification appears 
# to be an improvement as it is is more satisfactory than the linear term for low values of Artist_Follower, in that it does not yield 
# implausibly low predicted values of Artist_Popularity.
# for this model – for each 1-unit increase in anxiety, the y-x slope decreases by quad_coeff (2*.065). 

# average partial effect is 0.3389366, so when fa_2 increases by one standard deviation (or 0.389649 units), then the overall,
# average partial effect on Artist Popularity is that Artist Popularity increases by 0.3389366 standard deviations (or 5.18 units).

# break-even partial effect: partial effect is exactly offset iff 0.3389 + 2*(-0.2276)*fa_2 == 0. Solving for fa_2 gives
# ((lmfinal_z$coefficients[2]) / (2*lmfinal_z$coefficients[5]))*-1 = 0.7445204, so if fa_2 is equal to 0.7445204 the overall
# partial effect will be 0. All values below that yield a still positive overall marginal effect, all values above that
# yield an overall negative effect and then extreme YouTube popularity is associated with lower Artist popularity on Spotify.
fa_2_be <- ((lmfinal_z$coefficients[2]) / (2*lmfinal_z$coefficients[5]))*-1
fa_2_diminish_index <- as.numeric(rownames(x_noout_z[which(x = x_noout_z[, 'fa_2'] > fa_2_be),]))
x[fa_2_diminish_index, c("Track_Title", "Track_Artist", "viewsCount")]


# if Artist_Follower (in millions) increases by one unit (i.e. + 1 million followers), then Artist_Popularity increases by 
# 0.25 standard deviations (i.e. sd(x_noout[, 'Artist_Popularity'])*0.25 = 3.82 units) given the level of
# Artist_Follower is equal to zero. Since this realization doesn't exist, we can compute the average partial effect,
# noting that this estimate is highly affected from the uneven distribution of Artist_Follower and will likely 
# overestimate the true, overall partial effect

mean(lmfinal_z$coefficients[4]+2*lmfinal_z$coefficients[7]*x_noout_z[,'Artist_Follower'])

# average partial effect is 0.2259276, so when Artist_Follower increases by one unit (i.e. + 1 million followers),
# then the Artist Popularity increases by 0.2259276 standard deviations (i.e. sd(x[-outlier_index, 'Artist_Popularity'])*0.2259276 = 3.46 units)

# break-even partial effect: partial effect is exactly offset iff 0.250224 + 2*(-0.008995)*Artist_Follower == 0. Solving for Artist_Follower gives
# ((lmfinal_z$coefficients[4]) / (2*lmfinal_z$coefficients[7]))*-1 = 13.90843, so if Artist_Follower (in millions) is equal to 13.90843 the overall
# partial effect will be 0. All values below that yield a still positive overall marginal effect, all values above that
# yield an overall negative effect and then extreme followership is associated with lower Artist popularity on Spotify.
Artist_Follower_be <- ((lmfinal_z$coefficients[4]) / (2*lmfinal_z$coefficients[7]))*-1
Artist_Follower_diminish_index <- as.numeric(rownames(x_noout_z[which(x = x_noout_z[, 'Artist_Follower'] > Artist_Follower_be),]))
x[Artist_Follower_diminish_index, c("Track_Title", "Track_Artist", "Artist_Follower")]


# repeat all diagnostics for the final model from elasticnet
# Check the normality assumptions.
# Plot the studentized residuals against predicted values, the values of the indepen-
# dent variables. Do you see any disturbing patterns?
# Plot the leverage values. Do you see anything interesting?
# Plot the standardized changes in the regression coeffcients when cases are excluded
# from the analysis. Do you see any problems?
# Plot Cook's distance. Do you see any points that have a large effect?


# Non-parametric and semiparametric regression

library("np")
bw   <- npudensbw(~Track_Duration_ms, data=x_noout_z) # Track_Duration_ms bi-modal?
fhat <- npudens(bw)
fhat
plot(fhat, main=sprintf("%s with h=%.2f", fhat$pckertype, fhat$bw))
rug(x_noout_z$Track_Duration_ms)

bw <- npudensbw(~Artist_Popularity+Track_Duration_ms, data=x_noout_z, bwmethod="normal-reference")
fhat <- npudens(bw)
plot(fhat) # bi-modal

par(mfcol=c(1,1))
#1) at Track_Duration_ms ~ 1 and Artist_Popularity ~ -1 (meaning longer tracks' artists are less popular)
plot(fhat, view="fixed", phi=10, theta=320)
par(mfcol=c(1,1))
# 2) at Track_Duration_ms ~ -0.5 and Artist_Popularity ~ 1
plot(fhat, view="fixed", phi=10, theta=155)

nw2 <- npreg(Artist_Popularity~pc_2+Track_Duration_ms, data=x_noout_z)
summary(nw2)
plot(nw2)

bw <- npregbw(Artist_Popularity~Track_Duration_ms, data=x_noout_z)
mhat <- npreg(bw)
main <- sprintf("%s with h=%.2f", mhat$pckertype, mhat$bw)
plot(x_noout_z$Artist_Popularity, x_noout_z$Track_Duration_ms, pch=19, cex=0.3, main=main)
ind <- order(x_noout_z$Track_Duration_ms)
xs  <- cbind(x_noout_z$Track_Duration_ms, fitted(mhat))[ind,]
lines(xs[,1], xs[,2], col="red", lwd=2)
rug(x_noout_z$Track_Duration_ms)


bw <- npregbw(Artist_Popularity~Track_Duration_ms+Track_Popularity, data=x_noout_z)
mhat <- npreg(bw)
plot(mhat)


plotContour <- function (model, data, n=30) {
  mf <- model.frame(model$terms, data)
  mc <- lapply(as.list(mf[,-1]), pretty, n=n)
  zc <- predict(model, expand.grid(mc))
  dim(zc) <- sapply(mc, length)
  r2 <- 1-var(residuals(model))/var(mf[,1])
  contour(mc[[1]], mc[[2]], zc, xlab=names(mf)[2], ylab=names(mf)[3],
            main=sprintf("R^2=%.3f", r2))
  cc <- gray(0.75-0.75*(mf[,1]-min(mf[,1]))/(max(mf[,1])-min(mf[,1])))
  points(mf[,2], mf[,3], pch=19, cex=0.5, col=cc)
  }

model <- lm(Artist_Popularity~pc_1 + pc_2, data=x_noout_z)
par(mfrow=c(1,1))
plotContour(model, x_noout_z)

library("mgcv")
model <- gam(Artist_Popularity~s(Track_Popularity)+Track_Duration_ms, data=x_noout_z)
plotContour(model, x_noout_z$Track_Popularity, x_noout_z$Track_Duration_ms, x_noout_z$Artist_Popularity)
plot(model)




library("gam")

summary(lm(Artist_Popularity ~ fa_2 + pc_2 + Artist_Follower + fa_2sq + pc_2sq + Artist_Followersq, data=x_noout_z))

am1 <- gam(Artist_Popularity ~ s(fa_2) + s(pc_2) + s(Artist_Follower) + s(fa_2sq) + s(pc_2sq) + s(Artist_Followersq), data=x_noout_z)
print(am1)

par(mfrow=c(2,4))
plot(am1)
plot(fitted(am1), residuals(am1))
lines(lowess(fitted(am1), residuals(am1)), col="red", lwd=2)
n <- nrow(x_noout_z)
1-sum(residuals(am1)^2)/((n-1)*var(x_noout_z$Artist_Popularity))

sim <- npindex(Artist_Popularity ~ fa_2 + pc_2 + Artist_Follower + fa_2sq + pc_2sq + Artist_Followersq, data=x_noout_z)
summary(sim)

par(mfrow=c(2,4))
plot(sim)
plot(fitted(sim), residuals(sim))
lines(lowess(fitted(sim), residuals(sim)), col="red", lwd=2)
n <- nrow(x_noout_z)
1-sum(residuals(sim)^2)/((n-1)*var(x_noout_z$Artist_Popularity))

ppr <- ppr(Artist_Popularity ~ fa_2 + pc_2 + Artist_Follower + fa_2sq + pc_2sq + Artist_Followersq, data=x_noout_z, nterm=5)

summary(ppr)
par(mfrow=c(1,3))
plot(ppr)
plot(fitted(ppr), residuals(ppr))
lines(lowess(fitted(ppr), residuals(ppr)), col="red", lwd=2)
n <- nrow(x_noout_z)
1-sum(residuals(ppr)^2)/((n-1)*var(x_noout_z$Artist_Popularity))


##################################################################################
# unnecessary

# glmnet standardizes by default

lambda <- 10^seq(-3, 3, length = 100)


# Build the model
set.seed(123)
ridge <- caret::train(
  Artist_Popularity ~., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)
# Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)
# Make predictions
predictions_ridge_train <- ridge %>% predict(train)
predictions_ridge_test <- ridge %>% predict(test)
# Model prediction performance
data.frame(
  RMSE_train = RMSE(predictions_ridge_train, train$Artist_Popularity),
  Rsquare_train = R2(predictions_ridge_train, train$Artist_Popularity),
  RMSE_test = RMSE(predictions_ridge_test, test$Artist_Popularity),
  Rsquare_test = R2(predictions_ridge_test, test$Artist_Popularity)
)

# LASSO regression
# Build the model
set.seed(42)
lasso <- caret::train(
  Artist_Popularity ~., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)
# Model coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)
# Make predictions
predictions_lasso_train <- lasso %>% predict(train)
predictions_lasso_test <- lasso %>% predict(test)
# Model prediction performance
data.frame(
  RMSE_train = RMSE(predictions_lasso_train, train$Artist_Popularity),
  Rsquare_train = R2(predictions_lasso_train, train$Artist_Popularity),
  RMSE_test = RMSE(predictions_lasso_test, test$Artist_Popularity),
  Rsquare_test = R2(predictions_lasso_test, test$Artist_Popularity)
)

# Elasticnet: combines L1 (bias is sum of absolute value of coefficients times lambda, for high values of lambda many coefficients are
# exactly zeroed) and L2 (bias is sum of coefficients squared times lambda,
# tends to shrink coefficients towards 0 but cannot be exactly equal to 0, hence some coefficients will remain) penalties 
# of LASSO and Ridge regression, respectively linearly such that minimum MSE (which is the mean-variance trade-off) is achieved.

# Elastic net regression
# Build the model
set.seed(42)
elastic <- caret::train(
  Artist_Popularity ~., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Model coefficients
coef(elastic$finalModel, elastic$bestTune$lambda)
# Make predictions
predictions_elasticnet_train <- elastic %>% predict(train)
predictions_elasticnet_test <- elastic %>% predict(test)
# Model prediction performance
data.frame(
  RMSE_train = RMSE(predictions_elasticnet_train, train$Artist_Popularity),
  Rsquare_train = R2(predictions_elasticnet_train, train$Artist_Popularity),
  RMSE_test = RMSE(predictions_elasticnet_test, test$Artist_Popularity),
  Rsquare_test = R2(predictions_elasticnet_test, test$Artist_Popularity)
)

# Comparing models performance:

models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
resamples(models) %>% summary( metric = "RMSE")

# It can be seen that the elastic net model has the lowest median RMSE during cross-validation.

# Full data

x_full <- model.matrix(Artist_Popularity~., x_noout)[,-1] # without intercept as predict adds intercept term by default

# x_noout$Artist_Popularity[1] - predict(ridge.cv, newx = as.matrix(x_noout[1,-8])) # correct
# x_noout$Artist_Popularity[1] - predict(ridge.cv, x_full[1,])

# as.numeric(coef(ridge.cv)[,1]) %*% as.numeric(x_model[1,])
predict(ridge.cv, newx = x_full)



# R2(predict(ridge.cv, newx = as.matrix(x_noout[,-8])), as.matrix(x_noout[,8]))
# R2(predict(ridge.cv, newx = x_full), x_noout$Artist_Popularity)

# RMSE(predict(ridge.cv, newx = x_full), x_noout$Artist_Popularity) # correct, equivalent

# sqrt((sum(as.numeric(x_noout$Artist_Popularity - predict(ridge.cv, x_full))^2))/length(x_noout$Artist_Popularity)) # correct, equivalent



coef(ridge$finalModel, ridge$bestTune$lambda)
coef(lasso$finalModel, lasso$bestTune$lambda)
coef(elastic$finalModel, elastic$bestTune$lambda)

# training elasticnet on the full data set: 

set.seed(42)
elastic_full <- train(
  Artist_Popularity ~., data = x_noout, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

print(coef(elastic_full$finalModel, elastic_full$bestTune$lambda))
print(R2(predict(elastic_full, x_noout), x_noout$Artist_Popularity))







logitMod <- glm(x$Charts[-outlier_index] ~ fa_2 + I(fa_2^2) + fa_3 + I(fa_3^2) + fa_4 + pc_2 + Artist_Follower + I(Artist_Follower^2) + Track_Popularity, data=x_scaled[-outlier_index,], family=binomial(link="logit"))
summary(logitMod)
logitMod_0 <- glm(x$Charts[-outlier_index] ~ 1, family=binomial(link="logit"))

LLf <- logLik(logitMod)
LL0 <- logLik(logitMod_0)
as.vector(1- (LLf / LL0))

options(scipen=999)

x$Artist_Follower_k <- (x$Artist_Follower)/1000

logitMod <- glm(x$Charts[-outlier_index] ~ pc_1, data=x_scaled[-outlier_index,],
                family=binomial(link="logit"))
summary(logitMod)
logitMod_0 <- glm(x$Charts[-outlier_index] ~ 1, family=binomial(link="logit"))

LLf <- logLik(logitMod)
LL0 <- logLik(logitMod_0)
as.vector(1- (LLf / LL0))
(exp(logitMod$coefficients) - 1)*100 

exp(logitMod$coefficients)



#result: if Artist_Follower increases by one thousand, change in log-odds of being in charts rel. to not being in 
# charts is 0.27%.
# if Track_Popularity increases by 1 unit (between 0 and 100), then change in log-odds is 6.4%


# base probability of being in charts with mean followers, mean track popularity and mean pc_2
# mean follower: 1,198,952
# mean track popularity: 52.47

exp(logitMod$coefficients[1]) / (1 + exp(logitMod$coefficients[1]))*100 #

# probability of being in charts if track popularity increases by 1 standard deviation (i.e. 14 units)
exp(logitMod$coefficients[1] + logitMod$coefficients[2]) / (1+exp(logitMod$coefficients[1] + logitMod$coefficients[2]))*100
# 49%


# probability of being in charts if pc_2 increases by 1 standard deviation (i.e. 1.23 units)
exp(logitMod$coefficients[1] + logitMod$coefficients[3]) / (1+exp(logitMod$coefficients[1] + logitMod$coefficients[3]))*100
# 78%

# probability of being in charts if Artist_Popularity increases by 1 standard deviation (i.e. 15 units)
exp(logitMod$coefficients[1] + logitMod$coefficients[4]) / (1+exp(logitMod$coefficients[1] + logitMod$coefficients[4]))*100
# 90%




