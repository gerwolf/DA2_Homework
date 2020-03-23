# Check beforehand
# - inverse correlation matrix
# - Anti-image matrix
# - Kaiser-Meyer-Olkin-Criteria
# - Bartlett’s test of sphericity

# Check afterwards
# - communalities h2
# - reproduced correlation matrix

############### Data prep

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

drop.cols <- c('Artist_ID', 'Genre', 'Track_Artist','Track_ID', 'Track_Title', 'key',
               'mode', 'time_signature', 'video_ID', 'Charts', 'acousticness', 'danceability', 'energy', 'instrumentalness',
               'liveness', 'loudness', 'speechiness', 'tempo', 'valence', 'Artist_Popularity', 'Track_Popularity')

x_select <- dplyr::select(x, -one_of("Streams"))

# x_select <- dplyr::select(x, -one_of(drop.cols))
x_complete <- x_select[complete.cases(x_select), ]

x_sel <- dplyr::select(x_complete, -one_of(drop.cols))

complete_index <- as.numeric(rownames(x_sel))

# convert release dates of only year to beginning of year standard format

x_sel$Release_Date <- as.Date(paste(x_sel$Release_Date, 1, 1, sep = "-"))
  
x_sel$days_release <- as.integer(round(difftime('2020-03-01', x_sel$Release_Date, units = "days"), digits = 0))

x_sel$Release_Date <- NULL

z <- scale(x_sel)

#################

par(mfrow=c(1,1))

library("psych")
library("lattice")
# bfi: 25 personality self report items taken from 
# the International Personality Item Pool
colnames(z)

# inverse and partial correlations
p  <- solve(cor(z, use="complete.obs"))

# if the model holds then the non-diagonal elements of R􀀀1
# must be close to zero (relative to the diagonal element)

levelplot(p, main="Inverse & partial correlations", scales=list(x=list(rot=90)))

# anti-image
pr <- -p/sqrt(outer(diag(p), diag(p)))
levelplot(pr, main="Anti-Image correlation", scales=list(x=list(rot=90)))
# Kaiser-Meyer-Olkin & MSA
KMO(z) # result: 0.74 overall: meritorious, every individual MSA value is as least as high as 0.53 (middling)
# Bartlett test of sphericity
cortest.bartlett(z) # result: correlation matrix is NOT an identity matrix, so proceed with factor analysis

scree(z)

library("paran")

paran(z, centile=95, all=T, graph=T) # four factors are appropriate

# result: 4 factors, What could it be?

pca <- principal(z, nfactors=4, rotate="none") # h2 are "communalities"
pa <- fa(z, nfactors=4, rotate="none", fm="pa") # principal axis extraction
uls <- fa(z, nfactors=4, rotate="none")
ml <- fa(z, nfactors=4, rotate="none", fm="ml") # cum. variance: 72% explained
print(ml$loadings) # simple structre: each row of loadings matrix contains one zero at least, so each item is described
# by a maximum of q (=4) - 1 factors in this case

fa.congruence(pca, pa)
fa.congruence(pca, uls)
fa.congruence(pca, ml)
fa.congruence(pa, ml)

plot(ml$loadings[,1], ml$loadings[,2], xlim = c(-0.5, 1), ylim = c(-1,1))
abline(h = 0, v= 0)
print(ml$loadings, cutoff=.4, order = T)


# unclear items: Artist_Follower, Track_Duration_ms  

# using varimax is an orthogonal rotation, i.e. factor loadings are such that they are orthogonal to each other
# not appropriate if there is correlation between the factors, can be confirmed by computing correlation matrix
# between the scores from the benchmark methods like PCA, principal axis or unrotated ML

ml.varimax <- fa(z, nfactors=4, rotate="varimax", fm="ml") # now Track_Duration_ms loads on factor 4, Artist_Follower 
# still doesn't load on any factor
# when using an orthogonal rotation method then the loadings represent the correlations between the items and their
# corresponding factor
# communality: how much percent of the variance of a variable can be explained by the common factor
# example: 90% of variance in viewsCount can be explained by the common "YouTube" factor
print(ml.varimax$loadings)
print(ml.varimax$loadings, cutoff=.4, sort = T)

fa.congruence(ml, ml.varimax)

ml.promax <- fa(z, nfactors=4, rotate="promax", fm="ml")
ml.oblimin <- fa(z, nfactors=4, rotate="oblimin", fm="ml")

fa.congruence(ml.varimax, ml.promax)
fa.congruence(ml.varimax, ml.oblimin)
fa.congruence(ml.promax, ml.oblimin)

kaiser(ml)


# No single factor loads on ten or more items, at least four absolute loadings are above 0.6, so sample size doesn't matter
# (from Guadagnoli and Velicer (1988))

par(mfrow=c(1,2))

threshold <- 0.4
plot(ml.varimax$loadings[,1], ml.varimax$loadings[,2], xlim = c(-1, 1), ylim = c(-1,1.1), 
     main = 'Loadings plot of factors 3 and 1',xlab = 'ML3', ylab = 'ML1')
palette <- c('red', 'blue')
col_index <- ifelse((abs(ml.varimax$loadings[,1]) > threshold) | (abs(ml.varimax$loadings[,2]) > threshold), 1, 0)
cols <- palette[as.numeric(as.factor(col_index))]
points(ml.varimax$loadings[,1], ml.varimax$loadings[,2], pch=19, col=cols)
abline(h = 0, v= 0)
text(ml.varimax$loadings[,1:2], as.character(rownames(ml.varimax$loadings)), pos = 1, cex = 0.5, offset = 0.5)
rect(xleft = -threshold, ybottom = -threshold, xright = threshold, ytop = threshold)
fa.diagram(ml.varimax, simple=TRUE, cut=.2, digits=2)

#fa1 <- fa(z, nfactors = 4)
#fa2 <- kaiser(fa(z, nfactors = 4, rotate = 'none'))
#fa.congruence(fa1, fa2)

fa.congruence(ml.promax, ml.varimax)

# Correlatedness of factors:

ml.promax$Phi # compare this to the loadings matrix, e.g. factors 2 and 3 negatively correlated,
print(ml.promax$loadings, cutoff=.4, sort = T)
ml.promax$Phi
# Factor 2 (ML2): "YouTube success"
# Factor 3 (ML3): "music supply and cooperativeness/marketing"
# Factor 4 (ML4): 
# Factor 1 (ML1):

# Interestingly, factors 2 and 3 are negatively correlated which is what one would expect, given the interpretations
# above.
# Factors 3 and 4 are positively correlated.
# Factors 2 and 4 are negativeley correlated.


# As a conclusion, from the factor correlations matrix with |r| > 0.3 which holds for factors (3,4)
# it would be appropriate to use an oblique rotation such as promax.

fa.congruence(ml.promax, ml.varimax)

# A factor congruence test, however, shows that the factors between an orthogonally rotated factor model (here varimax)
# and an obliquely rotated factor model (here promax), both Kaiser normalized, are almost identical with entries of 1
# on the diagonal. Hence, It seems that correlation between the factors is not severe and it is reasonable to assume an
# orthogonal rotation is adequate.

# For orthoghonal rotations: cor(item, factor) = loadings ("pattern") matrix = structure matrix, 
# since structure matrix = loadings matrix *%* factor intercorrelation matrix (which is identity for orthogonal rotations)
# example: ml.promax$loadings %*% ml.promax$Phi = structure matrix



library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

ml.unrotated <- fa(z, nfactors=4, rotate="none", fm="ml")

library("plot.matrix")

par(mfrow=c(1,1))
par(las=2)
par(mar=c(15,5,2,1))
plot(t(ml.unrotated$loadings), cex = 0.5, ann = FALSE, key = NULL)
title(main = "Unrotated loadings")
title(xlab="Variable", line=10, cex.lab=1.2)
title(ylab="Factors", line=3, cex.lab=1.2)

par(mfrow=c(1,1))
par(las=2)
par(mar=c(15,5,2,1))
plot(t(ml.promax$loadings), cex = 0.5, ann = FALSE, key = NULL)
title(main = "Promax loadings")
title(xlab="Variable", line=10, cex.lab=1.2)
title(ylab="Factors", line=3, cex.lab=1.2)

par(mfrow=c(1,1))
par(las=2)
par(mar=c(15,5,2,1))
plot(t(ml.promax$loadings), cex = 0.5, ann = FALSE, key = NULL)
title(main = "Promax pattern matrix")
title(xlab="Variable", line=10, cex.lab=1.2)
title(ylab="Factors", line=3, cex.lab=1.2)

par(mfrow=c(1,1))
par(las=2)
par(mar=c(15,5,2,1))
plot(t(ml.promax$Structure), cex = 0.5, ann = FALSE, key = NULL)
title(main = "Promax structure matrix")
title(xlab="Variable", line=10, cex.lab=1.2)
title(ylab="Factors", line=3, cex.lab=1.2)


#corrplot(t(ml.unrotated$loadings), method="color", col=col(200), 
#         addCoef.col = "black", # Add coefficient of correlation
#         tl.col="black", tl.srt=90, #Text label color and rotation
#         diag=TRUE, mar = c(0,0,5,5), title = 'FA (ML, unrotated) - loadings matrix', 
#         tl.cex = 0.8, number.cex = 0.5, cl.cex = 0.8, cl.pos="n")

#corrplot(t(ml.promax$loadings), method="color", col=col(200), 
#         addCoef.col = "black", # Add coefficient of correlation
#         tl.col="black", tl.srt=90, #Text label color and rotation
#         diag=TRUE, mar = c(0,0,5,5), title = 'FA (ML, promax) - loadings matrix', 
#         tl.cex = 0.8, number.cex = 0.5, cl.cex = 0.8, cl.pos="n")

#corrplot(t(ml.promax$Structure), method="color", col=col(200), 
#         addCoef.col = "black", # Add coefficient of correlation
#         tl.col="black", tl.srt=90, #Text label color and rotation
#         diag=TRUE, mar = c(0,0,5,5), title = 'FA (ML, promax) - structure matrix', 
#         tl.cex = 0.8, number.cex = 0.5, cl.cex = 0.8, cl.pos="n")

# Pattern and structure matrix

cbind(ml.unrotated$loadings, ml.promax$loadings, ml.promax$Structure)

# Extract the scores - regression vs. Bartlett

ml.promax$scores # != psych::factor.scores(x = z, f = ml.promax$loadings) Why?

fa1 <-factanal(z, factors=4, scores = 'regression', lower = 0.1) # ML with Kaiser normalization 
head(fa1$scores)
fa2 <- fa(z, nfactors=4) # oblimin rotation without Kaiser normalization
head(fa2$scores)
cor(fa1$scores, fa2$scores)
cor(fa1$scores, ml.promax$scores)
cor(ml.promax$scores, ml.varimax$scores)

fa_bartlett_scores <- fa(z, nfactors=4, rotate="varimax", fm="ml", scores = 'Bartlett')
cor(ml.varimax$scores, fa_bartlett_scores$scores)
cor(fa_bartlett_scores$scores)


# Factor reliability

vars_1 <- abs(ml.promax$loadings[,1])>0.4
vars_2 <- abs(ml.promax$loadings[,2])>0.4
vars_3 <- abs(ml.promax$loadings[,3])>0.4
vars_4 <- abs(ml.promax$loadings[,4])>0.4
# Cronbachs alpha (items not reversed)

psych::alpha(cor(z[,vars_1])) # commentCount, dislikeCount, likeCount, viewsCount
psych::alpha(cor(z[,vars_2])) # Artist_Albums_Number, Artist_Albums_Tracks_Number, Artist_Compilations_Number, Artist_Compilations_Tracks_Number, Artist_Singles_Number
psych::alpha(cor(z[,vars_3])) # Artist_Albums_Tracks_Number, Artist_Appearances_Number, Artist_Appearances_Tracks_Number, Track_Duration_ms 
psych::alpha(cor(z[,vars_4])) # Artist_Singles_Number, Artist_Singles_Tracks_Number

# Cronbachs alpha (items reversed)

psych::alpha(cor(z[,vars_1]), check.keys=T)

# Cronbachs alpha and sum scores

key.list <- list(one = as.numeric(which(vars_1==1)), 
                two = as.numeric(which(vars_2==1)), 
                three = as.numeric(which(vars_3==1)), 
                four = as.numeric(which(vars_4==1)))

sign.mat <- cbind(sign(ml.promax$loadings[,1]), 
                      sign(ml.promax$loadings[,2]),
                      sign(ml.promax$loadings[,3]),
                      sign(ml.promax$loadings[,4]))

keys <- make.keys(z, key.list,item.labels = colnames(z))

si <- scoreItems(keys * sign.mat, z)
si
si$scores

splitHalf(z)

items_1 <- reverse.code((keys[,1] * sign.mat[,1])[(keys[,1] * sign.mat[,1]) != 0], z[, vars_1])
items_2 <- reverse.code((keys[,2] * sign.mat[,2])[(keys[,2] * sign.mat[,2]) != 0], z[, vars_2])
items_3 <- reverse.code((keys[,3] * sign.mat[,3])[(keys[,3] * sign.mat[,3]) != 0], z[, vars_3])
items_4 <- reverse.code((keys[,4] * sign.mat[,4])[(keys[,4] * sign.mat[,4]) != 0], z[, vars_4])

library("additivityTests")

tukey.test(items_1) # result: H0 rejected, i.e. sum score is insufficient to represent the variables (factor 2)
tukey.test(items_2) # result: H0 rejected (factor 3)
tukey.test(items_3) # result: H0 cannot be rejected (factor 4)
tukey.test(items_4) # result: H0 cannot be rejected (factor 1)

# Displaying correlatedness of (promax) rotated factors

pairs(cbind(si$scores[,1], si$scores[,2], si$scores[,3], si$scores[,4]))

# Manually compute sumscores:

print(ml.promax$loadings, cutoff = 0.5, sort = T)

f2 <- (z[,11] + z[,12] + z[,13] + z[,14])/4
f3 <- (z[,1] + z[,2] + z[,5] + z[,6])/4
f4 <- (z[,3] + z[,4])/2
f1 <- (z[,9] + z[,10])/2

cor(si$scores)

cor(cbind(f1, f2, f3, f4))

cor(cbind(f2, si$scores[,1], f1, si$scores[,4])) # f1(=ML4) and the manually computed score are highly correlated
# because I included Artist_Singles_Number which loads on both factors ML3 and ML1
 # but score of factor 3 and the manual sumscore and the score of factor 4
# and its manual sumscore are identical (correlations = 1)


par(mfrow=c(1,2))
hist(pca$scores[,1], main = "First component", xlab = "Score corresponding to component 1")
rug(pca$scores[ifelse(x[complete_index, 'Charts'] == 1, TRUE, FALSE), 1], col="blue")
rug(pca$scores[ifelse(x[complete_index, 'Charts'] != 1, TRUE, FALSE), 1], col="red")

hist(ml$scores[,1], main = "Second factor", xlab = "Score corresponding to factor 2 (uls)")
rug(ml$scores[ifelse(x[complete_index, 'Charts'] == 1, TRUE, FALSE), 1], col="blue")
rug(ml$scores[ifelse(x[complete_index, 'Charts'] != 1, TRUE, FALSE), 1], col="red")


par(mfrow=c(1,2))
hist(ml.varimax$scores[,1], main = 'Youtube popularity factor', xlab = 'Score corresponding to factor 2 (varimax)')
rug(ml.varimax$scores[ifelse(x[complete_index, 'Charts'] == 1, TRUE, FALSE), 1], col="blue")
rug(ml.varimax$scores[ifelse(x[complete_index, 'Charts'] != 1, TRUE, FALSE), 1], col="red")

hist(ml.varimax$scores[,2], main = 'Music supply factor', xlab = 'Score corresponding to factor 3 (varimax)')
rug(ml.varimax$scores[ifelse(x[complete_index, 'Charts'] == 1, TRUE, FALSE), 2], col="blue")
rug(ml.varimax$scores[ifelse(x[complete_index, 'Charts'] != 1, TRUE, FALSE), 2], col="red")

# Final result: four factors extracted

x_complete$factor_2 <- si$scores[,1]
x_complete$factor_3 <- si$scores[,2]
x_complete$factor_4 <- si$scores[,3]
x_complete$factor_1 <- si$scores[,4]

x_complete$man_fa2 <- f2
x_complete$man_fa3 <- f3
x_complete$man_fa4 <- f4
x_complete$man_fa1 <- f1

x_complete$days_release <- x_sel$days_release

pca_df <- import('https://docs.google.com/spreadsheets/d/1i78eZ6tMqF7aV3JOvg4jZ7vJmkr7eejQ/export?format=csv&gid=734613762')
pca_df<- dplyr::select(pca_df, c("pc_1", "pc_2"))
pca_df$pc_1 <- as.numeric(sub(",", ".", pca_df$pc_1, fixed = TRUE))
pca_df$pc_2 <- as.numeric(sub(",", ".", pca_df$pc_2, fixed = TRUE))
x_complete$pc_1 <- pca_df$pc_1
x_complete$pc_2 <- pca_df$pc_2

model1 <- lm(Artist_Popularity ~ factor_1 + factor_2 + factor_3 + factor_4 + pc_1 + pc_2 + Artist_Follower + days_release, data = x_complete)
options(scipen=999)
summary(model1)


model2 <- lm(Track_Popularity ~ man_fa2 + man_fa4, data = x_complete)
summary(model2)

# roughly the same results

print(ml.varimax$loadings, sort = TRUE, cutoff = 0.5)

library("xlsx")

write.xlsx(x_complete,"C:\\Users\\wolfg\\Desktop\\Uni\\HU Master\\Datenanalyse II\\DA2_Homework\\fa_df.xlsx", row.names = TRUE)
