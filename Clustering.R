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
z <- scale(x_features)

#############################################################

pc_cor <- prcomp(z, center = F, scale = F)
plot(pc_cor$x[,1], pc_cor$x[,2])

# k-means clustering

library(factoextra)

# Elbow method
fviz_nbclust(z, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


set.seed(42)
kz <- kmeans(z, c=4)

k_techno <- names(kz$cluster[kz$cluster == 1])
k_hiphop <- names(kz$cluster[kz$cluster == 2])
k_pop <- names(kz$cluster[kz$cluster == 3])
k_classic <- names(kz$cluster[kz$cluster == 4])

kz$cluster[k_techno] <- 4
kz$cluster[k_pop] <- 3
kz$cluster[k_hiphop] <- 2
kz$cluster[k_classic] <- 1

par(mfcol=c(1,2))
plot(pc_cor$x[,1], pc_cor$x[,2], col=kz$cluster,
     main="Cluster predictions, k-means (k=4)", pch=19, xlab = 'PC1', ylab = 'PC2')

# project cluster centers to PCA feature space:

cluster_centers_pca <- kz$centers %*% pc_cor$rotation[, 1:2]
points(cluster_centers_pca[,1], cluster_centers_pca[,2], col = 'magenta', pch = 10, cex = 3)

plot(pc_cor$x[,1], pc_cor$x[,2], col=as.numeric(x$Genre),
     main="Truth", pch=19, xlab = 'PC1', ylab = 'PC2')

sk_4 <- silhouette(kz$cluster, dist(z))
plot(sk_4, col=1:4, border=NA)


library("caret")
confusionMatrix(as.factor(as.numeric(kz$cluster)), as.factor(as.numeric(x$Genre)))

# Although k-means is not a classification algorithm, we can compute the proportions of genres per cluster to assign
# an interpretation for each cluster.

tab_kmeans <- table(kz$cluster, x$Genre)
round(tab_kmeans/colSums(tab_kmeans), digits = 2)

# It seems that Classic music and Techno music can be accurately distinguished but between Hip Hop and Pop there are 
# many false negatives, e.g. 62% of the tracks were predicted to be of the genre Pop although the truth was that they were
# of class pop, underlining the ambiguity between those genres in terms of music theoretical characteristics nowadays.
# Vice versa, 16% of the tracks predicted to be Hip Hop music were actually Pop music.

library("flexclust")
set.seed(42)
cl1 <- kcca(x = z, k = 4, family=kccaFamily('kmedians'))
cl1@second
plot(pc_cor$x[,1], pc_cor$x[,2], col=cl1@second)

# Elbow method
fviz_nbclust(z, cluster::pam, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

library("cluster")
set.seed(42)
cl2 <- pam(z, 4)
plot(pc_cor$x[,1], pc_cor$x[,2], col=cl2$clustering)


k_classic <- names(cl2$clustering[cl2$clustering == 1])
k_techno <- names(cl2$clustering[cl2$clustering == 2])
k_pop <- names(cl2$clustering[cl2$clustering == 3])
k_hiphop <- names(cl2$clustering[cl2$clustering == 4])

cl2$clustering[k_techno] <- 4
cl2$clustering[k_pop] <- 3
cl2$clustering[k_hiphop] <- 2
cl2$clustering[k_classic] <- 1


par(mfcol=c(1,2))
plot(pc_cor$x[,1], pc_cor$x[,2], col=cl2$clustering,
     main="Cluster predictions, k-medoids (k=4)", pch=19, xlab = 'PC1', ylab = 'PC2')
points(pc_cor$x[rownames(cl2$medoids),1], pc_cor$x[rownames(cl2$medoids),2], col = 'magenta', pch = 10, cex = 3)

plot(pc_cor$x[,1], pc_cor$x[,2], col=as.numeric(x$Genre),
     main="Truth", pch=19, xlab = 'PC1', ylab = 'PC2')

x[rownames(cl2$medoids), c('Track_Title', 'Track_Artist', 'Genre')]

sm_4 <- silhouette(cl2$clustering, dist(z))
plot(sm_4, col=1:4, border=NA)

confusionMatrix(as.factor(as.numeric(cl2$clustering)), as.factor(as.numeric(x$Genre)))

tab_kmedoids <- table(cl2$clustering, x$Genre)
round(tab_kmedoids/colSums(tab_kmedoids), digits = 2)

# Factor analysis + k-means on scores

library("psych")
KMO(z)
scree(z)
library("paran")
paran(z, centile=95, all=T, graph=T) # two factors are appropriate
fa <- fa(z, nfactors=2, rotate="varimax")
library("plot.matrix")
plot(loadings(fa), las=1)
al1 <- psych::alpha(z[,c("acousticness", "danceability", "energy", "loudness")], check.keys = TRUE)
al1
al2 <- psych::alpha(z[,c("instrumentalness", "valence")], check.keys = TRUE)
al2
scale <- cbind(al1$scores, al2$scores)


fviz_nbclust(scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

par(mfcol=c(2,2))


for (k in 2:5) {
  set.seed(42)
  cl <- kmeans(scale, k)
  plot(pc_cor$x[,1], pc_cor$x[,2], col=cl$cluster, pch=19, main=sprintf("%.0f clusters", k))
}

# Compare results from k-means on original scale to k-means to scores from factor analysis (with two factors):

reorder <- function(x, ...) {
  dr <- dist(x)
  hr <- hclust(dr)
  dc <- dist(t(x))
  hc <- hclust(dc)
  x[hr$order, hc$order]
}

set.seed(42)
cl1 <- kmeans(z, 4)
set.seed(42)
cl2 <- kmeans(scale, 4)

print(reorder(table(cl1$cluster,cl2$cluster))) # k-means on the 2-factor solution's scores did assign 26+1+3+12=42 cases
# differently!

par(mfcol=c(1,3))

plot(pc_cor$x[,1], pc_cor$x[,2], col=cl1$cluster, pch=19)
plot(pc_cor$x[,1], pc_cor$x[,2], col=cl2$cluster, pch=19)
plot(pc_cor$x[,1], pc_cor$x[,2], col=as.numeric(x$Genre), pch=19)

# So in this case k-means on factor scores achieved a worse result than using the original scales!

# soft-clustering with fuzzy c-means
par(mfcol=c(1,2))
library("e1071")
set.seed(42)
cl1 <- cmeans(z, 4)
mcolor <- colorRamp(c("black", "green", "blue", "red"))
col <- rgb(mcolor(cl1$membership[,1]), max=255)
plot(pc_cor$x[,1], pc_cor$x[,2], pch=19, col=col, main = 'Soft-clustering (fuzzy c-means, k = 4)')
plot(pc_cor$x[,1], pc_cor$x[,2], col=as.numeric(x$Genre),
     main="Truth", pch=19, xlab = 'PC1', ylab = 'PC2')

# Obviously, difficult to differentiate between Pop and Hip Hop music.

# Hierarchical clustering

# choose distance metric
# choose method for cluster merging

# 1) Divisive

cl  <- diana(z)
hcl <- cutree(as.hclust(cl), k = 4)
par(mfrow=c(1,1))
plot(pc_cor$x[,1], pc_cor$x[,2], col=hcl, 
     pch=19, cex=0.5)

# Strong overlaps between Techno, Hip Hop and Pop now, a fourth cluster is negligable.

# Distances

# euclidean distance
heatmap(as.matrix(dist(z)), revC = T)

# manhattan
heatmap(as.matrix(dist(z, 'manhattan')), revC = T)

# gower
heatmap(as.matrix(daisy(z, 'gower')), revC = T)

# Similarity

library("proxy")

d <- as.matrix(dist(z))
heatmap(d)
s <- pr_dist2simil(d)
heatmap(s)

# 2) Agglomerative

par(mfcol=c(1,1))

d <- dist(z)
# hclust
cl1 <- hclust(d, method = 'ward.D2')
memb <- cutree(cl1, 4)
plot(pc_cor$x[,1], pc_cor$x[,2], col=memb)
plot(cl1) # from the dendrogram it looks that there are is either a two or three cluster solution, 
# cutting the dendrogram at height ~ 8, two observations form a distinct cluster which is not meaniningful
# the biggest "jump" is at ~ 8, so a three cluster solution seems most appropriate

library("ggplot2")
library("tibble")

ggplot(cl1$height %>%
as_tibble() %>%
add_column(groups = length(cl1$height):1) %>%
rename(height=value),
aes(x=groups, y=height)) +
geom_point() +
geom_line()

# biggest jumps at 38.3 (1) to 21.9 (2) and 21.9 (2) to 13.3 (3), either two or three cluster solution from 
# hierarchical clustering

# EM-Clustering
# based on assumption that each within cluster multivariately normally distributed distributed,
# fails if this doesn't hold in the data

library("mclust")
set.seed(42)
cl <- Mclust(z)
summary(cl)
# 6 cluster solution was chosen
par(mfrow=c(2,2))
plot(cl, "BIC")
plot(pc_cor$x[,1], pc_cor$x[,2], col = cl$classification)
EM_density_plot <- plot(cl, "density")


# DBSCAN

par(mfcol=c(2,2))

library("fpc")
set.seed(42)
cl <- dbscan(z, 0.5, scale=F, MinPts = 5)
col <- c('grey', rainbow(max(cl$cluster)))
plot(pc_cor$x[,1], pc_cor$x[,2], pch=19, col=col[1+cl$cluster])

set.seed(42)
cl <- dbscan(z, 1, scale=F, MinPts = 5)
col <- c('grey', rainbow(max(cl$cluster)))
plot(pc_cor$x[,1], pc_cor$x[,2], pch=19, col=col[1+cl$cluster])

set.seed(42)
cl <- dbscan(z, 1.5, scale=F, MinPts = 5)
col <- c('grey', rainbow(max(cl$cluster)))
plot(pc_cor$x[,1], pc_cor$x[,2], pch=19, col=col[1+cl$cluster])

set.seed(42)
cl <- dbscan(z, 2, scale=F, MinPts = 5)
col <- c('grey', rainbow(max(cl$cluster)))
plot(pc_cor$x[,1], pc_cor$x[,2], pch=19, col=col[1+cl$cluster])


# Mixed clustering


# hclust
d    <- dist(z)
set.seed(42)
cl1  <- hclust(d, method="ward.D2")
memb <- cutree(cl1, 4)
# kmeans
groupm  <- aggregate(z, list(memb), mean)
centers <- cbind(groupm$acousticness, 
                 groupm$danceability, 
                 groupm$energy, 
                 groupm$instrumentalness,
                 groupm$liveness,
                 groupm$loudness,
                 groupm$speechiness,
                 groupm$tempo,
                 groupm$valence)
set.seed(42)
cl2 <- kmeans(z, centers = centers)
# compare results (confusion matrix)
table(memb, cl2$cluster) # result: k-means has classified 4 observations differently than hclust


par(mfcol=c(1,2))
plot(pc_cor$x[,1], pc_cor$x[,2], col=cl2$cluster,
     main="Cluster predictions, mixed clustering", pch=19, xlab = 'PC1', ylab = 'PC2')

# project cluster centers to PCA feature space:

cluster_centers_pca <- cl2$centers %*% pc_cor$rotation[, 1:2]
points(cluster_centers_pca[,1], cluster_centers_pca[,2], col = 'magenta', pch = 10, cex = 3)

plot(pc_cor$x[,1], pc_cor$x[,2], col=as.numeric(x$Genre),
     main="Truth", pch=19, xlab = 'PC1', ylab = 'PC2')

# Number of clusters

library("NbClust")

# Total variance explained
tve <- rep(NA, 15)
for (k in 2:15) {
  clk <- kmeans(z, k)
  tve[k] <- 1-clk$tot.withinss/clk$totss
}
plot(tve, type="b")

set.seed(42)
NbClust(z, method="ward.D2", index="ch") # result: k*=2
set.seed(42)
NbClust(z, method="ward.D2") # result: majority vote (12) for k*=3.


# Silhouette method
set.seed(42)
fviz_nbclust(z, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

d <- dist(z)
cl1 <- hclust(d, method="ward.D2")
memb2 <- cutree(cl1, 2)
memb3 <- cutree(cl1, 3)
memb4 <- cutree(cl1, 4)

library("cluster")
par(mfcol=c(2,2))
plot(pc_cor$x[,1], pc_cor$x[,2], col=memb2)
s2 <- silhouette(memb2, d)
plot(s2, col=1:2, border=NA)
plot(pc_cor$x[,1], pc_cor$x[,2], col=memb3)
s3 <- silhouette(memb3, d)
plot(s3, col=1:3, border=NA)
#plot(pc_cor$x[,1], pc_cor$x[,2], col=memb4)
#s3 <- silhouette(memb4, d)
#plot(s4, col=1:4, border=NA)

par(mfcol=c(1,3))
clusplot(z, memb2, col.p=memb2)
clusplot(z, memb3, col.p=memb3)
clusplot(z, memb4, col.p=memb4)

# Final result: k-medoids best

library("cluster")
set.seed(42)
cl2 <- pam(z, 4)
plot(pc_cor$x[,1], pc_cor$x[,2], col=cl2$clustering)

k_classic <- names(cl2$clustering[cl2$clustering == 1])
k_techno <- names(cl2$clustering[cl2$clustering == 2])
k_pop <- names(cl2$clustering[cl2$clustering == 3])
k_hiphop <- names(cl2$clustering[cl2$clustering == 4])

cl2$clustering[k_techno] <- 4
cl2$clustering[k_pop] <- 3
cl2$clustering[k_hiphop] <- 2
cl2$clustering[k_classic] <- 1


par(mfcol=c(1,2))
plot(pc_cor$x[,1], pc_cor$x[,2], col=cl2$clustering,
     main="Cluster predictions, k-medoids (k=4)", pch=19, xlab = 'PC1', ylab = 'PC2')
points(pc_cor$x[rownames(cl2$medoids),1], pc_cor$x[rownames(cl2$medoids),2], col = 'magenta', pch = 10, cex = 3)

plot(pc_cor$x[,1], pc_cor$x[,2], col=as.numeric(x$Genre),
     main="Truth", pch=19, xlab = 'PC1', ylab = 'PC2')

x[rownames(cl2$medoids), c('Track_Title', 'Track_Artist', 'Genre')]

sm_4 <- silhouette(cl2$clustering, dist(z))
plot(sm_4, col=1:4, border=NA)

confusionMatrix(as.factor(as.numeric(cl2$clustering)), as.factor(as.numeric(x$Genre)))

tab_kmedoids <- table(cl2$clustering, x$Genre)
round(tab_kmedoids/colSums(tab_kmedoids), digits = 2)

x$cluster <- as.factor(cl2$clustering)

write.csv(x,"C:\\Users\\wolfg\\Desktop\\Uni\\HU Master\\Datenanalyse II\\DA2_Homework\\cluster_df.csv", row.names = TRUE)
