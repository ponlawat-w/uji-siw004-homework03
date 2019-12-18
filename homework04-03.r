data <- read.csv('./data.csv', header = TRUE)

# 1. Preparing Data

data.original <- data.frame(data)
data <- data[, -1] # Eliminate column `group`

# 2. Hierarchical Clustering
## 2.1. Euclidean Distance

par(mfrow = c(1, 3))
hclust.euclidean.single <- hclust(dist(data), method = 'single')
plclust(hclust.euclidean.single, labels = row.names(data), ylab='Distance')
abline(h = 250, col = 'red')
title('Single Linkage')
hclust.euclidean.complete <- hclust(dist(data), method = 'complete')
plclust(hclust.euclidean.complete, labels = row.names(data), ylab='Distance')
abline(h = 2000, col = 'red')
title('Complete Linkage')
hclust.euclidean.average <- hclust(dist(data), method = 'average')
plclust(hclust.euclidean.average, labels = row.names(data), ylab='Distance')
abline(h = 1200, col = 'red')
title('Average Linkage')

## FIGURE: Three different linkages of 2-group clustering using euclidean distance

dev.off()

# From the figure, we dicided to use complete linkage.
treecut.euclidean <- cutree(hclust.euclidean.complete, h = 2000)
data.euclidean <- data.frame(data.original)
data.euclidean$NewGroup <- treecut.euclidean
data.euclidean

max(
  length(which(data.euclidean$Group == data.euclidean$NewGroup)) / nrow(data.euclidean),
  length(which(data.euclidean$Group != data.euclidean$NewGroup)) / nrow(data.euclidean)
)
# Result: [1] 0.5384615

# From the result, only 54% of data are clustered to the same group comparing to original data, which is quite low.

## 2.2. Manhattan Distance

par(mfrow = c(1, 3))
hclust.manhattan.single <- hclust(dist(data, method = "manhattan"), method = 'single')
plclust(hclust.manhattan.single, labels = row.names(data), ylab='Distance')
abline(h = 300, col = 'red')
title('Single Linkage')
hclust.manhattan.complete <- hclust(dist(data, method = "manhattan"), method = 'complete')
plclust(hclust.manhattan.complete, labels = row.names(data), ylab='Distance')
abline(h = 2000, col = 'red')
title('Complete Linkage')
hclust.manhattan.average <- hclust(dist(data, method = "manhattan"), method = 'average')
plclust(hclust.manhattan.average, labels = row.names(data), ylab='Distance')
abline(h = 1200, col = 'red')
title('Average Linkage')

## FIGURE: Three different linkages of 2-group clustering using manhattan distance

dev.off()

# From the figure, we dicided to use complete linkage.
treecut.manhattan <- cutree(hclust.manhattan.complete, h = 2000)
data.manhattan <- data.frame(data.original)
data.manhattan$NewGroup <- treecut.manhattan
data.manhattan

max(
  length(which(data.manhattan$Group == data.manhattan$NewGroup)) / nrow(data.manhattan),
  length(which(data.manhattan$Group != data.manhattan$NewGroup)) / nrow(data.manhattan)
)
# Result: [1] 0.5384615

# From the result, the manhattan distance in hierarchical clustering gives the same result with euclidean result.

# 3. K-Means Clustering

kmeansResult <- kmeans(data, 2)
data.kmeans <- data.frame(data.original)
data.kmeans$NewGroup <- kmeansResult$cluster
data.kmeans
max(
  length(which(data.kmeans$Group == data.kmeans$NewGroup)) / nrow(data.kmeans),
  length(which(data.kmeans$Group != data.kmeans$NewGroup)) / nrow(data.kmeans)
)
# Result: [1] 0.7230769

# From the result, K-Means Clustering gives 72% of identical to the original data which is quite high comparing to hierarchical clustering.

# 4. Linear Discriminant Analysis (LDA)

library(MASS)

# Prepare new data
newData <- rbind(
  c(110, 3320, 0.240, 39),
  c(120, 3310, 0.298, 37)
)
colnames(newData) <- colnames(data)
newData <- data.frame(newData)

result <- data.frame(newData)

# Predict new data using euclidean-distance hierarchical clustering
hclust.euclidean.lda <- lda(
  data.euclidean$NewGroup ~ HR + BW + Factor68 + Gesage
, data = data)
predict.euclidean <- predict(hclust.euclidean.lda, newdata = newData)
result$h.eu <- predict.euclidean$class
result$h.eu.p1 <- predict.euclidean$posterior[, 1]
result$h.eu.p2 <- predict.euclidean$posterior[, 2]

# Predict new data using manhattan-distance hierarchical clustering
hclust.manhattan.lda <- lda(
  data.manhattan$NewGroup ~ HR + BW + Factor68 + Gesage
, data = data)
predict.manhattan <- predict(hclust.manhattan.lda, newdata = newData)
result$h.mh <- predict.manhattan$class
result$h.mh.p1 <- predict.manhattan$posterior[, 1]
result$h.mh.p2 <- predict.manhattan$posterior[, 2]

# Predict new data using k-means clustering
hclust.kmeans.lda <- lda(
  data.kmeans$NewGroup ~ HR + BW + Factor68 + Gesage
, data = data)
predict.kmeans <- predict(hclust.kmeans.lda, newdata = newData)
result$h.km <- predict.kmeans$class
result$h.km.p1 <- predict.kmeans$posterior[, 1]
result$h.km.p2 <- predict.kmeans$posterior[, 2]

# Display results
result

# From the result table, regardless of clustering method, all the models classify both of new data to be group 1. However, it can be seen that models from hierarchical clustering give higher prosterior than k-means.
