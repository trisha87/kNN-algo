library("ggplot2")
knn.df <- read.csv("dummy_data.csv")
head(knn.df)
knn.df$Y <- factor(knn.df$Y)
ggplot(knn.df, aes(x=X1, y=X2, shape=Y, color=Y)) +
  geom_point()

dist.vectr <- fuction(knn.df, testdata)
{
  dist <- vector(, nrow(knn.df))
  for(i in 1:nrow(knn.df))
  {
    dist[i] <- sqrt((knn.df[i, 'X1'] - testdata[1])^2 + (knn.df[i, 'X2'] - testdata[2])^2)
  }
  return(dist)
}

knn <- function(knn.df, k=5)
{
  dist <- dist.vectr(knn.df, testdata)
  indices <- order(dist)[1:k]
  predicted.y <- ifelse(mean(knn.df[indices, 'Y']) > 0.5, 1, 0)
  return(predicted.y)
}
