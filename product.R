library(ggplot2)
library(GGally)
library(DMwR)

set.seed(5580)
prod <- read.csv("C:/CDA/Sem 2/Data Mining/Assignment1/productscluster.csv")
View(prod)

summary(prod[2:7])
ggpairs(prod[,2:5],upper=list(continuous=ggally_points), lower=list(continuous="points"),title="Products before outlier removal")

boxplot(prod$Baskets)


prod.clean <- prod[prod$ITEM_SK != 11740941, ]
ggpairs(prod.clean[,2:5],upper=list(continuous=ggally_points), lower=list(continuous="points"),title="Products after removing ITEM_SK=11740941")

summary(prod.clean[2:7])
prod.scale = scale(prod.clean[,2:5])
View(prod.scale)

withinSSrange <- function(data,low,high,maxIter)
{
  withinss = array(0, dim=c(high-low+1));
  print("K  : WCSS")
  print(" ")
  for(i in low:high)
  {
    withinss[i-low+1] <- kmeans(data, i, maxIter)$tot.withinss
    print (c(i,":",round((withinss[i-low+1]),3)))
  }
  withinss
}

plot(withinSSrange(prod.scale,1,50,150))

pkm = kmeans(prod.scale, 6,150)
prod.realCenters = unscale(pkm$centers,prod.scale)
clusteredProd = cbind(prod.clean,pkm$cluster)

View(clusteredProd)

plot(clusteredProd[,2:5],col=pkm$cluster)

write.csv(clusteredProd,file="C:/CDA/Sem 2/Data Mining/Assignment1/productcluster_output.csv",col.names = FALSE)

