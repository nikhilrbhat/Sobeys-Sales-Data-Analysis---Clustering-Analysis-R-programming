library(ggplot2)
library(GGally)
library(DMwR)

set.seed(5580)
cust <- read.csv("C:/CDA/Sem 2/Data Mining/Assignment1/customercluster.csv")
View(cust)

summary(cust[2:7])

ggpairs(cust[,2:7],upper=list(continuous=ggally_points), lower=list(continuous="points"),title="Customers before outlier removal")

cust.clean <- cust[cust$CUSTOMER_SK != 1, ]
ggpairs(cust.clean[,2:7],upper=list(continuous=ggally_points), lower=list(continuous="points"),title="Customers after removing CUSTOMER_SK=1")

summary(cust.clean[2:7])

cust.scale = scale(cust.clean[,2:7])
View(cust.scale)

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

plot(withinSSrange(cust.scale,1,50,150))

ckm = kmeans(cust.scale, 6,150)
cust.realCenters = unscale(ckm$centers,cust.scale)
clusteredCust = cbind(cust.clean,ckm$cluster)

View(clusteredCust)

plot(clusteredCust[,2:7],col=ckm$cluster)

write.csv(clusteredCust,file="C:/CDA/Sem 2/Data Mining/Assignment1/customercluster_output.csv",col.names = FALSE)