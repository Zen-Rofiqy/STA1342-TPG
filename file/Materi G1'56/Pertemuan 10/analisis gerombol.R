#######K-Means#####

#load data
library(factoextra)
data_mall <- read.csv('C:/Users/ASUS/Documents/APG/pt 10/Mall_Customers.csv')
head(data_mall)

#praproses data
data_mall <- data_mall[,c("Age", "Annual.Income", "Spending.Score")]
head(data_mall)

#standarisasi data
data_mall_stdr <- scale(data_mall)
apply(data_mall_stdr,2,mean)
apply(data_mall_stdr,2,sd)

#memilih banyak gerombol
?fviz_nbclust
fviz_nbclust(data_mall_stdr, FUNcluster = kmeans, method = "silhouette")
fviz_nbclust(data_mall_stdr, FUNcluster = kmeans, method = "wss")

#menerapkan k-means
kmeans_mall <- eclust(data_mall, stand = TRUE, FUNcluster = "kmeans",
                      k=4, graph=F)
kmeans_mall$cluster
kmeans_mall$centers
aggregate(data_mall, by=list(gerombol=kmeans_mall$cluster), FUN=mean)

#Visualisasi
fviz_cluster(kmeans_mall)

######Hierarchical Clustering####
#load data
library(factoextra)
data_mall <- read.csv('C:/Users/ASUS/Documents/APG/pt 10/Mall_Customers.csv')
head(data_mall)

#praproses data
data_mall <- data_mall[,c("Age", "Annual.Income", "Spending.Score")]
head(data_mall)

#standarisasi data
data_mall_stdr <- scale(data_mall)
apply(data_mall_stdr,2,mean)
apply(data_mall_stdr,2,sd)

#memilih banyak gerombol
#silhouette
fviz_nbclust(data_mall_stdr, FUNcluster = hcut, method = "silhouette",
             hc_method = "complete", hc_metric = "euclidean")
#wss
fviz_nbclust(data_mall_stdr, FUNcluster = hcut, method = "wss",
             hc_method = "complete", hc_metric = "euclidean")
#dendogram
linkage_methods <- c("complete","average","centroid","ward.D")
hc_mall_dend <- lapply(linkage_methods, function(i)
  hclust(dist(data_mall_stdr,method = 'euclidean'),method = i)
  )
#complete
fviz_dend(hc_mall_dend[[3]])

#menerapkan hierarchical
hc_mall <- eclust(data_mall,stand = TRUE,FUNcluster = "hclust",k=5,hc_method = "complete",hc_metric = "euclidean",graph = F)
hc_mall$cluster

aggregate(data_mall,by =list(gerombol=hc_mall$cluster),
          FUN = mean)

fviz_cluster(hc_mall)
