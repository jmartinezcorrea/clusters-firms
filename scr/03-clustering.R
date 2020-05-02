
# intro -------------------------------------------------------------------
source("scr/libraries.R")


# optimum K ---------------------------------------------------------------
# https://rpubs.com/Nitika/kmeans_Iris
# https://rpubs.com/Joaquin_AR/310338

set.seed(4567) 
df.new = readRDS(file="data/working/df.new.rds")


# Elbow method
fviz_nbclust(df.new, FUNcluster = kmeans, method = "wss", diss = dist(df.new, method = "manhattan")) +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df.new, FUNcluster = kmeans, method = "silhouette", diss = dist(df.new, method = "manhattan"))+
  labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(123123)
fviz_nbclust(df.new, FUNcluster = kmeans,  method = "gap_stat", k.max=15, nstart = 50, nboot = 500, diss = dist(df.new, method = "manhattan"), print.summary = TRUE)+
  labs(subtitle = "Gap statistic method") 

fviz_nbclust(df.new, FUNcluster = pam,  method = "gap_stat", nboot = 500, diss = dist(df.new, method = "manhattan"), print.summary = TRUE)+
  labs(subtitle = "Gap statistic method") 

gap_stat <- clusGap(df.new, FUN = kmeans, nstart = 30, K.max = 15, B = 500)
print(gap_stat, method = "Tibs2001SEmax")
fviz_gap_stat(gap_stat, linecolor = "steelblue", maxSE = list(method ="Tibs2001SEmax", SE.factor = 1))


## NbClust() function: 30 indices for choosing the best number of clusters

#NbClust(data = NULL, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = NULL)
#data: matrix
#diss: dissimilarity matrix to be used. By default, diss=NULL, but if it is replaced by a dissimilarity matrix, distance should be “NULL”
#distance: the distance measure to be used to compute the dissimilarity matrix. Possible values include “euclidean”, “manhattan” or “NULL”.
#min.nc, max.nc: minimal and maximal number of clusters, respectively
#method: The cluster analysis method to be used including “ward.D”, “ward.D2”, “single”, “complete”, “average”, “kmeans” and more.0

nb <- NbClust(df.new, distance = "manhattan", min.nc = 2,
              max.nc = 15, method = "kmeans", index="all")
fviz_nbclust(nb) +
  ggtitle("Número óptimo de clusters - K=2") + xlab("Número de clusters K") + ylab("Frecuencia entre todos los índices")

# CALINSKY CRITERION (solo para k-means)
cal_fit <- cascadeKM(df.new, 1, 10, iter = 1000)
plot(cal_fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best2 <- as.numeric(which.max(cal_fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best2, "\n")

CH <- as.data.frame(cal_fit$results[2,])
write.csv(CH, paste0(resultados.dir,"tabla-CH.csv"))

nb.ch <- NbClust(df.new, distance = "manhattan", min.nc = 2,
                 max.nc = 15, method = "kmeans", index="ch")
CH2 <- as.data.frame(nb$All.index)
write.csv(CH2, paste0(resultados.dir,"tabla-CH2.csv"))


# BAYESIAN INFORMATION CRITERION (BIC) FOR EXPECTATION - MAXIMIZATION; vbles continuas (?)
d_clust2 <- Mclust(as.matrix(df.new), G=1:20)
m.best2 <- dim(d_clust2$z)[2]
cat("model-based optimal number of clusters:", m.best2, "\n")
plot(d_clust2)

means<-d_clust2$parameters$mean
fviz_mclust(d_clust2, "uncertainty", palette = "jco")


# clustering  -------------------------------------------------------------
#Kmeans{amap} algorithm
K<- Kmeans(df.new,centers=2, method="manhattan", iter.max= 500, nstart = 50) 
table(K$cluster)
cent <- t(K$centers)
write.csv(as.data.frame(cent), paste0(resultados.dir,"tabla-cent.csv"))

wss <- sum(K$withinss)

fviz_cluster(object = K, data = df.new, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  theme_bw() +
  theme(legend.position = "none")

fviz_cluster(object = K, data = df.new,  geom = "point",  main = "")

#k-mediods (pam)
set.seed(123)
pam_clusters <- pam(x = df.new, k = 2, metric = "manhattan")
pam_clusters
modoids <- pam_clusters$medoids
table(pam_clusters$clustering)

fviz_cluster(object = pam_clusters, data = df.new, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  theme(legend.position = "none")


medoids <- prcomp(df.new)$x
# Se seleccionan únicamente las proyecciones de las observaciones que son medoids
medoids <- medoids[rownames(pam_clusters$medoids), c("PC1", "PC2")]
medoids <- as.data.frame(medoids)
# Se emplean los mismos nombres que en el objeto ggplot
colnames(medoids) <- c("x", "y")

# Creación del gráfico
fviz_cluster(object = pam_clusters, data = df.new, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  # Se resaltan las observaciones que actúan como medoids
  geom_point(data = medoids, color = "firebrick", size = 2) +
  theme(legend.position = "none")



# cluster validation ------------------------------------------------------

library(Hotelling)
library(corpcor)
library(clustertend)
hot <- hotelling.test(df.new[K$cluster==1,],df.new[K$cluster==2,], shrinkage = FALSE,perm = FALSE,B = 500)
hot 

### Tendencia de agrupamiento por cluster
# -Hopkins estadística: si el valor de Hopkins estadística está cerca de cero (muy por debajo de 0,5), 
#entonces podemos concluir que el conjunto de datos es significativamente clusterable.
# -VAT (evaluación visual de la tendencia de agrupamiento): la cubeta detecta la tendencia de
#agrupamiento en una forma visual contando el número de bloques oscuros (o coloreados) de forma cuadrada a lo largo de la diagonal en una imagen de cuba.

get_clust_tendency(df.new, n = nrow(df.new)-1,
                   gradient = list(low = "steelblue",  high = "white"))


dist_data <- dist(df.new, method = "manhattan")
ggplot <- fviz_dist(dist.obj = dist_data, show_labels = FALSE) +
  labs(title = "Datos iris") + theme(legend.position = "bottom")

set.seed(4567)
hopkins(data = df.new, n = nrow(df.new) - 1)


## Comparacion de algoritmos de clustering
library(clValid)
datos <- as.matrix(df.new)

val.test <- clValid(obj = datos, nClust = 2:10, metric = "manhattan",
                    clMethods = c("kmeans", "hierachical"),
                    validation = c("stability", "internal"))
summary(val.test)
write.csv(optimalScores(val.test), paste0(resultados.dir,"clvalid-optscores.csv"))

#Recuerde que la conectividad debe ser minimizada, mientras tanto el índice de dunn y el ancho de la silueta debe ser maximizada.


## Clusterwise cluster stability assessment by resampling 
library(gclus)
library(fpc)
set.seed(20)
km.boot2 <- clusterboot(df.new, B=100, bootmethod="boot",
                        clustermethod=kmeansCBI,
                        krange=2, seed=20)

km.boot3 <- clusterboot(df.new, B=100, bootmethod="boot",
                        clustermethod=kmeansCBI,
                        krange=3, seed=20)

km.boot4 <- clusterboot(df.new, B=100, bootmethod="boot",
                        clustermethod=kmeansCBI,
                        krange=4, seed=20)


print(km.boot2)
print(km.boot3)
print(km.boot4)

#Clusterwise Jaccard bootstrap mean should be maximised
#number of dissolved clusters should be minimised and
#number of recovered clusters should be maximised and as close to the number of pre-defined bootstraps as possible


# save data ---------------------------------------------------------------

df.val <- data.frame(df.oup,K$cluster)
saveRDS(df.val,"data/working/df.val.rds")

df <- data.frame(df.orig,K$cluster)
saveRDS(df, "data/wordking/df.rf.rds")

