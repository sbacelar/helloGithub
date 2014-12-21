library(foreign)
# Importar sem labels
joa_nl <- read.spss("reforma3.sav", use.value.labels = FALSE, to.data.frame = TRUE)

# lista de vars para cluster analysis
joa_nl2 <- joa_nl[,c("num", "horas_sono2d", "projFuturod", "IGDEd", "Caminhadasd", "fechCasad",
                     "q1.05bz", "AmbFamiliad", "RelsNetosd", "q6.07bz", "q5.08bz", "AjudFilhosd",
                     "IntActCultsd", "q4.06d", "q4.33b", "q4.13", "IndDespFerLazerd",
                     "q8.13z", "q8.14z", "q6.19z", "RelsReformd", "ConsumoCultTVd", "PossInid", "HISTPROFbz",
                     "q2.11bz", "IndANOMIAd", "IndSATISFACAOd", "q6.17z", "DifAdaptMudd", "q8.06bz",
                     "Voluntariadod", "IndDespSauded", "LeJornal", "LeLivros", "q2.20", "q4.19",
                     "Contact_Vizinhos", "Contact_Telef_Amigos",
                     "Ind_SitTrabPassd", "Ind_AtiForaTrabPassd", "Ind_Sauded", "Ind_AmbSociald",
                     "Escolaridaded", "q2.13", "Rendimentod")]

attach(joa_nl2)
pmatrix <- scale(joa_nl2[,-1])
d <- dist(pmatrix, method="binary")
pfit <- hclust(d, method="ward.D")
plot(pfit, labels=joa_nl2$num)
rect.hclust(pfit, k=6)

# Extracting the clusters

groups <- cutree(pfit, k=6)

print_clusters <- function(labels, k){
  for (i in 1:k){
    print(paste("cluster",i))
    print(joa_nl2[labels==i,1:45])
  }
}
# DESCOMENTAR PARA IMPRIMIR
# print_clusters(groups, 6) 

# Projecting the clusters on the first two principal components
library(ggplot2)
princ <- prcomp(na.omit(pmatrix), center = TRUE, scale = TRUE)
nComp <- 2
project <- predict(princ, newdata=pmatrix)[,1:1:nComp]
project.plus <- cbind(as.data.frame(project),
                      cluster=as.factor(groups),
                      ind=joa_nl2$num)
ggplot(project.plus, aes(x=PC1, y=PC2)) +
  geom_point(aes(shape=cluster)) +
  geom_text(aes(label=num),
            hjust=0, vjust=1)

# Running clusterboot
library(fpc)
kbest.p <- 6
cboot.hclust <- clusterboot(pmatrix, clustermethod=hclustCBI,
                            method="ward.D", k=kbest.p)

# summary(cboot.hclust$result)
# groups <- cboot.hclust$result$partition
# print_clusters(groups, kbest.p)
# cboot.hclust$bootmean
# cboot.hclust$bootbrd  NUMBER OF TIMES EACH CLUSTER WAS DISSOLVED

# example 8.7 of section 8.1.3 
# (example 8.7 of section 8.1.3)  : Unsupervised methods : Cluster analysis : Hierarchical clustering with hclust 
# Title: Calculating total within sum of squares 

sqr_edist <- function(x, y) {               # Note: 1 
  sum((x-y)^2)
}

wss.cluster <- function(clustermat) {     	# Note: 2 
  c0 <- apply(clustermat, 2, FUN=mean)    	# Note: 3 
  sum(apply(clustermat, 1, FUN=function(row){sqr_edist(row,c0)}))     	# Note: 4 
}

wss.total <- function(dmatrix, labels) {                               	# Note: 5 
  wsstot <- 0
  k <- length(unique(labels))
  for(i in 1:k)
    wsstot <- wsstot + wss.cluster(subset(dmatrix, labels==i))         	# Note: 6 
  wsstot
}

# Note 1: 
#   Function to calculate squared distance 
#   between two vectors. 

# Note 2: 
#   Function to calculate the WSS for a single 
#   cluster, which is represented as a matrix (one row 
#   for every point). 

# Note 3: 
#   Calculate the centroid of the cluster (the 
#   mean of all the points). 

# Note 4: 
#   Calculate the squared difference of every 
#   point in the cluster from the centroid, and sum 
#   all the distances. 

# Note 5: 
#   Function to compute the total WSS from a set 
#   of data points and cluster labels. 

# Note 6: 
#   Extract each cluster, calculate the 
#   cluster’s WSS, and sum all the values. 

# example 8.8 of section 8.1.3 
# (example 8.8 of section 8.1.3)  : Unsupervised methods : Cluster analysis : Hierarchical clustering with hclust 
# Title: The Calinski-Harabasz index 

totss <- function(dmatrix) {                   # Note: 1 
  grandmean <- apply(dmatrix, 2, FUN=mean)
  sum(apply(dmatrix, 1, FUN=function(row){sqr_edist(row, grandmean)}))
}


ch_criterion <- function(dmatrix, kmax, method="kmeans") {     	# Note: 2 
  if(!(method %in% c("kmeans", "hclust"))) {
    stop("method must be one of c('kmeans', 'hclust')")
  }
  npts <- dim(dmatrix)[1]  # number of rows.
  
  totss <- totss(dmatrix)                                       	# Note: 3 
  
  wss <- numeric(kmax)
  crit <- numeric(kmax)
  wss[1] <- (npts-1)*sum(apply(dmatrix, 2, var))                	# Note: 4 
  for(k in 2:kmax) {                                           	# Note: 5 
    if(method=="kmeans") {
      clustering<-kmeans(dmatrix, k, nstart=10, iter.max=100)
      wss[k] <- clustering$tot.withinss
    }else {  # hclust                                          	# Note: 6 
      d <- dist(dmatrix, method="euclidean")
      pfit <- hclust(d, method="ward.D")
      labels <- cutree(pfit, k=k)
      wss[k] <- wss.total(dmatrix, labels)
    }
  }
  bss <- totss - wss                                            	# Note: 7 
  crit.num <- bss/(0:(kmax-1))                                  	# Note: 8 
  crit.denom <- wss/(npts - 1:kmax)                             	# Note: 9 
  list(crit = crit.num/crit.denom, wss = wss, totss = totss)   	# Note: 10 
}

# Note 1: 
#   Convenience function to calculate the total 
#   sum of squares. 

# Note 2: 
#   A function to calculate the CH index for a 
#   number of clusters from 1 to kmax. 

# Note 3: 
#   The total sum of squares is independent of 
#   the clustering. 

# Note 4: 
#   Calculate WSS for k=1 (which is really just 
#   total sum of squares). 

# Note 5: 
#   Calculate WSS for k from 2 to kmax. kmeans() 
#   returns the total WSS as one of its 
#   outputs. 

# Note 6: 
#   For hclust(), calculate total WSS by 
#   hand. 

# Note 7: 
#   Calculate BSS for k from 1 to kmax. 

# Note 8: 
#   Normalize BSS by k-1. 

# Note 9: 
#   Normalize WSS by npts - k. 

# Note 10: 
#   Return a vector of CH indices and of WSS for 
#   k from 1 to kmax. Also return total sum of 
#   squares. 

# example 8.9 of section 8.1.3 
# (example 8.9 of section 8.1.3)  : Unsupervised methods : Cluster analysis : Hierarchical clustering with hclust 
# Title: Evaluating clusterings with different numbers of clusters 

library(reshape2)                                           # Note: 1 
# alterei para na.omit(pmatrix) por causa dos NA
clustcrit <- ch_criterion(na.omit(pmatrix), 10, method="hclust")     	# Note: 2 
critframe <- data.frame(k=1:10, ch=scale(clustcrit$crit),   	# Note: 3 
                        wss=scale(clustcrit$wss))
critframe <- melt(critframe, id.vars=c("k"),                	# Note: 4 
                  variable.name="measure",
                  value.name="score")
ggplot(critframe, aes(x=k, y=score, color=measure)) +     	# Note: 5 
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:10, labels=1:10)

# Note 1: 
#   Load the reshape2 package (for the melt() 
#   function). 

# Note 2: 
#   Calculate both criteria for 1–10 
#   clusters. 

# Note 3: 
#   Create a data frame with the number of 
#   clusters, the CH criterion, and the WSS criterion. 
#   We’ll scale both the CH and WSS criteria to 
#   similar ranges so that we can plot them both on 
#   the same graph. 

# Note 4: 
#   Use the melt() function to put the data 
#   frame in a shape suitable for ggplot 

# Note 5: 
#   Plot it. 
