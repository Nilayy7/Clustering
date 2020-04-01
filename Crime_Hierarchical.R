#Load the Data
mydata2<-crime_data

#######Scale The Data

mydata <-scale(mydata2[,2,5])

#Hierarchical Clustering Using euclidean distance and average linkage

n <- dist(mydata,method = "euclidean") #computing distance matrix
FIT<-hclust(n,method = "average") ##building the algo with average
plot(FIT)  #display dendrogram
Groups <- cutree(FIT,k=6)  #cut tree into 6 clusters
rect.hclust(FIT,k=6,border ="red") #draw dendrogram with red borders around 6 clusters
table(Groups)

#attach the cluster numbers to State
clust= data.frame('State'=mydata2[,1],'Cluster'=Groups)
View(clust)

CrimeRateCategory2 <- as.matrix(Groups) #Group or cluster numbers
g <- data.frame(crime_data,CrimeRateCategory2)

g1 <-g[,c(ncol(g),1:(ncol(g)-1))]
View(g1)
aggregate(crime_data[,-1],by=list(g$CrimeRateCategory),mean)


#Hierarchical Clustering using manhattan distance and centroid linkage

n1<-dist(mydata,method = "manhattan")
fit<- hclust(n1,method = "centroid")
plot(fit)
Groups1 <- cutree(fit,k=5)  #cut tree into 5 clusters
rect.hclust(fit,k=5,border ="blue")
table(Groups1)

#attach the cluster numbers to State
clust1= data.frame('State'=mydata2[,1],'Cluster'=Groups1)
View(clust1)



CrimeRateCategory1 <- as.matrix(Groups1) #Group or cluster numbers
f1 <- data.frame(crime_data,CrimeRateCategory1)

f2 <-f1[,c(ncol(f1),1:(ncol(f1)-1))]
View(f2)
aggregate(crime_data[,-1],by=list(f1$CrimeRateCategory),mean)


##Hierarchical Clustering Using euclidean distance and complete linkage

n2<-dist(mydata,method = "euclidean")
fit1 <- hclust(n2,method = "complete")
plot(fit1)
grp <- cutree(fit1,k=4) #cut tree into 4 clusters
rect.hclust(fit1,k=4,border ="green")
table(grp)

#attach the cluster numbers to State
clust2= data.frame('State'=mydata2[,1],'Cluster'=grp)
View(clust2)

CrimeRateCategory <- as.matrix(grp) #Group or cluster numbers
f <- data.frame(crime_data,CrimeRateCategory)

f1 <-f[,c(ncol(f),1:(ncol(f)-1))]
View(f1)
aggregate(crime_data[,-1],by=list(f$CrimeRateCategory),mean)

#Inference
#Cluster 1 has Maximum number of Rape,with other Assault onn top of the list.
#Cluster 2 has maximum number of Urban Pop and Assault
#Cluster 3 Has maximum murder and Assault 
#Cluster 4 has got the less vulnerable to all the Crime categories.