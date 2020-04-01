library(ggplot2)
library(cluster)
library(dendextend)
View(EastWestAirlines)

# Normalizing continuous columns to bring them under same scale
ndata <-scale(EastWestAirlines[,2:12])

#Hierarchical Clustering Using euclidean distance and ward.D2 linkage
d <- dist(ndata, method = "euclidean") # distancematrix
fit <- hclust(d, method="ward.D2")
fit <- as.dendrogram(fit) 
cd = color_branches(fit,k=3) #dendrogram

#Display Dendrogram
plot(cd)

groups <- cutree(fit, k=3) # cut tree into 3 clusters

table(groups)

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(EastWestAirlines, membership)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

final1

####WE CAN CONCLUDE THAT THE MEMBERS IN CLUSTER 3 HAVE MORE BALANCED MILES,MAX NO. OF NON FLIGHT TRANS
#THE CLUSTER 2 FORMS 2ND BEST PRIORITY FOR THE CUSTOMER,WITH 2ND HIGHEST BALANCE HOURS AND RECIEVED AWARDS AS WELL.
#The LEAST PRIORITY OF CUSTOMERS WERE FROM CLUSTER 1.


##Hierarchical Clustering Using euclidean distance and complete linkage
n <- dist(ndata, method = "euclidean") # distancematrix
fit1 <- hclust(n, method="complete")
fit1 <- as.dendrogram(fit1) 
cd1 = color_branches(fit1,k=4) #dendrogram
#Display Dendrogram
plot(cd1)

groups1 <- cutree(fit1, k=4) # cut tree into 4 clusters
table(groups1)

membership1<-as.matrix(groups1) # groups or cluster numbers
final2 <- data.frame(EastWestAirlines, membership1)

final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]

final3

##Similarly here cluster 4 has more number of balanced miles and the least priority cluster is cluster number1
