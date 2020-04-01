#Load the data
View(EastWestAirlines)

#LOad Packages
install.packages("plyr")
install.packages("animation")
library(plyr)
library(animation)

mydata <- EastWestAirlines
a <-scale(mydata[,2:12])

#Scree plot
wss<-c()
for(i in 2:12) wss[i]<-sum(kmeans(a,centers =i)$withinss)
plot(1:12,wss,type = "b",xlab = "No. of clusters",ylab="Sum of Squares")

#No.of clusters 3
b <- kmeans(a,3)
final <-data.frame(mydata,b$cluster)
aggregate(mydata[,2:12],by = list(b$cluster),FUN = mean)
View(final)

#Let us view the clusters using animation package
kn <- kmeans.ani(a,3)

#For 4 Clusters
c <- kmeans(a,4)
final1 <-data.frame(mydata,c$cluster)
aggregate(mydata[,2:12],by = list(c$cluster),FUN = mean)
View(final1)

#Let us view the clusters using animation package
km <- kmeans.ani(a,4)

#For 5 Clusters
d <- kmeans(a,5)
final2 <-data.frame(mydata,d$cluster)
aggregate(mydata[,2:12],by = list(d$cluster),FUN = mean)
View(final2)

#Let us view the clusters using animation package
ka <- kmeans.ani(a,5)
