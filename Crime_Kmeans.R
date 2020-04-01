install.packages("plyr")
install.packages("animation")

library(plyr)
library(animation)

View(crime_data)
mydata <- crime_data
str(mydata)
a <- scale(mydata[,2:5])

#Scree plot
wss<-c()
for(i in 2:15) wss[i]<-sum(kmeans(a,centers =i)$withinss)
plot(1:15,wss,type = "b",xlab = "No. of clusters",ylab="Sum of Squares")

#No.of clusters 4
b <- kmeans(a,4)
final <-data.frame(mydata,b$cluster)
View(final)

#Let us view the clusters using animation package
km <- kmeans.ani(a,4)

#For Number of Clusters = 3
c <- kmeans(a,3)
final1 <-data.frame(mydata,c$cluster)
View(final1)

#Let us view the clusters using animation package
kn <- kmeans.ani(a,3)

#For number of clusters= 5
d <- kmeans(a,5)
final2 <-data.frame(mydata,d$cluster)
View(final2)

#Let us view the clusters using animation package
ko <- kmeans.ani(a,5)

#Here we can conclude that cluster with more no. of murders are threat to live
#The second cluster has maximum no. of Assault and Rape Cases
#The Other two clusters would take rankings based on the remaining factors that is the Urban Pop metrics.
