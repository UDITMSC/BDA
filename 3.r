#Clustering in the utlities dataset
utilities = read.csv("C:/Users/PRIYANKA/Desktop/Sem 2/BDA Practicals/utilities.csv")
str(utilities)
utilities

# all possible plots
pairs(utilities[,2:8])

# scatterplot of Fuel_cost vs Sales
plot(Fuel_Cost ~ Sales,utilities)
with(utilities,text(Fuel_Cost ~ Sales,labels=Company, pos=1,cex=0.7))

utilities
# Normalization - Avg = 0 & std = 1
z=utilities[,2:9]
z
# Calculate mean and Standard dev for entire data set columnwise
m=apply(z,2,mean)
s=apply(z,2,sd)
m
s
z=scale(z,m,s)
#calculating euclidean distance
distance = dist(z)
print(distance,digits=3)

# Hierarchical agglomerative clustering
# Cluster dendogram with complete linkage
hc.c=hclust(distance)
plot(hc.c)
plot(hc.c,labels = utilities$Company)
plot(hc.c,hang=-1)
# Cluster dendogram with average linkage
hc.a=hclust(distance, method = "average")
plot(hc.a)
plot(hc.a,labels = utilities$Company)
plot(hc.a,hang=-1)
# cluster membership
member.c=cutree(hc.c,3)
member.c
member.a=cutree(hc.a,3)
member.a
table(member.c, member.a)
# cluster means
aggregate(z, list(member.c), mean)
aggregate(utilities[,-c(1,1)], list(member.c), mean)

- # Silhoutte Plot
library(cluster)
plot(silhouette(cutree(hc.c,3),distance))
#Scree Plot
wss <- numeric(20)
for (k in 1:20)
 wss[k]=sum(kmeans(z,centers=k, nstart=25 ) $withinss )
wss
plot(1 : 20, wss, type="b", xlab="Number of Clusters " ,
 ylab="Within Sum of Squares" )
kc = kmeans(z,3)
kc
member.a
member.c
kc$cluster

plot(Sales ~ D.Demand, utilities)
plot(Sales ~ D.Demand, utilities, col =kc$cluster)
