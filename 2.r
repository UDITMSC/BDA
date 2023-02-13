# Load libraries from installed pacakges
library (plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)
#How to set path of an input file in R - set Working Directory
# getwd in r - current working directory
getwd()
# To set working directory use- setwd()
setwd("C:/Users/PRIYANKA/Desktop/Sem 2/BDA Practicals")
grade_input = read.csv('C:/Users/PRIYANKA/Desktop/Sem 2/BDA Practicals/grades_km_input.csv')
str(grade_input)
kmdata = as.matrix(grade_input[,2:4])
str(kmdata)
kmdata[1:5,]
# To find appropriate number of clusters using elbow method- wss
wss = 15
for (k in 1:15)
 wss[k]=sum(kmeans(kmdata,centers=k,nstart=25)$withinss)
wss
plot(
 1:15,
 wss,
 type="b",
 xlab = "No. of clusters",
 ylab = "Withing Sum of Squares")

# Peform KMeans clustering with 3 clusters
km = kmeans(kmdata,3, nstart=25)
km
#visualize the data and assigned clusters
#prepare the student data and clustering results for plotting
df=grade_input[,2:4]

str(df)
km$cluster
df$cluster = factor(km$cluster)
str(df$cluster)
centers = as.data.frame(km$centers)
centers
ggplot(data=df, aes(x=English, y=Math, color=cluster ))+
 geom_point() + theme(legend.position="right")
 
library('factoextra')
library('tidyverse')
library('ggplot2')
fviz_cluster(km,data=kmdata, geom='point') + ggtitle('k=3')

g1= ggplot(data=df, aes(x=English, y=Math, color=cluster )) +
 geom_point() + theme(legend.position="right") +
 geom_point(data=centers,
 aes(x=English,y=Math, color=as.factor(c(1,2,3))),
 size=10, alpha=.3,show.legend = FALSE)
g2 =ggplot(data=df, aes(x=English, y=Science, color=cluster )) +
 geom_point () +
 geom_point(data=centers,
 aes(x=English,y=Science, color=as.factor(c(1,2,3))),
 size=10, alpha=.3,show.legend = FALSE)
g3 = ggplot(data=df, aes(x=Math, y=Science, color=cluster )) +
 geom_point () +
 geom_point(data=centers,
 aes(x=Math,y=Science, color=as.factor(c(1,2,3))),
 size=10, alpha=0.3,show.legend = FALSE)
tmp = ggplot_gtable(ggplot_build(g1))
library('gridExtra')
grid.arrange(g1,g2,g3, nrow=3)
