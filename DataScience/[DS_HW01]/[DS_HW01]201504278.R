#201504278_강천성
#Make DataSets 1,2
dataSet1 = cbind(rnorm(50,1,1), rnorm(50,-1,1))
dataSet2 = cbind(rnorm(50,10,1), rnorm(50,2,1.5))
#Make DataSet3 by RowBinding DataSets
dataSet3 = rbind(dataSet1,dataSet2)
#Plotting DataSet3
plot(dataSet3,xlab = 'x', ylab='y')
#Make 2-Cluster using kmeans Algorithm
kc = kmeans(dataSet3,2)
#Plotting 2-means Cluster Result

plot(dataSet3,xlab='x',ylab='y', col=kc$cluster)
#Make 5-Cluster using kmeans Algorithm
kc2 = kmeans(dataSet3,5)
#Plotting 5-means Cluster Result
plot(dataSet3,xlab='x',ylab='y', col=kc2$cluster)

#Calculate each element's distance 
d = dist(dataSet3)
#Make Hierarchical Cluster using complete dissimilarity Method
hr = hclust(1/d, method = 'complete', members=NULL)


#Plotting Hierachical Cluster
plot(hr,hand = 0.1)

#This code Plotting horizon form
#plot(as.dendrogram(hr), edgePar=list(col=3, lwd=4), horiz=T)

#Make cutting block
mycl = cutree(hr, h=max(hr$height)/2)
mycl[hr$labels[hr$order]]

#Make cutting for 2-Cluster
rect.hclust(hr, k=2, border='red')

#Make cutting for 5-Cluster
rect.hclust(hr, k=5, border='blue')

