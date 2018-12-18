# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))
install.packages(c("cluster", "rattle.data", "NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function [rows, columns]
wine1 <- scale(wine[,-1])
head(wine1)
# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){ 
	              wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)} 
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters", 
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine1) 

# Exercise 2:
#   * How many clusters does this method suggest?

## Looking at the chart.  There is significant drop in the chart going from 1-3 number of clusters vs group sum of squares.  
## The drop starts to diminish after the third cluster which indiates 3 clusters would be suggested. (Elbow Method)

#   * Why does this method work? What's the intuition behind it?

## K means cluster or elbow method is to define clusters such total within sum of square is minimized.  Within cluster SS measures compactness
## of clustering and this is best if it's small.   The Elbow method then looks at the total withing group sum of squares as a function of cluster.
## Then we look at the number of clusters so adding another cluster for example after the 4th cluster there is a huge dimishing drop and won't
## improve much better the total within group sum of square.  


#   * Look at the code for wssplot() and figure out how it works

#wssplot <- function(data, nc=15, seed=1234){   ### creating the function, with the data set loaded, number of clusters set to 15 and random seed generator set to 1234
 # wss <- (nrow(data)-1)*sum(apply(data,2,var)) ###(gives total sum of squares) within cluster sum of squares is set to the number of rows in the data -1 multiplied by the sum of the data, the apply goes through the data, margin = 2(representing the columns) and the function as the variance 
  #for (i in 2:nc){ ### creating the for loop for in in 2 ranging for number of clusters to compute total number of sum of squares 
    #set.seed(seed) #sets seed to 1234 
    #wss[i] <- sum(kmeans(data, centers=i)$withinss)} ###goal is to set number of sum of squres as small as possible, so would run kmeans multiple times
    ###raising the number of clusters each time.
    ###would compare the withiness each time, stopping when rate of improvement drops off.   Find low withiness and keep number of clusters low. 
    
  
  #plot(1:nc, wss, type="b", xlab="Number of Clusters", #plots number of clusters on x axis and within sum of squares on the y axis and giving them a x and y label.  The type is the type of plot to be drawn "b" for both so points and line
       #ylab="Within groups sum of squares")
#}

#wssplot(wine1)  ### this calls the plot on our wine1 data 

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine1, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

## looking at the number of clusters chosen by 26 criteria.  The higest number of criteria is 15 which recommends using 3 clusters. 

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

# fit.km <- kmeans( ... )

set.seed(1234) #
fit.km <- kmeans(wine1, centers=3,  nstart=25) #
fit.km$size #

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. 

#Would you consider this a good clustering?

## 59 + 65 + 48 / 59 + 65 + 48 + 3 + 3.  172/178 = 0.9663 Therefore this is a good clustering.  Confusion table evaluates how well k-means analysis performed. 

table(fit.km$cluster,wine$Type) #

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

## Library clusters allow us to represent (with the aid of PCA) the cluster solution into 2 dimensions.  55.41 of the multivariate data is captured by components 1 and 2
## If a thrid component were added it might explain more of the vairability.  

#clusplot( ... )
library(cluster) #calling library(cluster)
clusplot(wine1, fit.km$cluster, color = TRUE) #
