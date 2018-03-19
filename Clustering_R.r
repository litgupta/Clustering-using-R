#Clustering using R

#Step 1: Loading the data
data=read.csv('~/Desktop/coding/learning_R/DATA/Mall_Customers.csv')#We need to provide the absolute path in R
head(data)


#Step2 : Data Cleaning
#here I am selecting only the estimated salary column 
data=data[3:4]
#Step 2.1- Removing the NA values and changing categorical data into continuous
  

#creating a function to automatically perform this task
clean<-function(data){
  cols=colnames(data)
  for(col in cols){
    if(any(is.na(data[,col]))){
      if(class(data[,col])%in% c('numerical','integer')){
        data[,col]=ifelse(is.na(data[,col]),mean(data[,col],na.rm=TRUE),data[,col]) #replaces the NA value with mean value 
      }else{
        data[,col]=ifelse(is.na(data[,col]),'none',data[,col]) #replaces the NA value with mean value 
      }
    }else if(class(data[,col])=='factor'){
      data[,col]=factor(data[,col],levels=unique(data[,col]),labels=1:nlevels(data[,col]))
      #data[,col]=as.integer(data[,col])
    }
    
  }
return(data)
  }
data=clean(data)

#Step 2.2 splitting the data into train and test sets

library(caTools)
result=sample.split(data,SplitRatio = 0.8) #its a good practice to provide 80% weightage to training set
train_set=data[result==TRUE,]
test_set=data[result==FALSE,]

#Step 3 : clustering the data
#R provides in-build functions for clustering the data
clusters=kmeans(data,centers = 12)

#Step 4: Plotting the clustered data
#The best way to see how the algorithm has clustered the data is by plotting it, and R specifically provides library 
#for plotting clustered data
library(cluster)
clusplot(data,clusters$cluster,color=TRUE,lines = FALSE,main = 'Salary v/s Expenditure',xlab = 'Salary',ylab = 'Expenditure')

#Step 5: looking for alternative approaches to the solution

#One way could be to look for the WCSS (within cluster sum of squares value)
#In kmeans , main aim is to reduce the intra-cluster distance and increase the inter-cluster space
#The wcss value is the sum of squared differences of each positional point in a cluster with its centroid
#Intuitively, the higher the number of clusters, lesser the wcss value.
#So , lets move forward and quickly make a wcss value graph to determine the "most-optimum" 'k' value for clustering


wcss_values=vector() #initiliasing an empty vector
#running a loop for 10 repetitions
for ( i in 1:10){
  wcss_values[i]=sum(kmeans(data,centers = i)$withinss)
}

plot(wcss_values,type='b')

#Now as we can see, the rate of depreciation slows down at 5, hence the optimum 'K' value should be 5.
#once again, clustering the data using k=5
clusters=kmeans(data,centers = 5)
library(cluster)
clusplot(data,clusters$cluster,color=TRUE,shade=TRUE,lines = FALSE,main = 'Salary v/s Expenditure',xlab = 'Salary',ylab = 'Expenditure')

