#' ---
#' title: "Clustering Project for customer online retail"
#' author: "Hamed Ahmed Hamed Ahmed"
#'
#' ---

' 
Project: "Clustering Project for customer online retail"
Name: "Hamed Ahmed Hamed Ahmed"
StudentId: "454827"

Problem Statement: 
"A Company wants to increase their annual profit by selecting suitable customers
  who had better transactions and got good Revenue"

Dataset:
"Online retail is a transnational data set which contains all the transactions
 occurring between **01/12/2010 and 09/12/2011** for a UK-based and registered non-store online retail.
 The company mainly sells unique all-occasion gifts. Many customers of the company are wholesalers."
'

'so We are going to analysis the Customers based on below 3 factors:'
'R (Recency): Number of days since last purchase'
'F (Frequency): Number of tracsactions'
'M (Monetary): Total amount of transactions (revenue contributed)'


# changing the language to English
Sys.setlocale("LC_ALL","English")
Sys.setenv(LANGUAGE='en')

#load packages for data manipulation
library(tidyverse, quietly = TRUE)
library(lubridate)
library(stats)
library(factoextra)
library(flexclust)
library(fpc)
library(clustertend)
library(cluster)
library(ClusterR)
library("PerformanceAnalytics")
library(corrplot)
library(rmarkdown)
library(scales)

# Load data
myRetailData <- read_csv("OnlineRetail.csv")


# Observing Datatypes, Columns and Rows¶

## View the rows
head(myRetailData)

## Datatypes and columns
str(myRetailData)

## Summary for the data
summary(myRetailData)

## Dataset dimension 
dim(myRetailData)

# Data Cleaning and Preparing

# As we can see we have 135080 in CustomerId and 1454 in Description missing values 
colSums((is.na(myRetailData)))

# Start remove these missing values
myRetailDataCleaned <- na.omit(myRetailData)

# Now check again 
colSums((is.na(myRetailDataCleaned)))

#convert the invoiceDate to datetime
myRetailDataCleaned$InvoiceDate = as_datetime(myRetailDataCleaned$InvoiceDate, format="%d-%m-%Y %H:%M")

# Check the dimension
dim(myRetailDataCleaned)

# Data Head
head(myRetailDataCleaned)

# As we can see our data now is cleaned from the null value and we changed the InvoiceDate column format from char to datetime
#And Since we will work on FRM for the dataset we need to check the outlier for the following columns
#Let's check for outliers on the following colums [UnitPrice,InvoiceDate,Quantity]


#Checking for outlires before scaling
summary(myRetailDataCleaned)

'Actually from the summary i can see that we have outlier in Quantity and UnitPrice'
'Let\'s first visualize these 3 columns '

# As we can see the data for the unitPrice column is right skewed and contain outliers
hist(myRetailDataCleaned$UnitPrice, freq = T) 
boxplot(myRetailDataCleaned$UnitPrice)

# As we can see the data for the Quantity column contain outliers
hist(myRetailDataCleaned$Quantity, freq = T) 
boxplot(myRetailDataCleaned$Quantity)

# for date it contain some outliers but not high as much the others columns
hist(as.numeric(myRetailDataCleaned$InvoiceDate) , freq = T) 
boxplot(as.numeric(myRetailDataCleaned$InvoiceDate))


# Removing outliers 
'After viewing the columns i found out that the best option to handle these outliers by taking a subset of the data'
'that contain the most distruted values'

#Take subset 
myRetailDataCleaned <- myRetailDataCleaned[myRetailDataCleaned$UnitPrice <5  & myRetailDataCleaned$Quantity <= 12 & myRetailDataCleaned$Quantity >= 0 , ]

#check the summary of the data after removing the outliers

summary(myRetailDataCleaned)

#check the dimension of the data after removing the outliers
dim(myRetailDataCleaned)

# Visualize the columns after removing the outliers
#After removing the outlier 
hist(myRetailDataCleaned$UnitPrice, freq = T) 
boxplot(myRetailDataCleaned$UnitPrice)


hist(myRetailDataCleaned$Quantity, freq = T) 
boxplot(myRetailDataCleaned$Quantity)

hist(as.numeric(myRetailDataCleaned$InvoiceDate) , freq = T) 
boxplot(as.numeric(myRetailDataCleaned$InvoiceDate))



# we will create data frame based on customer Recency, Frequency and customer Spending

#Customer Spending
customerSpending <- myRetailDataCleaned %>% group_by(CustomerID) %>% summarize(Spending =  sum((UnitPrice * Quantity)) )
# frequency of the customer 
customerFrequency <- myRetailDataCleaned %>% group_by(CustomerID) %>% summarize(Frequency = n())
# recency of the customer 
recency <- myRetailDataCleaned %>% group_by(CustomerID)%>% summarise(Recency = as.numeric(difftime(max(myRetailDataCleaned$InvoiceDate),max(InvoiceDate)), units = "days") )

#Merge the dataFrame 
merged_df <- merge(customerSpending,customerFrequency,  by = "CustomerID", all.x = TRUE, all.y = TRUE)
merged_df <- merge(merged_df,recency,  by = "CustomerID", all.x = TRUE, all.y = TRUE)

# summary and dataset dimension
dim(merged_df)
summary(merged_df)

# let's visualize our Data 
hist(merged_df$Spending, freq = T) 
boxplot(merged_df$Spending) #0.1

hist(merged_df$Frequency, freq = T) 
boxplot(merged_df$Frequency)  # 0.15

hist(merged_df$Recency, freq = T) 
boxplot(merged_df$Recency)  #0.4

# It seems that our data is highly skewed , concentrated in one area  and also contain outliers

# Let's applu scaling and check again 
#let's scale the data to have a better overview of the outliers and spread the data

##scaling ===>
data_scaled <- merged_df %>% mutate_all(~ rescale(., to = c(0, 1)))


# after scaling
hist(data_scaled$Spending, freq = T) 
boxplot(data_scaled$Spending) #0.1

hist(data_scaled$Frequency, freq = T) 
boxplot(data_scaled$Frequency)  # 0.15

hist(data_scaled$Recency, freq = T) 
boxplot(data_scaled$Recency)  #0.4


# Same the data still skewed , concentrated in one area  and also contain outliers

# so Let's take a subset of the data based on the graph most of  data is distributed around values
data_scaled <- data_scaled[data_scaled$Spending <0.1  & data_scaled$Frequency <= 0.1 & data_scaled$Recency <= 0.4 , ]


# after removing the outliers 
#We still got some outliers and skewed distrubtion but it's much better than before 
hist(data_scaled$Spending, freq = T) 
boxplot(data_scaled$Spending) #0.2

hist(data_scaled$Frequency, freq = T) 
boxplot(data_scaled$Frequency)  # 0.2

hist(data_scaled$Recency, freq = T) 
boxplot(data_scaled$Recency)  #0.4


# checking the summary and dimension of the data 
summary(data_scaled)
dim(data_scaled)

# Let's Apply clustering but first we need to check how many clusters our data can fit 

# Elbow method
opt<-Optimal_Clusters_KMeans(data_scaled, max_clusters=10, plot_clusters = TRUE)

'It seems that our data can be in 3-4 clusters'


# check by using Silhouette width
silhouette_score = function(k){
  km = kmeans(data_scaled, centers = k, nstart=25)
  ss = silhouette(km$cluster, dist(data_scaled))
  mean(ss[, 3])}
k = 2:10
avg_sil = sapply(k,silhouette_score)
plot(k, type = 'b', avg_sil, xlab = 'number of clusters', ylab ='average silhouette scores', frame = 'False')

'It seems that our data can be in 4 clusters'


# Gap statistics
library("factoextra")
fviz_nbclust(data_scaled, kmeans, method = "gap_stat")

'It seems that our data can be in 4 clusters'

# So let's try to cluster our dataset by 3 clusters 
clusterNo = 3
# I will use Kmean and clara

# Convert the data frame into a matrix
data <- as.matrix(data_scaled[, c("Spending", "Frequency", "Recency")])

# Kmean 
cluster_3 <- kmeans(data,centers = clusterNo,nstart = 10)
cluster_3$cluster <- as.factor(cluster_3$cluster)


# Clustering in different graph 
fviz_cluster(list(data=data, cluster=cluster_3$cluster), 
             ellipse.type="norm", geom="point", stand=FALSE, palette="jco", ggtheme=theme_classic())

# Boxblot
groupBWplot(data, cluster_3$cluster, alpha=0.05)


# Try out with Clara 3 clusters  
clara_flex<-eclust(data, "clara", k=clusterNo) 
summary(clara_flex)
fviz_cluster(clara_flex)
fviz_silhouette(clara_flex)
clara_labels = data.frame(clara_flex$clustering)
names(clara_labels)[1] = "Cluster"


# Analysis based on the clustering 

'Based on silhouette width for 3 cluster we got 45% for kmeans and 58% for Clara 
 so it\'s better for this dataset to use Clara for clustering '

'Bind the cluster labels of clara with the dataset'
data_scaled_labeled = cbind(data_scaled,clara_labels)

'Check the data rows'
head(data_scaled_labeled)

'Visualizing each value of RFM on boxplot for analysis'
boxplot(data_scaled_labeled$Recency ~ data_scaled_labeled$Cluster,
        names = c("Cluster1", "Cluster2","Cluster3"),
        xlab = "Clusters", ylab = "Recency", 
        main = "Visualizing Customer Recency")

boxplot(data_scaled_labeled$Frequency ~ data_scaled_labeled$Cluster,
        names = c("Cluster1", "Cluster2","Cluster3"),
        xlab = "Clusters", ylab = "Frequency", 
        main = "Visualizing Customer Frequency")

boxplot(data_scaled_labeled$Spending ~ data_scaled_labeled$Cluster,
        names = c("Cluster1", "Cluster2","Cluster3"),
        xlab = "Clusters", ylab = "Monetary", 
        main = "Visualizing Customer Monetary")


#Based on Clara Clustering 
'
Let\'s discuss each of the customer segments in more detail:
  
1- Highly Engaged and Valuable Customers (Cluster 1)
This segment is comprised of customers who have recently made many purchases and with high monetary value.
These customers are highly engaged with the brand and are likely to continue making purchases in the future.
Businesses should focus on retaining these customers and maintaining their loyalty. This can be achieved through personalized promotions, exclusive offers, and by providing excellent customer service.
Consider implementing a loyalty program or VIP program for this segment to further incentivize repeat purchases.
This segment is a prime target for upselling and cross-selling opportunities. Offer these customers complementary products or services to enhance their experience with your brand.

2- Less Engaged and Moderate Purchasing Power Customers (Cluster 2)
This segment is comprised of customers who have a moderate level of recent purchases and monetary value.
These customers may be less engaged with the brand and may not be making purchases as frequently as the first cluster.
To increase engagement and drive repeat purchases, businesses should focus on providing targeted and relevant promotions, offers, and advertisements.
Consider gathering more information on these customers, such as their preferences and purchasing history, to create a more personalized experience.
This segment may also be open to trying new products or services, so businesses can use this as an opportunity to introduce new offerings and expand their customer base.

3- Less Engaged and Less Valuable Customers (Cluster 3)
This segment is comprised of customers who have made few purchases in recent times and with low monetary value.
These customers are less engaged and are not as valuable to the business as the other two clusters.
To increase engagement and drive repeat purchases, businesses can offer special promotions or discounts to this segment.
Consider gathering more information on these customers to determine what may be causing their lack of engagement and address any pain points.
This segment may also be a good target for re-engagement campaigns, such as sending a follow-up email or a special offer to encourage them to make a purchase.

By understanding and targeting each customer segment based on their 
recency, frequency, and monetary value, businesses can improve customer engagement and retention,
and optimize their spending on customer acquisition and retention.

'


