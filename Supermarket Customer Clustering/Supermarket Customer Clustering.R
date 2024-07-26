rm(list=ls())
cat("\014")
getwd()
setwd()
Sales<- read.csv("supermarket_sales.csv")
Branch<-as.factor(Sales$Branch)
City<-as.factor(Sales$City)
Customer<-as.factor(Sales$Customer.type)
Gender<-as.factor(Sales$Gender)
ProductLine<-as.factor(Sales$Product.line)
Payment<-as.factor(Sales$Payment)
Sales.norm <- data.frame(sapply(Sales[, c(7,8,10,16,17)], scale))
Sales$Customer1 <- ifelse(Sales$Customer.type == "Member", 0, ifelse(Sales$Customer.type == "Normal", 1, NA))
Sales$Gender1 <- ifelse(Sales$Gender == "Male", 0, ifelse(Sales$Gender == "Female", 1, NA))
choosek <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(choosek) <- c("numClusters", "totWithinSS", "avg_silhouette")
library(cluster)
for (k in 1:10) {
  set.seed(123)
  tempkm <- kmeans(Sales.norm,  centers = k, nstart = 10)
  
  
  if (k==1) {
    ss <- 0
  } else {
   
    ss <- silhouette(tempkm$cluster, dist(Sales.norm))[, 3]
  }
  
 
  tempdf <- data.frame(numClusters = k, totWithinSS = tempkm$tot.withinss, avg_silhouette = mean(ss))
  choosek <- rbind(choosek, tempdf)
}
library(ggplot2)
# elbow plot
g <- ggplot(choosek, aes(numClusters, totWithinSS))
g <- g + geom_line() + geom_point() + labs(x="Number of Clusters (k)", y="Total Within-Cluster Squared Distance") 
g
g + geom_text(aes(label=round(totWithinSS, 2)), vjust=-0.3) + scale_x_continuous(breaks = seq(1:10))
# Silhouette
g <- ggplot(choosek, aes(numClusters, avg_silhouette))
g <- g + geom_line() + geom_point() + labs(x="Number of Clusters (k)", y="Average Silhouette")
g
g + geom_text(aes(label=round(avg_silhouette, 3)), vjust=-0.3) + scale_x_continuous(breaks = seq(1:10))
#Kmeans
set.seed(123)
clus.out <- kmeans(Sales.norm,  centers = 2, nstart = 10)
Sales$cluster_km=clus.out$cluster


table(Sales$cluster_km)
Sales$cluster_km <- as.factor(Sales$cluster_km)



# visualize the cluster
# create box plot: by cluster

survey <- Sales[,c(7,8,10,16,17:20)]
library(ggplot2)
g <-ggplot(survey, aes(x = factor(cluster_km)))
g+geom_boxplot(aes(y = Unit.price), position = "dodge") 
    
g <-ggplot(survey, aes(x = factor(cluster_km))) 
g +  geom_boxplot(aes(y = Quantity), position = "dodge") 
  
g <-ggplot(survey, aes(x = factor(cluster_km))) 
g +  geom_boxplot(aes(y = Total), position = "dodge")
  
g <-ggplot(survey, aes(x = factor(cluster_km))) 
g +  geom_boxplot(aes(y = gross.income), position = "dodge")
  
g <-ggplot(survey, aes(x = factor(cluster_km))) 
g +  geom_boxplot(aes(y = Rating), position = "dodge")

# check aggregate demographics in each cluster

aggdf <- aggregate(cbind(Unit.price,Quantity,gross.income,Total,Rating,Customer1,Gender1) ~ cluster_km, data=survey, mean )
aggdf

#Story1
library(ggplot2)

ggplot(aggdf, aes(x = cluster_km, y = Unit.price)) + 
  geom_point() +  
  geom_text(aes(label=cluster_km, color=cluster_km),hjust=0, vjust=0) 

ggplot(aggdf, aes(x = cluster_km, y = Quantity)) + 
  geom_point() +  
  geom_text(aes(label=cluster_km, color=cluster_km),hjust=0, vjust=0) 

ggplot(aggdf, aes(x = cluster_km, y = gross.income)) + 
  geom_point() +  
  geom_text(aes(label=cluster_km, color=cluster_km),hjust=0, vjust=0) 

ggplot(aggdf, aes(x = cluster_km, y = Total)) + 
  geom_point() +  
  geom_text(aes(label=cluster_km, color=cluster_km),hjust=0, vjust=0) 

ggplot(aggdf, aes(x = cluster_km, y = Customer1)) + 
  geom_point() +  
  geom_text(aes(label=cluster_km, color=cluster_km),hjust=0, vjust=0) 

ggplot(aggdf, aes(x = cluster_km, y = Gender1)) + 
  geom_point() +  
  geom_text(aes(label=cluster_km, color=cluster_km),hjust=0, vjust=0) 

ggplot(aggdf, aes(x = cluster_km, y = Rating)) + 
  geom_point() +  
  geom_text(aes(label=cluster_km, color=cluster_km),hjust=0, vjust=0) 



#Story2
library(dplyr)

# Calculate city-wise average sales
city_avg_sales <- Sales %>%
  group_by(City, cluster_km) %>%
  summarise(avg_sales = mean(Total)) %>%
  ungroup()

# Reorder cities by average sales in descending order
city_avg_sales <- city_avg_sales %>%
  arrange(City, desc(avg_sales)) %>%
  mutate(City = factor(City, levels = unique(City)))

# Plotting details of each cluster within each city
ggplot(city_avg_sales, aes(x = City, y = avg_sales, fill = factor(cluster_km))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "City-wise Average Sales by Cluster",
       x = "City", y = "Average Sales",
       fill = "Cluster") +
  theme_minimal()

# Filter data 
Naypyitaw_data <- Sales %>%
  filter(City == "Naypyitaw")

# Calculate product line-wise average sales
Naypyitaw_avg_sales <- Naypyitaw_data %>%
  group_by(Product.line, cluster_km) %>%
  summarise(avg_sales = mean(Total)) %>%
  ungroup()

# Plotting details of each cluster within each product line in city 
ggplot(Naypyitaw_avg_sales, aes(x = Product.line, y = avg_sales, fill = factor(cluster_km))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Naypyitaw: Product Line-wise Average Sales by Cluster",
       x = "Product Line", y = "Average Sales",
       fill = "Cluster") +
  theme_minimal()+
theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter data for Naypyitaw and Food and Beverages product line
Naypyitaw_food_beverages <- Sales %>%
  filter(City == "Naypyitaw", Product.line == "Food and beverages")

# Calculate gender-wise sales
Naypyitaw_food_beverages_gender <- Naypyitaw_food_beverages %>%
  group_by(Gender) %>%
  summarise(Avg_sales = mean(Total)) %>%
  ungroup()

# Plotting sales of Food and Beverages against gender in Naypyitaw
ggplot(Naypyitaw_food_beverages_gender, aes(x = Gender, y = Avg_sales, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Naypyitaw: Sales of Food and Beverages by Gender",
       x = "Gender", y = "Average Sales",
       fill = "Gender") +
  theme_minimal()

# Filter data for Naypyitaw, Food and Beverages product line, and female gender
naypyitaw_food_beverages_female <- Sales %>%
  filter(City == "Naypyitaw", Product.line == "Food and beverages", Gender == "Female")

# Calculate membership-wise sales
naypyitaw_food_beverages_female_membership <- naypyitaw_food_beverages_female %>%
  group_by(Customer.type) %>%
  summarise(Avg_sales = mean(Total)) %>%
  ungroup()

# Plotting sales of Food and Beverages by female gender and membership in Naypyitaw
ggplot(naypyitaw_food_beverages_female_membership, aes(x = Customer.type, y = Avg_sales, fill = Customer.type)) +
  geom_bar(stat = "identity") +
  labs(title = "Naypyitaw: Sales of Food and Beverages by Female Gender and Membership",
       x = "Membership", y = "Average Sales",
       fill = "Membership") +
  theme_minimal()

#hcluster

With_City <- Sales.norm
With_City$City <- Sales$City
grouped_sales1 <- With_City %>%
  group_by(City) %>%
  summarise_all(mean) %>%
  as.data.frame()
rownames(grouped_sales1) <- grouped_sales1$City
euclidean_dist1 <- dist(grouped_sales1[, -1], method = "euclidean")
hcluster.out1 <- hclust(euclidean_dist1, method = "ward.D")
my_palette <- colorRampPalette(c("lightblue", "darkblue"))(n = 100)
heatmap(as.matrix(grouped_sales1[, -1]),  
        Rowv = as.dendrogram(hcluster.out1),  
        Colv = NULL,  
        scale = "none",  
        col = my_palette, 
        main = "HC(Grouped by City)",
        cexRow = 0.8,  
        cexCol = 0.8,  
        cex.main = 1,  
        
)

With_Product.line <- Sales.norm
With_Product.line$Product.line <- Sales$Product.line
grouped_sales2 <- With_Product.line %>%
  group_by(Product.line) %>%
  summarise_all(mean) %>%
  as.data.frame()
rownames(grouped_sales2) <- grouped_sales2$Product.line
euclidean_dist2 <- dist(grouped_sales2[, -1], method = "euclidean")
hcluster.out2 <- hclust(euclidean_dist2, method = "ward.D")
my_palette <- colorRampPalette(c("lightblue", "darkblue"))(n = 100)
heatmap(as.matrix(grouped_sales2[, -1]),  
        Rowv = as.dendrogram(hcluster.out2),  
        Colv = NULL,  
        scale = "none",  
        col = my_palette, 
        main = "HC(Grouped by Product.line)",
        cexRow = 0.8,  
        cexCol = 0.8,  
        cex.main = 1,  
        
)

With_Gender <- Sales.norm
With_Gender$Gender <- Sales$Gender
grouped_sales3 <- With_Gender %>%
  group_by(Gender) %>%
  summarise_all(mean) %>%
  as.data.frame()
rownames(grouped_sales3) <- grouped_sales3$Gender
euclidean_dist3 <- dist(grouped_sales3[, -1], method = "euclidean")
hcluster.out3 <- hclust(euclidean_dist3, method = "ward.D")
my_palette <- colorRampPalette(c("lightblue", "darkblue"))(n = 100)
heatmap(as.matrix(grouped_sales3[, -1]),  
        Rowv = as.dendrogram(hcluster.out3),  
        Colv = NULL,  
        scale = "none",  
        col = my_palette, 
        main = "HC(Grouped by Gender)",
        cexRow = 0.8,  
        cexCol = 0.8,  
        cex.main = 1,  
        
)

With_Customer.type <- Sales.norm
With_Customer.type$Customer.type <- Sales$Customer.type
grouped_sales4 <- With_Customer.type %>%
  group_by(Customer.type) %>%
  summarise_all(mean) %>%
  as.data.frame()
rownames(grouped_sales4) <- grouped_sales4$Customer.type
euclidean_dist4 <- dist(grouped_sales4[, -1], method = "euclidean")
hcluster.out4 <- hclust(euclidean_dist4, method = "ward.D")
my_palette <- colorRampPalette(c("lightblue", "darkblue"))(n = 100)
heatmap(as.matrix(grouped_sales4[, -1]),  
        Rowv = as.dendrogram(hcluster.out4),  
        Colv = NULL,  
        scale = "none",  
        col = my_palette, 
        main = "HC(Grouped by Customer.type)",
        cexRow = 0.8,  
        cexCol = 0.8,  
        cex.main = 1,  
        
)

With_Payment <- Sales.norm
With_Payment$Payment <- Sales$Payment
grouped_sales5 <- With_Payment %>%
  group_by(Payment) %>%
  summarise_all(mean) %>%
  as.data.frame()
rownames(grouped_sales5) <- grouped_sales5$Payment
euclidean_dist5 <- dist(grouped_sales5[, -1], method = "euclidean")
hcluster.out5 <- hclust(euclidean_dist5, method = "ward.D")
my_palette <- colorRampPalette(c("lightblue", "darkblue"))(n = 100)
heatmap(as.matrix(grouped_sales5[, -1]),  
        Rowv = as.dendrogram(hcluster.out5),  
        Colv = NULL,  
        scale = "none",  
        col = my_palette, 
        main = "HC(Grouped by Payment)",
        cexRow = 0.8,  
        cexCol = 0.8,  
        cex.main = 1,  
        
)

Sales.norm$Hour <- substr(Sales$Time, 1, 2)
grouped_sales_hourly <- Sales.norm %>%
  arrange(Hour) %>%
  group_by(Hour) %>%
  summarise_all(mean) %>%
  as.data.frame()
rownames(grouped_sales_hourly) <- grouped_sales_hourly$Hour
euclidean_dist_hourly <- dist(grouped_sales_hourly[, -1], method = "euclidean")
hcluster_out_hourly <- hclust(euclidean_dist_hourly, method = "ward.D")
my_palette <- colorRampPalette(c("lightblue", "darkblue"))(n = 100)
heatmap(as.matrix(grouped_sales_hourly[, -1]),  
        Rowv = as.dendrogram(hcluster_out_hourly),  
        Colv = NULL,  
        scale = "none",  
        col = my_palette, 
        main = "HC(Grouped by Hour)",
        cexRow = 0.8,  
        cexCol = 0.8,  
        cex.main = 1,
        labRow = grouped_sales_hourly$Hour  # Using Hour column as row labels
)


Sales.norm$Date <- as.Date(Sales$Date, format = "%m/%d/%y")
Sales.norm$Day <- format(Sales.norm$Date, "%A") 
grouped_sales_daily <- Sales.norm %>%
  group_by(Day) %>%
  summarise_if(is.numeric, mean) %>%
  as.data.frame()
rownames(grouped_sales_daily) <- grouped_sales_daily$Day
euclidean_dist_daily <- dist(grouped_sales_daily[, -1], method = "euclidean")
hcluster_out_daily <- hclust(euclidean_dist_daily, method = "ward.D")
my_palette <- colorRampPalette(c("lightblue", "darkblue"))(n = 100)
heatmap(as.matrix(grouped_sales_daily[, -1]),  
        Rowv = as.dendrogram(hcluster_out_daily),  
        Colv = NULL,  
        scale = "none",  
        col = my_palette, 
        main = "HC(Grouped by Day)",
        cexRow = 0.8,  
        cexCol = 0.8,  
        cex.main = 1  
)

Sales$Day=Sales.norm$Day
Sales$Hour=Sales.norm$Hour

#Story 3

mean_income <- Sales %>%
  group_by(Day, Hour, cluster_km) %>%
  summarise(mean_income = mean(Total, na.rm = TRUE))
ggplot(mean_income, aes(x = Hour, y = mean_income, color = factor(cluster_km))) +
  geom_point() +
  facet_wrap(~ Day) +
  labs(title = "Average Sales by Hour, Cluster_km, and Day",
       x = "Hour",
       y = "Average Sales",
       color = "Cluster_km") +
  theme_minimal()








