#install.packages("survey")
library("survey")

data = read.csv("shopping_trends with region.csv")
tail(data)
table(data$Location)
table(data$Region)

#identifying Cluster variable
tapply(data$Purchase.Amount..USD.,data$Region,mean)
tapply(data$Age,data$Region,mean)
#means of several variable are quite similar between clusters
#So we are using "Regions" as our Cluster variable

#First stage Clustering
regions=table(data$Region)
psus=names(regions)
psus
N=length(psus)
N #No. of psus in the population

##Sample 01##
set.seed(100)
ssus=sample(psus,2)
ssus
n=length(ssus)
n #No. of psus in the sample

M0=nrow(data)
M0

#Stage 1 Clustering
M=c()
clust=data.frame()
for(i in 1:length(ssus)){
  clust1 = data[data$Region==ssus[i],]
  M[i]=nrow(clust1)
  clust=rbind(clust,clust1)
}
M

#Stage 2 Clustering
m1=200
m2=220
m=c(m1,m2)

sample_clust=data.frame()
for(i in 1:length(ssus)){
  clust2 = data[data$Region==ssus[i],][sample(1:M[i],m[i]),]
  sample_clust=rbind(sample_clust,clust2)
}


#Calculating Weights
clus_weight=data.frame()
for(i in 1:length(ssus)){
  clus_weight[i,1]=ssus[i]
  clus_weight[i,2]=(N*M[i])/(n*m[i])
}
clus_weight
sample_clust$Weights=c(rep(clus_weight[1,2],m[1]),rep(clus_weight[2,2],m[2]))

#Designing the survey design
library(survey)
clust_design = svydesign(id=~Region, weights = ~Weights, data = sample_clust)
clust_design

#Estimating the population mean of Purchase Amount
svymean(~Purchase.Amount..USD., clust_design)

#Estimating the population total of Purchase Amount
svytotal(~Purchase.Amount..USD.,clust_design)

#Estimating population proportions of gender
svymean(~Gender,clust_design)

#Ratio Estimations

cor(data$Review.Rating,data$Purchase.Amount..USD.)
#since y and x are highly linearly correlated through the origin 
#We can calculate ratio estimations for cluster sampling

r = svyratio(~sample_clust$Purchase.Amount..USD., ~sample_clust$Review.Rating, clust_design)
r  #This is ratio for the clustered design of sample 01
tyr=predict(r, sum(data$Review.Rating))
tyr #This is the estimated total purchase amount we’ve obtained for sample 1 using Ratio estimation. 

#Regression Estimation
plot(sample_clust$Review.Rating,sample_clust$Purchase.Amount..USD.,main="Review Rating Vs Purchase Amount(USD) in the sample",xlab = "Review Rating",ylab = "Purchases Amount(USD)") #positive linear correlated
plot(data$Review.Rating,data$Purchase.Amount..USD.,main="Relationship between Review Rating and Purchase Amount(USD)",xlab = "Review Rating",ylab = "Purchases Amount(USD)")

#Fitting the linear regression model
lm(Purchase.Amount..USD.~Review.Rating,sample_clust)

est_mean_purchase.amount =-63.93 +  32.98 *mean(sample_clust$Review.Rating)
est_mean_purchase.amount #This is the regression estimate for the estimated mean of the purchases amount for sample 1
mean(data$Purchase.Amount..USD.)  #This is the actual population mean of the Purchase amount



###Sample 2##
set.seed(500)
ssus=sample(psus,2)
ssus
n=length(ssus)
n #No. of psus in the sample

M0=nrow(data)
M0

#Stage 1 Clustering
M=c()
clust=data.frame()
for(i in 1:length(ssus)){
  clust1 = data[data$Region==ssus[i],]
  M[i]=nrow(clust1)
  clust=rbind(clust,clust1)
}
M

#Stage 2 Clustering
m1=200
m2=220
m=c(m1,m2)

sample_clust2=data.frame()
for(i in 1:length(ssus)){
  clust2 = data[data$Region==ssus[i],][sample(1:M[i],m[i]),]
  sample_clust2=rbind(sample_clust2,clust2)
}


#Calculating Weights
weight=c()
for(i in 1:2){
  weight[i]=(N*M[i])/(n*m[i])
}

sample_clust2$Weights=c(rep(weight[1],m[1]),rep(weight[2],m[2]))

#Designing the survey design
library(survey)
clust_design2 = svydesign(id=~Region, weights = ~Weights, data = sample_clust2)

#estimating the pop mean of Purchase Amount
svymean(~Purchase.Amount..USD., clust_design2)

#estimating the pop total of Purchase Amount
svytotal(~Purchase.Amount..USD.,clust_design2)

#estimating pop proportions of gender
svymean(~Gender,clust_design2)


#Ratio Estimations
#We can calculate ratio estimations for cluster sampling in sample 2

r = svyratio(~sample_clust2$Purchase.Amount..USD., ~sample_clust2$Review.Rating, clust_design2)
r #this is the ratio for the clustered design of sample 02
tyr=predict(r, sum(data$Review.Rating))
tyr #This is the estimated total purchase amount we’ve obtained for sample 2 using Ratio estimation. 


#Regression Estimation
plot(sample_clust2$Review.Rating,sample_clust2$Purchase.Amount..USD.,main="Review Rating Vs Purchase Amount(USD) in the sample",xlab = "Review Rating",ylab = "Purchases Amount(USD)") #positive linear correlated
plot(data$Review.Rating,data$Purchase.Amount..USD.,main="Relationship between Review Rating and Purchase Amount(USD)",xlab = "Review Rating",ylab = "Purchases Amount(USD)")

#Fitting the linear regression model
lm(Purchase.Amount..USD.~Review.Rating,sample_clust2)
est_mean_purchase.amount =-63.93 +  32.98 *mean(sample_clust$Review.Rating)
est_mean_purchase.amount #This is the regression estimate for the estimated mean of the purchases amount for sample 2
mean(data$Purchase.Amount..USD.) #This is the actual population mean of the Purchase amount


#Graphical Analysis
#install.packages("ggplot2")
#install.packages("tidyverse")

##Graphical Analysis
library(ggplot2)
#Bar plots for the Clusters 
ggplot(data,aes(x=Region))+geom_bar(fill="purple")+labs(title = "Distibution of data according to clusters")

ggplot(sample_clust,aes(x=Region))+geom_bar(fill="pink")+labs(title = "Sample 01")
ggplot(sample_clust2,aes(x=Region))+geom_bar(fill="red")+labs(title = "Sample 01")

par(mfrow = c(1, 2))
## Histogram of Review.Rating for sample 1
svyhist(~Review.Rating, clust_design, main="Histogram of Review Rating \n(Sample 01)",col="Red",probability = FALSE)

## Histogram of Review.Rating for sample 2
svyhist(~Review.Rating, clust_design2, main="Histogram of Review Rating \n(Sample 02)",col="Blue",probability = FALSE)


## Histogram of Purchase.Amount..USD. for sample 1
svyhist(~Purchase.Amount..USD., clust_design, main="Histogram of the amount \n of the purchases (Sample 01)",col="Red",probability = FALSE)

## Histogram of Purchase.Amount..USD. for sample 2
svyhist(~Purchase.Amount..USD., clust_design2, main="Histogram of the amount \n of the purchases (Sample 02)",col="Blue",probability = FALSE)


library(ggplot2)
#group bar chart
df_base = ggplot(data = sample_clust,aes(x=Region,fill=Gender))
df_base+geom_bar(position = position_dodge())+labs(title = "Gender count for each region\n(Sample 01)")

df_base = ggplot(data = sample_clust2,aes(x=Region,fill=Gender))
df_base+geom_bar(position = position_dodge())+labs(title = "Gender count for each region\n(Sample 02)")

#Boxplots
#"The amount of the purchase in USD with Region sample 1
svyboxplot(~Purchase.Amount..USD.~Region, clust_design, main="Boxplot for the amount of the purchase with Region \n (Sample 01) ", col="Cyan" )
#"The amount of the purchase in USD with Region sample 2
svyboxplot(~Purchase.Amount..USD.~Region, clust_design2, main="Boxplot for the amount of the purchase with Region \n (Sample 01) ", col="Pink" )

par(mfrow = c(1, 2))
#"The amount of the purchase in USD with Region sample 1
svyboxplot(~Review.Rating~Region, clust_design, main="Boxplot for the Review Rating with Region \n (Sample 01) ", col="grey" )
#"The amount of the purchase in USD with Region sample 2
svyboxplot(~Review.Rating~Region, clust_design2, main="Boxplot for the Review Rating with Region \n (Sample 02) ", col="gold" )


