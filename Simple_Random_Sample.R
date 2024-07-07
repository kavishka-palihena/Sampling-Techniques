getwd()
setwd("C:/Users/Ganeshi/Documents")
dir()


df = read.csv("shopping_trends with region.csv" )

#Simple random sampling
set.seed(100)
# Define parameters
population_size <- 3900  # Example population size
confidence_level <- 0.95  # Example confidence level (95%)
margin_of_error <- 0.03  # Example margin of error (3%)

# Calculate sample size
z <- qnorm((1 + confidence_level) / 2)  # Z-score for the confidence level
p <- 0.5  # Assuming maximum variability if actual proportion is unknown
n0<- (z^2 * p * (1 - p)) / (margin_of_error^2)
n0

n= n0 /(1+(n0/population_size))
n

sample_size = round(n)
sample_size


#Sample weight 

Sample_Weight = population_size/sample_size
Sample_Weight



library(sampler)
Size = rsampcalc(nrow(df),e =3,ci =95, p=0.5)
Size


# Using colnames() function
variables <- colnames(df)
variables
head(df)


# Sample 1
set.seed(1000)
sample_SRS_1 = rsamp(df,sample_size,over = 0,rep = FALSE)
#sample_SRS_1

library(survey)
#Defining the survey design object
SRS_design=svydesign(id=~1,weights=Sample_Weight,data=sample_SRS_1)
SRS_design




#Estimations####################

#mean
#Estimating the population mean of Age
svymean(~Age,SRS_design)

#Estimating the population mean of Purchase Amount (USD)
svymean(~Purchase.Amount..USD.,SRS_design)

#Estimating the population mean of Review Rating
svymean(~Review.Rating,SRS_design)

#Estimating the population mean of Previous Purchases
svymean(~Previous.Purchases,SRS_design)


#Total
#Estimating the population total of Purchase Amount (USD)
svytotal(~Purchase.Amount..USD.,SRS_design)

#Proportion
#Estimating the Proportion of seasons
svymean(~Season,SRS_design)

#Estimating the Proportion of Gender
svymean(~Age,SRS_design)

#Estimating the Proportion of discounts
svymean(~Discount.Applied,SRS_design)

#Estimating the Proportion of Payment.Method
svymean(~Payment.Method,SRS_design)

#Estimating the Proportion of Frequency.of.Purchases
svymean(~Frequency.of.Purchases,SRS_design)



#Actual population values ################

#mean
#Actual Population mean of Age
mean(df$Age)

#Actual Population mean of Purchase Amount (USD)
mean(df$Purchase.Amount..USD.)

#Actual Population mean of Review Rating
mean(df$Review.Rating)

#Actual Population mean of Previous Purchases
mean(df$Previous.Purchases)

#Total
#Actual population total of Purchase Amount (USD)
sum(df$Purchase.Amount..USD.)

#Proportion
#Actual Proportion of seasons
table(df$Season)/population_size

#Actual Proportion of Gender
table(df$Gender)/population_size

#Actual Proportion of discounts
table(df$Discount.Applied)/population_size

#Actual Proportion of Payment.Method
table(df$Payment.Method)/population_size

#Actual Proportion of Frequency.of.Purchases
table(df$Frequency.of.Purchases)/population_size




###############
#Ration Estimation

r=cor(df$Purchase.Amount..USD.,df$Previous.Purchases)
r
print("Correlation coefficient is 0.9982469 for purchace amount and review rating")

plot(sample_SRS_1$Previous.Purchases,sample_SRS_1$Purchase.Amount..USD.,main = "Purchas Amount Vs review rating")
lin_c0 = line(sample_SRS_1$Previous.Purchases,sample_SRS_1$Purchase.Amount..USD.)
lin_c0

#Since the line nearly goes trough the origin(intercept =0), we select the ratio estimator
B.hat = sum(sample_SRS_1$Purchase.Amount..USD.)/sum(sample_SRS_1$Previous.Purchases)
B.hat
tx = sum(df$Previous.Purchases)
tx
x.bar.u = mean(df$Previous.Purchases)
x.bar.u

t.hat.yr = B.hat*tx
t.hat.yr
sum(df$Purchase.Amount..USD.)

#*We estimated the total of the purchase amount as 233278.6 using ratio estimation and
#*The actual population average purchase amount is 233081
#*Therefore we can assume that the ratio estimate gives a better estimate.

y.hat.yr = B.hat*x.bar.u
y.hat.yr
mean(df$Purchase.Amount..USD.)
#*We estimated the mean of the purchase amount as 59.81502 using ratio estimation and
#*The actual population mean purchase amount is  59.76436
#*Therefore we can assume that the ratio estimate gives a better estimate.


##Graphical Analysis
library(ggplot2)
n1=table(sample_SRS_1$Item.Purchased)
n2=table(sample_SRS_1$Gender)
n3=table(sample_SRS_1$Category)
n4=table(sample_SRS_1$Size)
n5=table(sample_SRS_1$Colour)
n6=table(sample_SRS_1$Season)
n7=table(sample_SRS_1$Subscription.Status)
n8=table(sample_SRS_1$Payment.Method)
n9=table(sample_SRS_1$Shipping.Type)
n10=table(sample_SRS_1$Discount.Applied)
n11=table(sample_SRS_1$Frequency.of.Purchases)

plot(n2,main="Distribution of Gender across the sample")
plot(n4,main="Distribution of Size across the sample")
plot(n6,main="Distribution of Season across the sample")
plot(n7,main="Distribution of Subscription.Status across the sample")
plot(n8,main="Distribution of Payment.Methods across the sample")
plot(n9,main="Distribution of Shipping.Type across the sample")
plot(n10,main="Distribution of Discount.Applied across the sample")
plot(n11,main="Distribution of Frequency.of.Purchases across the sample")

#Histogram

hist(sample_SRS_1$Previous.Purchases,main = "The distribution of the previous perchuse amount")
hist(sample_SRS_1$Purchase.Amount..USD.,main = "The distribution of the perchuse amount(USD)")

library(ggplot2)
#Bar plots
ggplot(sample_SRS_1,aes(x=Payment.Method))+geom_bar(fill="yellow")+labs(title = "Consumers used payment method")
ggplot(sample_SRS_1,aes(x=Gender))+geom_bar(fill="green")+labs(title = "Consumers gender")

#stacked bar chart
ggplot(sample_SRS_1,aes(x= Season,fill= Category))+geom_bar(fill="pink")+labs(title = "Consumers buy the type of catogories during the season")

#group bar chart
df_base = ggplot(data = sample_SRS_1,aes(x=Season,fill=Discount.Applied))
df_base+geom_bar(position = position_dodge())+labs(title = "Give the discount in difference seasons")

##contingency Table
t1=table(sample_SRS_1$Season,sample_SRS_1$Category)
t1

t2=table(sample_SRS_1$Gender,sample_SRS_1$Category)
t2


##Plot of Purchase price Vs Previous Purchases 
plot(sample_SRS_1$Previous.Purchases,sample_SRS_1$Purchase.Amount..USD.,xlab="Previous Purchases",ylab="Purchase Price",main="Plot of Purchase price Vs Previous Purchases ")
line_co=line(sample_SRS_1$Previous.Purchases,sample_SRS_1$Purchase.Amount..USD.)
line_co
abline(coef(line_co))
cor(sample_SRS_1$Previous.Purchases,sample_SRS_1$Purchase.Amount..USD.)





