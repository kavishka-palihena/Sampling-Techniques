getwd()
setwd("C:/Users/HIRUNI/Desktop/IS 3001 REPORT")
dir()

df=read.csv("shopping_trends with region.csv")
df


## Sample_Size
set.seed(100)
# Define parameters
N <- 3900  # Example population size
confidence_level <- 0.95  # Example confidence level (95%)
margin_of_error <- 0.03  # Example margin of error (5%)

# Calculate sample size
z <- qnorm((1 + confidence_level) / 2)  # Z-score for the confidence level
p <- 0.5  # Assuming maximum variability if actual proportion is unknown
n0 <- (z^2 * p * (1 - p)) / (margin_of_error^2)
n0


n=n0/(1+(n0/N))
n

sample_size=round(n)
sample_size


#install.packages("sampler")
library(sampler)
Size = rsampcalc(nrow(df),e =3,ci =95, p=0.5)
Size

#Sample 1
set.seed(1000)
Sample_SRS_1=rsamp(df,Size,over=0,rep=FALSE)
Sample_SRS_1
#mean(Sample_SRS_1$Age)

##Defining the survey design object
library(survey)
SRS_design=svydesign(id=~1,weights=NULL,data=Sample_SRS_1)
SRS_design


#Sample 2
set.seed(2000)
Sample_SRS_2=rsamp(df,Size,over=0,rep=FALSE)
Sample_SRS_2
#mean(Sample_SRS_2$Age)

##Defining the survey design object
library(survey)
SRS_design_2=svydesign(id=~1,weights=NULL,data=Sample_SRS_2)
SRS_design_2


##Sample Weight


Sample_Weight=N/sample_size
Sample_Weight



# Using colnames() function
variables <- colnames(df)
variables


######################### HISTOGRAM ##########################################

## Histogram of Age for sample 1
svyhist(~Age, SRS_design, main="Histogram of Age",col="Red",probability = FALSE)

## Histogram of Age for sample 2
svyhist(~Age, SRS_design_2, main="Histogram of Age",col="Red",probability = FALSE)


## Histogram of Purchase.Amount..USD. for sample 1
svyhist(~Purchase.Amount..USD., SRS_design, main="Histogram of The amount of the purchase in USD",col="Red",probability = FALSE)

## Histogram of Purchase.Amount..USD. for sample 2
svyhist(~Purchase.Amount..USD., SRS_design_2, main="Histogram of The amount of the purchase in USD",col="Red",probability = FALSE)


## Histogram of Previous.Purchases for sample 1
svyhist(~Previous.Purchases, SRS_design, main="Histogram of Previous.Purchases",col="Red",probability = FALSE)

## Histogram of Previous.Purchases for sample 2
svyhist(~Previous.Purchases, SRS_design_2, main="Histogram of Previous.Purchases",col="Red",probability = FALSE)



######################### BOXPLOT ##########################################

#"Boxplot of The amount of the purchase in USD with Sex sample 1
svyboxplot(~Purchase.Amount..USD.~Gender, SRS_design, main="Boxplot of The amount of the purchase in USD with Sex ", col="Cyan" )
#"Boxplot of The amount of the purchase in USD with Sex sample 2
svyboxplot(~Purchase.Amount..USD.~Gender, SRS_design_2, main="Boxplot of The amount of the purchase in USD with Sex ", col="Cyan" )


#"Boxplot of The amount of the purchase in USD with Season sample 1
svyboxplot(~Purchase.Amount..USD.~Season, SRS_design, main="Boxplot of The amount of the purchase in USD with Season ", col="Cyan" )
#"Boxplot of The amount of the purchase in USD with Season sample 2
svyboxplot(~Purchase.Amount..USD.~Season, SRS_design_2, main="Boxplot of The amount of the purchase in USD with Season ", col="Cyan" )


#"Boxplot of The amount of the purchase in USD with Size sample 1
svyboxplot(~Purchase.Amount..USD.~Size, SRS_design, main="Boxplot of The amount of the purchase in USD with Size ", col="Cyan" )
#"Boxplot of The amount of the purchase in USD with Size sample 2
svyboxplot(~Purchase.Amount..USD.~Size, SRS_design_2, main="Boxplot of The amount of the purchase in USD with Size ", col="Cyan" )

#"Boxplot of The amount of the purchase in USD with Region sample 1
svyboxplot(~Purchase.Amount..USD.~Region, SRS_design, main="Boxplot of The amount of the purchase in USD with Region ", col="Cyan" )
#"Boxplot of The amount of the purchase in USD with Region sample 2
svyboxplot(~Purchase.Amount..USD.~Region, SRS_design_2, main="Boxplot of The amount of the purchase in USD with Region ", col="Cyan" )



######################### SCATTERPLOT  ##########################################
#"Scatter plot of The amount of the purchase in USD  Vs Age  sample 1
svyplot(Purchase.Amount..USD.~Age,design = SRS_design,style = "bubble",main="Plot of The amount of the purchase in USD vs Age",xlab="Age",ylab="The amount of the purchase in USD")
#"Scatter plot of The amount of the purchase in USD  Vs Age  sample 1
svyplot(Purchase.Amount..USD.~Age,design = SRS_design_2,style = "bubble",main="Plot of The amount of the purchase in USD vs Age",xlab="Age",ylab="The amount of the purchase in USD")

#"Scatter plot of The amount of the purchase in USD  Vs Previous.Purchases  sample 1
svyplot(Purchase.Amount..USD.~Previous.Purchases,design = SRS_design,style = "bubble",main="Plot of The amount of the purchase in USD vs Previous.Purchases",xlab="Age",ylab="Previous.Purchases")
#"Scatter plot of The amount of the purchase in USD  Vs Previous.Purchases  sample 1
svyplot(Purchase.Amount..USD.~Previous.Purchases,design = SRS_design_2,style = "bubble",main="Plot of The amount of the purchase in USD vs Previous.Purchases",xlab="Age",ylab="Previous.Purchases")

#"Scatter plot of The amount of the purchase in USD  Vs Review.Rating  sample 1
svyplot(Purchase.Amount..USD.~Review.Rating,design = SRS_design,style = "bubble",main="Plot of The amount of the purchase in USD vs Review.Rating",xlab="Age",ylab="Review.Rating")
#"Scatter plot of The amount of the purchase in USD  Vs Review.Rating  sample 1
svyplot(Purchase.Amount..USD.~Review.Rating,design = SRS_design_2,style = "bubble",main="Plot of The amount of the purchase in USD vs Review.Rating",xlab="Age",ylab="Review.Rating")











