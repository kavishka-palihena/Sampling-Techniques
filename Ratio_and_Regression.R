getwd()
setwd("C:/Users/HIRUNI/Desktop/IS 3001 REPORT")
dir()

df=read.csv("shopping_trends with region_2.csv")
#df


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
#Sample_SRS_1
#mean(Sample_SRS_1$Age)


##Sample Weight


Sample_Weight=N/sample_size
Sample_Weight

##Defining the survey design object
library(survey)
SRS_design=svydesign(id=~1,weights=NULL,data=Sample_SRS_1)
SRS_design



variables <- names(df)

# Using colnames() function
variables <- colnames(df)
variables

##########
#Ratio and Regression

#Mannually
##Cheching linearity
# r=cor(df$Purchase.Amount..USD.,df$Review.Rating)
# r
# plot(Sample_SRS_1$Review.Rating,Sample_SRS_1$Purchase.Amount..USD.)
# line_co=line(Sample_SRS_1$Review.Rating,Sample_SRS_1$Purchase.Amount..USD.)
# line_co
# #RAtio
# B.hat=sum(Sample_SRS_1$Purchase.Amount..USD.)/sum(Sample_SRS_1$Review.Rating)
# B.hat
# tx=sum(df$Review.Rating)
# tx
# x.bar.u=mean(df$Review.Rating)
# x.bar.u
# 
# t.hat.yr=B.hat*tx
# t.hat.yr
# sum(df$Purchase.Amount..USD.)
# 
# y.bar.hat.r=B.hat*x.bar.u
# y.bar.hat.r
# mean(df$Purchase.Amount..USD.)
# 
# ######
# 
# # Checking wether there is a linear relationshio
# #RAtio
# r=cor(df$Purchase.Amount..USD.,df$Review.Rating)
# r
# plot(Sample_SRS_1$Review.Rating,Sample_SRS_1$Purchase.Amount..USD.)
# line_co=line(Sample_SRS_1$Review.Rating,Sample_SRS_1$Purchase.Amount..USD.)
# line_co
# #Ratio estimate using survey package
# r = svyratio(~Purchase.Amount..USD.,~Review.Rating,SRS_design)
# r
# tx=sum(df$Review.Rating)
# tx
# predict(r,total = tx) ##t_yr_hat_bar=B_hat*t_x
# x.bar.u=mean(df$Review.Rating)
# x.bar.u
#predict(r,x.bar.u)  ##yr_hat_bar=B_hat*x.bar.u


#######################################################################################################

########## Regression Analysis
# Assuming you have a survey design object 'design' and a data frame 'survey_data'

# Load necessary libraries (if not already loaded)
# install.packages("survey")
## Method 1
library(survey)

?svyglm

plot(Sample_SRS_1$Review.Rating,Sample_SRS_1$Purchase.Amount..USD.)
cor(Sample_SRS_1$Review.Rating,Sample_SRS_1$Purchase.Amount..USD.)

# Define the regression model
regression_model =svyglm(formula =Purchase.Amount..USD. ~ Review.Rating, design = SRS_design)

# Obtain regression coefficients and summary
coefficients = coef(regression_model)
summary = summary(regression_model)

# Print coefficients and summary
print(coefficients)
print(summary)

## Another method for Regression

#Method 2
plot(Sample_SRS_1$Review.Rating,Sample_SRS_1$Purchase.Amount..USD.)
line_co=line(Sample_SRS_1$Review.Rating,Sample_SRS_1$Purchase.Amount..USD.)
line_co

abline(coef(line_co))
r1=cor(Sample_SRS_1$Review.Rating,Sample_SRS_1$Purchase.Amount..USD.)
r1

## Fitting the linear rwgressrion model

y_bar_hat_reg1=-59.69+31.88*mean(df$Review.Rating)
y_bar_hat_reg1

## Avtual population mean
mean(df$Purchase.Amount..USD.)


