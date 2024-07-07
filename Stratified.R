install.packages("survey")
library("survey")





data = read.csv("shopping_trends with region.csv")

#ANOVA TEST One way
one.way <- aov(Purchase.Amount..USD.~Season, data = data)
summary(one.way)

#therefore we are using seasons as our stratified variable

s = aggregate(data$Purchase.Amount..USD.~data$Season,FUN=sd)
s1 = s[1,2]
s2 = s[2,2]
s3 = s[3,2]
s4 = s[4,2]

N1 = sum(data$Season == "Fall")
N2 = sum(data$Season == "Spring")
N3 = sum(data$Season == "Summer")
N4 = sum(data$Season == "Winter")
N = N1+N2+N3+N4

N1s1 = s1*N1
N2s2 = s2*N2
N3s3 = s3*N3
N4s4 = s4*N4


Nh = c(N1,N2,N3,N4)
NhSh = c(N1s1,N2s2,N3s3,N4s4)
sh = c(s1,s2,s3,s4)
sh

v = sum((sum(NhSh)/NhSh)*(Nh/N)^2*sh^2)

n0 = (1.96/2)^2*v

n = ceiling(n0/(1+(n0/N)))
n
#therefore total sample size= 473
#now lets find n1,n2,n3,n4

n1 = ceiling((N1s1/sum(NhSh))*n)
n2 = ceiling((N2s2/sum(NhSh))*n)
n3 = ceiling((N3s3/sum(NhSh))*n)
n4 = ceiling((N4s4/sum(NhSh))*n)

n1
n2
n3
n4

#getting statas seperately as data frames
Fall = data[data$Season=="Fall",]
Spring = data[data$Season=="Spring",]
Summer = data[data$Season=="Summer",]
Winter = data[data$Season=="Winter",]


#Defining weights for specific strats
W1 = N1/n1
W1
W2 = N2/n2
W2
W3 = N3/n3
W3
W4 = N4/n4
W4

#SAMPLE 1

#getting a sample from the above data frames
sample_Fall = Fall[sample(1:975,119),]
sample_Spring = Spring[sample(1:999,123),]
sample_Summer = Summer[sample(1:955,115),]
sample_Winter = Winter[sample(1:971,117),]



#combind the sratas to get the Stratified sample
sample_strat = rbind(sample_Fall,sample_Spring,sample_Summer,sample_Winter)
sample_strat$Weights = c(rep(W1,119),rep(W2,123),rep(W3,115),rep(W4,117))

#Defining the survey design object
strat_design = svydesign(id=~1, strata = ~Season, weights = ~Weights, data = sample_strat)

#estimating the pop mean of Purchase Amount
svymean(~Purchase.Amount..USD., strat_design)


#estimating the pop total of Purchase Amount
svytotal(~Purchase.Amount..USD.,strat_design)

#estimating pop proportions of gender
svymean(~Gender,strat_design)

#SAMPLE 2
#getting another sample from the above data frames
sample_Fall = Fall[sample(1:975,119),]
sample_Spring = Spring[sample(1:999,123),]
sample_Summer = Summer[sample(1:955,115),]
sample_Winter = Winter[sample(1:971,117),]


#combind the sratas to get the Stratified sample
sample_strat = rbind(sample_Fall,sample_Spring,sample_Summer,sample_Winter)
sample_strat$Weights = c(rep(W1,119),rep(W2,123),rep(W3,115),rep(W4,117))

#Defining the survey design object
strat_design = svydesign(id=~1, strata = ~Season, weights = ~Weights, data = sample_strat)

#estimating the pop mean of Purchase Amount
svymean(~Purchase.Amount..USD., strat_design)


#estimating the pop total of Purchase Amount
svytotal(~Purchase.Amount..USD.,strat_design)

#estimating pop proportions of gender
svymean(~Gender,strat_design)



#Ration Estimation

r = svyratio(~Purchase.Amount..USD.,~Previous.Purchases,strat_design)
r


















