library(readr)
Startup1<- read.csv(file.choose()) # choose the 50_Startup.csv data set
View(Startup1)
summary(Startup1)

attach(Startup1)
names(Startup1)

# Add factor variables for fuel type  Diesel and Petrol by taking CNG as a base 
Startup1$State= factor(Startup1$State,
                          levels = c('New York','California','Florida'),
                          labels = c(1,2,3))
View(Startup1)

Startup = Startup1[,-4] 
View(Startup)
# Find the correlation b/n Output (Profit) & (R.D.Spend,Administration,Marketing.Spend,California,Florida,New York)-Scatter plot
pairs(Startup)

# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Startup)


### Partial Correlation matrix - Pure Correlation  b/n the variables
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Startup))

# The Linear Model of interest with all the columns
model_Startup <- lm(Profit~.,data=Startup)

summary(model_Startup)
#as above all variables other than Administration & Marketing.Spend are siginificant as values are less than 0.05.
#R^2 value is 0.9507 which is less than 0.80 so we can say that model is best fit.
#As the  R^2 value is acceptable but  we have to check for significance of Administration & Marketing.Spend we by its multicollinearity individually
# Multicollinearity check
# Model based on only Administration 
model_1<-lm(Profit~Administration,data=Startup)
summary(model_1) #Administration became insignificant

# Model based on only Marketing.Spend
model_2<-lm(Profit~Marketing.Spend,data=Startup)
summary(model_2) # Marketing.Spend became significant

# Model based on Administration and Marketing.Spend
model_3<-lm(Profit~(Administration+Marketing.Spend),data=Startup)
summary(model_3) # Both became significant
install.packages("car")
library(car)
# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
vif(lm(Profit~.,data=Startup)) # Original model
## vif>10 then there exists collinearity among all the variables 
#as vif value is less than 10 so there is no autocollinearity in variables, we need to check about influencing entries

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model_Startup,id.n=2,id.cex=0.7)
# The above plots will reveal whether the Output Profit

influenceIndexPlot(model_Startup)
influencePlot(model_Startup)

#from avplot it is clear that all variables are contributing towards predicting profit.
#so we check influence index plot
#From influence index plot we can say that 49,50 no.observation are influencing badly so we need to remove those  
# Preparing new models by excluding above observations we check for any improvements in model or not

#model after deleting 49,50 no. influencing observations
model_4 <- lm(Profit~.,data=Startup[-c(49,50),])

summary(model_4)
# as R^2 value is good but Administration & Marketing.Spend are not still significant
# we need to do transformation

# Sqrt Model

# x = sqrt(Startup); y = profit

# Simple Linear Regression model-log transform
model_5 <- lm(Profit~ sqrt(R.D.Spend)+sqrt(Administration)+sqrt(Marketing.Spend), data=Startup )

summary(model_5) 
#R^2 value is acceptable but Administration & Marketing.Spend values are not significant.
#for this it is needed to get more data 

colnames(Startup)[colnames(Startup)=="R.D.Spend"] <- "RD"
colnames(Startup)[colnames(Startup)=="Marketing.Spend"] <- "Marketing"
View(Startup)
# Log Model

# x = sqrt(Startup); y = profits
log(Startup$RD)
Startup = Startup[-c(20,48,49,50),]
View(Startup)

# Simple Linear Regression model-log transform
model_6 <- lm(Profit~ log(RD)+log(Administration)+log(Marketing), data=Startup )
summary(model_6) 
#R^2 value is not acceptable and Administration & Marketing.Spend values are not significant.
#for this it is needed to get more data 