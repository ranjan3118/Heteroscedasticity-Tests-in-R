# or to use import dataset & excel  
install.packages("readxl")
library(readxl)

# Import dataset 

# Importing the data-set called, ‘het.csv’
het1<-read.csv(file.choose(), header = TRUE, sep = ",", skip = 0) 

# Attaching the dataset1 to the R-session
attach(het1)
# TO view the dataset
View(het1)
colnames(het1)

# Plotting the Income-Expenditure scatter plot using basic graphics
plot(Consumption.Expenditure~Disposable.Income, pch=8, col = "red", fg="blue", col.lab="blue",col.axis="blue", main= "Fig.-14.11: Scatter Plot of Disposable Income and Consumption Expenditure", col.main ="blue")
grid(NA, 5, lwd=2)
grid(6,4, lwd=2, col="green") 
grid(6,NA, lwd=2, col="green") 
grid(NA,NULL, lwd=2, col="gray", lty="dashed") 
grid(NA,NULL, lwd=2, col="gray", lty="dotted")
grid(NA,NULL, lwd=2, col="gray", lty=1) 

# Plotting the Income-Expenditure scatter plot using ggplot2 package
library(ggplot2)
ggplot(het1, aes(Disposable.Income, Consumption.Expenditure))+geom_point(shape=8, col="red")

?aes
# To fit the model
model1<- lm(Consumption.Expenditure~Disposable.Income)
summary(model1)

# To save the residuals of the regression by using the following code.
het1$uhat1<-resid(model1)
# To plot the histogram (both basic and advanced graphics) and look at its distributional pattern
attach(het1)
hist(uhat1^2, col = "darkgreen", col.lab="blue", col.axis="blue", fg="blue", main = "Fig.-14.12: Histogram of Squared Residuals",col.main="blue")
library(ggplot2)
ggplot(het1, aes(uhat1^2))+geom_histogram(fill="darkgreen")+labs(title="Fig.-14.12: Histogram of Squared Residuals")

#  Plotting the squared residuals against the fitted values of the dependent variable of the model
plot(fitted(model1), uhat1^2, pch=8, col="red", col.lab="blue", col.axis="blue",fg="blue", main = "Fig.-14.13:Scatter Plot of Squared Residuals and Fitted Values of the Dependent Variable", col.main="blue")
library(ggplot2)
ggplot(het1, aes(fitted(model1), uhat1^2))+geom_point(shape=8, col="red")+labs(title="Fig.-14.13:Scatter Plot of Squared Residuals and Fitted Values of the Dependent Variable")

# R built-in diagnostic checking
par(mfrow=c(2,2))
plot(model1)
dev.off()
#or 
par(mfrow=c(1,1))

# Park Test for heteroscedasticity
luhat1<-log(uhat1^2)			
ldispinc<-log(Disposable.Income)
park<-lm(luhat1~ldispinc)
summary(park)
park<-lm(log(uhat1^2)~log(Disposable.Income))
summary(park)


# Breusch-Godfrey Test in r 
install.packages("lmtest")
library(lmtest)
update.packages("lmtest")

bptest(model1)

# Goldfeld-Quandt test for heteroscedasticity
library(lmtest)
gqtest(model1, fraction = .267)


# White's gen test

model2<-lm(I(het1$uhat1^2)~Disposable.Income+I(Disposable.Income^2), data=het1)
summary(model2)
Rsq<-summary(model2)$r.squared
S <-summary(model2)$df
N=length(Disposable.Income)
chisq<-N*Rsq
options(scipen = 999)
1-pchisq(chisq, 2)


# Remedies- weighted least squares
# Assumption : residual variance is proportional to square term of xi 
model5 = lm(Consumption.Expenditure~Disposable.Income, weights = 	1/Disposable.Income, data = het1)
summary(model5)

bptest(model5)

model6 = lm(Consumption.Expenditure~Disposable.Income, weights = 	1/I(Disposable.Income^0.5), data = het1)
summary(model6)


# White's Heteroscedasticity-consistent Standard Errors or Robust Standard Errors:
install.packages("sandwich")
update.packages("sandwich")
library(sandwich)
# To get Heteroskedasticity-consistent estimation of the covariance matrix of the coefficient estimates in regression models.
vcovHC(model1)
# To get the normal variance-covariance matrix of the main OLS parameters
vcov(model1)

# To get the model estimation assuming homoscedasticity in the original model 
install.packages("lmtest")
library(lmtest)
coeftest(model1)
coeftest(model1, vcov. = vcovHC(model1, type = "const"))

# To get the Heteroskedasticity-consistent model estimation as suggested by White (1980) and justified by asymptotic arguments
coeftest(model1, vcov. = vcovHC(model1))

# To get the Heteroskedasticity-consistent model estimation as suggested by MacKinnon and White (1985) to improve the performance in small samples
coeftest(model1, vcov. = vcovHC(model1, type = "HC3"))










detach()
