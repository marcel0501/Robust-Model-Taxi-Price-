# Loading the Data
ds = read.csv("https://raw.githubusercontent.com/marcel0501/Robust-Model-Taxi-Price-/refs/heads/main/taxi_trip_pricing.csv")

# Inspecting the Data 
head(ds)

#Count of missing values and duplicates AND EVENTUALLY SOLVING ISSUES#
sapply(ds, function(x)(sum(is.null((x)))))
sapply(ds, function(x)(sum(is.na((x)))))
sapply(ds,function(x) any(as.character(x)== "", na.rm = TRUE))
sum(duplicated(ds)) # Duplicated rows count


#Filling the missing values, median for assymmetric numeric variables, average for symmetric numeric variables, mode for categorical variables
hist(ds$Trip_Distance_km) #Median
hist(ds$Base_Fare) #AVG
hist(ds$Passenger_Count)
hist(ds$Per_Km_Rate) #AVG
hist(ds$Per_Minute_Rate) #AVG
hist(ds$Trip_Duration_Minutes) #AVG
hist(ds$Trip_Price) #Median
#Starting with NA in numeric variables
library(Hmisc)
ds$Trip_Distance_km=impute(ds$Trip_Distance_km,median) 
ds$Base_Fare=impute(ds$Base_Fare,mean)
ds$Per_Km_Rate=impute(ds$Per_Km_Rate,mean)
ds$Per_Minute_Rate=impute(ds$Per_Minute_Rate,mean)
ds$Trip_Duration_Minutes=impute(ds$Trip_Duration_Minutes,mean)
ds$Trip_Price=impute(ds$Trip_Price,median)
ds$Passenger_Count=impute(ds$Passenger_Count,median)
#Now for categorical variables
library(modeest)
ds$Weather[ds$Weather == ""] <-NA
ds$Weather[is.na(ds$Weather)] <- mlv(ds$Weather, method = "mfv", na.rm = TRUE)
ds$Time_of_Day[ds$Time_of_Day == ""] <-NA
ds$Time_of_Day[is.na(ds$Time_of_Day)] <- mlv(ds$Time_of_Day, method = "mfv", na.rm = TRUE)
ds$Day_of_Week[ds$Day_of_Week == ""] <-NA
ds$Day_of_Week[is.na(ds$Day_of_Week)] <- mlv(ds$Day_of_Week, method = "mfv", na.rm = TRUE)
ds$Traffic_Conditions[ds$Traffic_Conditions == ""] <-NA
ds$Traffic_Conditions[is.na(ds$Traffic_Conditions)] <- mlv(ds$Traffic_Conditions, method = "mfv", na.rm = TRUE)


#Issues solved we can proceed with EDA
#EDA#
#Relazione tra
boxplot(ds$Trip_Price ~ ds$Time_of_Day, data = ds, main = "Boxplot di Y per categorie di X",
        xlab = "Categoria X", ylab = "Valore numerico Y")
boxplot(ds$Trip_Price ~ ds$Day_of_Week, data = ds, main = "Boxplot di Y per categorie di X",
        xlab = "Categoria X", ylab = "Valore numerico Y")
boxplot(ds$Trip_Price ~ ds$Traffic_Conditions, data = ds, main = "Boxplot di Y per categorie di X",
        xlab = "Categoria X", ylab = "Valore numerico Y")
boxplot(ds$Trip_Price ~ ds$Weather, data = ds, main = "Boxplot di Y per categorie di X",
        xlab = "Categoria X", ylab = "Valore numerico Y")
#We notice that for Weekends + High Traffic and worse Weather conditions we get higher prices
#Correlation between numeric variables
cor_matrix <- cor(ds[,sapply(ds,is.numeric)])
cor_matrix
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.7)
#We notice that Trip_Price is highly correlated with Trip_Distance_km

library(psych)
pairs.panels(ds[,sapply(ds,is.numeric)], 
                 method = "pearson", # correlation method
                 hist.col = "#00AFBB",
                 density = TRUE,  # show density plots
                 ellipses = FALSE # show correlation ellipses
)

#We notice already some influence of outliers on some variables
#Boxplots for numeric variables to spot outliers
boxplot(ds[,sapply(ds,is.numeric)], main="Boxplots for Numeric Variables", col="lightblue", las=2)

#Barcharts for categorical variables

barplot(table(ds$Time_of_Day), main="Time of Day", col="lightgreen")
barplot(table(ds$Day_of_Week), main="Day of Week", col="lightgreen")
barplot(table(ds$Traffic_Conditions), main="Traffic Conditions", col="lightgreen")
barplot(table(ds$Weather), main="Weather", col="lightgreen")


#End of EDA 

#Starting with modeling 1. Colinearity#
#We start by factoring categorical variables

#As we said in row 56, we will try to get the values of the factors low for good conditions so lower prices and high values for bad conditions so higher prices
# to improve interpretability of the coefficients
d1 <- ds
d1$Time_of_Day <- factor(d1$Time_of_Day, levels = c("Morning", "Afternoon", "Evening", "Night"), ordered = TRUE)
d1$Time_of_Day <- as.numeric(d1$Time_of_Day)
d1$Day_of_Week <- factor(d1$Day_of_Week, levels = c("Weekday","Weekend"), ordered = TRUE)
d1$Day_of_Week <- as.numeric(d1$Day_of_Week)
d1$Traffic_Conditions <- factor(d1$Traffic_Conditions, levels = c("Low","Medium","High"), ordered = TRUE)
d1$Traffic_Conditions <- as.numeric(d1$Traffic_Conditions)
d1$Weather <- factor(d1$Weather, levels = c("Clear","Rain","Snow"), ordered = TRUE)
d1$Weather <- as.numeric(d1$Weather)
#Checking TOL
y = as.numeric(d1$Trip_Price)
x <- ds[,sapply(ds,is.numeric)]
x <- as.matrix(x)

library(mctest)
m=lm(y ~ x)
imcdiag(m)

#TOLj > 0.1 for all j, no colinearity issues detected

#Fitting the full model

fit1 <- lm(Trip_Price ~ ., data = d1)
#Checking if the model is adequate with reset test
library(lmtest)
resettest(fit1, power = 2, type = "fitted")
# p-value < 0.05, model not adequate, we need to transform the response variable, Box-Cox transformation

library(MASS)
boxcoxreg1 <- boxcox(fit1)
lambda1 <- boxcoxreg1$x[which(boxcoxreg1$y==max(boxcoxreg1$y))]
lambda1

# Lambda vicino allo 0, serve trasformare la variabile risposta
hist(d1$Trip_Price)
hist(log(d1$Trip_Price)) # Poco Gaussiana ma non perde i valori intorno all'otto
hist(d1$Trip_Price^0.38) # Molto Gaussiana ma perde i valori intorno all'otto

fitLog <- update(fit1, log(.) ~ .)
summary(fitLog)
resettest(fitLog, power = 2, type = "fitted")

fitExponential <- update(fit1, .^0.38 ~ .)
summary(fitExponential)
resettest(fitExponential, power = 2, type = "fitted")

#Dal reset test notiamo che fitExponential è il modello migliore, andiamo avanti con questo
#Diagnostica del modello
par(mfrow=c(2,2))
plot(fitExponential)

#Notiamo la presenza di outliers e di eteroschedasticità
#Let's see if we need to transform some predictors
#library(car)
#spreadLevelPlot(fitExponential)
#Possiamo provare a trasformare Trip_Distance_km e Trip_Duration_Minutes
library(gam)
gam2 <- gam(Trip_Price^0.38 ~ . , data = d1)
summary(gam2)
gam1 <- gam(Trip_Price^0.38 ~ . -Trip_Distance_km +s(Trip_Distance_km)-Per_Km_Rate+s(Per_Km_Rate)-Per_Minute_Rate+s(Per_Minute_Rate)-Trip_Duration_Minutes+s(Trip_Duration_Minutes),data = d1)
summary(gam1)
plot(gam1)

fitUpdate <- update(fitExponential, . ~ . + I(Trip_Distance_km^(0.78)) + I(Trip_Duration_Minutes^(2)))
summary(fitUpdate)
resettest(fitUpdate, power = 2, type = "fitted")
bptest(fitUpdate)

crPlots(fitUpdate
        )


library(car)
influencePlot(fit,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


# Cook's D are contained in the fitted model (object fit)
methods(class = class(fitUpdate))

cooksd <- cooks.distance(fitUpdate)
cooksda=data.frame(cooksd)
summary(cooksd)

# cutoff of cookD  4/(n-k).. NB n should be n used in the model!!!

n_used=length(fitUpdate$residuals)
n_used
# be careful!!! 

cutoff <- 4/(n_used-length(fitUpdate$coefficients)-2)


plot(fitUpdate, which=4, cook.levels=cutoff)
abline(h=cutoff)

df_no_influential <- d1[cooksd < cutoff, ]
nrow(df_no_influential)
fitUpdate_no_influential <- lm(Trip_Price^0.38 ~ . + I(Trip_Distance_km^(0.78)) + I(Trip_Duration_Minutes^(2)), data = df_no_influential)
summary(fitUpdate_no_influential)
par(mfrow=c(2,2))
plot(fitUpdate_no_influential)
bptest(fitUpdate_no_influential)

library("car")
BOOT.MOD=Boot(fitUpdate_no_influential, R=1999)
summary(BOOT.MOD, high.moments=TRUE)

# confint boot
Confint(BOOT.MOD, level=c(.95), type="perc")
hist(BOOT.MOD, legend="separate")

Confint(BOOT.MOD, level=c(.95), type="norm")


library(tidyverse)
library(caret)

set.seed(123)
folds <- createFolds(df_no_influential$Trip_Price, k = 10)
results_rmse <- c()
results_r2adj <- c()

#Looping through folds
for(i in 1:10){
  train <- df_no_influential[-folds[[i]],]
  test <- df_no_influential[folds[[i]],]
  model <- lm(Trip_Price^0.38 ~ . + I(Trip_Distance_km^(0.78)) + I(Trip_Duration_Minutes^(2)), data = train)
  predictions <- predict(model, test)
  
  actual <- test$Trip_Price^0.38
  # RMSE
  rmse <- sqrt(mean((predictions - actual)^2, na.rm = TRUE))
  results_rmse <- c(results_rmse, rmse)
  
  # R² aggiustato sul test set
  ss_res <- sum((actual - predictions)^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm=TRUE))^2, na.rm = TRUE)
  r2 <- 1 - ss_res/ss_tot
  
  # calcolo degrees of freedom per adj R2: 
  # n = lunghezza test set, p = numero di predittori nel modello
  n <- nrow(test)
  p <- length(coef(model)) - 1
  r2_adj <- 1 - (1 - r2) * (n - 1) / (n - p - 1)
  results_r2adj <- c(results_r2adj, r2_adj)
}

mean_rmse <- mean(results_rmse, na.rm=TRUE)
mean_r2adj <- mean(results_r2adj, na.rm=TRUE)

print(mean_rmse)
print(mean_r2adj)
# Final Model Summary
final_model <- lm(Trip_Price^0.38 ~ . + I(Trip_Distance_km^(0.78)) + I(Trip_Duration_Minutes^(2)), data = df_no_influential)
summary(final_model)
# Fine Taxi.R

