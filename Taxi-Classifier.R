# Loading the Data
ds = read.csv("https://raw.githubusercontent.com/marcel0501/Robust-Model-Taxi-Price-/refs/heads/main/taxi_trip_pricing.csv")
summary(ds$Trip_Price^0.38)
hist(ds$Trip_Price^0.38)
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
##We'll proceed by transforming our response variable in a binary one by using CoxBox + Median 
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
#boxcoxreg1 <- boxcox(fit1)
#lambda1 <- boxcoxreg1$x[which(boxcoxreg1$y==max(boxcoxreg1$y))]
#lambda1
#Lambda is about 0.38
#d1$Trip_Price <- d1$Trip_Price^0.38
#We will divide our response variable in Low Price and High Price based on median
median_price <- median(d1$Trip_Price)
d1$Trip_Price <- ifelse(d1$Trip_Price <= median_price, 0, 1)

table(d1$Trip_Price)
prop.table(table(d1$Trip_Price))

fit2<- glm(Trip_Price ~ ., data = d1, family = binomial)
summary(fit2)
#AIC 626.82

#Let's see the coefficients
library(coefplot)
coefplot(fit2, intercept = FALSE, main = "Coefficient Plot for Logistic Regression Model")

library(forestmodel)
print(forest_model(fit2),text_size = 5)

drop1(fit2, test="LRT")
summary(fit2)

#Deviance of full model vs null model
null = glm(Trip_Price ~ 1, data=d1,family = binomial)
summary(null)
summary(fit2)

R2 = 1- (fit2$deviance / null$deviance)
R2

fit3 = glm(Trip_Price ~ Trip_Distance_km+Per_Km_Rate+Per_Minute_Rate+Trip_Duration_Minutes+Passenger_Count, data = d1, family = binomial)
summary(fit3)

fit3$deviance-fit2$deviance
anova(fit3,fit2, test="LRT")
R2 = 1- (fit3$deviance / null$deviance)
R2
par(mfrow=c(2,2)) 
plot(fit3)

#Accuracy
predicted_probabilities <- predict(fit3, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
confusion_matrix <- table(d1$Trip_Price, predicted_classes)
confusion_matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
#Accuracy 0.887 GREAT

#Check Zero Variance , Collinearity, Separation
#Zero variance
library(funModeling)
library(dplyr)
status=df_status(d1, print_results = F)
status

#No zero variance variables (Trip_Distance_km+Per_Km_Rate+Per_Minute_Rate+Trip_Duration_Minutes+Passenger_Count+Base_Fare

#Collinearity 
library(mctest)
imcdiag(fit3)

#No collinearity issues detected

#Separation


table(d1$Trip_Price, d1$Passenger_Count) #Ok 
table(d1$Trip_Price, d1$Per_Km_Rate) # Ok
table(d1$Trip_Price, d1$Per_Minute_Rate) #Ok
table(d1$Trip_Price, d1$Trip_Distance_km) #Ok


library(ggplot2)
ggplot(d1, aes(x = d1$Trip_Duration_Minutes, y = d1$Trip_Price)) +
  geom_jitter(height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) #Ok 
library(ggplot2)¯
ggplot(d1, aes(x = d1$Trip_Distance_km, y = d1$Trip_Price)) +
  geom_jitter(height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) #Ok ¯\_(ツ)_/¯

#All ok finish of the MODEL
