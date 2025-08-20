library(readxl)
library(ggplot2)
train_data <- read_excel("~/Downloads/BarcelonaRE_Data.xlsx", sheet = "413 properties for analysis")
test_data <- read_excel("~/Downloads/BarcelonaRE_Data.xlsx", sheet = "200 properties to be priced")

qplot(train_data$m2,train_data$Price,  xlab = "m2", ylab = "Price")
qplot(train_data$Rooms,train_data$Price,  xlab = "Rooms", ylab = "Price")
qplot(train_data$Bathrooms,train_data$Price,  xlab = "Bathrooms", ylab = "Price")
qplot(train_data$Elevator,train_data$Price,  xlab = "Elevator", ylab = "Price")
qplot(train_data$Terrasse,train_data$Price,  xlab = "Terrasse", ylab = "Price")
qplot(train_data$Parking,train_data$Price,  xlab = "Parking", ylab = "Price")
qplot(train_data$Kitchen,train_data$Price,  xlab = "Kitchen", ylab = "Price")
qplot(train_data$Yard,train_data$Price,  xlab = "Yard", ylab = "Price")
qplot(train_data$Atico,train_data$Price,  xlab = "Atico", ylab = "Price")

# Set Ciutat Vella as the base zone
Z1 <- ifelse(train_data$City.Zone=="Eixample", 1, 0)
Z2 <- ifelse(train_data$City.Zone=="Gràcia", 1, 0)
Z3 <- ifelse(train_data$City.Zone=="Horta - Guinardó", 1, 0)
Z4 <- ifelse(train_data$City.Zone=="Les Corts", 1, 0)
Z5 <- ifelse(train_data$City.Zone=="Nou Barris", 1, 0)
Z6 <- ifelse(train_data$City.Zone=="Sant Andreu", 1, 0)
Z7 <- ifelse(train_data$City.Zone=="Sant Marti", 1, 0)
Z8 <- ifelse(train_data$City.Zone=="Sants - Montjuïc", 1, 0)
Z9 <- ifelse(train_data$City.Zone=="Sarria - Sant Gervasi", 1, 0)
train_data <- data.frame(train_data, "Z1" = Z1, "Z2" = Z2, "Z3" = Z3,
                         "Z4" = Z4,"Z5" = Z5,"Z6" = Z6,"Z7" = Z7,"Z8" = Z8,"Z9" = Z9)
Z1 <- ifelse(test_data$City.Zone=="Eixample", 1, 0)
Z2 <- ifelse(test_data$City.Zone=="Gràcia", 1, 0)
Z3 <- ifelse(test_data$City.Zone=="Horta - Guinardó", 1, 0)
Z4 <- ifelse(test_data$City.Zone=="Les Corts", 1, 0)
Z5 <- ifelse(test_data$City.Zone=="Nou Barris", 1, 0)
Z6 <- ifelse(test_data$City.Zone=="Sant Andreu", 1, 0)
Z7 <- ifelse(test_data$City.Zone=="Sant Marti", 1, 0)
Z8 <- ifelse(test_data$City.Zone=="Sants - Montjuïc", 1, 0)
Z9 <- ifelse(test_data$City.Zone=="Sarria - Sant Gervasi", 1, 0)
test_data <- data.frame(test_data, "Z1" = Z1, "Z2" = Z2, "Z3" = Z3,
                         "Z4" = Z4,"Z5" = Z5,"Z6" = Z6,"Z7" = Z7,"Z8" = Z8,"Z9" = Z9)
View(test_data)


#Model 1
model_1 <- lm(Price~m2+Rooms+Bathrooms+Elevator+Terrasse+Parking+Kitchen+
                Yard+Atico+Z1+Z2+Z3+Z4+Z5+Z6+Z7+Z8+Z9,data=train_data)

summary(model_1)
forecast_1 <- predict(model_1, newdata = test_data, interval = "prediction")
hist(residuals(model_1), breaks = 100)
plot(fitted.values(model_1), residuals(model_1))
plot(train_data$m2, residuals(model_1))
plot(train_data$m2, train_data$Price)
abline(model_1)


MeanStErrFcst_1 <- mean((forecast_1[,"fit"] - forecast_1[,"lwr"])/qnorm(1-.05/2))

#Model 2
train_data <- data.frame(train_data, "sqrm2" = (train_data$m2)^2)
test_data <- data.frame(test_data, "sqrm2" = (test_data$m2)^2)
head(train_data)
model_2 <- lm(Price ~ sqrm2+Rooms+Bathrooms+Elevator+Terrasse+Parking+Kitchen+
                Yard+Atico+Z1+Z2+Z3+Z4+Z5+Z6+Z7+Z8+Z9,data=train_data)
summary(model_2)
forecast_2 <- predict(model_2, newdata = test_data, interval = "prediction")
hist(residuals(model_2), breaks = 100)
plot(fitted.values(model_2), residuals(model_2))
MeanStErrFcst_2 <- mean((forecast_2[,"fit"] - forecast_2[,"lwr"])/qnorm(1-.05/2))

#Model 3
train_data <- data.frame(train_data, "lnPrice" = log(train_data$Price))
train_data <- data.frame(train_data, "lnm2" = log(train_data$m2))
head(train_data)
model_3 <- lm(lnPrice ~ lnm2+Rooms+Bathrooms+Elevator+Terrasse+Parking+Kitchen+
                Yard+Atico+Z1+Z2+Z3+Z4+Z5+Z6+Z7+Z8+Z9,data=train_data)
summary(model_3)
test_data <- data.frame(test_data,"lnm2"=log(test_data$m2))
forecast_3 <- predict(model_3, newdata = test_data, interval = "prediction")
hist(residuals(model_3), breaks = 100)
plot(fitted.values(model_3), residuals(model_3))
MeanStErrFcst_3 <- mean((exp(forecast_3[,"fit"]) - exp(forecast_3[,"lwr"]))/qnorm(1-.05/2))


#Model 4
qplot(train_data$m2,train_data$Rooms,  xlab = "m2", ylab = "Rooms")
train_data <- data.frame(train_data, "m2*Rooms" = train_data$m2*train_data$Rooms)
test_data <- data.frame(test_data, "m2*Rooms" = test_data$m2*test_data$Rooms)
model_4 <- lm(lnPrice ~ lnm2+Rooms+m2*Rooms+Bathrooms+Elevator+Terrasse+Parking+Kitchen+
                Yard+Atico+Z1+Z2+Z3+Z4+Z5+Z6+Z7+Z8+Z9,data=train_data)
summary(model_4)
forecast_4 <- predict(model_4, newdata = test_data, interval = "prediction")
hist(residuals(model_4), breaks = 100)
plot(fitted.values(model_4), residuals(model_4))
MeanStErrFcst_4 <- mean((forecast_4[,"fit"] - forecast_4[,"lwr"])/qnorm(1-.05/2))

#Model 5
qplot(train_data$Bathrooms,train_data$Rooms,  xlab = "Bathrooms", ylab = "Rooms")
train_data <- data.frame(train_data, "Bathrooms*Rooms" = train_data$Bathrooms*train_data$Rooms)
test_data <- data.frame(test_data, "Bathrooms*Rooms" = test_data$Bathrooms*test_data$Rooms)
model_5 <- lm(lnPrice ~ lnm2+Rooms+Bathrooms*Rooms+Bathrooms+Elevator+Terrasse+Parking+Kitchen+
                Yard+Atico+Z1+Z2+Z3+Z4+Z5+Z6+Z7+Z8+Z9,data=train_data)
summary(model_5)
forecast_5 <- predict(model_5, newdata = test_data, interval = "prediction")
hist(residuals(model_5), breaks = 100)
plot(fitted.values(model_5), residuals(model_5))
MeanStErrFcst_5 <- mean((forecast_5[,"fit"] - forecast_5[,"lwr"])/qnorm(1-.05/2))


#Model 6

model_6 <- lm(lnPrice ~ lnm2+Bathrooms+Elevator+Terrasse+Parking+Kitchen+
                Yard+Atico+Z1+Z2+Z5+Z6+Z7+Z8+Z9,data=train_data)
summary(model_6)
forecast_6 <- predict(model_6, newdata = test_data, interval = "prediction")
forecast_6
hist(residuals(model_6), breaks = 100)
plot(fitted.values(model_6), residuals(model_6))
MeanStErrFcst_6 <- mean((exp(forecast_6[,"fit"]) - exp(forecast_6[,"lwr"]))/qnorm(1-.05/2))
MeanStErrFcst_6

#Model 7 (Final)

train_data <- data.frame(train_data, "facilities"=train_data$Elevator*train_data$Parking)
test_data <- data.frame(test_data, "facilities"=test_data$Elevator*test_data$Parking)

model_7 <- lm(lnPrice ~ lnm2+Bathrooms+Elevator+Terrasse+Parking+Kitchen+
                Yard+Atico+facilities+Z1+Z2+Z5+Z6+Z7+Z8+Z9,data=train_data)
summary(model_7)
forecast_7 <- predict(model_7, newdata = test_data, interval = "prediction")
forecast_7 <- exp(forecast_7)
forecast_7
hist(residuals(model_7), breaks = 100)
plot(fitted.values(model_7), residuals(model_7))
plot(train_data$lnm2, residuals(model_7))
plot(train_data$Bathrooms, residuals(model_7))
plot(train_data$Elevator, residuals(model_7))
plot(train_data$Terrasse, residuals(model_7))
plot(train_data$Parking, residuals(model_7))
plot(train_data$Kitchen, residuals(model_7))
plot(train_data$Yard, residuals(model_7))
plot(train_data$Atico, residuals(model_7))
plot(train_data$facilities, residuals(model_7))
plot(train_data$Z1, residuals(model_7))
plot(train_data$Z2, residuals(model_7))
plot(train_data$Z5, residuals(model_7))
plot(train_data$Z6, residuals(model_7))
plot(train_data$Z7, residuals(model_7))
plot(train_data$Z8, residuals(model_7))
plot(train_data$Z9, residuals(model_7))
