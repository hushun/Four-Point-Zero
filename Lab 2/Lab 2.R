# Lab 2

ameslist <- read.table("ames.csv", header = TRUE, sep = ",")

# Try it: Run the above, but instead specifying header = FALSE . What data type are the various columns? Now try ommitting the line altogether. What is the default behavior of the read.table function?
ameslist_2 <- read.table("ames.csv", header = FALSE, sep = ",")
# If we change it to "header = FALSE", the headers will be setted in row 1, which is not correct.
# The default behavior of read. table is to convert character variables (which are not converted to logical, numeric or complex) to factors.

names(ameslist)

# Try it: Go through the variables in the dataset and make a note about your interpretation for each. Many will be obvious, but some require additional thought.
# ID: Unique identifier for each row, int
# LotArea: Size of lot (units unknown), int
# SalePrice: Sale price of house ($), int
# ...
# ...

typeof(ameslist)

unique(ameslist$GarageType)


GarageTemp = model.matrix.lm( ~ ameslist$GarageType - 1, data=ameslist$GarageType, na.action = "na.pass")

# Try it: Figure out what’s going on above. Fix this code so that you have a working version.
# The numbers of rows are different. We nee to use model.matrix.lm() instead of model.matrix(), and add na.action = "na.pass".

ameslist <- cbind(ameslist, GarageTemp)

ameslist$GarageOutside <- ifelse(ameslist$`ameslist$GarageTypeDetchd` == 1 | ameslist$`ameslist$GarageTypeCarPort` == 1, 1, 0)
unique(ameslist$GarageOutside)

# Try it: Utilizing a similar approach to what you did above, fix this so that the only outputs are zero and one.

ameslist_3 <- ameslist
ameslist_3$`ameslist$GarageTypeDetchd`[is.na(ameslist_3$`ameslist$GarageTypeDetchd`)] <- 0
ameslist_3$`ameslist$GarageTypeCarPort`[is.na(ameslist_3$`ameslist$GarageTypeCarPort`)] <- 0
ameslist$GarageOutside <- ifelse(ameslist_3$`ameslist$GarageTypeDetchd` == 1 | ameslist_3$`ameslist$GarageTypeCarPort` == 1, 1, 0)
unique(ameslist$GarageOutside)



# Exercise 1

# 1.Prune the data to all of the variables that are type = int about which you have some reasonable intuition for what they mean. This must include the variable SalePrice. Save this new dataset as Ames. Produce documentation for this object in the form of a .txt file. This must describe each of the preserved variables, the values it can take (e.g., can it be negative?) and your interpretation of the variable.

Ames <- read.table("ames_2.csv", header = TRUE, sep = ",")


# 2.Produce a scatterplot matrix which includes 12 of the variables that are type = int in the data set. Choose those that you believe are likely to be correlated with SalePrice.

library(tidyverse)
library(gapminder)
library(ggplot2)
library(grid)
library(gridExtra)

p1 <- ggplot(data=Ames,aes(x=MSSubClass, y=SalePrice)) + geom_point()
p2 <- ggplot(data=Ames,aes(x=LotFrontage, y=SalePrice)) + geom_point()
p3 <- ggplot(data=Ames,aes(x=LotArea, y=SalePrice)) + geom_point()
p4 <- ggplot(data=Ames,aes(x=OverallQual, y=SalePrice)) + geom_point()
p5 <- ggplot(data=Ames,aes(x=OverallCond, y=SalePrice)) + geom_point()
p6 <- ggplot(data=Ames,aes(x=BsmtFinSF1, y=SalePrice)) + geom_point()
p7 <- ggplot(data=Ames,aes(x=BsmtUnfSF, y=SalePrice)) + geom_point()
p8 <- ggplot(data=Ames,aes(x=TotalBsmtSF, y=SalePrice)) + geom_point()
p9 <- ggplot(data=Ames,aes(x=X1stFlrSF, y=SalePrice)) + geom_point()
p10 <- ggplot(data=Ames,aes(x=GrLivArea, y=SalePrice)) + geom_point()
p11 <- ggplot(data=Ames,aes(x=GarageCars, y=SalePrice)) + geom_point()
p12 <- ggplot(data=Ames,aes(x=GarageArea, y=SalePrice)) + geom_point()

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)

# 3.Computeamatrixofcorrelationsbetweenthesevariablesusingthe function cor() . Does this match your prior beliefs? Briefly discuss the correlation between the miscellaneous variables and SalePrice.

correlation_matrix <- matrix(1:12, nrow=4)
correlation_matrix[1,1] <- cor(Ames$MSSubClass,Ames$SalePrice)
correlation_matrix[1,2] <- cor(Ames$LotFrontage,Ames$SalePrice)
correlation_matrix[1,3] <- cor(Ames$LotArea,Ames$SalePrice)
correlation_matrix[2,1] <- cor(Ames$OverallQual,Ames$SalePrice)
correlation_matrix[2,2] <- cor(Ames$OverallCond,Ames$SalePrice)
correlation_matrix[2,3] <- cor(Ames$BsmtFinSF1,Ames$SalePrice)
correlation_matrix[3,1] <- cor(Ames$BsmtUnfSF,Ames$SalePrice)
correlation_matrix[3,2] <- cor(Ames$TotalBsmtSF,Ames$SalePrice)
correlation_matrix[3,3] <- cor(Ames$X1stFlrSF,Ames$SalePrice)
correlation_matrix[4,1] <- cor(Ames$GrLivArea,Ames$SalePrice)
correlation_matrix[4,2] <- cor(Ames$GarageCars,Ames$SalePrice)
correlation_matrix[4,3] <- cor(Ames$GarageArea,Ames$SalePrice)
correlation_matrix

# Yes, this match our prior beliefs.
# MSSubClass and OverallCond have a negative correlation with SalePrice.
# Correlation between LotFrontage and SalePrice is NA.
# All the other variables have a positive correaltion with SalePrice.

# 4.Produce a scatterplot between SalePrice and GrLivArea . Use the abline() function to plot the relationship that you’ve found in the simple linear regression.
# What is the largest outlier that is above the regression line? Produce the other information about this house?

reg <- lm(SalePrice ~ GrLivArea, data = Ames)
plot(SalePrice ~ GrLivArea, data = Ames,
     main = "SalePrice vs GrLivArea",
     pch  = 20,
     cex  = 1,
     col  = "black")
abline(reg, col="red")

# The largest outlier that is above the regression line is (4316,755000)
# An increase in overall living area of 1 ft is correlated with an expected increase in sales price of $107.


# Building a Model

lm.fit = lm(SalePrice ~ GrLivArea, data = Ames)

# Try it: What is GrLivArea ? If you did not include this variable above, check its relationship to other variables in the dataset to get a better idea what it is.
# GrLivArea should be gross living area

# Try it: Use plot() to explore the model above. Do you suspect that some outliers have a large influence on the data?
plot(SalePrice ~ GrLivArea, data = Ames,
     main = "SalePrice vs GrLivArea",
     pch  = 20,
     cex  = 1,
     col  = "black")
abline(lm.fit, col="red")


lm.fit = lm(SalePrice ~ GrLivArea + LotArea)

# Try it: Does controlling for LotArea change the qualitative conclusions from the previous regression? What about the quantitative results? Does the direction of the change in the quantitative results make sense to you?
# It does not change the qualitative conclusions, but it does change the quantitative results. The result makes sense to me.



# Exercise 2

# 1.Use the lm() function in a simple linear regression (e.g., with only one predictor) with SalePrice as the response to determine the value of an indoor garage.
reg1 <- lm(SalePrice ~ GarageArea, data = Ames)
plot(SalePrice ~ GarageArea, data = Ames,
     main = "SalePrice vs GarageArea",
     pch  = 20,
     cex  = 1,
     col  = "black")
abline(reg1, col="red")
# An increase in garage area of 1 ft is correlated with an expected increase in sales price of $232.

# 2.Use the lm() function to perform a multiple linear regression with SalePrice as the response and all other variables from your Ames data as the predictors. Use the summary() function to print the results. Comment on the output. For instance:

reg2 <- lm(SalePrice ~ Id + MSSubClass + LotFrontage + LotArea + OverallQual + 
     OverallCond + YearBuilt + YearRemodAdd + 
     MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + 
     TotalBsmtSF + X1stFlrSF + X2ndFlrSF + LowQualFinSF + 
     GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + 
     HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + 
     Fireplaces + GarageYrBlt + GarageCars + GarageArea + 
     WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + 
     ScreenPorch + PoolArea + MiscVal + MoSold + 
     YrSold + SalePrice, data = Ames)

# Is there a relationship between the predictors and the response?
# Yes, there is a relationship between the predictors and the response.

# Which predictors appear to have a statistically significant relationship to the response?
# MSSubClass, LotArea, OverallQual, OverallCond, YearBuilt, MasVnrArea, BsmtFinSF1, X1stFlrSF, X2ndFlrSF, BsmtFullBath, BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd, Fireplaces, GarageCars, WoodDeckSF, ScreenPorch, PoolArea have a statistically significant relationship to the response, SalePrice.

# What does the coefficient for the year variable suggest?
# Only YearBuilt has a statistically significant relationship to SalePrice.
# An increase in YearBuilt of 1 is correlated with an expected increase in sales price of 3.164e+02

# 3.Use the plot() function applied to your model to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit. Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?
plot(reg2)
# Yes, there is some unusually large outliers.
# Yes, there is some points with unusually high leverage.

# 4.Use the * and : symbols to fit linear regression models with some well-chosen interaction effects. Do any interactions appear to be statistically significant?

reg4 <- lm(SalePrice ~ BedroomAbvGr:GarageArea, data = Ames)
summary(reg4)
# This interaction seems to appear statistically significant

# 5.Try a few(e.g.,three)differenttransformationsofthevariables,suchas $ln(x)$, $x^2$, $\sqrt x$. Do any of these make sense to include in a model of SalePrice ? Comment on your findings.

Ames$SalePrice_ln <- log(Ames$SalePrice)
Ames$GrLivArea_ln <- log(Ames$GrLivArea)

reg5 <- lm(SalePrice_ln ~ GrLivArea, data = Ames)
plot(SalePrice_ln ~ GrLivArea, data = Ames,
     main = "SalePrice_ln vs GrLivArea",
     pch  = 20,
     cex  = 1,
     col  = "black")
abline(reg5, col="red")

summary(reg5)
# Since P-value is very small, it is statistically significant.
# ln(SalePrice) and GrLivArea have a positive correlation




