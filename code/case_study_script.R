require(magrittr)
require(tidyverse)

housing.data <- read.csv("~/Documents/MSAN/MSAN601/case study/housing.txt")

# filter out id
housing.data %<>% select(-c(Id))

# change MSSubClass into factor
housing.data$MSSubClass %<>%  as.factor()

# Replace missing data in LotFrontage with average
housing.data$LotFrontage[which(is.na(housing.data$LotFrontage))] <- mean(housing.data$LotFrontage, na.rm = TRUE)

# Replace missing data in Alley with None
housing.data$Alley <- as.character(housing.data$Alley)
housing.data$Alley[which(is.na(housing.data$Alley))] <- 'None'
housing.data$Alley %<>%  as.factor()

# Refactor YearBuilt to be the age of the building
housing.data$YearBuilt <- 2017 - housing.data$YearBuilt

# Refactor YearRemodAdd to be the number of years after last modification
housing.data$YearRemodAdd <- 2017 - housing.data$YearRemodAdd

# Replace missing data in MasVnrType with None
housing.data$MasVnrType <- as.character(housing.data$MasVnrType)
housing.data$MasVnrType[which(is.na(housing.data$MasVnrType))] <- 'None'
housing.data$MasVnrType %<>%  as.factor()

# Replace missing data in MasVnrArea with 0
housing.data$MasVnrArea[which(is.na(housing.data$MasVnrArea))] <- 0

# Change ExterQual to integers
housing.data$ExterQual <- as.numeric(factor(housing.data$ExterQual, levels = c('Po','Fa','TA','Gd','Ex'), ordered = TRUE))

# Change ExterCond to integers
housing.data$ExterCond <- as.numeric(factor(housing.data$ExterCond, levels = c('Po','Fa','TA','Gd','Ex'), ordered = TRUE))

# Change BsmtQual to integers
housing.data$BsmtQual <- as.character(housing.data$BsmtQual)
housing.data$BsmtQual[which(is.na(housing.data$BsmtQual))] <- 'None'
housing.data$BsmtQual <- as.numeric(factor(housing.data$BsmtQual, levels = c('None','Po','Fa','TA','Gd','Ex'), ordered = TRUE))

# Change BsmtCond to integers
housing.data$BsmtCond <- as.character(housing.data$BsmtCond)
housing.data$BsmtCond[which(is.na(housing.data$BsmtCond))] <- 'None'
housing.data$BsmtCond <- as.numeric(factor(housing.data$BsmtCond, levels = c('None','Po','Fa','TA','Gd','Ex'), ordered = TRUE))

# Change BsmtExposure to integers
housing.data$BsmtExposure <- as.character(housing.data$BsmtExposure)
housing.data$BsmtExposure[which(is.na(housing.data$BsmtExposure))] <- 'None'
housing.data$BsmtExposure <- as.numeric(factor(housing.data$BsmtExposure, levels = c('None','No','Mn','Av','Gd'), ordered = TRUE))

# Replace missing data in BsmtFinType1 with None
housing.data$BsmtFinType1 <- as.character(housing.data$BsmtFinType1)
housing.data$BsmtFinType1[which(is.na(housing.data$BsmtFinType1))] <- 'None'
housing.data$BsmtFinType1 %<>%  as.factor()

# Replace missing data in BsmtFinType2 with None
housing.data$BsmtFinType2 <- as.character(housing.data$BsmtFinType2)
housing.data$BsmtFinType2[which(is.na(housing.data$BsmtFinType2))] <- 'None'
housing.data$BsmtFinType2 %<>%  as.factor()

# Change HeatingQC to integers
housing.data$HeatingQC <- as.numeric(factor(housing.data$HeatingQC, levels = c('Po','Fa','TA','Gd','Ex'), ordered = TRUE))

# Change KitchenQual to integers
housing.data$KitchenQual <- as.numeric(factor(housing.data$KitchenQual, levels = c('Po','Fa','TA','Gd','Ex'), ordered = TRUE))

# Change Functional to integers
housing.data$Functional <- as.numeric(factor(housing.data$Functional, levels = c('Sal','Sev','Maj2','Maj1','Mod','Min2','Min1','Typ'), ordered = TRUE))

# Change FireplaceQu to integers
housing.data$FireplaceQu <- as.character(housing.data$FireplaceQu)
housing.data$FireplaceQu[which(is.na(housing.data$FireplaceQu))] <- 'None'
housing.data$FireplaceQu <- as.numeric(factor(housing.data$FireplaceQu, levels = c('None','Po','Fa','TA','Gd','Ex'), ordered = TRUE))

# Replace missing data in GarageType with None
housing.data$GarageType <- as.character(housing.data$GarageType)
housing.data$GarageType[which(is.na(housing.data$GarageType))] <- 'None'
housing.data$GarageType %<>%  as.factor()

# Refactor GarageYrBlt to be the number of years after last modification
housing.data$GarageYrBlt <- 2017 - housing.data$GarageYrBlt
housing.data$GarageYrBlt[which(is.na(housing.data$GarageYrBlt))] <- 0

# Change GarageFinish to integers
housing.data$GarageFinish <- as.character(housing.data$GarageFinish)
housing.data$GarageFinish[which(is.na(housing.data$GarageFinish))] <- 'None'
housing.data$GarageFinish <- as.numeric(factor(housing.data$GarageFinish, levels = c('None','Unf','RFn','Fin'), ordered = TRUE))

# Change GarageQual to integers
housing.data$GarageQual <- as.character(housing.data$GarageQual)
housing.data$GarageQual[which(is.na(housing.data$GarageQual))] <- 'None'
housing.data$GarageQual <- as.numeric(factor(housing.data$GarageQual, levels = c('None','Po','Fa','TA','Gd','Ex'), ordered = TRUE))

# Change GarageCond to integers
housing.data$GarageCond <- as.character(housing.data$GarageCond)
housing.data$GarageCond[which(is.na(housing.data$GarageCond))] <- 'None'
housing.data$GarageCond <- as.numeric(factor(housing.data$GarageCond, levels = c('None','Po','Fa','TA','Gd','Ex'), ordered = TRUE))

# Change PavedDrive to integers
housing.data$PavedDrive <- as.numeric(factor(housing.data$PavedDrive, levels = c('N','P','Y'), ordered = TRUE))

# Change PoolQC to integers
housing.data$PoolQC <- as.character(housing.data$PoolQC)
housing.data$PoolQC[which(is.na(housing.data$PoolQC))] <- 'None'
housing.data$PoolQC <- as.numeric(factor(housing.data$PoolQC, levels = c('None','Fa','TA','Gd','Ex'), ordered = TRUE))

# Change Fence to integers
housing.data$Fence <- as.character(housing.data$Fence)
housing.data$Fence[which(is.na(housing.data$Fence))] <- 'None'
housing.data$Fence <- as.numeric(factor(housing.data$Fence, levels = c('None','MnWw','GdWo','MnPrv','GdPrv'), ordered = TRUE))

# Replace missing data in MiscFeature with None
housing.data$MiscFeature <- as.character(housing.data$MiscFeature)
housing.data$MiscFeature[which(is.na(housing.data$MiscFeature))] <- 'None'
housing.data$MiscFeature %<>%  as.factor()

# Refactor YrSold to be the length of time
housing.data$YrSold <- 2017 - housing.data$YrSold

# drop the row missing 'Electrical'
housing.data %<>% na.omit() 

#------log area------#
# bofore log
plot(density(housing.data$LotArea))
# after log
plot(density(log(housing.data$LotArea+1)))




#plot(density(housing.data$TotalBsmtSF))
#plot(density(housing.data$X2ndFlrSF))
#plot(density(housing.data$LowQualFinSF))
#plot(density(housing.data$PoolArea))

housing.data$LotArea <-  log(housing.data$LotArea +1)
housing.data$MasVnrArea <-  log(housing.data$MasVnrArea +1)
#housing.data$BsmtFinSF1 <-  log(housing.data$BsmtFinSF1 +1)
#housing.data$BsmtFinSF2 <-  log(housing.data$BsmtFinSF2 +1)
#housing.data$BsmtUnfSF <-  log(housing.data$BsmtUnfSF +1)
#housing.data$TotalBsmtSF <-  log(housing.data$TotalBsmtSF +1)
housing.data$X1stFlrSF <-  log(housing.data$X1stFlrSF +1)
#housing.data$X2ndFlrSF <-  log(housing.data$X2ndFlrSF +1)
#housing.data$LowQualFinSF <-  log(housing.data$LowQualFinSF +1)
housing.data$GrLivArea <-  log(housing.data$GrLivArea +1)
#housing.data$GarageArea <-  log(housing.data$GarageArea +1)
#housing.data$WoodDeckSF <-  log(housing.data$WoodDeckSF +1)
#housing.data$OpenPorchSF <-  log(housing.data$OpenPorchSF +1)
#housing.data$EnclosedPorch <-  log(housing.data$EnclosedPorch +1)
#housing.data$X3SsnPorch <-  log(housing.data$X3SsnPorch +1)
#housing.data$ScreenPorch <-  log(housing.data$ScreenPorch +1)
#housing.data$PoolArea <-  log(housing.data$PoolArea +1)
#housing.data$SalePrice <-  log(housing.data$SalePrice +1)

#------end of log------#

#------condition/exterior------#
temp=c('Artery', 'Feedr', 'RRNn', 'RRAn', 'PosN', 'PosA', 'RRNe', 'RRAe')

for (j in 1:length(temp)){
  housing.data[[temp[j]]]=0
}

for (i in 1:length(housing.data[,1])){
  for (j in 1:length(temp)){
    if (housing.data$Condition1[i]==temp[j] | housing.data$Condition2[i]==temp[j]){
      housing.data[[temp[j]]][i]=1
    }
  }
}

temp=c('AsbShng', 'AsphShn', 'BrkComm', 'BrkFace', 'CBlock', 'CemntBd', 'HdBoard',
       'ImStucc', 'MetalSd', 'Plywood', 'PreCast', 'Stone', 'Stucco', 'VinylSd', 'Wd Sdng', 'WdShing')

for (j in 1:length(temp)){
  housing.data[[temp[j]]]=0
}

for (i in 1:length(housing.data[,1])){
  for (j in 1:length(temp)){
    if (housing.data$Exterior1st[i]==temp[j] | housing.data$Exterior2nd[i]==temp[j]){
      housing.data[[temp[j]]][i]=1
    }
  }
}

housing.data %<>% dplyr::select(-c(Condition1,Condition2,Exterior1st,Exterior2nd))
#------end------#

#tmp <- sapply(data.frame(is.na(housing.data)),sum)

#Lasso variable selection
library(glmnet)
x <- model.matrix(SalePrice~., data = housing.data)[,-1]
y <- housing.data$SalePrice
grid.lambda <- 10^seq(10,-2,length = 100)

set.seed (1)
cv.out <- cv.glmnet(x, y, alpha = 1)
best.lambda <- cv.out$lambda.min
plot(cv.out)
abline(v = log(best.lambda), col = "blue", lwd = 2)
best.lambda
lasso_cv <- glmnet(x, y, alpha = 1, lambda = best.lambda)
coef_lasso <- coef(lasso_cv)

coef_df <- data.frame(as.matrix(coef_lasso))
nrow(coef_df)

j=0
for(i in 1:nrow(coef_df)){
  if (coef_df[i,] != 0){
    j <- j + 1
    print(rownames(coef_df)[i])
  }
}
print(j)

#Linear model with y transformed
lm.model <- lm(SalePrice~.,data = housing.datasummary(lm.model)
mean(((lm.model$fitted.values)^2 - housing.data$SalePrice)^2)
library(MASS)
boxcox(lm.model)

tmp <- dffits(backwards_log)
plot(tmp, 
     ylab = "Standardized dfFits", xlab = "Index", 
     main = paste("Standardized DfFits, \n critical value = 2*sqrt(k/n) = +/-", round(cv,3)),
     pch = 20,
     cex = 0.6,
     col = 'blue')
library(olsrr)
ols_dffits_plot(backwards_log)
ols_srsd_chart(lm.model)
#Critical Value horizontal lines
abline(h = cv, lty = 2)
abline(h = -cv, lty = 2)

n <- nrow(housing.data)
k <- length(lm.model$coefficients)-1
cv <- 2*sqrt(k/n)

plot(tmp)



e <- lm.model$residuals
std.resi <- (e-mean(e))/sd(e)
ks.test(std.resi, rnorm(1000))


#Linear model with selected variables
lm.model2 <- lm(SalePrice~MSSubClass + LotArea + Street+ LotShape + 
                  LandContour + Utilities + LotConfig + LandSlope + Neighborhood + 
                  BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd +
                  RoofStyle + RoofMatl + MasVnrType + ExterQual +
                  Foundation + BsmtQual + BsmtExposure + 
                  BsmtFinType1 + BsmtFinSF1 + TotalBsmtSF + Heating + X1stFlrSF + X2ndFlrSF + 
                  BsmtFullBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + 
                  Functional + Fireplaces + GarageType + GarageYrBlt + GarageFinish + 
                  GarageCars + WoodDeckSF + X3SsnPorch + ScreenPorch + PoolQC + MiscFeature + 
                  MoSold + SaleType + SaleCondition + Artery + Feedr + PosN + PRAe + BrkFace + CemntBd +
                  HdBoard + ImStucc + Stucco, data = housing.data)

par(mfrow=c(2,2))
plot(lm.model2, add.smooth = F)

housing.data <- housing.data[-c(584, 1004, 1231),]

sqrt(mean((lm.model2$fitted.values - housing.data$SalePrice)^2))


#colinearity
library(faraway)
mod_tst <- lm(SalePrice ~ ., data = housing.data)
vif(mod_tst) > 10

for (i in 1:length(housing.data)){
  if vif(mod_tst)[i] > 10{
    print(vif(mod_tst)[i])
  }
}


