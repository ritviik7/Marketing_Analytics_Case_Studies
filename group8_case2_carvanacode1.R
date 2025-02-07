#######################
# Logistic Regression #
####################### 

## Load Packages (if needed) and Set Seed

set.seed(1)

## Read in Logistic Regression data

logit <- read.csv(file.choose()) ## Choose training.csv file

## Transform and Create Data

## Dealing with Variables that have NULL Values
logit$MMRAcquisitionAuctionAveragePrice <- as.numeric(ifelse(logit$MMRAcquisitionAuctionAveragePrice == "NULL", 0, logit$MMRAcquisitionAuctionAveragePrice))
logit$MMRAcquisitionRetailAveragePrice <- as.numeric(ifelse(logit$MMRAcquisitionRetailAveragePrice == "NULL", 0, logit$MMRAcquisitionRetailAveragePrice))
logit$MMRCurrentAuctionAveragePrice <- as.numeric(ifelse(logit$MMRCurrentAuctionAveragePrice == "NULL", 0, logit$MMRCurrentAuctionAveragePrice))
logit$MMRCurrentRetailAveragePrice <- as.numeric(ifelse(logit$MMRCurrentRetailAveragePrice == "NULL", 0, logit$MMRCurrentRetailAveragePrice))

## Dealing with Variables that have outliers
logit$lnVehBCost <- log(logit$VehBCost + 1)

## Dealing with Variables that have different categories in training and test
logit$VehYear_2005 = ifelse(logit$VehYear == "2005",1,0)
logit$Make_Chevrolet = ifelse(logit$Make == "CHEVROLET",1,0)
logit$Make_Ford = ifelse(logit$Make == "FORD",1,0)
logit$Make_Chrysler = ifelse(logit$Make == "CHRYSLER",1,0)
logit$Model_PTCRUISER = ifelse(logit$Model == "PT CRUISER",1,0)
logit$State_TX = ifelse(logit$VNST == "TX",1,0)
logit$State_FL = ifelse(logit$VNST == "FL",1,0)
logit$WheelType_Alloy = ifelse(logit$WheelType == "Alloy",1,0)
logit$WheelType_Covers = ifelse(logit$WheelType == "Covers",1,0)
logit$WheelType_Special = ifelse(logit$WheelType == "Special",1,0)
logit$VNZIP1_32824 = ifelse(logit$VNZIP1 == "32824",1,0)
logit$VNZIP1_27542 = ifelse(logit$VNZIP1 == "27542",1,0)
logit$VehSize_MediumSUV = ifelse(logit$Size == "MEDIUM SUV",1,0)


## Run Logistic Regression using GLM

logit_result <- glm(formula = IsBadBuy ~ VehicleAge + lnVehBCost + VehYear_2005 + Make_Chevrolet + Model_PTCRUISER + State_TX + State_FL +
                      WheelType_Alloy + WheelType_Covers + WheelType_Special + VNZIP1_27542 + VehSize_MediumSUV + 
                      MMRCurrentRetailAveragePrice + MMRCurrentAuctionAveragePrice + MMRAcquisitionRetailAveragePrice + MMRAcquisitionAuctionAveragePrice, data = logit, family = "binomial")
summary(logit_result)

## Pseudo R-square - McFadden

null_result <- glm(formula = IsBadBuy ~ 1, data = logit, family = "binomial")
1 - logLik(logit_result)/logLik(null_result)

## Odds Ratio
exp(logit_result$coefficients)

## Predicted Probability

logit$predict <- predict(logit_result, logit, type = "response")

## This is just the decile chart of probabilities
quantile(logit$predict, prob = seq(0,1,length=11), type=5) 

## Hit Rate Table

logit$pIsBadBuy <- ifelse(logit$predict >= 0.5, 1, 0)
hitrate <- table(logit$IsBadBuy, logit$pIsBadBuy)
hitrate
sum(diag(hitrate))/sum(hitrate)   
  
## Read in holdout data

logit_test <- read.csv(file.choose()) ## Choose test.csv file

## Transform and Create Data - Create same variables as above; just add _test to table name
logit_test$MMRAcquisitionAuctionAveragePrice <- as.numeric(ifelse(logit_test$MMRAcquisitionAuctionAveragePrice == "NULL", 0, logit_test$MMRAcquisitionAuctionAveragePrice))
logit_test$MMRAcquisitionRetailAveragePrice <- as.numeric(ifelse(logit_test$MMRAcquisitionRetailAveragePrice == "NULL", 0, logit_test$MMRAcquisitionRetailAveragePrice))
logit_test$MMRCurrentAuctionAveragePrice <- as.numeric(ifelse(logit_test$MMRCurrentAuctionAveragePrice == "NULL", 0, logit_test$MMRCurrentAuctionAveragePrice))
logit_test$MMRCurrentRetailAveragePrice <- as.numeric(ifelse(logit_test$MMRCurrentRetailAveragePrice == "NULL", 0, logit_test$MMRCurrentRetailAveragePrice))
logit_test$lnVehBCost <- log(logit_test$VehBCost + 1)
logit_test$VehYear_2005 = ifelse(logit_test$VehYear == "2005",1,0)
logit_test$Make_Chevrolet = ifelse(logit_test$Make == "CHEVROLET",1,0)
logit_test$Make_Ford = ifelse(logit_test$Make == "FORD",1,0)
logit_test$Make_Chrysler = ifelse(logit_test$Make == "CHRYSLER",1,0)
logit_test$Model_PTCRUISER = ifelse(logit_test$Model == "PT CRUISER",1,0)
logit_test$State_TX = ifelse(logit_test$VNST == "TX",1,0)
logit_test$State_FL = ifelse(logit_test$VNST == "FL",1,0)
logit_test$WheelType_Alloy = ifelse(logit_test$WheelType == "Alloy",1,0)
logit_test$WheelType_Covers = ifelse(logit_test$WheelType == "Covers",1,0)
logit_test$WheelType_Special = ifelse(logit_test$WheelType == "Special",1,0)
logit_test$VNZIP1_32824 = ifelse(logit_test$VNZIP1 == "32824",1,0)
logit_test$VNZIP1_27542 = ifelse(logit_test$VNZIP1 == "27542",1,0)
logit_test$VehSize_MediumSUV = ifelse(logit_test$Size == "MEDIUM SUV",1,0)

## Predicted Probability for Holdout

logit_test$predict <- predict(logit_result, logit_test, type = "response")
logit_test$IsBadBuy <- ifelse(logit_test$predict >= 0.5, 1, 0)
table(logit_test$IsBadBuy) ## This shows you how many 0s and 1s you predicted

## Create Entry Table for Kaggle

example_entry <- logit_test[c("RefId", "IsBadBuy")]

## Export Logistic Regression Results 

write.csv(example_entry, file = file.choose(new=TRUE), row.names = FALSE) ## Name file example_entry.csv


