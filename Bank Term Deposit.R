setwd("C:/Users/Hp/Desktop/R Programming")
getwd()

library(corrplot)
library(dplyr)
dim(TrainingData)
TrainingData <- read.csv("train_set.csv", header = TRUE)
head(TrainingData)
summary(TrainingData)
str(TrainingData)
TrainingData$ID <- NULL
TrainingData$emp.var.rate <- NULL
TrainingData$Term <- as.factor(TrainingData$Term)
summary(TrainingData$Term)

colSums(is.na(TrainingData))
TrainingData$age[is.na(TrainingData$age)] = mean(TrainingData$age, na.rm = T)
TrainingData$duration[is.na(TrainingData$duration)] = 0
TrainingData$campaign[is.na(TrainingData$campaign)] = median(TrainingData$campaign, na.rm = T) 
TrainingData$pdays[is.na(TrainingData$pdays)] = 999
TrainingData$previous[is.na(TrainingData$previous)] = 0
TrainingData$cons.price.idx[is.na(TrainingData$cons.price.idx)] = mean(TrainingData$cons.price.idx, na.rm = T)
TrainingData$cons.conf.idx[is.na(TrainingData$cons.conf.idx)] = median(TrainingData$cons.conf.idx, na.rm = T)
TrainingData$euribor3m[is.na(TrainingData$euribor3m)] = mean(TrainingData$cons.conf.idx, na.rm = T)
TrainingData$nr.employed[is.na(TrainingData$nr.employed)] = mean(TrainingData$nr.employed, na.rm = T)

Matrix <- cor(TrainingData[,c(1,11,12,13,14,16,17,18,19)])
corrplot(Matrix, method = "pie", type = "upper" )

Matrix1 <- cor(TrainingData[,c(1,11,12,13,16,17)])
corrplot(Matrix1, method = "pie", type = "upper" )

boxplot(TrainingData$duration) #Dictionary says its imp
boxplot(TrainingData$campaign) #Outlier Treatment
boxplot(TrainingData$pdays) 
boxplot(TrainingData$cons.price.idx)
boxplot(TrainingData$cons.conf.idx)
boxplot(TrainingData$age)

IQRage = IQR(TrainingData$age)
LLage = quantile(TrainingData$age,0.25) - 1.5*IQRage
ULage = quantile(TrainingData$age,0.75) + 1.5*IQRage
ageOut = subset(TrainingData, age < LLage | age > ULage)
dim(ageOut)

ageWOUT = subset(TrainingData, age >= LLage & age <= ULage)
dim(ageWOUT)
max(ageWOUT$age)
summary(TrainingData$age)
TrainingData$age[TrainingData$age > 72] = 72
summary(TrainingData$age)

IQRDuration = IQR(TrainingData$duration)
LLDuration = quantile(TrainingData$duration,0.25) - 1.5*IQRDuration
ULDuration = quantile(TrainingData$duration,0.75) + 1.5*IQRDuration
DurationOut = subset(TrainingData, duration < LLDuration | duration > ULDuration)
dim(DurationOut)

DurationWOUT = subset(TrainingData, duration >= LLDuration & duration <= ULDuration)
dim(DurationWOUT)
max(DurationWOUT$duration)

summary(TrainingData$duration)
TrainingData$duration[TrainingData$duration > 683] = 683
summary(TrainingData$duration)


IQRCampaign = IQR(TrainingData$campaign)
LLCampaign = quantile(TrainingData$campaign,0.25) - 1.5*IQRCampaign
ULCampaign = quantile(TrainingData$campaign,0.75) + 1.5*IQRCampaign
CampaignOut = subset(TrainingData, campaign < LLCampaign | campaign > ULCampaign)
dim(CampaignOut)

summary(TrainingData$campaign)
ULCampaign
TrainingData$campaign[TrainingData$campaign > 6] = NA
library(mice)
md.pattern(TrainingData)
mymice = mice(TrainingData, m = 5, method = "pmm")
mymice$imp$campaign
mymiceComplete = complete(mymice, 3)
summary(mymiceComplete)
summary(TrainingData)
summary(mymiceComplete)
TrainingData = mymiceComplete

IQRcpi = IQR(TrainingData$cons.price.idx)
LLcpi = quantile(TrainingData$cons.price.idx,0.25) - 1.5*IQRcpi
ULcpi = quantile(TrainingData$cons.price.idx,0.75) + 1.5*IQRcpi
cpiOut = subset(TrainingData, cons.price.idx < LLcpi | cons.conf.idx > ULcpi)
dim(cpiOut)

cpiWOUT = subset(TrainingData, cons.price.idx >= LLcpi & cons.price.idx <= ULcpi)
dim(cpiWOUT)
max(cpiWOUT$cons.price.idx)
median(cpiWOUT$cons.price.idx)

TrainingData$cons.price.idx[TrainingData$cons.price.idx > 94.215] = 93.2
summary(TrainingData)


summary(TrainingData$job)
TrainingData$job[TrainingData$job == ""] = "unknown"
droplevels(TrainingData$job)
TrainingData$job <- as.factor(TrainingData$job)

summary(TrainingData$marital)
TrainingData$marital[TrainingData$marital == ""] <- "unknown"
droplevels(TrainingData$marital)
TrainingData$marital <- as.character(TrainingData$marital)
TrainingData$marital <- as.factor(TrainingData$marital)

summary(TrainingData$education)
TrainingData$education[TrainingData$education == ""] <- "unknown"
droplevels(TrainingData$education)
TrainingData$education <- as.character(TrainingData$education)
TrainingData$education <- as.factor(TrainingData$education)

summary(TrainingData$default)
TrainingData$default[TrainingData$default == ""] <- "unknown"
droplevels(TrainingData$default)
TrainingData$default <- as.character(TrainingData$default)
TrainingData$default <- as.factor(TrainingData$default)

summary(TrainingData$housing)
TrainingData$housing[TrainingData$housing == ""] <- "unknown"
droplevels(TrainingData$housing)
TrainingData$housing <- as.character(TrainingData$housing)
TrainingData$housing <- as.factor(TrainingData$housing)

summary(TrainingData$loan)
TrainingData$loan[TrainingData$loan == ""] <- "unknown"
TrainingData$loan[TrainingData$loan == 0] <- "unknown"
droplevels(TrainingData$loan)
TrainingData$loan <- as.character(TrainingData$loan)
TrainingData$loan <- as.factor(TrainingData$loan)

summary(TrainingData$contact)
TrainingData$contact[TrainingData$contact == ""] <- "cellular"
droplevels(TrainingData$contact)
TrainingData$contact <- as.character(TrainingData$contact)
TrainingData$contact <- as.factor(TrainingData$contact)

summary(TrainingData$month)
TrainingData$month[TrainingData$month == ""] <- "jul"
droplevels(TrainingData$month)
TrainingData$month <- as.character(TrainingData$month)
TrainingData$month <- as.factor(TrainingData$month)

summary(TrainingData$month)
TrainingData$month[TrainingData$month == ""] <- "jul"
droplevels(TrainingData$month)
TrainingData$month <- as.character(TrainingData$month)
TrainingData$month <- as.factor(TrainingData$month)

summary(TrainingData$day_of_week)
TrainingData$day_of_week[TrainingData$day_of_week == ""] <- "tue"
droplevels(TrainingData$day_of_week)
TrainingData$day_of_week <- as.character(TrainingData$day_of_week)
TrainingData$day_of_week <- as.factor(TrainingData$day_of_week)

summary(TrainingData$poutcome)
TrainingData$poutcome[TrainingData$poutcome == ""] <- "nonexistent"
droplevels(TrainingData$poutcome)
TrainingData$poutcome <- as.character(TrainingData$poutcome)
TrainingData$poutcome <- as.factor(TrainingData$poutcome)

#Sample

Log.Reg <- glm(Term~., data = TrainingData[,c(-1,-3,-4,-10,-14,-18,-19)], family = binomial(link = "logit"))
summary(Log.Reg)

library(rms)
vif(Log.Reg)

plot(Log.Reg$fitted.values)
plot(TrainingData$Term,Log.Reg$fitted.values)
Predicted.train = predict(Log.Reg, data = TrainingData, type = "response")
table(TrainingData$Term, Predicted.train > 0.21)
summary(TrainingData$Term)

TestData <- read.csv("test_set.csv", header = TRUE)
head(TestData)
summary(TestData)
str(TestData)
TestData$job[TestData$job == "blue-collar"] <- "unknown"
TestData$job[TestData$job == "self-employed"] <- "unknown"
TestData$job[TestData$job == ""] <- "unknown"
droplevels(TestData$job)
TestData$job <- as.character(TestData$job)
TestData$job <- as.factor(TestData$job)

TestData$age <- NULL
TestData$default[TestData$default == ""] <- "unknown"
TestData$housing[TestData$housing == ""] <- "unknown"
TestData$loan[TestData$loan == ""] <- "unknown"
TestData$contact[TestData$contact == ""] <- "cellular"
TestData$poutcome[TestData$poutcome == ""] <- "nonexistent"
TestData$education[TestData$education == ""] <- "unknown"
TestData$day_of_week[TestData$day_of_week == ""] <- "tue"
dim(TestData)

Predicted.Test = predict(Log.Reg, newdata = TestData, type = "response")
TestData$Term <- ifelse(Predicted.Test>0.21, "1", "0")
table(TestData$Term)
dim(TestData)

TestData$Term <- as.factor(TestData$Term)
Dataframe <- subset(TestData[,c(1,22)])
head(Dataframe)
summary(Dataframe)

write.csv(Dataframe, "C:/Users/Hp/Desktop/R Programming/Sub.csv")

library(rpart)
library(ipred)

bank.bagging <- bagging(Term~., data = TrainingData,
                        control=rpart.control(maxdepth=4, minsplit=13))


TestData$Term <- predict(bank.bagging, TestData)
summary(TestData$Term)
dim(TestData)

TestData$Term <- as.factor(TestData$Term)
Dataframe <- subset(TestData[,c(1,22)])
head(Dataframe)
summary(Dataframe)
dim(Dataframe)

write.csv(Dataframe, "C:/Users/Hp/Desktop/R Programming/Sub.csv")

library(gbm)
str(TrainingData)
gbm.fit.cars <- gbm(formula = Term~.,
                    distribution = "bernoulli",
                    data = TrainingData[,c(-3,-5,-6,-7,-8,-10,-12,-13,-14,-16,-18,-19)],
                    n.trees = 10000,
                    interaction.depth = 5,
                    shrinkage = .001,
                    cv.folds = 5,
                    n.cores = NULL,
                    verbose = FALSE)

summary(gbm.fit.cars)

GB.Fit <- predict(gbm.fit.cars, newdata = TestData, type = "response")
head(TestData)
TestData$Term <- ifelse(GB.Fit > 0.5, "1", "0")
table(TestData$Term)

TestData$Term <- as.factor(TestData$Term)
Dataframe <- subset(TestData[,c(1,22)])
head(Dataframe)
summary(Dataframe)

write.csv(Dataframe, "C:/Users/Hp/Desktop/R Programming/Sub.csv")

library(xgboost)

str(TrainingData)
str(TestData)

TrainingData$Term <- as.numeric(TrainingData$Term)
TrainingData <- transform(TrainingData, Term = Term - 1)


GD.Features.Train <- as.matrix(TrainingData[,c(1,11,12,13,14,16,17,18,19)])
GD.Label <- as.matrix(TrainingData[,20])
GD.Features.Test <- as.matrix(TestData[,c(2,12,13,14,15,18,19,20,21)])

XGB.Fit.Cars <- xgboost(data = GD.Features.Train,
                        label = GD.Label,
                        eta = 0.00001,
                        max_depth = 6,
                        min_child_weight = 3,
                        nrounds = 10000,
                        nfold = 15,
                        objective = "binary:logistic",
                        verbose = 1,               
                        early_stopping_rounds = 25)
XGB.Fit.Cars

XGB.Pred.Class.Test <- predict(XGB.Fit.Cars,GD.Features.Test)
TestData$Term <- ifelse(XGB.Pred.Class.Test > 0.5, "1" , "0" )

TestData$Term <- as.factor(TestData$Term)
Dataframe <- subset(TestData[,c(1,22)])
head(Dataframe)
summary(Dataframe)

write.csv(Dataframe, "C:/Users/Hp/Desktop/R Programming/Sub.csv")


