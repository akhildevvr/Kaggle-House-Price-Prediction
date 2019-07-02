setwd("C:\\Users\\Jose Chiramel\\Desktop\\excelr new batch\\Project")
traindata<-read.csv("train.csv", header = TRUE,stringsAsFactors = FALSE)
testdata<-read.csv("test.csv", header = TRUE,stringsAsFactors = FALSE)
View(traindata)
View(testdata)
 str(traindata)
summary(traindata)
traindata<-traindata[,-1]

#Drop factor variables with less than 2 levels & keep non-factor vars
names(traindata) <- make.names(names(traindata))

features <- setdiff(colnames(traindata), c("Id", "SalePrice"))
for (f in features) {
  if (any(is.na(traindata[[f]]))) 
    if (is.character(traindata[[f]])){ 
      traindata[[f]][is.na(traindata[[f]])] <- "None"
    }else{
      traindata[[f]][is.na(traindata[[f]])] <- mean(traindata[[f]], na.rm = TRUE) 
    }
}

column_class <- lapply(traindata,class)
column_class <- column_class[column_class != "factor"]
factor_levels <- lapply(traindata, nlevels)
factor_levels <- factor_levels[factor_levels > 1]
traindata <- traindata[,names(traindata) %in% c(names(factor_levels), names(column_class))]

traindata <- as.data.frame(unclass(traindata))


traindata$MSZoning <- factor(traindata$MSZoning, levels=c("None","A", "C (all)", "FV", "I", "RH","RL","RP","RM"))

levels_Utilities<- c("AllPub","NoSeWa","NoSewr", "ELO","None")
traindata$Utilities<- factor(traindata$Utilities, levels=levels_Utilities)


str(traindata)
levels(traindata$Condition2)
levels_Condition2<-c("Artery", "Feedr", "Norm", "PosA", "PosN", "RRAe",  "RRAn", "RRNn", "RRNe")
traindata$Condition2 <- factor(traindata$Condition2, levels=levels_Condition2)
str(traindata$Condition2)

traindata$YearBuilt <- as.integer(traindata$YearBuilt)

anyNA(traindata$Exterior1st) # Checking if NA shoulw be a level
levels_Exterior <- c("AsbShng", "AsphShn","Brk Cmn", "BrkFace", "CBlock", "CmentBd", "PreCast","HdBoard","None","ImStucc", "MetalSd","Other", "Stone", "Stucco","VinylSd","Wd Sdng","Wd Shng")
traindata$Exterior1st<- factor(traindata$Exterior1st, levels=c(levels(traindata$Exterior1st),"PreCast", "Other","None"))
levels(traindata$Exterior1st)

traindata$Exterior2nd<- factor(traindata$Exterior2nd, levels=c(levels(traindata$Exterior2nd),"PreCast"))
levels(traindata$Exterior2nd)

traindata$ExterQual <- factor(traindata$ExterQual, levels=c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
traindata$ExterQual<- factor(traindata$ExterQual, levels=c(levels(traindata$ExterQual),"Po"))
levels(traindata$ExterQual)

levels(traindata$BsmtQual)
traindata$BsmtQual<- factor(traindata$BsmtQual, levels=c(levels(traindata$BsmtQual),"Po"))
str(traindata$BsmtQual)

levels(traindata$BsmtCond)
traindata$BsmtCond<-factor(traindata$BsmtCond, levels= c("None","Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered = TRUE)

traindata$BsmtExposure<-factor(traindata$BsmtExposure, levels= c("None","No", "Mn", "Av", "Gd"), exclude = NULL, ordered = TRUE)

traindata$BsmtFinType1<-factor(traindata$BsmtFinType1, levels= c("None","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), exclude = NULL, ordered = TRUE)

traindata$BsmtFinType2<-factor(traindata$BsmtFinType2, levels= c("None","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), exclude = NULL, ordered = TRUE)

traindata$KitchenQual <- factor(traindata$KitchenQual, levels=c("Po", "Fa", "TA", "Gd", "Ex","None"), ordered = TRUE)
levels(traindata$KitchenQual)

traindata$Functional <- factor(traindata$Functional, levels=c("Typ", "Min1", "Min2", "Mod", "Maj1", "Maj2", "Sev", "Sal" ,"None"), ordered = TRUE)
levels(traindata$Functional)

traindata$FireplaceQu <- factor(traindata$FireplaceQu, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered=TRUE)
levels(traindata$FireplaceQu)

levels(traindata$GarageQual)
traindata$GarageQual <- factor(traindata$GarageQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered=TRUE)

traindata$GarageCond <- factor(traindata$GarageCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered=TRUE)
levels(traindata$GarageCond)

traindata$PoolQC <-  factor(traindata$PoolQC, levels = c("None", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered=TRUE)


traindata$SaleType <-  factor(traindata$SaleType, levels = c("VWD","WD","CWD","New","COD","Con","ConLw","ConLI","ConLD","Oth","None"), exclude = NULL, ordered=TRUE)
levels(traindata$SaleType) 



number.of.mv <-function(a){
  mv.list <- c()
  for(i in 1:ncol(a))
  {
    mv.list[i] <- sum(is.na(a[,i]))
  }
  return(mv.list)
}
number.of.mv(traindata)

get.mv <- function(a){
  missing.values<-cbind(colnames(a[which(number.of.mv(a)!=0)]),number.of.mv(a)[!(number.of.mv(a)==0)])
  return(missing.values)
}

get.mv(traindata)


numeric.columns <- traindata[,sapply(traindata, is.numeric)]
factor.columns <- traindata[, sapply(traindata, is.factor)]

par(mfrow=c(1,1))

correlation.matrix <- cor(numeric.columns[,2:ncol(numeric.columns)])
correlation.matrix

corrplot(correlation.matrix, method="square")


names(testdata) <- make.names(names(testdata))

features <- setdiff(colnames(testdata), c("Id"))
for (f in features) {
  if (any(is.na(testdata[[f]]))) 
    if (is.character(testdata[[f]])){ 
      testdata[[f]][is.na(testdata[[f]])] <- "None"
    }else{
      testdata[[f]][is.na(testdata[[f]])] <- mean(testdata[[f]], na.rm = TRUE) 
    }
}

column_class <- lapply(testdata,class)
column_class <- column_class[column_class != "factor"]
factor_levels <- lapply(testdata, nlevels)
factor_levels <- factor_levels[factor_levels > 1]
testdata <- testdata[,names(testdata) %in% c(names(factor_levels), names(column_class))]

testdata <- as.data.frame(unclass(testdata))

testdata<-testdata[,-1]


testdata$MSZoning <- factor(testdata$MSZoning, levels=c("A", "C (all)", "FV", "I", "RH","RL","RP","RM","None"))

levels_Utilities<- c("AllPub","NoSeWa","NoSewr", "ELO")
testdata$Utilities<- factor(testdata$Utilities, levels=levels_Utilities)
str(traindata$Utilities)
levels(traindata$Utilities)

str(traindata)
levels(traindata$Condition2)
levels_Condition2<-c("Artery", "Feedr", "Norm", "PosA", "PosN", "RRAe",  "RRAn", "RRNn", "RRNe")
testdata$Condition2 <- factor(testdata$Condition2, levels=levels_Condition2)
str(traindata$Condition2)

testdata$OverallQual <- factor(testdata$OverallQual, levels=1:10, ordered = TRUE)
testdata$OverallCond <- factor(testdata$OverallCond, levels=1:10, ordered = TRUE)
testdata$OverallCond <- factor(testdata$OverallCond, levels=1:10, ordered = TRUE)
testdata$YearBuilt <- as.integer(testdata$YearBuilt)

anyNA(testdata$Exterior1st) # Checking if NA shoulw be a level
levels_Exterior <- c("AsbShng", "AsphShn","Brk Cmn", "BrkFace", "CBlock", "CmentBd", "HdBoard","ImStucc", "MetalSd","Other","None", "Stone","PreCast", "Stucco","VinylSd","Wd Sdng","Wd Shng")
testdata$Exterior1st<- factor(testdata$Exterior1st, levels=c(levels(testdata$Exterior1st),"PreCast", "Other"))
levels(testdata$Exterior1st)

testdata$Exterior2nd<- factor(testdata$Exterior2nd, levels=c(levels(testdata$Exterior2nd),"PreCast"))
levels(testdata$Exterior2nd)

testdata$ExterQual <- factor(testdata$ExterQual, levels=c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
traindata$ExterQual<- factor(traindata$ExterQual, levels=c(levels(traindata$ExterQual),"Po"))
levels(testdata$ExterQual)

levels(testdata$BsmtQual)
testdata$BsmtQual<- factor(testdata$BsmtQual, levels=c(levels(testdata$BsmtQual),"Po"))
str(traindata$BsmtQual)

levels(testdata$BsmtCond)
testdata$BsmtCond<-factor(testdata$BsmtCond, levels= c("None","Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered = TRUE)

testdata$BsmtExposure<-factor(testdata$BsmtExposure, levels= c("None","No", "Mn", "Av", "Gd"), exclude = NULL, ordered = TRUE)

testdata$BsmtFinType1<-factor(testdata$BsmtFinType1, levels= c("None","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), exclude = NULL, ordered = TRUE)

testdata$BsmtFinType2<-factor(testdata$BsmtFinType2, levels= c("None","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), exclude = NULL, ordered = TRUE)

testdata$KitchenQual <- factor(testdata$KitchenQual, levels=c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
levels(testdata$KitchenQual)

testdata$Functional <- factor(testdata$Functional, levels=c("Typ", "Min1", "Min2", "Mod", "Maj1", "Maj2", "Sev", "Sal"), ordered = TRUE)
levels(testdata$Functional)

testdata$FireplaceQu <- factor(testdata$FireplaceQu, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered=TRUE)
levels(testdata$FireplaceQu)

levels(testdata$GarageQual)
testdata$GarageQual <- factor(testdata$GarageQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered=TRUE)

testdata$GarageCond <- factor(testdata$GarageCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered=TRUE)
levels(testdata$GarageCond)

testdata$PoolQC <-  factor(testdata$PoolQC, levels = c("None", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered=TRUE)

testdata$SaleType <-  factor(testdata$SaleType, levels = c("VWD","WD","CWD","New","COD","Con","ConLw","ConLI","ConLD","Oth"), exclude = NULL, ordered=TRUE)
testdata$SaleType <- factor(testdata$SaleType, levels= c(levels(testdata$SaleType), "VWD","WD","CWD","New","COD","Con","ConLw","ConLI","ConLD","Oth"))
levels(testdata$SaleType) 



number.of.mv <-function(a){
  mv.list <- c()
  for(i in 1:ncol(a))
  {
    mv.list[i] <- sum(is.na(a[,i]))
  }
  return(mv.list)
}
number.of.mv(testdata)

get.mv <- function(a){
  missing.values<-cbind(colnames(a[which(number.of.mv(a)!=0)]),number.of.mv(a)[!(number.of.mv(a)==0)])
  return(missing.values)
}

get.mv(traindata)

testdata<-na.omit(testdata)

common <- intersect(names(traindata), names(testdata)) 
for (p in common) { 
  if (class(traindata[[p]]) == "factor") { 
    levels(testdata[[p]]) <- levels(traindata[[p]]) 
  } 
}




#Train model
fit <- lm(SalePrice ~ . , data=traindata)
summary(fit)
anova(fit)
par(mfrow=c(2,2))
plot(fit)
infIndexPlot(fit)
residualPlots(fit)
avPlots(fit)
vif(fit)

#Taking only the significant variables

fit <- lm(SalePrice~LotArea+Street+Exterior1st+
            LotConfig+LandSlope+Neighborhood+Condition1+OverallQual+OverallCond+
            YearBuilt+RoofStyle+RoofMatl+MasVnrArea+ExterQual+BsmtQual+BsmtExposure+BsmtFinSF2 +
            BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+KitchenQual+GarageQual+PoolQC+
            ScreenPorch+PoolArea+PoolQC+SaleType , data=traindata)
fit1<-step(fit)
plot(fit$fit,fit$residuals)
abline(h=0,col="blue")
my_prediction <- predict(fit,newdata=testdata,type = "response")
length(testdata$SalePrice)
length(prediction)
my_prediction
out <- data.frame(Id = testdata$Id)
out$SalePrice <- my_prediction
write.csv(out,"result.csv",row.names = FALSE)





library(rpart)
regressor = rpart(formula = SalePrice~LotArea+Street+MSZoning+
                    LotConfig+LandSlope+Neighborhood+Condition1+OverallQual+OverallCond+
                    YearBuilt+RoofStyle+RoofMatl+MasVnrArea+ExterQual+BsmtQual+BsmtExposure+BsmtFinSF2 +
                    BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+KitchenQual+GarageQual+PoolQC+
                    ScreenPorch+PoolArea+PoolQC+SaleType,
                  data = traindata,
                  control = rpart.control(minsplit = 5))
my_prediction1 <- predict(regressor,newdata=testdata)
my_prediction1
regressor
summary(regressor)
plot(regressor)
text(regressor)



install.packages("randomForest")
library(randomForest)
Sale_Forest<-randomForest(SalePrice~LotArea+Street+MSZoning+
                              LotConfig+LandSlope+Neighborhood+Condition1+OverallQual+OverallCond+
                              YearBuilt+RoofStyle+RoofMatl+MasVnrArea+ExterQual+BsmtQual+BsmtExposure+BsmtFinSF2 +
                              BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+KitchenQual+GarageQual+PoolQC+
                              ScreenPorch+PoolArea+PoolQC+SaleType,data=traindata,mtry=27,ntree=500)
Sale_Forest
plot(Sale_Forest)
varImpPlot(Sale_Forest)
getTree(Sale_Forest)
mtry<-sqrt(ncol(traindata))
mtry <- tuneRF(traindata,traindata$SalePrice, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)

pred_rf<-predict(Sale_Forest,newdata= testdata)




