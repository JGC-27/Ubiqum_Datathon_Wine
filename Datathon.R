#### Libraries
library(rstudioapi)
library(ggplot2)
library(caret)
library(dplyr)
library(plyr)
library(tidyverse)

#### LoadData
setwd(dirname(getActiveDocumentContext()$path))
wine <- read.csv("training.csv", 
                 sep = ",", header = TRUE)
validation <- read.csv("validation.csv")

#### rename attributes
names(wine)[names(wine) == "fixed.acidity"] <- "FixedAcidity"
names(wine)[names(wine) == "volatile.acidity"] <- "VolatileAcidity"
names(wine)[names(wine) == "citric.acid"] <- "CitricAcid"           
names(wine)[names(wine) == "residual.sugar"] <- "ResidualSugar"
names(wine)[names(wine) == "chlorides"] <- "Chlorides"
names(wine)[names(wine) == "free.sulfur.dioxide"] <- "FreeSulfurDioxide"            
names(wine)[names(wine) == "total.sulfur.dioxide"] <- "TotalSulfurDioxide"
names(wine)[names(wine) == "density"] <- "Density"
names(wine)[names(wine) == "pH"] <- "PH"        
names(wine)[names(wine) == "sulphates"] <- "Sulphates"
names(wine)[names(wine) == "alcohol"] <- "Alcohol"
names(wine)[names(wine) == "qualiy"] <- "Quality"

#### rename attributes on Validation file
names(validation)[names(validation) == "fixed.acidity"] <- "FixedAcidity"
names(validation)[names(validation) == "volatile.acidity"] <- "VolatileAcidity"
names(validation)[names(validation) == "citric.acid"] <- "CitricAcid"           
names(validation)[names(validation) == "residual.sugar"] <- "ResidualSugar"
names(validation)[names(validation) == "chlorides"] <- "Chlorides"
names(validation)[names(validation) == "free.sulfur.dioxide"] <- "FreeSulfurDioxide"            
names(validation)[names(validation) == "total.sulfur.dioxide"] <- "TotalSulfurDioxide"
names(validation)[names(validation) == "density"] <- "Density"
names(validation)[names(validation) == "pH"] <- "PH"        
names(validation)[names(validation) == "sulphates"] <- "Sulphates"
names(validation)[names(validation) == "alcohol"] <- "Alcohol"
names(validation)[names(validation) == "qualiy"] <- "Quality"

#### DataExploration
attributes(wine)
summary(wine)
str(wine) #IMPORTAN]T
names(wine)
attributes(wine)
dim(wine) #3919 rows, 13 columns

#### NA? --> there's not 
sum(is.na(wine))

#### RepeatedRows? --> no duplicated
sum(duplicated(wine)) 

#### Outliers? --> can't remove outliers

#### density 
ggplot(wine, aes(quality)) +
  geom_bar()  +
  labs(title=" Dependent variable distribution",
       x ="Quality of the wine",
       y = "")


#### combining levels of quality into 3 main bins (characters)
#1st 
wine$quality <- as.factor(wine$quality)
str(wine)
#2nd
wine$quality<-revalue(wine$quality,c("3"="Low"))
wine$quality<-revalue(wine$quality,c("4"="Low"))
wine$quality<-revalue(wine$quality,c("5"="Low"))
wine$quality<-revalue(wine$quality,c("6"="Medium"))
wine$quality<-revalue(wine$quality,c("7"="High"))
wine$quality<-revalue(wine$quality,c("8"="High"))
wine$quality<-revalue(wine$quality,c("9"="High"))

#### Split
set.seed(123)
trainSize<-round(nrow(wine)*0.8)
testSize<-nrow(wine)-trainSize
training_indices<-sample(seq_len(nrow(wine)),size = trainSize)
trainSet<-wine[training_indices,]
testSet<-wine[-training_indices,]
str(testSet)
str(trainSet)
str(trainSet)


#### Run model
RF_model<-trainControl(method="cv", number=10)
model<-train(quality~., data=trainSet,
                      method="rf",
                      trControl=RF_model,
                      verbose=FALSE,
                      downsampling=T)
model

#### Predictions
pred<-predict(model,testSet)
res<-postResample(pred,as.factor(testSet$quality))
actuals_preds <- data.frame(cbind(actuals=testSet$quality),
                            predicteds=pred, stringsAsFactors =TRUE)
varImp(model)

#### Error Analysis
actuals_preds$actuals<-as.numeric(actuals_preds$actuals) 
actuals_preds$predicteds<-as.numeric(actuals_preds$predicteds)
actuals_preds <- data.frame(cbind(actuals=testSet$quality),
                            predicteds=pred, 
                            error=abs(actuals_preds$actuals-actuals_preds$predicteds), 
                            stringsAsFactors =TRUE)

testSet$error<-abs(actuals_preds$actuals - actuals_preds$predicteds) 
error<-abs(actuals_preds$actuals-actuals_preds$predicteds) 

#### Applying new dataset
predictValidation<-predict(model,validation)
validation$quality<-predictValidation
print(validation)

#to export Validation into csv
write.csv2(validation,"NewValidation.csv")
