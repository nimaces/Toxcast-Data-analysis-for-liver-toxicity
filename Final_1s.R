#DILI project

library(car)
library(plyr)
library(dplyr)
library(tidyverse)
library(tidyr)

#Importing  datasets

setwd("C:/Users/Sumit Kashyap/Desktop/Capstone")

rawdata <- read.csv("ac50_Matrix_190226.csv")
sdata <- read.csv("medNAS_dili_tt.csv", header = T)

xsdata <- read.csv("AC50_DILI.csv", head =T)
cmax <- read.csv("cmaxstand.csv")


#Count total number of distinct drugs
install.packages("sqldf")
library(sqldf)
chemicals <- sqldf("select distinct(chnm) from sdata")
nrow(chemicals)

#Toal number of test targets
testtargets <- sqldf("select distinct(testtarget) from sdata")
nrow(testtargets)

#Types of classifications present according to NAS values
Classifications <- sqldf("select distinct(Classification) from sdata")
nrow(Classifications)

#count the number of samples for each dili catagory
MDILI <- sdata[sdata$Classification == 'MostDILI Drugs',]
nrow(MDILI)
ADILI <- sdata[sdata$Classification == 'AmbiDILIDrugs',]
nrow(ADILI)
LDILI <- sdata[sdata$Classification == 'LessDILIDrugs',]
nrow(LDILI)
NDILI <- sdata[sdata$Classification == 'NoDILI Drugs',]
nrow(NDILI)

#Distinct code for all the chemicals
chm <- data.frame(sdata[unique(sdata$chnm),4 ])

#Named the Variable "code"
names(chm) <- c("code")

#Created a new dataset joining chemical codes with their test and its values
newww <- merge(chm, rawdata, by = "code")
newdata <- newww

nd1 <- newdata %>% gather(test,values,2:1400)

#Removing 1000000
fdata <- subset(nd1, nd1$values!=1000000)

#Removig NA
predata <- na.omit(fdata)
# write.csv(predata, file = 'cleaned.csv' )

#Cleaned data without dubplicate rows
finaldata <- unique(predata)
names(finaldata) <- c("code", "test", "EC50")

#Exported the file to cross check the values
# write.csv(finaldata, file = 'cleaned1.csv' )

factor(finaldata$test)

finaldata1 <- data.frame(finaldata)

#Here, we joined both the tables finaldata1 and sdata to get Cmax values with EC50 values
z <- merge(finaldata1, sdata, by.x = 1, by.y = 4, all.x = TRUE)
#write.csv(z, file = 'cleaned2.csv' )

#Here, we removed all the duplicate values generated while joining the above two files
y <- z[!duplicated(z[1:3]),]
#write.csv(y, file = 'cleaned3.csv' )

#Removed unecessary variables from collected from previous batch file
a <- subset(y, select = -c(casn, testname, NAS, EC, tgt_abbr))
#write.csv(a, file = 'cleaned4.csv' )
str(a)

#Generated Nas values from EC50 and Cmax Values 
a$Cratio <- a$EC50/a$CmaxStand
str(a)
View(a)

#Excluding AmbiDILI from the data as instructed
b <- subset(a, a$Classification != "AmbiDILIDrugs")
str(b)
View(b)

#Renaming "LessDILI" & "NODILI" as "Other"
b$Classification <- as.character(b$Classification)
b$Classification <- ifelse((b$Classification == "LessDILIDrugs" | b$Classification == "NoDILI Drugs"), "Other", "MostDILI Drugs")
b$Classification <- as.factor(b$Classification)
str(b)

unique(b$test)
#b$test1 <- as.numeric(as.factor(b$test))

#Converting Classification (categorical values) to numerical values
b$Classcode <-ifelse(b$Classification == "MostDILI Drugs",1, 0)
names(b)

b <- b[c("Classcode","Classification","code", "chnm", "test", "testtarget", "EC50", "CmaxStand", "Cratio")]
names(b)
View(b)

# write.csv(b, file = 'C:/Users/Dell-pc/Desktop/MPS in Analytics/ALY 6980 Capstone/cleaned5.csv')

summary(b)

c <- b[,-c(2:4)]
d <- c[, -c(4:5)]


#One Hot Encodeing
install.packages("data.table")
install.packages("mltools")
library(data.table)
library(mltools)
testencode <- data.frame(d$test)
testencoded <- one_hot(as.data.table(testencode))

testtargetencode <- data.frame(d$testtarget)
testtargetencoded <- one_hot(as.data.table(testtargetencode))

data1 <- d[, -c(2:3)]

final <- cbind(data1, testencoded, testtargetencoded)
summary(final)

#Divide the data into train and test data for RandomForest Classification
set.seed(521) 
row.number <- sample(x=1:nrow(final), size=0.7*nrow(final))
train = final[row.number, ]
test = final[-row.number,]
dim(train)
dim(test)

names(train) <- make.names(names(train))
names(test) <- make.names(names(test))

train$Classcode <- as.character(train$Classcode)
train$Classcode <- as.factor(train$Classcode)

test$Classcode <- as.character(test$Classcode)
test$Classcode <- as.factor(test$Classcode)

table(train$Classcode)
View(train)

# set.seed(9560)
# bal_train <- downSample(x = train[, -ncol(train)],
#                          y = train$Classcode)
# table(bal_train$Classcode)   


######################################################################################

#RandomForest
install.packages("randomForest")
library(randomForest)
set.seed(123)
RF <- randomForest(Classcode~., data = train, importance = TRUE, ntree=10)
#, n_estimators = 65, min_samples_split = 2,max_features = sqrt, criterion = 'gini')
RF

RF$importance
varImpPlot(RF,type=2)

train$RFPred <- predict(RF, newdata = train, type="class") #Returns the predicted class
train$RFProb <- predict(RF, newdata = train, type="prob")  #Returns the predicted class


test$RFPred <- predict(RF,test) #Returns the predicted class
test$RFProb <- predict(RF, newdata = test, type="prob")  #Returns the predicted prob

mean(test$RFPred == test$Classcode) 

###### graph #######


barchart(test$RFPred, test)

barchart(test$Classcode, test)

#write.csv(test, file = "tableau.csv")

#mean(test$RFPred == test$Classcode) 
install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)
confusionMatrix(bal_train$RFPred,bal_train$Classcode)
RF$predicted
############################################################################

#Divide the data into train and test data for Gradient Bossting
set.seed(123) 
row.number <- sample(x=1:nrow(final), size=0.7*nrow(final))
train1 = final[row.number, ]
test1 = final[-row.number,]
dim(train1)
dim(test1)

names(train1) <- make.names(names(train1))
names(test1) <- make.names(names(test1))
# 
# set.seed(9560)
# bal_train1 <- downSample(x = train1[, -ncol(train1)],
#                         y = train1$Classcode)
# table(bal_train1$Classcode)   


train1$Classcode <- as.character(train1$Classcode)
train1$Classcode <- as.factor(train1$Classcode)

test1$Classcode <- as.character(test1$Classcode)
test1$Classcode <- as.factor(test1$Classcode)
##########################################################################
# Gradient Boosting
install.packages("gbm")
install.packages("doParallel")
library(gbm)
library(doParallel)
grid <- expand.grid(n.trees = 1000, interaction.depth=c(1:2), shrinkage=c(0.01,0.05), n.minobsinnode=c(20))
ctrl <- trainControl(method = "repeatedcv",number = 5, repeats = 2, allowParallel = T)
registerDoParallel(detectCores()-1)
set.seed(124) #for reproducability
unwantedoutput <- capture.output(GBMModel <- train(Classcode~.,data = train1,method = "gbm", trControl = ctrl, tuneGrid = grid))
print(GBMModel)
confusionMatrix(GBMModel)

train1$GBMModelPred <- predict(GBMModel, newdata = train1, type="raw") #Returns the predicted class
train1$GBMModelProb <- predict(GBMModel, newdata = train1, type="prob")  #Returns the predicted prob

table(train1$GBMModelPred)

test1$GBMModelPred <- predict(GBMModel, newdata = test1, type="raw") #Returns the predicted class
test1$GBMModelProb <- predict(GBMModel, newdata = test1, type="prob")  #Returns the predicted prob

table(test1$GBMModelPred)

names(train1)
names(test1)

confusionMatrix(test1$GBMModelPred, test1$Classcode)
train1$GBMModelPred <-ifelse(train1$GBMModelPred == 1,"MostDILI Drug", "Other")
train1$Classcode <-ifelse(train1$Classcode == 1,"MostDILI Drug", "Other")
train1 <- train1 %>% select(GBMModelPred, everything())
colnames(train1)[1] <- "Predicted DILI Category" 
colnames(train1)[2] <- "Classification"
predicted <- train1

View(predicted)

############################################## xs Dataset ###################################################



#Splitting the dataset #Divide the data into train and test data

b11 <- subset(xsdata, select = -c(chnm, casn))

b11 <- unique(b11)

names(b11) <- c("classification","code", "Testname", "EC50")

newdata11 <- merge(cmax, b11, by = "code")

b11 <- newdata11
#names(b11) <- c("code","cmax", "classification", "EC50" )

b11 <- subset(b11, select = -c(code, Testname))

b11 <- unique(b11)



#removing ambi dili

b11 <- droplevels(b11[b11$classification != "AmbiDILIDrugs",])

b11$Cratio <- (b11$EC50)/b11$Cmax
str(b11)

b11$Cratio <- sub("Inf","0",b11$Cratio)

#Applying one-hot encoding to data

#splitting the data to apply encoding on tests

b1a <- subset(b11, select = -c(Testname))

b1b <- subset(b11, select = c(Testname))

applying
dmy <- dummyVars(" ~ .", data = b1b)
b1b <- data.frame(predict(dmy, newdata = b1b))

#merging data
b11 <- cbind(b1a,b1b)
str(b11)



set.seed(180) 
row.number <- sample(nrow(b11), 0.7*nrow(b11), replace =F)
train1 = b11[row.number,]
test1 = b11[-row.number,]
head(train1)
head(test1)
summary(train1)



#Random forest analysis

mod2 <- randomForest(classification ~ ., data = train1, ntree = 300, importnace =T)
mod2
varImpPlot(mod2,type=2)

#predicting on Trainset
predTrain1 <- predict(mod2, train1, type ="class")

#Checking accuracy on Train

table(predTrain1,train1$classification)

#Predicting on test set
predTest1 <- predict(mod2, test1, type ="class")

#Checking accuracy on Test

table(predTest1,test1$classification)


mean(predTest1 == test1$classification)


########################################## Model on Xs Data ###############################################
xsdata <- read.csv("AC50_DILI.csv", head =T)
cmax <- read.csv("cmaxstand.csv")

#Splitting the dataset #Divide the data into train and test data

b11 <- subset(xsdata, select = -c(chnm, casn))

b11 <- unique(b11)

names(b11) <- c("classification","code", "Testname", "EC50")

newdata11 <- merge(cmax, b11, by = "code")

b11 <- newdata11
#names(b11) <- c("code","cmax", "classification", "EC50" )

b11 <- subset(b11, select = -c(code, Testname))

b11 <- unique(b11)



#removing ambi dili

b11 <- droplevels(b11[b11$classification != "AmbiDILIDrugs",])

b11$Cratio <- (b11$EC50)/b11$Cmax
str(b11)

b11$Cratio <- sub("Inf","0",b11$Cratio)

#Applying one-hot encoding to data

#splitting the data to apply encoding on tests
b1a <- subset(b11, select = -c(Testname))

b1b <- subset(b11, select = c(Testname))

#applying
dmy <- dummyVars(" ~ .", data = b1b)
b1b <- data.frame(predict(dmy, newdata = b1b))

#merging data
b11 <- cbind(b1a,b1b)
str(b11)



set.seed(180) 
row.number <- sample(nrow(b11), 0.7*nrow(b11), replace =F)
train1 = b11[row.number,]
test1 = b11[-row.number,]
head(train1)
head(test1)
summary(train1)



#Random forest analysis

mod2 <- randomForest(classification ~ ., data = train1, ntree = 300, importnace =T)
mod2

varImpPlot(mod2,type=2)

#predicting on Trainset
predTrain1 <- predict(mod2, train1, type ="class")

#Checking accuracy on Train

table(predTrain1,train1$classification)

#Predicting on test set
predTest1 <- predict(mod2, test1, type ="class")

#Checking accuracy on Test

table(predTest1,test1$classification)


mean(predTest1 == test1$classification)



#Reference:
#for gradient boosting link: https://rstudio-pubs-static.s3.amazonaws.com/374759_30994015da0a4efdb249142ebfb1d0cd.html
#https://stats.stackexchange.com/questions/215790/gradient-boosting-machine-accuracy-decreases-as-number-of-iterations-increases

