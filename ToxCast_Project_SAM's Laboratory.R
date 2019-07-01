#capstone project

library(car)
library(plyr)
library(dplyr)
library(tidyverse)
library(tidyr)
#Importing both medNAS_dili_tt and ac50_Matrix_190226 datasets
rawdata <- read.csv("C:/Users/Dell-pc/Desktop/MPS in Analytics/ALY 6980 Capstone/ac50_Matrix_190226.csv")
sdata <- read.csv("C:/Users/Dell-pc/Desktop/MPS in Analytics/ALY 6980 Capstone/medNAS_dili_tt.csv", header = T)

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
a$Cratio <- (a$CmaxStand - a$EC50)/a$CmaxStand
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

final <- cbind(data1, testencoded)

# Data Visualization
install.packages("ggplot2")
library(ggplot2)
library(plyr)

# Distribution of Category from cleaned data
Color = c("green", "orange", "blue", "red")
ggplot(data.frame(sdata$Classification), aes(x=sdata$Classification)) +
  geom_bar(fill = Color)

# Distribution of Category from train dataset
Color = c("green", "blue")
ggplot(data.frame(train$Classcode), aes(x=train$Classcode)) +
  geom_bar(fill = Color)


################################## Model 1: considering tests ################################### 

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
#                           y = train$Classcode)
# table(bal_train$Classcode)   


######################################

#RandomForest
install.packages("randomForest")
library(randomForest)
set.seed(123)
RF <- randomForest(Classcode~., data = train, importance = TRUE, ntree=300)
#, n_estimators = 65, min_samples_split = 2,max_features = sqrt, criterion = 'gini')
RF

RF$importance
varImpPlot(RF,type=2)

train$RFPred <- predict(RF, newdata = train, type="class") #Returns the predicted class
train$RFProb <- predict(RF, newdata = train, type="prob")  #Returns the predicted prob


test$RFPred <- predict(RF,test) #Returns the predicted class
test$RFProb <- predict(RF, newdata = test, type="prob")  #Returns the predicted prob

mean(test$RFPred == test$Classcode) 

#mean(test$RFPred == test$Classcode) 
install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)
confusionMatrix(train$RFPred,train$Classcode)
confusionMatrix(test$RFPred,test$Classcode)
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
table(train1$Classcode)   

Color2 = c("blue", "red")
ggplot(data.frame(train1$Classcode), aes(x=train1$Classcode)) +
  geom_bar(fill = Color2)


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

confusionMatrix(train1$GBMModelPred, train1$Classcode)
confusionMatrix(test1$GBMModelPred, test1$Classcode)

train1$GBMModelPred <-ifelse(train1$GBMModelPred == 1,"MostDILI Drug", "Other")
train1$Classcode <-ifelse(train1$Classcode == 1,"MostDILI Drug", "Other")
train1 <- train1 %>% select(GBMModelPred, everything())
colnames(train1)[1] <- "PredictedClassification" 
colnames(train1)[2] <- "Classification"
predicted <- train1
#write.csv(predicted, file = 'C:/Users/Dell-pc/Desktop/MPS in Analytics/ALY 6980 Capstone/predicted.csv')
View(predicted)


plot(train1, GBMModelPred ~ Classcode, groups = train1$Classcode) 

predicted1 <- as.data.frame(train1$GBMModelPred) %>%
  mutate(actual = as.vector(train1[, 2]))

predicted1 %>%
  group_by(actual, train1$GBMModelPred ) %>%
  summarise_all(funs(n = n())) %>%
  mutate(freq = n / sum(n)) 


predicted1 %>%
  ggplot(aes(x = actual, fill = train1$GBMModelPred)) +
  geom_bar() +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap( ~ actual, scales = "free", ncol = 2)

##################################### Model 2: considering testtargets##############################

testtargetencode <- data.frame(d$testtarget)
testtargetencoded <- one_hot(as.data.table(testtargetencode))

data1 <- d[, -c(2:3)]

final1 <- cbind(data1, testtargetencoded)

#Divide the data into train and test data for RandomForest Classification
set.seed(721) 
row.number <- sample(x=1:nrow(final1), size=0.7*nrow(final1))
train2 = final1[row.number, ]
test2 = final1[-row.number,]
dim(train2)
dim(test2)

names(train2) <- make.names(names(train2))
names(test2) <- make.names(names(test2))

train2$Classcode <- as.character(train2$Classcode)
train2$Classcode <- as.factor(train2$Classcode)

test2$Classcode <- as.character(test2$Classcode)
test2$Classcode <- as.factor(test2$Classcode)

######################################

#RandomForest
install.packages("randomForest")
library(randomForest)
set.seed(223)
RF1 <- randomForest(Classcode~., data = train2, importance = TRUE, ntree=300)
RF1

RF1$importance
varImpPlot(RF1,type=2)

train2$RFPred <- predict(RF1, newdata = train2, type="class") #Returns the predicted class
train2$RFProb <- predict(RF1, newdata = train2, type="prob")  #Returns the predicted prob

test2$RFPred <- predict(RF1, newdata = test2, type="class") #Returns the predicted class
test2$RFProb <- predict(RF1, newdata = test2, type="prob")  #Returns the predicted prob

install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)
confusionMatrix(train2$RFPred,train2$Classcode)
confusionMatrix(test2$RFPred,test2$Classcode)
#################################################

#Divide the data into train and test data for Gradient Bossting
set.seed(323) 
row.number <- sample(x=1:nrow(final1), size=0.7*nrow(final1))
train3 = final1[row.number, ]
test3 = final1[-row.number,]
dim(train3)
dim(test3)

names(train3) <- make.names(names(train3))
names(test3) <- make.names(names(test3))

table(train3$Classcode)   

train3$Classcode <- as.character(train3$Classcode)
train3$Classcode <- as.factor(train3$Classcode)

test3$Classcode <- as.character(test3$Classcode)
test3$Classcode <- as.factor(test3$Classcode)
##########################################################################
# Gradient Boosting
install.packages("gbm")
install.packages("doParallel")
library(gbm)
library(doParallel)
grid <- expand.grid(n.trees = 1000, interaction.depth=c(1:2), shrinkage=c(0.01,0.05), n.minobsinnode=c(20))
ctrl <- trainControl(method = "repeatedcv",number = 5, repeats = 2, allowParallel = T)
registerDoParallel(detectCores()-1)
set.seed(224) #for reproducability
unwantedoutput <- capture.output(GBMModel <- train(Classcode~.,data = train3,method = "gbm", trControl = ctrl, tuneGrid = grid))
print(GBMModel)

confusionMatrix(GBMModel)

train3$GBMModelPred <- predict(GBMModel, newdata = train3, type="raw") #Returns the predicted class
train3$GBMModelProb <- predict(GBMModel, newdata = train3, type="prob")  #Returns the predicted prob

table(train3$GBMModelPred)

test3$GBMModelPred <- predict(GBMModel, newdata = test3, type="raw") #Returns the predicted class
test3$GBMModelProb <- predict(GBMModel, newdata = test3, type="prob")  #Returns the predicted prob

table(test3$GBMModelPred)

names(train3)
names(test3)

confusionMatrix(train3$GBMModelPred, train3$Classcode)
confusionMatrix(test3$GBMModelPred, test3$Classcode)

################################################# Model 3: test and test target combined################

testencode <- data.frame(d$test)
testencoded <- one_hot(as.data.table(testencode))

testtargetencode <- data.frame(d$testtarget)
testtargetencoded <- one_hot(as.data.table(testtargetencode))

data1 <- d[, -c(2:3)]

final2 <- cbind(data1, testencoded, testtargetencoded)

#Divide the data into train and test data for RandomForest Classification
set.seed(721) 
row.number <- sample(x=1:nrow(final2), size=0.7*nrow(final2))
train4 = final2[row.number, ]
test4 = final2[-row.number,]
dim(train4)
dim(test4)

names(train4) <- make.names(names(train4))
names(test4) <- make.names(names(test4))

train4$Classcode <- as.character(train4$Classcode)
train4$Classcode <- as.factor(train4$Classcode)

test4$Classcode <- as.character(test4$Classcode)
test4$Classcode <- as.factor(test4$Classcode)

######################################

#RandomForest
install.packages("randomForest")
library(randomForest)
set.seed(323)
RF2 <- randomForest(Classcode~., data = train4, importance = TRUE, ntree=300)
RF2

RF2$importance
varImpPlot(RF2,type=2)

train4$RFPred <- predict(RF2, newdata = train4, type="class") #Returns the predicted class
train4$RFProb <- predict(RF2, newdata = train4, type="prob")  #Returns the predicted prob

test4$RFPred <- predict(RF2, newdata = test4, type="class") #Returns the predicted class
test4$RFProb <- predict(RF2, newdata = test4, type="prob")  #Returns the predicted prob

install.packages("e1071")
install.packages("caret")
library(e1071)
library(caret)
confusionMatrix(train4$RFPred,train4$Classcode)
confusionMatrix(test4$RFPred,test4$Classcode)
################################################

#Divide the data into train and test data for Gradient Bossting
set.seed(523) 
row.number <- sample(x=1:nrow(final2), size=0.7*nrow(final2))
train5 = final2[row.number, ]
test5 = final2[-row.number,]
dim(train5)
dim(test5)

names(train5) <- make.names(names(train5))
names(test5) <- make.names(names(test5))

table(train5$Classcode)   

train5$Classcode <- as.character(train5$Classcode)
train5$Classcode <- as.factor(train5$Classcode)

test5$Classcode <- as.character(test5$Classcode)
test5$Classcode <- as.factor(test5$Classcode)
##########################################################################
# Gradient Boosting
install.packages("gbm")
install.packages("doParallel")
library(gbm)
library(doParallel)
grid <- expand.grid(n.trees = 1000, interaction.depth=c(1:2), shrinkage=c(0.01,0.05), n.minobsinnode=c(20))
ctrl <- trainControl(method = "repeatedcv",number = 5, repeats = 2, allowParallel = T)
registerDoParallel(detectCores()-1)
set.seed(224) #for reproducability
unwantedoutput <- capture.output(GBMModel <- train(Classcode~.,data = train5,method = "gbm", trControl = ctrl, tuneGrid = grid))
print(GBMModel)

confusionMatrix(GBMModel)

train5$GBMModelPred <- predict(GBMModel, newdata = train5, type="raw") #Returns the predicted class
train5$GBMModelProb <- predict(GBMModel, newdata = train5, type="prob")  #Returns the predicted prob

table(train3$GBMModelPred)

test5$GBMModelPred <- predict(GBMModel, newdata = test5, type="raw") #Returns the predicted class
test5$GBMModelProb <- predict(GBMModel, newdata = test5, type="prob")  #Returns the predicted prob

table(test5$GBMModelPred)

names(train5)
names(test5)

confusionMatrix(train5$GBMModelPred, train5$Classcode)
confusionMatrix(test5$GBMModelPred, test5$Classcode)


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



