#capstone project

setwd("C:/Users/Sumit Kashyap/Desktop/Capstone")

library(dplyr)
library(tidyverse)
library(tidyr)
install.packages("randomForest")
library("randomForest")
install.packages("caret")
library("caret")


#Importing both medNAS_dili_tt and ac50_Matrix_190226 datasets
rawdata <- read.csv("ac50_Matrix_190226.csv")
sdata <- read.csv("medNAS_dili_tt.csv", header = T)


#Count total number of distinct drugs
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
chm <- data.frame(sdata[unique(sdata$chnm), 4])


#Named the Variable "code"
names(chm) <- c("X")


#Created a new dataset joining chemical codes with their test and its values
newdata <- merge(chm, rawdata, by = "X")


#transposing the tests
nd1 <- newdata %>% gather(test,values,2:1400)

#Removing 1000000
fdata <- nd1
#fdata <- subset(nd1, nd1$values!=1000000)

#Removig NA
predata <- na.omit(fdata)
summary(predata)
write.csv(predata, file = 'cleaned.csv' )

#Cleaned data without dubplicate rows
finaldata <- unique(predata)
names(finaldata) <- c("code", "test", "EC50")

#Exported the file to cross check the values
write.csv(finaldata, file = 'cleaned1.csv' )


finaldata1 <- data.frame(finaldata)

#Here, we joined both the tables finaldata1 and sdata to get Cmax values with EC50 values
z <- merge(finaldata1, sdata, by.x = 1, by.y = 4, all.x = TRUE)
#write.csv(z, file = 'cleaned2.csv' )

#Here, we removed all the duplicate values generated while joining the above two files
y <- z[!duplicated(z[1:3]),]
#write.csv(y, file = 'cleaned3.csv' )

#Removed unecessary variables from collected from previous batch file
a <- subset(y, select = -c(NAS, EC, chnm, code, testname, casn, testname, tgt_abbr, testtarget, test))
#write.csv(a, file = 'cleaned4.csv' )
str(a)

#Generated Cmax values from EC50 and Cmax Values 
a$Cratio <- (a$EC50)/a$CmaxStand
str(a)
View(a)

#Excluding AmbiDILI from the data as instructed
b <- droplevels(a[a$Classification != "AmbiDILIDrugs",])
str(b)


#Renaming "LessDILI" & "NODILI" as "Other"
b$Classification <- as.character(b$Classification)
b$Classification <- ifelse((b$Classification == "LessDILIDrugs" | b$Classification == "NoDILI Drugs"), "Other", "MostDILI Drugs")
b$Classification <- as.factor(b$Classification)
str(b)


########################################################### Modeling 1 ##########################################

#We will use EC50 values to build a predictive model
#Divide the data into train and test data
set.seed(187) 
row.number <- sample(nrow(b), 0.7*nrow(b), replace =F)
train = b[row.number,]
test = b[-row.number,]
head(train)
head(test)
str(train)

summary(train)
summary(test)


#Random forest analysis

mod1 <- randomForest(Classification ~ ., data = train, ntree = 300, mtry = 3, importnace =T)
mod1

#predicting on Trainset
predTrain <- predict(mod1, train, type ="class")

#Checking accuracy on Train

table(predTrain,train$Classification)

#Predicting on test set
predTest <- predict(mod1, test, type ="class")

#Checking accuracy on Test

table(predTest,test$Classification)

#checking overall acurracy of model

mean(predTest == test$Classification)


#################################################### Model2 #####################################################

#In this model we are changing tests into dummy variables and use them to build a predictive model

#Generated Datasset

a1 <- subset(y, select = -c(NAS, EC, chnm, code, testname, casn, testname, tgt_abbr, testtarget))

#Generated Cmax values from EC50 and Cmax Values 
a1$Cratio <- (a1$EC50)/a1$CmaxStand
str(a1)
View(a1)

summary(a1)

#Excluding AmbiDILI from the data as instructed
b1 <- droplevels(a1[a1$Classification != "AmbiDILIDrugs",])
str(b1)
summary(b1)

#Applying one-hot encoding to data

#splitting the data to apply encoding on tests
b1a <- subset(b1, select = -c(test))

b1b <- subset(b1, select = c(test))

#applying
dmy <- dummyVars(" ~ .", data = b1b)
b1b <- data.frame(predict(dmy, newdata = b1b))
b1b

#merging data
b11 <- cbind(b1a,b1b)
str(b11)


#Splitting the dataset #Divide the data into train and test data

set.seed(180) 
row.number <- sample(nrow(b11), 0.7*nrow(b11), replace =F)
train1 = b11[row.number,]
test1 = b11[-row.number,]
head(train1)
head(test1)
summary(train1)


#Random forest analysis

mod2 <- randomForest(Classification ~ ., data = train1, ntree = 1200, mtry = 6, importnace =T)
mod2
varImp(mod2)
#predicting on Trainset
predTrain1 <- predict(mod2, train1, type ="class")

#Checking accuracy on Train

table(predTrain1,train1$Classification)

#Predicting on test set
predTest1 <- predict(mod2, test1, type ="class")

#Checking accuracy on Test

table(predTest1,test1$Classification)


mean(predTest1 == test1$Classification)


