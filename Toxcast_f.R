#capstone project

library(dplyr)
library(tidyverse)
library(tidyr)
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
chm <- data.frame(sdata[unique(sdata$chnm),4 ])

#Named the Variable "code"
names(chm) <- c("X")

#Created a new dataset joining chemical codes with their test and its values
newww <- merge(chm, rawdata, by = "X")
newdata <- newww

nd1 <- newdata %>% gather(test,values,2:1400)

#Removing 1000000
fdata <- subset(nd1, nd1$values!=1000000)

#Removig NA
predata <- na.omit(fdata)
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
a <- subset(y, select = -c(casn, testname, NAS, EC, testtarget, tgt_abbr))
#write.csv(a, file = 'cleaned4.csv' )
str(a)

#Generated Nas values from EC50 and Cmax Values 
a$NAS <- (a$CmaxStand - a$EC50)/a$CmaxStand


#Divide the data into train and test data
set.seed(1) 
row.number <- sample(x=1:nrow(a), size=0.7*nrow(a))
train = a[row.number,]
test = a[-row.number,]
head(train)
head(test)

#Linear Regression
model1 <- lm(formula = NAS ~ ., data = train)
summary(model1)







#library(data.table)
#z1 <- data.table(z) 
#setkey(z1)
#str(z1)
#y <- unique(z1[list("code", "test", "EC50"), nomatch = 0])


#finaldata1$Cmax <- NA

#for (i in length(finaldata1$code)){
 # for(j in length(sdata$code)){
  # if (i > j){
   #   finaldata1$Cmax <- sdata$CmaxStand
  #  } else finaldata1$Cmax <- NA
  #} 
#}
  
#dat3 <- merge(finaldata1, sdata, by=intersect(finaldata1$code(finaldata1), sdata$code(sdata)))


#
 
#View(finaldata1)

#sdata1 <- data.frame(sdata)
#sdata1 <- sdata1[unique(sdata1$code), 1:10]

#finaldata1 <- merge(finaldata, sdata1, by = c("code"))

#finaldata1 <- finaldata1[unique(finaldata1$code,finaldata1$EC50), 1:12]

#str(finaldata1)
#write.csv(finaldata1, file = 'cleaned2.csv' )


