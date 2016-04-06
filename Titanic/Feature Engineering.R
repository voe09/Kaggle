#This is possible feature engineerings of Kaggle Titanic

#Load train data
train.data <- read.csv('train.csv', stringsAsFactors = FALSE)
test.data <- read.csv('test.csv', stringsAsFactors = FALSE)

#combine the two data set's features, then do feature engineering
feature.data <- rbind(train.data[,c(-2)], test.data)

#Inspect train data and clean train data
str(feature.data)
feature.data$Pclass <- as.factor(feature.data$Pclass)
feature.data$Sex <- as.factor(feature.data$Sex)
feature.data$Embarked <- as.factor(feature.data$Embarked)

#Transfer original feature name into useful feature Title
feature.data$Title <- sapply(feature.data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
feature.data$Title <- sub(' ', '', feature.data$Title)
feature.data$Title[feature.data$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
feature.data$Title[feature.data$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
feature.data$Title[feature.data$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
feature.data$Title <- factor(feature.data$Title)

#Combine features SibSp and Parch into FamilySize, reduce dimensions
feature.data$FamilySize <- feature.data$SibSp + feature.data$Parch + 1

#Find the people in the same family, with the same Surname and concerning the familysize
feature.data$Surname<- sapply(feature.data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
feature.data$FamilyID <- paste(as.character(feature.data$FamilySize), feature.data$Surname, sep="")
feature.data$FamilyID[feature.data$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(feature.data$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
feature.data$FamilyID[feature.data$FamilyID %in% famIDs$Var1] <- 'Small'
feature.data$FamilyID <- factor(feature.data$FamilyID)

#Fill in the value of missing data in Fare
feature.data[is.na(feature.data$Fare),]$Fare <- mean(feature.data[feature.data$Pclass == 3,]$Fare, na.rm = TRUE)

#Fill in the value of missing data in Age by Title
fill_in_age_by_title <- function(obs) {
    if(is.na(obs['Age'])){
        title <- obs['Title']
        age <- mean(feature.data[feature.data$Title == title,]$Age, na.rm = TRUE)
        return(age)
    }else {
        return(obs['Age'])
    }
}
feature.data$Age <- apply(feature.data, 1, FUN=fill_in_age_by_title)
feature.data$Age <- as.numeric(feature.data$Age)


#Extract Cabin Num and deck from Cabin
feature.data$CabinNum <- sapply(feature.data$Cabin, function(x) strsplit(x, '[A-Z]')[[1]][2])
feature.data$CabinNum <- as.numeric(feature.data$CabinNum)
feature.data$Deck <- sapply(feature.data$Cabin, function(x) strsplit(x, '')[[1]][1])

#Cabin Num 1-50 as Front, 50-100 as Middle, >100 as End
feature.data$CabinPos <- NA
feature.data$CabinPos[feature.data$CabinNum<50]<-'Front'
feature.data$CabinPos[feature.data$CabinNum>=50 & feature.data$CabinNum<100]<-'Middle'
feature.data$CabinPos[feature.data$CabinNum>=100]<-'End'
feature.data$CabinPos <- as.factor(feature.data$CabinPos)
#If Cabin Num is odd, on the right, else, on the left
feature.data$CabinLoc <- NA
feature.data$CabinLoc[feature.data$CabinNum %% 2 == 0] <- 'left'
feature.data$CabinLoc[feature.data$CabinNum %% 2 != 0] <- 'right'
feature.data$CabinLoc <- as.factor(feature.data$CabinLoc)

# Weik below

#Select feature
feature<- c("Pclass","Age","Sex","Fare","Parch","SibSp","Embarked","CabinPos","CabinLoc")
new.data<- feature.data[,feature]
 
#Applying RF
library(randomForest)
 
train.y<- as.factor(train.data$Survived)
train.x<- new.data[c(1:nrow(train.data)),]
test<- new.data[-c(1:nrow(train.data)),]

#selcet data with cabin information
location_train<- which(is.na(train.x$CabinPos) == TRUE)
location_test<- which(is.na(test$CabinPos) == TRUE)

# model one: with cabin information
train.x1<- train.x[-location_train,]
train.y1<- train.y[-location_train]
test1<- test[-location_test,]

set.seed(10)
rf1<- randomForest(train.x1, train.y1,ntree = 10000, mtry = 8)
mean(rf1$err.rate[,1])
 
rfpredice1<- predict(rf1, test1)

#model two: without cabin information
feature2<- c("Pclass","Age","Sex","Fare","Parch","SibSp","Embarked")
train.x2<- train.x[location_train,feature2]
train.y2<- train.y[location_train]
test2<- test[location_test,feature2]

set.seed(10)
rf2<- randomForest(train.x2, train.y2,ntree = 10000, mtry = 3)
mean(rf2$err.rate[,1])

rfpredice2<- predict(rf2, test2)




