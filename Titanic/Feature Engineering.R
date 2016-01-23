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

# Weik below

#Select feature
feature<- c("Pclass","Age","Sex","Fare","FamilySize","Embarked")
new.data<- feature.data[,feature]

#Applying RF
library(randomForest)

train.y<- as.factor(train.data$Survived)
train.x<- new.data[c(1:nrow(train.data)),]

test<- new.data[-c(1:nrow(train.data)),]

rf<- randomForest(train.x, train.y,ntree = 500)

rfpredice<- predict(rf, test)

