#This is possible feature engineerings of Kaggle Titanic

#Load train data
train.data <- read.csv('train.csv', stringsAsFactors = FALSE)

#Inspect train data and clean train data
str(train.data)
train.data$Survived <- as.factor(train.data$Survived)
train.data$Pclass <- as.factor(train.data$Pclass)
train.data$Sex <- as.factor(train.data$Sex)
train.data$Embarked <- as.factor(train.data$Embarked)

#Transfer original feature name into useful feature Title
train.data$Title <- sapply(train.data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
train.data$Title <- sub(' ', '', train.data$Title)
train.data$Title[train.data$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
train.data$Title[train.data$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
train.data$Title[train.data$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
train.data$Title <- factor(train.data$Title)

#Combine features SibSp and Parch into FamilySize, reduce dimensions
train.data$FamilySize <- train.data$SibSp + train.data$Parch + 1

#Find the people in the same family, with the same Surname and concerning the familysize
train.data$Title <- sapply(train.data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
train.data$FamilyID <- paste(as.character(train.data$FamilySize), train.data$Surname, sep="")
train.data$FamilyID[train.data$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(train.data$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
train.data$FamilyID[train.data$FamilyID %in% famIDs$Var1] <- 'Small'
train.data$FamilyID <- factor(train.data$FamilyID)
