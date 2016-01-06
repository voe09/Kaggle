library(e1071)
library(FSelector)

#read training data
train_features.data <- read.csv('train.csv',header = FALSE)
train_response.data <- read.csv('trainLabels.csv',header = FALSE, col.name = 'response')
train.data <- cbind(train_features.data, train_response.data)

#Clean data
str(train.data)
train.data$response <- as.factor(train.data$response)

#Use Random Forest Importance to select features
weights <- random.forest.importance(response ~ ., train.data, importance.type = 1)
feature_index <- c(1:40)
weights <- cbind(weights, feature_index)
eights[order(weights$attr_importance, decreasing = TRUE),]
train_filter.data <- train.data[,c(15,13,40,37,19,7,30,5,33,29,35,24,8,23,39,41)]

#Use Radian Kernal SVM

#Use 10-fold cross-validation to select tunning parameter
#According to the result, we choose cost = 2.7, gamma = 0.09
cost_list <- c(0.01, 0.1, 1, 2.7, 10, 100, 150, 200, 250, 300, 350)
gamma_list <- c(0.0001, 0.0005, 0.0007, 0,001, 0.01, 0.09, 0.015, 0.02, 0.025, 0.03, 0.1, 1)
for(i in cost_list) {
    for(j in gamma_list){
        set.seed(10)
        model <- svm(response ~ ., data = train_filter.data, kernel = 'radial',gamma = j, cost = i, cross = 10)
        accuracy <- summary(model)$tot.accuracy
        observation <- c(i, j, accuracy)
        print(observation)
    }
}

#Use PCA to reduce dimensions, and use cross-validation to select the number of principle components
#Thus we choose the first 13 princple components
train_PCA.data <- data.frame(prcomp(train_filter.data[, c(-16)], scale = TRUE)$x)
train_PCA.data <- cbind(train_PCA.data, train_response.data)
train_PCA.data$response <- as.factor(train_PCA.data$response)
pc_num <- c(1:15)
for(k in pc_num) {
    set.seed(100)
    model <- svm(response ~ ., data = train_PCA.data[, c(1:k, 16)], kernel = 'radial',gamma = 0.09, cost = 2.7, cross = 10)
    accuracy <- summary(model)$tot.accuracy
    observation <- c(1, 0.03, k, accuracy)
    print(observation)
}

#At last the model is the following
set.seed(10)
model <- svm(response ~ ., data = train_PCA.data[, c(1:13, 16)], kernal = 'radial', gamma = 0.09, cost = 2.7, cross = 10)

#Read test data
test_feature.data <- read.csv('test.csv', header = FALSE)

#Manipulate the test features
test_feature.data <- test_feature.data[,c(15,13,40,37,19,7,30,5,33,29,35,24,8,23,39)]
test_PCA.data <- data.frame(prcomp(test_feature.data, scale = TRUE)$x)[,c(1:13)]
test_response.data <- predict(model, newdata = test_PCA.data)
test_response.data <- as.integer(as.character(test_response.data))
test_ID <- c(1:9000)
test.data <- data.frame(cbind(test_ID, test_response.data))
names(test.data) <- c('Id', 'Solution')
write.csv(test.data, file = 'solution.csv')

