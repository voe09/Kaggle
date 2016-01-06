cost_list <- c(0.01, 0.1, 1, 10, 100, 150, 200, 250, 300, 350)
gamma_list <- c(0.0001, 0.0005, 0.0007, 0,001, 0.01, 0.015, 0.02, 0.025, 0.03, 0.1, 1)
for(i in cost_list) {
    for(j in gamma_list){
        set.seed(10)
        model <- svm(train.data$result.data ~ ., data = train.data, kernel = 'radial',gamma = j, cost = i, cross = 10)
        accuracy <- summary(model)$tot.accuracy
        observation <- c(i, j, accuracy)
        print(observation)
    }
}

pc_num <- c(1:40)
for(k in pc_num) {
    tr_data <- pc_train.data[, c(1:k, 41)]
    set.seed(100)
    tr_data$V41 <- as.factor(tr_data$V41)
    model <- svm(tr_data$V41 ~ ., data = tr_data, kernel = 'radial',gamma = 0.03, cost = 1, cross = 10)
    accuracy <- summary(model)$tot.accuracy
    observation <- c(1, 0.03, k, accuracy)
    print(observation)
}

weights <- random.forest.importance(result.data ~ ., train.data, importance.type = 1)
train_filter.data <- train.data[,c(15,13,40,37,19,7,30,5,33,29,35,24,8,23,39,6,14,18,20,26,25,17,2,9,28,4,27,41)]
model <- svm(result.data ~ ., data = train_filter.data, kernel = 'radial',gamma = 0.03, cost = 1, cross = 10)
