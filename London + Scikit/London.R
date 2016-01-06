install.packages("e1071")
.packages(all.available = TRUE)

library(e1071)

train <- read.csv('new_data.csv',header = FALSE)
trainlabel <- read.csv('trainLabels.csv',header = FALSE,col.names = "lable")

RandomF_feture<- c(15,13,40,37,19,7,30,5,33,29,35,24,8,23,39)

train.x<- train[,RandomF_feture]
train.y<- trainlabel


set.seed(10)
#$B2<LL???9TE*(Bcost$BOB(Bgamma???$B2D0JJ|0lBgBO?t!$(Btune$BH!?t2q<+9THf??????P$;2?t;;=PE*(Berror
tune.radial<-tune(svm,train.x,train.y, kernel='radial', ranges = 
                    list(cost=c(2.7),gamma=c(0.09)),scale = T)
performance_radial<-tune.radial$performances

performance_radial

rate<- (1-performance_radial[1,3])*100
print(rate)