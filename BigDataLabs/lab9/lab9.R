library(class)
library(ggplot2)
library(fossil)
library(e1071)
library(caret)

#set.seed(5) #фуууу
set.seed(16)
sample1 <- data.frame(x = rnorm(200,3, 1.5), y = rnorm(200,3, 1.5))
sample2 <- data.frame(x = rnorm(200,9, 1), y = rnorm(200,2, 1))
sample3 <- data.frame(x = rnorm(200,9, 1), y = rnorm(200,6, 1))


#task1 <- function(){
data <- rbind(sample1, sample2, sample3)

# Кластеризация методом k-средних
kmeans_model <- kmeans(data, centers = 3)

plot(data$x, data$y, pch=21, cex = 2, bg=c("red","blue", "green") [unclass(kmeans_model$cluster)], main="Метод K-средних")
points(kmeans_model$centers[,1], kmeans_model$centers[,2], pch=3, col="black", cex = 4, lwd = 3)

#}

task2 <- function(){
  print("Метод К-ближайших соседей")
  
  class1 <- rep(1, 200)
  class2 <- rep(2, 200)
  class3 <- rep(3, 200)
  
  # Объединение данных
  x <- c(sample1$x, sample2$x, sample3$x)
  y <- c(sample1$y, sample2$y, sample3$y)
  classes <- kmeans_model$cluster
  
  data <- data.frame(x, y, classes)
  
  trainIndex <- sample(1:nrow(data), 0.8*nrow(data))
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  
  knn_pred <- knn(train = trainData[,1:2], test = testData[,1:2], cl = trainData[,3], k = 10)
  
  knn_train <- knn(train = trainData[,1:2], test = trainData[,1:2], cl = trainData[,3], k = 10)
  accuracy_knn_test <- sum(knn_train == trainData$classes) / length(trainData$classes)
  print("Обучение")
  print(confusionMatrix(as.factor(trainData$classes), as.factor(knn_train))$table)
  print(paste("Accuracy:", accuracy_knn_test))
  
  
  
  
  accuracy_knn_test <- sum(knn_pred == testData$classes) / length(testData$classes)
  print("Тестовые")
  print(confusionMatrix(as.factor(testData$classes), as.factor(knn_pred))$table)
  print(paste("Accuracy:", accuracy_knn_test))
  
  plot(testData[,1], testData[,2], pch=21, bg=c("red","blue", "green") [unclass(testData$classes)], main="Метод K-ближайших соседей")
  points(sample1$x, sample1$y, pch=3, col="darkred", cex = 2, lwd = 2)
  points(sample2$x, sample2$y, pch=3, col="darkblue", cex = 2, lwd = 2)
  points(sample3$x, sample3$y, pch=3, col="darkgreen", cex = 2, lwd = 2)
  points(testData[,1], testData[,2], pch=21, bg=c("red","blue", "green") [unclass(testData$classes)], cex = 3)
  
  
}

task3 <- function(){
  print("Наивный метод Байеса")
  
  data1 <- data.frame(x = sample1$x, y = sample1$y, class = kmeans_model$cluster[1:200])
  data2 <- data.frame(x = sample2$x, y = sample2$y, class = kmeans_model$cluster[201:400])
  data3 <- data.frame(x = sample3$x, y = sample3$y, class = kmeans_model$cluster[401:600])
  data <- rbind(data1, data2, data3)
  
  # Разделение данных на обучающую и тестовую выборки
  trainIndex <- sample(1:nrow(data), 0.8 * nrow(data))
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  
  # Обучение модели наивного байесовского классификатора
  model <- naiveBayes(class ~ x + y, data=trainData)
  
  predict_train <- predict(model, trainData)
  
  accuracy_bayes_train <- sum(predict_train == trainData$class) / length(trainData$class)
  print("Обучение")
  print(confusionMatrix(as.factor(trainData$class), as.factor(predict_train))$table)
  print(paste("Accuracy:", accuracy_bayes_train))
  
  # Предсказание классов для тестовых данных
  predicted <- predict(model, testData)
  
  accuracy_bayes <- sum(predicted == testData$class) / length(testData$class)
  
  print("Тест")
  print(confusionMatrix(as.factor(testData$class), as.factor(predicted))$table)
  print(paste("Accuracy:", accuracy_bayes))
  
  plot(testData[,1], testData[,2], pch=21, bg=c("red","blue", "green") [unclass(testData$class)], main="Наивный метод Байеса")
  points(sample1$x, sample1$y, pch=3, col="darkred", cex = 2, lwd = 2)
  points(sample2$x, sample2$y, pch=3, col="darkblue", cex = 2, lwd = 2)
  points(sample3$x, sample3$y, pch=3, col="darkgreen", cex = 2, lwd = 2)
  points(testData[,1], testData[,2], pch=21, bg=c("red","blue", "green") [unclass(testData$class)], cex = 3)
}

task1 <- function(){
  print("Метод K-средних")
  clusters <- c(rep(1, 200), rep(2, 200), rep(3, 200))
  print(confusionMatrix(as.factor(clusters), as.factor(kmeans_model$cluster))$table)
  print(paste("Accuracy:", rand.index(clusters, kmeans_model$cluster)))
  
}

task1()
task2()
task3()

