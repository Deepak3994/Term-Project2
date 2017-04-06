svm_data <- read.csv("/home/deepak/Documents/projects/Term-Project2/properdata.csv",header = TRUE, sep = ',')
svm_data <- svm_data[-1]
dim(svm_data)
head(svm_data,5)
attach(svm_data)

library("class")
svmfit <- svm(Class ~ ., data = svm_data, kernel = "linear", cost = 10, scale = FALSE) # linear svm, scaling turned OFF

print(svmfit)
summary(svmfit)

plot(svmfit, svm_data)

compareTable <- table (svm_data$Class, predict(svmfit))  # tabulate

compareTable
mean(svm_data$Class != predict(svmfit)) # 19.44% misclassification error


svmfit <- svm(Class ~ ., data = svm_data, kernel = "radial", cost = 10, scale = FALSE) # linear svm, scaling turned OFF

print(svmfit)
summary(svmfit)

plot(svmfit, svm_data)

compareTable <- table (svm_data$Class, predict(svmfit))  # tabulate
compareTable

mean(svm_data$Class != predict(svmfit))


set.seed(100) # for reproducing results

rowIndices <- 1 : nrow(svm_data) # prepare row indices

sampleSize <- 0.8 * length(rowIndices) # training sample size

trainingRows <- sample (rowIndices, sampleSize) # random sampling

trainingData <- svm_data[trainingRows, ] # training data

testData <- svm_data[-trainingRows, ] # test data

tuned <- tune.svm(Class ~., data = trainingData, gamma = 10^(-6:-1), cost = 10^(1:2)) # tune

summary (tuned) # to select best gamma and cost


svmfit <- svm(Class ~ ., data = svm_data, kernel = "radial", cost = 100,gamma = 0.1, scale = FALSE) # linear svm, scaling turned OFF
svmfit
print(svmfit)
summary(svmfit)


plot(svmfit, svm_data)

compareTable <- table (testData$Class, predict(svmfit, testData))  # tabulate
compareTable
mean(svm_data$Class != predict(svmfit, testData))

svmpred <- predict(compareTable, testData[,-10])
predict <- predict(compareTable, type = 'response')


plot (tuned, transform.x=log10, xlab=expression(log[10](gamma)), ylab="C")

# plot(tuned$trainingData$Class~compareTable$testData$Class, ylab = 'predicted', xlab = 'observed')
# abline(0,1, col=2)

# n_points_in_grid = 60 # num grid points in a line
# x_axis_range <- range (svm_data[, 5]) # range of X axis
# y_axis_range <- range (svm_data[, 1]) # range of Y axis
# X_grid_points <- seq (from=x_axis_range[1], to=x_axis_range[5], length=n_points_in_grid) # grid points along x-axis

x <- subset(svm_data, select=-Class)
y <- Class


svm_model <- svm(Class ~ ., data=svm_data)
summary(svm_model)


pred <- predict(svm_model,x)
system.time(pred <- predict(svm_model,x))

table(pred,y)


svm_tune <- tune(svm, train=x, train=y,kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)