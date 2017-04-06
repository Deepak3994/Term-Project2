#libraries
library("car", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("caret", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("ROCR", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

#Importing the datsheet
datasheet <- read.csv('/home/deepak/Documents/projects/Term-Project2/Datasheet1.csv', header = TRUE, sep = ",", na.strings = "?")
write.csv(datasheet, file = "Data2.csv")
data <- read.csv('/home/deepak/Documents/projects/Term-Project2/Data2.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE)


#Removing thest extra columns
data$X <- NULL
data$NA. <- NULL


#To get the count of NA values
#colSums(is.na(data))

#To replace the numeric value
#na.aggregate(data$Age)
data$Age[is.na(data$Age)] <- 51
data$Blood.Pressure[is.na(data$Blood.Pressure)] <- 76
data$Specific.Gravity[is.na(data$Specific.Gravity)] <- 1.017
data$Albumin[is.na(data$Albumin)] <- 1
data$Sugar[is.na(data$Sugar)] <- 0
data$Blood.Glucose.Random[is.na(data$Blood.Glucose.Random)] <- 148
data$Blood.Urea[is.na(data$Blood.Urea)] <- 57
data$Serum.Creatinine[is.na(data$Serum.Creatinine)] <- 3.07
data$Sodium[is.na(data$Sodium)] <- 137
data$Potassium[is.na(data$Potassium)] <- 4.6
data$Hemoglobin[is.na(data$Hemoglobin)] <- 12.5
data$Packed.Cell.Volume[is.na(data$Packed.Cell.Volume)] <- 38
data$White.Blood.Cell.Count[is.na(data$White.Blood.Cell.Count)] <- 8406
data$Red.Blood.Cell.Count[is.na(data$Red.Blood.Cell.Count)] <- 4.7

#To replace the categorical value
# count(data, 'Red.Blood.Cells')
data$Red.Blood.Cells[is.na(data$Red.Blood.Cells)] <- "normal"
data$Pus.Cell[is.na(data$Pus.Cell)] <- "normal"
data$Pus.Cell.clumps[is.na(data$Pus.Cell.clumps)] <- "notpresent"
data$Bacteria[is.na(data$Bacteria)] <- "notpresent"
data$Hypertension[is.na(data$Hypertension)] <- "no"
data$Diabetes.Mellitus[is.na(data$Diabetes.Mellitus)] <- "no"
data$Coronary.Artery.Disease[is.na(data$Coronary.Artery.Disease)] <- "no"
data$Appetite[is.na(data$Appetite)] <- "good"
data$Pedal.Edema[is.na(data$Pedal.Edema)] <- "no"
data$Anemia[is.na(data$Anemia)] <- "no"

data[,c(25)] <- trimws(data[,c(25)])
data[,c(20)] <- trimws(data[,c(20)])

write.csv(data, file="/home/deepak/Documents/projects/Term-Project2/properdata.csv")

#Applying chi-square test
#Applyting independant t-test
#Using step-wise forward approach

data <- subset(data, select = -c(Age,Blood.Pressure,Sugar,Red.Blood.Cells,Pus.Cell,Pus.Cell.clumps,Bacteria,Blood.Glucose.Random,Blood.Urea,Sodium,Potassium,Hemoglobin,Packed.Cell.Volume,White.Blood.Cell.Count,Red.Blood.Cell.Count,Hypertension,Diabetes.Mellitus,Coronary.Artery.Disease,Appetite,Pedal.Edema,Anemia))
data$Class <- recode(data$Class,"'notckd'= 0; 'ckd'=1")

#Training and testing (70-30)
#caret - createDataPartition
sample_size <- floor(0.70 * nrow(data))
train_ind1 <- sample(seq_len(nrow(data)), size=sample_size)
train <- data[train_ind1, ]
test <- data[-train_ind1, ]

#Logistic regression algorithm
model <- glm(Class ~ Serum.Creatinine + Specific.Gravity + Albumin, family=binomial(logit), data=train)
summary(model)
predicted <- predict(model, type = 'response')
prediction<- predict(model, test)
predictedvalue <- ifelse(prediction>0.5,1,0)

#Confusion matrix
value <- table(test$Class, predictedvalue > 0.5)
#To calculate the accuracy of the model
sum(diag(value))/sum(value)

#ROC curve (Receiver operation curve)
ROCRpred <- prediction(predicted, train$Class)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#To check the model prediction with test data set
# prediction<- predict(model, test)
output <- cbind(test, predictedvalue)
output






#Decidion tree algorithm
library("rattle", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("party", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("rpart", lib.loc="/usr/lib/R/library")
library("rpart.plot", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("RColorBrewer", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
data1 <- read.csv('/home/deepak/Documents/projects/Term-Project2/properdata.csv', header = TRUE, sep = ",", stringsAsFactors = TRUE)
data1$X <- NULL
sample_size1 <- floor(0.70 * nrow(data1))
train_ind <- sample(seq_len(nrow(data1)), size=sample_size1)
train1 <- data1[train_ind, ]
test1 <- data1[-train_ind, ]

#decision tree
fit <- rpart(Class ~ Age + Blood.Pressure + Specific.Gravity + Albumin + Sugar + Red.Blood.Cells + Pus.Cell + Pus.Cell.clumps + Bacteria + Blood.Glucose.Random + Blood.Urea + Serum.Creatinine + Sodium + Potassium + Hemoglobin + Packed.Cell.Volume + White.Blood.Cell.Count + Red.Blood.Cell.Count + Hypertension + Diabetes.Mellitus + Coronary.Artery.Disease + Appetite + Pedal.Edema + Anemia, data = train1 ,method="class")
fancyRpartPlot(fit)
prediction1 <- predict(fit, test1, type = "class")
prediction1 <- as.numeric(prediction1)
predictedvalue1 <- ifelse(prediction1>0.5,1,0)
value1 <- table(test1$Class, predictedvalue1 > 0.5)
(value1[1,1]+42)/(78+42+10+7)




