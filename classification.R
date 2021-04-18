# load the CSV file from the local directory
df <- read.csv("Iris.csv", header=TRUE)
# Removing first column
df <- df[,c(-1)]
#EDA
head(df)
#checking missing values
colSums(is.na(df))
str(df)

#Summarizing class distribution
summary(df$Species)

#Summarize dataaet
summary(df)
#Mean and median are closeby, hence no outliers

#Uisng visualizations to see outliers
par(mfrow=c(1,1))
long <- melt(df[,-5])
plot(value~variable, data  = long, xlab="", ylab = "cent")

#checking data distribution
par(mfrow=c(2,2))
for (i in 1:4){
  hist(df[,i],main=names(df)[i], xlab="Cent", ylab="Freq")
}

#Hence the input variables are asymmetric
#Understanding relationship between the variables
ggscatmat(df, columns = 1:4, color = "Species")
#High degree of correlation between sepal len and sepal width
#High degree of correlation between petal length and petal width

#splitting test and train data
library(caret)
set.seed(100)
partition <- createDataPartition(y = df$Species, p = 0.8, list = F)
#selecting 20% for validation
validation <- df[-partition,]

#Rest 80% used for training and testing
train <- df[partition,]

#checking distribution in the training dataset, it should be equal since we used 
#createDataPartition function

#We reset random number seed before each algorithm so that the results are comparable

set.seed(7)
metric <- "Accuracy"
control <- trainControl(method = "cv", number = 10)

#LDA
set.seed(7)
fit.lda<- train(Species~., data=train, method = "lda", metric= metric, trControl= control)

# CART algorithm
set.seed(7)
fit.cart <- train(Species~., data=train, method="rpart", metric=metric, trControl=control)

# KNN algorithm
set.seed(7)
fit.knn <- train(Species~., data=train, method="knn", metric=metric, trControl=control)

# SVM algorithm
set.seed(7)
fit.svm <- train(Species~., data=train, method="svmRadial", metric=metric, trControl=control)

# RF (Random Forest) algorithm
set.seed(7)
fit.rf <- train(Species~., data=train, method="rf", metric=metric, trControl=control)

# NB (Naive Bayes) algorithm
set.seed(7)
fit.nb <- train(Species~., data=train, method="nb", metric=metric, trControl=control)

# LR (Logistic regression) algorithm
set.seed(7)
fit.lr <- train(Species~., data=train, method="multinom", metric=metric, trControl=control)


results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf, lr=fit.lr, nb=fit.nb))
summary(results)
dotplot(results)
#LR has best accuracy

prediction<- predict(fit.lr, validation)
confusionMatrix(prediction, validation$Species)

#validation by row
val <- as.data.frame(c(validation,as.data.frame(prediction)))
val





