#The dataset set be loaded using MASS library
#Structure of code:
#Loading data
#Data prep
#EDA
#Model building and accuracy analysis
#Final Analysis


## DATA LOADING
library(MASS)
housing <- Boston

library(corrplot) #seeing correlation
library(ggplot2) #visualization
library(caTools) #splitting into test and train
library(dplyr) #data manipulation
library(plotly) #converting ggplot2 to interactive viz

## DATA PREP
colSums(is.na(housing)) # no NA

## EDA
str(housing)
#rad and chas are int
head(housing)
summary(housing)
#too much difference between mean and median of a variable indicates outliers

#finding correlation
corrplot(cor(housing))

## Data splitting into test and train
#sample.split should be used for classification since
#it splits data from vector Y into 2 bins in pre-defined
#ration while preserving ratios of different labels

#for regression sample fucntion should work
set.seed(100)
sample <- sample.int(n= nrow(housing), size= floor(0.75*nrow(housing)), replace =F)
train <- housing[sample,]
test <- housing[-sample,]


#Fitting Simple Linear regression
model <- lm(medv ~., data= train)
summary(model) #Multiple R square = 0.77, F= 98.74
#Null hypothesis: Coeff associated with var = 0
#Alternate hypotheis: Coef associated with var != 0
# Rsquare = 0.7786, F= 98.74
# P value < 0.05: Statistically significant
# P value > 0.05: Not statistically significant and indicates strong evidence of null hypothesis

#Improving the model
#removing the var which are not statistically significant

model1 <- lm(medv~.-age-indus, data = train)
summary(model1) #Multiple R square = 0.77, F= 117.1

# PART 2: Variable selection using best subset regression

model2<- regsubsets(medv~., data = train, nvmax= 13)
reg_summary= summary(model2)

#regsubset() has built in plot function
#finding model with largest Adjusted R^2
which.max(reg_summary$adjr2) #model with 11 variables
plot(model2, scale = "r2") #r^2 becomes 0.78 when we include 11 variables and does not
#change as we increase the number of variables as per the output.
plot(model2, scale ="bic")

# PART 3: Variable selection using stepwise regression

nullmodel <- lm(medv~1, data= train)
fullmodel <- lm(medv~., data = train)

#forward selection
model3 <- step(nullmodel, scope = list(lower= nullmodel, upper= fullmodel), direction = "forward")
#forward selection gives model with least AIC with 11 variables 
#medv ~ lstat + rm + ptratio + dis + black + chas + zn + crim + 
#nox + rad + tax
#These are the same variables which were given by best subset regression model with max Adjusted R2 value


#backward selection
model4 <- step(fullmodel, direction = "backward")
#model with least AIC: 
#medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
#black + lstat
#Hence same variables are covered


#STEPWISE SELECTION
model5 <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction="both")
AIC(model5)
BIC(model5)
summary(model5)


#MODEL ASSESSMENT
par(mfrow=c(2,2))
plot(model5)

#Residuals vs Fitted plot shows that the relationship between medv and predictors is not completely linear. Also, normal qq plot is skewed implying that residuals are not normally distributed. A different functional from may be required.

#Models are compared based on adjusted r square, AIC, BIC criteria for in-sample performance and mean square prediction error (MSPE) for out-of-sample performance

#In-sample performance
#MSE
model3.sum= summary(model3)
(model3.sum$sigma)^2
model3.sum$r.squared
model3.sum$adj.r.squared
AIC(model3)
BIC(model3)

#17.20317 sigma^2
#0.7782574 r^2
# 0.7716111 Adjusted R^2
# 2167.652 AIC
# 2218.84 BIC


model4.sum= summary(model4)
(model4.sum$sigma)^2
model4.sum$r.squared
model4.sum$adj.r.squared
AIC(model4)
BIC(model4)

#17.20317
#0.7782574
#0.7716111
#2167.652
#2218.84


model5.sum= summary(model5)
(model5.sum$sigma)^2
model5.sum$r.squared
model5.sum$adj.r.squared
AIC(model5)
BIC(model5)

#17.20317
#0.7782574
#0.7716111
#2167.652
#2218.84

#Hence same values for all the models
#Out-of-sample Prediction or test error (MSPE)
model3.pred.test <- predict(model3, newdata = test)
model3.mspe <- mean((model3.pred.test - test$medv) ^ 2)
model3.mspe
#39.63285

model4.pred.test <- predict(model4, newdata = test)
model4.mspe <- mean((model4.pred.test - test$medv) ^ 2)
model4.mspe
#39.63285

model5.pred.test <- predict(model5, newdata = test)
model5.mspe <- mean((model5.pred.test - test$medv) ^ 2)
model5.mspe
#39.63285

#All the statistics are same for all models using `forward`, `backward` and `both` variable selection techniques



#Cross Validation
model.glm = glm(medv ~ . -indus -age, data = housing)
x= cv.glm(data = housing, glmfit = model.glm, K = 5)
#23.17014

#We did not split the dataset into validation as showing validation was was not 
#the purpose here. Purpose was variable selection

##################### MSPE FOR TEST WAS = 39.63285###################
#We noted in above exercise that there might be some non linearity between medv and "x" variables
#Let's explore that

plot(medv~., data = housing) #non linearity between lstat and medv

#Implementing Generalized Additive model
#Please read more about gam, it's also a library in R
#Splines can only be included for continuous variables
#Hence cannot be applied on chas and rad

housing.gam <- gam(medv ~ s(crim) + s(zn) + s(indus) + s(nox) + s(rm) + s(age) + s(dis) + 
                     s(tax) + s(ptratio) + s(black) + s(lstat) + chas + rad, data = train)

summ = summary(housing.gam)
#Varibles with edf = 1 have linear relation with medv
#age and black

#Plotting non linear relation 
plot(housing.gam)

pred= predict(housing.gam, data= test)
mspe = mean(pred - test$medv)^2
#6.397296

#Very low as compared to the model formed by variable selection
#Hence going ahead with this model


### COMPARING AGAINST TREE BASED MODEL

#2 CART
library(rpart)
library(ROCR)
housing.rpart <- rpart(formula = medv ~ ., data = train, cp = 0.001)
housing.rpart
plot(housing.rpart)
text(housing.rpart, pretty=2)
plotcp(housing.rpart) #choosing cp = 0.029 and pruning the tree
text(housing.rpart)


pruned <- prune(housing.rpart, cp = 0.0077)
pruned
plot(pruned)
text(pruned, pretty = 2)

#Comparing MSE 
#Large tree: training data
mean((predict(housing.rpart)- train$medv)^2)
#6.827475

#Pruned tree: training data
mean((predict(pruned)- train$medv)^2)
#10.4313

#Out of sample MSE
mean((predict(pruned, newdata = test)- test$medv)^2)
#44.30111
#Worse than Gneralized linear regression model



#######################NEURAL NET#####################
######################################################

installed.packages("nnet")
library(nnet)

#For neural net implimentation we require scaled variables
maxs <- apply(housing, 2, max)
mins <- apply(housing, 2, min)

housing_scaled <- as.data.frame(scale(housing, center= mins, scale= maxs-mins))

#splitting dataset
set.seed(100)
sample_scaled <- sample.int(n= nrow(housing_scaled), size= floor(0.75*nrow(housing_scaled)), replace =F)
train_scaled <- housing_scaled[sample_scaled,]
test_scaled <- housing_scaled[-sample_scaled,]

#using as.data.frame in front of scale since it returns a matrix
#Number of neurons should be between the input and output layer size
#Usually 2/3 of input size

n <- names(train_scaled)
f<- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = "+")))
net <- neuralnet(f, data= train_scaled, hidden=c(5,3), linear.output = T)


#y~. is not acceptable in neuralnet()
#linear.output specifies if we want to do regression.
#If TRUE then regression, if FALSE then classification

plot(net)
predict_nn <- compute(net, test_scaled[,1:13])

#converting scaled predictions to original values
predicted_nn_unscale <- predict_nn$net.result*(max(housing$medv)- min(housing$medv))+min(housing$medv)

#converting scaled test to original value
test_original <- (test_scaled$medv)*(max(housing$medv)-min(housing$medv))+min(housing$medv)

#calculating MSE
MSE_nn <- sum((test_original- predicted_nn_unscale)^2)/nrow(test_scaled)
MSE_nn
#22.26195




