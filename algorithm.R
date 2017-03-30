### R code from vignette source 'algorithm.Rnw'

###################################################
### code chunk number 1: algorithm.Rnw:22-48
###################################################

setwd("C:/Users/eelsayed/Google Drive/CSE6242")
rawDatalLoaded <- TRUE

if(file.exists("mnist_train.csv")){
  train <- read.csv(file="mnist_train.csv", header = FALSE)
}else{
  rawDatalLoaded <- FALSE
}

if(file.exists("mnist_test.csv")){
  test <- read.csv(file="mnist_test.csv", header = FALSE)
}else{
  rawDatalLoaded <- FALSE
}

if(!rawDatalLoaded){
  print("Data wasn't loaded correctly.")
}

train <- as.data.frame(t(train))
names(train)[785] <- "Label"

test <- as.data.frame(t(test))
names(test)[785] <- "Label"



###################################################
### code chunk number 2: algorithm.Rnw:53-55
###################################################
train_0_1 <- train[(train$Label == 0) | (train$Label == 1),]
train_3_5 <- train[(train$Label == 3) | (train$Label == 5),]


###################################################
### code chunk number 3: algorithm.Rnw:60-62
###################################################
test_0_1 <- test[(test$Label == 0) | (test$Label == 1),]
test_3_5 <- test[(test$Label == 3) | (test$Label == 5),]


###################################################
### code chunk number 4: algorithm.Rnw:67-78
###################################################
true_label_train_0_1 <- train_0_1$Label
train_0_1 <- subset(train_0_1, select = names(train_0_1) != "Label" )

true_label_train_3_5 <- train_3_5$Label
train_3_5 <- subset(train_3_5, select = names(train_3_5) != "Label" )

true_label_test_0_1 <- test_0_1$Label
test_0_1 <- subset(test_0_1, select = names(test_0_1) != "Label" )

true_label_test_3_5 <- test_3_5$Label
test_3_5 <- subset(test_3_5, select = names(test_3_5) != "Label" )


###################################################
### code chunk number 5: g
###################################################
save_digit_image <- function(df, digitClass, imageTitle, fileName) {
  tmp <- df[df$Label == digitClass,]
  m <- matrix(unlist(tmp[1,1:784]), ncol = 28, byrow = TRUE)
  
  jpeg(filename = fileName)
  image(z = m, col = gray.colors(256))
  title(main = imageTitle)
  dev.off()
}
save_digit_image(train, 0, "Class label : 0", "0.jpg")
save_digit_image(train, 1, "Class label : 1", "1.jpg")
save_digit_image(train, 3, "Class label : 3", "3.jpg")
save_digit_image(train, 5, "Class label : 5", "5.jpg")


###################################################
### code chunk number 6: algorithm.Rnw:152-181
###################################################
# Gredient Descent
gradient_descent <- function(X, Y, XY, theta){
    z <- as.matrix(X) %*% theta
    yz_exp <- 1 / (1 + exp(-Y * z))
    # Precalculate that instead for performance
    #tmp <- apply(X,2,function(x) X * Y)
    delta_theta <- t(XY) %*% yz_exp
    return(delta_theta)
}

# Logistic Regression
# threshold should be higher than 100 but I used 100 by default to speed up the calculations need for the report
logistic_regression <- function(X, Y, deviation, alpha, eta, seed = 100, threshold = 100) {
  # initializations
  set.seed(seed)
  X$intercept <- 1
  theta <- rnorm(ncol(X), sd = deviation)
  delta_theta <- rep(10000, ncol(X))
  numIterations <- 1
  
  #XY <- apply(X,2,function(x) X * Y)
  XY <- sweep(X, 1,Y, "*")
  while (any(abs(alpha * delta_theta/(theta + 0.0001)) > eta) & numIterations < threshold){
    delta_theta <- gradient_descent(X,Y, XY,theta)
    theta <- theta - alpha * delta_theta
    numIterations <- numIterations + 1
  }
  return(theta)
}


###################################################
### code chunk number 7: algorithm.Rnw:187-204
###################################################
# Calculate the predictions
predict <- function(thetasFromTraining, classLabel){
  classLabel$intercept <- 1
  -sign(as.matrix(classLabel) %*% thetasFromTraining)
}

# Calculate the accuracy
accuracy <- function(model_function, X, Y, deviation, alpha = 0.001, eta = 0.000001, seed = 100, threshold = 100){
  model_output <- model_function(X, Y, deviation, alpha, eta, seed, threshold)
  a <- predict(model_output, X)
  tmp <- mean(Y == a)
  return(tmp)
}

theta <- rnorm(ncol(train_0_1), sd = 0.5)
accuracy(logistic_regression, train_0_1, true_label_train_0_1, deviation = 0.5)
accuracy(logistic_regression, train_3_5, true_label_train_3_5, deviation = 0.5)


###################################################
### code chunk number 8: algorithm.Rnw:210-236
###################################################

train_0_1_accuracies <- numeric()
for(i in 1:10){ 
  train_0_1_accuracies <- append(train_0_1_accuracies, accuracy(logistic_regression, train_0_1, true_label_train_0_1, deviation = 0.5 ))
}
mean(train_0_1_accuracies)

train_3_5_accuracies <- numeric()
for(i in 1:10){ 
  train_3_5_accuracies <- append(train_3_5_accuracies, accuracy(logistic_regression, train_3_5, true_label_train_3_5, deviation = 0.5))
}
mean(train_3_5_accuracies)


test_0_1_accuracies <- numeric()
for(i in 1:10){
 test_0_1_accuracies <- append(test_0_1_accuracies, accuracy(logistic_regression, test_0_1, true_label_test_0_1, deviation = 0.5)) 
}
mean(test_0_1_accuracies)

test_3_5_accuracies <- numeric()
for(i in 1:10){
  test_3_5_accuracies <- append(test_3_5_accuracies, accuracy(logistic_regression, test_3_5, true_label_test_3_5, deviation = 0.5))
}
mean(test_3_5_accuracies)



###################################################
### code chunk number 9: algorithm.Rnw:270-300
###################################################
train_0_1_accuracies <- numeric()
t <- rnorm(ncol(train_0_1), sd = 0.7)
for(i in 1:10){ 
  train_0_1_accuracies <- append(train_0_1_accuracies, 
                                 accuracy(logistic_regression, 
                                          train_0_1, true_label_train_0_1, 
                                          deviation = 0.7
                                          )
                                 )
}
mean(train_0_1_accuracies)

train_3_5_accuracies <- numeric()
for(i in 1:10){ 
  train_3_5_accuracies <- append(train_3_5_accuracies, accuracy(logistic_regression, train_3_5, true_label_train_3_5, deviation = 0.7))
}
mean(train_3_5_accuracies)


test_0_1_accuracies <- numeric()
for(i in 1:10){
 test_0_1_accuracies <- append(test_0_1_accuracies, accuracy(logistic_regression, test_0_1, true_label_test_0_1, deviation = 0.7)) 
}
mean(test_0_1_accuracies)

test_3_5_accuracies <- numeric()
for(i in 1:10){
  test_3_5_accuracies <- append(test_3_5_accuracies, accuracy(logistic_regression, test_3_5, true_label_test_3_5, deviation = 0.7))
}
mean(test_3_5_accuracies)


###################################################
### code chunk number 10: algorithm.Rnw:317-341
###################################################
train_0_1_accuracies <- numeric()
for(i in 1:10){ 
  train_0_1_accuracies <- append(train_0_1_accuracies, accuracy(logistic_regression, train_0_1, true_label_train_0_1, deviation = 0.5, alpha = 0.01, eta = 0.0001, threshold = 1000))
}
mean(train_0_1_accuracies)

train_3_5_accuracies <- numeric()
for(i in 1:10){ 
  train_3_5_accuracies <- append(train_3_5_accuracies, accuracy(logistic_regression, train_3_5, true_label_train_3_5, deviation = 0.5, alpha = 0.01, eta = 0.0001, threshold = 1000))
}
mean(train_3_5_accuracies)


test_0_1_accuracies <- numeric()
for(i in 1:10){
 test_0_1_accuracies <- append(test_0_1_accuracies, accuracy(logistic_regression, test_0_1, true_label_test_0_1, deviation = 0.5, alpha = 0.01, eta = 0.0001, threshold = 1000)) 
}
mean(test_0_1_accuracies)

test_3_5_accuracies <- numeric()
for(i in 1:10){
  test_3_5_accuracies <- append(test_3_5_accuracies, accuracy(logistic_regression, test_3_5, true_label_test_3_5, deviation = 0.5, alpha = 0.01, eta = 0.0001, threshold = 1000))
}
mean(test_3_5_accuracies)


###################################################
### code chunk number 11: algorithm.Rnw:347-406
###################################################
# Calculate the accuracy for samples of divisions in the training data using the passed cutRatio
accuracy <- function(model_function, X, Y, deviation, cutRatio, divisions = 10, alpha = 0.001, eta = 0.000001, seed = 100, threshold = 100){
  set.seed(seed)
  samples <- lapply(1:divisions, function(division){
    sample(1:nrow(X), round(cutRatio * nrow(X)))
    })
  
  samplesAccuracies <- sapply(samples, function(s){
    X <- X[sa,]
    Y <- as.data.frame(Y)
    Y <- Y[sa,]
    
    model_output <- model_function(X, Y, deviation, alpha, eta, seed, threshold)
    a <- predict(model_output, X)
    mean(Y == a)
  })
  
  return(mean(samplesAccuracies))
}

train_0_1_Splits_Accuracies <- data.frame(Split = double(), Accuracy = double())
for(i in 1:20)
{
  train_0_1_Splits_Accuracies <- rbind(train_0_1_Splits_Accuracies,
                                       data.frame(
                                         i/10,
                                         accuracy(logistic_regression, train_0_1, true_label_train_0_1, deviation = 0.5, cutRatio = i/10 )) 
                                       )
}

train_3_5_Splits_Accuracies <- data.frame(Split = double(), Accuracy = double())
for(i in 1:20)
{
  train_3_5_Splits_Accuracies <- rbind(train_3_5_Splits_Accuracies,
                                       data.frame(
                                         i/10,
                                         accuracy(logistic_regression, train_3_5, true_label_train_3_5, deviation = 0.5, cutRatio = i/10 )) 
                                       )
}

test_0_1_Splits_Accuracies <- data.frame(Split = double(), Accuracy = double())
for(i in 1:20)
{
  test_0_1_Splits_Accuracies <- rbind(test_0_1_Splits_Accuracies,
                                       data.frame(
                                         i/10,
                                         accuracy(logistic_regression, test_0_1, true_label_test_0_1, deviation = 0.5, cutRatio = i/10 )) 
                                       )
}

test_3_5_Splits_Accuracies <- data.frame(Split = double(), Accuracy = double())
for(i in 1:20)
{
  test_3_5_Splits_Accuracies <- rbind(test_3_5_Splits_Accuracies,
                                       data.frame(
                                         i/10,
                                         accuracy(logistic_regression, test_3_5, true_label_test_3_5, deviation = 0.5, cutRatio = i/10 )) 
                                       )
}


