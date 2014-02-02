#odabir skupa za treniranje i testiranje
idx_train <- sample(nrow(train), 1000, replace = FALSE)
 
xtrain <- train[idx_train, ]
xtest <- train[-(idx_train),]
  
labels <- as.factor(xtrain[,1])
xtrain <- xtrain[,-1]
correct_labels <- xtest[,1]
xtest <- xtest[,-1]

#treniranje i testiranje algoritama  
rf <- randomForest(xtrain, labels, xtest, ntree=50)
knn.results <- (0:9)[knn(xtrain, xtest, labels, k = 10, algorithm="cover_tree")]
rf.predictions <- as.numeric(levels(labels)[rf$test$predicted])

#vrijednosti za tablicu kontigencije  
n00 <- sum(rf.predictions != correct_labels & knn.results!= correct_labels )
n01 <- sum(rf.predictions != correct_labels & knn.results== correct_labels )
n10 <- sum(rf.predictions == correct_labels & knn.results!= correct_labels )
n11 <- sum(rf.predictions == correct_labels & knn.results== correct_labels )
n<-n00 + n01 + n10 + n11
contigencies <- matrix(c(n00, n01,   n10, n11), nrow = 2, ncol = 2)

#Test razlika
p_A <- (n00 + n01)/n
p_B <- (n00 + n10)/n
p <- (p_A + p_B)/2
se <- sqrt(2*p*(1-p)/n)
z = (p_A-p_B)/se
odbaci_h0.test_razlika <- abs(z)> 1.96
