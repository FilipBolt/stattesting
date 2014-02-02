#varijabla kojom se biljezi pojava dvostruke razlike u performansama 
S = 0
#broj uzoraka
M = 20
#apriori pretpostavljena razlika u performansama
d_1 = 0.1

  for (i in 1:M){
    idx_train <- sample(nrow(train), 1000, replace = TRUE)
    
    xtrain <- train[idx_train, ]
    xtest <- train[-(idx_train),]
    
    labels <- as.factor(xtrain[,1])
    xtrain <- xtrain[,-1]
    correct_labels <- xtest[,1]
    xtest <- xtest[,-1]  
    
    rf <- randomForest(xtrain, labels, xtest, ntree=50)
    knn.results <- (0:9)[knn(xtrain, xtest, labels, k = 10, algorithm="cover_tree")]
    rf.predictions <- as.numeric(levels(labels)[rf$test$predicted])
    
    rf.result <- sum(rf.predictions == correct_labels) / length(correct_labels)
    knn.result <- sum(knn.results== correct_labels ) / length(correct_labels)
    
    if (rf.result - knn.result > 2*d_1){
      S <- S + 1
    }
  }
#konacna vrijednost boostrapa
boostrap.p <- S / M

