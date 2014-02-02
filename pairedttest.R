#odabir broja ponavljanja uzorkovanja
ponavljanje <- 30
p_A_arr <- list(rep(0, ponavljanje))
p_B_arr <- list(rep(0, ponavljanje))
  
for (i in 1:ponavljanje){
  idx_train <- sample(nrow(train), 1000, replace = FALSE)
   
  xtrain <- train[idx_train, ]
  xtest <- train[-(idx_train),]
   
  labels <- as.factor(xtrain[,1])
  xtrain <- xtrain[,-1]
  correct_labels <- xtest[,1]
  xtest <- xtest[,-1]
  
  rf <- randomForest(xtrain, labels, xtest, ntree=50)
  knn.results <- (0:9)[knn(xtrain, xtest, labels, k = 10, algorithm="cover_tree")]
  rf.predictions <- as.numeric(levels(labels)[rf$test$predicted])
  n00 <- sum(rf.predictions != correct_labels & knn.results!= correct_labels )
  n01 <- sum(rf.predictions != correct_labels & knn.results== correct_labels )
  n10 <- sum(rf.predictions == correct_labels & knn.results!= correct_labels )
  n11 <- sum(rf.predictions == correct_labels & knn.results== correct_labels )
  n<-n00 + n01 + n10 + n11
  p_A_arr[i] <- (n00 + n01)/n
  p_B_arr[i] <- (n00 + n10)/n
}

#ispitivanje znacajnosti
paired_t_test_resample <- t.test(unlist(p_A_arr), unlist(p_B_arr), paired=TRUE)

odbaci_h0.upareni_t_test <- abs(paired_t_test_resample$statistic) > 2.05
