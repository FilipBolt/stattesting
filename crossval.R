#odabir parametra k
k <- 10
p_A_arr <- list(rep(0, k))
p_B_arr <- list(rep(0, k))


subset.size <- nrow(train) / k
cross_val.idx <- sample(nrow(train), 2000, replace = FALSE)
i<-1
for (i in 1:k){
	#izgradnja skupova za test i trening
	cross_val.idx_test <- cross_val.idx[c(i:(i+subset.size-1))]
	cross_val.test <- train[cross_val.idx_test,]
	cross_val.train <- train[-cross_val.idx_test,]
    
	labels <- as.factor(cross_val.train[,1])
	cross_val.train <- cross_val.train[,-1]
	correct_labels <- cross_val.test[,1]
	cross_val.test <- cross_val.test[,-1]
    
	rf <- randomForest(cross_val.train, labels, cross_val.test, ntree=50)
	knn.results <- (0:9)[knn(cross_val.train, cross_val.test, labels, k = 10, algorithm="cover_tree")]
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
cross_val_t_test<-t.test(unlist(p_A_arr), unlist(p_B_arr), paired=TRUE)
odbaci_h0.cross_val_t_test <- (abs(cross_val_t_test$statistic) > 2.05)
