#inicijalizacija listi za spremanje vjerojatnosti
p_A_1_arr <- list(rep(0, 5))
p_B_1_arr <- list(rep(0, 5))
 
p_A_2_arr <- list(rep(0, 5))
p_B_2_arr <- list(rep(0, 5))
  
#5 puta uzorkovanje
for (i in 1:5){
    #izrada disjunktnih skupova S_1 i S_2
    cv5x2.idx <- sample(nrow(train), 2000, replace = FALSE)
    n <- length(cv5x2.idx) / 2
    S_1.idx <- cv5x2.idx[c(1:n)]
    S_2.idx <- cv5x2.idx[c((n+1):(2*n))]
    
    # treniramo s S_1, testiramo s S_2
    # ...
    p_A_1_arr[i] <- (n00 + n01)/n
    p_B_1_arr[i] <- (n00 + n10)/n
    
    # treniramo s S_2, testiramo s S_1
    # ...
    p_A_2_arr[i] <- (n00 + n01)/n
    p_B_2_arr[i] <- (n00 + n10)/n
    
  }
  #izracun prema Diettrichovoj formuli
  p_1 <- unlist(p_A_1_arr) - unlist(p_B_1_arr)
  p_2 <- unlist(p_A_2_arr) - unlist(p_B_2_arr)
  p.hat <- (p_1 + p_2) / 2
  s.var <- (p_1 - p.hat)^2 + (p_2 - p.hat)^2
  #ovdje je moguce dodati poboljsanje prema Alpaydinu
  T_obs.5xcv <- p_1[1] / sqrt(sum(s.var)/5)
  odbaci_h0.5xcvtest <- (abs(T_obs.5xcv) > 2.05)
  
