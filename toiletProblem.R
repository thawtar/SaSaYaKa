BestToiletProb <- function(n,k)
  {
  ntrials <- 1000
  success_indicator <- numeric(ntrials) #1 if we select the best toilet
  for (trial in 1:ntrials){
    toilet_score <- rnorm(n)
    best_toilet_index <- which.max(toilet_score)
    
    #Learning phase (size of learning phase is k)
    M <- max(toilet_score[1:k]) #max of toilet score in the learning phase
    
    #Exploitation phase 
    #We want to pick the first toilet with a better score than M
    toilet_index <- k + which(toilet_score[(k+1):n]>M)[1]
    
    #Forced to go with the last toilet if nothing is better
    if (is.na(toilet_index)){
      toilet_index <- n
    }
    success_indicator[trial] <- (toilet_index==best_toilet_index)
  }
  mean(success_indicator)
}
