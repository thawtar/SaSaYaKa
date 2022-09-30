## Task 2 
BestToiletProb <- function(n,k){
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

ToiletDistribution <- function(n,k){
  ntrials <- 1000
  success_indicator <- numeric(ntrials) #1 if we select the best toilet
  ranks <- numeric(ntrials)
  for (trial in 1:ntrials){
    toilet_score <- rnorm(n)
    best_toilet_index <- which.max(toilet_score)
    sortedScores <- sort(toilet_score,decreasing=TRUE)
    S
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


## Task 3
#Try varying k
n = 100
prob <- numeric(n)
for (k in 1:n){
  prob[k] <- BestToiletProb(n,k)  
}
plot(1:n,prob,col="red",main="Prob. of choosing best toilet",
     xlab="k: size of learning phase",ylab="probability")
ind_max <- which.max(prob)
max_prob <- max(prob)
points(ind_max,max_prob,pch='*',col="black",cex=2)
ind_max
max_prob

## In-class discussion: Closed-form expression
n=100
k=1:n
x=k/n
y=x*log(1/x)

plot(1:n,prob,col="red",main="Prob. of choosing the best toilet",
     xlab="k: size of learning period",ylab="probability")
lines(k,y)

legend("topright", legend=c("Simulation", "Closed-form"),
       col=c("red", "black"), lty=c(1,1), pch=c(1,1),cex=1)