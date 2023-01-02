# Sampling uncertainty functions

mcmcRandomSamplerCat <- function(numberOfSamples, mcmcMatrix, ageVector, ageTotals){
  
  outDf <- matrix(NA,nrow=numberOfSamples, ncol = length(ageVector))
  
  for (i in 1:numberOfSamples){
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample <- mcmcMatrix[randomNumber,1]
    
    newRow <- 1 - exp(-ageVector*(lambdaSample))
    updateRow <- c()
    for(j in 1:length(ageTotals)){
      randomlySampleBinomialDis <- rbinom(1,size = ageTotals[j],prob = newRow[j])
      if(randomlySampleBinomialDis > 0){
        result <- randomlySampleBinomialDis / ageTotals[j]
      } else {
        result <- 0
      }
      updateRow[j] <- result
    }
    outDf[i,] <- updateRow
  }
  outDf
}


ageQuantiles <- function(mcmcDF){
  quantileMatrix <- matrix(NA,nrow=ncol(mcmcDF), ncol = 3)
  for(i in 1:ncol(mcmcDF)){
    quantiles <- mcmcDF[,i] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrix[i,] <- quantiles
  }
  quantileMatrix
}


# sampling uncertainty for time-discrete model 
mcmcRandomSamplerCatTimeDiscrete <- function(numberOfSamples, mcmcMatrix, ageVector, ageTotals){
  
  outDf <- matrix(NA,nrow=numberOfSamples, ncol = length(ageVector))
  
  for (i in 1:numberOfSamples){

    randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    lambdaSample1 <- mcmcMatrix[randomNumber,"lambda_1"]
    lambdaSample2 <- mcmcMatrix[randomNumber,"lambda_2"]
    deltaSample   <- mcmcMatrix[randomNumber,"delta"]
    
    newRow <- ifelse(ageVector > (2022-deltaSample), 1-exp(-(lambdaSample1*(ageVector-(2022-deltaSample)) + lambdaSample2*(2022-deltaSample))),
                     1-exp(-lambdaSample2*ageVector))
    updateRow <- c()
    for(j in 1:length(ageTotals)){
      randomlySampleBinomialDis <- rbinom(1,size = ageTotals[j],prob = newRow[j])
      if(randomlySampleBinomialDis > 0){
        result <- randomlySampleBinomialDis / ageTotals[j]
      } else {
        result <- 0
      }
      updateRow[j] <- result
    }
    outDf[i,] <- updateRow
  }
  outDf
}




