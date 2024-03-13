library(data.table)
library(dplyr)
library(MASS)
library(fitdistrplus)

epidt <- data.table(mcmcMatrix)
EpiPeriod <- epidt[, c("dis1", "dis2", "avg") := .(delta1 - delta2, delta2 - delta3, ((delta1 - delta2) + (delta2 - delta3))/2)]
EpiPeriod <- EpiPeriod %>% mutate(inv = 1/avg)

fit <- fitdist(EpiPeriod$inv, "beta") # estimating beta dist parameter
alpha <- fit$estimate[1]
beta  <- fit$estimate[2]

lo <- qbeta(0.025, alpha,beta)
hi <- qbeta(0.975, alpha,beta)
mid <- qbeta(0.5, alpha,beta)

gen_rbinom <- function(x){
  rbinom(1,1, as.integer(x))
}

EpiRand <- as.data.frame(matrix(nrow=79, ncol=1000))

for(j in 1:1000){
  binom_vals <- rbinom(79,1, p = EpiPeriod$inv)
  EpiRand[,j] <- sample(binom_vals, 79)
}

colnames(EpiRand) <- paste(1:1000)



agemat <- matrix(NA, nrow = 79, ncol = 79)
agemat[,1] <- c(0:78)
agemat[,2] <- agemat[,1]-1
agemat[,3] <- agemat[,2]-1

agemat[,1] <- c(0:78)
for(i in 2:ncol(agemat)) {
  agemat[,i] <- agemat[,i-1]-1 
}
agemat[agemat < 0] <- NA
colnames(agemat) <- paste(2019:2097)
agemat <- as.data.frame(agemat)


allobyear <- list()
for(i in 1:1000){
  result    <- which(EpiRand[,i]==1)
  allobyear[[i]] <- result 
}


for(ii in 1:length(lambdaSample)){
  for (kk in 1:numSamples) {
  lambdaSample <- mcmcMatrix[sample(nrow(mcmcMatrix), 1, replace=T), "lambda"]
  for(j in 1:length(numbs)){
      prevmat1 <- 1 - exp(-lambdaSample*numbs[j])
      if(j == 1) {
       prevmat[kk,1]        <- prevmat1
      }else{
       prevmat[kk,20*(j-1)] <- prevmat1
   }
  }
 }
}
prevmat[is.na(prevmat)] <- 0
susmat <- 1 - prevmat
incmat <- prevmat



# initialize matlist with empty data frames
result_list <- vector("list", length = length(allobyear)) 
numCols <- sapply(result_list, function(x) length(x[[1]]))
matlist <- lapply(numCols, function(n) data.frame(matrix(nrow = numSamples, ncol = nrow(EpiRand))))
inclist <- lapply(numCols, function(n) data.frame(matrix(nrow = numSamples, ncol = nrow(EpiRand))))
incresult <- vector("list", length = length(allobyear)) 

for (ii in 1:length(matlist)) {
  for (kk in 1:numSamples) {
    lambdaSample <- mcmcMatrix[sample(nrow(mcmcMatrix), 1, replace=T), "lambda"]
    result_list <- lapply(allobyear, function(x) {
      result <- 1 - exp(-lambdaSample*seq_along(x))
      if(all(result == 0)) {
        return(NA)
      }else{
        return(result)
      }
    })
    if(length(result_list[[ii]]>0)){
      for (jj in 1:length(result_list[[ii]])) {
        matlist[[ii]][kk,jj] <- result_list[[ii]][jj]
       }
      }
    }
}  # extracting 1000 matrices based on 1000 columns in EpiRand 

for(i in 1:length(allobyear)){
  colnames(matlist[[i]]) <- paste0(allobyear[[i]])
} # attaching colnames based on allobyear 

suslist = matlist
for(i in 1: length(allobyear)){
  suslist[[i]] <- 1 - matlist[[i]]
}

newcols <- lapply(numCols, function(n) data.frame(matrix(nrow = numSamples, ncol = 1)))

for(i in 1:length(allobyear)){
  for(kk in 1:numSamples){
    lambdaSample <- mcmcMatrix[sample(nrow(mcmcMatrix), 1, replace=T), "lambda"]
    if(length(allobyear[[i]]) > 0) {
      result <- exp(-lambdaSample*length(allobyear[[i]])) - exp(-lambdaSample*(length(allobyear[[i]])+1))
      newcols[[i]][kk,] <- result
    }
  }
}

for(i in 1:length(allobyear)){
  suslist[[i]] <- suslist[[i]][ , colSums(is.na(suslist[[i]]))==0]
}

subtract <- function(col){
  return(diff(col))
}

for(i in 1:length(allobyear)){
  inclist[[i]] <- apply(matlist[[i]],1,subtract)
  inclist[[i]] <- t(inclist[[i]])
  inclist[[i]] <- inclist[[i]][ , colSums(is.na(inclist[[i]]))==0]
  inclist[[i]] <- cbind(inclist[[i]], newcols[[i]])
  if(length(allobyear[[i]]) > 0) {
    colnames(inclist[[i]]) <- paste(1:length(allobyear[[i]]))
  }
} # incidence lists for 1000 matrices

cohorts <- paste0("benin", seq(2011,2020))
bcohort <- mget(cohorts)

for(i in 1:10){
  colnames(bcohort[[i]]) <- paste(0:100)
}   # name values in the columns

#for each column in binary outbreak matrix, first create N (=length(allobyear)) of age matrix 
bcohort[[1]][,allobyear[[1]]]

cohortlist <- lapply(allobyear, function(year){
  data.frame(matrix(nrow = numSamples, ncol = length(year)))
})

rep_lists <- list()
for(i in 1:length(allobyear)){
  rep_lists[[i]] <- replicate(10, cohortlist[[i]], simplify = F)
}


for(i in 1:10){
  for(j in 1:length(allobyear)){
    rep_lists[[i]] <- bcohort[[i]][,allobyear[[j]]]
  }
}


# columns to extract
cols_to_extract <- c(allobyear[[1]])
cols_to_extract[2:5]

newcol_list <- list()
# calculate decreasing column indices

for(i in 1:10){
    newcol_val <- cols_to_extract - i 
    newcol_list <- c(newcol_list, list(newcol_val))
  }

newcol_list <- lapply(newcol_list, function(x) x[x >=0])



newlist <- list()
  for(j in 1:10){
    value <- bcohort[[1]][newcol_list[[j]]]
    newlist[[j]] <- value
}


foimatrix <- matrix(NA, nrow = 79, ncol = 1000)

for(i in 1:1000){
  foimatrix[EpiRand[,i] == 0 , i] <- 0
  foimatrix[EpiRand[,i] >  0 , i] <- sample(mcmcMatrix[,"lambda"], sum(EpiRand[,i]), replace=TRUE)
  
}




