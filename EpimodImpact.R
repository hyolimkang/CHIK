library(data.table)
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
hi <- qbeta(0.975, alpha,beta)

gen_rbinom <- function(x){
  rbinom(1,1, as.integer(x))
}

EpiRand <- data.table(matrix(nrow=79, ncol=1000))

for(j in 1:1000){
  binom_vals <- rbinom(79,1, p = EpiPeriod$inv)
  EpiRand[,j] <- sample(binom_vals, 79)
}

colnames(EpiRand) <- paste(1:1000)

fwrite(EpiRand, "D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/dataresults/EpiRand.csv")


EpiSus <- 1-outDf

  IncidenceRow <- matrix(NA, nrow=80, ncol = 1)
  IncidenceDf  <- matrix(NA, nrow=numSamples, ncol = 80)

  colnames(IncidenceDf) <- paste(0:79)
  IncidenceDf[, c(1:4)]<-0
  
  IncidenceDf[,5] <- outDf[,5]-outDf[,4]
  IncidenceDf[,6:19] <- IncidenceDf[,5]
  IncidenceDf[,20:39] <- outDf[,20]-outDf[,19]
  IncidenceDf[,40:79] <- outDf[,40]-outDf[,39]
  
PreburdenEpi <- data.frame(matrix(NA, nrow = nrow(EpiRand), ncol = ncol(EpiRand)))


