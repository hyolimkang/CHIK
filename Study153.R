# study 153 
library(readxl)
require(tidyverse)
require(rjags)
require(binom)
library(ggplot2)
library(dplyr)
library(varhandle)
require(MCMCvis)
library(cowplot)
options(scipen=999)
##Read file
chik_systematic_review_v1 <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "prac")
#Rename data
df_chik = chik_systematic_review_v1 

df_chik <-  df_chik %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2) %>% filter(study == 5) %>% filter(age_min <75)

df_chik[,c("midpoint","lower","upper")] = binom.confint(df_chik$N.pos, df_chik$N, method="exact")[,c("mean","lower","upper")]

# Define model
jcode <- "model{ 
	for (i in 1:length(N)){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# age cut-off model 
jcode <- "model{ 
	for (i in 1:length(N)){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] <- ifelse(age[i] < cutoff, 
    1-exp(-lambda_1*age[i]),
    1-((exp(-lambda_1*cutoff))*(exp((-lambda_2*(age[i]-cutoff))))))
loglik[i] <- logdensity.bin(n.pos[i],seropos_est[i],N[i])
	}
	#  prior dists
  lambda_1 ~ dunif(0,1) #uninformative prior
  lambda_2 ~ dunif(0,1) #uninformative prior
  cutoff   ~ dunif(0,70) #uninformative prior

}"
paramVector1 <- c("lambda_1", "lambda_2", "cutoff")

# age cut off and reverse catalytic
jcode <- "model{
for (i in 1:length(N)){ 
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < cutoff,
(lambda / (lambda + delta)) * (1 - exp(-(lambda + delta)*age[i])),
((lambda / (lambda+ delta))* (1-exp(-(lambda +delta)*cutoff)) 
- ((lambda*alpha) / ((lambda*alpha)+delta))) * exp(-((lambda*alpha) + delta)*(age[i]-cutoff)) 
+ ((lambda*alpha) / ((lambda*alpha)+delta)))
loglik[i] <- logdensity.bin(n.pos[i],seropos_est[i],N[i])
}
	#  prior dists
  lambda ~ dunif(0,1)  #uninformative prior
  delta  ~ dunif(0,1)  #uninformative prior
  cutoff ~ dunif(0,70) #uninformative prior
  alpha  ~ dgamma(5,5)  #uninformative prior

}"
paramVector2 <- c("lambda", "delta", "cutoff", "alpha")

# model4
jcode <- "model{ 
	for (i in 1:length(N)){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] > (2022-delta), 1-exp(-(lambda_1*(age[i]-(2022-delta)) + lambda_2*(2022-delta))),
                            1-exp(-lambda_2*age[i]))
    # time-varying catalytic model
	}

	#  prior dists
  lambda_1 ~ dunif(0,1)       #uninformative prior
  lambda_2 ~ dunif(0,1)       #uninformative prior
  delta    ~ dunif(2014,2015) #uninformative prior
}"
paramVector3 <- c("lambda_1", "lambda_2", "delta")

# Run model
mcmc.length=50000
jdat = list(n.pos= df_chik$N.pos,
            N=df_chik$N,
            age=df_chik$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=1, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVector3, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

#output
numSamples=1000
ager=1:80

# Sampling for Model1
for(ii in 1:length(paramVector)) {
  
  outDf <- matrix(NA,nrow=numSamples, ncol = length(ager))
  
  for (kk in 1:numSamples) {
    
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    lambdaSample <- mcmcMatrix[randomNumber,"lambda"]

    newRow <-  1-exp(-lambdaSample*ager)
    outDf[kk,] <- newRow
  }
}

# Sampling for Model2
for(ii in 1:length(paramVector1)) {
  
  outDf <- matrix(NA,nrow=numSamples, ncol = length(ager))
  
  for (kk in 1:numSamples) {
    
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    lambdaSample1 <- mcmcMatrix[randomNumber,"lambda_1"]
    lambdaSample2 <- mcmcMatrix[randomNumber,"lambda_2"]
    cutoffSample  <- mcmcMatrix[randomNumber,"cutoff"]
    
    newRow <-  ifelse(ager < cutoffSample, 
                      1-exp(-lambdaSample1*ager),
                      1-((exp(-lambdaSample1*cutoffSample))*(exp((-lambdaSample2*(ager-cutoffSample))))))
    outDf[kk,] <- newRow
  }
}

# Sampling for Model3
for(ii in 1:length(paramVector2)) {
  
  outDf <- matrix(NA,nrow=numSamples, ncol = length(ager))
  
  for (kk in 1:numSamples) {
    
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    lambdaSample <- mcmcMatrix[randomNumber,"lambda"]
    deltaSample  <- mcmcMatrix[randomNumber,"delta"]
    cutoffSample <- mcmcMatrix[randomNumber,"cutoff"]
    alphaSample  <- mcmcMatrix[randomNumber,"alpha"]
    
    newRow <- ifelse(ager < cutoffSample,
                     (lambdaSample / (lambdaSample + deltaSample)) * (1 - exp(-(lambdaSample + deltaSample)*ager)),
                     ((lambdaSample / (lambdaSample+ deltaSample))* (1-exp(-(lambdaSample +deltaSample)*cutoffSample)) 
                      - ((lambdaSample*alphaSample) / ((lambdaSample*alphaSample)+deltaSample))) * exp(-((lambdaSample*alphaSample) + deltaSample)*(ager-cutoffSample)) 
                     + ((lambdaSample*alphaSample) / ((lambdaSample*alphaSample)+deltaSample))) 
    outDf[kk,] <- newRow
  }
}

# Sampling for Model4
for(ii in 1:length(paramVector3)) {
  
  outDf <- matrix(NA,nrow=numSamples, ncol = length(ager))
  
  for (kk in 1:numSamples) {
    
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    lambdaSample1 <- mcmcMatrix[randomNumber,"lambda_1"]
    lambdaSample2 <- mcmcMatrix[randomNumber,"lambda_2"]
    deltaSample  <- mcmcMatrix[randomNumber,"delta"]
    
    newRow <- ifelse(ager > (2022-deltaSample), 1-exp(-(lambdaSample1*(ager-(2022-deltaSample)) + lambdaSample2*(2022-deltaSample))),
                     1-exp(-lambdaSample2*ager))
    outDf[kk,] <- newRow
  }
}

# get quantile matrices 
quantileMatrix_1 <- matrix(NA,nrow=ncol(outDf), ncol = 3)
for(jj in 1:ncol(outDf)){
  quantiles <- outDf[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_1[jj,] <- quantiles
  df_upperLower_1 <- cbind(ager, quantileMatrix_1)
  df_upperLower_1 <- as.data.frame(df_upperLower_1)
  colnames(df_upperLower_1) <- c('agemid', 'mean', 'upper', 'lower')
  
}

# Binomial sample uncertainty - accounts for the sample size of the underlying data

############################################################
## Plots
############################################################

ggplot()+
  geom_line(data = df_upperLower_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = df_chik, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = df_chik, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")


## FoI graph
lambda1PointEst <- mcmcMatrix[,"lambda_1"] %>% quantile(probs=c(.5,.025,.975))
lambda2PointEst <- mcmcMatrix[,"lambda_2"] %>% quantile(probs=c(.5,.025,.975))
deltaPointEst   <- mcmcMatrix[,"delta"] %>% quantile(probs=c(.5,.025,.975))

paramEstimates <- list(lambda1PointEst, lambda2PointEst, deltaPointEst)

foiEstimates_1 = paramEstimates[[1]]
foiEstimates_1 <- data.frame(foiEstimates_1)
lambda_1 <- foiEstimates_1[1,]
foiEstimates_2 = paramEstimates[[2]]
foiEstimates_2 <- data.frame(foiEstimates_2)
lambda_2 <- foiEstimates_2[1,]
deltaEstimates = paramEstimates[[3]]
deltaEstimates <- data.frame(deltaEstimates)
delta <- floor(deltaEstimates[1,])



data  = matrix(1930:2022,nrow=93, ncol = 1)
data1 = matrix(lambda_1, nrow=84, ncol = 1)
data2 = matrix(lambda_2, nrow=9, ncol = 1)
data3 = rbind(data1, data2)
FoIdata = cbind(data, data3)
FoIdata = data.frame(FoIdata)
colnames(FoIdata)[which(names(FoIdata) == "X1")] <- "Year"
colnames(FoIdata)[which(names(FoIdata) == "X2")] <- "FoI"

ggplot(data = FoIdata, aes(x= Year, y = FoI))+
  geom_line(color = "#558C8C")+
  theme_bw()

#adding sampling uncertainty to the graph
source("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/codes/CHIK/SamplingUncertaintyFunc.R")

ageVector <- df_chik$agemid
ageTotals <- df_chik$N

SampUncertainMCMCdiscrete <- mcmcRandomSamplerCatTimeDiscrete(1000, mcmcMatrix, ageVector, ageTotals)
ageQuantSampDiscrete      <- ageQuantiles(SampUncertainMCMCdiscrete)

df_sampling = data.frame(
  midpoint = df_chik$agemid,
  mean = ifelse(df_chik$agemid > (2022-delta), 1-exp(-(lambda_1*(df_chik$agemid-(2022-delta)) + lambda_2*(2022-delta))),
                1-exp(-lambda_2*ageVector)),
  upper = ageQuantSampDiscrete[,3],
  lower = ageQuantSampDiscrete[,2]
)

ggplot()+
  geom_line(data = df_upperLower_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = df_chik, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = df_chik, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  geom_ribbon(data = df_sampling, alpha=0.2, aes(x=midpoint, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")
