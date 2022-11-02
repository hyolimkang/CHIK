library(readxl)
require(tidyverse)
require(rjags)
require(binom)
library(ggplot2)
library(dplyr)
library(varhandle)
require(MCMCvis)
library(cowplot)
library(Rsero)

##Read file
chik_systematic_review_v1 <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "prac")
#Rename data
df_chik = chik_systematic_review_v1 

df_chik[,c("mid","lo","hi")] = binom.confint(df_chik$N.pos, df_chik$N, method="exact")[,c("mean","lower","upper")]

df_chik <-  df_chik %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

#sub-setting datafiles by study
datS1 <- df_chik[df_chik$study == "1",]
datS2 <- df_chik[df_chik$study == "2",]
datS3 <- df_chik[df_chik$study == "3",]

# Define model
jcode <- "model{ 
	for (i in 1:3){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda_1*age[i]) #catalytic model
	}
	for (i in 4:7){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda_2*age[i]) #catalytic model
	}
	for (i in 8:10){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda_3*age[i]) #catalytic model
		}
	#  prior dists
  lambda_1 ~ dunif(0,1) #uninformative prior
  lambda_2 ~ dunif(0,1) #uninformative prior
  lambda_3 ~ dunif(0,1) #uninformative prior
  
}"

#vector 
paramVector <- c("lambda_1", "lambda_2", "lambda_3")

# Run model
mcmc.length=50000
jdat = list(n.pos= df_chik$N.pos,
            N=df_chik$N,
            age=df_chik$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVector , n.iter=mcmc.length)
plot(jpos) # check convergence

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

#test
jpos_matrix <- as.matrix(jpos)
jpos_array <- coda::as.array.mcmc.list(jpos,chains = TRUE)
MCMCsummary(jpos, round = 2)


######################
##  Point Estimates
####################
lambda1PointEst <- mcmcMatrix[,"lambda_1"] %>% quantile(probs=c(.5,.025,.975))
lambda2PointEst <- mcmcMatrix[,"lambda_2"] %>% quantile(probs=c(.5,.025,.975))
lambda3PointEst <- mcmcMatrix[,"lambda_3"] %>% quantile(probs=c(.5,.025,.975))

paramEstimates <- list(lambda1PointEst, lambda2PointEst, lambda3PointEst)

### Outputting point estimates for inclusion within tables

for(i in 1:length(paramEstimates)){
  var = paramEstimates[[i]]
  varOut = paste(round(var[[1]],2)," (",round(var[[2]],2)," - ",round(var[[3]],2),")",sep = "")
  varOutput = c(varOut)
}

paramDat = data.frame(paramVector,varOutput)

##creating datasets for plotting 
## Sample from mcmc chains for credible intervals
## Add binomial sampling uncertainty

## 1. Sample from mcmc chain to get 95% credible intervals (model uncertainty)
ager1=14:42
ager2=5:85
ager3=15:49

numSamples = 1000
outDf_1 <- matrix(NA, nrow=numSamples, ncol= length(ager1))
outDf_2 <- matrix(NA, nrow=numSamples, ncol= length(ager2))
outDf_3 <- matrix(NA, nrow=numSamples, ncol= length(ager3))

for (i in 1:numSamples ) {
  randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  
  lambda1Sample <- mcmcMatrix[randomNumber, "lambda_1"]
  lambda2Sample <- mcmcMatrix[randomNumber, "lambda_2"]
  lambda3Sample <- mcmcMatrix[randomNumber, "lambda_3"]
  
  newRow_1 <- 1-exp(-lambda1Sample*ager1)
  newRow_2 <- 1-exp(-lambda2Sample*ager2)
  newRow_3 <- 1-exp(-lambda3Sample*ager3)
  
    outDf_1[i,] <- newRow_1
    outDf_2[i,] <- newRow_2
    outDf_3[i,] <- newRow_3
    
  }


