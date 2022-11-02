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


library(readxl)
chik_systematic_review_v1 <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "prac")
View(chik_systematic_review_v1)

library(readxl)
chik_systematic_review_v1 <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "prac")
View(chik_systematic_review_v1)

df_chik = chik_systematic_review_v1 

df_chik[,c("mid","lo","hi")] = binom.confint(df_chik$N.pos, df_chik$N, method="exact")[,c("mean","lower","upper")]

df_chik <-  df_chik %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

datS1 <- df_chik[df_chik$study == "1",]
datS2 <- df_chik[df_chik$study == "2",]
datS3 <- df_chik[df_chik$study == "3",]


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

# create data  for plots 
ager=0:85
numSamples = 1000
foiVector = paramVector[1:3]

for (ii in 1:length(foiVector)) {
  outDf <- matrix(NA,nrow=numSamples, ncol = length(ager))
  foiStudy <- foiVector[ii]
  for (kk in 1:numSamples) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    lambdaSample <- mcmcMatrix[randomNumber,foiStudy]
    newRow <- 1-exp(-lambdaSample*ager)
    outDf[kk,] <- newRow
  }
  quantileMatrix <- matrix(NA,nrow=ncol(outDf), ncol = 3)
  for(jj in 1:ncol(outDf)){
    quantiles <- outDf[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrix[jj,] <- quantile
  }
  df_upperLower = data.frame(
    midpoint = ager,
    upper = quantileMatrix[,3],
    lower = quantileMatrix[,2]
  )
  
  assign(paste0("df_", foiVector[ii]), df_upperLower)
}


ggplot(df_mod_chik, aes(x= agemid, y= mid, ymin=lo, ymax=hi)) +
  geom_ribbon(alpha=0.2)+
  geom_line()+
  geom_point(data=df_chik)+
  geom_linerange(data=df_chik) +
  xlab("Age (years)") + ylab("seropositivity") +
  scale_x_continuous(breaks=seq(0,100,by=5))
