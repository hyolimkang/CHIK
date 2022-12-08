##time-varying catalyitc model

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
library(readxl)
chik_systematic_review_v1 <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "prac")
View(chik_systematic_review_v1)
#Rename data
df_chik = chik_systematic_review_v1 

df_chik <-  df_chik %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2) %>% filter(study == 4)

df_chik[,c("midpoint","lower","upper")] = binom.confint(df_chik$N.pos, df_chik$N, method="exact")[,c("mean","lower","upper")]

# Model1: discrete FOI model (estimate time point where FOI changes: 1 time change - assuming that FoI changes before/after outbreak)
jcode <- "model{ 
	for (i in 1:length(N)){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] > (survey[i]-delta), 1-exp(-(lambda_1*(age[i]-(survey[i]-delta)) + lambda_2*(survey[i]-delta))),
                            1-exp(-lambda_2*age[i]))
    # time-varying catalytic model
	}

	#  prior dists
  lambda_1 ~ dunif(0,1)       #uninformative prior
  lambda_2 ~ dunif(0,1)       #uninformative prior
  delta    ~ dunif(1939,2018) #uninformative prior
}"

# Model2: discrete FOI model (estimate time point where FOI changes: 2 time changes)
jcode <- "model{ 
	for (i in 1:length(N)){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]>(survey[i]-delta1), 1-exp(-(lambda_1*(age[i]-(survey[i]-delta1)) + lambda_2*(delta2-delta1) + lambda_3*(survey[i]-delta2)))
       , ifelse(age[i]>survey[i]-delta2 && age[i]<survey[i]-delta1, 1-exp(-(lambda_2*(age[i]-(survey[i]-delta2)) + lambda_3*(survey[i]-delta2))),
       1-exp(-lambda_3*age[i])))
    # time-varying catalytic model
	}

	#  prior dists
  lambda_1 ~ dunif(0,1)       #uninformative prior
  lambda_2 ~ dunif(0,1)       #uninformative prior
  lambda_3 ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1939,2018)  #uninformative prior
  delta2  ~ dunif(1939,2018)  #uninformative prior

}"

# Model3: FoI varies in 4 age groups defined in the seroprevalence data 
jcode <- "model{ 
	for (i in 1:length(N)){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]>1 && age[i]<20, 1-exp(-10*lambda1)
       ,ifelse(age[i]>20 && age[i]<40, 1-exp(-(20*lambda1+10*lambda2)),
       ifelse(age[i]>40 && age[i]<60, 1-exp(-(20*lambda1+20*lambda2+10*lambda3)),
       1-exp(-(20*lambda1+20*lambda2+20*lambda3+10*lambda4)))))
    # time-varying catalytic model
	}

	#  prior dists
  lambda1 ~ dunif(0,1)       #uninformative prior
  lambda2 ~ dunif(0,1)       #uninformative prior
  lambda3 ~ dunif(0,1)       #uninformative prior
  lambda4 ~ dunif(0,1)       #uninformative prior

}"

#vector 
paramVector1 <- c("lambda_1", "lambda_2", "delta")
paramVector2 <- c("lambda_1", "lambda_2", "lambda_3", "delta1", "delta2")
paramVector3 <- c("lambda1", "lambda2", "lambda3", "lambda4")

# Run model
mcmc.length=50000
jdat = list(n.pos  = df_chik$N.pos,
            N      = df_chik$N,
            age    = df_chik$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=5, n.adapt = 15000)
update(jmod, 1000)
jpos = coda.samples(jmod, paramVector3, n.iter=mcmc.length)
plot(jpos) # check convergence

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

#test
jpos_matrix <- as.matrix(jpos)
jpos_array <- coda::as.array.mcmc.list(jpos,chains = TRUE)
MCMCsummary(jpos, round = 2)

# calculate DIC 
dic.samples(jmod, n.iter = mcmc.length)

### Outputting point estimates for inclusion within tables

for(i in 1:length(paramEstimates)){
  var = paramEstimates[[i]]
  varOut = paste(round(var[[1]],2)," (",round(var[[2]],2)," - ",round(var[[3]],2),")",sep = "")
  varOutput = c(varOut)
}

paramDat = data.frame(paramVector3,varOutput)

##creating datasets for plotting 
## Sample from mcmc chains for credible intervals
## Add binomial sampling uncertainty

## 1. Sample from mcmc chain to get 95% credible intervals (model uncertainty)
ager=1:80
range1=rep(10, times = 20)
range2=rep(20, times = 20)
numSamples = 1000

# Sampling for Model1

for(ii in 1:length(paramVector1)) {
  
  outDf <- matrix(NA,nrow=numSamples, ncol = length(ager))
  
  for (kk in 1:numSamples) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample_1 <- mcmcMatrix[randomNumber,"lambda_1"]
    lambdaSample_2 <- mcmcMatrix[randomNumber,"lambda_2"]
    deltaSample    <- mcmcMatrix[randomNumber,"delta"]
    
    
    newRow <-  ifelse(ager > (2018-deltaSample), 1-exp(-(lambdaSample_1*(ager-(2018-deltaSample)) + lambdaSample_2*(2018-deltaSample))),
                      1-exp(-lambdaSample_2*ager))
    outDf[kk,] <- newRow
  }
}

# Sampling for Model2 

for(ii in 1:length(paramVector2)) {
  
  outDf <- matrix(NA,nrow=numSamples, ncol = length(ager))
  
  for (kk in 1:numSamples) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample_1 <- mcmcMatrix[randomNumber,"lambda_1"]
    lambdaSample_2 <- mcmcMatrix[randomNumber,"lambda_2"]
    lambdaSample_3 <- mcmcMatrix[randomNumber,"lambda_3"]
    deltaSample1   <- mcmcMatrix[randomNumber,"delta1"]
    deltaSample2   <- mcmcMatrix[randomNumber,"delta2"]
    
    
    newRow <-  ifelse(ager>(2018-deltaSample1), 1-exp(-(lambdaSample_1*(ager-(2018-deltaSample1)) + lambdaSample_2*(deltaSample2-deltaSample1) + lambdaSample_3*(2018-deltaSample2)))
                      , ifelse(ager<2018-deltaSample2, 1-exp(-lambdaSample_3*ager), 1-exp(-(lambdaSample_2*(ager-(2018-deltaSample2)) + lambdaSample_3*(2018-deltaSample2)))))
    outDf[kk,] <- newRow
  }
}

# sampling for Model3
for(ii in 1:length(paramVector3)) {
  
  for(i in 1:4) {
    assign(paste0("outDf_", i), matrix(NA, nrow=numSamples, ncol = length(range1)))
  }
  
  for (kk in 1:numSamples) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample_1 <- mcmcMatrix[randomNumber,"lambda1"]
    lambdaSample_2 <- mcmcMatrix[randomNumber,"lambda2"]
    lambdaSample_3 <- mcmcMatrix[randomNumber,"lambda3"]
    lambdaSample_4 <- mcmcMatrix[randomNumber,"lambda4"]
    
    #seroprevalence for 4 different age groups 
    newRow1 <-  1-exp(-range1*lambdaSample_1)
    newRow2 <-  1-exp(-(range2*lambdaSample_1+range1*lambdaSample_2))
    newRow3 <-  1-exp(-(range2*lambdaSample_1+range2*lambdaSample_2+range1*lambdaSample_3))
    newRow4 <-  1-exp(-(range2*lambdaSample_1+range2*lambdaSample_2+range2*lambdaSample_3+range1*lambdaSample_4))
    
    #store results of each age group's seroprevalence in 4 matrices
    outDf_1[kk,] <- newRow1
    outDf_2[kk,] <- newRow2
    outDf_3[kk,] <- newRow3
    outDf_4[kk,] <- newRow4
    
    #combine seroprevalence of 4 age groups 
    outDf <- cbind(outDf_1,outDf_2,outDf_3,outDf_4)
    
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

################################
##  Point Estimates for Model 3 
###############################
lambda1PointEst <- mcmcMatrix[,"lambda1"] %>% quantile(probs=c(.5,.025,.975))
lambda2PointEst <- mcmcMatrix[,"lambda2"] %>% quantile(probs=c(.5,.025,.975))
lambda3PointEst <- mcmcMatrix[,"lambda3"] %>% quantile(probs=c(.5,.025,.975))
lambda4PointEst <- mcmcMatrix[,"lambda4"] %>% quantile(probs=c(.5,.025,.975))

paramEstimates <- list(lambda1PointEst, lambda2PointEst, lambda3PointEst,lambda4PointEst)

# 50% values  
foiEstimates_1 = paramEstimates[[1]]
foiEstimates_1 <- data.frame(foiEstimates_1)
lambda_1 <- foiEstimates_1[1,]
foiEstimates_2 = paramEstimates[[2]]
foiEstimates_2 <- data.frame(foiEstimates_2)
lambda_2 <- foiEstimates_2[1,]
foiEstimates_3 = paramEstimates[[3]]
foiEstimates_3 <- data.frame(foiEstimates_3)
lambda_3 <- foiEstimates_3[1,]
foiEstimates_4 = paramEstimates[[4]]
foiEstimates_4 <- data.frame(foiEstimates_4)
lambda_4 <- foiEstimates_4[1,]

Foi1<-data.frame(transpose(foiEstimates_1))
Foi1$year <- c("1939-1958")
Foi2<-data.frame(transpose(foiEstimates_2))
Foi2$year <- c("1959-1978")
Foi3<-data.frame(transpose(foiEstimates_3))
Foi3$year <- c("1979-1998")
Foi4<-data.frame(transpose(foiEstimates_4))
Foi4$year <- c("1999-2018")

# FOI estiamtes bars
ggplot()+
  geom_errorbar(data = Foi1, aes(x   = year,
                                 ymin= foiEstimates_1.1,
                                 ymax= foiEstimates_1.2),
                color = "#558C8C", width = 0.1)+
  geom_point(data = Foi1, aes(x=year, y=foiEstimates_1),color = "#558C8C")+
  geom_errorbar(data = Foi2, aes(x   = year,
                                 ymin= foiEstimates_2.1,
                                 ymax= foiEstimates_2.2),
                color = "#C05746", width = 0.1)+
  geom_point(data = Foi2, aes(x=year, y=foiEstimates_2),color = "#C05746")+
  geom_errorbar(data = Foi3, aes(x   = year,
                                 ymin= foiEstimates_3.1,
                                 ymax= foiEstimates_3.2),
                color = "#075E9D", width = 0.1) +
  geom_point(data = Foi3, aes(x=year, y=foiEstimates_3),color = "#075E9D")+
  geom_errorbar(data = Foi4, aes(x   = year,
                                 ymin= foiEstimates_4.1,
                                 ymax= foiEstimates_4.2),
                color = "#CD0BBC", width = 0.1) +
  geom_point(data = Foi4, aes(x=year, y=foiEstimates_4),color = "#CD0BBC")+
  xlab("Time Periods") + ylab("Force of Infection (FoI)") +
  theme_bw()



