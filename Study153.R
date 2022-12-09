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
  dplyr::mutate(agemid = (age_min+age_max)/2) %>% filter(study == 5)

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
# Run model
mcmc.length=50000
jdat = list(n.pos= df_chik$N.pos,
            N=df_chik$N,
            age=df_chik$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=1, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVector, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

#output
numSamples=1000
ager=1:100

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




