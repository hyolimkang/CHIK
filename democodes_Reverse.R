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
library(readxl)
chik_systematic_review_v1 <- read_excel("Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "Sheet1")
View(chik_systematic_review_v1)

chik_systematic_review_v1 <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "Sheet1")
#Rename data
df_chik = chik_systematic_review_v1 

df_chik <-  df_chik %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2) %>% filter(study == 4)

df_chik[,c("midpoint","lower","upper")] = binom.confint(df_chik$N.pos, df_chik$N, method="exact")[,c("mean","lower","upper")]


# Define model
jcode <- "model{ 
	for (i in 1:length(N)){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = (lambda / (lambda + delta)) * (1-exp(-(lambda + delta)*age[i])) #reverse catalytic model

		}
	#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
  delta  ~ dunif(0,5) #uninformative prior

}"

#vector 
paramVector <- c("lambda", "delta")

# Run model
mcmc.length=50000
jdat = list(n.pos= df_chik$N.pos,
            N=df_chik$N,
            age=df_chik$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=1, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVector , n.iter=mcmc.length)
plot(jpos) # check convergence

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

#test
jpos_matrix <- as.matrix(jpos)
jpos_array <- coda::as.array.mcmc.list(jpos,chains = TRUE)
MCMCsummary(jpos, round = 2)

# calculate DIC 
dic.samples(jmod, n.iter = mcmc.length)

######################
##  Point Estimates
####################
lambdaPointEst <- mcmcMatrix[,"lambda"] %>% quantile(probs=c(.5,.025,.975))
deltaPointEst <- mcmcMatrix[,"delta"] %>% quantile(probs=c(.5,.025,.975))

paramEstimates <- list(lambda1PointEst, lambda2PointEst, lambda3PointEst)


##creating datasets for plotting 
## Sample from mcmc chains for credible intervals
## Add binomial sampling uncertainty

## 1. Sample from mcmc chain to get 95% credible intervals (model uncertainty)
ager1=1:80
numSamples = 1000

for(ii in 1:length(paramVector)) {
  
  outDf <- matrix(NA,nrow=numSamples, ncol = length(ager1))

for (kk in 1:numSamples ) {
  randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  
  lambdaSample <- mcmcMatrix[randomNumber,"lambda"]
  deltaSample <- mcmcMatrix[randomNumber,"delta"]
  
  newRow <- (lambdaSample / (lambdaSample+deltaSample)) *(1 - exp(-ager1*(lambdaSample+deltaSample)))
  outDf[kk,] <- newRow
  }
}

view(OutDf)

# get quantile matrices 

quantileMatrix <- matrix(NA,nrow=ncol(outDf), ncol = 3)
for(jj in 1:ncol(outDf)){
  quantiles <- outDf[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix[jj,] <- quantiles
  df_upperLower <- cbind(ager1, quantileMatrix)
  df_upperLower <- as.data.frame(df_upperLower)
  colnames(df_upperLower) <- c('agemid', 'mean', 'upper', 'lower')
  
}


############################################################
## Plots
############################################################


ggplot()+
  geom_line(data = df_upperLower, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = df_chik, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = df_chik, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")
  theme_bw()+
  xlab("Age (years)") + ylab("Proportion Seropositive")


Foi1<-data.frame(transpose(foiEstimates_1))
Foi1$country <- c("C1")
Foi2<-data.frame(transpose(foiEstimates_2))
Foi2$country <- c("C2")
Foi3<-data.frame(transpose(foiEstimates_3))
Foi3$country <- c("C3")

# FOI estiamtes bars
ggplot()+
  geom_errorbar(data = Foi1, aes(x   = country,
                                 ymin= foiEstimates_1.1,
                                 ymax= foiEstimates_1.2),
                color = "#558C8C", width = 0.1)+
  geom_point(data = Foi1, aes(x=country, y=foiEstimates_1),color = "#558C8C")+
  geom_errorbar(data = Foi2, aes(x   = country,
                                 ymin= foiEstimates_2.1,
                                 ymax= foiEstimates_2.2),
                color = "#C05746", width = 0.1)+
  geom_point(data = Foi2, aes(x=country, y=foiEstimates_2),color = "#C05746")+
  geom_errorbar(data = Foi3, aes(x   = country,
                                 ymin= foiEstimates_3.1,
                                 ymax= foiEstimates_3.2),
                color = "#075E9D", width = 0.1) +
  geom_point(data = Foi3, aes(x=country, y=foiEstimates_3),color = "#075E9D")+
  xlab("Country") + ylab("Force of Infection") +
  theme_bw()



