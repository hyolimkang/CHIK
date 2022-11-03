library(readxl)
require(tidyverse)
require(rjags)
require(binom)
library(ggplot2)
library(dplyr)
library(varhandle)
require(MCMCvis)
library(cowplot)

##Read file
chik_systematic_review_v1 <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "prac")
#Rename data
df_chik = chik_systematic_review_v1 

df_chik[,c("midpoint","lower","upper")] = binom.confint(df_chik$N.pos, df_chik$N, method="exact")[,c("mean","lower","upper")]

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

ageComb <- c(ager1, ager2, ager3)
ageComb[1: length(ager1)]
ageComb[30: length(ager2)]



numSamples = 1000
outDf_1 <- matrix(NA, nrow=numSamples, ncol= length(ager1))
outDf_2 <- matrix(NA, nrow=numSamples, ncol= length(ager2))
outDf_3 <- matrix(NA, nrow=numSamples, ncol= length(ager3))

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

meanLambda1 <- 1-exp(-lambda_1*ager1)
meanLambda2 <- 1-exp(-lambda_2*ager2)
meanLambda3 <- 1-exp(-lambda_3*ager3)

mean <- c(meanLambda1, meanLambda2, meanLambda3)

#create multiple dataframes 
numbs <- data.frame(c(1:3))

## for loop for multiple age ranges?? 

#for (i in 1:length(df_chik$study)) {
#  assign(paste0("outDf_", numbs[i,]), matrix(NA, nrow=numSamples, ncol = length(ager1)))
# }


assign(paste0("outDf_", 1), matrix(NA, nrow=numSamples, ncol = length(ager1)))
assign(paste0("outDf_", 2), matrix(NA, nrow=numSamples, ncol = length(ager2)))
assign(paste0("outDf_", 3), matrix(NA, nrow=numSamples, ncol = length(ager3)))


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

quantileMatrix_1 <- matrix(NA,nrow=ncol(outDf_1), ncol = 3)
for(jj in 1:ncol(outDf_1)){
  quantiles <- outDf_1[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_1[jj,] <- quantiles
}

quantileMatrix_2 <- matrix(NA,nrow=ncol(outDf_2), ncol = 3)
for(jj in 1:ncol(outDf_2)){
  quantiles <- outDf_2[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_2[jj,] <- quantiles
}

quantileMatrix_3 <- matrix(NA,nrow=ncol(outDf_3), ncol = 3)
for(jj in 1:ncol(outDf_3)){
  quantiles <- outDf_3[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_3[jj,] <- quantiles
}

# Create a dataframe for plotting 
## how can i loop over df_upperLower_1~N ?
df_upperLower_1 = data.frame(
  agemid = ager1,
  mean = meanLambda1,
  upper = quantileMatrix_1[,3],
  lower = quantileMatrix_1[,2]
)

df_upperLower_2 = data.frame(
  agemid = ager2,
  mean = meanLambda2,
  upper = quantileMatrix_2[,3],
  lower = quantileMatrix_2[,2]) 

df_upperLower_3 = data.frame(
  agemid = ager3,
  mean = meanLambda3,
  upper = quantileMatrix_3[,3],
  lower = quantileMatrix_3[,2]) 

# for loop for dataframes 

for(i in 1:3) {
  assign(paste0("df_upperLower_" , numbs[i,]), "NA")
}

for(i in 1:3) {
  assign(paste0("df_upperLower_" , numbs[i,]), data.frame(
    midpoint = ageComb[i],
    mean = meanLambda1,
    upper = quantileMatrix_1[,3],
    lower = quantileMatrix_1[,2]
  ))
}

############################################################
## Plots
############################################################


ggplot()+
  geom_line(data = df_upperLower_2, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_2, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = datS2, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = datS2, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  geom_line(data = df_upperLower_1, aes(x=agemid, y=mean),color = "#C05746")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper),fill = "#C05746")+
  geom_point(data = datS1, aes(x=agemid, y=midpoint),color = "#C05746")+
  geom_linerange(data = datS1, aes(x=agemid, ymin=lower, ymax=upper),color = "#C05746")+  ## data 1
  geom_line(data = df_upperLower_3, aes(x=agemid, y=mean),color = "#075E9D")+
  geom_ribbon(data = df_upperLower_3, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper),fill = "#075E9D")+
  geom_point(data = datS3, aes(x=agemid, y=midpoint),color = "#075E9D")+
  geom_linerange(data = datS3, aes(x=agemid, ymin=lower, ymax=upper),color = "#075E9D")+   ## data 3 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")




