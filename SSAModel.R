library(readxl)
require(tidyverse)
require(rjags)
require(binom)
library(ggplot2)
library(dplyr)
library(varhandle)
require(MCMCvis)
library(cowplot)

#Read file
CountryModel <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "India")
View(CountryModel)
#Rename data
df_chik = CountryModel 

df_chik[,c("midpoint","lower","upper")] = binom.confint(df_chik$N.pos, df_chik$N, method="exact")[,c("mean","lower","upper")]

df_chik <-  df_chik %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

#sub-setting datafiles by study
datS1 <- df_chik[df_chik$study == "1",]
datS2 <- df_chik[df_chik$study == "3",]
datS3 <- df_chik[df_chik$study == "4",]
datS4 <- df_chik[df_chik$study == "5",]

# Define model
jcode <- "model{ 
	for (i in 1:4){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda1*age[i]) #catalytic model
	}
	for (i in 5:8){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda2*age[i]) #catalytic model
	}
	for (i in 9:11){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda3*age[i]) #catalytic model
	}
	for (i in 12:18){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda4*age[i]) #catalytic model
	}
	#  prior dists
  lambda1 ~ dunif(0,1) #uninformative prior
  lambda2 ~ dunif(0,1) #uninformative prior
  lambda3 ~ dunif(0,1) #uninformative prior
  lambda4 ~ dunif(0,1) #uninformative prior
}"

#vector 
paramVector <- c("lambda1", "lambda2","lambda3", "lambda4")

# Run model
mcmc.length=50000
jdat = list(n.pos= df_chik$N.pos,
            N=df_chik$N,
            age=df_chik$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVector, n.iter=mcmc.length)
plot(jpos) # check convergence

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

#age range
ager1=1:50
ager2=1:80
ager3=6:66
ager4=1:70
numSamples = 1000

## for loop multiple age ranges

ages <- list(ager1, ager2, ager3, ager4)

for(i in 1: length(ages)) {
  assign(paste0("outDf_", i), matrix(NA, nrow=numSamples, ncol = length(ages[[i]])))
}

# outputting  

for(i in 1:numSamples) {
  
  # outputting  
  for(ii in 1:4) {
    
    lambdas <- list("lambda1", "lambda2", "lambda3", "lambda4")
    
    assign(paste0("lambdaSamples_", ii), mcmcMatrix[sample(nrow(mcmcMatrix), numSamples, replace=T), 
                                                    lambdas[[ii]]])
    
  }
  outDf_1 <- 1 - exp(-lambdaSamples_1 %*% t(ager1))
  outDf_2 <- 1 - exp(-lambdaSamples_2 %*% t(ager2))
  outDf_3 <- 1 - exp(-lambdaSamples_3 %*% t(ager3))
  outDf_4 <- 1 - exp(-lambdaSamples_3 %*% t(ager4))
  
}


# get quantile matrices 
  
quantileMatrix_1 <- matrix(NA,nrow=ncol(outDf_1), ncol = 3)
  for(jj in 1:ncol(outDf_1)){
    quantiles <- outDf_1[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrix_1[jj,] <- quantiles
    df_upperLower_1 <- cbind(ager1, quantileMatrix_1)
    df_upperLower_1 <- as.data.frame(df_upperLower_1)
    colnames(df_upperLower_1) <- c('agemid', 'mean', 'upper', 'lower')
    
  }

quantileMatrix_2 <- matrix(NA,nrow=ncol(outDf_2), ncol = 3)
for(jj in 1:ncol(outDf_2)){
  quantiles <- outDf_2[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_2[jj,] <- quantiles
  df_upperLower_2 <- cbind(ager2, quantileMatrix_2)
  df_upperLower_2 <- as.data.frame(df_upperLower_2)
  colnames(df_upperLower_2) <- c('agemid', 'mean', 'upper', 'lower')
  
}

quantileMatrix_3 <- matrix(NA,nrow=ncol(outDf_3), ncol = 3)
for(jj in 1:ncol(outDf_3)){
  quantiles <- outDf_3[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_3[jj,] <- quantiles
  df_upperLower_3 <- cbind(ager3, quantileMatrix_3)
  df_upperLower_3 <- as.data.frame(df_upperLower_3)
  colnames(df_upperLower_3) <- c('agemid', 'mean', 'upper', 'lower')
  
}

quantileMatrix_4 <- matrix(NA,nrow=ncol(outDf_4), ncol = 3)
for(jj in 1:ncol(outDf_4)){
  quantiles <- outDf_4[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_4[jj,] <- quantiles
  df_upperLower_4 <- cbind(ager4, quantileMatrix_4)
  df_upperLower_4 <- as.data.frame(df_upperLower_4)
  colnames(df_upperLower_4) <- c('agemid', 'mean', 'upper', 'lower')
}


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
  geom_line(data = df_upperLower_4, aes(x=agemid, y=mean),color = "#EF9A9A")+
  geom_ribbon(data = df_upperLower_4, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper),fill = "#EF9A9A")+
  geom_point(data = datS4, aes(x=agemid, y=midpoint),color = "#EF9A9A")+
  geom_linerange(data = datS4, aes(x=agemid, ymin=lower, ymax=upper),color = "#EF9A9A")+   ## data 4 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")

ggplot()+
geom_line(data = df_upperLower_3, aes(x=agemid, y=mean),color = "#075E9D")+
  geom_ribbon(data = df_upperLower_3, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper),fill = "#075E9D")+
  geom_point(data = datS3, aes(x=agemid, y=midpoint),color = "#075E9D")+
  geom_linerange(data = datS3, aes(x=agemid, ymin=lower, ymax=upper),color = "#075E9D")+   ## data 3 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")

ggplot()+
  geom_line(data = df_upperLower_2, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_2, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = datS2, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = datS2, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")

# source function
source("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/codes/CHIK/SamplingUncertaintyFunc.R")

ageVector <- datS1$agemid 
ageTotals <- datS1$N

SampUncertainMCMC <- mcmcRandomSamplerCat(1000, mcmcMatrix, ageVector, ageTotals)
ageQuantSamp      <- ageQuantiles(SampUncertainMCMC)

## Create a df with sample uncertainty
lambdaEst <- outDf_1 %>% quantile(probs=c(.5,.025,.975))

df_sampling = data.frame(
  midpoint = datS1$agemid,
  mean = 1 - exp(-datS1$agemid*(lambdaEst[1])),
  upper = ageQuantSamp[,3],
  lower = ageQuantSamp[,2]
)

# plot 
ggplot()+
  geom_line(data = df_upperLower_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = datS1, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = datS1, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  geom_ribbon(data = df_sampling, alpha=0.2, aes(x=midpoint, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")

# incidence rate
NewDf <- matrix(NA, nrow=numSamples, ncol=length(ager))
a <- c(1,6,11,16,21,26,31,36,41,46)
for(i in a) {
  NewDf[, i] <-1-((1-outDf_1[,i+4])/(1-outDf_1[,i])^(1/4))
  NewDf[, i+1] <- NewDf[, i]
  NewDf[, i+2] <- NewDf[, i]
  NewDf[, i+3] <- NewDf[, i]
  NewDf[, i+4] <- NewDf[, i]
}

samp <- matrix(1, nrow=numSamples, ncol=length(ager1))
sprop  <- samp - outDf_1

library(readxl)
WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/code/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx", 
                                                                 sheet = "India2021")
View(WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES)

pop<-WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES
pop <- pop[ -c(1) ]

Susceptible <- sprop * pop
NewCases    <- NewDf * Susceptible

quantileMatrixInc <- matrix(NA,nrow=ncol(outDf_1), ncol = 3)
for(jj in 1:ncol(outDf_1)){
  quantiles <- NewCases[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrixInc[jj,] <- quantiles
  df_upperLower_Inc <- cbind(ager1, quantileMatrixInc)
  df_upperLower_Inc <- as.data.frame(df_upperLower_Inc)
  colnames(df_upperLower_Inc) <- c('agemid', 'mean', 'upper', 'lower')
}

plot_ly(data=df_upperLower_Inc, x = ~agemid, y = ~mean, type = 'bar', name = 'Susceptible') %>%
  layout(yaxis = list(title = 'N of new cases'), barmode = 'stack')

# quantile matrix for susceptible pop
quantileMatrixSus <- matrix(NA,nrow=ncol(outDf_1), ncol = 3)
for(jj in 1:ncol(outDf_1)){
  quantiles <- Susceptible[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrixSus[jj,] <- quantiles
  df_upperLowerSus <- cbind(ager1, quantileMatrixSus)
  df_upperLowerSus <- as.data.frame(df_upperLowerSus)
  colnames(df_upperLowerSus) <- c('agemid', 'mean', 'upper', 'lower')
}

plot_ly(data=df_upperLowerSus, x = ~agemid, y = ~mean, type = 'bar', name = 'Susceptible') %>%
  layout(yaxis = list(title = 'N of Susceptible'), barmode = 'stack')


################################################################################
### Sub-Saharan Africa #########################################################
#Read file
CountryModel <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "Africa")
View(CountryModel)
#Rename data
df_africa = CountryModel 

df_africa[,c("midpoint","lower","upper")] = binom.confint(df_africa$N.pos, df_africa$N, method="exact")[,c("mean","lower","upper")]

df_africa <-  df_africa %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2) 
df_africa <- df_africa[-c(14),]
#sub-setting datafiles by study
datA1 <- df_africa[df_africa$study == "1",]
datA2 <- df_africa[df_africa$study == "2",]
datA3 <- df_africa[df_africa$study == "3",]
datA4 <- df_africa[df_africa$study == "4",]
datA5 <- df_africa[df_africa$study == "5",]
datA6 <- df_africa[df_africa$study == "6",]
datA7 <- df_africa[df_africa$study == "7",]
datA8 <- df_africa[df_africa$study == "8",]

# Define model
jcode <- "model{ 
	for (i in 1:4){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda1*age[i]) #catalytic model
	}
	for (i in 5:8){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda2*age[i]) #catalytic model
	}
	for (i in 9:13){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda3*age[i]) #catalytic model
	}
	for (i in 14:19){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda4*age[i]) #catalytic model
	}
	for (i in 20:24){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda5*age[i]) #catalytic model
	}
	for (i in 25:28){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda6*age[i]) #catalytic model
	}
	for (i in 29:32){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda7*age[i]) #catalytic model
	}
	for (i in 33:39){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda8*age[i]) #catalytic model
	}
	#  prior dists
  lambda1 ~ dunif(0,1) #uninformative prior
  lambda2 ~ dunif(0,1) #uninformative prior
  lambda3 ~ dunif(0,1) #uninformative prior
  lambda4 ~ dunif(0,1) #uninformative prior
  lambda5 ~ dunif(0,1) #uninformative prior
  lambda6 ~ dunif(0,1) #uninformative prior
  lambda7 ~ dunif(0,1) #uninformative prior
  lambda8 ~ dunif(0,1) #uninformative prior
}"


#vector 
paramVectorAfrica <- c("lambda1", "lambda2","lambda3", "lambda4",
                       "lambda5", "lambda6","lambda7", "lambda8")

# Run model
mcmc.length=50000
jdat = list(n.pos= df_africa$N.pos,
            N=df_africa$N,
            age=df_africa$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVectorAfrica, n.iter=mcmc.length)
plot(jpos) # check convergence

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

#age range
agerA1=1:80
numSamples = 1000

## for loop multiple age ranges

ages <- list(agerA1)

for(i in 1: length(ages)) {
  assign(paste0("afroDf_", i), matrix(NA, nrow=numSamples, ncol = length(ages[[i]])))
}

# outputting  

for(i in 1:numSamples) {
  
  # outputting  
  for(ii in 1:4) {
    
    lambdas <- list("lambda1", "lambda2", "lambda3", "lambda4")
    
    assign(paste0("lambdaSamples_", ii), mcmcMatrix[sample(nrow(mcmcMatrix), numSamples, replace=T), 
                                                    lambdas[[ii]]])
  }
  afroDf_1 <- 1 - exp(-lambdaSamples_1 %*% t(agerA1))
  afroDf_2 <- 1 - exp(-lambdaSamples_2 %*% t(agerA1))
  afroDf_3 <- 1 - exp(-lambdaSamples_3 %*% t(agerA1))
  afroDf_4 <- 1 - exp(-lambdaSamples_4 %*% t(agerA1))
  afroDf_5 <- 1 - exp(-lambdaSamples_4 %*% t(agerA1))
  afroDf_6 <- 1 - exp(-lambdaSamples_4 %*% t(agerA1))
  afroDf_7 <- 1 - exp(-lambdaSamples_4 %*% t(agerA1))
  afroDf_8 <- 1 - exp(-lambdaSamples_4 %*% t(agerA1))
}


# get quantile matrices 
quantileMatrix_A1 <- matrix(NA,nrow=ncol(afroDf_1), ncol = 3)
for(jj in 1:ncol(afroDf_1)){
  quantiles <- afroDf_1[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_A1[jj,] <- quantiles
  df_upperLower_A1 <- cbind(agerA1, quantileMatrix_A1)
  df_upperLower_A1 <- as.data.frame(df_upperLower_A1)
  colnames(df_upperLower_A1) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_A2 <- matrix(NA,nrow=ncol(afroDf_2), ncol = 3)
for(jj in 1:ncol(afroDf_2)){
  quantiles <- afroDf_2[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_A2[jj,] <- quantiles
  df_upperLower_A2 <- cbind(agerA1, quantileMatrix_A2)
  df_upperLower_A2 <- as.data.frame(df_upperLower_A2)
  colnames(df_upperLower_A2) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_A3 <- matrix(NA,nrow=ncol(afroDf_3), ncol = 3)
for(jj in 1:ncol(afroDf_3)){
  quantiles <- afroDf_3[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_A3[jj,] <- quantiles
  df_upperLower_A3 <- cbind(agerA1, quantileMatrix_A3)
  df_upperLower_A3 <- as.data.frame(df_upperLower_A3)
  colnames(df_upperLower_A3) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_A4 <- matrix(NA,nrow=ncol(afroDf_4), ncol = 3)
for(jj in 1:ncol(afroDf_4)){
  quantiles <- afroDf_4[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_A4[jj,] <- quantiles
  df_upperLower_A4 <- cbind(agerA1, quantileMatrix_A4)
  df_upperLower_A4 <- as.data.frame(df_upperLower_A4)
  colnames(df_upperLower_A4) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_A5 <- matrix(NA,nrow=ncol(afroDf_5), ncol = 3)
for(jj in 1:ncol(afroDf_5)){
  quantiles <- afroDf_5[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_A5[jj,] <- quantiles
  df_upperLower_A5 <- cbind(agerA1, quantileMatrix_A5)
  df_upperLower_A5 <- as.data.frame(df_upperLower_A5)
  colnames(df_upperLower_A5) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_A6 <- matrix(NA,nrow=ncol(afroDf_6), ncol = 3)
for(jj in 1:ncol(afroDf_6)){
  quantiles <- afroDf_6[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_A6[jj,] <- quantiles
  df_upperLower_A6 <- cbind(agerA1, quantileMatrix_A6)
  df_upperLower_A6 <- as.data.frame(df_upperLower_A6)
  colnames(df_upperLower_A6) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_A7 <- matrix(NA,nrow=ncol(afroDf_7), ncol = 3)
for(jj in 1:ncol(afroDf_7)){
  quantiles <- afroDf_7[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_A7[jj,] <- quantiles
  df_upperLower_A7 <- cbind(agerA1, quantileMatrix_A7)
  df_upperLower_A7 <- as.data.frame(df_upperLower_A7)
  colnames(df_upperLower_A7) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_A8 <- matrix(NA,nrow=ncol(afroDf_8), ncol = 3)
for(jj in 1:ncol(afroDf_8)){
  quantiles <- afroDf_8[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_A8[jj,] <- quantiles
  df_upperLower_A8 <- cbind(agerA1, quantileMatrix_A8)
  df_upperLower_A8 <- as.data.frame(df_upperLower_A8)
  colnames(df_upperLower_A8) <- c('agemid', 'mean', 'upper', 'lower')
}

# source function
source("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/codes/CHIK/SamplingUncertaintyFunc.R")

ageVector1 <- datA1$agemid 
ageTotals1 <- datA1$N
ageVector2 <- datA2$agemid 
ageTotals2 <- datA2$N
ageVector3 <- datA3$agemid 
ageTotals3 <- datA3$N


SampUncertainMCMC_1 <- mcmcRandomSamplerCat(1000, mcmcMatrix, ageVector1, ageTotals1)
ageQuantSamp_1      <- ageQuantiles(SampUncertainMCMC_1)

SampUncertainMCMC_3 <- mcmcRandomSamplerCat(1000, mcmcMatrix, ageVector3, ageTotals3)
ageQuantSamp_3      <- ageQuantiles(SampUncertainMCMC_3)

## Create a df with sample uncertainty
lambdaEst <- afroDf_1 %>% quantile(probs=c(.5,.025,.975))

df_sampling_A1 = data.frame(
  midpoint = datA1$agemid,
  mean = 1 - exp(-datA1$agemid*(lambdaEst[1])),
  upper = ageQuantSamp_1[,3],
  lower = ageQuantSamp_1[,2]
)

lambdaEst3 <- afroDf_3 %>% quantile(probs=c(.5,.025,.975))

df_sampling_A3 = data.frame(
  midpoint = datA3$agemid,
  mean = 1 - exp(-datA3$agemid*(lambdaEst3[1])),
  upper = ageQuantSamp_3[,3],
  lower = ageQuantSamp_3[,2]
)

#country1
ggplot()+
  geom_line(data = df_upperLower_A1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_A1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = datA1, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = datA1, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  geom_ribbon(data = df_sampling_A1, alpha=0.2, aes(x=midpoint, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seroprevalence in Benin (A,Bacci et al)")+
  theme(plot.title = element_text(color="black", size=10))
#country2
ggplot()+
  geom_line(data = df_upperLower_A2, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_A2, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = datA2, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = datA2, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seroprevalence in DRC (A, De Weggheleire et al)")+
  theme(plot.title = element_text(color="black", size=10))
#country3
ggplot()+
  geom_line(data = df_upperLower_A3, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_A3, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = datA3, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = datA3, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seroprevalence in La Reunion (A, Fred et al)")+
  theme(plot.title = element_text(color="black", size=10))
#country4
ggplot()+
  geom_line(data = df_upperLower_A4, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_A4, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = datA4, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = datA4, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seroprevalence in Comoros (K, Sergon et al)")+
  theme(plot.title = element_text(color="black", size=10))
#country5
ggplot()+
  geom_line(data = df_upperLower_A5, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_A5, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = datA5, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = datA5, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seroprevalence in Senegal (A, Sow et al)")+
  theme(plot.title = element_text(color="black", size=10))
#country6
ggplot()+
  geom_line(data = df_upperLower_A6, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_A6, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = datA6, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = datA6, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seroprevalence in Mozambique (Antonio, VS et al)")+
  theme(plot.title = element_text(color="black", size=10))
#country7-8 (kenya)
ggplot()+
  geom_line(data = df_upperLower_A8, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_A8, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = datA8, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = datA8, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seroprevalence in Kenya")+
  theme(plot.title = element_text(color="black", size=10))


