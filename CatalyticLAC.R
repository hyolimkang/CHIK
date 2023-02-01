library(readxl)
require(tidyverse)
require(rjags)
library(binom)
library(ggplot2)
library(dplyr)
library(varhandle)
require(MCMCvis)
library(cowplot)
library(viridis)
library(hrbrthemes)
library(ggridges)
library(plotly)

#Read file
CountryModel <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "inclusion")
View(CountryModel)
#Rename data
df_lac = CountryModel %>% filter(region == "LAC")

df_lac[,c("midpoint","lower","upper")] = binom.confint(df_lac$N.pos, df_lac$N, method="exact")[,c("mean","lower","upper")]

df_lac <-  df_lac %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

study52  <-  df_lac %>% filter(study_no == 52)
study66  <-  df_lac %>% filter(study_no == 66)
study110 <- df_lac %>% filter(study_no == 110)
study124 <- df_lac %>% filter(study_no == 124)
study153 <- df_lac %>% filter(study_no == 153)
study165 <- df_lac %>% filter(study_no == 165)
study169 <- df_lac %>% filter(study_no == 169)
study174 <- df_lac %>% filter(study_no == 174)
study179 <- df_lac %>% filter(study_no == 179)
study229 <- df_lac %>% filter(study_no == 229)
study229 <- df_lac %>% filter(study_no == 229)
study243 <- df_lac %>% filter(study_no == 243)
study251 <- df_lac %>% filter(study_no == 251)
study260 <- df_lac %>% filter(study_no == 260)
study299 <- df_lac %>% filter(study_no == 299)

# samping uncertainty
source("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/codes/CHIK/SamplingUncertaintyFunc.R")

# Define model
jcode <- "model{ 
	for (i in 1:4){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda1*age[i]) #catalytic model
	}
	for (i in 5:11){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda2*age[i]) #catalytic model
	}
	for (i in 12:16){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda3*age[i]) #catalytic model
	}
	for (i in 17:20){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda4*age[i]) #catalytic model
	}
	for (i in 41:44){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda5*age[i]) #catalytic model
	}
	for (i in 45:48){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda6*age[i]) #catalytic model
	}
	for (i in 49:55){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda7*age[i]) #catalytic model
	}
	for (i in 56:61){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda8*age[i]) #catalytic model
	}
	for (i in 62:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda9*age[i]) #catalytic model
	}
	for (i in 66:68){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda10*age[i]) #catalytic model
	}
	for (i in 69:73){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda11*age[i]) #catalytic model
	}
	for (i in 74:78){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda12*age[i]) #catalytic model
	}
	for (i in 79:81){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda13*age[i]) #catalytic model
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
  lambda9 ~ dunif(0,1) #uninformative prior
  lambda10 ~ dunif(0,1) #uninformative prior
  lambda11 ~ dunif(0,1) #uninformative prior
  lambda12 ~ dunif(0,1) #uninformative prior
  lambda13 ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda1", "lambda2","lambda3","lambda4",
                 "lambda5", "lambda6", "lambda7", "lambda8",
                 "lambda9", "lambda10", "lambda11", "lambda12",
                 "lambda13")
# Run model
mcmc.length=50000
jdat = list(n.pos= df_lac$N.pos,
            N=df_lac$N,
            age=df_lac$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVector, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

# Calculate DIC
dic.samples(jmod, n.iter = mcmc.length)

#age range
ager=1:80
numSamples = 1000

# credible interval
for(ii in 1:length(paramVector)) {
  
  for(i in 1:13) {
    assign(paste0("outDf_", i), matrix(NA, nrow=numSamples, ncol = length(ager)))
  }
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))

    lambdas <- list("lambda1", "lambda2", "lambda3",
                    "lambda4", "lambda5", "lambda6",
                    "lambda7", "lambda8", "lambda9",
                    "lambda10", "lambda11", "lambda12",
                    "lambda13")
    
    for(ii in 1:length(lambdas)) {
      assign(paste0("lambdaSample_", ii), mcmcMatrix[sample(nrow(mcmcMatrix), 1, replace=T), 
                                                     lambdas[[ii]]])
    }
    
    newRow_1 <-  1-exp(-lambdaSample_1*ager)
    newRow_2 <-  1-exp(-lambdaSample_2*ager)
    newRow_3 <-  1-exp(-lambdaSample_3*ager)
    newRow_4 <-  1-exp(-lambdaSample_4*ager)
    newRow_5 <-  1-exp(-lambdaSample_5*ager)
    newRow_6 <-  1-exp(-lambdaSample_6*ager)
    newRow_7 <-  1-exp(-lambdaSample_7*ager)
    newRow_8 <-  1-exp(-lambdaSample_8*ager)
    newRow_9 <-  1-exp(-lambdaSample_9*ager)
    newRow_10 <-  1-exp(-lambdaSample_10*ager)
    newRow_11 <-  1-exp(-lambdaSample_11*ager)
    newRow_12 <-  1-exp(-lambdaSample_12*ager)
    newRow_13 <-  1-exp(-lambdaSample_13*ager)
    
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    outDf_3[kk,] <- newRow_3
    outDf_4[kk,] <- newRow_4
    outDf_5[kk,] <- newRow_5
    outDf_6[kk,] <- newRow_6
    outDf_7[kk,] <- newRow_7
    outDf_8[kk,] <- newRow_8
    outDf_9[kk,] <- newRow_9
    outDf_10[kk,] <- newRow_10
    outDf_11[kk,] <- newRow_11
    outDf_12[kk,] <- newRow_12
    outDf_13[kk,] <- newRow_13
  }
}

# get quantile matrices 
quantileMatrix_1 <- matrix(NA,nrow=ncol(outDf_1), ncol = 3)
for(jj in 1:ncol(outDf_1)){
  quantiles <- outDf_1[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_1[jj,] <- quantiles
  df_upperLower_1 <- cbind(ager, quantileMatrix_1)
  df_upperLower_1 <- as.data.frame(df_upperLower_1)
  colnames(df_upperLower_1) <- c('agemid', 'mean', 'upper', 'lower')
  
}

quantileMatrix_2 <- matrix(NA,nrow=ncol(outDf_2), ncol = 3)
for(jj in 1:ncol(outDf_2)){
  quantiles <- outDf_2[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_2[jj,] <- quantiles
  df_upperLower_2 <- cbind(ager, quantileMatrix_2)
  df_upperLower_2 <- as.data.frame(df_upperLower_2)
  colnames(df_upperLower_2) <- c('agemid', 'mean', 'upper', 'lower')
  
}

quantileMatrix_3 <- matrix(NA,nrow=ncol(outDf_3), ncol = 3)
for(jj in 1:ncol(outDf_3)){
  quantiles <- outDf_3[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_3[jj,] <- quantiles
  df_upperLower_3 <- cbind(ager, quantileMatrix_3)
  df_upperLower_3 <- as.data.frame(df_upperLower_3)
  colnames(df_upperLower_3) <- c('agemid', 'mean', 'upper', 'lower')
  
}

quantileMatrix_4 <- matrix(NA,nrow=ncol(outDf_4), ncol = 3)
for(jj in 1:ncol(outDf_4)){
  quantiles <- outDf_4[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_4[jj,] <- quantiles
  df_upperLower_4 <- cbind(ager, quantileMatrix_4)
  df_upperLower_4 <- as.data.frame(df_upperLower_4)
  colnames(df_upperLower_4) <- c('agemid', 'mean', 'upper', 'lower')
  
}

quantileMatrix_5 <- matrix(NA,nrow=ncol(outDf_5), ncol = 3)
for(jj in 1:ncol(outDf_5)){
  quantiles <- outDf_5[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_5[jj,] <- quantiles
  df_upperLower_5 <- cbind(ager, quantileMatrix_5)
  df_upperLower_5 <- as.data.frame(df_upperLower_5)
  colnames(df_upperLower_5) <- c('agemid', 'mean', 'upper', 'lower')
  
}

quantileMatrix_6 <- matrix(NA,nrow=ncol(outDf_6), ncol = 3)
for(jj in 1:ncol(outDf_6)){
  quantiles <- outDf_6[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_6[jj,] <- quantiles
  df_upperLower_6 <- cbind(ager, quantileMatrix_6)
  df_upperLower_6 <- as.data.frame(df_upperLower_6)
  colnames(df_upperLower_6) <- c('agemid', 'mean', 'upper', 'lower')
  
}

quantileMatrix_7 <- matrix(NA,nrow=ncol(outDf_7), ncol = 3)
for(jj in 1:ncol(outDf_7)){
  quantiles <- outDf_7[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_7[jj,] <- quantiles
  df_upperLower_7 <- cbind(ager, quantileMatrix_7)
  df_upperLower_7 <- as.data.frame(df_upperLower_7)
  colnames(df_upperLower_7) <- c('agemid', 'mean', 'upper', 'lower')
  
}

quantileMatrix_8 <- matrix(NA,nrow=ncol(outDf_8), ncol = 3)
for(jj in 1:ncol(outDf_8)){
  quantiles <- outDf_8[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_8[jj,] <- quantiles
  df_upperLower_8 <- cbind(ager, quantileMatrix_8)
  df_upperLower_8 <- as.data.frame(df_upperLower_8)
  colnames(df_upperLower_8) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_9 <- matrix(NA,nrow=ncol(outDf_9), ncol = 3)
for(jj in 1:ncol(outDf_9)){
  quantiles <- outDf_9[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_9[jj,] <- quantiles
  df_upperLower_9 <- cbind(ager, quantileMatrix_9)
  df_upperLower_9 <- as.data.frame(df_upperLower_9)
  colnames(df_upperLower_9) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_10 <- matrix(NA,nrow=ncol(outDf_10), ncol = 3)
for(jj in 1:ncol(outDf_10)){
  quantiles <- outDf_10[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_10[jj,] <- quantiles
  df_upperLower_10 <- cbind(ager, quantileMatrix_10)
  df_upperLower_10 <- as.data.frame(df_upperLower_10)
  colnames(df_upperLower_10) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_11 <- matrix(NA,nrow=ncol(outDf_11), ncol = 3)
for(jj in 1:ncol(outDf_11)){
  quantiles <- outDf_11[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_11[jj,] <- quantiles
  df_upperLower_11 <- cbind(ager, quantileMatrix_11)
  df_upperLower_11 <- as.data.frame(df_upperLower_11)
  colnames(df_upperLower_11) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_12 <- matrix(NA,nrow=ncol(outDf_12), ncol = 3)
for(jj in 1:ncol(outDf_12)){
  quantiles <- outDf_12[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_12[jj,] <- quantiles
  df_upperLower_12 <- cbind(ager, quantileMatrix_12)
  df_upperLower_12 <- as.data.frame(df_upperLower_12)
  colnames(df_upperLower_12) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_13 <- matrix(NA,nrow=ncol(outDf_13), ncol = 3)
for(jj in 1:ncol(outDf_13)){
  quantiles <- outDf_13[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_13[jj,] <- quantiles
  df_upperLower_13 <- cbind(ager, quantileMatrix_13)
  df_upperLower_13 <- as.data.frame(df_upperLower_13)
  colnames(df_upperLower_13) <- c('agemid', 'mean', 'upper', 'lower')
}

ggplot()+
  geom_line(data = df_upperLower_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study52, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study52, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Brazil (Perisse et al)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_2, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_2, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study66, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study66, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Ecuador (Stewart-Ibarra et al)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_3, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_3, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study110, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study110, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Martinique (Curlier, E)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_4, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_4, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study124, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study124, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Brazil (Braga et al)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_5, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_5, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study165, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study165, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Brazil (Barreto et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_6, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_6, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study169, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study169, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Suriname (FT, van et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_7, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_7, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study174, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study174, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Nicaragua (G, Kuan et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_8, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_8, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study179, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study179, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Puerto Rico (G.,Simmons et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_9, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_9, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study229, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study229, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Brazil (Juarez et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_10, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_10, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study243, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study243, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Brazil (Kanunfre et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_11, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_11, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study251, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study251, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Mexico (Eligio et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_12, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_12, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study260, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study260, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Puerto Rico (Adams et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_13, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_13, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study299, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study299, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Virgin Island (Hennessey et al.)") +
  scale_fill_viridis(discrete = T) 

mcmcMatrix <- data.frame(mcmcMatrix)
mcmc <- stack(mcmcMatrix)
mcmc$country <- c(NA)
mcmc$country[mcmc$ind == "lambda1"] <- "Brazil"
mcmc$country[mcmc$ind == "lambda2"] <- "Ecuador"
mcmc$country[mcmc$ind == "lambda3"] <- "Martinique"
mcmc$country[mcmc$ind == "lambda4"] <- "Brazil"
mcmc$country[mcmc$ind == "lambda5"] <- "Brazil"
mcmc$country[mcmc$ind == "lambda6"] <- "Suriname"
mcmc$country[mcmc$ind == "lambda7"] <- "Nicaragua"
mcmc$country[mcmc$ind == "lambda8"] <- "Puerto Rico"
mcmc$country[mcmc$ind == "lambda9"] <- "Brazil"
mcmc$country[mcmc$ind == "lambda10"] <- "Brazil"
mcmc$country[mcmc$ind == "lambda11"] <- "Mexico"
mcmc$country[mcmc$ind == "lambda12"] <- "Puerto Rico"
mcmc$country[mcmc$ind == "lambda13"] <- "Virgin Island"

mcmc %>%
  mutate(text = fct_reorder(country, values)) %>% # Reorder data
  ggplot(aes(x=country, y=values, fill=country, color=country)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("Country") +
  ylab("FoI")

## Epidemic model for study 153
jcode <- "model{ 
	for (i in 21:40){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2014-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,100)       #uninformative prior
  delta  ~ dunif(2010,2014) #uninformative prior
}"
paramVectorEpi1 <- c("lambda", "delta")

# Run model
mcmc.length=50000
jdat = list(n.pos= df_lac$N.pos,
            N=df_lac$N,
            age=df_lac$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVectorEpi1, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)
View(mcmcMatrix)

# Calculate DIC
dic.samples(jmod, n.iter = mcmc.length)

# age range for epimodel 1 
ager=0:99
ager1=0:1
ager2=2:99

# generate ramdomNmber for epidemic model
for(ii in 1:length(paramVectorEpi1)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
  
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample <- mcmcMatrix[randomNumber,"lambda"]
    deltaSample  <- mcmcMatrix[randomNumber,"delta"]

    newRow_1 <- 0
    newRow_2 <- 1-exp(-lambdaSample)
    
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    
    outDf <- cbind(outDf_1,outDf_2)
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

#serograph
ggplot()+
  geom_line(data = df_upperLower_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study153, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study153, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositive in Haiti (EW, Rogier et al)")+
  theme(plot.title = element_text(color="black", size=10))

######### Epidemic Model ################
## study 124
##simple model 
jcode <- "model{ 
	for (i in 17:20){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] <- 1-exp(-lambda*age[i])
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
}"
paramVectorSimple <- c("lambda")

## 1 epidemic model 
jcode <- "model{ 
	for (i in 17:20){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2019-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2005,2019) #uninformative prior
}"
paramVectorEpi1 <- c("lambda", "delta")

# epimodel 2 
jcode <- "model{ 
	for (i in 17:20){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta2), 1-exp(-2*lambda)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 14, 1-exp(-lambda),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2006,2019)
  delta2  ~ dunif(1990,2004)
}"
paramVectorEpi2 <- c("lambda", "delta1", "delta2")

# epimodel 3 
jcode <- "model{ 
	for (i in 17:20){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta3), 1-exp(-3*lambda),
        ifelse(age[i]> (2019-delta2) && age[i] < 30, 1-exp(-2*lambda),
        ifelse(age[i]> (2019-delta1) && age[i] < 14, 1-exp(-lambda),
        0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2006,2019)
  delta2  ~ dunif(1990,2004)
  delta3  ~ dunif(1960,1988)
}"
paramVectorEpi3 <- c("lambda","delta1", "delta2", "delta3")

# Run model
mcmc.length=50000
jdat = list(n.pos= df_lac$N.pos,
            N=df_lac$N,
            age=df_lac$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVectorEpi2, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

# Calculate DIC
dic.samples(jmod, n.iter = mcmc.length)


# age range for epimodel 1 
ager=1:80
ager1=1:4
ager2=5:80

# age range for epimodel2
ager=0:80
ager1=0:3
ager2=4:18
ager3=19:80

# age range for epimodel3
ager=0:80
ager1=0:3
ager2=4:18
ager3=19:38
ager4=39:80

# generate ramdomNumber for epidemic model (epi1)
for(ii in 1:length(paramVectorEpi1)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
  
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample <- mcmcMatrix[randomNumber,"lambda"]
    deltaSample  <- mcmcMatrix[randomNumber,"delta"]
    
    newRow_1 <- 0
    newRow_2 <- 1-exp(-lambdaSample)
    
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    
    outDf <- cbind(outDf_1,outDf_2)
  }
}

# generate ramdomNmber for epidemic model (epi2)
for(ii in 1:length(paramVectorEpi2)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
  outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager3))
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample   <- mcmcMatrix[randomNumber,"lambda"]
    deltaSample_1  <- mcmcMatrix[randomNumber,"delta1"]
    deltaSample_2  <- mcmcMatrix[randomNumber,"delta2"]
    
    newRow_1 <- 0
    newRow_2 <- 1-exp(-lambdaSample)
    newRow_3 <- 1-exp(-lambdaSample*2)
    
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    outDf_3[kk,] <- newRow_3
    
    outDf <- cbind(outDf_1,outDf_2,outDf_3)
  }
}

# generate ramdomNmber for epidemic model (epi3)
for(ii in 1:length(paramVectorEpi3)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
  outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager3))
  outDf_4 <- matrix(NA,nrow=numSamples, ncol = length(ager4))
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample    <- mcmcMatrix[randomNumber,"lambda"]
    deltaSample_1   <- mcmcMatrix[randomNumber,"delta1"]
    deltaSample_2   <- mcmcMatrix[randomNumber,"delta2"]
    deltaSample_3   <- mcmcMatrix[randomNumber,"delta3"]
    
    newRow_1 <- 0
    newRow_2 <- 1-exp(-lambdaSample)
    newRow_3 <- 1-exp(-(2*lambdaSample))
    newRow_4 <- 1-exp(-(3*lambdaSample))
    
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    outDf_3[kk,] <- newRow_3
    outDf_4[kk,] <- newRow_4
    
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

ggplot()+
  geom_line(data = df_upperLower_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study124, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study124, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("3 Epidemic: Seropositive in Brazil (Braga et al)")+
  theme(plot.title = element_text(color="black", size=10))

###study 260 Epidemic models 
##simple model 
jcode <- "model{ 
	for (i in 74:78){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] <- 1-exp(-lambda*age[i])
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
}"
paramVectorSimple <- c("lambda")

## 1 epidemic model 
jcode <- "model{ 
	for (i in 74:78){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2019-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2010,2018) #uninformative prior
}"
paramVectorEpi1 <- c("lambda", "delta")

# epimodel 2 
jcode <- "model{ 
	for (i in 74:78){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta2), 1-exp(-2*lambda)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 10, 1-exp(-lambda),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2010,2018)
  delta2  ~ dunif(2000,2008)
}"
paramVectorEpi2 <- c("lambda", "delta1", "delta2")

# epimodel 3 
jcode <- "model{ 
	for (i in 74:78){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta3), 1-exp(-3*lambda),
        ifelse(age[i]> (2019-delta2) && age[i] < 20, 1-exp(-2*lambda),
        ifelse(age[i]> (2019-delta1) && age[i] < 10, 1-exp(-lambda),
        0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2010,2018)
  delta2  ~ dunif(2000,2008)
  delta3  ~ dunif(1990,1998)
}"
paramVectorEpi3 <- c("lambda","delta1", "delta2", "delta3")

# Run model
mcmc.length=50000
jdat = list(n.pos= df_lac$N.pos,
            N=df_lac$N,
            age=df_lac$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVectorEpi3, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

# Calculate DIC
dic.samples(jmod, n.iter = mcmc.length)

# age range for epimodel 1 
ager=0:80
ager1=0:3
ager2=4:80

# age range for epimodel2
ager=0:80
ager1=0:3
ager2=4:13
ager3=14:80

# age range for epimodel3
ager=0:80
ager1=0:3
ager2=4:13
ager3=14:23
ager4=24:80

# generate ramdomNumber for epidemic model (epi1)
for(ii in 1:length(paramVectorEpi1)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
  
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample <- mcmcMatrix[randomNumber,"lambda"]
    deltaSample  <- mcmcMatrix[randomNumber,"delta"]
    
    newRow_1 <- 0
    newRow_2 <- 1-exp(-lambdaSample)
    
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    
    outDf <- cbind(outDf_1,outDf_2)
  }
}

# generate ramdomNmber for epidemic model (epi2)
for(ii in 1:length(paramVectorEpi2)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
  outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager3))
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample   <- mcmcMatrix[randomNumber,"lambda"]
    deltaSample_1  <- mcmcMatrix[randomNumber,"delta1"]
    deltaSample_2  <- mcmcMatrix[randomNumber,"delta2"]
    
    newRow_1 <- 0
    newRow_2 <- 1-exp(-lambdaSample)
    newRow_3 <- 1-exp(-lambdaSample*2)
    
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    outDf_3[kk,] <- newRow_3
    
    outDf <- cbind(outDf_1,outDf_2,outDf_3)
  }
}

# generate ramdomNmber for epidemic model (epi3)
for(ii in 1:length(paramVectorEpi3)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
  outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager3))
  outDf_4 <- matrix(NA,nrow=numSamples, ncol = length(ager4))
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample    <- mcmcMatrix[randomNumber,"lambda"]
    deltaSample_1   <- mcmcMatrix[randomNumber,"delta1"]
    deltaSample_2   <- mcmcMatrix[randomNumber,"delta2"]
    deltaSample_3   <- mcmcMatrix[randomNumber,"delta3"]
    
    newRow_1 <- 0
    newRow_2 <- 1-exp(-lambdaSample)
    newRow_3 <- 1-exp(-(2*lambdaSample))
    newRow_4 <- 1-exp(-(3*lambdaSample))
    
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    outDf_3[kk,] <- newRow_3
    outDf_4[kk,] <- newRow_4
    
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

ggplot()+
  geom_line(data = df_upperLower_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study260, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study260, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("3 Epidemic: Seropositive in Puerto Rico (Adams et al)")+
  theme(plot.title = element_text(color="black", size=10))


