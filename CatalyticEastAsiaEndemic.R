library(readxl)
require(tidyverse)
require(rjags)
library(binom)
library(ggplot2)
library(dplyr)
library(varhandle)
require(MCMCvis)
library(cowplot)
library(vioplot)
library(viridis)
library(hrbrthemes)
library(ggridges)

#Read file
CountryModel <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "Sheet1")
View(CountryModel)
#Rename data
df_asia = CountryModel 

df_asia[,c("midpoint","lower","upper")] = binom.confint(df_asia$N.pos, df_asia$N, method="exact")[,c("mean","lower","upper")]

df_asia <-  df_asia %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

thai      <- df_asia[df_asia$country == "Thailand",]
indonesia <- df_asia[df_asia$country == "Indonesia",]
myanmar   <- df_asia[df_asia$country == "Myanmar",]

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
	for (i in 9:12){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda3*age[i]) #catalytic model
	}
	#  prior dists
  lambda1 ~ dunif(0,1) #uninformative prior
  lambda2 ~ dunif(0,1) #uninformative prior
  lambda3 ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda1", "lambda2", "lambda3")
# Run model
mcmc.length=50000
jdat = list(n.pos= df_asia$N.pos,
            N=df_asia$N,
            age=df_asia$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVector, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

#age range
ager=1:80
numSamples = 1000

# credible interval
for(ii in 1:length(paramVector)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample_1 <- mcmcMatrix[randomNumber,"lambda1"]
    lambdaSample_2 <- mcmcMatrix[randomNumber,"lambda2"]
    lambdaSample_3 <- mcmcMatrix[randomNumber,"lambda3"]
    
    newRow_1 <-  1-exp(-lambdaSample_1*ager)
    newRow_2 <-  1-exp(-lambdaSample_2*ager)
    newRow_3 <-  1-exp(-lambdaSample_3*ager)
    
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    outDf_3[kk,] <- newRow_3
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

ggplot()+
  geom_line(data = df_upperLower_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = thai, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = thai, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Thailand (based on 2014 data)") +
  scale_fill_viridis(discrete = T) 
  
ggplot()+
  geom_line(data = df_upperLower_2, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_2, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = indonesia, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = indonesia, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Indonesia (2013-2016)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_3, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_3, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = myanmar, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = myanmar, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Myanmar (2013-2018)") +
  scale_fill_viridis(discrete = T) 

vioplot(mcmcMatrix)

mcmcFOI <- data.frame(mcmcMatrix)
myanFOI = data.frame(mcmcFOI[,3])
indoFOI = data.frame(mcmcFOI[,2])
thaiFOI = data.frame(mcmcFOI[,1])

colnames(myanFOI)[1] <- "lambda"
colnames(indoFOI)[1] <- "lambda"
colnames(thaiFOI)[1] <- "lambda"

thaiFOI$country <- c("Thailand")
indoFOI$country <- c("Indonesia")
myanFOI$country <- c("Myanmar")
FOIdata<- rbind(thaiFOI, indoFOI, myanFOI)

ggplot(FOIdata, aes(x= country, y = lambda, fill = country)) +  # Change filling color
  geom_boxplot()+
  theme_ipsum()+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)

FOIdata %>%
  mutate(text = fct_reorder(country, lambda)) %>% # Reorder data
  ggplot(aes(x=country, y=lambda, fill=country, color=country)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("Country") +
  ylab("FoI")
