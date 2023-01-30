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
df_asia = CountryModel %>% filter(region == "EAP")

df_asia[,c("midpoint","lower","upper")] = binom.confint(df_asia$N.pos, df_asia$N, method="exact")[,c("mean","lower","upper")]

df_asia <-  df_asia %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

study70 <- df_asia %>% filter(study_no == 70)
study75 <- df_asia %>% filter(study_no == 75)
study81 <- df_asia %>% filter(study_no == 81)
study146 <- df_asia %>% filter(study_no == 146)
study162 <- df_asia %>% filter(study_no == 162)
study234 <- df_asia %>% filter(study_no == 234)
study277 <- df_asia %>% filter(study_no == 277)
study281 <- df_asia %>% filter(study_no == 281)
study423 <- df_asia %>% filter(study_no == 423)


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
	for (i in 13:16){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda4*age[i]) #catalytic model
	}
	for (i in 17:24){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda5*age[i]) #catalytic model
	}
	for (i in 25:29){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda6*age[i]) #catalytic model
	}
	for (i in 30:35){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda7*age[i]) #catalytic model
	}
	for (i in 36:38){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda8*age[i]) #catalytic model
	}
	for (i in 40:42){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda9*age[i]) #catalytic model
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
}"
paramVector <- c("lambda1", "lambda2", "lambda3", "lambda4", "lambda5",
                 "lambda6", "lambda7","lambda8","lambda9")
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
  
  for(i in 1: 9) {
    assign(paste0("outDf_", i), matrix(NA, nrow=numSamples, ncol = length(ager)))
  }
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    #lambdaSample_1 <- mcmcMatrix[randomNumber,"lambda1"]
    #lambdaSample_2 <- mcmcMatrix[randomNumber,"lambda2"]
    #lambdaSample_3 <- mcmcMatrix[randomNumber,"lambda3"]
    #lambdaSample_4 <- mcmcMatrix[randomNumber,"lambda4"]
    #lambdaSample_5 <- mcmcMatrix[randomNumber,"lambda5"]
    
    lambdas <- list("lambda1", "lambda2", "lambda3",
                    "lambda4","lambda5","lambda6",
                    "lambda7","lambda8","lambda9")
    
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
    
    
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    outDf_3[kk,] <- newRow_3
    outDf_4[kk,] <- newRow_4
    outDf_5[kk,] <- newRow_5
    outDf_6[kk,] <- newRow_6
    outDf_7[kk,] <- newRow_7
    outDf_8[kk,] <- newRow_8
    outDf_9[kk,] <- newRow_9
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

ggplot()+
  geom_line(data = df_upperLower_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study70, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study70, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Indonesia (Arif, M et al.)") +
  scale_fill_viridis(discrete = T) 
  
ggplot()+
  geom_line(data = df_upperLower_2, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_2, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study75, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study75, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Malyasia (Azami, NAM et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_3, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_3, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study81, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study81, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Thailand (B.C.T.,et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_4, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_4, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study146, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study146, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Myanmar (EAC, Luvai et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_5, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_5, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study162, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study162, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Indonesia (F.Y., Sitepu et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_6, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_6, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study234, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study234, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Indonesia (K, Laras et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_7, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_7, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study277, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study277, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Singapore (LW, Ang et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_8, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_8, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study281, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study281, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Papua New Guinea (M, Graham et al.)") +
  scale_fill_viridis(discrete = T) 

ggplot()+
  geom_line(data = df_upperLower_9, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_9, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study423, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study423, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Thailand (Sompong Vongpunsawad et al.)") +
  scale_fill_viridis(discrete = T) 

mcmcMatrix <- data.frame(mcmcMatrix)
mcmc <- stack(mcmcMatrix)
mcmc$country <- c(NA)
mcmc$country[mcmc$ind == "lambda1"] <- "Indonesia"
mcmc$country[mcmc$ind == "lambda2"] <- "Malaysia"
mcmc$country[mcmc$ind == "lambda3"] <- "Thailand"
mcmc$country[mcmc$ind == "lambda4"] <- "Myanmar"
mcmc$country[mcmc$ind == "lambda5"] <- "Indonesia"
mcmc$country[mcmc$ind == "lambda6"] <- "Indonesia"
mcmc$country[mcmc$ind == "lambda7"] <- "Singapore"
mcmc$country[mcmc$ind == "lambda8"] <- "Papua New Guinea"
mcmc$country[mcmc$ind == "lambda9"] <- "Thailand"


ggplot(mcmc, aes(x= ind, y = values, fill = ind)) +  # Change filling color
  geom_boxplot()+
  theme_ipsum()+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)

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

#### Incidence
## incidence by every age : exp(-lambda(a))-exp(-lambda(a+1))
ager <- seq(1:80)


for(i in 1:9) {
  assign(paste0("IncidenceRow", i), matrix(NA, nrow=80, ncol = 1))
  assign(paste0("IncidenceDf",  i), matrix(NA, nrow=numSamples, ncol = 80))
}

for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  
  for(ii in 1:9) {
    assign(paste0("lambdaSample", ii), mcmcMatrix[randomNumber,ii])
  }
  
  lambdalist <- lapply(paste0("lambdaSample", c(1:9)), get)
  
  for(j in seq_along(ager)) {
    if (j<80) {
      IncidenceRow1[j,] <- exp(-lambdaSample1*ager[j])-exp(-lambdaSample1*ager[j+1]) 
    } else {
      IncidenceRow1[j,] <- exp(-lambdaSample1*ager[j])-exp(-lambdaSample1*81)
    }
  }
  IncidenceDf1[i,] <- (IncidenceRow1)*100000
}
for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  
  for(ii in 1:9) {
    assign(paste0("lambdaSample", ii), mcmcMatrix[randomNumber,ii])
  }
  
  lambdalist <- lapply(paste0("lambdaSample", c(1:9)), get)
  
  for(j in seq_along(ager)) {
    if (j<80) {
      IncidenceRow2[j,] <- exp(-lambdaSample2*ager[j])-exp(-lambdaSample2*ager[j+1]) 
    } else {
      IncidenceRow2[j,] <- exp(-lambdaSample2*ager[j])-exp(-lambdaSample2*81)
    }
  }
  IncidenceDf2[i,] <- (IncidenceRow2)*100000
}
for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  
  for(ii in 1:9) {
    assign(paste0("lambdaSample", ii), mcmcMatrix[randomNumber,ii])
  }
  
  lambdalist <- lapply(paste0("lambdaSample", c(1:9)), get)
  
  for(j in seq_along(ager)) {
    if (j<80) {
      IncidenceRow3[j,] <- exp(-lambdaSample3*ager[j])-exp(-lambdaSample3*ager[j+1]) 
    } else {
      IncidenceRow3[j,] <- exp(-lambdaSample3*ager[j])-exp(-lambdaSample3*81)
    }
  }
  IncidenceDf3[i,] <- (IncidenceRow3)*100000
}
for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  
  for(ii in 1:9) {
    assign(paste0("lambdaSample", ii), mcmcMatrix[randomNumber,ii])
  }
  
  lambdalist <- lapply(paste0("lambdaSample", c(1:9)), get)
  
  for(j in seq_along(ager)) {
    if (j<80) {
      IncidenceRow3[j,] <- exp(-lambdaSample3*ager[j])-exp(-lambdaSample3*ager[j+1]) 
    } else {
      IncidenceRow2[j,] <- exp(-lambdaSample3*ager[j])-exp(-lambdaSample3*81)
    }
  }
  IncidenceDf3[i,] <- (IncidenceRow3)*100000
}
for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  
  for(ii in 1:9) {
    assign(paste0("lambdaSample", ii), mcmcMatrix[randomNumber,ii])
  }
  
  lambdalist <- lapply(paste0("lambdaSample", c(1:9)), get)
  
  for(j in seq_along(ager)) {
    if (j<80) {
      IncidenceRow4[j,] <- exp(-lambdaSample4*ager[j])-exp(-lambdaSample4*ager[j+1]) 
    } else {
      IncidenceRow4[j,] <- exp(-lambdaSample4*ager[j])-exp(-lambdaSample4*81)
    }
  }
  IncidenceDf4[i,] <- (IncidenceRow4)*100000
}
for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  
  for(ii in 1:9) {
    assign(paste0("lambdaSample", ii), mcmcMatrix[randomNumber,ii])
  }
  
  lambdalist <- lapply(paste0("lambdaSample", c(1:9)), get)
  
  for(j in seq_along(ager)) {
    if (j<80) {
      IncidenceRow5[j,] <- exp(-lambdaSample5*ager[j])-exp(-lambdaSample5*ager[j+1]) 
    } else {
      IncidenceRow5[j,] <- exp(-lambdaSample5*ager[j])-exp(-lambdaSample5*81)
    }
  }
  IncidenceDf5[i,] <- (IncidenceRow5)*100000
}
for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  
  for(ii in 1:9) {
    assign(paste0("lambdaSample", ii), mcmcMatrix[randomNumber,ii])
  }
  
  lambdalist <- lapply(paste0("lambdaSample", c(1:9)), get)
  
  for(j in seq_along(ager)) {
    if (j<80) {
      IncidenceRow6[j,] <- exp(-lambdaSample6*ager[j])-exp(-lambdaSample6*ager[j+1]) 
    } else {
      IncidenceRow6[j,] <- exp(-lambdaSample6*ager[j])-exp(-lambdaSample6*81)
    }
  }
  IncidenceDf6[i,] <- (IncidenceRow6)*100000
}
for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  
  for(ii in 1:9) {
    assign(paste0("lambdaSample", ii), mcmcMatrix[randomNumber,ii])
  }
  
  lambdalist <- lapply(paste0("lambdaSample", c(1:9)), get)
  
  for(j in seq_along(ager)) {
    if (j<80) {
      IncidenceRow7[j,] <- exp(-lambdaSample7*ager[j])-exp(-lambdaSample7*ager[j+1]) 
    } else {
      IncidenceRow7[j,] <- exp(-lambdaSample7*ager[j])-exp(-lambdaSample7*81)
    }
  }
  IncidenceDf7[i,] <- (IncidenceRow7)*100000
}
for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  
  for(ii in 1:9) {
    assign(paste0("lambdaSample", ii), mcmcMatrix[randomNumber,ii])
  }
  
  lambdalist <- lapply(paste0("lambdaSample", c(1:9)), get)
  
  for(j in seq_along(ager)) {
    if (j<80) {
      IncidenceRow8[j,] <- exp(-lambdaSample8*ager[j])-exp(-lambdaSample8*ager[j+1]) 
    } else {
      IncidenceRow8[j,] <- exp(-lambdaSample8*ager[j])-exp(-lambdaSample8*81)
    }
  }
  IncidenceDf8[i,] <- (IncidenceRow8)*100000
}


# Dataframes for each study
IncidenceDf1 <- as.data.frame(IncidenceDf1)
colnames(IncidenceDf1) <- c(1:80)
IncLong1 <- stack(IncidenceDf1)
colnames(IncLong1) <- c("incidence", "age")

IncidenceDf2 <- as.data.frame(IncidenceDf2)
colnames(IncidenceDf2) <- c(1:80)
IncLong2 <- stack(IncidenceDf2)
colnames(IncLong2) <- c("incidence", "age")

IncidenceDf3 <- as.data.frame(IncidenceDf3)
colnames(IncidenceDf3) <- c(1:80)
IncLong3 <- stack(IncidenceDf3)
colnames(IncLong3) <- c("incidence", "age")

IncidenceDf4 <- as.data.frame(IncidenceDf4)
colnames(IncidenceDf4) <- c(1:80)
IncLong4 <- stack(IncidenceDf4)
colnames(IncLong4) <- c("incidence", "age")

IncidenceDf5 <- as.data.frame(IncidenceDf5)
colnames(IncidenceDf5) <- c(1:80)
IncLong5 <- stack(IncidenceDf5)
colnames(IncLong5) <- c("incidence", "age")

IncidenceDf6 <- as.data.frame(IncidenceDf6)
colnames(IncidenceDf6) <- c(1:80)
IncLong6 <- stack(IncidenceDf6)
colnames(IncLong6) <- c("incidence", "age")

IncidenceDf7 <- as.data.frame(IncidenceDf7)
colnames(IncidenceDf7) <- c(1:80)
IncLong7 <- stack(IncidenceDf7)
colnames(IncLong7) <- c("incidence", "age")

IncidenceDf8 <- as.data.frame(IncidenceDf8)
colnames(IncidenceDf8) <- c(1:80)
IncLong8 <- stack(IncidenceDf8)
colnames(IncLong8) <- c("incidence", "age")

# susceptible proportion
list_df <- mget(ls(pattern = "outDf_"))   # store dataframes into a list

for(ii in 1:9) {
  assign(paste0("susprop", ii), 1-list_df[[ii]])
}

# pop size (2010~2020)
WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/code/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx", 
                                                                 sheet = "thai_2010")
thaipop <-WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES
thaipop <- thaipop*1000 

for(i in 1:80) {
  ThaiSuspop2010 <- thaipop[1,i]*susprop3
  ThaiSuspop2011 <- thaipop[2,i]*susprop3
  ThaiSuspop2012 <- thaipop[3,i]*susprop3
  ThaiSuspop2013 <- thaipop[4,i]*susprop3
  ThaiSuspop2014 <- thaipop[5,i]*susprop3
  ThaiSuspop2015 <- thaipop[6,i]*susprop3
  ThaiSuspop2016 <- thaipop[7,i]*susprop3
  ThaiSuspop2017 <- thaipop[8,i]*susprop3
  ThaiSuspop2018 <- thaipop[9,i]*susprop3
  ThaiSuspop2019 <- thaipop[10,i]*susprop3
  ThaiSuspop2020 <- thaipop[11,i]*susprop3
}

Case3_2010     <- ThaiSuspop2010*(IncidenceDf3*1/100000)
Case3_2011     <- ThaiSuspop2011*(IncidenceDf3*1/100000)

thaisuspop_list <- mget(ls(pattern = "ThaiSuspop"))   # store dataframes into a list

for(i in 1:11) {
  for(j in 2010:2020){
    assign(paste0("Case3_", i), thaisuspop_list[[i]]*(IncidenceDf3*1/100000))
  }  
}




#Case3_2010$tot <- rowSums(Case3_2010[,c(1:80)])
LongCase3_2010 <- stack(Case3_2010)
LongCase3_2011 <- stack(Case3_2011)

# incidence boxplot (/100,000)
ggplot(IncLong1, aes(x= age, y = incidence, fill = age)) +  # Change filling color
  geom_boxplot(show.legend = FALSE, outlier.shape = NA)+
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 7))+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Annual incidence (/100,000) by age in Indonesia (Arif et al)")

ggplot(IncLong2, aes(x= age, y = incidence, fill = age)) +  # Change filling color
  geom_boxplot(show.legend = FALSE, outlier.shape = NA)+
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 7))+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Annual incidence (/100,000) by age in Malaysis (Azami, NAM et al)")

ggplot(IncLong3, aes(x= age, y = incidence, fill = age)) +  # Change filling color
  geom_boxplot(show.legend = FALSE, outlier.shape = NA)+
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 7))+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Annual incidence (/100,000) by age in Thailand (B.C.T et al)")

ggplot(IncLong4, aes(x= age, y = incidence, fill = age)) +  # Change filling color
  geom_boxplot(show.legend = FALSE, outlier.shape = NA)+
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 7))+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Annual incidence (/100,000) by age in Myanmar (EAC, Luvai et al)")

ggplot(IncLong5, aes(x= age, y = incidence, fill = age)) +  # Change filling color
  geom_boxplot(show.legend = FALSE, outlier.shape = NA)+
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 7))+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Annual incidence (/100,000) by age in Indonesia (Sitepu et al)")

ggplot(IncLong6, aes(x= age, y = incidence, fill = age)) +  # Change filling color
  geom_boxplot(show.legend = FALSE, outlier.shape = NA)+
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 7))+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Annual incidence (/100,000) by age in Indonesia (K, Laras et al)")

ggplot(IncLong7, aes(x= age, y = incidence, fill = age)) +  # Change filling color
  geom_boxplot(show.legend = FALSE, outlier.shape = NA)+
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 7))+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Annual incidence (/100,000) by age in Singapore (LW, Ang et al)")

ggplot(IncLong8, aes(x= age, y = incidence, fill = age)) +  # Change filling color
  geom_boxplot(show.legend = FALSE, outlier.shape = NA)+
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 7))+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Annual incidence (/100,000) by age in Papua New Guinea (M, Graham et al)")


# Case plots
ggplot(LongCase3_2010, aes(x= ind, y = values, fill = ind)) +  # Change filling color
  geom_boxplot(show.legend = FALSE, outlier.shape = NA)+
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 7))+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Number of CHIK cases in Thailand 2010")





#################################################################
########## Annual Incidence Rate (/100,000) by age group ########

# test seq_along func
for(j in seq_along(age1)) {
  if(j < 5) {
    a<- age1[j+1]-age1[j]
  } else {
    a<- age1[j]
  }
  print(a)
}

age1 <- c(1,5,10,15,20)

IncidenceDf1  <- matrix(NA,nrow=numSamples, ncol = 5)
IncidenceRow1 <- matrix(NA,nrow=5, ncol = 1)

for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  lambdaSample  <- mcmcMatrix[randomNumber,1]
  for(j in seq_along(age1)) {
    if(j<5) {
  IncidenceRow1[j,] <- (exp(-lambdaSample*age1[j]) - exp(-lambdaSample*age1[j+1]))
    } else {
  IncidenceRow1[j,] <- 0
    }
  }
  IncidenceDf1[i,] <- IncidenceRow1
}
IncidenceDf1 <- IncidenceDf1[,-5]
View(IncidenceDf1)

age2 <-c(20,40,65,80)

IncidenceDf2  <- matrix(NA,nrow=numSamples, ncol = 4)
IncidenceRow2 <- matrix(NA,nrow=4, ncol = 1)

for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  lambdaSample  <- mcmcMatrix[randomNumber,1]
  for(j in seq_along(age2)) {
    if(j<4) {
      IncidenceRow2[j,] <- (exp(-lambdaSample*age2[j]) - exp(-lambdaSample*age2[j+1]))
    } else {
      IncidenceRow2[j,] <- 0
    }
  }
  IncidenceDf2[i,] <- IncidenceRow2
}
IncidenceDf2 <- IncidenceDf2[,-4]
View(IncidenceDf2)

IncidenceDf <- cbind(IncidenceDf1, IncidenceDf2)
View(IncidenceDf)

age3 <- c(1,10,20,30,40,50,60,70,80)
IncidenceDf3  <- matrix(NA,nrow=numSamples, ncol = 9)
IncidenceRow3 <- matrix(NA,nrow=9, ncol = 1)

for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  lambdaSample  <- mcmcMatrix[randomNumber,1]
  for(j in seq_along(age3)) {
    if(j<9) {
      IncidenceRow3[j,] <- (exp(-lambdaSample*age3[j]) - exp(-lambdaSample*age3[j+1]))
    } else {
      IncidenceRow3[j,] <- 0
    }
  }
  IncidenceDf3[i,] <- IncidenceRow3
}
IncidenceDf3 <- IncidenceDf3[,-9]
View(IncidenceDf3)

AnnIncidenceDf3 <- (IncidenceDf3/10)*100000

AnnIncidenceDf  <- matrix(NA,nrow=numSamples, ncol = 7)
k <- c(4,5,5,5,19,24,15)

# Annual Incidence
AnnIncidenceDf[,1] <- IncidenceDf[,1]/k[1]
AnnIncidenceDf[,2] <- IncidenceDf[,2]/k[2]
AnnIncidenceDf[,3] <- IncidenceDf[,3]/k[3]
AnnIncidenceDf[,4] <- IncidenceDf[,4]/k[4]
AnnIncidenceDf[,5] <- IncidenceDf[,5]/k[5]
AnnIncidenceDf[,6] <- IncidenceDf[,6]/k[6]
AnnIncidenceDf[,7] <- IncidenceDf[,7]/k[7]
View(AnnIncidenceDf)

# Annual Incidence(/100,000)
AnnIncidenceDf[,1] <- (IncidenceDf[,1]/k[1])*1000
AnnIncidenceDf[,2] <- (IncidenceDf[,2]/k[2])*1000
AnnIncidenceDf[,3] <- (IncidenceDf[,3]/k[3])*1000
AnnIncidenceDf[,4] <- (IncidenceDf[,4]/k[4])*1000
AnnIncidenceDf[,5] <- (IncidenceDf[,5]/k[5])*1000
AnnIncidenceDf[,6] <- (IncidenceDf[,6]/k[6])*1000
AnnIncidenceDf[,7] <- (IncidenceDf[,7]/k[7])*1000
View(AnnIncidenceDf)

## incidence by every age : exp(-lambda(a))-exp(-lambda(a+1))
ager <- seq(1:80)
IncidenceDf  <- matrix(NA,nrow=numSamples, ncol = 80)
IncidenceRow <- matrix(NA,nrow=80, ncol = 1)

for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  lambdaSample  <- mcmcMatrix[randomNumber,1]
for(j in seq_along(ager)) {
  if (j<80) {
    IncidenceRow[j,] <- exp(-lambdaSample*ager[j])-exp(-lambdaSample*ager[j+1]) 
  } else {
    IncidenceRow[j,] <- exp(-lambdaSample*ager[j])-exp(-lambdaSample*81)
  }
}
  IncidenceDf[i,] <- IncidenceRow
}
IncidenceDf <- as.data.frame(IncidenceDf)
colnames(IncidenceDf) <- c(1:80)
IncLong <- stack(IncidenceDf)
colnames(IncLong) <- c("incidence", "age")

# susceptible prop
sus_prop <- 1-outDf_1

# tot pop
WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/code/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx", 
                                                                 sheet = "thai")
pop <-WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES
pop <- pop*1000 

# sus pop
sus_pop <- pop*sus_prop
susLong <- stack(sus_pop)
colnames(susLong) <- c("susceptible", "age")
#cases
cases <- sus_pop*IncidenceDf
caseLong <- stack(cases)
colnames(caseLong) <- c("case", "age")


# incidence (/100,000)
ggplot(IncLong, aes(x= age, y = incidence, fill = age)) +  # Change filling color
  geom_boxplot(show.legend = FALSE, outlier.shape = NA)+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 7))+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)
# cases
ggplot(caseLong, aes(x= age, y = case, fill = age)) +  # Change filling color
  geom_boxplot(show.legend = FALSE, outlier.shape = NA)+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 7))+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)
#sus pop
options(scipen = 999)
ggplot(susLong, aes(x= age, y = susceptible, fill = age)) +  # Change filling color
  geom_boxplot(show.legend = FALSE, outlier.shape = NA)+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, size = 7))+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)



quantiles <- matrix(NA, nrow=3, ncol=80)
qrange    <- c(0.5,0.025,0.975)
for(i in 1:3){
  quantiles[i,] <- apply(IncidenceDf[1:80], 2 , quantile , probs = qrange[i] , na.rm = TRUE)
}
quantiles <- as.data.frame(quantiles)
quantiles[nrow(quantiles) + 1,] <- c(1:80)

quantiles$range <- c("mid","lo","hi", "age")
long$value <- as.numeric(gsub(",", "", long$value))

colnames(quantiles) <- c(1:80)

#https://towardsdatascience.com/wide-to-long-data-how-and-when-to-use-pandas-melt-stack-and-wide-to-long-7c1e0f462a98

quantile_long <- matrix(NA,nrow=ncol(IncidenceDf), ncol = 3)
for(jj in 1:ncol(IncidenceDf)){
  quantiles <- IncidenceDf[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantile_long[jj,] <- quantiles
  df_upperLower_Case <- cbind(ager, quantile_long)
  df_upperLower_Case <- as.data.frame(df_upperLower_Case)
  colnames(df_upperLower_Case) <- c('agemid', 'mean', 'lo', 'hi')
}

df_upperLower_Case <- df_upperLower_Case %>% mutate(sd = hi - lo)


## bargraph 
ager <-c(1:7)
quantileMatrix <- matrix(NA,nrow=ncol(AnnIncidenceDf), ncol = 3)
for(jj in 1:ncol(AnnIncidenceDf)){
  quantiles <- AnnIncidenceDf[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix[jj,] <- quantiles
  df_upperLower <- cbind(ager, quantileMatrix)
  df_upperLower <- as.data.frame(df_upperLower)
  colnames(df_upperLower) <- c('age', 'mean', 'lower', 'upper')
}
View(df_upperLower)
write_xlsx(df_upperLower,"incidence.xlsx")

plot_ly(data=df_upperLower, x = ~age, y = ~mean, type = 'box', name = 'incidence') %>%
  layout(yaxis = list(title = 'Annual Incidence(/100,000'), barmode = 'stack')

agegr1 = data.frame(AnnIncidenceDf[,1])
agegr2 = data.frame(AnnIncidenceDf[,2])
agegr3 = data.frame(AnnIncidenceDf[,3])
agegr4 = data.frame(AnnIncidenceDf[,4])
agegr5 = data.frame(AnnIncidenceDf[,5])
agegr6 = data.frame(AnnIncidenceDf[,6])
agegr7 = data.frame(AnnIncidenceDf[,7])

colnames(agegr1)[1] <- "Incidence"
colnames(agegr2)[1] <- "Incidence"
colnames(agegr3)[1] <- "Incidence"
colnames(agegr4)[1] <- "Incidence"
colnames(agegr5)[1] <- "Incidence"
colnames(agegr6)[1] <- "Incidence"
colnames(agegr7)[1] <- "Incidence"

agegr1$age <- c("1-4")
agegr2$age <- c("5-9")
agegr3$age <- c("10-14")
agegr4$age <- c("15-19")
agegr5$age <- c("20-39")
agegr6$age <- c("40-64")
agegr7$age <- c("65-80")

IncidenceTot <- rbind(agegr1,agegr2,agegr3,agegr4,agegr5,agegr6,agegr7)

IncidenceTot$age <- factor(IncidenceTot$age, levels = c("1-4", "5-9", "10-14", "15-19", "20-39",
                                                        "40-64", "65-80"))

ggplot(IncidenceTot, aes(x= age, y = Incidence, fill = age)) +  # Change filling color
  geom_boxplot()+
  theme_ipsum()+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)+
  scale_x_discrete(limits = c("1-4", "5-9", "10-14", "15-19", "20-39",
                              "40-64", "65-80"))




#Susceptible pop
SusProp <- matrix(NA, nrow = numSamples, ncol = 7)

SusProp[,1] <- outDf[,4]
SusProp[,2] <- outDf[,9]-outDf[,4]
SusProp[,3] <- outDf[,14]-outDf[,9]
SusProp[,4] <- outDf[,19]-outDf[,14]
SusProp[,5] <- outDf[,39]-outDf[,19]
SusProp[,6] <- outDf[,64]-outDf[,39]
SusProp[,7] <- outDf[,80]-outDf[,64]

#Susceptible N = N* SProp
WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/code/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx", 
                                                                 sheet = "thai_group")
View(WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES)

pop <-WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES
pop <- pop*1000 

SusN <- matrix(NA, nrow = numSamples, ncol = 7)

for(i in 1:ncol(SusN)) {
  SusN[,i] <- SusProp[,i]*pop[1,i]
}
View(SusN)

# New Cases (=SusN * Annual Incidence rate)
NewCases <- matrix(NA, nrow = numSamples, ncol = 7)

for(i in 1:ncol(NewCases)) {
  NewCases[,i] <- SusN[,i]*AnnIncidenceDf[,i]
}
View(NewCases)

# case graph
case1 = data.frame(NewCases[,1])
case2 = data.frame(NewCases[,2])
case3 = data.frame(NewCases[,3])
case4 = data.frame(NewCases[,4])
case5 = data.frame(NewCases[,5])
case6 = data.frame(NewCases[,6])
case7 = data.frame(NewCases[,7])

colnames(case1)[1] <- "Cases"
colnames(case2)[1] <- "Cases"
colnames(case3)[1] <- "Cases"
colnames(case4)[1] <- "Cases"
colnames(case5)[1] <- "Cases"
colnames(case6)[1] <- "Cases"
colnames(case7)[1] <- "Cases"

case1$age <- c("1-4")
case2$age <- c("5-9")
case3$age <- c("10-14")
case4$age <- c("15-19")
case5$age <- c("20-39")
case6$age <- c("40-64")
case7$age <- c("65-80")

CaseTot <- rbind(case1,case2,case3,case4,case5,case6,case7)

CaseTot$age <- factor(CaseTot$age, levels = c("1-4", "5-9", "10-14", "15-19", "20-39",
                                                        "40-64", "65-80"))

ggplot(CaseTot, aes(x= age, y = Cases, fill = age)) +  # Change filling color
  geom_boxplot()+
  theme_ipsum()+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)+
  scale_x_discrete(limits = c("1-4", "5-9", "10-14", "15-19", "20-39",
                              "40-64", "65-80"))
