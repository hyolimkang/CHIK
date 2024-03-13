## only non-fever: test constant vs. epidemic
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
library(maps)
library(rgdal)
library(rgeos)
library(sf)
library(ggpubr)
library(tidyr)
library(ggridges)
library(gridExtra)

CountryModel <- read_excel("Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "inclusion", col_types = c("numeric", 
                                                              "text", "numeric", "numeric", "numeric", 
                                                              "numeric", "text", "text", "text", 
                                                              "text", "text", "text", "text", "text", 
                                                              "text", "text", "text"))
View(CountryModel)
#Rename data
df_eca = CountryModel %>% filter(region == "ECA")

df_eca[,c("midpoint","lower","upper")] = binom.confint(df_eca$N.pos, df_eca$N, method="exact")[,c("mean","lower","upper")]

df_eca <-  df_eca %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

study318  <-  df_eca %>% filter(study_no == 318)

# study 318
# constant model: test DIC
jcode <- "model{ 
	for (i in 1:4){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 1:5){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2007-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1989,2007) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 1:5){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2007-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2007-delta1) && age[i] < 19, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1989,2007)
  delta2  ~ dunif(1969,1987)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 1:5){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2007-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2007-delta2) && age[i] < 39, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2007-delta1) && age[i] < 19, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1989,2007)
  delta2  ~ dunif(1969,1987)
  delta3  ~ dunif(1949,1967)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 1:5){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2007-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2007-delta3) && age[i] < 59, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2007-delta2) && age[i] < 39, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2007-delta1) && age[i] < 19, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1989,2007)
  delta2  ~ dunif(1969,1987)
  delta3  ~ dunif(1949,1967)
  delta4  ~ dunif(1929,1947)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4", "delta1", "delta2", "delta3", "delta4")

# 5 epidemic
jcode <- "model{ 
	for (i in 1:5){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2007-delta5), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5)
       ,ifelse(age[i]> (2007-delta4) && age[i] < 79, 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2007-delta3) && age[i] < 59, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2007-delta2) && age[i] < 39, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2007-delta1) && age[i] < 19, 1-exp(-lambda1),
       0)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1989,2007)
  delta2  ~ dunif(1969,1987)
  delta3  ~ dunif(1949,1967)
  delta4  ~ dunif(1929,1947)
  delta5  ~ dunif(1908,1927)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4","lambda5", "delta1", "delta2", "delta3", "delta4","delta5")


# Run model
mcmc.length=50000
jdat = list(n.pos= df_eca$N.pos,
            N=df_eca$N,
            age=df_eca$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVector, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

# Calculate DIC
dic.samples(jmod, n.iter = mcmc.length)

# credible interval
constantCR <- function(paramVector, mcmcMatrix) {
  
  ager = 0:100
  numSamples = 1000
  
  for(ii in 1:length(paramVector)) {
    
    outDf <- matrix(NA, nrow=numSamples, ncol=length(ager))
    
    for (kk in 1:numSamples ) {
      randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
      
      lambdaSample <- mcmcMatrix[sample(nrow(mcmcMatrix), 1, replace=T), "lambda"]
      
      newRow  <-  1-exp(-lambdaSample*ager)
      
      outDf[kk,] <- newRow
    }
  }
  return(outDf)
}

Epi1CR <- function(paramVector, mcmcMatrix, ager1, ager2){
  
  numSamples = 1000
  
  for(ii in 1:length(paramVector)) {
    
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
  return(outDf)
}

Epi2CR <- function(paramVector, mcmcMatrix, ager1, ager2, ager3){
  numSamples = 1000
  for(ii in 1:length(paramVector)) {
    
    outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
    outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
    outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager3))
    
    for (kk in 1:numSamples ) {
      randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
      
      lambdaSample1   <- mcmcMatrix[randomNumber,"lambda1"]
      lambdaSample2   <- mcmcMatrix[randomNumber,"lambda2"]
      deltaSample_1   <- mcmcMatrix[randomNumber,"delta1"]
      deltaSample_2   <- mcmcMatrix[randomNumber,"delta2"]
      
      newRow_1 <- 0
      newRow_2 <- 1-exp(-lambdaSample1)
      newRow_3 <- 1-exp(-lambdaSample1-lambdaSample2)
      
      outDf_1[kk,] <- newRow_1
      outDf_2[kk,] <- newRow_2
      outDf_3[kk,] <- newRow_3
      
      outDf <- cbind(outDf_1,outDf_2,outDf_3)
    }
  }
  return(outDf)
}

Epi3CR <- function(paramVector, mcmcMatrix, ager1, ager2, ager3, ager4){
  numSamples = 1000
  for(ii in 1:length(paramVector)) {
    
    outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
    outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
    outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager3))
    outDf_4 <- matrix(NA,nrow=numSamples, ncol = length(ager4))
    
    for (kk in 1:numSamples ) {
      randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
      
      lambdaSample1    <- mcmcMatrix[randomNumber,"lambda1"]
      lambdaSample2    <- mcmcMatrix[randomNumber,"lambda2"]
      lambdaSample3    <- mcmcMatrix[randomNumber,"lambda3"]
      
      deltaSample_1   <- mcmcMatrix[randomNumber,"delta1"]
      deltaSample_2   <- mcmcMatrix[randomNumber,"delta2"]
      deltaSample_3   <- mcmcMatrix[randomNumber,"delta3"]
      
      newRow_1 <- 0
      newRow_2 <- 1-exp(-lambdaSample1)
      newRow_3 <- 1-exp(-lambdaSample1-lambdaSample2)
      newRow_4 <- 1-exp(-lambdaSample1-lambdaSample2-lambdaSample3)
      
      outDf_1[kk,] <- newRow_1
      outDf_2[kk,] <- newRow_2
      outDf_3[kk,] <- newRow_3
      outDf_4[kk,] <- newRow_4
      
      outDf <- cbind(outDf_1,outDf_2,outDf_3,outDf_4)
    }
  }
  return(outDf)
}

Epi4CR <- function(paramVector, mcmcMatrix, ager1, ager2, ager3, ager4, ager5){
  numSamples = 1000
  for(ii in 1:length(paramVector)) {
    
    outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
    outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
    outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager3))
    outDf_4 <- matrix(NA,nrow=numSamples, ncol = length(ager4))
    outDf_5 <- matrix(NA,nrow=numSamples, ncol = length(ager5))
    
    for (kk in 1:numSamples ) {
      randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
      
      lambdaSample1    <- mcmcMatrix[randomNumber,"lambda1"]
      lambdaSample2    <- mcmcMatrix[randomNumber,"lambda2"]
      lambdaSample3    <- mcmcMatrix[randomNumber,"lambda3"]
      lambdaSample4    <- mcmcMatrix[randomNumber,"lambda4"]
      
      deltaSample_1   <- mcmcMatrix[randomNumber,"delta1"]
      deltaSample_2   <- mcmcMatrix[randomNumber,"delta2"]
      deltaSample_3   <- mcmcMatrix[randomNumber,"delta3"]
      deltaSample_4   <- mcmcMatrix[randomNumber,"delta4"]
      
      newRow_1 <- 0
      newRow_2 <- 1-exp(-lambdaSample1)
      newRow_3 <- 1-exp(-(lambdaSample1+lambdaSample2))
      newRow_4 <- 1-exp(-(lambdaSample1+lambdaSample2+lambdaSample3))
      newRow_5 <- 1-exp(-(lambdaSample1+lambdaSample2+lambdaSample3+lambdaSample4))
      
      outDf_1[kk,] <- newRow_1
      outDf_2[kk,] <- newRow_2
      outDf_3[kk,] <- newRow_3
      outDf_4[kk,] <- newRow_4
      outDf_5[kk,] <- newRow_5
      
      outDf <- cbind(outDf_1,outDf_2,outDf_3,outDf_4,outDf_5)
    }
  }
  return(outDf)
}

# get quantile matrices function
quantmat <- function(outDf){
  ager = 0:100
  quantileMatrix <- matrix(NA,nrow=ncol(outDf), ncol = 3)
  for(jj in 1:ncol(outDf)){
    quantiles <- outDf[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrix[jj,] <- quantiles
  }
  df_upperLower <- cbind(ager, quantileMatrix)
  df_upperLower <- as.data.frame(df_upperLower)
  colnames(df_upperLower) <- c('agemid', 'mean', 'upper', 'lower')
  return(df_upperLower)
}
plotfuncXY <- function(df_upperLower, study){
  g <- ggplot()+
    geom_line(data = df_upperLower, aes(x=agemid, y=mean), color = "#558C8C")+
    geom_ribbon(data = df_upperLower, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
    geom_point(data = study, aes(x=agemid, y=midpoint), color = "#558C8C")+  
    geom_linerange(data = study, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
    theme_bw()+
    theme(axis.line.x = element_blank(),
          axis.title.x = element_blank())+
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    ylim(0, 1)+
    xlab(element_blank()) + 
    ylab(element_blank())+
    scale_fill_viridis(discrete = T)+
    facet_wrap(~ country, nrow = 1) 
  return(g)
}

##results computation
outDf318 <- Epi4CR(paramVector, mcmcMatrix, ager1 = 0:4, ager2 = 5:24, ager3 = 25:44, ager4 = 45:64, ager5 = 65:100)
df_upperLower318 <- quantmat(outDf318)
g1<- plot318 <- plotfuncYonly(df_upperLower318, study318)

outDf318 <- constantCR(paramVector, mcmcMatrix)
df_upperLower318 <- quantmat(outDf318)
g2<- plot318 <- plotfuncXY(df_upperLower318, study318)


eca_graph<-  grid.arrange(g2, ncol=1,
                         left = "Proportion Seropositive",
                         bottom = "Age (years)",
                         top = "Seropositivity in ECA (non-fever population based studies)")
ggsave("ecaAll.pdf", eca_graph, dpi=1000, device= "pdf", height=8, width=10,units="in", bg=NULL)


