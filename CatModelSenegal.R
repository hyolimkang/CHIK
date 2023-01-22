## IgG only 
options(scipen =999)
library(readxl)
require(tidyverse)
require(rjags)
require(binom)
library(ggplot2)
library(dplyr)
library(varhandle)
require(MCMCvis)
library(cowplot)
library(hrbrthemes)
library(viridis)
library(loo)

CountryModel <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "Africa")
View(CountryModel)
#Rename data
df_chik = CountryModel 

df_chik <-  df_chik %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2) 

df_chik[,c("midpoint","lower","upper")] = binom.confint(df_chik$N.pos, df_chik$N, method="exact")[,c("mean","lower","upper")]

senegal <- df_chik[df_chik$country == "Senegal",]

# simple catalytic model
jcode <- "model{ 
	for (i in 21:25){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] <- 1 - exp(-lambda*age[i])
	}
	#  prior dists
  lambda ~ dunif(0,10)       #uninformative prior
}"
paramVectorSimple <- c("lambda")

# discrete FOI model (estimate time point where FOI changes: 1 time change)
jcode <- "model{ 
	for (i in 21:25){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] > (2012-delta), 1-exp(-(lambda_1*(age[i]-(2012-delta)) + lambda_2*(2012-delta))),
                            1-exp(-lambda_2*age[i]))
    # time-varying catalytic model
	}

	#  prior dists
  lambda_1 ~ dunif(0,1)       #uninformative prior
  lambda_2 ~ dunif(0,1)       #uninformative prior
  delta    ~ dunif(1933,2011) #uninformative prior
}"

paramVectorSenegal <- c("lambda_1", "lambda_2","delta")

# time-varying model
jcode <- "model{ 
	for (i in 21:25){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]>5 && age[i]<10, 1-exp(-lambda1*2.5)
       ,ifelse(age[i]>10 && age[i]<20, 1-exp(-5*lambda1-5*lambda2),
       ifelse(age[i]>20 && age[i]<40, 1-exp(-5*lambda1-10*lambda2-10*lambda3),
       ifelse(age[i]>40 && age[i]<60, 1-exp(-5*lambda1-10*lambda2-20*lambda3-10*lambda4),
       1-exp(-5*lambda1-10*lambda2-20*lambda3-20*lambda4-10*lambda5)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1 ~ dunif(0,1)       #uninformative prior
  lambda2 ~ dunif(0,1)       #uninformative prior
  lambda3 ~ dunif(0,1)       #uninformative prior
  lambda4 ~ dunif(0,1)       #uninformative prior
  lambda5 ~ dunif(0,1)       #uninformative prior
}"
paramVectorSenegalTV <- c("lambda1", "lambda2","lambda3","lambda4",
                          "lambda5")

# epidemic model 
jcode <- "model{ 
	for (i in 21:25){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2012-delta2), 0,
                            1-exp(-lambda1*(delta2-delta1)-lambda2*(2012-delta2)))
    # time-varying catalytic model
	}

	#  prior dists
  lambda1 ~ dunif(0,1)       #uninformative prior
  lambda2 ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2002,2007) #uninformative prior
  delta2  ~ dunif(2002,2007) #uninformative prior
}"

paramVectorEpi <- c("lambda1", "lambda2","delta1", "delta2")


# epimodel 
jcode <- "model{ 
	for (i in 21:25){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2012-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2002,2007) #uninformative prior
}"
paramVectorEpi1 <- c("lambda", "delta")

# epimodel 2 
jcode <- "model{ 
	for (i in 21:25){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2012-delta2), 1-exp(-2*lambda)
       ,ifelse(age[i]> (2012-delta1) && age[i] < 10, 1-exp(-lambda),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2003,2007)
  delta2  ~ dunif(1993,2002)
}"
paramVectorEpi2 <- c("lambda", "delta1", "delta2")

# epimodel 3 
jcode <- "model{ 
	for (i in 21:25){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2012-delta3), 1-exp(-3*lambda),
        ifelse(age[i]> (2012-delta2) && age[i] < 20, 1-exp(-2*lambda),
        ifelse(age[i]> (2012-delta1) && age[i] < 10, 1-exp(-lambda),
        0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2003,2007)
  delta2  ~ dunif(1993,2002)
  delta3  ~ dunif(1973,1992)

}"
paramVectorEpi3 <- c("lambda", "delta1", "delta2", "delta3")

# epimodel 4 
jcode <- "model{ 
	for (i in 21:25){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2012-delta4), 1-exp(-4*lambda),
        ifelse(age[i]> (2012-delta3) && age[i] < 40, 1-exp(-3*lambda),
        ifelse(age[i]> (2012-delta2) && age[i] < 20, 1-exp(-2*lambda),
        ifelse(age[i]> (2012-delta1) && age[i] < 10, 1-exp(-lambda),
        0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2003,2007)
  delta2  ~ dunif(1993,2002)
  delta3  ~ dunif(1973,1992)
  delta4  ~ dunif(1953,1972)
}"
paramVectorEpi4 <- c("lambda", "delta1", "delta2", "delta3", "delta4")

# epimodel 5 
jcode <- "model{ 
	for (i in 21:25){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2012-delta5), 1-exp(-5*lambda),
        ifelse(age[i]> (2012-delta4) && age[i] < 60, 1-exp(-4*lambda),
        ifelse(age[i]> (2012-delta3) && age[i] < 40, 1-exp(-3*lambda),
        ifelse(age[i]> (2012-delta2) && age[i] < 20, 1-exp(-2*lambda),
        ifelse(age[i]> (2012-delta1) && age[i] < 10, 1-exp(-lambda),
        0)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2003,2007)
  delta2  ~ dunif(1993,2002)
  delta3  ~ dunif(1973,1992)
  delta4  ~ dunif(1953,1972)
  delta5  ~ dunif(1933,1952)
}"
paramVectorEpi5 <- c("lambda", "delta1", "delta2", "delta3", "delta4", "delta5")


# simple model
jcode <- "model{ 
	for (i in 21:25){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] <- 1-exp(-lambda*age[i])
    loglik[i] <- logdensity.bin(n.pos[i],seropos_est[i],N[i])
	}

	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
}"

paramVector <- c("lambda")

# Run model
mcmc.length=50000
jdat = list(n.pos= df_chik$N.pos,
            N=df_chik$N,
            age=df_chik$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVectorSimple, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)
View(mcmcMatrix)

# Calculate DIC
dic.samples(jmod, n.iter = mcmc.length)

#age range
ager =1:80
numSamples = 1000

# age range for epimodel2
ager1=1:7
ager2=8:13
ager3=14:80

# age range for epimodel3
ager1=1:7
ager2=8:13
ager3=14:25
ager4=26:80

# age range for epimodel4
ager1=1:7
ager2=8:13
ager3=14:25
ager4=26:45
ager5=46:80

# age range for epimodel4
ager1=1:7
ager2=8:13
ager3=14:25
ager4=26:45
ager5=46:66
ager6=67:80

# generate ramdomNmber for simple model
for(ii in 1:length(paramVectorSimple)) {
  
  outDf <- matrix(NA,nrow=numSamples, ncol = length(ager))

  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample <- mcmcMatrix[randomNumber,"lambda"]

    newRow <- 1-exp(-lambdaSample*ager)
    
    outDf[kk,] <- newRow
  }
}

# generate ramdomNmber for credible interval (for 1 point change)
for(ii in 1:length(paramVectorSenegal)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))

  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample_1 <- mcmcMatrix[randomNumber,"lambda_1"]
    lambdaSample_2 <- mcmcMatrix[randomNumber,"lambda_2"]
    deltaSample <- mcmcMatrix[randomNumber,"delta"]
    
    newRow_1 <- 1-exp(-lambdaSample_2*ager1)
    newRow_2 <- 1-exp(-(lambdaSample_1*(ager2-(2012-deltaSample)) + lambdaSample_2*(2012-deltaSample)))
                      
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    outDf <- cbind(outDf_1,outDf_2)
  }
}

# generate ramdomNmber for credible interval (for 5 lambdas)
for(ii in 1:length(paramVectorSenegalTV)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
  outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager3))
  outDf_4 <- matrix(NA,nrow=numSamples, ncol = length(ager4))
  outDf_5 <- matrix(NA,nrow=numSamples, ncol = length(ager5))
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample_1 <- mcmcMatrix[randomNumber,"lambda1"]
    lambdaSample_2 <- mcmcMatrix[randomNumber,"lambda2"]
    lambdaSample_3 <- mcmcMatrix[randomNumber,"lambda3"]
    lambdaSample_4 <- mcmcMatrix[randomNumber,"lambda4"]
    lambdaSample_5 <- mcmcMatrix[randomNumber,"lambda5"]
    
    for(j in 1:length(ager1)) {
      newRow1  <- 1- exp(-lambdaSample_1*ager1)
    }
    for(m in 1:length(ager2)) {
      newRow2 <-  1-exp(-5*lambdaSample_1-ager2*lambdaSample_2)
    } 
    for(n in 1:length(ager3)) {
      newRow3 <-  1-exp(-5*lambdaSample_1 -10*lambdaSample_2 -ager3*lambdaSample_3)
    }  
    for(mm in 1:length(ager4)) {
      newRow4 <-  1-exp(-5*lambdaSample_1 -10*lambdaSample_2-20*lambdaSample_3-ager4*lambdaSample_4)
    }  
    for(k in 1:length(ager5)) {
      newRow5 <-  1-exp(-5*lambdaSample_1-10*lambdaSample_2-20*lambdaSample_3-20*lambdaSample_4-ager5*lambdaSample_5)
    }  
     outDf_1[kk,] <- newRow1
     outDf_2[kk,] <- newRow2
     outDf_3[kk,] <- newRow3
     outDf_4[kk,] <- newRow4
     outDf_5[kk,] <- newRow5
     
     outDf <- cbind(outDf_1, outDf_2, outDf_3, outDf_4, outDf_5)
  }
}

# generate ramdomNmber for epidemic model
for(ii in 1:length(paramVectorEpi)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))

  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample_1 <- mcmcMatrix[randomNumber,"lambda1"]
    lambdaSample_2 <- mcmcMatrix[randomNumber,"lambda2"]
    deltaSample_1  <- mcmcMatrix[randomNumber,"delta1"]
    deltaSample_2  <- mcmcMatrix[randomNumber,"delta2"]
    
    newRow_1 <- 0
    newRow_2 <- 1-exp(-lambdaSample_1*(deltaSample_2-deltaSample_1)-lambdaSample_2*(2012-deltaSample_2))

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
    
    lambdaSample   <- mcmcMatrix[randomNumber,"lambda"]
    deltaSample_1  <- mcmcMatrix[randomNumber,"delta1"]
    deltaSample_2  <- mcmcMatrix[randomNumber,"delta2"]
    deltaSample_3  <- mcmcMatrix[randomNumber,"delta3"]
    
    newRow_1 <- 0
    newRow_2 <- 1-exp(-lambdaSample)
    newRow_3 <- 1-exp(-lambdaSample*2)
    newRow_4 <- 1-exp(-lambdaSample*3)
    
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    outDf_3[kk,] <- newRow_3
    outDf_4[kk,] <- newRow_4
    
    outDf <- cbind(outDf_1,outDf_2,outDf_3,outDf_4)
  }
}

# generate ramdomNmber for epidemic model (epi4)
for(ii in 1:length(paramVectorEpi4)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
  outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager3))
  outDf_4 <- matrix(NA,nrow=numSamples, ncol = length(ager4))
  outDf_5 <- matrix(NA,nrow=numSamples, ncol = length(ager5))
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample   <- mcmcMatrix[randomNumber,"lambda"]
    deltaSample_1  <- mcmcMatrix[randomNumber,"delta1"]
    deltaSample_2  <- mcmcMatrix[randomNumber,"delta2"]
    deltaSample_3  <- mcmcMatrix[randomNumber,"delta3"]
    deltaSample_4  <- mcmcMatrix[randomNumber,"delta4"]
    
    newRow_1 <- 0
    newRow_2 <- 1-exp(-lambdaSample)
    newRow_3 <- 1-exp(-lambdaSample*2)
    newRow_4 <- 1-exp(-lambdaSample*3)
    newRow_5 <- 1-exp(-lambdaSample*4)
  
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    outDf_3[kk,] <- newRow_3
    outDf_4[kk,] <- newRow_4
    outDf_5[kk,] <- newRow_5
    
    outDf <- cbind(outDf_1,outDf_2,outDf_3,outDf_4,outDf_5)
  }
}

# generate ramdomNmber for epidemic model (epi5)
for(ii in 1:length(paramVectorEpi5)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
  outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager3))
  outDf_4 <- matrix(NA,nrow=numSamples, ncol = length(ager4))
  outDf_5 <- matrix(NA,nrow=numSamples, ncol = length(ager5))
  outDf_6 <- matrix(NA,nrow=numSamples, ncol = length(ager6))
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample   <- mcmcMatrix[randomNumber,"lambda"]
    deltaSample_1  <- mcmcMatrix[randomNumber,"delta1"]
    deltaSample_2  <- mcmcMatrix[randomNumber,"delta2"]
    deltaSample_3  <- mcmcMatrix[randomNumber,"delta3"]
    deltaSample_4  <- mcmcMatrix[randomNumber,"delta4"]
    deltaSample_5  <- mcmcMatrix[randomNumber,"delta5"]
    
    newRow_1 <- 0
    newRow_2 <- 1-exp(-lambdaSample)
    newRow_3 <- 1-exp(-lambdaSample*2)
    newRow_4 <- 1-exp(-lambdaSample*3)
    newRow_5 <- 1-exp(-lambdaSample*4)
    newRow_6 <- 1-exp(-lambdaSample*5)
    
    outDf_1[kk,] <- newRow_1
    outDf_2[kk,] <- newRow_2
    outDf_3[kk,] <- newRow_3
    outDf_4[kk,] <- newRow_4
    outDf_5[kk,] <- newRow_5
    outDf_6[kk,] <- newRow_6
    
    outDf <- cbind(outDf_1,outDf_2,outDf_3,outDf_4,outDf_5,outDf_6)
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

#serograph -senegal
ggplot()+
  geom_line(data = df_upperLower_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = senegal, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = senegal, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositive in Senegal (A,Sow et al)")+
  theme(plot.title = element_text(color="black", size=10))

mcmcFOI <- data.frame(mcmcMatrix)
young = data.frame(mcmcFOI[,3])
old = data.frame(mcmcFOI[,2])
colnames(young)[1] <- "lambda"
colnames(old)[1] <- "lambda"
young$year <- c("2003-2012")
old$year <- c("1933-2002")
FOIdata<- rbind(young, old)

ggplot(FOIdata, aes(x= year, y = lambda, fill = year)) +  # Change filling color
  geom_boxplot()+
  theme_ipsum()+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)
  

#######################Zambia (age vary) ####################
# varying
jcode <- "model{ 
	for (i in 44:47){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]>18 && age[i]<24, 1-exp(-lambda1*age[i])
       ,ifelse(age[i]>24 && age[i]<35, 1-exp(-24*lambda1-(age[i]-24)*lambda2),
       ifelse(age[i]>36 && age[i]<45, 1-exp(-24*lambda1 -11*lambda2 - (age[i]-34)*lambda3),
       1-exp(-24*lambda1-11*lambda2-10*lambda3-(age[i]-46)*lambda4))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1 ~ dunif(0,1)       #uninformative prior
  lambda2 ~ dunif(0,1)       #uninformative prior
  lambda3 ~ dunif(0,1)       #uninformative prior
  lambda4 ~ dunif(0,1)       #uninformative prior
}"
paramVectorZambia <- c("lambda1", "lambda2","lambda3", "lambda4")

# Run model
mcmc.length=50000
jdat = list(n.pos= df_africa$N.pos,
            N=df_africa$N,
            age=df_africa$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVectorZambia, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)
View(mcmcMatrix)

## 1. Sample from mcmc chain to get 95% credible intervals (model uncertainty)
ager=1:80
ager1=1:20
ager2=21:40
ager3=41:60
ager4=61:80
numSamples = 1000

# sampling for yearly variation
for(ii in 1:length(paramVectorZambia)) {
  
  outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
  outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager3))
  outDf_4 <- matrix(NA,nrow=numSamples, ncol = length(ager4))
  
  
  for (kk in 1:numSamples) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample_1 <- mcmcMatrix[randomNumber,"lambda1"]
    lambdaSample_2 <- mcmcMatrix[randomNumber,"lambda2"]
    lambdaSample_3 <- mcmcMatrix[randomNumber,"lambda3"]
    lambdaSample_4 <- mcmcMatrix[randomNumber,"lambda4"]
    
    newRow1 <-  1-exp(-lambdaSample_1*ager1)
    newRow2 <-  1-exp(-24*lambdaSample_1-(ager2-24)*lambdaSample_2)
    newRow3 <-  1-exp(-24*lambdaSample_1 -11*lambdaSample_2 - (ager3-34)*lambdaSample_3)
    newRow4 <-   1-exp(-24*lambdaSample_1-11*lambdaSample_2-10*lambdaSample_3-(ager4-46)*lambdaSample_4)
    
    outDf_1[kk,] <- newRow1
    outDf_2[kk,] <- newRow2
    outDf_3[kk,] <- newRow3
    outDf_4[kk,] <- newRow4
    
    outDf <- cbind(outDf_1,outDf_2,outDf_3,outDf_4)
    
  }
}

# get quantile matrices 
ager= 1:80

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
  geom_point(data = datA10, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = datA10, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")

#####Zambdia simple
jcode <- "model{ 
	for (i in 44:47){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] <- 1-exp(-lambda1*age[i])
    # time-varying catalytic model
	}
	#  prior dists
  lambda1 ~ dunif(0,1)       #uninformative prior
}"
paramVectorZambia <- c("lambda1")

# Run model
mcmc.length=50000
jdat = list(n.pos= df_africa$N.pos,
            N=df_africa$N,
            age=df_africa$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVectorZambia, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)
View(mcmcMatrix)

## 1. Sample from mcmc chain to get 95% credible intervals (model uncertainty)
ager=1:80
numSamples = 1000

# sampling for yearly variation
for(ii in 1:length(paramVectorZambia)) {
  
  outDf <- matrix(NA,nrow=numSamples, ncol = length(ager))
  
  for (kk in 1:numSamples) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample_1 <- mcmcMatrix[randomNumber,"lambda1"]

    newRow1 <-  1-exp(-lambdaSample_1*ager)

    outDf[kk,] <- newRow1

  }
}

# get quantile matrices 
ager= 1:80

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
  geom_point(data = datA10, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = datA10, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+ ### data 2 
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")




