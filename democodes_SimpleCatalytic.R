###simple catalytic model
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
chik_systematic_review_v1 <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "prac")
chik_systematic_review_v1 <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "Sheet1")

#Rename data
df_chik = chik_systematic_review_v1 

df_chik[,c("midpoint","lower","upper")] = binom.confint(df_chik$N.pos, df_chik$N, method="exact")[,c("mean","lower","upper")]

df_chik <-  df_chik %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2) %>% filter(study == 4)

#sub-setting datafiles by study
datS1 <- df_chik[df_chik$study == "1",]
datS2 <- df_chik[df_chik$study == "2",]
datS3 <- df_chik[df_chik$study == "3",]

# Define model
jcode <- "model{ 
	for (i in 1:length(N)){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
		}
	#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"


# Run model
mcmc.length=50000
jdat = list(n.pos= df_chik$N.pos,
            N=df_chik$N,
            age=df_chik$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, "lambda", n.iter=mcmc.length)
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
ager=1:80


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

## for loop multiple age ranges

ages <- list(ager1, ager2, ager3)

for(i in 1: length(ages)) {
  assign(paste0("outDf_", i), matrix(NA, nrow=numSamples, ncol = length(ages[[i]])))
}

# store results into a long-format lists using lapply function
ll_age=list(age1=14:42, age2=5:85, age3=15:49)
ll_long = lapply(1:length(ll_age),
                 function(x) as.data.frame(matrix(ncol=length(ll_age[[x]]),nrow=1000)) %>% 
                   # converting to long format
                   mutate(index=x) %>% pivot_longer(!index)) 
# convert to a single dataframe, with age in a new column
a <- ll_long %>% bind_rows() %>% mutate(name=as.numeric(gsub("V","",name))) %>% rowwise() %>% mutate(agegroup=ll_age[[index]][name] )

# generate ramdomNmber for credible interval

for(ii in 1:length(paramVector)) {
  
  outDf <- matrix(NA,nrow=numSamples, ncol = length(ager1))
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample <- mcmcMatrix[randomNumber,"lambda"]

    newRow <-  1 - exp(-lambdaSample * (ager))

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



