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
rm(list = ls())    # remove any variables in R's memory 

options(scipen = 999)
#Read file
CountryModel <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "inclusion")
CountryModel <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "inclusion")
View(CountryModel)
#Rename data
df_ssa = CountryModel %>% filter(region == "SSA")

df_ssa[,c("midpoint","lower","upper")] = binom.confint(df_ssa$N.pos, df_ssa$N, method="exact")[,c("mean","lower","upper")]

df_ssa <-  df_ssa %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

df_ssa <- df_ssa[df_ssa$antibody != "IgM",] # remove data that has only IgM

study7       <-  df_ssa %>% filter(study_no == 7)
study7       <-  study7[-4,]
study17      <-  df_ssa %>% filter(study_no == 17)
study67      <-  df_ssa %>% filter(study_no == 67)
study101     <-  df_ssa %>% filter(study_no == 101)
study106     <-  df_ssa %>% filter(study_no == 106)
study132     <-  df_ssa %>% filter(study_no == 132)
study136     <-  df_ssa %>% filter(study_no == 136)
study149     <-  df_ssa %>% filter(study_no == 149)
study170     <-  df_ssa %>% filter(study_no == 170)
study173     <-  df_ssa %>% filter(study_no == 173)
study204_1   <-  df_ssa %>% filter(study_no == 204 & country == "Burkina Faso")
study204_2   <-  df_ssa %>% filter(study_no == 204 & country == "Gabon")
study236     <-  df_ssa %>% filter(study_no == 236)
study279     <-  df_ssa %>% filter(study_no == 279)
study306     <-  df_ssa %>% filter(study_no == 306)
study321     <-  df_ssa %>% filter(study_no == 321)
study322     <-  df_ssa %>% filter(study_no == 322)
study323     <-  df_ssa %>% filter(study_no == 323)
study354     <-  df_ssa %>% filter(study_no == 354)
study355     <-  df_ssa %>% filter(study_no == 355)
study416     <-  df_ssa %>% filter(study_no == 416)
study485     <-  df_ssa %>% filter(study_no == 485)
study494     <-  df_ssa %>% filter(study_no == 494)
study142     <-  df_ssa %>% filter(study_no == 142)
study218     <-  df_ssa %>% filter(study_no == 218)
study283     <-  df_ssa %>% filter(study_no == 283)

# Define model
jcode <- "model{ 
	for (i in 1:3){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda1*age[i]) #catalytic model
	}
	for (i in 5:9){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda2*age[i]) #catalytic model
	}
	for (i in 10:14){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda3*age[i]) #catalytic model
	}
	for (i in 15:19){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda4*age[i]) #catalytic model
	}
	for (i in 21:24){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda5*age[i]) #catalytic model
	}
	for (i in 25:30){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda6*age[i]) #catalytic model
	}
	for (i in 31:36){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda7*age[i]) #catalytic model
	}
	for (i in 37:41){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda8*age[i]) #catalytic model
	}
	for (i in 42:44){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda9*age[i]) #catalytic model
	}
	for (i in 45:48){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda10*age[i]) #catalytic model
	}
	for (i in 49:59){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda11*age[i]) #catalytic model
	}
	for (i in 60:70){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda12*age[i]) #catalytic model
	}
	for (i in 71:76){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda13*age[i]) #catalytic model
	}
	for (i in 77:81){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda14*age[i]) #catalytic model
	}
	for (i in 82:89){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda15*age[i]) #catalytic model
	}
	for (i in 90:92){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda16*age[i]) #catalytic model
	}
	for (i in 93:95){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda17*age[i]) #catalytic model
	}
	for (i in 96:98){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda18*age[i]) #catalytic model
	}
	for (i in 99:105){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda19*age[i]) #catalytic model
	}
	for (i in 107:109){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda20*age[i]) #catalytic model
	}
	for (i in 110:113){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda21*age[i]) #catalytic model
	}
	for (i in 114:117){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda22*age[i]) #catalytic model
	}
	for (i in 119:122){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda23*age[i]) #catalytic model
	}
	for (i in 123:127){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda24*age[i]) #catalytic model
	}
	for (i in 129:134){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda25*age[i]) #catalytic model
	}
	for (i in 136:139){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda26*age[i]) #catalytic model
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
  lambda14 ~ dunif(0,1) #uninformative prior
  lambda15 ~ dunif(0,1) #uninformative prior
  lambda16 ~ dunif(0,1) #uninformative prior
  lambda17 ~ dunif(0,1) #uninformative prior
  lambda18 ~ dunif(0,1) #uninformative prior
  lambda19 ~ dunif(0,1) #uninformative prior
  lambda20 ~ dunif(0,1) #uninformative prior
  lambda21 ~ dunif(0,1) #uninformative prior
  lambda22 ~ dunif(0,1) #uninformative prior
  lambda23 ~ dunif(0,1) #uninformative prior
  lambda24 ~ dunif(0,1) #uninformative prior
  lambda25 ~ dunif(0,1) #uninformative prior
  lambda26 ~ dunif(0,1) #uninformative prior
  
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4",
                 "lambda5","lambda6","lambda7","lambda8","lambda9",
                 "lambda10","lambda11","lambda12","lambda13",
                 "lambda14","lambda15","lambda16","lambda17","lambda18",
                 "lambda19","lambda20","lambda21","lambda22","lambda23","lambda24",
                 "lambda25","lambda26")
# Run model
mcmc.length=50000
jdat = list(n.pos= df_ssa$N.pos,
            N=df_ssa$N,
            age=df_ssa$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVector, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

# Calculate DIC
dic.samples(jmod, n.iter = mcmc.length)

# FOIs
mcmcMatrix <- data.frame(mcmcMatrix)
mcmc <- stack(mcmcMatrix)
mcmc$country <- c(NA)
mcmc$country[mcmc$ind == "lambda1"]  <- "Benin"
mcmc$country[mcmc$ind == "lambda2"]  <- "Senegal"
mcmc$country[mcmc$ind == "lambda3"]  <- "Mozambique"
mcmc$country[mcmc$ind == "lambda4"]  <- "Nigeria"
mcmc$country[mcmc$ind == "lambda5"]  <- "Zambia"
mcmc$country[mcmc$ind == "lambda6"]  <- "Sudan"
mcmc$country[mcmc$ind == "lambda7"]  <- "Tanzania"
mcmc$country[mcmc$ind == "lambda8"]  <- "Ethiopia"
mcmc$country[mcmc$ind == "lambda9"]  <- "Ethiopia"
mcmc$country[mcmc$ind == "lambda10"] <- "Ethiopia"
mcmc$country[mcmc$ind == "lambda11"] <- "Burkina Faso"
mcmc$country[mcmc$ind == "lambda12"] <- "Gabon"
mcmc$country[mcmc$ind == "lambda13"] <- "Comoros"
mcmc$country[mcmc$ind == "lambda14"] <- "Cameroon"
mcmc$country[mcmc$ind == "lambda15"] <- "Senegal"
mcmc$country[mcmc$ind == "lambda16"] <- "Mozambique"
mcmc$country[mcmc$ind == "lambda17"] <- "Mozambique"
mcmc$country[mcmc$ind == "lambda18"] <- "Tanzania"
mcmc$country[mcmc$ind == "lambda19"] <- "Nigeria"
mcmc$country[mcmc$ind == "lambda20"] <- "Nigeria"
mcmc$country[mcmc$ind == "lambda21"] <- "DRC"
mcmc$country[mcmc$ind == "lambda22"] <- "Mozambique"
mcmc$country[mcmc$ind == "lambda23"] <- "Gabon"
mcmc$country[mcmc$ind == "lambda24"] <- "Tanzania"
mcmc$country[mcmc$ind == "lambda25"] <- "Ghana"
mcmc$country[mcmc$ind == "lambda26"] <- "Kenya"

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


df_sample <- mcmc %>% group_by(country) %>%
               sample_n(1)

df_quant <- mcmc %>% group_by(country) %>%
              summarise(lo  = quantile(values, 0.025),
                        mid = quantile(values, 0.5),
                        hi  = quantile(values, 0.975))

country_names <- unique(mcmc$country)

sampled_df <- list()

for(i in seq_along(country_names)){
  foi <- mcmc[mcmc$country == country_names[i],]
  #sample 1000 rows from the subsetted df
  sampfoi <- foi[sample(nrow(foi),1000,replace=TRUE),]
  sampled_df[[i]] <-sampfoi
}

dflist <- lapply(sampled_df, data.frame)
final  <- do.call(rbind,dflist)
countryfoi <- unstack(final, values~country)


for(ii in 1:length(country_names)) {
  assign(paste0("countrylambda_", ii), countryfoi[sample(nrow(countryfoi), 1, replace=T), 
                                                 country_names[[ii]]])
}

assign(paste0("countrylambda_",3), countryfoi[sample(nrow(countryfoi),1, replace=T),
                                             "Burkina.Faso"])

# Prevalence profiles from pooled foi ------------------------------------------
#age range
ager = 0:100
numSamples = 1000

for(ii in 1:length(countryfoi)) {
  
  for(i in 1:15) {
    assign(paste0("conDf_", i), matrix(NA, nrow=numSamples, ncol = length(ager)))
  }
  for (kk in 1:numSamples) {
    
    for(ii in 1:length(country_names)) {
      assign(paste0("countrylambda_", ii), countryfoi[sample(nrow(countryfoi), 1, replace=T), 
                                                      country_names[[ii]]])
    }
    
    assign(paste0("countrylambda_",3), countryfoi[sample(nrow(countryfoi),1, replace=T),
                                                  "Burkina.Faso"])
    
    
    conRow_1  <-  1-exp(-countrylambda_1*ager)
    conRow_2  <-  1-exp(-countrylambda_2*ager)
    conRow_3  <-  1-exp(-countrylambda_3*ager)
    conRow_4  <-  1-exp(-countrylambda_4*ager)
    conRow_5  <-  1-exp(-countrylambda_5*ager)
    conRow_6  <-  1-exp(-countrylambda_6*ager)
    conRow_7  <-  1-exp(-countrylambda_7*ager)
    conRow_8  <-  1-exp(-countrylambda_8*ager)
    conRow_9  <-  1-exp(-countrylambda_9*ager)
    conRow_10 <-  1-exp(-countrylambda_10*ager)
    conRow_11 <-  1-exp(-countrylambda_11*ager)
    conRow_12 <-  1-exp(-countrylambda_12*ager)
    conRow_13 <-  1-exp(-countrylambda_13*ager)
    conRow_14 <-  1-exp(-countrylambda_14*ager)
    conRow_15 <-  1-exp(-countrylambda_15*ager)

    conDf_1[kk,] <- conRow_1
    conDf_2[kk,] <- conRow_2
    conDf_3[kk,] <- conRow_3
    conDf_4[kk,] <- conRow_4
    conDf_5[kk,] <- conRow_5
    conDf_6[kk,] <- conRow_6
    conDf_7[kk,] <- conRow_7
    conDf_8[kk,] <- conRow_8
    conDf_9[kk,] <- conRow_9
    conDf_10[kk,] <- conRow_10
    conDf_11[kk,] <- conRow_11
    conDf_12[kk,] <- conRow_12
    conDf_13[kk,] <- conRow_13
    conDf_14[kk,] <- conRow_14
    conDf_15[kk,] <- conRow_15

  }
}

# incidence function
for(i in 1:15) {
  assign(paste0("conIncRow", i), matrix(NA, nrow=101, ncol = 1))
  assign(paste0("conIncDf",  i), matrix(NA, nrow=numSamples, ncol = 101))
}

for(i in 1:numSamples) {
  randomNumber  <- floor(runif(1, min = 1, max = nrow(countryfoi)))
  
  for(ii in 1:15) {
    assign(paste0("conlambda", ii), countryfoi[randomNumber,ii])
  }
  
  for(j in 0:101) {
    if (j<101) {
      conIncRow1[j,] <- exp(-conlambda1*ager[j])-exp(-conlambda1*ager[j+1]) 
    } else {
      conIncRow1[j,] <- exp(-conlambda1*ager[j])-exp(-conlambda1*101)
    }
  }
  conIncDf1[i,] <- (conIncRow1)
  colnames(conIncDf1) <- paste(0:100)
  conIncDf1 <- as.data.frame(conIncDf1)
}





#-------------------------------------------------------------------------------

# credible interval
for(ii in 1:length(paramVector)) {
  
  for(i in 1:26) {
    assign(paste0("outDf_", i), matrix(NA, nrow=numSamples, ncol = length(ager)))
  }
  
  for (kk in 1:numSamples ) {
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdas <- list("lambda1","lambda2","lambda3","lambda4",
                    "lambda5","lambda6","lambda7","lambda8","lambda9",
                    "lambda10","lambda11","lambda12","lambda13",
                    "lambda14","lambda15","lambda16","lambda17","lambda18",
                    "lambda19","lambda20","lambda21","lambda22","lambda23","lambda24",
                    "lambda25","lambda26")
    
    for(ii in 1:length(lambdas)) {
      assign(paste0("lambdaSample_", ii), mcmcMatrix[sample(nrow(mcmcMatrix), 1, replace=T), 
                                                     lambdas[[ii]]])
    }
    
    newRow_1  <-  1-exp(-lambdaSample_1*ager)
    newRow_2  <-  1-exp(-lambdaSample_2*ager)
    newRow_3  <-  1-exp(-lambdaSample_3*ager)
    newRow_4  <-  1-exp(-lambdaSample_4*ager)
    newRow_5  <-  1-exp(-lambdaSample_5*ager)
    newRow_6  <-  1-exp(-lambdaSample_6*ager)
    newRow_7  <-  1-exp(-lambdaSample_7*ager)
    newRow_8  <-  1-exp(-lambdaSample_8*ager)
    newRow_9  <-  1-exp(-lambdaSample_9*ager)
    newRow_10 <-  1-exp(-lambdaSample_10*ager)
    newRow_11 <-  1-exp(-lambdaSample_11*ager)
    newRow_12 <-  1-exp(-lambdaSample_12*ager)
    newRow_13 <-  1-exp(-lambdaSample_13*ager)
    newRow_14 <-  1-exp(-lambdaSample_14*ager)
    newRow_15 <-  1-exp(-lambdaSample_15*ager)
    newRow_16 <-  1-exp(-lambdaSample_16*ager)
    newRow_17 <-  1-exp(-lambdaSample_17*ager)
    newRow_18 <-  1-exp(-lambdaSample_18*ager)
    newRow_19 <-  1-exp(-lambdaSample_19*ager)
    newRow_20 <-  1-exp(-lambdaSample_20*ager)
    newRow_21 <-  1-exp(-lambdaSample_21*ager)
    newRow_22 <-  1-exp(-lambdaSample_22*ager)
    newRow_23 <-  1-exp(-lambdaSample_23*ager)
    newRow_24 <-  1-exp(-lambdaSample_24*ager)
    newRow_25 <-  1-exp(-lambdaSample_25*ager)
    newRow_26 <-  1-exp(-lambdaSample_26*ager)
    
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
    outDf_14[kk,] <- newRow_14
    outDf_15[kk,] <- newRow_15
    outDf_16[kk,] <- newRow_16
    outDf_17[kk,] <- newRow_17
    outDf_18[kk,] <- newRow_18
    outDf_19[kk,] <- newRow_19
    outDf_20[kk,] <- newRow_20
    outDf_21[kk,] <- newRow_21
    outDf_22[kk,] <- newRow_22
    outDf_23[kk,] <- newRow_23
    outDf_24[kk,] <- newRow_24
    outDf_25[kk,] <- newRow_25
    outDf_26[kk,] <- newRow_26
    
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

quantileMatrix_14 <- matrix(NA,nrow=ncol(outDf_14), ncol = 3)
for(jj in 1:ncol(outDf_14)){
  quantiles <- outDf_14[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_14[jj,] <- quantiles
  df_upperLower_14 <- cbind(ager, quantileMatrix_14)
  df_upperLower_14 <- as.data.frame(df_upperLower_14)
  colnames(df_upperLower_14) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_15 <- matrix(NA,nrow=ncol(outDf_15), ncol = 3)
for(jj in 1:ncol(outDf_15)){
  quantiles <- outDf_15[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_15[jj,] <- quantiles
  df_upperLower_15 <- cbind(ager, quantileMatrix_15)
  df_upperLower_15 <- as.data.frame(df_upperLower_15)
  colnames(df_upperLower_15) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_16 <- matrix(NA,nrow=ncol(outDf_16), ncol = 3)
for(jj in 1:ncol(outDf_16)){
  quantiles <- outDf_16[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_16[jj,] <- quantiles
  df_upperLower_16 <- cbind(ager, quantileMatrix_16)
  df_upperLower_16 <- as.data.frame(df_upperLower_16)
  colnames(df_upperLower_16) <- c('agemid', 'mean', 'upper', 'lower')
}
quantileMatrix_17 <- matrix(NA,nrow=ncol(outDf_17), ncol = 3)
for(jj in 1:ncol(outDf_17)){
  quantiles <- outDf_17[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_17[jj,] <- quantiles
  df_upperLower_17 <- cbind(ager, quantileMatrix_17)
  df_upperLower_17 <- as.data.frame(df_upperLower_17)
  colnames(df_upperLower_17) <- c('agemid', 'mean', 'upper', 'lower')
}
quantileMatrix_18 <- matrix(NA,nrow=ncol(outDf_18), ncol = 3)
for(jj in 1:ncol(outDf_18)){
  quantiles <- outDf_18[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_18[jj,] <- quantiles
  df_upperLower_18 <- cbind(ager, quantileMatrix_18)
  df_upperLower_18 <- as.data.frame(df_upperLower_18)
  colnames(df_upperLower_18) <- c('agemid', 'mean', 'upper', 'lower')
}
quantileMatrix_19 <- matrix(NA,nrow=ncol(outDf_19), ncol = 3)
for(jj in 1:ncol(outDf_19)){
  quantiles <- outDf_19[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_19[jj,] <- quantiles
  df_upperLower_19 <- cbind(ager, quantileMatrix_19)
  df_upperLower_19 <- as.data.frame(df_upperLower_19)
  colnames(df_upperLower_19) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_20 <- matrix(NA,nrow=ncol(outDf_20), ncol = 3)
for(jj in 1:ncol(outDf_20)){
  quantiles <- outDf_20[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_20[jj,] <- quantiles
  df_upperLower_20 <- cbind(ager, quantileMatrix_20)
  df_upperLower_20 <- as.data.frame(df_upperLower_20)
  colnames(df_upperLower_20) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_21 <- matrix(NA,nrow=ncol(outDf_21), ncol = 3)
for(jj in 1:ncol(outDf_21)){
  quantiles <- outDf_21[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_21[jj,] <- quantiles
  df_upperLower_21 <- cbind(ager, quantileMatrix_21)
  df_upperLower_21 <- as.data.frame(df_upperLower_21)
  colnames(df_upperLower_21) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_22 <- matrix(NA,nrow=ncol(outDf_22), ncol = 3)
for(jj in 1:ncol(outDf_22)){
  quantiles <- outDf_22[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_22[jj,] <- quantiles
  df_upperLower_22 <- cbind(ager, quantileMatrix_22)
  df_upperLower_22 <- as.data.frame(df_upperLower_22)
  colnames(df_upperLower_22) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_23 <- matrix(NA,nrow=ncol(outDf_23), ncol = 3)
for(jj in 1:ncol(outDf_23)){
  quantiles <- outDf_23[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_23[jj,] <- quantiles
  df_upperLower_23 <- cbind(ager, quantileMatrix_23)
  df_upperLower_23 <- as.data.frame(df_upperLower_23)
  colnames(df_upperLower_23) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_24 <- matrix(NA,nrow=ncol(outDf_24), ncol = 3)
for(jj in 1:ncol(outDf_24)){
  quantiles <- outDf_24[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_24[jj,] <- quantiles
  df_upperLower_24 <- cbind(ager, quantileMatrix_24)
  df_upperLower_24 <- as.data.frame(df_upperLower_24)
  colnames(df_upperLower_24) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_25 <- matrix(NA,nrow=ncol(outDf_25), ncol = 3)
for(jj in 1:ncol(outDf_25)){
  quantiles <- outDf_25[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_25[jj,] <- quantiles
  df_upperLower_25 <- cbind(ager, quantileMatrix_25)
  df_upperLower_25 <- as.data.frame(df_upperLower_25)
  colnames(df_upperLower_25) <- c('agemid', 'mean', 'upper', 'lower')
}

quantileMatrix_26 <- matrix(NA,nrow=ncol(outDf_26), ncol = 3)
for(jj in 1:ncol(outDf_26)){
  quantiles <- outDf_26[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix_26[jj,] <- quantiles
  df_upperLower_26 <- cbind(ager, quantileMatrix_26)
  df_upperLower_26 <- as.data.frame(df_upperLower_26)
  colnames(df_upperLower_26) <- c('agemid', 'mean', 'upper', 'lower')
}





#-------------------------------------------------------------------------------

graph1 <- ggplot()+
  geom_line(data = df_upperLower_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study7, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study7, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Benin (A, Bacci et al)") +
  scale_fill_viridis(discrete = T) 

graph2 <- ggplot()+
  geom_line(data = df_upperLower_2, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_2, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study17, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study17, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Senegal (A, Sow et al)") +
  scale_fill_viridis(discrete = T) 

graph3 <- ggplot()+
  geom_line(data = df_upperLower_3, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_3, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study67, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study67, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Mozambique (Antonio)") +
  scale_fill_viridis(discrete = T) 

graph4 <- ggplot()+
  geom_line(data = df_upperLower_4, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_4, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study101, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study101, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Nigeria (C.A., Omatola et al)") +
  scale_fill_viridis(discrete = T) 

graph5 <- ggplot()+
  geom_line(data = df_upperLower_5, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_5, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study106, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study106, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Zambia (Chisenga et al.)") +
  scale_fill_viridis(discrete = T) 

graph6 <- ggplot()+
  geom_line(data = df_upperLower_6, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_6, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study132, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study132, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Sudan (DM, Watts et al.)") +
  scale_fill_viridis(discrete = T) 

graph7 <- ggplot()+
  geom_line(data = df_upperLower_7, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_7, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study136, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study136, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Tanzania (E, Kinimi et al.)") +
  scale_fill_viridis(discrete = T) 

graph8 <- ggplot()+
  geom_line(data = df_upperLower_8, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_8, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study149, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study149, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Ethiopia (Endale, A et al.)") +
  scale_fill_viridis(discrete = T) 

graph9 <- ggplot()+
  geom_line(data = df_upperLower_9, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_9, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study170, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study170, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Ethiopia (G, Asebe et al.)") +
  scale_fill_viridis(discrete = T) 

graph10 <- ggplot()+
  geom_line(data = df_upperLower_10, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_10, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study173, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study173, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Ethiopia (G, Asebe et al.)") +
  scale_fill_viridis(discrete = T) 

graph11 <- ggplot()+
  geom_line(data = df_upperLower_11, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_11, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study204_1, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study204_1, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Burkina Faso (J, Kyungah Lim et al.)") +
  scale_fill_viridis(discrete = T) 

graph12 <- ggplot()+
  geom_line(data = df_upperLower_12, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_12, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study204_2, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study204_2, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Gabon (J, Kyungah Lim et al.)") +
  scale_fill_viridis(discrete = T) 

graph13<- ggplot()+
  geom_line(data = df_upperLower_13, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_13, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study236, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study236, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Comoros (K, Sergon and AA et al.)") +
  scale_fill_viridis(discrete = T) 

graph14<- ggplot()+
  geom_line(data = df_upperLower_14, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_14, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study279, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study279, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Cameroon (M, Demanou et al.)") +
  scale_fill_viridis(discrete = T) 

graph15<- ggplot()+
  geom_line(data = df_upperLower_15, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_15, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study306, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study306, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Senegal (MC, Seck et al.)") +
  scale_fill_viridis(discrete = T) 

graph16<- ggplot()+
  geom_line(data = df_upperLower_16, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_16, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study321, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study321, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Mozambique (Mugabe, VA et al.)") +
  scale_fill_viridis(discrete = T) 

graph17<- ggplot()+
  geom_line(data = df_upperLower_17, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_17, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study322, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study322, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Mozambique (Muianga, A et al.)") +
  scale_fill_viridis(discrete = T) 

graph18<- ggplot()+
  geom_line(data = df_upperLower_18, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_18, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study323, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study323, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Tanzania (Mwanyika, GO et al.)") +
  scale_fill_viridis(discrete = T) 

graph19<- ggplot()+
  geom_line(data = df_upperLower_19, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_19, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study354, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study354, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Nigeria (OM, Kolawole et al.)") +
  scale_fill_viridis(discrete = T) 

graph20 <- ggplot()+
  geom_line(data = df_upperLower_20, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_20, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study355, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study355, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Nigeria (OM, Olajiga et al.)") +
  scale_fill_viridis(discrete = T) 

graph21 <- ggplot()+
  geom_line(data = df_upperLower_21, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_21, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study416, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study416, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in DRC (S, Proesmans et al.)") +
  scale_fill_viridis(discrete = T) 

graph22 <- ggplot()+
  geom_line(data = df_upperLower_22, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_22, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study485, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study485, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Mozambique (VS, António et al.)") +
  scale_fill_viridis(discrete = T) 

graph23 <- ggplot()+
  geom_line(data = df_upperLower_23, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_23, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study494, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study494, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Gabon (Y, Ushijima et al.)") +
  scale_fill_viridis(discrete = T) 

graph24 <- ggplot()+
  geom_line(data = df_upperLower_24, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_24, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study142, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study142, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Tanzania (E., Kinimi et al.)") +
  scale_fill_viridis(discrete = T) 

graph25 <- ggplot()+
  geom_line(data = df_upperLower_25, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_25, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study218, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study218, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Ghana (JA, Adusei et al.)") +
  scale_fill_viridis(discrete = T) 

graph26 <- ggplot()+
  geom_line(data = df_upperLower_26, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_upperLower_26, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  geom_point(data = study283, aes(x=agemid, y=midpoint), color = "#558C8C")+  
  geom_linerange(data = study283, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("Seropositivity in Kenya (M, Inziani et al.)") +
  scale_fill_viridis(discrete = T)

figure1 <- ggarrange(graph1, graph2, graph3, graph4 + font("x.text", size = 10),
                     ncol = 2, nrow = 2)
ggsave("ssafig1.pdf", figure1, dpi=1000, device= "pdf", height=8, width=10,units="in", bg=NULL)

figure2 <- ggarrange(graph5, graph6, graph7, graph8 + font("x.text", size = 10),
                     ncol = 2, nrow = 2)

figure3 <- ggarrange(graph9, graph10, graph11, graph12 + font("x.text", size = 10),
                     ncol = 2, nrow = 2)

figure4 <- ggarrange(graph13, graph14, graph15, graph16 + font("x.text", size = 10),
                     ncol = 2, nrow = 2)

figure5 <- ggarrange(graph17, graph18, graph19, graph20 + font("x.text", size = 10),
                     ncol = 2, nrow = 2)

figure6 <- ggarrange(graph21, graph22, graph23, graph24 + font("x.text", size = 10),
                     ncol = 2, nrow = 2)

figure7 <- ggarrange(graph25, graph26 + font("x.text", size = 10),
                     ncol = 2, nrow = 2)




#-------------------------------------------------------------------------------
# Map https://stackoverflow.com/questions/61182881/specific-country-map-with-district-cities-using-r
#-------------------------------------------------------------------------------
map(database="world", region="Ethiopia", fill= TRUE)
sf <- st_read(dsn="D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/ETH_adm", 
              layer="ETH_adm2")
shape <- readOGR(dsn="D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/ETH_adm", 
                 layer="ETH_adm2")
plot(sf["NAME_2"], axes = TRUE, main = "Districts")
plot(shape)
plot(sf)
unique(sf$NAME_1)
unique(sf$NAME_2)

south_omo <- subset(sf, NAME_2 == "South Omo")
gambella <- subset(sf, NAME_1 == "Gambela Peoples")
himowa   <- subset(sf, NAME_2 == "North Gonder")

ggplot() + 
  geom_sf(data = sf, aes(fill = NAME_2)) + theme(legend.position = "none")+
  theme_minimal()+
  guides(fill = "none")+
  coord_sf() +
  theme_void()

eth_foi <- mcmcMatrix[,c(25,26,2)]
eth_foi_mat <- data.frame(matrix(NA, nrow=3, ncol=2))
eth_foi_mat[1,2] <- quantile(eth_foi[,c(1)], c(0.5))
eth_foi_mat[2,2] <- quantile(eth_foi[,c(2)], c(0.5))
eth_foi_mat[3,2] <- quantile(eth_foi[,c(3)], c(0.5))
eth_foi_mat[1,1] <- "lambda8"
eth_foi_mat[2,1] <- "lambda9"
eth_foi_mat[3,1] <- "lambda10"
colnames(eth_foi_mat) <- c("lambda", "value")

south_omo$value <- eth_foi_mat[1,2]
gambella$value  <- eth_foi_mat[2,2]
himowa$value    <- eth_foi_mat[3,2]

eth <- ggplot() + 
  geom_sf(data = sf, aes(fill = "darkgrey")) + 
  scale_fill_identity() + 
  theme_minimal() +
  guides(fill = "none") +
  geom_sf(data = south_omo, fill = "red", alpha = 0.5) +
  geom_sf(data = gambella, fill = "red", alpha = 0.5) + 
  geom_sf(data = himowa, fill = "red", alpha = 0.5) + 
  coord_sf() +
  theme_void()

ssa_sf <- st_read(dsn="D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/afr_g2014_2013_0", 
              layer="afr_g2014_2013_0")
ssa_shape <- readOGR(dsn="D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/afr_g2014_2013_0", 
                 layer="afr_g2014_2013_0")

plot(ssa_sf["ADM0_NAME"], axes = TRUE, main = "ISO3")

ggplot() + 
  geom_sf(data = ssa_sf, aes(fill = ADM0_NAME)) + theme(legend.position = "none")+
  theme_minimal()+
  guides(fill = "none")+
  coord_sf() +
  theme_void()

ssa_con <- subset(ssa_sf, ADM0_NAME %in% c("Benin","Ethiopia", "Senegal", "Mozambique",
                                           "Nigeria"))

ggplot() + 
  geom_sf(data = ssa_sf, aes(fill = "darkgrey")) + 
  scale_fill_identity() + 
  theme_minimal() +
  guides(fill = "none") +
  geom_sf(data = ssa_con, fill = "yellow", alpha = 0.5) +
  coord_sf() +
  theme_void()


#-------------------------------------------------------------------------------

