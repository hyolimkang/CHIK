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

CountryModel <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "inclusion", col_types = c("numeric", 
                                                              "text", "numeric", "numeric", "numeric", 
                                                              "numeric", "text", "text", "text", 
                                                              "text", "text", "text", "text", "text", 

                                                                                                                          "text", "text", "text"))
CountryModel <- read_excel("Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "inclusion", col_types = c("numeric", 
                                                              "text", "numeric", "numeric", "numeric", 
                                                              "numeric", "text", "text", "text", 
                                                              "text", "text", "text", "text", "text", 
                                                              "text", "text", "text"))
View(CountryModel)
#Rename data
df_sa = CountryModel %>% filter(region == "SA")

df_sa[,c("midpoint","lower","upper")] = binom.confint(df_sa$N.pos, df_sa$N, method="exact")[,c("mean","lower","upper")]

df_sa <-  df_sa %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

df_sa <- df_sa %>%
  dplyr::mutate(age_rand = purrr::map2_dbl(age_min, age_max, ~runif(1, .x, .y)))

bootstrap_age <- function(df, varname) {
  df %>%
    dplyr::mutate(!!varname := purrr::map2_dbl(age_min, age_max, ~runif(1, .x, .y))) -> df
  return(df)
}

for(i in 1:1000) {
  varname <- paste0("age_rand_", i)
  df_sa <- bootstrap_age(df_sa, varname)
}

# Generate column names
col_names <- paste0("age_rand_", 1:1000)


df_sa <- df_sa[df_sa$antibody != "IgM",] # remove data that has only IgM

study197       <-  df_sa %>% filter(study_no == 197)
study249       <-  df_sa %>% filter(study_no == 249)
study486       <-  df_sa %>% filter(study_no == 486)
study450_1     <-  df_sa %>% filter(study_no == 450 & year == "2009") 
study450_2     <-  df_sa %>% filter(study_no == 450 & year == "2019")

# study 197
# constant model: test DIC
jcode <- "model{ 
	for (i in 8:12){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 8:12){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2011-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2003,2011) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 8:12){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2011-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2011-delta1) && age[i] < 9, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2003,2011)
  delta2  ~ dunif(1998,2001)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 8:12){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2011-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2011-delta2) && age[i] < 14, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2011-delta1) && age[i] < 9, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2003,2011)
  delta2  ~ dunif(1998,2001)
  delta3  ~ dunif(1992,1996)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# study 249
# constant model: test DIC
jcode <- "model{ 
	for (i in 13:15){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 13:15){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2017-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2010,2017) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 13:15){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2017-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2017-delta1) && age[i] < 8, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2010,2017)
  delta2  ~ dunif(2001,2008)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 13:15){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2017-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2017-delta2) && age[i] < 17, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2017-delta1) && age[i] < 8, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2010,2017)
  delta2  ~ dunif(2001,2008)
  delta3  ~ dunif(1973,1999)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")


# study 450_1
# constant model: test DIC
jcode <- "model{ 
	for (i in 24:29){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 24:29){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2009-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2001,2009) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 24:29){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2009-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2009-delta1) && age[i] < 9, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2001,2009)
  delta2  ~ dunif(1991,1999)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 24:29){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2009-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2009-delta2) && age[i] < 19, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2009-delta1) && age[i] < 9, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2001,2009)
  delta2  ~ dunif(1991,1999)
  delta3  ~ dunif(1981,1989)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 24:29){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2009-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2009-delta3) && age[i] < 29, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2009-delta2) && age[i] < 19, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2009-delta1) && age[i] < 9, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2001,2009)
  delta2  ~ dunif(1991,1999)
  delta3  ~ dunif(1981,1989)
  delta4  ~ dunif(1971,1979)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4", "delta1", "delta2", "delta3", "delta4")

# study 450_2
# constant model: test DIC
jcode <- "model{ 
	for (i in 30:35){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 30:35){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2019-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2011,2019) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 30:35){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 9, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2011,2019)
  delta2  ~ dunif(2001,2009)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 30:35){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2019-delta2) && age[i] < 19, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 9, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2011,2019)
  delta2  ~ dunif(2001,2009)
  delta3  ~ dunif(1991,1999)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 30:35){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2019-delta3) && age[i] < 29, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2019-delta2) && age[i] < 19, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 9, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2011,2019)
  delta2  ~ dunif(2001,2009)
  delta3  ~ dunif(1991,1999)
  delta4  ~ dunif(1981,1989)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4", "delta1", "delta2", "delta3", "delta4")

# study 486
# constant model: test DIC
jcode <- "model{ 
	for (i in 36:40){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 36:40){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2019-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1981,1989) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 36:40){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (1989-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (1989-delta1) && age[i] < 9, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1981,1989)
  delta2  ~ dunif(1971,1979)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 36:40){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (1989-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (1989-delta2) && age[i] < 19, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (1989-delta1) && age[i] < 9, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1981,1989)
  delta2  ~ dunif(1971,1979)
  delta3  ~ dunif(1961,1969)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 36:40){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (1989-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (1989-delta3) && age[i] < 29, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (1989-delta2) && age[i] < 19, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (1989-delta1) && age[i] < 9, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1981,1989)
  delta2  ~ dunif(1971,1979)
  delta3  ~ dunif(1961,1969)
  delta4  ~ dunif(1951,1959)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4", "delta1", "delta2", "delta3", "delta4")

# 5 epidemic
jcode <- "model{ 
	for (i in 36:40){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (1989-delta5), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5)
       ,ifelse(age[i]> (1989-delta4) && age[i] < 39, 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (1989-delta3) && age[i] < 29, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (1989-delta2) && age[i] < 19, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (1989-delta1) && age[i] < 9, 1-exp(-lambda1),
       0)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1981,1989)
  delta2  ~ dunif(1971,1979)
  delta3  ~ dunif(1961,1969)
  delta4  ~ dunif(1951,1959)
  delta5  ~ dunif(1910,1959)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4","lambda5", "delta1", "delta2", "delta3", "delta4","delta5")


# Run model
mcmc.length=50000
jdat = list(n.pos= df_sa$N.pos,
            N=df_sa$N,
            age=df_sa$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVector, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

# Calculate DIC
dic.samples(jmod, n.iter = mcmc.length)

# credible interval
constantCR <- function(paramVector, mcmcMatrix) {
  
  ager = 0:80
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

# get quantile matrices function
quantmat <- function(outDf){
  ager = 0:80
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
plotfuncNone <- function(df_upperLower, study){
  g <- ggplot()+
    geom_line(data = df_upperLower, aes(x=agemid, y=mean), color = "#558C8C")+
    geom_ribbon(data = df_upperLower, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
    geom_point(data = study, aes(x=agemid, y=midpoint), color = "#558C8C")+  
    geom_linerange(data = study, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
    theme_bw()+
    theme(axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank())+
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    ylim(0, 1)+
    xlab(element_blank()) + 
    ylab(element_blank())+
    scale_fill_viridis(discrete = T)+
    facet_wrap(~ country, nrow = 1) 
  return(g)
}
plotfuncYonly <- function(df_upperLower, study){
  g <- ggplot()+
    geom_line(data = df_upperLower, aes(x=agemid, y=mean), color = "#558C8C")+
    geom_ribbon(data = df_upperLower, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
    geom_point(data = study, aes(x=agemid, y=midpoint), color = "#558C8C")+  
    geom_linerange(data = study, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
    theme_bw()+
    theme(axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank())+
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    ylim(0, 1)+
    xlab(element_blank()) + 
    ylab(element_blank())+
    scale_fill_viridis(discrete = T)+
    facet_wrap(~ country, nrow = 1) 
  return(g)
}
plotfuncXonly <- function(df_upperLower, study){
  g <- ggplot()+
    geom_line(data = df_upperLower, aes(x=agemid, y=mean), color = "#558C8C")+
    geom_ribbon(data = df_upperLower, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
    geom_point(data = study, aes(x=agemid, y=midpoint), color = "#558C8C")+  
    geom_linerange(data = study, aes(x=agemid, ymin=lower, ymax=upper), color = "#558C8C")+
    theme_bw()+
    theme(axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank())+
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    ylim(0, 1)+
    xlab(element_blank()) + 
    ylab(element_blank())+
    scale_fill_viridis(discrete = T)+
    facet_wrap(~ country, nrow = 1) 
  return(g)
}

##results computation
outDf197 <- Epi1CR(paramVector, mcmcMatrix, ager1 = 0:2, ager2 = 3:80)
df_upperLower197 <- quantmat(outDf197)
g1<- plot197 <- plotfuncYonly(df_upperLower197, study197)

outDf249 <- Epi3CR(paramVector, mcmcMatrix, ager1 = 0:2, ager2 = 3:10, ager3 = 11:24, ager4 = 25:80)
df_upperLower249 <- quantmat(outDf249)
g2<- plot249 <- plotfuncNone(df_upperLower249, study249)

outDf450_1 <- Epi3CR(paramVector, mcmcMatrix, ager1 = 0:1, ager2 = 2:11, ager3 = 12:21, ager4 = 22:80)
df_upperLower450_1 <- quantmat(outDf450_1)
g3<- plot450_1 <- plotfuncNone(df_upperLower450_1, study450_1)

outDf450_2 <- Epi3CR(paramVector, mcmcMatrix, ager1 = 0:2, ager2 = 3:12, ager3 = 13:22, ager4 = 23:80)
df_upperLower450_2 <- quantmat(outDf450_2)
g4<- plot450_2 <- plotfuncXY(df_upperLower450_2, study450_2)

outDf486 <- Epi3CR(paramVector, mcmcMatrix, ager1 = 0:1, ager2 = 2:11, ager3 = 12:21, ager4 = 22:80)
df_upperLower486 <- quantmat(outDf486)
g5<- plot486 <- plotfuncXonly(df_upperLower486, study486)


sa_graph<-  grid.arrange(g1, g2, g3, g4,
                         g5,  ncol=3,
                          left = "Proportion Seropositive",
                          bottom = "Age (years)",
                          top = "Seropositivity in SA (non-fever population based studies)")
ggsave("saAll.pdf", sa_graph, dpi=1000, device= "pdf", height=8, width=10,units="in", bg=NULL)

