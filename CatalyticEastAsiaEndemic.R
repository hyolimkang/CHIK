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
