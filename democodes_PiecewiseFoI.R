library(readxl)
require(tidyverse)
require(rjags)
require(binom)
library(ggplot2)
library(dplyr)
library(varhandle)
require(MCMCvis)
library(cowplot)
library(epimdr)

options(scipen=999)

##Read file
library(readxl)
chik_systematic_review_v1 <- read_excel("Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "prac")
View(chik_systematic_review_v1)

chik_systematic_review_v1 <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "prac")
#Rename data
df_chik = chik_systematic_review_v1 

df_chik <-  df_chik %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2) %>% filter(study == 4)

df_chik[,c("midpoint","lower","upper")] = binom.confint(df_chik$N.pos, df_chik$N, method="exact")[,c("mean","lower","upper")]


# Define model
cate <- c(1,20,40,60,80)
dur<-c(19,20,20,20)

# Piecewise constant FoI model (age-specific catalytic model)

jcode <- "model{ 
cate <- c(1,20,40,60,80)
dur<-c(19,20,20,20)
	for (i in 1:length(N)){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1- exp(-(lambda4*(age[4]-agemin[4]) + lambda3*dur[3] + lambda2*dur[2]+ lambda1*dur[1]))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1 ~ dunif(0,1)       #uninformative prior
  lambda2 ~ dunif(0,1)       #uninformative prior
  lambda3 ~ dunif(0,1)       #uninformative prior
  lambda4 ~ dunif(0,1)       #uninformative prior
}"

# Run model

paramVector <- c("lambda1", "lambda2", "lambda3", "lambda4")

mcmc.length=50000
jdat = list(n.pos  = df_chik$N.pos,
            N      = df_chik$N,
            age    = df_chik$agemid,
            agemin = df_chik$age_min)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=5, n.adapt = 15000)
update(jmod, 1000)
jpos = coda.samples(jmod, paramVector, n.iter=mcmc.length)
plot(jpos) # check convergence
mcmcMatrix <- as.matrix(jpos)

lambda1PointEst <- mcmcMatrix[,"lambda1"] %>% quantile(probs=c(.5,.025,.975))
lambda2PointEst <- mcmcMatrix[,"lambda2"] %>% quantile(probs=c(.5,.025,.975))
lambda3PointEst <- mcmcMatrix[,"lambda3"] %>% quantile(probs=c(.5,.025,.975))
lambda4PointEst <- mcmcMatrix[,"lambda4"] %>% quantile(probs=c(.5,.025,.975))
paramEstimates <- list(lambda1PointEst, lambda2PointEst, lambda3PointEst, lambda4PointEst)


# functions for integral
integrandpc = function(a, up, foi){
  #Find which interval a belongs to
  wh = findInterval(a, sort(c(0,up)))
  #Calculate duration of each interval
  dur = diff(sort(c(0,up)))
  #Evaluate integrand
  inte = ifelse(wh == 1, foi[1]*a, 
                sum(foi[1:(wh-1)]*dur[1:(wh-1)])+
                  foi[wh]*(a-up[wh-1]))
  return(inte)
}

llik.pc = function(par, age, num, denom, up) {
  ll = 0
  for (i in 1:length(age)) {
    p = 1 - exp(-integrandpc(a=age[i], up = up, 
                             foi = exp(par)))
    ll = ll + dbinom(num[i], denom[i], p, log = T)
  }
  return(-ll)
}

cate <- c(20,40,60,80)
para = rep(0.1, length(cate))


est = optim(par=log(para),fn=llik.pc, age=df_chik$agemid, 
            num=df_chik$N.pos,  denom=df_chik$N, up=cate, 
            method="Nelder-Mead", control=list(trace=2))

round(exp(est$par), 5)

#Make space for left and right axes
par(mar = c(5,5,2,5))
#Add beginning and ends to x and y for step plot
xvals=c(0,cate)
yvals=exp(c(est$par, est$par[3]))
plot(xvals, yvals, type="s", xlab="age", ylab="FoI")

#Superimpose predicted curve
par(new=T)
p = rep(0, 28)
for (i in 1:28) {
  p[i] = 1 - exp(-integrandpc(a=i, up = cate, 
                              foi = exp(est$par)))
}
plot(p~c(1:28), ylim=c(0,1), type="l", col="red", 
     axes=FALSE, xlab=NA, ylab=NA)

#Add right axis and legend
axis(side = 4)
mtext(side = 4, line = 4, "Prevalence")
legend("right", legend=c("FoI", "Prevalence"),
       lty=c(1,1), col=c("black", "red"))