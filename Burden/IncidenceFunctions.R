# getting susceptible population from seroprevalence 

# getting incidence rates and number of new cases

#model
incidenceFunction <- function(outDf) {
  IncDf <- matrix(NA, nrow=numSamples, ncol=length(ager))
  for(i in 1:ncol(outDf)){       # for-loop over columns
    if(i < 80) {
      IncDf[, i] <- (outDf[,i+1]-outDf[,i])/(1-outDf[,i])
    }else{
      IncDf[, i] <- IncDf[,i-1]
    }
  }
  return(IncDf)
}
incidenceFunction(outDf)


NewDf <- matrix(NA, nrow=numSamples, ncol=length(ager))

for(i in 1:ncol(outDf)){       # for-loop over columns
  if(i < 80) {
    NewDf[, i] <-(outDf[,i+1]-outDf[,i])/(1-outDf[,i])
  }else{
    NewDf[, i] <- NewDf[,i-1]
  }
}

IncidenceCalcFunction <- function(outDf) {
  NewDf <- matrix(NA, nrow=numSamples, ncol=length(ager))
  a <- c(1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76)
  for(i in a) {
    NewDf[, i] <-1-((1-outDf[,i+4])/(1-outDf[,i])^(1/4))
    NewDf[, i+1] <- NewDf[, i]
    NewDf[, i+2] <- NewDf[, i]
    NewDf[, i+3] <- NewDf[, i]
    NewDf[, i+4] <- NewDf[, i]
  }
  NewDf
}
IncidenceCalcFunction(outDf)

samp <- matrix(1, nrow=numSamples, ncol=length(ager))
sprop  <- samp - outDf

library(readxl)
WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/code/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx", 
                                                                 sheet = "haiti2021")
View(WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES)

pop<-WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES
pop <- pop[ -c(1) ]

Susceptible <- sprop * pop
NewCases    <- NewDf * Susceptible


quantileMatrix <- matrix(NA,nrow=ncol(outDf), ncol = 3)
for(jj in 1:ncol(outDf)){
  quantiles <- sprop[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrix[jj,] <- quantiles
  df_upperLower <- cbind(ager, quantileMatrix)
  df_upperLower <- as.data.frame(df_upperLower)
  colnames(df_upperLower) <- c('agemid', 'mean', 'upper', 'lower')
  
}

quantileMatrixInc <- matrix(NA,nrow=ncol(outDf), ncol = 3)
for(jj in 1:ncol(outDf)){
  quantiles <- NewCases[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrixInc[jj,] <- quantiles
  df_upperLower_Inc <- cbind(ager, quantileMatrixInc)
  df_upperLower_Inc <- as.data.frame(df_upperLower_Inc)
  colnames(df_upperLower_Inc) <- c('agemid', 'mean', 'upper', 'lower')
  
}


# quantile matrix for susceptible pop
quantileMatrixSus <- matrix(NA,nrow=ncol(outDf), ncol = 3)
for(jj in 1:ncol(outDf)){
  quantiles <- Susceptible[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrixSus[jj,] <- quantiles
  df_upperLowerSus <- cbind(ager, quantileMatrixSus)
  df_upperLowerSus <- as.data.frame(df_upperLowerSus)
  colnames(df_upperLowerSus) <- c('agemid', 'mean', 'upper', 'lower')
}

infectious <- data.frame(1 - df_upperLower$mean)

colnames(df_upperLower) <- c('Age', 'susceptible', 'upper', 'lower', 'infectious')

plot_ly(data=df_upperLower, x = ~Age, y = ~susceptible, type = 'bar', name = 'Susceptible') %>%
  add_trace(y = ~infectious, name = 'Infectious') %>%
  layout(yaxis = list(title = 'Proportion of Susceptible'), barmode = 'stack')

plot_ly(data=df_upperLowerSus, x = ~agemid, y = ~mean, type = 'bar', name = 'Susceptible') %>%
  layout(yaxis = list(title = 'N of Susceptible'), barmode = 'stack')

plot_ly(data=df_upperLower_Inc, x = ~agemid, y = ~mean, type = 'bar', name = 'Susceptible') %>%
  layout(yaxis = list(title = 'N of new cases'), barmode = 'stack')


## Number of cases in the pre-vaccination scenario

PreVaccCases  <- function(numSamples, ager, pop)

  CaseDf <- matrix(NA,nrow=numSamples, ncol = length(ager))  
  for (i in 1:numSamples){
  randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
  lambdaSample <- mcmcMatrix[randomNumber,1]
  CaseRow      <- (1-exp(-lambdaSample))*(exp(-lambdaSample*ager))*pop
  CaseDf       <- data.frame(CaseRow)
  }
  
quantileMatrixCase <- matrix(NA,nrow=ncol(CaseDf), ncol = 3)
for(jj in 1:ncol(CaseDf)){
  quantiles <- CaseDf[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantileMatrixCase[jj,] <- quantiles
  df_upperLower_Case <- cbind(ager, quantileMatrixCase)
  df_upperLower_Case <- as.data.frame(df_upperLower_Case)
  colnames(df_upperLower_Case) <- c('agemid', 'mean', 'upper', 'lower')
}

plot_ly(data=df_upperLower_Case, x = ~agemid, y = ~mean, type = 'bar', name = 'Susceptible') %>%
  layout(yaxis = list(title = 'N of new cases'), barmode = 'stack')

ggplot(data = world) +
  geom_sf()
