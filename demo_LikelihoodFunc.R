# likelihood function - to get optimal FoI
library(readxl)
chik_systematic_review_v1 <- read_excel("Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "prac")
View(chik_systematic_review_v1)

#Rename data
df_chik = chik_systematic_review_v1 

df_chik <-  df_chik %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2) %>% filter(study == 4)

df_chik[,c("midpoint","lower","upper")] = binom.confint(df_chik$N.pos, df_chik$N, method="exact")[,c("mean","lower","upper")]

### data processing
data_processing <- function(data){
  
  # new dataframe for proportions seropositive by age
  new_data <- data.frame(Age=seq(1:80), AgeMid=seq(1:80)+0.5,
                         PosData=NA, TotData=NA, serop=NA, lower_ci=NA, upper_ci=NA)
  
  # for each age 
  for(a in unique(data$age)){
    
    age <- data[data$age==a, ] # subset by age
    new_data$PosData[a] <- sum(age$sero) # number positive aged a
    new_data$TotData[a] <- nrow(age) # total number aged a
    
    # if there are no samples from a single year of age replace Na with 0
    new_data$TotData[which(is.na(new_data$TotData)==TRUE)] <- 0
    new_data$PosData[which(is.na(new_data$PosData)==TRUE)] <- 0
    
    # seroprevalence
    new_data$serop <- new_data$PosData/new_data$TotData
    
    # confidence intervals for age-specific seroprevalence
    if(new_data$TotData[a]>0){
      ci <- binom.exact(new_data$PosData[a], new_data$TotData[a], conf.level=0.95)
      new_data$lower_ci[a] <- ci$lower
      new_data$upper_ci[a] <- ci$upper
    }
  }
  
  # remove rows where there were no samples (there are only a couple)
  new_data <- new_data[(is.na(new_data$serop)==FALSE), ]
  loc.all<- data.frame("0"=(new_data$TotData-new_data$PosData), "1"=new_data$PosData,  "Age"=new_data$Age+0.5)
  return(loc.all)
  
}

# liklihood
likeli.cons<- function(theta, data, npar) {
  
  ##### Get relevant data
  shift <- min(data$Age)
  age<-(1:(max(as.numeric(dimnames(data)[[1]]))))+0.5
  age.dat<-age
  
  nyk<-data[,which(dimnames(data)[[2]]=="X1")] #SEROPOSITIVE
  N<-data[,which(dimnames(data)[[2]]=="X0")]+nyk
  nxk<-N-nyk #SERONEGATIVE
  
  #### Create a vector (or matrix) for your lambdas. This npar=1 for constant foi, npar>1 for time varying foi (divided equally in npar parts)	
  if (npar==1) {
    lambda<-matrix(theta, nrow=length(age), ncol=1)
    lambda[1,]<-theta*shift #To correct for the fact that this is the floor of the ages 
  }
  
  if (npar>1) {
    cut.ages<-1:max(age)
    lambda<-matrix(NA, nrow=length(age), ncol=1)
    
    lambda[,1] <-theta[cut.ages]
    lambda[1,]<-theta[1]*shift
  }
  
  #####Compute cumulative FOI experienced up to age a (cum.lambda)
  xaprod2<-cumsum(lambda)
  
  x<-exp(-xaprod2)	 #Proportion that remains susceptible at age a
  
  # Get proportions susceptible for observed age groups
  x.lik<-x
  y.lik<-1-x.lik
  
  #Add the log likelihood terms. This is a binomial likelihood of the form x^nxk * (y)^(N-nxk)
  lognxk<-nxk*log(x.lik) 
  lognyk<-(N-nxk)*log(y.lik)
  logterms<-lognxk+lognyk
  #Sum the two log likelihoods
  minuslogl<- -sum(logterms)
  return(minuslogl)
}



