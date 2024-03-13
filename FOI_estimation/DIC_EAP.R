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
View(CountryModel)

#Rename data
df_eap = CountryModel %>% filter(region == "EAP")

df_eap[,c("midpoint","lower","upper")] = binom.confint(df_eap$N.pos, df_eap$N, method="exact")[,c("mean","lower","upper")]

df_eap <-  df_eap %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

df_eap <- df_eap[df_eap$antibody != "IgM",] # remove data that has only IgM

study75      <-  df_eap %>% filter(study_no == 75)
study146     <-  df_eap %>% filter(study_no == 146)
study277     <-  df_eap %>% filter(study_no == 277)
study281     <-  df_eap %>% filter(study_no == 281)
study341     <-  df_eap %>% filter(study_no == 341)
study423     <-  df_eap %>% filter(study_no == 423)
study452     <-  df_eap %>% filter(study_no == 452)
study505_1   <-  df_eap %>% filter(study_no == 505 & year == "2013")
study505_2   <-  df_eap %>% filter(study_no == 505 & year == "2015")

# constant model: test DIC
jcode <- "model{ 
	for (i in 45:48){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# Test DIC: 75
# 1 epidemic
jcode <- "model{ 
	for (i in 5:8){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2008-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1965,2008) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 5:8){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2008-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2008-delta1) && age[i] < 44, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1965,2008)
  delta2  ~ dunif(1955,1986)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 5:8){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2008-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2008-delta2) && age[i] < 54, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2008-delta1) && age[i] < 44, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1965,2008)
  delta2  ~ dunif(1955,1986)
  delta3  ~ dunif(1955,1986)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 5:8){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2008-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2008-delta3) && age[i] < 64, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2008-delta2) && age[i] < 54, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2008-delta1) && age[i] < 44, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1965,2008)
  delta2  ~ dunif(1955,1986)
  delta3  ~ dunif(1945,1986)
  delta4  ~ dunif(1935,1986)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4", "delta1", "delta2", "delta3", "delta4")

# Test DIC: 146
# 1 epidemic
jcode <- "model{ 
	for (i in 13:16){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2015-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2011,2015) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 13:16){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 5, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2011,2015)
  delta2  ~ dunif(2001,2009)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 13:16){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2015-delta2) && age[i] < 15, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 5, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2011,2015)
  delta2  ~ dunif(2001,2009)
  delta3  ~ dunif(1971,1999)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 13:16){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2015-delta3) && age[i] < 45, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2015-delta2) && age[i] < 15, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 5, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2011,2015)
  delta2  ~ dunif(2001,2009)
  delta3  ~ dunif(1971,1999)
  delta4  ~ dunif(1936,1969)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4", "delta1", "delta2", "delta3", "delta4")

# Test DIC: 277
# 1 epidemic
jcode <- "model{ 
	for (i in 30:35){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2010-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1982,2010) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 30:35){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2010-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2010-delta1) && age[i] < 29, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1982,2010)
  delta2  ~ dunif(1972,1980)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 30:35){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2010-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2010-delta2) && age[i] < 39, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2010-delta1) && age[i] < 29, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1982,2010)
  delta2  ~ dunif(1972,1980)
  delta3  ~ dunif(1962,1970)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 30:35){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2010-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2010-delta3) && age[i] < 49, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2010-delta2) && age[i] < 39, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2010-delta1) && age[i] < 29, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1982,2010)
  delta2  ~ dunif(1972,1980)
  delta3  ~ dunif(1962,1970)
  delta4  ~ dunif(1952,1960)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4", "delta1", "delta2", "delta3", "delta4")

# Test DIC: 281
# 1 epidemic
jcode <- "model{ 
	for (i in 36:38){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2019-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1985,2019) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 36:38){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 35, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1985,2019)
  delta2  ~ dunif(1970,1983)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 36:38){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2019-delta2) && age[i] < 50, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 35, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1985,2019)
  delta2  ~ dunif(1970,1983)
  delta3  ~ dunif(1958,1968)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# Test DIC: 341
# 1 epidemic
jcode <- "model{ 
	for (i in 45:48){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2009-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1966,2009) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 45:48){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2009-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2009-delta1) && age[i] < 44, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1966,2009)
  delta2  ~ dunif(1956,1964)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 45:48){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2009-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2009-delta2) && age[i] < 54, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2009-delta1) && age[i] < 44, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1966,2009)
  delta2  ~ dunif(1956,1964)
  delta3  ~ dunif(1946,1954)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 45:48){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2009-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2009-delta3) && age[i] < 64, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2009-delta2) && age[i] < 54, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2009-delta1) && age[i] < 44, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1966,2009)
  delta2  ~ dunif(1956,1964)
  delta3  ~ dunif(1946,1954)
  delta4  ~ dunif(1936,1944)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4", "delta1", "delta2", "delta3", "delta4")

# Test DIC: 423
# 1 epidemic
jcode <- "model{ 
	for (i in 49:52){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2014-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2000,2014) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 49:52){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2014-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2014-delta1) && age[i] < 15, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2000,2014)
  delta2  ~ dunif(1985,1999)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 49:52){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2014-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2014-delta2) && age[i] < 30, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2014-delta1) && age[i] < 15, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2000,2014)
  delta2  ~ dunif(1985,1999)
  delta3  ~ dunif(1970,1983)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 49:52){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2014-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2014-delta3) && age[i] < 45, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2014-delta2) && age[i] < 30, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2014-delta1) && age[i] < 15, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2000,2014)
  delta2  ~ dunif(1985,1999)
  delta3  ~ dunif(1970,1983)
  delta4  ~ dunif(1935,1968)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4", "delta1", "delta2", "delta3", "delta4")

# Test DIC: 452
# 1 epidemic
jcode <- "model{ 
	for (i in 61:63){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2007-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1990,2007) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 61:63){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2007-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2007-delta1) && age[i] < 18, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1990,2007)
  delta2  ~ dunif(1959,1988)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 61:63){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2007-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2007-delta2) && age[i] < 49, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2007-delta1) && age[i] < 18, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1990,2007)
  delta2  ~ dunif(1959,1988)
  delta3  ~ dunif(1928,1957)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# Test DIC: 505_1
# 1 epidemic
jcode <- "model{ 
	for (i in 68:71){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2013-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1995,2013) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 64:67){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2013-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2013-delta1) && age[i] < 19, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1995,2013)
  delta2  ~ dunif(1975,1993)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 64:67){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2013-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2013-delta2) && age[i] < 39, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2013-delta1) && age[i] < 19, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1995,2013)
  delta2  ~ dunif(1975,1993)
  delta3  ~ dunif(1955,1973)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 64:67){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2013-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2013-delta3) && age[i] < 59, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2013-delta2) && age[i] < 39, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2013-delta1) && age[i] < 19, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1995,2013)
  delta2  ~ dunif(1975,1993)
  delta3  ~ dunif(1955,1973)
  delta4  ~ dunif(1929,1953)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4", "delta1", "delta2", "delta3", "delta4")

# Test DIC: 505_2
# 1 epidemic
jcode <- "model{ 
	for (i in 68:71){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2015-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1997,2015) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 68:71){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 19, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(1997,2015)   #uninformative prior
  delta2   ~ dunif(1977,1995)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 68:71){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2015-delta2) && age[i] < 39, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 19, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(1997,2015)   #uninformative prior
  delta2   ~ dunif(1977,1995)
  delta3   ~ dunif(1957,1975)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 68:71){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2015-delta3) && age[i] < 59, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2015-delta2) && age[i] < 39, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 19, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(1997,2015)   #uninformative prior
  delta2   ~ dunif(1977,1995)
  delta3   ~ dunif(1957,1975)
  delta4   ~ dunif(1936,1955)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4", "delta1", "delta2", "delta3", "delta4")


# Run model
mcmc.length=50000
jdat = list(n.pos= df_eap$N.pos,
            N=df_eap$N,
            age=df_eap$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=4, n.adapt = 15000)
update(jmod, 500)
jpos = coda.samples(jmod, paramVector, n.iter=mcmc.length)

summary(jpos) 
mcmcMatrix <- as.matrix(jpos)

# Calculate DIC
dic.samples(jmod, n.iter = mcmc.length)

#### Credible interval functions################################################

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

#ggplot function 1) y axis only 
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
#ggplot function 1) x axis only 
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


################################################################################

##results computation
outDf75 <- constantCR(paramVector, mcmcMatrix)
df_upperLower75 <- quantmat(outDf75)
g1<- plot75 <- plotfuncYonly(df_upperLower75, study75)

outDf341 <- constantCR(paramVector, mcmcMatrix)
df_upperLower341 <- quantmat(outDf341)
g2<- plot314 <- plotfuncNone(df_upperLower341, study341)

outDf281 <- Epi1CR(paramVector, mcmcMatrix, ager1= 0:13, ager2 =14:80)
df_upperLower281 <- quantmat(outDf281)
g3<- plot281 <- plotfuncNone(df_upperLower281, study281)

outDf452 <- Epi1CR(paramVector, mcmcMatrix, ager1= 0:4, ager2 =5:80)
df_upperLower452 <- quantmat(outDf452)
g4<- plot452 <- plotfuncYonly(df_upperLower452, study452)

outDf505_1 <- Epi1CR(paramVector, mcmcMatrix, ager1= 0:5, ager2 =6:80)
df_upperLower505_1 <- quantmat(outDf505_1)
g5<- plot505_1 <- plotfuncNone(df_upperLower505_1, study505_1)

outDf505_2 <- Epi1CR(paramVector, mcmcMatrix, ager1= 0:5, ager2 =6:80)
df_upperLower505_2 <- quantmat(outDf505_2)
g6<- plot505_2 <- plotfuncNone(df_upperLower505_2, study505_2)

outDf277 <- Epi2CR(paramVector, mcmcMatrix, ager1= 0:11, ager2 =12:40, ager3 = 41:80)
df_upperLower277 <- quantmat(outDf277)
g7<- plot277 <- plotfuncXY(df_upperLower277, study277)

outDf146 <- Epi4CR(paramVector, mcmcMatrix, ager1= 0:0, ager2 =1:7, ager3 = 8:19, ager4 = 20:50, ager5=51:80)
df_upperLower146 <- quantmat(outDf146)
g8<- plot146 <- plotfuncXonly(df_upperLower146, study146)

outDf423 <- Epi4CR(paramVector, mcmcMatrix, ager1= 0:3, ager2 =4:18, ager3 = 19:34, ager4 = 35:54, ager5=55:80)
df_upperLower423 <- quantmat(outDf423)
g9<- plot423 <- plotfuncXonly(df_upperLower423, study423)

eap_graph<-  grid.arrange(g1, g2, g3, g4,
                          g5, g6, g7, g8,
                          g9, ncol=3,
                          left = "Proportion Seropositive",
                          bottom = "Age (years)",
                          top = "Seropositivity in EAP (non-fever population based studies)")
ggsave("eapAll.pdf", eap_graph, dpi=1000, device= "pdf", height=8, width=10,units="in", bg=NULL)

###FOI graph
plotFOIXY <- function(foimatrix){
  g <- ggplot(foimatrix, aes(x = Year, ymin = lo, ymax = hi, y = FOI))+
    geom_ribbon(alpha= 0.2, fill = "#C05746")+
    geom_line(color = "#C05746")+
    theme_bw()+
    theme(axis.text.y = element_text(angle = 45, hjust = 1, size = 8)) +
    ylab(element_blank())+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    facet_wrap(~ country, nrow = 1) 
  return(g)
}

plotFOIXOnly <- function(foimatrix){
  g <- ggplot(foimatrix, aes(x = Year, ymin = lo, ymax = hi, y = FOI))+
    geom_ribbon(alpha= 0.2, fill = "#C05746")+
    geom_line(color = "#C05746")+
    theme_bw()+
    theme(axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank())+
    ylab(element_blank())+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    facet_wrap(~ country, nrow = 1) 
  return(g)
}

# study75
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat1 <- as.data.frame(matrix(NA, nrow = 2, ncol = 4))
colnames(foimat1) <- c("Year", "FOI", "lo", "hi")
foimat1[,1] <- c(1934, 2008)
foimat1[1:2,2] <- quantile(mcmcMatrix$lambda, c(0.5))
foimat1[1:2,3] <- quantile(mcmcMatrix$lambda, c(0.025))
foimat1[1:2,4] <- quantile(mcmcMatrix$lambda, c(0.975))
foimat1$country <- c("Malaysia")

#study341
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat2 <- as.data.frame(matrix(NA, nrow = 2, ncol = 4))
colnames(foimat2) <- c("Year", "FOI", "lo", "hi")
foimat2[,1] <- c(1935, 2009)
foimat2[1:2,2] <- quantile(mcmcMatrix$lambda, c(0.5))
foimat2[1:2,3] <- quantile(mcmcMatrix$lambda, c(0.025))
foimat2[1:2,4] <- quantile(mcmcMatrix$lambda, c(0.975))
foimat2$country <- c("Malaysia")

#study 281
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat3 <- as.data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(foimat3) <- c("Year", "FOI", "lo", "hi")
foimat3[,1] <- c(1957, 2005, 2005, 2019)
foimat3[1:2,2] <- quantile(mcmcMatrix$lambda, c(0.5))
foimat3[1:2,3] <- quantile(mcmcMatrix$lambda, c(0.025))
foimat3[1:2,4] <- quantile(mcmcMatrix$lambda, c(0.975))
foimat3[3:4,2:4] <- 0
foimat3$country <- c("Papua New Guinea")

#study 452
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat4 <- as.data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(foimat4) <- c("Year", "FOI", "lo", "hi")
foimat4[,1] <- c(1927, 2002, 2002, 2007)
foimat4[1:2,2] <- quantile(mcmcMatrix$lambda, c(0.5))
foimat4[1:2,3] <- quantile(mcmcMatrix$lambda, c(0.025))
foimat4[1:2,4] <- quantile(mcmcMatrix$lambda, c(0.975))
foimat4[3:4,2:4] <- 0
foimat4$country <- c("Thailand")

#study 505_1
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat5 <- as.data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(foimat5) <- c("Year", "FOI", "lo", "hi")
foimat5[,1] <- c(1928, 2007, 2007, 2013)
foimat5[1:2,2] <- quantile(mcmcMatrix$lambda, c(0.5))
foimat5[1:2,3] <- quantile(mcmcMatrix$lambda, c(0.025))
foimat5[1:2,4] <- quantile(mcmcMatrix$lambda, c(0.975))
foimat5[3:4,2:4] <- 0
foimat5$country <- c("Fiji")

#study 505_2
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat6 <- as.data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(foimat6) <- c("Year", "FOI", "lo", "hi")
foimat6[,1] <- c(1935, 2006, 2006, 2015)
foimat6[1:2,2] <- quantile(mcmcMatrix$lambda, c(0.5))
foimat6[1:2,3] <- quantile(mcmcMatrix$lambda, c(0.025))
foimat6[1:2,4] <- quantile(mcmcMatrix$lambda, c(0.975))
foimat6[3:4,2:4] <- 0
foimat6$country <- c("Fiji")

# study 277
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat7 <- as.data.frame(matrix(NA, nrow = 6, ncol = 4))
colnames(foimat7) <- c("Year", "FOI", "lo", "hi")
foimat7[,1] <- c(1931, 1977, 1977, 1992, 1992, 2010)
foimat7[1:2,2] <- quantile(mcmcMatrix$lambda1, c(0.5))
foimat7[1:2,3] <- quantile(mcmcMatrix$lambda1, c(0.025))
foimat7[1:2,4] <- quantile(mcmcMatrix$lambda1, c(0.975))

foimat7[3:4,2] <- quantile(mcmcMatrix$lambda2, c(0.5))
foimat7[3:4,3] <- quantile(mcmcMatrix$lambda2, c(0.025))
foimat7[3:5,4] <- quantile(mcmcMatrix$lambda2, c(0.975))

foimat7[5:6,2:4] <- 0
foimat7$country <- c("Singapore")

#study146
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat8 <- as.data.frame(matrix(NA, nrow = 10, ncol = 4))
colnames(foimat8) <- c("Year", "FOI", "lo", "hi")
foimat8[,1] <- c(1935, 1961, 1961, 1992, 1992, 2007, 2007, 2014, 2014, 2015)
foimat8[1:2,2] <- quantile(mcmcMatrix$lambda1, c(0.5))
foimat8[1:2,3] <- quantile(mcmcMatrix$lambda1, c(0.025))
foimat8[1:2,4] <- quantile(mcmcMatrix$lambda1, c(0.975))

foimat8[3:4,2] <- quantile(mcmcMatrix$lambda2, c(0.5))
foimat8[3:4,3] <- quantile(mcmcMatrix$lambda2, c(0.025))
foimat8[3:4,4] <- quantile(mcmcMatrix$lambda2, c(0.975))

foimat8[5:6,2] <- quantile(mcmcMatrix$lambda3, c(0.5))
foimat8[5:6,3] <- quantile(mcmcMatrix$lambda3, c(0.025))
foimat8[5:6,4] <- quantile(mcmcMatrix$lambda3, c(0.975))

foimat8[7:8,2] <- quantile(mcmcMatrix$lambda4, c(0.5))
foimat8[7:8,3] <- quantile(mcmcMatrix$lambda4, c(0.025))
foimat8[7:8,4] <- quantile(mcmcMatrix$lambda4, c(0.975))

foimat8[9:10,2:4] <- 0
foimat8$country <- c("Myanmar")

#study423
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat9 <- as.data.frame(matrix(NA, nrow = 10, ncol = 4))
colnames(foimat9) <- c("Year", "FOI", "lo", "hi")
foimat9[,1] <- c(1934, 1960, 1960, 1980, 1980, 1995, 1995, 2010, 2010, 2014)
foimat9[1:2,2] <- quantile(mcmcMatrix$lambda1, c(0.5))
foimat9[1:2,3] <- quantile(mcmcMatrix$lambda1, c(0.025))
foimat9[1:2,4] <- quantile(mcmcMatrix$lambda1, c(0.975))

foimat9[3:4,2] <- quantile(mcmcMatrix$lambda2, c(0.5))
foimat9[3:4,3] <- quantile(mcmcMatrix$lambda2, c(0.025))
foimat9[3:4,4] <- quantile(mcmcMatrix$lambda2, c(0.975))

foimat9[5:6,2] <- quantile(mcmcMatrix$lambda3, c(0.5))
foimat9[5:6,3] <- quantile(mcmcMatrix$lambda3, c(0.025))
foimat9[5:6,4] <- quantile(mcmcMatrix$lambda3, c(0.975))

foimat9[7:8,2] <- quantile(mcmcMatrix$lambda4, c(0.5))
foimat9[7:8,3] <- quantile(mcmcMatrix$lambda4, c(0.025))
foimat9[7:8,4] <- quantile(mcmcMatrix$lambda4, c(0.975))

foimat9[9:10,2:4] <- 0
foimat9$country <- c("Thailand")

g1 <- plotFOIXY(foimat1)
g2 <- plotFOIXOnly(foimat2)
g3 <- plotFOIXOnly(foimat3)
g4 <- plotFOIXY(foimat4)
g5 <- plotFOIXOnly(foimat5)
g6 <- plotFOIXOnly(foimat6)
g7 <- plotFOIXY(foimat7)
g8 <- plotFOIXOnly(foimat8)
g9 <- plotFOIXOnly(foimat9)

eap_foi<-  grid.arrange(g1, g2, g3, g4,
                        g5, g6, g7, g8,
                        g9, g10, g11,g12, ncol=3,
                        left = "FoI",
                        bottom = "Year",
                        top = "FoI in EAP (non-fever population based studies)")
ggsave("eapFOI.pdf", eap_foi, dpi=1000, device= "pdf", height=8, width=10,units="in", bg=NULL)

