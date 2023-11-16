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
df_ssa = CountryModel %>% filter(region == "SSA")

df_ssa[,c("midpoint","lower","upper")] = binom.confint(df_ssa$N.pos, df_ssa$N, method="exact")[,c("mean","lower","upper")]

df_ssa <-  df_ssa %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

df_ssa <- df_ssa[df_ssa$antibody != "IgM",] # remove data that has only IgM

study7       <-  df_ssa %>% filter(study_no == 7)
study7       <-  study7[1:3,]
study17      <-  df_ssa %>% filter(study_no == 17)
study106     <-  df_ssa %>% filter(study_no == 106)
study132     <-  df_ssa %>% filter(study_no == 132)
study136     <-  df_ssa %>% filter(study_no == 136)
study149     <-  df_ssa %>% filter(study_no == 149)
study170     <-  df_ssa %>% filter(study_no == 170)
study204_1   <-  df_ssa %>% filter(study_no == 204 & country == "Burkina Faso")
study204_2   <-  df_ssa %>% filter(study_no == 204 & country == "Gabon")
study236     <-  df_ssa %>% filter(study_no == 236)
study237     <-  df_ssa %>% filter(study_no == 237)
study279     <-  df_ssa %>% filter(study_no == 279)
study306     <-  df_ssa %>% filter(study_no == 306)
study321     <-  df_ssa %>% filter(study_no == 321)
study494     <-  df_ssa %>% filter(study_no == 494)

# constant model: test DIC
jcode <- "model{ 
	for (i in 105:111){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")
                 
# 1-epidemic model: test DIC
# 7
jcode <- "model{ 
	for (i in 1:3){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2007-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1988,2007) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 17
jcode <- "model{ 
	for (i in 5:9){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2012-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2003,2012) #uninformative prior
}"
paramVector <- c("lambda", "delta")
# 106
jcode <- "model{ 
	for (i in 21:24){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2016-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1993,2016) #uninformative prior
}"
paramVector <- c("lambda", "delta")
# 149
jcode <- "model{ 
	for (i in 43:47){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2018-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2009,2018) #uninformative prior
}"
paramVector <- c("lambda", "delta")
# 170
jcode <- "model{ 
	for (i in 48:50){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2019-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1990,2019) #uninformative prior
}"
paramVector <- c("lambda", "delta")
# 204_1
jcode <- "model{ 
	for (i in 55:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2015-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2012,2015) #uninformative prior
}"
paramVector <- c("lambda", "delta")
# 204_2
jcode <- "model{ 
	for (i in 66:76){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2015-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2012,2015) #uninformative prior
}"
paramVector <- c("lambda", "delta")
# 236
jcode <- "model{ 
	for (i in 84:89){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2005-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1992,2005) #uninformative prior
}"
paramVector <- c("lambda", "delta")
# 237
jcode <- "model{ 
	for (i in 90:94){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2004-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2001,2004) #uninformative prior
}"
paramVector <- c("lambda", "delta")
# 279
jcode <- "model{ 
	for (i in 95:99){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2007-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1994,2007) #uninformative prior
}"
paramVector <- c("lambda", "delta")
# 306
jcode <- "model{ 
	for (i in 104:111){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2014-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2010,2014) #uninformative prior
}"
paramVector <- c("lambda", "delta")
# 494
jcode <- "model{ 
	for (i in 141:144){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2017-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2016,2017) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2-epidemics model DIC
# 7
jcode <- "model{ 
	for (i in 1:3){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2007-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2007-delta1) && age[i] < 20, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1988,2007)
  delta2  ~ dunif(1978,1986)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")
# 17
jcode <- "model{ 
	for (i in 5:9){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2012-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2012-delta1) && age[i] < 10, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2003,2012)
  delta2  ~ dunif(1993,2002)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")
# 106
jcode <- "model{ 
	for (i in 21:24){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2016-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2016-delta1) && age[i] < 24, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1993,2016)
  delta2  ~ dunif(1982,1992)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 149
jcode <- "model{ 
	for (i in 43:47){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2018-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2018-delta1) && age[i] < 10, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2009,2018)
  delta2  ~ dunif(1999,2007)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 170
jcode <- "model{ 
	for (i in 48:50){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2018-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2018-delta1) && age[i] < 30, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1989,2018)
  delta2  ~ dunif(1979,1987)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 204_1
jcode <- "model{ 
	for (i in 55:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 204_2
jcode <- "model{ 
	for (i in 66:76){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 236
jcode <- "model{ 
	for (i in 84:89){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2005-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2005-delta1) && age[i] < 14, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1992,2005)
  delta2  ~ dunif(1982,1990)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 237
jcode <- "model{ 
	for (i in 90:94){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2004-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2004-delta1) && age[i] < 4, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2002,2005)
  delta2  ~ dunif(1991,2000)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 279
jcode <- "model{ 
	for (i in 95:99){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2007-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2007-delta1) && age[i] < 14, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1994,2007)
  delta2  ~ dunif(1979,1992)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 306
jcode <- "model{ 
	for (i in 104:110){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2014-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2014-delta1) && age[i] < 4, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2011,2014)
  delta2  ~ dunif(2005,2009)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 494
jcode <- "model{ 
	for (i in 141:144){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2017-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2017-delta1) && age[i] < 2, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2016,2017)
  delta2  ~ dunif(2007,2014)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3-epidemics model DIC
# 7
jcode <- "model{ 
	for (i in 1:3){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2007-delta3), 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2007-delta2) && age[i] < 30, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2007-delta1) && age[i] < 20, 1-exp(-lambda1),
        0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1988,2007)
  delta2  ~ dunif(1978,1986)
  delta3  ~ dunif(1966,1976)
}"
paramVector <- c("lambda1","lambda2","lambda3", "delta1", "delta2", "delta3")

# 17
jcode <- "model{ 
	for (i in 5:9){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2012-delta3), 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2012-delta2) && age[i] < 20, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2012-delta1) && age[i] < 10, 1-exp(-lambda1),
        0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2003,2012)
  delta2  ~ dunif(1993,2002)
  delta3  ~ dunif(1973,1992)
}"
paramVector <- c("lambda1","lambda2","lambda3", "delta1", "delta2", "delta3")

# 106
jcode <- "model{ 
	for (i in 21:24){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2016-delta3), 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2016-delta2) && age[i] < 35, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2016-delta1) && age[i] < 24, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1993,2016)
  delta2  ~ dunif(1982,1992)
  delta3  ~ dunif(1972,1980)
  
}"
paramVector <- c("lambda1","lambda2","lambda3", "delta1", "delta2", "delta3")

# 149
jcode <- "model{ 
	for (i in 43:47){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2018-delta3), 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2018-delta2) && age[i] < 20, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2018-delta1) && age[i] < 10, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2009,2018)
  delta2  ~ dunif(1999,2007)
  delta3  ~ dunif(1984,1997)
  
}"
paramVector <- c("lambda1","lambda2","lambda3", "delta1", "delta2", "delta3")

# 204_1
jcode <- "model{ 
	for (i in 55:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta3), 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  
}"
paramVector <- c("lambda1","lambda2","lambda3", "delta1", "delta2", "delta3")

# 204_2
jcode <- "model{ 
	for (i in 66:76){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta3), 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  
}"
paramVector <- c("lambda1","lambda2","lambda3", "delta1", "delta2", "delta3")

# 236
jcode <- "model{ 
	for (i in 84:89){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2005-delta3), 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2005-delta2) && age[i] < 24, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2005-delta1) && age[i] < 14, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1992,2005)
  delta2  ~ dunif(1982,1990)
  delta3  ~ dunif(1972,1980)
  
}"
paramVector <- c("lambda1","lambda2","lambda3", "delta1", "delta2", "delta3")

# 237
jcode <- "model{ 
	for (i in 90:94){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2004-delta3), 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2004-delta2) && age[i] < 14, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2004-delta1) && age[i] < 4, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2001,2004)
  delta2  ~ dunif(1991,1999)
  delta3  ~ dunif(1981,1989)
  
}"
paramVector <- c("lambda1","lambda2","lambda3", "delta1", "delta2", "delta3")

# 279
jcode <- "model{ 
	for (i in 95:99){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2007-delta3), 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2007-delta2) && age[i] < 29, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2007-delta1) && age[i] < 14, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1994,2007)
  delta2  ~ dunif(1979,1992)
  delta3  ~ dunif(1964,1977)
  
}"
paramVector <- c("lambda1","lambda2","lambda3", "delta1", "delta2", "delta3")

# 306
jcode <- "model{ 
	for (i in 104:111){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2014-delta3), 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2014-delta2) && age[i] < 10, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2014-delta1) && age[i] < 4, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2011,2014)
  delta2  ~ dunif(2005,2009)
  delta3  ~ dunif(2000,2003)
  
}"
paramVector <- c("lambda1","lambda2","lambda3", "delta1", "delta2", "delta3")

# 494
jcode <- "model{ 
	for (i in 141:144){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2017-delta3), 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2017-delta2) && age[i] < 11, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2017-delta1) && age[i] < 2, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2016,2017)
  delta2  ~ dunif(2007,2014)
  delta3  ~ dunif(2001,2005)
  
}"
paramVector <- c("lambda1","lambda2","lambda3", "delta1", "delta2", "delta3")

## 4-epidemic DIC test

# 17
jcode <- "model{ 
	for (i in 5:9){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2012-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2012-delta3) && age[i] < 40, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2012-delta2) && age[i] < 20, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2012-delta1) && age[i] < 10, 1-exp(-lambda1),
        0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2003,2012)
  delta2  ~ dunif(1993,2002)
  delta3  ~ dunif(1973,1992)
  delta4  ~ dunif(1953,1972)
  
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","delta1", "delta2", "delta3",
                     "delta4")
# 106
jcode <- "model{ 
	for (i in 21:24){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2016-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2016-delta3) && age[i] < 45, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2016-delta2) && age[i] < 35, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2016-delta1) && age[i] < 24, 1-exp(-lambda1),
        0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1993,2016)
  delta2  ~ dunif(1982,1992)
  delta3  ~ dunif(1972,1980)
  delta4  ~ dunif(1937,1970)
  
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","delta1", "delta2", "delta3",
                    "delta4")

# 149
jcode <- "model{ 
	for (i in 43:47){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2018-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2018-delta3) && age[i] < 35, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2018-delta2) && age[i] < 20, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2018-delta1) && age[i] < 10, 1-exp(-lambda1),
        0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2009,2018)
  delta2  ~ dunif(1999,2007)
  delta3  ~ dunif(1984,1997)
  delta4  ~ dunif(1964,1982)
  
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","delta1", "delta2", "delta3",
                    "delta4")

# study 237
jcode <- "model{ 
	for (i in 90:94){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2004-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2004-delta3) && age[i] < 24, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2004-delta2) && age[i] < 14, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2004-delta1) && age[i] < 4, 1-exp(-lambda1),
        0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2001,2004)
  delta2  ~ dunif(1991,1999)
  delta3  ~ dunif(1981,1989)
  delta4  ~ dunif(1956,1979)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","delta1", "delta2", "delta3",
                 "delta4")


# 204_1
jcode <- "model{ 
	for (i in 55:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
        0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  delta4  ~ dunif(1997,2000)
  
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","delta1", "delta2", "delta3",
                    "delta4")

jcode <- "model{ 
	for (i in 55:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta5), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2015-delta4) && age[i] < 19, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
        0)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  delta4  ~ dunif(1997,2000)
  delta5  ~ dunif(1992,1995)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","delta1", "delta2", "delta3",
                    "delta4","delta5")
jcode <- "model{ 
	for (i in 55:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta6), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6),
        ifelse(age[i]> (2015-delta5) && age[i] < 24, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2015-delta4) && age[i] < 19, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
        0))))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  lambda6  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  delta4  ~ dunif(1997,2000)
  delta5  ~ dunif(1992,1995)
  delta6  ~ dunif(1987,1990)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","lambda6","delta1", "delta2", "delta3",
                    "delta4","delta5","delta6")
jcode <- "model{ 
	for (i in 55:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta7), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7),
        ifelse(age[i]> (2015-delta6) && age[i] < 29, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6),
        ifelse(age[i]> (2015-delta5) && age[i] < 24, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2015-delta4) && age[i] < 19, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
        0)))))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  lambda6  ~ dunif(0,1)       #uninformative prior
  lambda7  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  delta4  ~ dunif(1997,2000)
  delta5  ~ dunif(1992,1995)
  delta6  ~ dunif(1987,1990)
  delta7  ~ dunif(1982,1985)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","lambda6","lambda7","delta1", "delta2", "delta3",
                    "delta4","delta5","delta6","delta7")

jcode <- "model{ 
	for (i in 55:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta8), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7-lambda8),
        ifelse(age[i]> (2015-delta7) && age[i] < 34, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7),
        ifelse(age[i]> (2015-delta6) && age[i] < 29, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6),
        ifelse(age[i]> (2015-delta5) && age[i] < 24, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2015-delta4) && age[i] < 19, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
        0))))))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  lambda6  ~ dunif(0,1)       #uninformative prior
  lambda7  ~ dunif(0,1)       #uninformative prior
  lambda8  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  delta4  ~ dunif(1997,2000)
  delta5  ~ dunif(1992,1995)
  delta6  ~ dunif(1987,1990)
  delta7  ~ dunif(1982,1985)
  delta8  ~ dunif(1977,1980)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","lambda6","lambda7","lambda8","delta1", "delta2", "delta3",
                 "delta4","delta5","delta6","delta7","delta8")


jcode <- "model{ 
	for (i in 55:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta9), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7-lambda8-lambda9),
        ifelse(age[i]> (2015-delta8) && age[i] < 39, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7-lambda8),
        ifelse(age[i]> (2015-delta7) && age[i] < 34, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7),
        ifelse(age[i]> (2015-delta6) && age[i] < 29, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6),
        ifelse(age[i]> (2015-delta5) && age[i] < 24, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2015-delta4) && age[i] < 19, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
        0)))))))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  lambda6  ~ dunif(0,1)       #uninformative prior
  lambda7  ~ dunif(0,1)       #uninformative prior
  lambda8  ~ dunif(0,1)       #uninformative prior
  lambda9  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  delta4  ~ dunif(1997,2000)
  delta5  ~ dunif(1992,1995)
  delta6  ~ dunif(1987,1990)
  delta7  ~ dunif(1982,1985)
  delta8  ~ dunif(1977,1980)
  delta9  ~ dunif(1972,1975)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","lambda6","lambda7","lambda8","lambda9","delta1", "delta2", "delta3",
                 "delta4","delta5","delta6","delta7","delta8","delta9")


jcode <- "model{ 
	for (i in 55:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta10), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7-lambda8-lambda9-lambda10),
        ifelse(age[i]> (2015-delta9) && age[i] < 44, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7-lambda8-lambda9),
        ifelse(age[i]> (2015-delta8) && age[i] < 39, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7-lambda8),
        ifelse(age[i]> (2015-delta7) && age[i] < 34, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7),
        ifelse(age[i]> (2015-delta6) && age[i] < 29, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6),
        ifelse(age[i]> (2015-delta5) && age[i] < 24, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2015-delta4) && age[i] < 19, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
        0))))))))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  lambda6  ~ dunif(0,1)       #uninformative prior
  lambda7  ~ dunif(0,1)       #uninformative prior
  lambda8  ~ dunif(0,1)       #uninformative prior
  lambda9  ~ dunif(0,1)       #uninformative prior
  lambda10  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  delta4  ~ dunif(1997,2000)
  delta5  ~ dunif(1992,1995)
  delta6  ~ dunif(1987,1990)
  delta7  ~ dunif(1982,1985)
  delta8  ~ dunif(1977,1980)
  delta9  ~ dunif(1972,1975)
  delta10  ~ dunif(1967,1970)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","lambda6","lambda7","lambda8","lambda9","lambda10","delta1", "delta2", "delta3",
                 "delta4","delta5","delta6","delta7","delta8","delta9","delta10")


jcode <- "model{ 
	for (i in 55:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta11), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7-lambda8-lambda9-lambda10-lambda11),
        ifelse(age[i]> (2015-delta10) && age[i] < 49, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7-lambda8-lambda9-lambda10),
        ifelse(age[i]> (2015-delta9) && age[i] < 44, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7-lambda8-lambda9),
        ifelse(age[i]> (2015-delta8) && age[i] < 39, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7-lambda8),
        ifelse(age[i]> (2015-delta7) && age[i] < 34, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6-lambda7),
        ifelse(age[i]> (2015-delta6) && age[i] < 29, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6),
        ifelse(age[i]> (2015-delta5) && age[i] < 24, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2015-delta4) && age[i] < 19, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
        0)))))))))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  lambda6  ~ dunif(0,1)       #uninformative prior
  lambda7  ~ dunif(0,1)       #uninformative prior
  lambda8  ~ dunif(0,1)       #uninformative prior
  lambda9  ~ dunif(0,1)       #uninformative prior
  lambda10  ~ dunif(0,1)       #uninformative prior
  lambda11  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  delta4  ~ dunif(1997,2000)
  delta5  ~ dunif(1992,1995)
  delta6  ~ dunif(1987,1990)
  delta7  ~ dunif(1982,1985)
  delta8  ~ dunif(1977,1980)
  delta9  ~ dunif(1972,1975)
  delta10  ~ dunif(1967,1970)
  delta11  ~ dunif(1961,1965)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","lambda6","lambda7","lambda8","lambda9","lambda10","lambda11","delta1", "delta2", "delta3",
                 "delta4","delta5","delta6","delta7","delta8","delta9","delta10","delta11")

# Study 204_1
jcode <- "model{ 
	for (i in 55:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta5), 1-exp(-lambda5*age[i]),
        ifelse(age[i]> (2015-delta4) && age[i] < 19, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
        0)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  delta4  ~ dunif(1997,2000)
  delta5  ~ dunif(1992,1995)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","delta1", "delta2", "delta3",
                 "delta4","delta5")



# 204_2
jcode <- "model{ 
	for (i in 66:76){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
        0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  delta4  ~ dunif(1997,2000)
  
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","delta1", "delta2", "delta3",
                    "delta4")

jcode <- "model{ 
	for (i in 66:76){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta5), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2015-delta4) && age[i] < 19, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
        0)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2012,2015)
  delta2  ~ dunif(2007,2010)
  delta3  ~ dunif(2002,2005)
  delta4  ~ dunif(1997,2000)
  delta5  ~ dunif(1992,1995)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","delta1", "delta2", "delta3",
                    "delta4","delta5")


# study 236
jcode <- "model{ 
	for (i in 84:89){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2005-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2005-delta3) && age[i] < 34, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2005-delta2) && age[i] < 24, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2005-delta1) && age[i] < 14, 1-exp(-lambda1),
        0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1992,2005)
  delta2  ~ dunif(1982,1990)
  delta3  ~ dunif(1972,1980)
  delta4  ~ dunif(1962,1970)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","delta1", "delta2", "delta3",
                    "delta4")
# study 236
jcode <- "model{ 
	for (i in 84:89){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2005-delta5), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2005-delta4) && age[i] < 44, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2005-delta3) && age[i] < 34, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2005-delta2) && age[i] < 24, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2005-delta1) && age[i] < 14, 1-exp(-lambda1),
        0)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1992,2005)
  delta2  ~ dunif(1982,1990)
  delta3  ~ dunif(1972,1980)
  delta4  ~ dunif(1962,1970)
  delta5  ~ dunif(1952,1960)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","delta1", "delta2", "delta3",
                    "delta4","delta5")

# study 236
jcode <- "model{ 
	for (i in 84:89){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2005-delta6), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5-lambda6),
        ifelse(age[i]> (2005-delta5) && age[i] < 54, 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2005-delta4) && age[i] < 44, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2005-delta3) && age[i] < 34, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2005-delta2) && age[i] < 24, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2005-delta1) && age[i] < 14, 1-exp(-lambda1),
        0))))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  lambda6  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1992,2005)
  delta2  ~ dunif(1982,1990)
  delta3  ~ dunif(1972,1980)
  delta4  ~ dunif(1962,1970)
  delta5  ~ dunif(1952,1960)
  delta6  ~ dunif(1926,1950)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","lambda6","delta1", "delta2", "delta3",
                    "delta4","delta5","delta6")


# study 237
jcode <- "model{ 
	for (i in 90:94){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2004-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2004-delta3) && age[i] < 24, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2004-delta2) && age[i] < 14, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2004-delta1) && age[i] < 4, 1-exp(-lambda1),
        0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2001,2004)
  delta2  ~ dunif(1991,1999)
  delta3  ~ dunif(1981,1989)
  delta4  ~ dunif(1956,1979)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","delta1", "delta2", "delta3",
                    "delta4")

# study 237
jcode <- "model{ 
	for (i in 90:94){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2004-delta5), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2004-delta4) && age[i] < 49, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2004-delta3) && age[i] < 24, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2004-delta2) && age[i] < 14, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2004-delta1) && age[i] < 4, 1-exp(-lambda1),
        0)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2001,2004)
  delta2  ~ dunif(1991,1999)
  delta3  ~ dunif(1981,1989)
  delta4  ~ dunif(1956,1979)
  delta5  ~ dunif(1925,1954)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","delta1", "delta2", "delta3",
                    "delta4","delta5")


# 279
jcode <- "model{ 
	for (i in 95:99){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2007-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2007-delta3) && age[i] < 44, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2007-delta2) && age[i] < 29, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2007-delta1) && age[i] < 14, 1-exp(-lambda1),
        0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1994,2007)
  delta2  ~ dunif(1979,1992)
  delta3  ~ dunif(1964,1977)
  delta4  ~ dunif(1949,1962)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","delta1", "delta2", "delta3",
                    "delta4")

# 279
jcode <- "model{ 
	for (i in 95:99){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2007-delta5), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2007-delta4) && age[i] < 59, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2007-delta3) && age[i] < 44, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2007-delta2) && age[i] < 29, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2007-delta1) && age[i] < 14, 1-exp(-lambda1),
        0)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(1994,2007)
  delta2  ~ dunif(1979,1992)
  delta3  ~ dunif(1964,1977)
  delta4  ~ dunif(1949,1962)
  delta5  ~ dunif(1928,1947)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","delta1", "delta2", "delta3",
                    "delta4","delta5")


# 306
jcode <- "model{ 
	for (i in 104:111){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2014-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2014-delta3) && age[i] < 15, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2014-delta2) && age[i] < 10, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2014-delta1) && age[i] < 4, 1-exp(-lambda1),
        0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2011,2014)
  delta2  ~ dunif(2005,2009)
  delta3  ~ dunif(2000,2003)
  delta4  ~ dunif(1995,1998)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","delta1", "delta2", "delta3",
                    "delta4")

# 5 epidemic model DIC
# 17
jcode <- "model{ 
	for (i in 5:9){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2012-delta5), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2012-delta4) && age[i] < 60, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2012-delta3) && age[i] < 40, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2012-delta2) && age[i] < 20, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2012-delta1) && age[i] < 10, 1-exp(-lambda1),
        0)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2003,2012)
  delta2  ~ dunif(1993,2002)
  delta3  ~ dunif(1973,1992)
  delta4  ~ dunif(1953,1972)
  delta5  ~ dunif(1933,1952)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","delta1", "delta2", "delta3",
                    "delta4","delta5")

# 149
jcode <- "model{ 
	for (i in 43:47){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2018-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5),
        ifelse(age[i]> (2018-delta3) && age[i] < 55, 1-exp(-lambda1-lambda2-lambda3-lambda4),
        ifelse(age[i]> (2018-delta2) && age[i] < 35, 1-exp(-lambda1-lambda2-lambda3),
        ifelse(age[i]> (2018-delta1) && age[i] < 20, 1-exp(-lambda1-lambda2),
        ifelse(age[i]> (2018-delta1) && age[i] < 10, 1-exp(-lambda1),
        0)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2009,2018)
  delta2  ~ dunif(1999,2007)
  delta3  ~ dunif(1984,1997)
  delta4  ~ dunif(1964,1982)
  delta5  ~ dunif(1939,1962)
}"
paramVector <- c("lambda1","lambda2","lambda3","lambda4","lambda5","delta1", "delta2", "delta3",
                    "delta4","delta5")



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

#### Credible interval functions################################################

# 1) constant model
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

Epi5Con <- function(paramVector, mcmcMatrix, ager1, ager2, ager3, ager4, ager5,ager6){
  numSamples = 1000
  for(ii in 1:length(paramVector)) {
    
    outDf_1 <- matrix(NA,nrow=numSamples, ncol = length(ager1))
    outDf_2 <- matrix(NA,nrow=numSamples, ncol = length(ager2))
    outDf_3 <- matrix(NA,nrow=numSamples, ncol = length(ager3))
    outDf_4 <- matrix(NA,nrow=numSamples, ncol = length(ager4))
    outDf_5 <- matrix(NA,nrow=numSamples, ncol = length(ager5))
    outDf_6 <- matrix(NA,nrow=numSamples, ncol = length(ager6))
    
    for (kk in 1:numSamples ) {
      randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
      
      lambdaSample1    <- mcmcMatrix[randomNumber,"lambda1"]
      lambdaSample2    <- mcmcMatrix[randomNumber,"lambda2"]
      lambdaSample3    <- mcmcMatrix[randomNumber,"lambda3"]
      lambdaSample4    <- mcmcMatrix[randomNumber,"lambda4"]
      lambdaSample5    <- mcmcMatrix[randomNumber,"lambda5"]
      
      deltaSample_1   <- mcmcMatrix[randomNumber,"delta1"]
      deltaSample_2   <- mcmcMatrix[randomNumber,"delta2"]
      deltaSample_3   <- mcmcMatrix[randomNumber,"delta3"]
      deltaSample_4   <- mcmcMatrix[randomNumber,"delta4"]
      deltaSample_5   <- mcmcMatrix[randomNumber,"delta5"]
      
      newRow_1 <- 0
      newRow_2 <- 1-exp(-lambdaSample1)
      newRow_3 <- 1-exp(-(lambdaSample1+lambdaSample2))
      newRow_4 <- 1-exp(-(lambdaSample1+lambdaSample2+lambdaSample3))
      newRow_5 <- 1-exp(-(lambdaSample1+lambdaSample2+lambdaSample3+lambdaSample4))
      newRow_5 <- 1-exp(-(lambdaSample1+lambdaSample2+lambdaSample3+lambdaSample4))
      newRow_6 <- 1-exp(-lambdaSample5*ager6)
      
      outDf_1[kk,] <- newRow_1
      outDf_2[kk,] <- newRow_2
      outDf_3[kk,] <- newRow_3
      outDf_4[kk,] <- newRow_4
      outDf_5[kk,] <- newRow_5
      outDf_6[kk,] <- newRow_6
      
      outDf <- cbind(outDf_1,outDf_2,outDf_3,outDf_4,outDf_5,outDf_6)
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

outDf170 <- constantCR(paramVector, mcmcMatrix)
df_upperLower170 <- quantmat(outDf170)
g1<- plot170 <- plotfuncYonly(df_upperLower170, study170)

outDf306 <- constantCR(paramVector, mcmcMatrix)
df_upperLower306 <- quantmat(outDf306)
g2 <-plot306 <- plotfuncNone(df_upperLower306, study306)

outDf7 <-Epi1CR(paramVector, mcmcMatrix, ager1=0:8, ager2=9:80) 
df_upperLower7 <- quantmat(outDf7)
g3<-plot7 <- plotfuncNone(df_upperLower7, study7)

outDf17 <-Epi1CR(paramVector, mcmcMatrix, ager1=0:3, ager2=4:80) 
df_upperLower17 <- quantmat(outDf17)
g4<-plot17 <- plotfuncYonly(df_upperLower17, study17)

outDf236 <-Epi1CR(paramVector, mcmcMatrix, ager1=0:2, ager2=3:80) 
df_upperLower236 <- quantmat(outDf236)
g5<-plot236 <- plotfuncNone(df_upperLower236, study236)

outDf149 <- Epi2CR(paramVector, mcmcMatrix, ager1=0:3, ager2=4:13, ager3=14:80)
df_upperLower149 <- quantmat(outDf149)
g6<-plot149 <- plotfuncNone(df_upperLower149, study149)

outDf204_2 <- Epi2CR(paramVector, mcmcMatrix, ager1=0:1, ager2=2:5, ager3=6:80)
df_upperLower204_2 <- quantmat(outDf204_2)
g7<-plot204_2 <- plotfuncYonly(df_upperLower204_2, study204_2)

outDf494 <- Epi3CR(paramVector, mcmcMatrix, ager1 = 0:0, ager2= 1:3, ager3= 4:13, ager4=14:80)
df_upperLower494 <- quantmat(outDf494)
g8<-plot494 <- plotfuncNone(df_upperLower494, study494)

outDf106 <- Epi4CR(paramVector, mcmcMatrix, ager1 = 0:10, ager2= 11:26, ager3= 27:38, ager4=39:54, ager5=55:80)
df_upperLower106 <- quantmat(outDf106)
g9<-plot106 <- plotfuncNone(df_upperLower106, study106)

outDf237 <- Epi4CR(paramVector, mcmcMatrix, ager1 = 0:1, ager2=2:7, ager3=8:17, ager4=18:31, ager5=32:80)
df_upperLower237 <- quantmat(outDf237)
g10<-plot237 <- plotfuncXY(df_upperLower237, study237)

outDf279 <- Epi4CR(paramVector, mcmcMatrix, ager1 = 0:4, ager2=5:18, ager3=19:33, ager4=34:48, ager5=49:80)
df_upperLower279 <- quantmat(outDf279)
g11<-plot279 <- plotfuncXonly(df_upperLower279, study279)

outDf204_1 <- Epi5Con(paramVector, mcmcMatrix, ager1 = 0:0, ager2=1:5, ager3=6:10, ager4=11:15, ager5=16:20, ager6=21:80)
df_upperLower204_1 <- quantmat(outDf204_1)
g12<-plot204_1 <- plotfuncXonly(df_upperLower204_1, study204_1)


ssa_graph<-  grid.arrange(g1, g2, g3, g4,
                          g5, g6, g7, g8,
                          g9, g10, g11,g12, ncol=3,
                          left = "Proportion Seropositive",
                          bottom = "Age (years)",
                          top = "Seropositivity in SSA (non-fever population based studies)")
ggsave("ssaAll.pdf", ssa_graph, dpi=1000, device= "pdf", height=8, width=10,units="in", bg=NULL)


################################################################################
# FOI graphs
################################################################################

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

# study170
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat1 <- as.data.frame(matrix(NA, nrow = 2, ncol = 4))
colnames(foimat1) <- c("Year", "FOI", "lo", "hi")
foimat1[,1] <- c(1939, 2019)
foimat1[1:2,2] <- quantile(mcmcMatrix$lambda, c(0.5))
foimat1[1:2,3] <- quantile(mcmcMatrix$lambda, c(0.025))
foimat1[1:2,4] <- quantile(mcmcMatrix$lambda, c(0.975))
foimat1$country <- c("Ethiopia")

# study 306
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat2 <- as.data.frame(matrix(NA, nrow = 2, ncol = 4))
colnames(foimat2) <- c("Year", "FOI", "lo", "hi")
foimat2[,1] <- c(1933, 2014)
foimat2[1:2,2] <- quantile(mcmcMatrix$lambda, c(0.5))
foimat2[1:2,3] <- quantile(mcmcMatrix$lambda, c(0.025))
foimat2[1:2,4] <- quantile(mcmcMatrix$lambda, c(0.975))
foimat2$country <- c("Senegal")

# study 7
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat3 <- as.data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(foimat3) <- c("Year", "FOI", "lo", "hi")
foimat3[,1] <- c(1965, 1998, 1998, 2007)
foimat3[1:2,2] <- quantile(mcmcMatrix$lambda, c(0.5))
foimat3[1:2,3] <- quantile(mcmcMatrix$lambda, c(0.025))
foimat3[1:2,4] <- quantile(mcmcMatrix$lambda, c(0.975))
foimat3[3:4,2:4] <- 0
foimat3$country <- c("Benin")

# study 17
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat4 <- as.data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(foimat4) <- c("Year", "FOI", "lo", "hi")
foimat4[,1] <- c(1932, 2008, 2008, 2012)
foimat4[1:2,2] <- quantile(mcmcMatrix$lambda, c(0.5))
foimat4[1:2,3] <- quantile(mcmcMatrix$lambda, c(0.025))
foimat4[1:2,4] <- quantile(mcmcMatrix$lambda, c(0.975))
foimat4[3:4,2:4] <- 0
foimat4$country <- c("Senegal")

# study 236
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat5 <- as.data.frame(matrix(NA, nrow = 4, ncol = 4))
colnames(foimat5) <- c("Year", "FOI", "lo", "hi")
foimat5[,1] <- c(1925, 2000, 2000, 2005)
foimat5[1:2,2] <- quantile(mcmcMatrix$lambda, c(0.5))
foimat5[1:2,3] <- quantile(mcmcMatrix$lambda, c(0.025))
foimat5[1:2,4] <- quantile(mcmcMatrix$lambda, c(0.975))
foimat5[3:4,2:4] <- 0
foimat5$country <- c("Comoros")

# study 149
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat6 <- as.data.frame(matrix(NA, nrow = 6, ncol = 4))
colnames(foimat6) <- c("Year", "FOI", "lo", "hi")
foimat6[,1] <- c(1938, 2004, 2004, 2014, 2014, 2018)
foimat6[1:2,2] <- quantile(mcmcMatrix$lambda1, c(0.5))
foimat6[1:2,3] <- quantile(mcmcMatrix$lambda1, c(0.025))
foimat6[1:2,4] <- quantile(mcmcMatrix$lambda1, c(0.975))

foimat6[3:4,2] <- quantile(mcmcMatrix$lambda2, c(0.5))
foimat6[3:4,3] <- quantile(mcmcMatrix$lambda2, c(0.025))
foimat6[3:5,4] <- quantile(mcmcMatrix$lambda2, c(0.975))

foimat6[5:6,2:4] <- 0
foimat6$country <- c("Ethiopia")

# study 204_2
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat7 <- as.data.frame(matrix(NA, nrow = 6, ncol = 4))
colnames(foimat7) <- c("Year", "FOI", "lo", "hi")
foimat7[,1] <- c(1960, 2008, 2008, 2013, 2013, 2015)
foimat7[1:2,2] <- quantile(mcmcMatrix$lambda1, c(0.5))
foimat7[1:2,3] <- quantile(mcmcMatrix$lambda1, c(0.025))
foimat7[1:2,4] <- quantile(mcmcMatrix$lambda1, c(0.975))

foimat7[3:4,2] <- quantile(mcmcMatrix$lambda2, c(0.5))
foimat7[3:4,3] <- quantile(mcmcMatrix$lambda2, c(0.025))
foimat7[3:5,4] <- quantile(mcmcMatrix$lambda2, c(0.975))

foimat7[5:6,2:4] <- 0
foimat7$country <- c("Gabon")

# study 494
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat8 <- as.data.frame(matrix(NA, nrow = 8, ncol = 4))
colnames(foimat8) <- c("Year", "FOI", "lo", "hi")
foimat8[,1] <- c(1917, 2003, 2003, 2012, 2012, 2016, 2016, 2017)
foimat8[1:2,2] <- quantile(mcmcMatrix$lambda1, c(0.5))
foimat8[1:2,3] <- quantile(mcmcMatrix$lambda1, c(0.025))
foimat8[1:2,4] <- quantile(mcmcMatrix$lambda1, c(0.975))

foimat8[3:4,2] <- quantile(mcmcMatrix$lambda2, c(0.5))
foimat8[3:4,3] <- quantile(mcmcMatrix$lambda2, c(0.025))
foimat8[3:4,4] <- quantile(mcmcMatrix$lambda2, c(0.975))

foimat8[5:6,2] <- quantile(mcmcMatrix$lambda3, c(0.5))
foimat8[5:6,3] <- quantile(mcmcMatrix$lambda3, c(0.025))
foimat8[5:6,4] <- quantile(mcmcMatrix$lambda3, c(0.975))


foimat8[7:8,2:4] <- 0
foimat8$country <- c("Gabon")

# study 106
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat9 <- as.data.frame(matrix(NA, nrow = 10, ncol = 4))
colnames(foimat9) <- c("Year", "FOI", "lo", "hi")
foimat9[,1] <- c(1936, 1961, 1961, 1977, 1977, 1989, 1989, 2005, 2005, 2016)
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
foimat9$country <- c("Zambia")

# study 237
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat10 <- as.data.frame(matrix(NA, nrow = 10, ncol = 4))
colnames(foimat10) <- c("Year", "FOI", "lo", "hi")
foimat10[,1] <- c(1924, 1972, 1972, 1987, 1987, 1997, 1997, 2003, 2003, 2004)
foimat10[1:2,2] <- quantile(mcmcMatrix$lambda1, c(0.5))
foimat10[1:2,3] <- quantile(mcmcMatrix$lambda1, c(0.025))
foimat10[1:2,4] <- quantile(mcmcMatrix$lambda1, c(0.975))

foimat10[3:4,2] <- quantile(mcmcMatrix$lambda2, c(0.5))
foimat10[3:4,3] <- quantile(mcmcMatrix$lambda2, c(0.025))
foimat10[3:4,4] <- quantile(mcmcMatrix$lambda2, c(0.975))

foimat10[5:6,2] <- quantile(mcmcMatrix$lambda3, c(0.5))
foimat10[5:6,3] <- quantile(mcmcMatrix$lambda3, c(0.025))
foimat10[5:6,4] <- quantile(mcmcMatrix$lambda3, c(0.975))

foimat10[7:8,2] <- quantile(mcmcMatrix$lambda4, c(0.5))
foimat10[7:8,3] <- quantile(mcmcMatrix$lambda4, c(0.025))
foimat10[7:8,4] <- quantile(mcmcMatrix$lambda4, c(0.975))

foimat10[9:10,2:4] <- 0
foimat10$country <- c("Kenya")


# study 279
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat11 <- as.data.frame(matrix(NA, nrow = 10, ncol = 4))
colnames(foimat11) <- c("Year", "FOI", "lo", "hi")
foimat11[,1] <- c(1927, 1958, 1958, 1973, 1973, 1988, 1988, 2002, 2002, 2007)
foimat11[1:2,2] <- quantile(mcmcMatrix$lambda1, c(0.5))
foimat11[1:2,3] <- quantile(mcmcMatrix$lambda1, c(0.025))
foimat11[1:2,4] <- quantile(mcmcMatrix$lambda1, c(0.975))

foimat11[3:4,2] <- quantile(mcmcMatrix$lambda2, c(0.5))
foimat11[3:4,3] <- quantile(mcmcMatrix$lambda2, c(0.025))
foimat11[3:4,4] <- quantile(mcmcMatrix$lambda2, c(0.975))

foimat11[5:6,2] <- quantile(mcmcMatrix$lambda3, c(0.5))
foimat11[5:6,3] <- quantile(mcmcMatrix$lambda3, c(0.025))
foimat11[5:6,4] <- quantile(mcmcMatrix$lambda3, c(0.975))

foimat11[7:8,2] <- quantile(mcmcMatrix$lambda4, c(0.5))
foimat11[7:8,3] <- quantile(mcmcMatrix$lambda4, c(0.025))
foimat11[7:8,4] <- quantile(mcmcMatrix$lambda4, c(0.975))

foimat11[9:10,2:4] <- 0
foimat11$country <- c("Cameroon")

# study 204_1
mcmcMatrix <- as.data.frame(mcmcMatrix)
foimat12 <- as.data.frame(matrix(NA, nrow = 12, ncol = 4))
colnames(foimat12) <- c("Year", "FOI", "lo", "hi")
foimat12[,1] <- c(1960, 1994, 1994, 1999, 1999, 2004, 2004, 2009, 2009, 2014, 2014, 2015)
foimat12[1:2,2] <- quantile(mcmcMatrix$lambda1, c(0.5))
foimat12[1:2,3] <- quantile(mcmcMatrix$lambda1, c(0.025))
foimat12[1:2,4] <- quantile(mcmcMatrix$lambda1, c(0.975))

foimat12[3:4,2] <- quantile(mcmcMatrix$lambda2, c(0.5))
foimat12[3:4,3] <- quantile(mcmcMatrix$lambda2, c(0.025))
foimat12[3:4,4] <- quantile(mcmcMatrix$lambda2, c(0.975))

foimat12[5:6,2] <- quantile(mcmcMatrix$lambda3, c(0.5))
foimat12[5:6,3] <- quantile(mcmcMatrix$lambda3, c(0.025))
foimat12[5:6,4] <- quantile(mcmcMatrix$lambda3, c(0.975))

foimat12[7:8,2] <- quantile(mcmcMatrix$lambda4, c(0.5))
foimat12[7:8,3] <- quantile(mcmcMatrix$lambda4, c(0.025))
foimat12[7:8,4] <- quantile(mcmcMatrix$lambda4, c(0.975))

foimat12[9:10,2] <- quantile(mcmcMatrix$lambda5, c(0.5))
foimat12[9:10,3] <- quantile(mcmcMatrix$lambda5, c(0.025))
foimat12[9:10,4] <- quantile(mcmcMatrix$lambda5, c(0.975))

foimat12[11:12,2:4] <- 0
foimat12$country <- c("Burkina Faso")




g1 <- plotFOIXY(foimat1)
g2 <- plotFOIXOnly(foimat2)
g3 <- plotFOIXOnly(foimat3)
g4 <- plotFOIXY(foimat4)
g5 <- plotFOIXOnly(foimat5)
g6 <- plotFOIXOnly(foimat6)
g7 <- plotFOIXY(foimat7)
g8 <- plotFOIXOnly(foimat8)
g9 <- plotFOIXOnly(foimat9)
g10 <- plotFOIXY(foimat10)
g11 <- plotFOIXOnly(foimat11)
g12 <- plotFOIXOnly(foimat12)


ssa_foi<-  grid.arrange(g1, g2, g3, g4,
                          g5, g6, g7, g8,
                          g9, g10, g11,g12, ncol=3,
                          left = "FoI",
                          bottom = "Year",
                          top = "FoI in SSA (non-fever population based studies)")
ggsave("ssaFOI.pdf", ssa_foi, dpi=1000, device= "pdf", height=8, width=10,units="in", bg=NULL)


# second version FoI graph
plotFOIXY_const <- function(df){
  g <- ggplot(df, aes(x = Year, y = FOI))+
    geom_point(color = "#FDE725FF")+
    geom_errorbar(aes(ymin = lo, ymax = hi), color = "#FDE725FF", width = 0, linewidth = 1)+
    labs(x = "Year", y = "FoI")+ 
    theme_bw()+
    facet_wrap(~ country, nrow = 1)+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    scale_x_continuous(limits = c(1900, 2023), breaks = seq(1900, 2023, by = 10))+
    theme(axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())+
    ylab(element_blank())
    return(g)
}
plotFOIXY_epi1 <- function(df){
  g <- ggplot(df, aes(x = Year, y = FOI))+
    geom_point(color = "#B4DE2CFF")+
    geom_errorbar(aes(ymin = lo, ymax = hi), color = "#B4DE2CFF", width = 0, linewidth = 1)+
    labs(x = "Year", y = "FoI")+ 
    theme_bw()+
    facet_wrap(~ country, nrow = 1)+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    scale_x_continuous(limits = c(1900, 2023), breaks = seq(1900, 2023, by = 10))+
    ylab(element_blank())+
    theme(axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank())
    return(g)
}
plotFOIXY_epi2 <- function(df){
  g <- ggplot(df, aes(x = Year, y = FOI))+
    geom_point(color = "#6DCD59FF")+
    geom_errorbar(aes(ymin = lo, ymax = hi), color = "#6DCD59FF", width = 0, linewidth = 1)+
    labs(x = "Year", y = "FoI")+ 
    theme_bw()+
    facet_wrap(~ country, nrow = 1)+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    scale_x_continuous(limits = c(1900, 2023), breaks = seq(1900, 2023, by = 10))+
    ylab(element_blank())
    return(g)
}
plotFOIXY_epi3 <- function(df){
  g <- ggplot(df, aes(x = Year, y = FOI))+
    geom_point(color = "#35B779FF")+
    geom_errorbar(aes(ymin = lo, ymax = hi), color = "#35B779FF", width = 0, linewidth = 1)+
    labs(x = "Year", y = "FoI")+ 
    theme_bw()+
    facet_wrap(~ country, nrow = 1)+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    scale_x_continuous(limits = c(1900, 2023), breaks = seq(1900, 2023, by = 10))+
    ylab(element_blank())
    return(g)
}
plotFOIXY_epi4 <- function(df){
  g <- ggplot(df, aes(x = Year, y = FOI))+
    geom_point(color = "#1F9E89FF")+
    geom_errorbar(aes(ymin = lo, ymax = hi), color = "#1F9E89FF", width = 0, linewidth = 1)+
    scale_color_manual(values = "#1F9E89FF") +
    labs(x = "Year", y = "FoI")+ 
    theme_bw()+
    facet_wrap(~ country, nrow = 1)+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    scale_x_continuous(limits = c(1900, 2023), breaks = seq(1900, 2023, by = 10))+
    theme(axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())+
    ylab(element_blank())
    return(g)
}
plotFOIXY_epi5 <- function(df){
  g <- ggplot(df, aes(x = Year, y = FOI))+
    geom_point(color = "#26828EFF")+
    geom_errorbar(aes(ymin = lo, ymax = hi), color = "#26828EFF", width = 0, linewidth = 1)+
    labs(x = "Year", y = "FoI")+ 
    theme_bw()+
    facet_wrap(~ country, nrow = 1)+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    scale_x_continuous(limits = c(1900, 2023), breaks = seq(1900, 2023, by = 10))+
    theme(axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())+
    ylab(element_blank())
  return(g)
}
plotFOIXY_ethi <- function(df){
  g <- ggplot(df, aes(x = Year, y = FOI, color = model))+
    geom_point()+
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 1)+
    scale_color_manual(values = c("const" = "#FDE725FF", "epi" = "#6DCD59FF")) +
    labs(x = "Year", y = "FoI")+ 
    theme_bw()+
    facet_wrap(~ country, nrow = 1)+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    scale_x_continuous(limits = c(1900, 2023), breaks = seq(1900, 2023, by = 10))+
    theme(legend.position = "none")+
    theme(axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())+
    ylab(element_blank())
  return(g)
}
plotFOIXY_gabon <- function(df){
  g <- ggplot(df, aes(x = Year, y = FOI, color = model))+
    geom_point()+
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 1)+
    scale_color_manual(values = c("epi2" = "#6DCD59FF", "epi3" = "#35B779FF")) +
    labs(x = "Year", y = "FoI")+ 
    theme_bw()+
    facet_wrap(~ country, nrow = 1)+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    scale_x_continuous(limits = c(1900, 2023), breaks = seq(1900, 2023, by = 10))+
    theme(legend.position = "none")+
    theme(axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank())+
    ylab(element_blank())
  return(g)
}
plotFOIXY_senegal <- function(df){
  g <- ggplot(df, aes(x = Year, y = FOI, color = model))+
    geom_point()+
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 1, size = 1.5)+
    scale_color_manual(values = c("epi1" = "#B4DE2CFF", "const" = "#FDE725FF")) +
    labs(x = "Year", y = "FoI")+ 
    theme_bw()+
    facet_wrap(~ country, nrow = 1)+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    scale_x_continuous(limits = c(1900, 2023), breaks = seq(1900, 2023, by = 10))+
    theme(legend.position = "none")+
    theme(axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())+
    ylab(element_blank())
  return(g)
}
plotFOIXY_zambia <- function(df){
  g <- ggplot(df, aes(x = Year, y = FOI))+
    geom_point(color = "#1F9E89FF")+
    geom_errorbar(aes(ymin = lo, ymax = hi), color = "#1F9E89FF", width = 0, linewidth = 1)+
    scale_color_manual(values = "#1F9E89FF") +
    labs(x = "Year", y = "FoI")+ 
    theme_bw()+
    facet_wrap(~ country, nrow = 1)+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    scale_x_continuous(limits = c(1900, 2023), breaks = seq(1900, 2023, by = 10))+
    ylab(element_blank())+
    xlab(element_blank())+  # Remove the x-axis label
    theme(axis.line.x = element_blank())
  return(g)
}
plotFOIXY_cameroon <- function(df){
  g <- ggplot(df, aes(x = Year, y = FOI))+
    geom_point(color = "#1F9E89FF")+
    geom_errorbar(aes(ymin = lo, ymax = hi), color = "#1F9E89FF", width = 0, linewidth = 1)+
    scale_color_manual(values = "#1F9E89FF") +
    labs(x = "Year", y = "FoI")+ 
    theme_bw()+
    facet_wrap(~ country, nrow = 1)+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    scale_x_continuous(limits = c(1900, 2023), breaks = seq(1900, 2023, by = 10))+
    theme(axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank())+
    ylab(element_blank())+
    xlab(element_blank())
  return(g)
}


# study7: Benin
df1 <- foimat3[2,]
foi1 <- plotFOIXY_const(df1)
# study204: Burkina
df2 <- foimat12[c(1,3,5,7,9),]
foi2 <- plotFOIXY_epi5(df2)
# study149/170: Ethiopia
df3 <- foimat6[c(1,3),]
df4 <- foimat1[2,]
dfethi <- rbind(df3, df4)
dfethi$model <- c("epi", "epi", "const")
foi3 <- plotFOIXY_ethi(dfethi)

# study204_2/494: Gabon
df5 <- foimat7[c(1,3),]
df6 <- foimat8[c(1,3,5),]
dfgabon <- rbind(df5,df6)
dfgabon$model <- c("epi2","epi2", "epi3","epi3","epi3")
foi4 <- plotFOIXY_gabon(dfgabon)

# study237: Kenya
df7 <- foimat10[c(1,3,5,7),]
foi5 <- plotFOIXY_epi4(df7)

# study17/306: Senegal
df8 <- foimat4[2,]
df9 <- foimat2[2,]
dfsenegal <- rbind(df8, df9)
dfsenegal$model <- c("epi1", "const")
foi6 <- plotFOIXY_senegal(dfsenegal)

# study106: Zambia
df10 <- foimat9[c(1,3,5,7),]
foi7 <- plotFOIXY_zambia(df10)

# study279: Cameroon
df11 <- foimat11[c(1,3,5,7),]
foi8 <- plotFOIXY_cameroon(df11)

# study236
df12 <- foimat5[2,]
foi9 <- plotFOIXY_epi1(df12)

ssa_foi_v2<-  grid.arrange(foi1, foi2, foi3, 
                           foi4, foi5, foi6, 
                           foi7, foi8, foi9, ncol=3,
                           left = "FoI",
                           bottom = "Year",
                           top = "FoI in SSA (non-fever population based studies)")
ggsave("ssaFOIv2.pdf", ssa_foi_v2, dpi=1000, device= "pdf", height=10, width=13,units="in", bg=NULL)

df12$model <- "NA"
dflegend <- rbind(dfgabon, df12)
dflegend$Model <- c("Constant", "Epi1", "Epi2", "Epi3", "Epi4", "Epi5")
plotFOIXY_legend <- function(df){
  g <- ggplot(df, aes(x = Year, y = FOI, color = Model))+
    geom_point()+
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 1, size = 1.5)+
    scale_color_manual(values = c(Constant = "#FDE725FF", Epi1 = "#B4DE2CFF", Epi2 = "#6DCD59FF",
                                  Epi3 = "#35B779FF", Epi4 = "#1F9E89FF", Epi5 = "#26828EFF")) +
    labs(x = "Year", y = "FoI")+ 
    theme_bw()+
    facet_wrap(~ country, nrow = 1)+
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.05))+
    scale_x_continuous(limits = c(1900, 2023), breaks = seq(1900, 2023, by = 10))
    return(g)
}
plotFOIXY_legend(dflegend)


foiaverage <- rbind(df1[,c(2,5)], df2[,c(2,5)], df3[,c(2,5)],
                    df4[,c(2,5)], df5[,c(2,5)], df6[,c(2,5)],
                    df7[,c(2,5)], df8[,c(2,5)], df9[,c(2,5)],
                    df10[,c(2,5)], df11[,c(2,5)], df12[,c(2,5)])

foiaverage_mid <- foiaverage %>% 
                    group_by(country) %>%
                    summarise(FOI = median(FOI))
foiaverage_mid$long <- c(3.5572267, 0.20273423, 12.601367, )
foiaverage_mid$lat <- 

world <- map_data("world")

world %>%
  merge(foiaverage_mid, by.x = "region", by.y = "country", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = FOI)) +
  #geom_point(aes(x = long, y = lat, color = FOI), size = 3) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_viridis(name = "Average FoI", na.value = "gray90") +
  labs(color = "number")+
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

# https://sarahleejane.github.io/learning/r/2014/09/21/plotting-data-points-on-maps-with-r.html

merge <- world %>%
  merge(foiaverage_mid, by.x = "region", by.y = "country", all.x = T)
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = merge,
    aes(long, lat, color = FOI, alpha = 0.7),
    alpha = 0.7
  ) +
  scale_color_viridis_c(name = "Average FoI")
