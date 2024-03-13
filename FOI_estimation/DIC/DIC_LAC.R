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

CountryModel <- read_excel("Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "inclusion", col_types = c("numeric", 
                                                              "text", "numeric", "numeric", "numeric", 
                                                              "numeric", "text", "text", "text", 
                                                              "text", "text", "text", "text", "text", 
                                                              "text", "text", "text"))
View(CountryModel)
#Rename data
df_lac = CountryModel %>% filter(region == "LAC")

df_lac[,c("midpoint","lower","upper")] = binom.confint(df_lac$N.pos, df_lac$N, method="exact")[,c("mean","lower","upper")]

df_lac <-  df_lac %>% 
  dplyr::mutate(agemid = (age_min+age_max)/2)

df_lac <- df_lac[df_lac$antibody != "IgM",] # remove data that has only IgM

study52        <-  df_lac %>% filter(study_no == 52)
study124       <-  df_lac %>% filter(study_no == 124)
study153       <-  df_lac %>% filter(study_no == 153)
study165       <-  df_lac %>% filter(study_no == 165)
study174       <-  df_lac %>% filter(study_no == 174)
study179       <-  df_lac %>% filter(study_no == 179)
study229       <-  df_lac %>% filter(study_no == 229)
study243       <-  df_lac %>% filter(study_no == 243)
study251       <-  df_lac %>% filter(study_no == 251)
study260       <-  df_lac %>% filter(study_no == 260)
study299       <-  df_lac %>% filter(study_no == 299)
study345       <-  df_lac %>% filter(study_no == 345)
study396       <-  df_lac %>% filter(study_no == 396)
study406       <-  df_lac %>% filter(study_no == 406)

# study 52
# constant model: test DIC
jcode <- "model{ 
	for (i in 1:4){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 1:4){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2018-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2005,2018) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 1:4){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2018-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2018-delta1) && age[i] < 14, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2005,2018)
  delta2  ~ dunif(1990,2003)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 1:4){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2018-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2018-delta2) && age[i] < 29, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2018-delta1) && age[i] < 14, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2005,2018)
  delta2  ~ dunif(1990,2003)
  delta3  ~ dunif(1960,1988)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 3 epidemic
jcode <- "model{ 
	for (i in 1:4){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2018-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2018-delta3) && age[i] < 59, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2018-delta2) && age[i] < 29, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2018-delta1) && age[i] < 14, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2005,2018)
  delta2  ~ dunif(1990,2003)
  delta3  ~ dunif(1960,1988)
  delta4  ~ dunif(1939,1988)
}"
paramVector <- c("lambda1","lambda2", "lambda3","lambda4", "delta1", "delta2", "delta3", "delta4")


# study 124
# constant model: test DIC
jcode <- "model{ 
	for (i in 17:20){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 17:20){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2019-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2006,2019) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 17:20){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 14, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2006,2019)
  delta2  ~ dunif(1990,2004)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 17:20){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2019-delta2) && age[i] < 30, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 14, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2006,2019)
  delta2  ~ dunif(1990,2004)
  delta3  ~ dunif(1960,1988)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 17:20){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2019-delta3) && age[i] < 60, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2019-delta2) && age[i] < 30, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 14, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2006,2019)
  delta2  ~ dunif(1990,2004)
  delta3  ~ dunif(1960,1988)
  delta4  ~ dunif(1940,1958)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "lambda4", "delta1", "delta2", "delta3", "delta4")

# study 153
# constant model: test DIC
jcode <- "model{ 
	for (i in 21:39){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 22:40){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2014-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2010,2014) #uninformative prior
}"
paramVectorEpi <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 21:40){
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
  delta2  ~ dunif(2006,2009)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 21:40){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2014-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2014-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2014-delta1) && age[i] < 4, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2011,2014)
  delta2  ~ dunif(2006,2009)
  delta3  ~ dunif(2001,2004)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# study 165
# constant model: test DIC
jcode <- "model{ 
	for (i in 41:44){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 41:44){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2018-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2000,2018) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 41:44){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2018-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2018-delta1) && age[i] < 19, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2000,2018)
  delta2  ~ dunif(1974,1998)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 41:44){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2014-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2014-delta2) && age[i] < 45, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2014-delta1) && age[i] < 19, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2000,2018)
  delta2  ~ dunif(1974,1998)
  delta3  ~ dunif(1959,1972)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 3 epidemic
jcode <- "model{ 
	for (i in 41:44){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2014-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2014-delta3) && age[i] < 60, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2014-delta2) && age[i] < 45, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2014-delta1) && age[i] < 19, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1  ~ dunif(2000,2018)
  delta2  ~ dunif(1974,1998)
  delta3  ~ dunif(1959,1972)
  delta4  ~ dunif(1939,1957)
}"
paramVector <- c("lambda1","lambda2", "lambda3","lambda4", "delta1", "delta2", "delta3", "delta4")

# study 174
# constant model: test DIC
jcode <- "model{ 
	for (i in 49:55){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 49:55){
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

# 2 epidemic
jcode <- "model{ 
	for (i in 49:55){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(2012,2015) #uninformative prior
  delta2   ~ dunif(2007,2010)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 49:55){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(2012,2015) 
  delta2   ~ dunif(2007,2010)
  delta3   ~ dunif(2002,2005)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# 4 epidemic
jcode <- "model{ 
	for (i in 49:55){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta4), 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
       0))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(2012,2015) 
  delta2   ~ dunif(2007,2010)
  delta3   ~ dunif(2002,2005)
  delta4   ~ dunif(1987,2000)
}"
paramVector <- c("lambda1","lambda2", "lambda3","lambda4", "delta1", "delta2", "delta3","delta4")

# 5 epidemic
jcode <- "model{ 
	for (i in 49:55){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta5), 1-exp(-lambda1-lambda2-lambda3-lambda4-lambda5)
       ,ifelse(age[i]> (2015-delta4) && age[i] < 29, 1-exp(-lambda1-lambda2-lambda3-lambda4)
       ,ifelse(age[i]> (2015-delta3) && age[i] < 14, 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2015-delta2) && age[i] < 9, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 4, 1-exp(-lambda1),
       0)))))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  lambda4  ~ dunif(0,1)       #uninformative prior
  lambda5  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(2012,2015) 
  delta2   ~ dunif(2007,2010)
  delta3   ~ dunif(2002,2005)
  delta4   ~ dunif(1987,2000)
  delta5   ~ dunif(1972,1985)
}"
paramVector <- c("lambda1","lambda2", "lambda3","lambda4","lambda5", "delta1", "delta2", "delta3","delta4","delta5")

# study 179
# constant model: test DIC
jcode <- "model{ 
	for (i in 56:61){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 56:61){
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
	for (i in 56:61){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 19, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(1997,2015) 
  delta2   ~ dunif(1987,1995)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 56:61){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2015-delta2) && age[i] < 29, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 19, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(1997,2015) 
  delta2   ~ dunif(1987,1995)
  delta3   ~ dunif(1977,1985)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")


# study 229
# constant model: test DIC
jcode <- "model{ 
	for (i in 62:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 62:65){
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
	for (i in 62:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 19, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(1997,2015) #uninformative prior
  delta2   ~ dunif(1987,1995)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 62:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2015-delta2) && age[i] < 29, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 19, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(1997,2015) 
  delta2   ~ dunif(1987,1995)
  delta3   ~ dunif(1977,1985)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# study 243
# constant model: test DIC
jcode <- "model{ 
	for (i in 66:68){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 66:68){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2016-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1998,2016) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 66:68){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2016-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2016-delta1) && age[i] < 19, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(1998,2016) #uninformative prior
  delta2   ~ dunif(1983,1996)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# study 251
# constant model: test DIC
jcode <- "model{ 
	for (i in 69:73){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 69:73){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2019-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2001,2019) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 69:73){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 19, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(2001,2019) #uninformative prior
  delta2   ~ dunif(1996,1999)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 62:65){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2019-delta2) && age[i] < 24, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 19, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(2001,2019) 
  delta2   ~ dunif(1996,1999)
  delta3   ~ dunif(1991,1994)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# study 260
# constant model: test DIC
jcode <- "model{ 
	for (i in 74:78){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 74:78){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2019-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2010,2019) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 74:78){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 10, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(2010,2019) 
  delta2   ~ dunif(2000,2008)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 74:78){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2019-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2019-delta2) && age[i] < 20, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2019-delta1) && age[i] < 10, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(2010,2019) 
  delta2   ~ dunif(2000,2008)
  delta3   ~ dunif(1990,1998)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")

# study 299
# constant model: test DIC
jcode <- "model{ 
	for (i in 84:86){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 84:86){
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
	for (i in 84:86){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2015-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2015-delta1) && age[i] < 10, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(1997,2015) 
  delta2   ~ dunif(1977,1995)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# study 345
# constant model: test DIC
jcode <- "model{ 
	for (i in 87:90){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 87:90){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2016-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(1977,2016) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# study 396
# constant model: test DIC
jcode <- "model{ 
	for (i in 91:93){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 91:93){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2017-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2004,2017) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 91:93){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2017-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2017-delta1) && age[i] < 14, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(2004,2017) 
  delta2   ~ dunif(1979,2002)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# study 406
# constant model: test DIC
jcode <- "model{ 
	for (i in 94:98){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
	}
		#  prior dists
  lambda ~ dunif(0,1) #uninformative prior
}"
paramVector <- c("lambda")

# 1 epidemic
jcode <- "model{ 
	for (i in 94:98){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i] < (2016-delta),0,
                            1-exp(-lambda))
    # time-varying catalytic model
	}
	#  prior dists
  lambda ~ dunif(0,1)       #uninformative prior
  delta  ~ dunif(2003,2016) #uninformative prior
}"
paramVector <- c("lambda", "delta")

# 2 epidemic
jcode <- "model{ 
	for (i in 94:98){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2016-delta2), 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2016-delta1) && age[i] < 14, 1-exp(-lambda1),
       0))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(2003,2016) 
  delta2   ~ dunif(1988,2002)
}"
paramVector <- c("lambda1","lambda2", "delta1", "delta2")

# 3 epidemic
jcode <- "model{ 
	for (i in 94:98){
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ifelse(age[i]> (2016-delta3), 1-exp(-lambda1-lambda2-lambda3)
       ,ifelse(age[i]> (2016-delta2) && age[i] < 29, 1-exp(-lambda1-lambda2)
       ,ifelse(age[i]> (2016-delta1) && age[i] < 14, 1-exp(-lambda1),
       0)))
    # time-varying catalytic model
	}
	#  prior dists
  lambda1  ~ dunif(0,1)       #uninformative prior
  lambda2  ~ dunif(0,1)       #uninformative prior
  lambda3  ~ dunif(0,1)       #uninformative prior
  delta1   ~ dunif(2003,2016) 
  delta2   ~ dunif(1988,2002)
  delta3   ~ dunif(1973,1986)
}"
paramVector <- c("lambda1","lambda2", "lambda3", "delta1", "delta2", "delta3")


# Run model
mcmc.length=50000
jdat = list(n.pos= df_lac$N.pos,
            N=df_lac$N,
            age=df_lac$agemid)
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
outDf345 <- constantCR(paramVector, mcmcMatrix)
df_upperLower345 <- quantmat(outDf345)
g1<- plot345 <- plotfuncYonly(df_upperLower345, study345)

outDf52 <- Epi1CR(paramVector, mcmcMatrix, ager1 = 0:3, ager2 = 4:80)
df_upperLower52 <- quantmat(outDf52)
g2<- plot52 <- plotfuncNone(df_upperLower52, study52)

outDf165 <- Epi1CR(paramVector, mcmcMatrix, ager1 = 0:5, ager2 = 6:80)
df_upperLower165 <- quantmat(outDf165)
g3<- plot165 <- plotfuncNone(df_upperLower165, study165)

outDf179 <- Epi1CR(paramVector, mcmcMatrix, ager1 = 0:8, ager2 = 9:80)
df_upperLower179 <- quantmat(outDf179)
g4<- plot179 <- plotfuncYonly(df_upperLower179, study179)

outDf229 <- Epi1CR(paramVector, mcmcMatrix, ager1 = 0:4, ager2 = 5:80)
df_upperLower229 <- quantmat(outDf229)
g5<- plot229 <- plotfuncNone(df_upperLower229, study229)

outDf243 <- Epi1CR(paramVector, mcmcMatrix, ager1 = 0:8, ager2 = 9:80)
df_upperLower243 <- quantmat(outDf243)
g6<- plot243 <- plotfuncNone(df_upperLower243, study243)

outDf299 <- Epi1CR(paramVector, mcmcMatrix, ager1 = 0:4, ager2 = 5:80)
df_upperLower299 <- quantmat(outDf299)
g7<- plot299 <- plotfuncYonly(df_upperLower299, study299)

outDf396 <- Epi1CR(paramVector, mcmcMatrix, ager1 = 0:4, ager2 = 5:80)
df_upperLower396 <- quantmat(outDf396)
g8<- plot396 <- plotfuncNone(df_upperLower396, study396)

outDf406 <- Epi1CR(paramVector, mcmcMatrix, ager1 = 0:3, ager2 = 4:80)
df_upperLower406 <- quantmat(outDf406)
g9<- plot406 <- plotfuncNone(df_upperLower406, study406)

outDf251 <- Epi2CR(paramVector, mcmcMatrix, ager1 = 0:7, ager2 = 8:20, ager3 = 21:80)
df_upperLower251 <- quantmat(outDf251)
g10<- plot251 <- plotfuncYonly(df_upperLower251, study251)

outDf260 <- Epi2CR(paramVector, mcmcMatrix, ager1 = 0:2, ager2 = 3:13, ager3 = 14:80)
df_upperLower260 <- quantmat(outDf260)
g11<- plot260 <- plotfuncNone(df_upperLower260, study260)

outDf124 <- Epi3CR(paramVector, mcmcMatrix, ager1 = 0:2, ager2 = 3:18, ager3 = 19:37, ager4 = 38:80)
df_upperLower124 <- quantmat(outDf124)
g12<- plot124 <- plotfuncNone(df_upperLower124, study124)

outDf174 <- Epi4CR(paramVector, mcmcMatrix, ager1 = 0:0, ager2 = 1:5, ager3 = 6:10, ager4 = 11:17, ager5 = 18:80)
df_upperLower174 <- quantmat(outDf174)
g13<- plot174 <- plotfuncXY(df_upperLower174, study174)

lac_graph<-  grid.arrange(g1, g2, g3, g4,
                          g5, g6, g7, g8,
                          g9, g10, g11, g12,
                          g13, ncol=3,
                          left = "Proportion Seropositive",
                          bottom = "Age (years)",
                          top = "Seropositivity in LAC (non-fever population based studies)")
ggsave("lacAll.pdf", lac_graph, dpi=1000, device= "pdf", height=8, width=10,units="in", bg=NULL)
