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

# Get the data in the appropriate format for RSero package
chik_systematic_review_v1 <- read_excel("Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "Rsero")
View(chik_systematic_review_v1)



p.infection=0.2
N.samples=500
age = round(runif(n=N.samples, min =1, max = 70))
seropositive = runif(n=N.samples)<p.infection 


simulated.survey  = SeroData(age_at_sampling = age, Y = seropositive) 

seroprevalence.plot(simulated.survey,YLIM=0.3)

data = SeroData(age_at_sampling = c(10,32,24), Y=c(0,1,1), 
                max_age = 50, age_cats = 1, 
                sampling_year = 2017)
OutbreakModel = FOImodel( type='outbreak', K=1)

data('one_peak_simulation')
FOIfit.outbreak = fit( data = one_peak_simulation,  
                      model = OutbreakModel, chains=1)
seroprevalence.fit(FOIfit.outbreak)

SeroData(age_at_sampling=c(1,1,1), age = c(1,1,1), Y=c(0,1,1), age_cats = 1, max_age = NULL,
         sampling_year = 2010, location = NULL, sex = NULL)



