require(sp)
library(raster)
library(rgdal)
library(rasterVis)
library(ggplot2)
library(sf)
library(tiff)
library(rnaturalearth)
library(MVSE)
library(data.table)
library(tidyr)
library(ncdf4)
options(scipen = 999)
setwd("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/rasterfiles")

###---Step1: get world climate/humidity data (netCDF form)----------------------
##------------------------------------------------------------------------------
pathClim <- "D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/rasterfiles/air.2m.gauss.2022.nc"
clim_raster <- brick(pathClim)
plot(clim_raster)
climdf <- as.data.frame(clim_raster, xy = T)

pathRh <- "D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/rasterfiles/rhum.2022.nc"
rhum_raster <- brick(pathRh)
plot(rhum_raster)
rhumdf <- as.data.frame(rhum_raster, xy = T)

##---Step2: get country shape data----------------------------------------------
#-------------------------------------------------------------------------------
# load country shapefile https://www.diva-gis.org/datadown
gabon <- readOGR(dsn="D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/GAB_adm", 
                 layer="GAB_adm0")
gabonsf <- spTransform(gabon, CRS(proj4string(clim_raster)))
gabon_temp <- raster::extract(clim_raster, gabonsf) # extract country data by its shape grid cell 
gabon_tempdf <- do.call(rbind, gabon_temp) - 273.15  # convert Kelvin scale to Celsius degree
gabon_rh <- raster::extract(rhum_raster, gabonsf) # extract country data by its shape grid cell 
gabon_rhdf <- do.call(rbind, gabon_rh) 

# counstruct dataframes
gabonData <- as.data.frame(matrix(NA, nrow = 365, ncol = 3))
colnames(gabonData) <- c("date", "T", "H")
gabonData[,1] <- as.character(seq(as.Date('2022-01-01'), as.Date('2022-12-31'), by = 'day'))
gabonData[,2] <- colMeans(gabon_tempdf, na.rm = T)
gabonData[,3] <- colMeans(gabon_rhdf, na.rm = T)

# load country shapefile: SGP
sgp <- readOGR(dsn="D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/SGP_adm", 
                 layer="SGP_adm0")
sgpsf <- spTransform(sgp, CRS(proj4string(clim_raster)))
sgp_temp <- raster::extract(clim_raster, sgpsf) # extract country data by its shape grid cell 
sgp_tempdf <- do.call(rbind, sgp_temp) - 273.15  # convert Kelvin scale to Celsius degree
sgp_rh <- raster::extract(rhum_raster, sgpsf) # extract country data by its shape grid cell 
sgp_rhdf <- do.call(rbind, sgp_rh) 

# counstruct dataframes
sgpData <- as.data.frame(matrix(NA, nrow = 365, ncol = 3))
colnames(sgpData) <- c("date", "T", "H")
sgpData[,1] <- as.character(seq(as.Date('2022-01-01'), as.Date('2022-12-31'), by = 'day'))
sgpData[,2] <- colMeans(sgp_tempdf, na.rm = T)
sgpData[,3] <- colMeans(sgp_rhdf, na.rm = T)



##---Step3: Index P prior setting-----------------------------------------------
##------------------------------------------------------------------------------
data("climateFSA")
priors <- list(mosq_life_exp=list(pars=c(mean=12, sd=2), dist="normal"),
               mosq_inc_per=list(pars=c(mean=7, sd=2), dist="normal"),
               mosq_biting_freq=list(pars=c(mean=0.25, sd=0.01), dist="normal"),
               human_life_exp=list(pars=c(mean=71.1, sd=2), dist="normal"),
               human_inc_per=list(pars=c(mean=5.8, sd=1), dist="normal"),
               human_inf_per=list(pars=c(mean=5.9, sd=1), dist="normal"))
gab_mvse_model <- mvse_model(model_name="All", climate_data=gabonData, priors=priors)
gab_mvse_fit <- sampling(gab_mvse_model, verbose=FALSE)
gabindexP <- mcmc_index_dist(gab_mvse_fit, index="indexP")
plot_priors(object=gab_mvse_model)
mcmc_traceplot(gab_mvse_fit, options=list(max_iter=10^5))
plot_climate(object=gab_mvse_model)

Gaballoutput <- MVSE::extract(gab_mvse_fit, pars = "indexP")
Gaballoutput <- do.call(rbind, Gaballoutput)
GaballoutputVals <- Gaballoutput[,-1]
Gabquantindex <- as.data.frame(apply(GaballoutputVals, 1, function(row) quantile(row, probs = 0.5)))

sgp_mvse_model <- mvse_model(model_name="All", climate_data=sgpData, priors=priors)
sgp_mvse_fit <- sampling(sgp_mvse_model, verbose=FALSE)
sgpindexP <- mcmc_index_dist(sgp_mvse_fit, index="indexP")
plot_priors(object=sgp_mvse_model)
mcmc_traceplot(sgp_mvse_fit, options=list(max_iter=10^5))
plot_climate(object=sgp_mvse_model)

sgpalloutput <- MVSE::extract(sgp_mvse_fit, pars = "indexP")
sgpalloutput <- do.call(rbind, sgpalloutput)
sgpalloutputVals <- sgpalloutput[,-1]
sgpquantindex <- as.data.frame(apply(sgpalloutputVals, 1, function(row) quantile(row, probs = 0.5)))


## 1. Raw indexP graph
GabindexRaw <- as.data.frame(matrix(NA, nrow = 365, ncol = 3))
colnames(GabindexRaw) <- c("id", "date", "IndexP")
GabindexRaw[,1] <- paste0("indexP", seq(1,365))
GabindexRaw[,2] <- Gaballoutput[,1]
GabindexRaw[,3] <- Gabquantindex[,1]

GabindexRaw$date <- as.Date(paste0(GabindexRaw$date, "-01"), format="%Y-%m-%d")
ggplot(data = GabindexRaw, aes(x = date, y = IndexP))+
  scale_x_date(date_labels = "%Y.%m") + # %Y represents 4-digit year and $m represents the months as number (01-12), and its preceded by a dot. -> 2020.01 ~ 2020.12
  geom_line()+
  scale_y_continuous(breaks = seq(0, max(GabindexRaw$IndexP, na.rm=TRUE), by = 1)) +
  theme_bw()+
  geom_smooth(method = "loess")+
  ggtitle("Daily IndexP in Gabon")

sgpindexRaw <- as.data.frame(matrix(NA, nrow = 365, ncol = 3))
colnames(sgpindexRaw) <- c("id", "date", "IndexP")
sgpindexRaw[,1] <- paste0("indexP", seq(1,365))
sgpindexRaw[,2] <- sgpalloutput[,1]
sgpindexRaw[,3] <- sgpquantindex[,1]

sgpindexRaw$date <- as.Date(paste0(sgpindexRaw$date, "-01"), format="%Y-%m-%d")
ggplot(data = sgpindexRaw, aes(x = date, y = IndexP))+
  scale_x_date(date_labels = "%Y.%m") + # %Y represents 4-digit year and $m represents the months as number (01-12), and its preceded by a dot. -> 2020.01 ~ 2020.12
  geom_line()+
  scale_y_continuous(breaks = seq(0, max(sgpindexRaw$IndexP, na.rm=TRUE), by = 1)) +
  theme_bw()+
  geom_smooth(method = "loess")+
  ggtitle("Daily IndexP in SGP")

## 2. Cumulative indexP graph
GabindexCum <- GabindexRaw
GabindexCum[,3] <- cumsum(GabindexRaw[,3])

GabindexCum$date <- as.Date(paste0(GabindexCum$date, "-01"), format="%Y-%m-%d")
ggplot(data = GabindexCum, aes(x = date, y = IndexP))+
  scale_x_date(date_labels = "%Y.%m") + # %Y represents 4-digit year and $m represents the months as number (01-12), and its preceded by a dot. -> 2020.01 ~ 2020.12
  scale_y_continuous(breaks = seq(0, max(GabindexCum$IndexP, na.rm=TRUE), by = 100)) +
  geom_line()+
  theme_bw()+
  ggtitle("Cumulative IndexP in Gabon")

## 3. Cumulative FoI graph (bring FoI estimation from MCMC results)
annualFOI1 <- quantile(mcmcMatrix[,3], c(0.5))
annualFOI2 <- quantile(mcmcMatrix[,4], c(0.5))  # it's epidemic model which has 2 time-varying FOIs
FoIcum1 <- annualFOI1* (GabindexCum[,3]/max(GabonindexCum$IndexP))

GabFoIcum <- as.data.frame(matrix(NA, nrow = 365, ncol = 3))
colnames(GabFoIcum) <- c("id", "date", "FoI")
GabFoIcum$date <- as.Date(GabindexCum$date, origin = "1970-01-01")
GabFoIcum[,1] <- paste0("FoI", seq(1,365))
GabFoIcum[,2] <- Gaballoutput[,1]
GabFoIcum[,3] <- FoIcum1

GabFoIcum$date <- as.Date(paste0(GabFoIcum$date, "-01"), format="%Y-%m-%d")
ggplot(data = GabFoIcum, aes(x = date, y = FoI))+
  scale_x_date(date_labels = "%Y.%m") + # %Y represents 4-digit year and $m represents the months as number (01-12), and its preceded by a dot. -> 2020.01 ~ 2020.12
  geom_line()+
  scale_y_continuous(breaks = seq(0, max(GabFoIcum$FoI, na.rm=TRUE), by = 0.01)) +
  theme_bw()+
  ggtitle("Cumulative FoI1 in Gabon")

## 4. Scale down to daily FOI
GabFoIdaily <- as.data.frame(matrix(NA, nrow = 365, ncol = 3))
colnames(GabFoIdaily) <- c("id", "date", "FoI")
GabFoIdaily[,1] <- paste0("FoI", seq(1,365))
GabFoIdaily[,2] <- Gaballoutput[,1]
GabFoIdaily[1,3] <- GabFoIcum[1,3] # fill the first row

for(i in 1:nrow(GabFoIcum)){
  if(i == 1){
    GabFoIdaily[1,3] <- GabFoIcum[1,3]
  }else{
    GabFoIdaily[i,3] <- GabFoIcum[i, 3] - GabFoIcum[i-1, 3] 
  }
}   # rest are the consecutive differences 

# final daily time-step FoI
max_total <- max(GabFoIdaily$FoI, na.rm = TRUE)
breaks <- seq(0, max_total + (0.0001 - max_total %% 0.0001), by = 0.0001)

GabFoIdaily$date <- as.Date(paste0(GabFoIdaily$date, "-01"), format="%Y-%m-%d")
ggplot(data = GabFoIdaily, aes(x = date, y = FoI))+
  geom_line()+
  scale_x_date(date_labels = "%Y.%m") + # %Y represents 4-digit year and $m represents the months as number (01-12), and its preceded by a dot. -> 2020.01 ~ 2020.12
  scale_y_continuous(breaks = breaks) +
  geom_smooth(method = "loess")+
  theme_bw()+
  ggtitle("Daily FoI1 in Gabon")

## 5. Age-stratified susceptible population
sus_age <- 1 - outDf204_2

# birth cohort 2024: pop_size * susceptible
suspop <- sus_age * benin2020
suspop <- suspop[1:365,]
colnames(suspop) <- paste(0:100)
GabFoIdaily <- cbind(GabFoIdaily, suspop)

# daily infection: daily foi * suspop
for(i in 3:103){
  GabFoIdaily[paste0("case", i-3)] <- GabFoIdaily[,3] * GabFoIdaily[,i+1]
}
col_names <- paste0("case", 0:100)
rsums <- rowSums(GabFoIdaily[,col_names])
GabFoIdaily$Total <- rsums

GabFoIdaily$date <- as.Date(paste0(GabFoIdaily$date, "-01"), format="%Y-%m-%d")
max_total <- max(GabFoIdaily$Total, na.rm = TRUE)
breaks <- seq(0, max_total + (500 - max_total %% 500), by = 500)

ggplot(data = GabFoIdaily, aes(x = date, y = Total))+
  geom_line()+
  geom_smooth(method = "loess")+
  scale_y_continuous(breaks = breaks) +
  scale_x_date(date_labels = "%Y.%m") + # %Y represents 4-digit year and $m represents the months as number (01-12), and its preceded by a dot. -> 2020.01 ~ 2020.12
  scale_fill_viridis()+
  theme_bw()+
  ggtitle("Daily toal new infections in Gabon")

  