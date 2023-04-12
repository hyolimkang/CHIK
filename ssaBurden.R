# 1. Run after CatalyticSSA file 
rm(list = ls())    # remove any variables in R's memory 

#-------------------------------------------------------------------------------

beninSus  <- as.data.frame(1 - conDf_1)
colnames(beninSus) <- paste(0:100)

# import multiple population data 
benindata <- function(year) {
  sheet_name <- paste0(year, collapse = "")
  pop_data <- read_excel("ssa_pop.xlsx", sheet = sheet_name)  #calendar year data
  return(pop_data)
}             

benindata <- function(year) {
  sheet_name <- paste0(year, collapse = "")
  pop_data <- read_excel("benin_bcohort.xlsx", sheet = sheet_name) # birth cohort data
  return(pop_data)
}

datalist<- list()
for(i in 2011:2020){
  popdata <- benindata(year = i)
  datalist[[i-2010]] <- popdata
}
beninpoplist <- lapply(datalist, data.frame) # store all pop data in a list

for(i in 2011:2020){
  assign(paste0("benin",i), beninpoplist[[i-2010]])   # name each pop data (benin2011~benin2020)
}

# Pre-vacc burden FUNCTION -----------------------------------------------------
PreBurdenCalculation <- function(cohort, susprop, IncidenceDf) {
  
  # susceptible population (pre-vacc)
  suspop_prevacc <- cohort*susprop
  
  # burden in terms of number of cases (pre-vacc)
  burden_prevacc <- as.data.frame(suspop_prevacc*IncidenceDf)
  
  # return lists
  return(list(suspop_prevacc   = suspop_prevacc,
              burden_prevacc   = burden_prevacc))
  
}

#-------------------------------------------------------------------------------
# start the program
#-------------------------------------------------------------------------------

burden_list <- list()   # Create an empty list to store the burden data for each year

for (i in 1:10) {
  result      <- PreBurdenCalculation(cohort = beninpoplist[[i]], susprop = beninSus, IncidenceDf = conIncDf1)
  burden_list <- append(burden_list, result) # append the results of each iteration to the list 

}

for(j in 11:20) {
  assign(paste0("burden_prevacc",j), burden_list[[2*(j-10)]])  ## burdenlist has 2*10 elements, and prevacc burden elements are in the 2,4,~20 th elements
}


for(j in 11:20){
  assign(paste0("burden_prevacc",j), as.data.frame(get(paste0("burden_prevacc",j))))
}

list <- lapply(ls(pattern = "burden_prevacc"), get)

for(i in 1:10) {
  list[[i]]$tot <- rowSums(as.matrix(list[[i]]))
}

for (i in 11:20) {
  assign(paste0("burden_prevacc", i), as.data.frame(list[[i-10]]))
}

burden_prevacc11 <- as.data.frame(list[[1]])
burden_prevacc12 <- as.data.frame(list[[2]])
burden_prevacc13 <- as.data.frame(list[[3]])
burden_prevacc14 <- as.data.frame(list[[4]])
burden_prevacc15 <- as.data.frame(list[[5]])
burden_prevacc16 <- as.data.frame(list[[6]])
burden_prevacc17 <- as.data.frame(list[[7]])
burden_prevacc18 <- as.data.frame(list[[8]])
burden_prevacc19 <- as.data.frame(list[[9]])
burden_prevacc20 <- as.data.frame(list[[10]])

#-------------------------------------------------------------------------------
# tot burden dataframe from 2011~2022

preburdennames <- ls(pattern = "burden_prevacc*")
preburdenbenin <- lapply(preburdennames, get)

totburden <- NULL
for(i in seq_along(preburdenbenin)){
  burden    <- preburdenbenin[[i]]$tot
  if(is.null(totburden)) {
    totburden <- burden
  } else {
    totburden <- cbind(totburden, burden)
  }
}

colnames(totburden) <- paste(2011:2020)
totburden <- as.data.frame(totburden)
totburden$tot <- rowSums(totburden)


totburdenstack <- stack(totburden[,-11])

totburdenquant <- totburdenstack %>% 
                    group_by(ind) %>%
                      summarise(lo  = quantile(values, 0.025),
                                mid = quantile(values, 0.5),
                                hi  = quantile(values, 0.975))


ggplot(totburdenquant, aes(x = ind, y = mid, fill= as.numeric(ind))) +
  geom_bar(stat = "identity")+
  theme_bw()+
  scale_fill_viridis()+
  geom_linerange(aes(ymin = lo, ymax = hi))+
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::comma)

#-------------------------------------------------------------------------------
## incidence graph
quantconIncDf1 <- matrix(NA,nrow=ncol(conIncDf1), ncol = 3)
for(jj in 1:ncol(conIncDf1)){
  quantiles <- conIncDf1[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantconIncDf1[jj,] <- quantiles
  df_incBenin_1 <- cbind(ager, quantconIncDf1)
  df_incBenin_1 <- as.data.frame(df_incBenin_1)
  colnames(df_incBenin_1) <- c('agemid', 'mean', 'upper', 'lower')
  
}
df_incBenin_1[,2:4] <- 1000*df_incBenin_1[,2:4]

ggplot()+
  geom_line(data = df_incBenin_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_incBenin_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 20)+
  xlab("Age (years)") + ylab("Incident rate")+
  ggtitle("Incident rate per 1000 by age in Benin") +
  scale_fill_viridis(discrete = T) 

# sus prop graph
quantsusDf1 <- matrix(NA,nrow=ncol(beninSus), ncol = 3)
for(jj in 1:ncol(beninSus)){
  quantiles <- beninSus[,jj] %>% quantile(probs=c(.5,.025,.975))
  quantsusDf1[jj,] <- quantiles
  df_susBenin_1 <- cbind(ager, quantsusDf1)
  df_susBenin_1 <- as.data.frame(df_susBenin_1)
  colnames(df_susBenin_1) <- c('agemid', 'mean', 'upper', 'lower')
  
}

ggplot()+
  geom_line(data = df_susBenin_1, aes(x=agemid, y=mean), color = "#558C8C")+
  geom_ribbon(data = df_susBenin_1, alpha=0.2, aes(x=agemid, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("%")+
  ggtitle("Susceptible % Benin") +
  scale_fill_viridis(discrete = T) 











