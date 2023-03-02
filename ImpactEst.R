susceptible <- 1- df_upperLower_1[,2:4]
Infection   <- 1- susceptible           # initial values
 
time_sir <- seq(1, 80, by=1) # discrete time values
y_sir    <- matrix(data=NA, nrow= length(time_sir),
                   ncol=3)
y_sir[1,] <- c(0.9835779, 0.01642210, 0) #pre-allocate values

update_sir <- function(t, y, parms){
  
  lambda  <- parms['lambda1']
  
  out     <- c(-lambda*y[1]*y[2],
             lambda*y[1]*y[2],
             0)
  return(out)
}

parms_sir <- c(lambdaSample1)

update_sir(time_sir[2],
           y_sir[1,],
           parms_sir)  # checking function


for(i in 1:(nrow(y_sir)-1)){
  y_sir[i + 1,] <- y_sir[i,]+
    update_sir(time_sir[i + 1],
               y_sir[i, ],
               parms_sir)
} 


y_sir_df <- as.data.frame(y_sir)
names(y_sir_df) <- c('S',
                     'I',
                     'V')
y_sir_df <- as.data.frame(y_sir_df)

y_sir_stack <- stack(y_sir_df)

time <- rep(1:80, each = 1, times = 3)
time <- data.frame(time)
y_sir_stack <- cbind(y_sir_stack, time)
names(y_sir_stack) <- c('values',
                        'compartment',
                        'time')

# ggplot
ggplot(y_sir_stack, aes(x=time, y=values))+
  geom_line()+
  facet_wrap(~compartment)+
  theme_bw()



# for vaccine periods (10 yrs of duration)

time_sir <- seq(1, 20, by=1) # discrete time values
y_sir    <- matrix(data=NA, nrow= length(time_sir),
                   ncol=3)
y_sir[1,] <- c(0.9835779, 0.01642210, 0) #pre-allocate values

update_sir <- function(t, y, parms){
  
  lambda  <- parms['lambda1']
  
  for(i in 1:length(time_sir)){
    if(i<10){
      out <- c(-lambda*y[1]*y[2],
                lambda*y[1]*y[2],
                0)
    }else{
      out <- c(-lambda*y[1]*y[2] - y[1]*0.9*0.7,
               lambda*y[1]*y[2],
               y[1]*0.9*0.7)    
    }
  }
  return(out)
}

parms_sir <- c(lambdaSample1)

update_sir(time_sir[2],
           y_sir[1,],
           parms_sir)  # checking function


for(i in 1:(nrow(y_sir)-1)){
  y_sir[i + 1,] <- y_sir[i,]+
    update_sir(time_sir[i + 1],
               y_sir[i, ],
               parms_sir)
} 

y_sir_df <- as.data.frame(y_sir)
names(y_sir_df) <- c('S',
                     'I',
                     'V')
y_sir_df <- as.data.frame(y_sir_df)

y_sir_stack <- stack(y_sir_df)

time <- rep(1:20, each = 1, times = 3)
time <- data.frame(time)
y_sir_stack <- cbind(y_sir_stack, time)
names(y_sir_stack) <- c('values',
                        'compartment',
                        'time')

# ggplot
ggplot(y_sir_stack, aes(x=time, y=values))+
  geom_step()+
  facet_wrap(~compartment)+
  theme_bw()


# total periods

time_sir <- seq(21, 80, by=1) # discrete time values
y_sir    <- matrix(data=NA, nrow= length(time_sir),
                   ncol=3)
y_sir[1,] <- c(0.000000006061032,
               0.01684155,
               0.9831584) #pre-allocate values

update_sir <- function(t, y, parms){
  
  lambda  <- parms['lambda1']
  
  for(i in 1:length(time_sir)){
    
      out <- c(-lambda*y[1]*y[2] + lambda*y[3]*y[1],
               lambda*y[1]*y[2],
               -lambda*y[3]*y[1])    
  return(out)
  }
}

parms_sir <- c(lambdaSample1)

update_sir(time_sir[2],
           y_sir[1,],
           parms_sir)  # checking function


for(i in 1:(nrow(y_sir)-1)){
  y_sir[i + 1,] <- y_sir[i,]+
    update_sir(time_sir[i + 1],
               y_sir[i, ],
               parms_sir)
} 

y_sir_df_last <- as.data.frame(y_sir_df_last)
names(y_sir_df_last) <- c('S',
                     'I',
                     'V')
y_sir_stack_last <- stack(y_sir_df_last)

time <- rep(1:60, each = 1, times = 3)
time <- data.frame(time)
y_sir_stack_last <- cbind(y_sir_stack_last, time)
names(y_sir_stack_last) <- c('values',
                        'compartment',
                        'time')

# ggplot
ggplot(y_sir_stack_last, aes(x=time, y=values))+
  geom_step()+
  facet_wrap(~compartment)+
  theme_bw()



##  vaccination
susprop2022 <- 1-outDf_1
ager=0:78
Infection   <- df_upperLower_1[,2:4]
susceptible <- 1- df_upperLower_1[,2:4]

cols_to_multiply <- c(13)
sus2022Vacc <- susprop2022
sus2022Vacc[,13:79] <- sus2022Vacc[,13:79]*(1- 0.9*0.7)        # susceptible prop >12yrs old: susceptible*(1-ve*vc)
sus2022Vacc <- as.data.frame(sus2022Vacc)

ager=0:78
susMatrix <- matrix(NA,nrow=ncol(sus2022Vacc), ncol = 3)
for(jj in 1:ncol(sus2022Vacc)){
  quantiles <- sus2022Vacc[,jj] %>% quantile(probs=c(.5,.025,.975))
  susMatrix[jj,] <- quantiles
  susDf <- cbind(ager, susMatrix)
  susDf <- as.data.frame(susDf)
  colnames(susDf) <- c('agemid', 'mean', 'lower', 'upper')
}

rows_to_multiply <- c(12)

vacc <- susDf[,2:4]
vacc[1:11,] <-0
vacc[12,] <- susceptible[rows_to_multiply,]*0.9*0.7
vacc[13:79,] <- vacc[12,]

vaccWide <- sus2022Vacc
vaccWide[,1:12] <-0
vaccWide[,13] <-susprop2022[,13]*0.9*0.7
vaccWide[,13:79] <- vaccWide[,13]
colnames(vaccWide) <- paste(0:78)


natInf <- 1- (susDf[,2:4]+vacc[,1:3])

ggplot()+
  geom_line(data = susDf, aes(x=ager, y=mean), color = "#558C8C")+
  geom_ribbon(data = susDf, alpha=0.2, aes(x=ager, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("SIV: vaccination at 12yrs old (lifelong)")+
  geom_line(data = vacc, aes(x=ager, y=mean), color = "blue")+
  geom_ribbon(data = vacc, alpha=0.2, aes(x=ager, y=mean, ymin=lower, ymax=upper), fill = "blue")+
  geom_line(data = natInf, aes(x=ager, y=mean), color = "#FF00FF")+
  geom_ribbon(data = natInf, alpha=0.2, aes(x=ager, y=mean, ymin=lower, ymax=upper), fill = "#FF00FF")+
  geom_line(data = df_upperLower_1, aes(x=ager, y=mean), color = "black")+
  geom_ribbon(data = df_upperLower_1, alpha=0.2, aes(x=ager, y=mean, ymin=lower, ymax=upper), fill = "black")

# cohort projection
epidemic_FOI_Oli <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/epidemic_FOI_Oli.xlsx", 
                               sheet = "2022cohort")
View(epidemic_FOI_Oli)

thai2022cohort <- epidemic_FOI_Oli*1000

susprop2022 <- 1- outDf_1

ThaiSuspop2022 <- thai2022cohort*susprop2022

burden2022BV <- as.data.frame(ThaiSuspop2022*IncidenceDf1)
burden2022BV <- stack(burden2022BV)
burden2022BV$values

sus2022Vacc <- susprop2022
colnames(sus2022Vacc) <- paste(0:78)

sus2022Vacc[,13:79] <- sus2022Vacc[,13:79]*(1- 0.9*0.7)
sus2022Vacc <- as.data.frame(sus2022Vacc)


ThaiSuspop2022Vacc <- as.data.frame(sus2022Vacc*thai2022cohort)

## burden calculation based on the proportion graph

IncidenceDf1Vacc         <- IncidenceDf1
IncidenceDf1Vacc[,13:79] <- IncidenceDf1[,13:79]*(1-0.7*0.9)


burden2022cohort <- susprop2022
burden2022cohort <-  ThaiSuspop2022Vacc*IncidenceDf1   

burden2022cohort <- as.data.frame(burden2022cohort)
colnames(burden2022cohort) <- paste(0:78)

burden2022cohort <- stack(burden2022cohort)
burden2022cohort$values

combined <- bind_rows(
  data.frame(data = "burden2022cohort", x= factor(burden2022cohort$ind), y= burden2022cohort$values),
  data.frame(data = "burden2022BV", x = factor(burden2022BV$ind), y = burden2022BV$values)
)


Impact <- burden2022BV - burden2022cohort  ## before stack
colnames(Impact) <- paste(2022:2100)
Impact$sum <- rowSums(Impact[,1:78])
ImpactStack <- stack(Impact)

compute_quantiles_df <- function(x, probs) {
  
  # Compute quantiles of x
  q <- quantile(x, probs = probs)
  
  # Store quantile results into a dataframe
  df <- data.frame(quantiles = paste0("Q", 1:length(q)), value = q)
  
  # Return the dataframe
  return(df)
}

Impact.df    <- compute_quantiles_df(Impact$sum, probs=c(0.025,0.5,0.975))

# NNV calculation
## fully vaccinated pop: vaccinated pop (%) * total population by each age of cohort
full.vacc.pop <- vaccWide*thai2022cohort
full.vacc.pop$tot.full <- rowSums(full.vacc.pop[,13:79])
## NNV = raw number of case averted / fully vaccinated op
cases.averted.perFV    <- Impact/full.vacc.pop  # before stack Impact
cases.averted.per1000FV <- 1000*cases.averted.perFV
NNV.per1000FV <- 1000/cases.averted.per1000FV  # to prevent 1 case in 1000 FVP ~ 

full.vacc.pop.df <- compute_quantiles_df(full.vacc.pop$tot.full, c(0.025,0.5,0.975))
 
#total value 
quantile(NNV.per1000FV$sum, c(0.025,0.5,0.975))

NNVStack <- stack(NNV.per1000FV[,13:79])
NNVquantile <- sapply(12:78, function(x) {
  subset_df  <- subset(NNVStack, ind==x)
  quantile <- quantile(subset_df$values, 0.5)
  c(quantile)
})

NNVdf <- data.frame(ind    = 12:78, 
                    NNV    = NNVquantile)

NNVdf$year <- c(2034:2100)

ggplot(NNVdf, aes(x = year, y = NNV, fill = year)) + 
  geom_bar(stat = "identity",position = position_stack(reverse = TRUE)) + 
  labs(x = "year", y = "NNV", title = "NNV for 2022 cohort")+
  theme_light()+
  scale_y_continuous(labels = scales::number_format(big.mark = ','))


##differences 

options(scipen = 999)
Rawstacked <- cbind(burden2022BV$values, burden2022cohort$values, ImpactStack) 
colnames(Rawstacked) <- c("before","after","impact","year")

Diffonly <- Rawstacked[,c("after","impact","year")]
Diffonly$year <- as.numeric(Diffonly$year)
Diffonly <- Diffonly %>% filter(Diffonly$year > 12)
quantile <- sapply(13:79, function(x) {
  subset_df  <- subset(Diffonly, year==x)
  quantile_1 <- quantile(subset_df$after, 0.5)
  quantile_2 <- quantile(subset_df$impact, 0.5)
  c(quantile_1, quantile_2)
})

new_df <- data.frame(year   = 13:79, 
                     after  = quantile[1,],
                     impact = quantile[2,])
colSums(new_df[1:67,])

df_long <- pivot_longer(new_df, cols = c("after", "impact"), names_to = "Variable", 
                        values_to = "Value")

# Create stacked bar graph
ggplot(df_long, aes(x = year, y = Value, fill = Variable)) + 
  geom_bar(stat = "identity",position = position_stack(reverse = TRUE)) + 
  labs(x = "Category", y = "Value", title = "2022 cohort vaccine impact")+
  theme_light()+
  scale_y_continuous(labels = scales::number_format(big.mark = ','))

###

ggplot(combined, aes(x = x, y = y, fill = x)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA) +
  facet_wrap(~ data, ncol = 2) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 7)) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Burden of 2022 cohort (Before/After vaccination)")


ggplot() +
  geom_boxplot(data = burden2022cohort, aes(x = ind, y = values, fill = ind)) +  # Change filling color
  geom_boxplot(data = burden2022BV, aes(x = ind, y = values, fill = ind), outlier.shape = NA) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Burden of 2022 cohort") +
  guides(fill = "none") +
  theme_ipsum(plot_title_size = 10) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 7))


# cohort projection
epidemic_FOI_Oli <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/epidemic_FOI_Oli.xlsx", 
                               sheet = "2023cohort")
View(epidemic_FOI_Oli)

thai2023cohort <- epidemic_FOI_Oli*1000

susprop2023 <- 1- outDf_1[,1:78]

ThaiSuspop2023 <- thai2023cohort*susprop2023

burden2023BV <- as.data.frame(ThaiSuspop2023*IncidenceDf1)
burden2023BV <- stack(burden2023BV)
burden2023BV$values


cols_to_multiply <- c(12)
sus2023Vacc <- susprop2023
sus2023Vacc[,12:78] <- sus2023Vacc[,12:78]*(1- 0.9*0.7)
sus2023Vacc <- as.data.frame(sus2023Vacc)


ThaiSuspop2023Vacc <- as.data.frame(sus2023Vacc*thai2023cohort)

## burden calculation based on the proportion graph

IncidenceDf1Vacc         <- IncidenceDf1[,1:78]
IncidenceDf1Vacc[,12:78] <- IncidenceDf1[,12:78]*(1-0.7*0.9)


burden2023cohort <- susprop2023
burden2023cohort <-  ThaiSuspop2023Vacc*IncidenceDf1[,1:78]  
burden2023cohort[,12:78] <- ThaiSuspop2023Vacc[,12:78]*IncidenceDf1Vacc[,12:78]

burden2023cohort <- as.data.frame(burden2023cohort)
colnames(burden2023cohort) <- paste(0:77)

burden2023cohort <- stack(burden2023cohort)
burden2023cohort$values

combined <- bind_rows(
  data.frame(data = "burden2023cohort", x= factor(burden2023cohort$ind), y= burden2023cohort$values),
  data.frame(data = "burden2023BV", x = factor(burden2023BV$ind), y = burden2023BV$values)
)


Impact23 <- burden2023BV - burden2023cohort  ## before stack
colnames(Impact23) <- paste(2023:2100)
Impact23$sum <- rowSums(Impact23[,1:77])
ImpactStack23 <- stack(Impact23)



##differences 

options(scipen = 999)
Rawstacked <- cbind(burden2023BV$values, burden2023cohort$values, ImpactStack23) 
colnames(Rawstacked) <- c("before","after","impact","year")

Diffonly <- Rawstacked[,c("after","impact","year")]
Diffonly$year <- as.numeric(Diffonly$year)
Diffonly <- Diffonly %>% filter(Diffonly$year > 11)
quantile <- sapply(12:78, function(x) {
  subset_df  <- subset(Diffonly, year==x)
  quantile_1 <- quantile(subset_df$after, 0.5)
  quantile_2 <- quantile(subset_df$impact, 0.5)
  c(quantile_1, quantile_2)
})
new_df <- data.frame(year   = 12:78, 
                     after  = quantile[1,],
                     impact = quantile[2,])

df_long <- pivot_longer(new_df, cols = c("after", "impact"), names_to = "Variable", 
                        values_to = "Value")

colSums(new_df[1:67,])
row <- c(NA,NA,164042.83 )
new_df <- rbind(new_df, row)

# Create stacked bar graph
ggplot(df_long, aes(x = year, y = Value, fill = Variable)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + 
  labs(x = "Category", y = "Value", title = "2023 cohort vaccine impact")+
  theme_light()+
  scale_y_continuous(labels = scales::number_format(big.mark = ','))




# vaccine protection is only 10 years
 
vacc10 <- susDf[,2:4]
vacc10[1:11,] <-0
vacc10[12,] <- susceptible[rows_to_multiply,]*0.9*0.7
vacc10[13:21,] <- vacc10[12,]
for(i in 22:80){
  vacc10[i,] <- exp(-lambdaSample1*i)*(0.9*0.7)
}


Diff <- vacc-vacc10

susVacc10 <- susDf[,2:4]+Diff

natInf10 <- 1-(susVacc10+vacc10)

ggplot()+
  geom_line(data = susVacc10, aes(x=ager, y=mean), color = "#558C8C")+
  geom_ribbon(data = susVacc10, alpha=0.2, aes(x=ager, y=mean, ymin=lower, ymax=upper), fill = "#558C8C")+
  theme_ipsum(plot_title_size = 10)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 1)+
  xlab("Age (years)") + ylab("Proportion Seropositive")+
  ggtitle("SIV: vaccination at 12yrs old (10yrs protection)")+
  geom_line(data = vacc10, aes(x=ager, y=mean), color = "blue")+
  geom_ribbon(data = vacc10, alpha=0.2, aes(x=ager, y=mean, ymin=lower, ymax=upper), fill = "blue")+
  geom_line(data = natInf10, aes(x=ager, y=mean), color = "#FF00FF")+
  geom_ribbon(data = natInf10, alpha=0.2, aes(x=ager, y=mean, ymin=lower, ymax=upper), fill = "#FF00FF")
  




