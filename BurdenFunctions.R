#-------------------------------------------------------------------------------
#function for estimating burden proportion (pre/post vacc)
#-------------------------------------------------------------------------------
# remove all objects from workspace
remove (list = objects() )
# source file from parent R script 

source("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/CatalyticEastAsiaEndemic.R")
source("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/ImpactEst.R")

#-------------------------------------------------------------------------------
print (Sys.time ())
 
BurdenCalculation <- function(cohort, susprop, IncidenceDf) {
  
  # vaccinated population
  vacc           <- susprop
  vacc[,1:12]    <- 0
  vacc[,13]      <- susprop[,13]*0.9*0.7
  vacc[,13:79]   <- vacc[,13]
  colnames(vacc) <- paste(0:78)

  # susceptible population (pre-vacc)
  suspop_prevacc <- cohort*susprop
  
  # burden in terms of number of cases (pre-vacc)
  burden_prevacc <- as.data.frame(suspop_prevacc*IncidenceDf)
  
  # post-vacc susceptible proportion
  susprop_postvacc <- susprop
  susprop_postvacc[,13:79] <- susprop_postvacc[,13:79]*(1- 0.9*0.7)        # susceptible prop >12yrs old: susceptible*(1-ve*vc)
  susprop_postvacc <- as.data.frame(susprop_postvacc)
  colnames(susprop_postvacc) <- paste(0:78)
  
  # susceptible population (post-vacc)
  suspop_postvacc <- as.data.frame(susprop_postvacc*cohort)
  
  # burden in terms of number of cases (post-vacc)
  burden_postvacc <- susprop
  burden_postvacc <- suspop_postvacc*IncidenceDf   
  
  # Impact
  Impact <- burden_prevacc - burden_postvacc  
  
  # return lists
  return(list(suspop_prevacc   = suspop_prevacc,
              burden_prevacc   = burden_prevacc,
              susprop_postvacc = susprop_postvacc,
              suspop_postvacc  = suspop_postvacc,
              burden_postvacc  = burden_postvacc,
              Impact           = Impact,
              vacc             = vacc))
  
}


#-------------------------------------------------------------------------------
# Function for storing burden estimates into a csv file 
#-------------------------------------------------------------------------------
allburden <- function(cohort, burden_prevacc, burden_postvacc, IncidenceDf,
                      susprop_prevacc, susprop_postvac) {
  # tyding prevacc data -- making it as a long-format data
  prevacc            <- stack(burden_prevacc)
  prevacc$scenario   <- c("prevacc") 
  prevacc$run_id     <- rep(1:1000,times=79)
  postvacc           <- stack(burden_postvacc)
  postvacc$scenario  <- c("postvacc") 
  postvacc$run_id    <- rep(1:1000,times=79)
  burden             <- rbind(prevacc,postvacc)

  # tyding incidence data -- making it as a long-format data  
  IncidenceDf               <- as.data.frame(IncidenceDf)
  inc.chik                  <- stack(IncidenceDf)
  colnames(inc.chik)        <-c("incidence","age")
  burden                    <- cbind(burden, inc.chik$incidence)
  cohort.size               <- stack(cohort)
  colnames(cohort.size)     <- c("cohort_size","age")
  burden                    <- cbind(burden,cohort.size$cohort_size)

  # tyding susceptible data -- making it as a long-format data
  susprop_prevacc           <- as.data.frame(susprop_prevacc)
  susprop_prevacc           <- stack(susprop_prevacc)
  susprop_prevacc$scenario  <- c("prevacc")
  susprop_postvacc          <- stack(susprop_postvacc)
  susprop_postvacc$scenario <- c("postvacc")
  susprop                   <- rbind(susprop_prevacc,susprop_postvacc)
  colnames(susprop)         <- c("susprop","age","scenario")
  burden                    <- cbind(burden,susprop$susprop)
  colnames(burden)          <- c("burden","age","scenario","run_id","incidence","cohort_size","susprop")
  burden                    <- burden[,c("scenario","age","run_id","cohort_size",
                                           "incidence","burden","susprop")]
  setDT(burden)
  
  # return
  return(burden = burden)
  
}


#-------------------------------------------------------------------------------
# start the program 
#-------------------------------------------------------------------------------

# 1. burden for 2022

thaicohort22 <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/epidemic_FOI_Oli.xlsx", 
                           sheet = "2022cohort")
thaicohort22 <- 1000*thaicohort22

burden22 <- BurdenCalculation(cohort      = thaicohort22,
                              susprop     = thaisusprop,
                              IncidenceDf = IncidenceDf1)

burden22Df           <- lapply(burden22, as.data.frame)
suspop_prevacc22     <- burden22Df[[1]]
burden_prevacc22     <- burden22Df[[2]]
susprop_postvacc22   <- burden22Df[[3]]
suspop_postvacc22    <- burden22Df[[4]]
burden_postvacc22    <- burden22Df[[5]]
Impact22             <- burden22Df[[6]]

# 2. burden for 2023

thaicohort23 <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/epidemic_FOI_Oli.xlsx", 
                           sheet = "2023cohort")
thaicohort23 <- 1000*thaicohort23

burden23 <- BurdenCalculation(cohort      = thaicohort23,
                              susprop     = thaisusprop,
                              IncidenceDf = IncidenceDf1)

burden23Df           <- lapply(burden23, as.data.frame)
suspop_prevacc23     <- burden23Df[[1]]
burden_prevacc23     <- burden23Df[[2]]
susprop_postvacc23   <- burden23Df[[3]]
suspop_postvacc23    <- burden23Df[[4]]
burden_postvacc23    <- burden23Df[[5]]
Impact23             <- burden23Df[[6]]


result_burden23 <- allburden(cohort          = thaicohort23,
                             burden_prevacc  = burden_prevacc23, 
                             burden_postvacc = burden_postvacc23, 
                             IncidenceDf     = IncidenceDf1,
                             susprop_prevacc = thaisusprop22, 
                             susprop_postvac = susprop_postvacc23)

# 3. burden for 2024 
thaicohort24 <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/epidemic_FOI_Oli.xlsx", 
                           sheet = "2024cohort")
thaicohort24 <- 1000*thaicohort24

burden24 <- BurdenCalculation(cohort      = thaicohort24,
                              susprop     = thaisusprop,
                              IncidenceDf = IncidenceDf1)

burden24Df           <- lapply(burden24, as.data.frame)
suspop_prevacc24     <- burden24Df[[1]]
burden_prevacc24     <- burden24Df[[2]]
susprop_postvacc24   <- burden24Df[[3]]
suspop_postvacc24    <- burden24Df[[4]]
burden_postvacc24    <- burden24Df[[5]]
Impact24             <- burden24Df[[6]]


result_burden24 <- allburden(cohort          = thaicohort24,
                             burden_prevacc  = burden_prevacc24, 
                             burden_postvacc = burden_postvacc24, 
                             IncidenceDf     = IncidenceDf1,
                             susprop_prevacc = thaisusprop22, 
                             susprop_postvac = susprop_postvacc24)

# 4. burden for 2025 
thaicohort25 <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/epidemic_FOI_Oli.xlsx", 
                           sheet = "2025cohort")
thaicohort25 <- 1000*thaicohort25

burden25 <- BurdenCalculation(cohort      = thaicohort25,
                              susprop     = thaisusprop,
                              IncidenceDf = IncidenceDf1)

burden25Df           <- lapply(burden25, as.data.frame)
suspop_prevacc25     <- burden25Df[[1]]
burden_prevacc25     <- burden25Df[[2]]
susprop_postvacc25   <- burden25Df[[3]]
suspop_postvacc25    <- burden25Df[[4]]
burden_postvacc25    <- burden25Df[[5]]
Impact25             <- burden25Df[[6]]


result_burden25 <- allburden(cohort          = thaicohort25,
                             burden_prevacc  = burden_prevacc25, 
                             burden_postvacc = burden_postvacc25, 
                             IncidenceDf     = IncidenceDf1,
                             susprop_prevacc = thaisusprop, 
                             susprop_postvac = susprop_postvacc25)


#------------------------------------------------------------------------------- 
#save result burden files into a csv 
#-------------------------------------------------------------------------------
result <- c("thai22","thai23","thai24","thai25")
resultlist <- list()
for(i in seq_along(result)) {
  filepath <- file.path("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/dataresults", paste0(result[i], "_results.csv")) 
  # append the filepath to the list
  resultlist[[i]] <- filepath
}

fwrite(result_burden22, resultlist[[1]])
fwrite(result_burden23, resultlist[[2]])
fwrite(result_burden24, resultlist[[3]])
fwrite(result_burden25, resultlist[[4]])


#-------------------------------------------------------------------------------
#combine burden estimates
#-------------------------------------------------------------------------------

combine_burden_estimate <- function () {
  
  # simulation scenarios
  results = c("thai22", "thai23", "thai24","thai25")
  
  # burden estimates for different simulation scenarios
  allburden <- NULL
  for (i in 1:length(results)) {
    
    # read burden estimate of one simulation scenario
    burdenfile <- paste0 ("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/dataresults/", results[i], "_results.csv")
    burden <- fread (burdenfile, header = "auto", stringsAsFactors = F)
    
    # set scenario number
    burden [, bcohort := results[i]]
    
    # combine burden estimate of this simulation scenario 
    # to other simulation scenarios
    if (is.null(allburden)) {
      allburden <- burden
    } else {
      allburden <- rbind (allburden, burden)
    }
  }
  # set to data table
  setDT (allburden)
  # return comnbined burden estimates from all simulation scenarios
  return (allburden)
  
} # end of function -- combine_burden_estimate

allburden <- combine_burden_estimate()

fwrite(allburden, "D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/dataresults/allburden.csv")


#-------------------------------------------------------------------------------
#add burden calculation 
#-------------------------------------------------------------------------------


# Add columns for cases per 1000
allburden[, cases_p1000 := burden/cohort_size * 1000,
       by =. (run_id, scenario, bcohort)]
allburden[, totburden   := sum(burden),
       by =.(run_id, scenario, bcohort)]

allburden[, totcohort  := sum(cohort_size),
       by =.(run_id,scenario, bcohort)]

allburden[, cases_p1000_tot := totburden / totcohort * 1000,
       by =.(run_id,scenario, bcohort)]

allburden[, suspop := cohort_size*susprop,
       by = . (run_id, scenario, bcohort)]

allburden[scenario == "postvacc", fvp := suspop*0.9*0.7,
       by =. (run_id,scenario, bcohort)]

allburden[, totvacc := sum(fvp),
       by =.(run_id,scenario)]

preburden  <- allburden[scenario == "prevacc"]
postburden <- allburden[scenario == "postvacc"]

vaccine_impact <- preburden [postburden, on = .(run_id = run_id, age=age, bcohort = bcohort)] 

vaccine_impact[, `:=` (cases_averted = burden - i.burden)]
vaccine_impact[, `:=` (case_averted_perVG = cases_averted / i.fvp * 1000)]
vaccine_impact[, `:=` (totcases_averted = totburden - i.totburden)]
vaccine_impact[, `:=` (totcases_averted_perVG = totcases_averted / i.totvacc * 1000)]


fwrite(allburden, "D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/dataresults/allburden.csv")
