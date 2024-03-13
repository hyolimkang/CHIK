# CHIK
In this systematic review, meta-analysis, and modelling study, we searched PubMed, Ovid, and Web of Science for articles published from database inception until Sept 26, 2022, for prospective and retrospective cross-sectional studies that addressed serological chikungunya virus infection in any geographical region, age group, and population subgroup and for longitudinal prospective and retrospective cohort studies with data on chronic chikungunya or hospital admissions in people with chikungunya. We did a systematic review of studies on chikungunya seroprevalence and fitted catalytic models to each survey to estimate location-specific FOI (ie, the rate at which susceptible individuals acquire chikungunya infection). We performed a meta-analysis to estimate the proportion of symptomatic patients with laboratory-confirmed chikungunya who had chronic chikungunya or were admitted to hospital following infection. We used a random-effects model to assess the relationship between chronic sequelae and follow-up length using linear regression.

## FoI estimation

1. DIC test for each survey
   - constant FoI model
   - Time-varying FoI model (N epidemics)
2. Conduct Bayesian MCMC to generate full-posterior distributions of FoI
3. Fit seroprevalence curve as below example

![image](https://github.com/hyolimkang/CHIK/assets/66198338/bcdf07f5-47e5-4e5e-96c2-7963594c5dcd)

4. Estimate the average annual long-term FoI for each survey
   - For each time-varying FoI, conduct 10000 random simulations for 100 yrs based on annual outbreak probability (=1/(average inter-epidemic period)).
   - Each column represents a random simulation and each row represents year (spanning from year 1 - 100)
   - Estimate average FoI for 100 years for each 1000 column and estiamte 95% UI of long-term average annual FoI
![image](https://github.com/hyolimkang/CHIK/assets/66198338/81a565b3-6bac-4e2d-b823-0d4bb5f4eb77)
