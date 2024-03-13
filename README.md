# CHIK
In this systematic review, meta-analysis, and modelling study, we searched PubMed, Ovid, and Web of Science for articles published from database inception until Sept 26, 2022, for prospective and retrospective cross-sectional studies that addressed serological chikungunya virus infection in any geographical region, age group, and population subgroup and for longitudinal prospective and retrospective cohort studies with data on chronic chikungunya or hospital admissions in people with chikungunya. We did a systematic review of studies on chikungunya seroprevalence and fitted catalytic models to each survey to estimate location-specific FOI (ie, the rate at which susceptible individuals acquire chikungunya infection). We performed a meta-analysis to estimate the proportion of symptomatic patients with laboratory-confirmed chikungunya who had chronic chikungunya or were admitted to hospital following infection. We used a random-effects model to assess the relationship between chronic sequelae and follow-up length using linear regression.

## FoI estimation

1. DIC test for each survey

   test both constant and time-varying models for each survey and select a model that has the lowest DIC value. 
   - constant FoI model
   𝑃(𝑎,𝑡)=1 −𝑒𝑥𝑝⁡(−𝜆 ∗𝑎)
   - Time-varying FoI model (N epidemics)
   𝑃(𝑎,𝑡)= 1 −𝑒𝑥𝑝⁡(−∑_(𝑖=1)^𝑛 𝜆_i)

3. Conduct Bayesian MCMC to generate full-posterior distributions of FoI
4. Fit seroprevalence curve as below example
   - Constant FoI (image1)
   - Time-varying FoI (image2) 

![image](https://github.com/hyolimkang/CHIK/assets/66198338/bcdf07f5-47e5-4e5e-96c2-7963594c5dcd)
![image](https://github.com/hyolimkang/CHIK/assets/66198338/51617a04-5696-47b8-86a1-65ca14ace8ad)

4. Estimate the average annual long-term FoI for each survey
   - For each time-varying FoI, conduct 10000 random simulations for 100 yrs based on annual outbreak probability (=1/(average inter-epidemic period)).
   - Each column represents a random simulation and each row represents year (spanning from year 1 - 100)
   - Estimate average FoI for 100 years for each 1000 column and estiamte 95% UI of long-term average annual FoI
![image](https://github.com/hyolimkang/CHIK/assets/66198338/81a565b3-6bac-4e2d-b823-0d4bb5f4eb77)
