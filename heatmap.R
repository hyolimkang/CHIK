library(pheatmap)
library(meta)
library(rmarkdown)
library(metafor)
library(ggplot2)
library(dplyr)
# no dendogram
# https://r-charts.com/correlation/pheatmap/
  

library(readxl)
chik_systematic_review_v1 <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/chik_systematic_review_v1.xlsx", 
                                        sheet = "Sheet6")
View(chik_systematic_review_v1)

df_chik <- as.matrix(chik_systematic_review_v1)
rownames(df_chik) <- c("East Asia and Pacific","Europe and Central Asia","Latin America & Carribean",
                       "Middle East", "South Asia", "Sub Saharan Africa")
pheatmap(df_chik, display_numbers = TRUE, number_color = "black",
         fontsize_number = 5,
         cluster_rows = FALSE,
         cluster_cols = FALSE)

library(readxl)
CountryModel <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "Sheet3")
View(CountryModel)

library(readxl)
CountryModel <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "meta")
CountryModel <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "meta")

View(CountryModel)

data1 <- CountryModel %>% filter(subgroup != "Europe & Central Asia")
data1 <- data1 %>% filter(subgroup != "South Asia")
data1 <- data1 %>% filter(subgroup != "Sub-Saharan Africa")
data2 <- CountryModel %>% filter(subgroup != "East Asia & Pacific")
data2 <- data2 %>% filter(subgroup != "Latin America & Caribbean")
data2 <- data2 %>% filter(subgroup != "Middle East & North Africa")

# only igg & non-fever
IgGonly <- CountryModel %>% filter(antibody == 1 & age_stratified == 1)
IgGonly_SSA <- IgGonly %>% filter(subgroup == "Sub-Saharan Africa")
IgG_SSA_nfvr <- IgGonly %>% filter(subgroup == "Sub-Saharan Africa" & pop_group == "non fever")
IgG_SSA_fvr <- IgGonly %>% filter(subgroup == "Sub-Saharan Africa" & pop_group == "fever")
IgGonly_LAC <- IgGonly %>% filter(subgroup == "Latin America & Caribbean")
IgG_LAC_nfvr <- IgGonly %>% filter(subgroup == "Latin America & Caribbean" & pop_group == "non fever")
IgG_LAC_fvr <- IgGonly %>% filter(subgroup == "Latin America & Caribbean" & pop_group == "fever")
IgGonly_EAP <- IgGonly %>% filter(subgroup == "East Asia & Pacific")
IgG_EAP_nfvr <- IgGonly %>% filter(subgroup == "East Asia & Pacific" & pop_group == "non fever")
IgG_EAP_fvr <- IgGonly %>% filter(subgroup == "East Asia & Pacific" & pop_group == "fever")

meta_dat1 <- metaprop(positive.n, total.n, sm="PLN", data=IgG_SSA_nfvr, studlab=paste(study,year), comb.fixed=F,
                    byvar = subgroup, bylab = "Setting", byseparator=":")
meta_dat2 <- metaprop(positive.n, total.n, sm="PLN", data=IgG_SSA_fvr, studlab=paste(study,year), comb.fixed=F,
                      byvar = subgroup, bylab = "Setting", byseparator=":")

meta_dat3 <- metaprop(positive.n, total.n, sm="PLN", data=IgG_LAC_nfvr, studlab=paste(study,year), comb.fixed=F,
                      byvar = subgroup, bylab = "Setting", byseparator=":")
meta_dat4 <- metaprop(positive.n, total.n, sm="PLN", data=IgG_LAC_fvr, studlab=paste(study,year), comb.fixed=F,
                      byvar = subgroup, bylab = "Setting", byseparator=":")
meta_dat5 <- metaprop(positive.n, total.n, sm="PLN", data=IgG_EAP_nfvr, studlab=paste(study,year), comb.fixed=F,
                      byvar = subgroup, bylab = "Setting", byseparator=":")
meta_dat6 <- metaprop(positive.n, total.n, sm="PLN", data=IgG_EAP_fvr, studlab=paste(study,year), comb.fixed=F,
                      byvar = subgroup, bylab = "Setting", byseparator=":")


forest <- forest(meta_dat1, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                 text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                 leftlabs=c("Study","Country", "Positive","Total"),
                 leftcols = c("study", "country", "positive.n", "total.n"))

forest1 <- forest(meta_dat2, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                  text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                  leftlabs=c("Study","Country", "Positive","Total"),
                  leftcols = c("study", "country", "positive.n", "total.n"))

forest3 <- forest(meta_dat3, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                 text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                 leftlabs=c("Study","Country", "Positive","Total"),
                 leftcols = c("study", "country", "positive.n", "total.n"))

forest4 <- forest(meta_dat4, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                  text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                  leftlabs=c("Study","Country", "Positive","Total"),
                  leftcols = c("study", "country", "positive.n", "total.n"))
forest5 <- forest(meta_dat5, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                  text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                  leftlabs=c("Study","Country", "Positive","Total"),
                  leftcols = c("study", "country", "positive.n", "total.n"))

forest6 <- forest(meta_dat6, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                  text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                  leftlabs=c("Study","Country", "Positive","Total"),
                  leftcols = c("study", "country", "positive.n", "total.n"))

# overall seropreve
brazil <- CountryModel %>% filter(country == "Brazil")
brazil_dat <- metaprop(positive.n, total.n, sm="PLN", data=brazil, studlab=paste(study,year), comb.fixed=F,
                      byvar = subgroup, bylab = "Setting", byseparator=":")
brazil_forest <- forest(brazil_dat, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                  text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                  leftlabs=c("Study","Country", "Positive","Total"),
                  leftcols = c("study", "country", "positive.n", "total.n"))

ethiopia <- CountryModel %>% filter(country == "Ethiopia")
ethi_dat <- metaprop(positive.n, total.n, sm="PLN", data=ethiopia, studlab=paste(study,year), comb.fixed=F,
                       byvar = subgroup, bylab = "Setting", byseparator=":")
ethi_forest <- forest(ethi_dat, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                        text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                        leftlabs=c("Study","Country", "Positive","Total"),
                        leftcols = c("study", "country", "positive.n", "total.n"))

india <- CountryModel %>% filter(country == "India")
india_dat <- metaprop(positive.n, total.n, sm="PLN", data=india, studlab=paste(study,year), comb.fixed=F,
                     byvar = subgroup, bylab = "Setting", byseparator=":")
india_forest <- forest(india_dat, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                      text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                      leftlabs=c("Study","Country", "Positive","Total"),
                      leftcols = c("study", "country", "positive.n", "total.n"))

indonesia <- CountryModel %>% filter(country == "Indonesia")
indonesia_dat <- metaprop(positive.n, total.n, sm="PLN", data=indonesia, studlab=paste(study,year), comb.fixed=F,
                      byvar = subgroup, bylab = "Setting", byseparator=":")
indonesia_forest <- forest(indonesia_dat, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                       text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                       leftlabs=c("Study","Country", "Positive","Total"),
                       leftcols = c("study", "country", "positive.n", "total.n"))

mexico <- CountryModel %>% filter(country == "Mexico")
mexico_dat <- metaprop(positive.n, total.n, sm="PLN", data=mexico, studlab=paste(study,year), comb.fixed=F,
                          byvar = subgroup, bylab = "Setting", byseparator=":")
mexico_forest <- forest(mexico_dat, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                           text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                           leftlabs=c("Study","Country", "Positive","Total"),
                           leftcols = c("study", "country", "positive.n", "total.n"))

kenya <- CountryModel %>% filter(country == "Kenya")
kenya_dat <- metaprop(positive.n, total.n, sm="PLN", data=kenya, studlab=paste(study,year), comb.fixed=F,
                       byvar = subgroup, bylab = "Setting", byseparator=":")
kenya_forest <- forest(kenya_dat, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                        text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                        leftlabs=c("Study","Country", "Positive","Total"),
                        leftcols = c("study", "country", "positive.n", "total.n"))

tanzania <- CountryModel %>% filter(country == "Tanzania")
tanzania_dat <- metaprop(positive.n, total.n, sm="PLN", data=tanzania, studlab=paste(study,year), comb.fixed=F,
                      byvar = subgroup, bylab = "Setting", byseparator=":")
tanzania_forest <- forest(tanzania_dat, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                       text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                       leftlabs=c("Study","Country", "Positive","Total"),
                       leftcols = c("study", "country", "positive.n", "total.n"))

mozambique <- CountryModel %>% filter(country == "Mozambique")
mozambique_dat <- metaprop(positive.n, total.n, sm="PLN", data=mozambique, studlab=paste(study,year), comb.fixed=F,
                         byvar = subgroup, bylab = "Setting", byseparator=":")
mozambique_forest <- forest(mozambique_dat, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                          text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                          leftlabs=c("Study","Country", "Positive","Total"),
                          leftcols = c("study", "country", "positive.n", "total.n"))
thai <- CountryModel %>% filter(country == "Thailand")
thai_dat <- metaprop(positive.n, total.n, sm="PLN", data=thai, studlab=paste(study,year), comb.fixed=F,
                           byvar = subgroup, bylab = "Setting", byseparator=":")
thai_forest <- forest(thai_dat, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                            text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                            leftlabs=c("Study","Country", "Positive","Total"),
                            leftcols = c("study", "country", "positive.n", "total.n"))

sudan <- CountryModel %>% filter(country == "Sudan")
sudan_dat <- metaprop(positive.n, total.n, sm="PLN", data=sudan, studlab=paste(study,year), comb.fixed=F,
                     byvar = subgroup, bylab = "Setting", byseparator=":")
sudan_forest <- forest(sudan_dat, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                      text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                      leftlabs=c("Study","Country", "Positive","Total"),
                      leftcols = c("study", "country", "positive.n", "total.n"))

puerto <- CountryModel %>% filter(country == "Puerto Rico")
puerto_dat <- metaprop(positive.n, total.n, sm="PLN", data=puerto, studlab=paste(study,year), comb.fixed=F,
                      byvar = subgroup, bylab = "Setting", byseparator=":")
puerto_forest <- forest(puerto_dat, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                       text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                       leftlabs=c("Study","Country", "Positive","Total"),
                       leftcols = c("study", "country", "positive.n", "total.n"))
iran <- CountryModel %>% filter(country == "Iran")
iran_dat <- metaprop(positive.n, total.n, sm="PLN", data=iran, studlab=paste(study,year), comb.fixed=F,
                       byvar = subgroup, bylab = "Setting", byseparator=":")
iran_forest <- forest(iran_dat, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                        text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                        leftlabs=c("Study","Country", "Positive","Total"),
                        leftcols = c("study", "country", "positive.n", "total.n"))
