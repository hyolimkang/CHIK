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
View(CountryModel)

data1 <- CountryModel %>% filter(subgroup != "Europe & Central Asia")
data1 <- data1 %>% filter(subgroup != "South Asia")
data1 <- data1 %>% filter(subgroup != "Sub-Saharan Africa")
data2 <- CountryModel %>% filter(subgroup != "East Asia & Pacific")
data2 <- data2 %>% filter(subgroup != "Latin America & Caribbean")
data2 <- data2 %>% filter(subgroup != "Middle East & North Africa")


meta_dat1 <- metaprop(positive.n, total.n, sm="PLN", data=data1, studlab=paste(study,year), comb.fixed=F,
                    byvar = subgroup, bylab = "Setting", byseparator=":")
meta_dat2 <- metaprop(positive.n, total.n, sm="PLN", data=data2, studlab=paste(study,year), comb.fixed=F,
                    byvar = subgroup, bylab = "Setting", byseparator=":")


forest <- forest(meta_dat1, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                 text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                 leftlabs=c("Study","Country", "Positive","Total"),
                 leftcols = c("study", "country", "positive.n", "total.n"))

forest1 <- forest(meta_dat2, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                  text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                  leftlabs=c("Study","Country", "Positive","Total"),
                  leftcols = c("study", "country", "positive.n", "total.n"))

