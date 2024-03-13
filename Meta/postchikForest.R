library(pheatmap)
library(meta)
library(rmarkdown)
library(metafor)
library(ggplot2)
library(dplyr)
library(data.table)
library(readxl)
library(ggplotify)
library(patchwork)

#1. prevalence of chronic ------------------------------------------------------
chronic <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                      sheet = "chronic")
chronic <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                      sheet = "chronic")
chronic <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                      sheet = "chronic")

chronic <- chronic[chronic$stage        == "chronic",]
chronic <- chronic[-c(1, 15:17),]
chronic <- arrange(chronic, order)

post.chronic         <- metaprop(N.chronic, N.symptomatic, sm="PLN", data=chronic, studlab=paste(symptom_name,year), comb.fixed=T, comb.random = F,
                                 byvar = stage, bylab = "stage", byseparator=":")
filepath1 <- "D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/chronic.pdf"
filepath1 <- "~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/codes/CHIK/chronic.pdf"
filepath1 <- "C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/chronic.pdf"

pdf(file = filepath1, width = 15, height = 5)

chronic.forest       <- forest(post.chronic, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                               text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                               leftlabs=c("Symptom","Country", "F/U", "N.Chronic","N.Symptomatic"),
                               leftcols = c("symptom_name", "country", "FU", "N.chronic", "N.symptomatic") ,
                               comb.random = T,  col.square = "Gold", col.diamond = "red",
                               col.square.lines = "black" ,comb.random = FALSE, xlim=c(0, 1))
dev.new()
plot.new()
metafor:: forest(post.chronic, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
       text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
       leftlabs=c("Symptom","Country", "F/U", "N.Chronic","N.Symptomatic"),
       leftcols = c("symptom_name", "country", "FU", "N.chronic", "N.symptomatic") ,
       comb.random = T,  col.square = "Gold", col.diamond = "red",
       col.square.lines = "black" ,comb.random = FALSE, xlim=c(0, 1))
text(30, 15, "A", pos = 4)
dev.off()

#1.2 linearity check------------------------------------------------------------
chronic$label <- paste(chronic$symptom_name, "(", chronic$FU, ")")
linear.check <- rma.glmm(measure = "PLO", xi = N.chronic, 
                         ni = N.symptomatic, mods = ~fu_year, data = chronic,
                         slab = chronic$label)
forest(linear.check)
fu_year_range <- seq(min(chronic$fu_year), max(chronic$fu_year), length.out=100)
predicted_values <- predict(linear.check, newmods= fu_year_range)
pred_data <- data.frame(fu_year=fu_year_range, predicted=predicted_values$pred)
p <- ggplot(chronic, aes(x=fu_year, y=log(N.chronic / (N.symptomatic - N.chronic)))) +
  geom_point(aes(color="Observed Values")) +
  labs(title="Meta-regression: Log Odds vs FU Year",
       y="Log Odds",
       x="FU Year",
       color="Legend")+
  theme_bw()
p + geom_line(data=pred_data, aes(x=fu_year, y=predicted, color="Predicted Line"))

residuals_linear <- residuals(linear.check)
cor.test(chronic$fu_year, residuals_linear, method="spearman")
spearman_test <- spearman_test(chronic$fu_year ~ residuals_linear, distribution="approximate", nresample=10000)
summary(spearman_test)

# 1.3 compare btw simple vs. quadratic term model to test the null-hypothesis of linearity -------------
linear.model <- lm(prop ~ fu_year, data = chronic)
quad <- lm(prop ~ fu_year + I((fu_year)^2), data = chronic)
anova(linear.model, quad)

chronic$linear_pred <- predict(linear.model)
chronic$quad_pred <- predict(quad)

plot(chronic$fu_year, chronic$prop, main="Comparison of Linear and Quadratic Models", 
     xlab="fu_year", ylab="prop", pch=16, col="blue", cex=1.2)
lines(chronic$fu_year, chronic$linear_pred, col="red", lwd=2, type="l")
lines(chronic$fu_year, chronic$quad_pred, col="green", lwd=2, type="l")

legend("topright", legend=c("Observed Data", "Linear Model", "Quadratic Model"), 
       col=c("blue", "red", "green"), lty=1, lwd=2, bty="n")
#2. prevalence of hospitalised--------------------------------------------------
chronic <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                      sheet = "chronic")
chronic <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                      sheet = "chronic")
chronic <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "chronic")
hosp <- chronic[complete.cases(chronic$severity) & chronic$severity == "hospitalisation", ]

hosp.chronic         <- metaprop(N.chronic, N.symptomatic, sm="PLN", data=hosp, studlab=paste(severity,year), comb.fixed=T,comb.random = F,
                                 byvar = severity, bylab = "severity", byseparator=":")
filepath1 <- "D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/hosp.pdf"
filepath1 <- "~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/codes/CHIK/hosp.pdf"
filepath1 <- "C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/hosp.pdf"
pdf(file = filepath1, width = 15, height = 5)

hosp.forest          <- forest(hosp.chronic, print.tau2 = FALSE, col.by="black", text.fixed = "Total",
                            text.fixed.w = "Subtotal", rightcols = c("effect","ci"), 
                            leftlabs=c("Symptom","Country", "N.hospitalised","N.Symptomatic"),
                            leftcols = c("symptom_name", "country", "N.chronic", "N.symptomatic"),
                            comb.fixed = F, comb.random = T,  col.square = "Gold", col.diamond = "red",
                            col.square.lines = "black", xlim=c(0, 0.3))

dev.off()

par(mfrow = c(2, 1))
forest(chronic, xlab = "title")
forest(hosp, xlab = "title")

p_both
