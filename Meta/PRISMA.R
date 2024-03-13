library(PRISMAstatement)
library(ggpubr)
library(rempsyc)
lab1 <- paste("Recodrs identified from 
             PubMed (n=2,981), Ovid (n=2,328), 
              and Web of Science (n=3,646)")
lab2 <- paste("Studies included in 
              Overall qualitative synthesis (n = 181)
               1. Seroprevalence studies (n = 131)
               2. Post-Chikungunya studies (n = 50)")
lab3 <- paste("Inclusion for quantitative synthesis: sero-catalytic models (n=43)
              (Age-stratified IgG non-fever (n=35),
               Age-stratified IgG non-fever with specific occupations (n=3),
               Age-stratified IgG non-fever with age-restricted populations (n=5))")
prisma <- prisma(found = 8955,
                found_other = 11,
                no_dupes = 5280,
                screened = 5280,
                screen_exclusions = 4771,
                full_text = 509,
                full_text_exclusions = 328,
                qualitative = 181,
                quantitative = 181,
                font_size = 9,
                dpi = 70,
                labels = list(found = lab1,
                     qualitative = lab2,
                     quantitative = lab3))

ggsave("prisma.pdf", prisma, dpi=1000, device= "pdf", height=8, width=10,units="in", bg=NULL)

overall_result <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/overall_result.xlsx", 
                             sheet = "Sheet4")
descript <- nice_table(
  overall_result, 
  title = c("Table 1", "Characteristics of included studies"),
  note = c("Others* "))

flextable::save_as_docx(descript, path = "descript.docx")




