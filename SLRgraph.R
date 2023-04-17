library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(readxl)
library(hrbrthemes)
library(stringr)
library(viridis)
library(binom)
library(ggpubr)

# https://edrub.in/ARE212/section06.html


rm(list = ls())    # remove any variables in R's memory 
options(scipen = 999)

# age stratifed prevalence
#by author and country 
CountryModel <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "sa_agestrat")
ggplot(CountryModel, aes(x = age_min, y = interaction(author, year, sep = "."), color = as.numeric(prevalence), group = interaction(country, antibody))) +
  geom_segment(aes(x = age_min, yend = interaction(author, year, sep = "."), xend = age_max, y = interaction(author, year, sep = ".")), size = 2) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_color_gradient(low = "white", high = "red") +
  facet_grid(country~ antibody, scales = "free_y", space = "free_y") +
  labs(x = "Age", y = "South Asian Countries", color = "Prevalence Rate")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("sa_agestrat.pdf", dpi=1000, device= "pdf", height=13, width=15,units="in", bg=NULL)


CountryModel <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "ssa_agestrat")
View(CountryModel)
ggplot(CountryModel, aes(x = age_min, y = interaction(author, year, sep = "."), color = as.numeric(prevalence), group = interaction(country, antibody))) +
  geom_segment(aes(x = age_min, yend = interaction(author, year, sep = "."), xend = age_max, y = interaction(author, year, sep = ".")), size = 2) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_color_gradient(low = "white", high = "red") +
  facet_grid(country~ antibody, scales = "free_y", space = "free_y") +
  labs(x = "Age", y = "Sub Sahara African Countries", color = "Prevalence Rate")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("ssa.pdf", dpi=1000, device= "pdf", height=15, width=12,units="in", bg=NULL)


CountryModel <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "lac_agestrat")
View(CountryModel)

ggplot(CountryModel, aes(x = age_min, y = interaction(author, year, sep = "."), color = as.numeric(prevalence), group = interaction(country, antibody))) +
  geom_segment(aes(x = age_min, yend = interaction(author, year, sep = "."), xend = age_max, y = interaction(author, year, sep = ".")), size = 2) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_color_gradient(low = "white", high = "red") +
  facet_grid(country~ antibody, scales = "free_y", space = "free_y") +
  labs(x = "Age", y = "Latin America & Caribbean Countries", color = "Prevalence Rate")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("lac_agestrat.pdf", dpi=1000, device= "pdf", height=10, width=10,units="in", bg=NULL)


CountryModel <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "eap_agestrat")
View(CountryModel)
ggplot(CountryModel, aes(x = age_min, y = interaction(author, year, sep = "."), color = as.numeric(prevalence), group = interaction(country, antibody))) +
  geom_segment(aes(x = age_min, yend = interaction(author, year, sep = "."), xend = age_max, y = interaction(author, year, sep = ".")), size = 2) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_color_gradient(low = "white", high = "red") +
  facet_grid(country~ antibody, scales = "free_y", space = "free_y") +
  labs(x = "Age", y = "East Asia & Pacific Countries", color = "Prevalence Rate")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("eap.pdf", dpi=1000, device= "pdf", height=12, width=12,units="in", bg=NULL)


CountryModel <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "me_agestrat")
View(CountryModel)
ggplot(CountryModel, aes(x = age_min, y = interaction(author, year, sep = "."), color = as.numeric(prevalence), group = interaction(country, antibody))) +
  geom_segment(aes(x = age_min, yend = interaction(author, year, sep = "."), xend = age_max, y = interaction(author, year, sep = ".")), size = 2) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_color_gradient(limits = c(0,100),low = "white", high = "red") +
  facet_grid(country~ antibody, scales = "free_y", space = "free_y") +
  labs(x = "Age", y = "Middle East Countries", color = "Prevalence Rate")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("me.pdf", dpi=1000, device= "pdf", height=12, width=12,units="in", bg=NULL)

CountryModel <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "eca_agestrat")
View(CountryModel)
ggplot(CountryModel, aes(x = age_min, y = interaction(author, year, sep = "."), color = as.numeric(prevalence), group = interaction(country, antibody))) +
  geom_segment(aes(x = age_min, yend = interaction(author, year, sep = "."), xend = age_max, y = interaction(author, year, sep = ".")), size = 2) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_color_gradient(limits = c(0,100), low = "white", high = "red") +
  facet_grid(country~ antibody, scales = "free_y", space = "free_y") +
  labs(x = "Age", y = "European Countries", color = "Prevalence Rate")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("eca.pdf", dpi=1000, device= "pdf", height=12, width=12,units="in", bg=NULL)



# publication numbers
CountryModel <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "pub_number")
CountryModel <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx",
                           sheet = "pub_number")

ggplot(data = CountryModel, aes(x = pub_year, y = region, size = values, fill = region))+
  geom_point(aes()) +
  scale_size(range = c(0,25))+
  geom_point(alpha=0.8, shape=21, color="black")+
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A")+
  labs(x = "Year", y = "Region")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_discrete(limits = c("<1990", "1991-2000", "2001-2010", ">2011"))

ggplot(data = CountryModel, aes(x = pub_year, y = region, size = values, fill = region))+
  geom_point(aes(color = region, size = values), alpha = 0.5) +
  scale_size(range = c(0,25))+
  labs(x = "Year", y = "Region")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_discrete(limits = c("<1990", "1991-2000", "2001-2010", ">2011"))




  
# point prevalence: facet by country and stack years --> interaction in the facet_grid
CountryModel <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "pointprevalence")

CountryModel[,c("midpoint","lower","upper")] = binom.confint(CountryModel$N.pos, CountryModel$N, method="exact")[,c("mean","lower","upper")]

eap <- CountryModel %>% filter(region == "EAP")


eap <- eap %>% mutate(period = case_when(year <1990 ~ "<1990",
                          year >1991 & year <2001 ~ "1991-2000",
                          year >2000 & year <2011 ~ "2000-2010",
                          year >2010 ~ ">2010"))

p1 <- ggplot(data = eap, aes(x = midpoint, y = year, color = midpoint))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0)+
  facet_grid(country~., scales = "free_y", space = "free_y")+
  theme_bw(10)+
  theme(panel.spacing = unit(0.1, "cm"),
        panel.spacing.x = unit(0, "cm"),
        strip.background = element_rect(fill = "grey", color = "black"),
        strip.text       = element_text(face = "bold", size = 7, hjust = 0.5, vjust = 0.5))+
  labs(x = "Prevalence", y = "Year")+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))


ggplot(data = eap, aes(x = midpoint, y = author, color = midpoint))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0)+
  facet_grid(country~factor(period, levels = c("2000-2010", ">2010")), scales = "free_y", space = "free_y")+
  theme_bw(10)+
  theme(panel.spacing = unit(0.1, "cm"),
        panel.spacing.x = unit(0, "cm"),
        strip.background = element_rect(fill = "grey", color = "black"),
        strip.text       = element_text(face = "bold", size = 7, hjust = 0.5, vjust = 0.5))+
  labs(x = "Prevalence", y = "Author")+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))



lac <- CountryModel %>% filter(region == "LAC")
lac <- lac %>% mutate(period = case_when(year <1990 ~ "<1990",
                                             year >1991 & year <2001 ~ "1991-2000",
                                             year >2000 & year <2011 ~ "2000-2010",
                                             year >2010 ~ ">2010"))


p2 <- ggplot(data = lac, aes(x = midpoint, y = year, color = midpoint))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0)+
  facet_grid(country~., scales = "free_y", space = "free_y")+
  theme_bw(10)+
  theme(panel.spacing = unit(0.1, "cm"),
        panel.spacing.x = unit(0, "cm"),
        strip.background = element_rect(fill = "grey", color = "black"),
        strip.text       = element_text(face = "bold", size = 7, hjust = 0.5, vjust = 0.5))+
  labs(x = "Prevalence", y = "Year")+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))


sa <- CountryModel %>% filter(region == "SA")
sa <- sa %>% mutate(period = case_when(year <1990 ~ "<1990",
                                 year >1991 & year <2001 ~ "1991-2000",
                                 year >2000 & year <2011 ~ "2000-2010",
                                 year >2010 ~ ">2010"))

p3 <- ggplot(data = sa, aes(x = midpoint, y = year, color = midpoint))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0)+
  facet_grid(country~., scales = "free_y", space = "free_y")+
  theme_bw(10)+
  theme(panel.spacing = unit(0.1, "cm"),
        panel.spacing.x = unit(0, "cm"),
        strip.background = element_rect(fill = "grey", color = "black"),
        strip.text       = element_text(face = "bold", size = 7, hjust = 0.5, vjust = 0.5))+
  labs(x = "Prevalence", y = "Year")+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))


me <- CountryModel %>% filter(region == "ME")
me <- me %>% mutate(period = case_when(year <1990 ~ "<1990",
                                    year >1991 & year <2001 ~ "1991-2000",
                                    year >2000 & year <2011 ~ "2000-2010",
                                    year >2010 ~ ">2010"))

p4 <- ggplot(data = me, aes(x = midpoint, y = year, color = midpoint))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0)+
  facet_grid(country~., scales = "free_y", space = "free_y")+
  theme_bw(10)+
  theme(panel.spacing = unit(0.1, "cm"),
        panel.spacing.x = unit(0, "cm"),
        strip.background = element_rect(fill = "grey", color = "black"),
        strip.text       = element_text(face = "bold", size = 7, hjust = 0.5, vjust = 0.5))+
  labs(x = "Prevalence", y = "Year")+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))


ssa <- CountryModel %>% filter(region == "SSA")

ssa <- ssa %>% mutate(period = case_when(year <1990 ~ "<1990",
                                         year >1991 & year <2001 ~ "1991-2000",
                                         year >2000 & year <2011 ~ "2000-2010",
                                         year >2010 ~ ">2010"))


p5 <- ggplot(data = ssa, aes(x = midpoint, y = year, color = midpoint))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0)+
  facet_grid(country~., scales = "free_y", space = "free_y")+
  theme_bw(10)+
  theme(panel.spacing = unit(0.1, "cm"),
        panel.spacing.x = unit(0, "cm"),
        strip.background = element_rect(fill = "grey", color = "black"),
        strip.text       = element_text(face = "bold", size = 7, hjust = 0.5, vjust = 0.5))+
  labs(x = "Prevalence", y = "Year")+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))


## facet by time and country within region

ggplot(data = ssa, aes(x = midpoint, y = author, color = antibody, shape= study_type))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0)+
  facet_grid(country~factor(period, levels = c("<1990", "2000-2010", ">2010")), scales = "free_y", space = "free_y")+
  theme_bw(10)+
  theme(panel.spacing = unit(0.1, "cm"),
        panel.spacing.x = unit(0, "cm"),
        strip.background = element_rect(fill = "grey", color = "black"),
        strip.text       = element_text(face = "bold", size = 7, hjust = 0.5, vjust = 0.5))+
  labs(x = "Prevalence", y = "Author")+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

ssa$country_author <- interaction(ssa$country, ssa$author, sep= "_")

ggplot(data = ssa, aes(x = midpoint, y = country_author, color = antibody, shape = study_type))+
  geom_point(size=3)+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height =0.3)+
  facet_grid(study_type ~ antibody, scales = "free", space = "free")+
  labs(x = "prevalence", y = "country", color = "antibody", shape = "study type")+
  theme_minimal()


ggplot(data = eap, aes(x = midpoint, y = author, color = antibody, shape= study_type))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0)+
  facet_grid(country~factor(period, levels = c("<1990", "2000-2010", ">2010")), scales = "free_y", space = "free_y")+
  theme_bw(10)+
  theme(panel.spacing = unit(0.1, "cm"),
        panel.spacing.x = unit(0, "cm"),
        strip.background = element_rect(fill = "grey", color = "black"),
        strip.text       = element_text(face = "bold", size = 7, hjust = 0.5, vjust = 0.5))+
  labs(x = "Prevalence", y = "Author")+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

ggplot(data = sa, aes(x = midpoint, y = author, color = antibody, shape= study_type))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0)+
  facet_grid(country~factor(period, levels = c("<1990", "2000-2010", ">2010")), scales = "free_y", space = "free_y")+
  theme_bw(10)+
  theme(panel.spacing = unit(0.1, "cm"),
        panel.spacing.x = unit(0, "cm"),
        strip.background = element_rect(fill = "grey", color = "black"),
        strip.text       = element_text(face = "bold", size = 7, hjust = 0.5, vjust = 0.5))+
  labs(x = "Prevalence", y = "Author")+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

ggplot(data = lac, aes(x = midpoint, y = author, color = antibody, shape= study_type))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0)+
  facet_grid(country~factor(period, levels = c("<1990", "2000-2010", ">2010")), scales = "free_y", space = "free_y")+
  theme_bw(10)+
  theme(panel.spacing = unit(0.1, "cm"),
        panel.spacing.x = unit(0, "cm"),
        strip.background = element_rect(fill = "grey", color = "black"),
        strip.text       = element_text(face = "bold", size = 7, hjust = 0.5, vjust = 0.5))+
  labs(x = "Prevalence", y = "Author")+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

ggplot(data = me, aes(x = midpoint, y = author, color = antibody, shape= study_type))+
  geom_point()+
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0)+
  facet_grid(country~factor(period, levels = c("<1990", "2000-2010", ">2010")), scales = "free_y", space = "free_y")+
  theme_bw(10)+
  theme(panel.spacing = unit(0.1, "cm"),
        panel.spacing.x = unit(0, "cm"),
        strip.background = element_rect(fill = "grey", color = "black"),
        strip.text       = element_text(face = "bold", size = 7, hjust = 0.5, vjust = 0.5))+
  labs(x = "Prevalence", y = "Author")+
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))
