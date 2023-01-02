library(readxl)
library (ggplot2)
library (dplyr)
library (data.table)
library (tidyverse)
library (ggpubr)
library (ggrepel)
library(scales)
library(viridis)
library(hrbrthemes)

#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
#https://www.rdocumentation.org/packages/hrbrthemes/versions/0.8.0/topics/theme_ipsum

CountryModel <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "Sheet2")
View(CountryModel)

thai      <- CountryModel %>% filter(Country == "Thailand")
indonesia1 <- CountryModel %>% filter(Country == "Indonesia" & `Study Yeaer` == "2013-2016")
indonesia2 <- CountryModel %>% filter(Country == "Indonesia" & `Study Yeaer` == "2009-2010")
malaysia     <- CountryModel %>% filter(Country == "Malaysia")
myanmar     <- CountryModel %>% filter(Country == "Myanmar")
brazilAlto  <- CountryModel %>% filter(Country == "Brazil, Alto")
brazilFeira  <- CountryModel %>% filter(Country == "Brazil, Feira")
brazilRio    <- CountryModel %>% filter(Country =="Brazil, Rio")
brazilQuixada <- CountryModel %>% filter(Country == "Brazil, Quixadá, Ceará")
brazilJuaz   <- CountryModel %>% filter(Country == "Brazil, Juazeiro do Norte")
Ecuador   <- CountryModel %>% filter(Country =="Ecuador")
Martinique   <- CountryModel %>% filter(Country =="Martinique")
Haiti        <- CountryModel %>% filter(Country == "Haiti")
suriname       <- CountryModel %>% filter(Country == "Suriname")
nicaragua       <- CountryModel %>% filter(Country == "Nicaragua")
puertorico    <- CountryModel %>% filter(Country == "Puerto Rico")


# East Asia
ggplot(thai, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Thailand 2014") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 500), breaks = c(0,100,200,300,400,500),labels = scales::number_format(big.mark = ','))



ggplot(indonesia1, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Indonesia 2013-2016") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 500), breaks = c(0,100,200,300,400,500),labels = scales::number_format(big.mark = ','))+
  scale_x_discrete(limits = c("1-5", "5-18", "18-45", "46-65", "66-Max"))



ggplot(indonesia2, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Indonesia 2009-2010") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 800), breaks = c(0,100,200,300,400,500,600,700, 800),labels = scales::number_format(big.mark = ','))+
  scale_x_discrete(limits = c("0-5", "5-14", "15-24", "25-34", "35-44","45-54","55-64","65-80"))




ggplot(malaysia, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Malaysia 2008") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 800), breaks = c(0,100,200,300,400,500,600,700, 800),labels = scales::number_format(big.mark = ','))


ggplot(myanmar, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Myanmar 2013-2018") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 800), breaks = c(0,100,200,300,400,500,600,700, 800),labels = scales::number_format(big.mark = ','))+
  scale_x_discrete(limits = c("0-5", "6-15", "16-45", "46-Max"))



## Latin America
## Brazil
ggplot(brazilAlto, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Brazil, Alto 2015") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 150), breaks = c(0,50,100,150),labels = scales::number_format(big.mark = ','))


ggplot(brazilFeira, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Brazil, Feira 2015") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 150), breaks = c(0,50,100,150),labels = scales::number_format(big.mark = ','))


ggplot(brazilRio, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Brazil, Rio 2018") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 1000), breaks = c(0,250,500,750,1000),labels = scales::number_format(big.mark = ','))


ggplot(brazilJuaz, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Brazil, Juazeiro do Nort 2018") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 1000), breaks = c(0,250,500,750,1000),labels = scales::number_format(big.mark = ','))+
  scale_x_discrete(limits = c("4-19", "20-45", "46-60", "61-Max"))



ggplot(brazilQuixada, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Brazil, Quixadá, Ceará 2018/19") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 1000), breaks = c(0,250,500,750,1000),labels = scales::number_format(big.mark = ','))

ggplot(Ecuador, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Ecuador 2014/15") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 200), breaks = c(0,50,100,150,200),labels = scales::number_format(big.mark = ','))


ggplot(Martinique, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Martinique 2015") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 200), breaks = c(0,50,100,150,200),labels = scales::number_format(big.mark = ','))


ggplot(Haiti, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Haiti 2014/15") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 1500), breaks = c(0,500,1000, 1500),labels = scales::number_format(big.mark = ','))+
  scale_x_discrete(limits = c("0-4", "5-9", "10-14", "15-19","20-24", "25-29",
                              "30-34","35-39", "40-44", "45-49","50-54","55-59",
                              "60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99"))



ggplot(suriname, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Suriname 2014/15") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 1500), breaks = c(0,500,1000, 1500),labels = scales::number_format(big.mark = ','))


ggplot(nicaragua, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Nicaragua 2014/15") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 1500), breaks = c(0,500,1000, 1500),labels = scales::number_format(big.mark = ','))+
  scale_x_discrete(limits = c("2-4", "5-9", "10-14", "15-29","30-44","45-59",""))



ggplot(puertorico, aes(fill=seropositivity, y=N, x=Age)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("")+
  ggtitle("Seropositivity in Puerto Rico 2014-2016") +
  theme_ipsum(plot_title_size = 10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 200), breaks = c(0,50,100,150,200),labels = scales::number_format(big.mark = ','))



