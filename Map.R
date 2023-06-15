library(readxl)
require(tidyverse)
require(rjags)
require(binom)
library(ggplot2)
library(dplyr)
library(varhandle)
require(MCMCvis)
library(cowplot)
library(maps)
library(mapproj)
library(viridis)
library(nord)
library(rnaturalearth)
library(rnaturalearthdata)

world <- map_data("world")

Mapdata <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "mapdata")
Mapdata <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx", 
                           sheet = "map_studies")
Mapdata <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/CountryModel.xlsx",
                      sheet = "map_studies")
# https://stackoverflow.com/questions/24136868/plot-map-with-values-for-countries-as-color-in-r

map <- world %>%
  merge(Mapdata, by.x = "region", by.y = "country", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = number)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_viridis(name = "Number of CHIKV studies by country", na.value = "gray90") +
  labs(color = "number")+
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

ggsave("map.pdf", dpi=1000, device= "pdf", height=10, width=15,units="in", bg=NULL)


fwrite(world, "D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/world.csv")
