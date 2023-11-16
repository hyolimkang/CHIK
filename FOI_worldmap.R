# first bring the worldmap
library(RColorBrewer)
library(ggplot2)
library(maps)
library(readxl)
library(sf)
library(grid)
library(cowplot)
require(sp)
library(raster)
library(rgdal)
library(rasterVis)
library(ggplot2)
library(sf)
library(tiff)
library(rnaturalearth)
library(wesanderson)
library(dplyr)
library(tidyr)
library(gridExtra)
options(scipen = 999)
world <- map_data("world")
continent_map <- subset(world_map, region == "continentname")

#1. base map -------------------------------------------------------------------
base_map <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey88", color="black", linewidth =0.08) +
  theme_minimal()
# data with lat, long, value of FOI, country
overall_result <- read_excel("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/all_countries/overall_result.xlsx", 
                             sheet = "foi_all")

overall_result <- read_excel("D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/all_countries/overall_result.xlsx", 
                             sheet = "foi_all")
View(overall_result)
#2 combine the base_map with data ----------------------------------------------
map_with_data <- base_map +
   geom_point(data = overall_result, aes(x = long, y = lat, color = mfoi), 
              size = 1)+
  scale_color_viridis_c(name = "FOI", option = "viridis")

ggsave("foimap.pdf", map_with_data, dpi=1000, device= "pdf", height=4, width=8,units="in", bg=NULL)


epimodel <- base_map +
  geom_point(data = overall_result, aes(x = long, y = lat, color = factor(model)), size = 1) +
  scale_color_brewer(name = "Model", palette = "Set1")

ggsave("modelselec.pdf", epimodel, dpi=1000, device= "pdf", height=4, width=8,units="in", bg=NULL)

foiworld <- ggarrange(map_with_data, epimodel,
            labels = c("A", "B"),
            ncol = 2, nrow = 1, align = "hv")

ggsave("foiworld.pdf", foiworld, dpi=1000, device= "pdf", height=4, width=12,units="in", bg=NULL)


#3. foi distribution box plot (with 95% UI)----------------------------------------------
filepath1 <- "D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/foidist.pdf"
filepath1 <- "/Users/hyolimkang/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/codes/CHIK/foidist.pdf"

pdf(file = filepath1, width = 15, height = 7.5)

overall_result$location <- factor(overall_result$location, levels = overall_result$location[order(overall_result$mfoi)])

wesander <- names(wes_palettes)[c(10,13,16,4,5,6,9)]
combcols <- unlist(lapply(wesander, wes_palette))

ggplot(overall_result, aes(x = location, fill = country)) +
  geom_crossbar(aes(ymin = mfoi_lo, ymax = mfoi_hi, y = mfoi), width = 0.7) +
  facet_wrap(. ~region, scales = "free_x", ncol = 3) +
  theme_bw() +
  labs(title = "B")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(face = "bold", size = rel(1))) +
  ylim(c(0, 0.2))+
 # guides(fill= "none")+
  ylab("FOI")+
  xlab("Location")+
  labs(fill = "Country")+
  scale_fill_manual(values = combcols)

dev.off()

#4. foi map --------------------------------------------------------------------
min_lat <- min(overall_result$lat, na.rm = TRUE)
max_lat <- max(overall_result$lat, na.rm = TRUE)

min_long <- min(overall_result$long, na.rm = TRUE)
max_long <- max(overall_result$long, na.rm = TRUE)

buffer <- 5  # adjust this value to your preference
min_lat <- min_lat - buffer
max_lat <- max_lat + buffer
min_long <- min_long - buffer
max_long <- max_long + buffer

min_val <- min(overall_result$mfoi)
max_val <- max(overall_result$mfoi)
interval <- (max_val - min_val) / 4
boundaries <- seq(min_val, max_val, by = interval)
overall_result$cat_val2 <- cut(overall_result$mfoi,
                       breaks=c(0.000100, 0.001, 0.005, 0.01, 0.1, 0.2),
                       labels=c("0.0001-0.001","0.001-0.005","0.005-0.01", "0.01-0.1", ">0.1"))

show_col(pal_lancet("lanonc")(9))
ylorrd_palette_9 <- brewer.pal(9, "YlOrRd")
lanonc     <- c("#FED976", "#FD8D3C", "#E31A1C", "#800026", "#1B1919FF")
overall_result$cat_val <- as.factor(overall_result$cat_val)
overall_result$cat_val2 <- as.factor(overall_result$cat_val2)
overall_result$cat_val <- factor(overall_result$cat_val, levels = c("0.0001 -  0.001",  
                                                                    "0.001 -  0.01",
                                                                    "0.01 -  0.1",
                                                                    ">  0.1"))
overall_result$cat_val2 <- factor(overall_result$cat_val2, levels = c("0.0001-0.001",  
                                                                    "0.001-0.005",
                                                                    "0.005-0.01",
                                                                    "0.01-0.1",
                                                                    ">0.1"))

fill_colors <- setNames(lanonc, levels(overall_result$cat_val2))
fill_labels <- setNames(labels, names(fill_colors))
border_layer <- borders("world", colour = "lightgrey", size = 0.1)

combined_plot_facet <-  base_map +
  border_layer+
  geom_point(data = overall_result, 
             aes(x = long, y = lat, fill = cat_val, color = cat_val, shape = factor(model)), 
             size = 1.5, alpha = 0.7) +  # Adjust size as necessary for better visuals
  scale_fill_manual(name = "FOI", values = fill_colors) +
  scale_color_manual(name = "FOI", values = fill_colors)+
  scale_shape_manual(name = "Model", values = c(24, 21)) +  # 24 is triangle, 21 is hollow circle
  coord_cartesian(xlim = c(min_long, max_long), ylim = c(min_lat, max_lat)) +
  theme_bw()+
  facet_wrap(~cut(seroprevalence, breaks = c(0, 0.0005, 0.005, 0.05, 0.15), labels = c("0-0.05%", "0.05-0.5 %", "0.5-5%", "5-20%")))

jitter_width <- 0.7
jitter_height <- 0.7

filepath1 <- "~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/codes/CHIK/foiMap.pdf"
filepath1 <- "D:/OneDrive - London School of Hygiene and Tropical Medicine/CHIK/1.Aim1/codes/CHIK/foiMap.pdf.pdf"
pdf(file = filepath1, width = 11, height = 11)

base_map +
  border_layer+
  geom_point(data = overall_result, 
             aes(x = long, y = lat, fill = cat_val2, color = cat_val2, shape = factor(model)), 
             size = 0.8, alpha = 0.7, position = position_jitter(width = jitter_width, height = jitter_height)) +  # Adjust size as necessary for better visuals
  scale_fill_manual(name = "FOI", values = fill_colors) +
  scale_color_manual(name = "FOI", values = fill_colors)+
  scale_shape_manual(name = "Model", values = c(24, 21)) +  # 24 is triangle, 21 is hollow circle
  coord_cartesian(xlim = c(min_long, max_long), ylim = c(min_lat, max_lat)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 10))+
  xlab(element_blank())+
  ylab(element_blank())+
  coord_sf(crs = st_crs(3112),ylim = c(-40, 55))+
  labs(title = "A")+
  theme(
    legend.key.size = unit(0.3, "cm"),       # Adjust size as desired
    legend.text = element_text(size = 8), # Adjust text size as desired
    legend.title = element_text(size = 10), # Adjust title size as desired
    plot.title = element_text(face = "bold", size = rel(1))
  )
# inset data

dev.off()

inset <- subset(overall_result, country == "Malaysia")
inset$cat_val <- factor(inset$cat_val, levels = c("0.0001 -  0.001",  
                                                  "0.001 -  0.01",
                                                  "0.01 -  0.1"))
inset_map <- base_map + 
  border_layer+
  geom_point(data = inset, aes(x = long, y = lat, fill = cat_val, color = cat_val,
                               shape = factor(model), alpha = 0.7), size = 0.9)+
  coord_sf(xlim = c(99.6, 104.3), ylim = c(1.20, 6.7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab(element_blank())+
  ylab(element_blank())+
  scale_fill_manual(name = "FOI", values = fill_colors) +
  scale_color_manual(name = "FOI", values = fill_colors)+
  scale_shape_manual(name = "Model", values = c(24, 21))+ 
  theme(legend.position = "non",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  theme(
    plot.margin = margin(5.5, 5.5, 5.5, 5.5), # Adjust space around plot
    plot.background = element_rect(color = "black", fill = NA, linewidth = 1), # Add border
    plot.title = element_text(hjust = 0.5), # Center the title
    plot.title.position = "plot"
  ) +
  facet_wrap(~country)

foimap_zoomed <- ggdraw() +
  draw_plot(combined_plot_org) +
  draw_plot(inset_map,
            height = 0.2,
            x = 0.14,
            y = 0.3)

setwd("/Users/hyolimkang/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/codes/CHIK/rasterfiles")
file_paths <- c("/Users/hyolimkang/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/CHIK/1.Aim1/codes/CHIK/MYS_adm")
layers    <- c("MYS_adm1")                

  result <- readOGR(dsn = file_paths, layer = layers)
  mys <- result
  mys_sf <- st_as_sf(mys)



inset_eap <- subset(overall_result, region == "EAP")
mys <- overall_result %>%
  filter(country == "Malaysia") %>%
  mutate(point_size = ifelse(country == "Malaysia", 1, 0)) %>% 
  ggplot() + 
  geom_sf(data = mys_sf) +
  geom_point(aes(x = long, y = lat, color = cat_val, shape = factor(model), size = point_size > 0), alpha = 0.75) +
  scale_size_manual(values=c(2,3.5)) +
  theme_test() +
  guides(size = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = "non",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  xlab(element_blank())+
  ylab(element_blank())+
  facet_wrap(~country)
  
ggdraw() +
  draw_plot(combined_plot_org) +
  draw_plot(mys,
            height = 0.2,
            x = 0.15,
            y = 0.3
  )


ggsave("FOIall.pdf", combined_plot_org, dpi=1000, device= "pdf", height=3, width=8,units="in", bg=NULL)
ggsave("FOIzoomed.pdf", foimap_zoomed, dpi=1000, device= "pdf", height=3, width=8,units="in", bg=NULL)

# 5. median foi values ---------------------------------------------------------
med_glob <- median(overall_result$mfoi)
med_eap  <- median(overall_result$mfoi[overall_result$region == "EAP"])
med_ssa  <- median(overall_result$mfoi[overall_result$region == "SSA"])
med_sa   <- median(overall_result$mfoi[overall_result$region == "SA"])
med_lac  <- median(overall_result$mfoi[overall_result$region == "LAC"])
med_eca  <- median(overall_result$mfoi[overall_result$region == "ECA"])
med_me   <- median(overall_result$mfoi[overall_result$region == "ME"])
med_epi  <- median(overall_result$mfoi[overall_result$model == "epidemic"])
med_con  <- median(overall_result$mfoi[overall_result$model == "constant"])

foifigure <- ggarrange(comb, foidist, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

ggsave("foifigure.pdf", foifigure, dpi=500, device= "pdf", height=9, width=13,units="in", bg=NULL)

comb <- comb + labs(title = "A")
foidist <- foidist + labs(title = "B")

comb <- ggplotGrob(comb)
foidist <- ggplotGrob(foidist)

combined_plot <- marrangeGrob(comb, foidist, nrow = 1, ncol = 1, top = NULL)


