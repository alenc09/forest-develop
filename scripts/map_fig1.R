# Tue Jun  1 19:56:13 2021 ------------------------------
#Chapter one - maps

#libraries####
library(sf)
library(dplyr)
library(ggplot2)
library(geobr)
library(ggthemes)
library(ggpubr)
library(cowplot)
library(readxl)

#data####
read_xlsx("data/dbcap1_rma.xlsx") -> dbcap1_rma
read_state() %>% 
  filter(abbrev_state %in% c("BA","SE","PE", "AL","PB","RN","CE","PI", "MG"))->uf_caat #Download and filter polygons of brazilian states with Caatinga from oficial data source (IBGE)

read_municipality(simplified = F) ->mun_br #download polygons of brazilian municipalities

#Map----
dbcap1_rma%>%
  dplyr::select(code_muni, nvc.perc_10, def.stage_10)%>%
  left_join(y = mun_br, by = "code_muni") -> dbcap1_rma.map

ggplot()+  
  geom_sf(data=dbcap1_rma.map$geom,
          aes(fill = factor(dbcap1_rma$def.stage_10)), lwd = 0.1, na.rm = T)+
  scale_fill_manual(name = "Deforestation stage",
                    labels = c("Initial", "Intermediate", "Advanced"),
                    values = c("#5c3811", "#ac6c13", "#ffa600"),
                    na.value = "Grey60")+
  geom_sf(data=uf_caat, fill="transparent", color = "black", lwd = 0.3)+
  coord_sf(xlim = c(-47,-33), ylim = c(-18,-3))+
  geom_text(data = uf_caat, aes(x= c(-42, -39.5,-36.5,-35.5, -34.5, -34.4, -36, -39,-42.4),
                                y = c(-16.8, -15, -11, -10, -8.5, -7, -4.7, -2.9, -5),
                                label = c("MG", "BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI")),
            size = 2)+
  theme_map()+
  theme(legend.position = c(0.65, 0.1),
        panel.background = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))-> defStage_map

#Figure 1----
##deforested area----
dbcap1_rma %>% 
  dplyr::select(code_muni, nvc.perc_10, def.stage_10) %>% 
  glimpse %>% 
  ggplot()+
  geom_boxplot(aes(x = as.factor(def.stage_10), y = 100 - nvc.perc_10, fill = as.factor(def.stage_10)))+
  scale_x_discrete(labels = c("Initial", "Intermediate", "Advanced"))+
  scale_fill_manual(values = c("#5c3811", "#ac6c13", "#ffa600"))+
  ylab("Deforestation (%) in 2010")+
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") -> desmat.front_2010

##deforestation rates---
dbcap1_rma %>% 
  dplyr::select(code_muni, def.stage_10, tx.desmat.perc_91, tx.desmat.perc_00, tx.desmat.perc_10) %>% 
  mutate(mean.defRate = (tx.desmat.perc_91 + tx.desmat.perc_00 + tx.desmat.perc_10)/3) %>% 
  glimpse %>% 
  ggplot()+
  geom_boxplot(aes(x = as.factor(def.stage_10), y = mean.defRate, fill = as.factor(def.stage_10)))+
  scale_x_discrete(labels = c("Initial", "Intermediate", "Advanced"))+
  scale_fill_manual(values = c("#5c3811", "#ac6c13", "#ffa600"))+
  ylab("Mean def. rate (%)")+
  xlab("Deforestation stage (2010)")+
  theme(panel.background = element_blank()
        , axis.line = element_line(colour = "black")
        , legend.position = "none") -> mean_defRate

##inset map----
read_state() -> br_states
read_biomes() %>% 
  filter(name_biome == "Caatinga") ->caat_shp

ggplot()+
  geom_sf(data = caat_shp$geom, color = "transparent", fill = "grey60")+
  geom_sf(data = br_states$geom, fill = "transparent")+
  theme_map() -> inset_map

##panel figure----
plot_grid(
  defStage_map,
  plot_grid(
    desmat.front_2010,
    mean_defRate,
    nrow = 2,
    axis = "lr",
    labels = c("b", "c"),
    label_y = c(1, 1.1),
    label_x = c(-0.05,-0.05)
  ),
  rel_widths = c(2, 1),
  labels = "a"
) -> fig1

ggdraw()+
  draw_plot(fig1)+
  draw_plot(inset_map,
            x = -0.03, y = 0.7, width = 0.30, height = 0.30)-> fig1_inset

ggsave(plot = fig1_inset,
       filename = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/outros_trampos/Manuscritos/boom-bust_caat/Manuscript/FPE/figures/figure_1.jpg",
       dpi = 300,
       height = 6,
       width = 8,
       bg = "white")

