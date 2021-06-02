# Tue Jun  1 19:56:13 2021 ------------------------------
#Chapter one - maps

#libraries####
library(sf)
library(dplyr)
library(ggplot2)
library(geobr)
library(ggthemes)

#data####
st_read(dsn = "/home/lucas/Documentos/Doutorado/Dados/shapes/", layer = "muncat_2020")->map_caat

dbcap1_rma%>%
  select(code_muni, nvc.perc_10, def.stage_10)%>%
  left_join(x = map_caat, y = ., by = c("code_mn" = "code_muni"))%>%
  glimpse->map_caat.data

uf<-read_state()

uf %>% 
  filter(abbrev_state %in% c("BA","SE","PE", "AL","PB","RN","CE","PI", "MG"))->uf_caat
#Maps####
map_caat.data%>%
ggplot()+  
  geom_sf(data=map_caat.data$geometry,
          aes(fill = factor(map_caat.data$def.stage_10)), lwd = 0.1, na.rm = T)+
  scale_fill_manual(name = "Deforestation stage",
                    labels = c("Initial", "Intermediate", "Advanced", "No data"),
                    values = c("#4575b4", "#E6E600", "#d73027"),
                    na.value = "Grey60")+
  geom_sf(data=uf_caat, fill="transparent", color = "black", lwd = 0.3)+
  coord_sf(xlim = c(-47,-33), ylim = c(-18,-3))+
  theme_map()+
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

        