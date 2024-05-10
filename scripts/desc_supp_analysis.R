# Tue Jun  1 15:44:26 2021 ------------------------------
#supplementary analysis

#library####
library(dplyr)
library(tidyr)

#data####
dbcap1_rma%>%
  mutate(mean_defHa= rowSums(.[,33:52])/19,
         mean_defPerc = mean_defHa*100/area_mun,
         sd_defHa = apply(.[,33:52],1, sd, na.rm = TRUE),
         sd_defPerc = sd_defHa*100/area_mun,
         ) %>%glimpse->dbcap1_rma
#Analysis####
##Mean deforestation rates####
dbcap1_rma %>% 
  dplyr::select(code_muni, def.stage_10, tx.desmat.perc_91, tx.desmat.perc_00, tx.desmat.perc_10) %>%
  mutate(mean.defRate = (tx.desmat.perc_91 + tx.desmat.perc_00 + tx.desmat.perc_10)/3) %>%
  group_by(def.stage_10) %>%
  summarise(mean.defRate_group = mean(mean.defRate),
            sd.defRate_group = sd(mean.defRate)) %>%
           glimpse

dbcap1_rma%>%
ggplot(data = ., aes(x = as.factor(def.stage_10), y = mean_defPerc,
                                fill = as.factor(def.stage_10)))+# , group = def_stage, fill = def_stage))+
  geom_boxplot()+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"))+
  #scale_x_discrete(labels = c("Initial", "Intermediate", "Advanced"))+
  labs(y = "Annual Def. rate (1991-2010)")+
  theme_classic(base_size = 12)+
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(vjust = 1.5),)->mean_defRate

TukeyHSD(aov(data = dbcap1_rma, mean_defPerc ~ as.factor(def.stage_10)))

dbcap1_rma%>%
  ggplot(data = ., aes(x = as.factor(def.stage_10), y = sd_defPerc,
                       fill = as.factor(def.stage_10)))+# , group = def_stage, fill = def_stage))+
  geom_boxplot()+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"))+
  scale_x_discrete(labels = c("Initial", "Intermediate", "Advanced"))+
  labs(x = "Deforestation stage (2010)", y = "SD Def. rate (1991-2010)")+
  theme_classic(base_size = 22)+
  theme(legend.position="none")->sd_defRate

##Deforestation stage change####
dbcap1_rma%>%
  #filter(def.stage_91 == 3 & def.stage_10 == 3)%>% #change the values to change what groups to compare
  mutate(nvc.cc = nvc.perc_10 - nvc.perc_91)%>%
  #group_by(def.stage_10)%>%
  summarise(nvc.var.mean = mean(nvc.cc),
            nvc.var.sd = sd(nvc.cc))%>%
  glimpse

#supp fig 1####
dbcap1_rma%>%
  select(code_muni, nvc.perc_91, nvc.perc_10, def.stage_10)%>%
  mutate(net.nvc = nvc.perc_10 - nvc.perc_91,
         mean.nvc.change = mean(net.nvc),
         sd.nvc.change = sd(net.nvc))%>%
  glimpse->db.net.nvc

db.net.nvc%>%
  ggplot(aes(x = as.factor(def.stage_10), y = net.nvc, fill=as.factor(def.stage_10)))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_boxplot()+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"))+
  scale_x_discrete(labels = c("Initial", "Intermediate", "Advanced"))+
  labs(x = "Deforestation stage in 2010", y = "Net forest cover")+
  stat_summary(fun = mean, geom = "point", size = 1, colour = "red")+
  theme_classic(base_size = 22)+
  theme(legend.position="none") -> net_nvc  
ggsave(plot = net_nvc, filename = "/home/lucas/Documentos/Doutorado/tese/cap1/Manuscript/supp_fig1.png")  

#descriptive evolution of indeces####
plF_nvcs%>%
  group_by(year)%>%
  summarize(meanidhE = mean(IDHM_E),
         seidhE = sd(IDHM_E)/sqrt(length(IDHM_E)),
         meanu5mort = mean(u5mort),
         seu5mort = sd(u5mort)/sqrt(length(u5mort))
         )%>%
  glimpse

plF_nvcs%>%
  group_by(year, def.stage)%>%
  summarize(meanidhE = mean(IDHM_E),
            seidhE = sd(IDHM_E)/sqrt(length(IDHM_E)),
            meanidhL = mean(IDHM_L),
            seidhL = sd(IDHM_L)/sqrt(length(IDHM_L)),
            meanidhR = mean(IDHM_R),
            seidhR = sd(IDHM_R)/sqrt(length(IDHM_R)),
            meanexpov = mean(expov),
            seexpov = sd(expov)/sqrt(length(expov)),
            meangini = mean(gini),
            segini = sd(gini)/sqrt(length(gini)),
            meanu5mort = mean(u5mort),
            seu5mort = sd(u5mort)/sqrt(length(u5mort))
  )%>%
  pivot_wider(id_cols = def.stage,
              names_from = year,
              values_from = c(meanidhE,meanidhL,meanidhR,meanexpov,meangini,meanu5mort))%>%
  mutate(g_idhe = 1-(meanidhE_1991/meanidhE_2010),
         g_idhl = 1-(meanidhL_1991/meanidhL_2010),
         g_idhr = 1-(meanidhR_1991/meanidhR_2010),
         g_expov = 1-(meanexpov_1991/meanexpov_2010),
         g_gini = 1-(meangini_1991/meangini_2010),
         g_u5mort = 1-(meanu5mort_1991/meanu5mort_2010))%>%
  glimpse#->summary_devlop

summary_devlop%>%
  mutate(meanG_idhE = )
  glimpse

