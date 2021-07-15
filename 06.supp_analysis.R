# Tue Jun  1 15:44:26 2021 ------------------------------
#supplementary analysis

#library####
library(dplyr)

#data####
dbcap1_rma%>%
  mutate(mean_defHa= rowSums(.[,33:52])/19,
         mean_defPerc = mean_defHa*100/area_mun,
         sd_defHa = apply(.[,33:52],1, sd, na.rm = TRUE),
         sd_defPerc = sd_defHa*100/area_mun,
         ) %>%glimpse->dbcap1_rma
#Analysis####
##Mean deforestation rates####
dbcap1_rma%>%
ggplot(data = ., aes(x = as.factor(def.stage_10), y = mean_defPerc,
                                fill = as.factor(def.stage_10)))+# , group = def_stage, fill = def_stage))+
  geom_boxplot()+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"))+
  scale_x_discrete(labels = c("Initial", "Intermediate", "Advanced"))+
  labs(x = "Deforestation stage", y = "Mean Def. rate (1991-2010)")+
  theme_classic(base_size = 12)+
  theme(legend.position="none")->mean_defRate

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
