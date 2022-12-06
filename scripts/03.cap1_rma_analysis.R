# Tue May 25 18:35:10 2021 ------------------------------

#library####
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(emmeans)
library(ggsignif)

#data####
read_xlsx("/home/alenc/Documents/Doutorado/tese/cap1/dbcap1_rma.xlsx")->dbcap1_rma

#Analysis####
##frontier 1991 - classifying by grouping####
dbcap1_rma$def.stage_91<- 
  if_else(condition = dbcap1_rma$nvc.perc_91 < 100 & dbcap1_rma$nvc.perc_91 >66 ,
          true = 1,
          false = if_else(condition = dbcap1_rma$nvc.perc_91 <= 66 & dbcap1_rma$nvc.perc_91 > 33,
                          true = 2,
                          false = if_else(condition = dbcap1_rma$nvc.perc_91 <= 33 & dbcap1_rma$nvc.perc_91 >= 0,
                                          true = 3,
                                          false = 0)))

ggplot(data = dbcap1_rma, aes(x = as.factor(def.stage_91), y = nvc.perc_91,
                                   fill = as.factor(def.stage_91)))+ 
  geom_boxplot()+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"))+
  scale_x_discrete(labels = c("Initial", "Intermediate", "Advanced"))+
  labs(x = "Deforestation stage", y = "NVC (%) in 1991")+
  theme_classic(base_size = 22)+
  theme( legend.position="none")

ggplot(data = dbcap1_rma, aes(x = as.factor(def.stage_91), y = tx.desmat.perc_91,
                                   fill = as.factor(def.stage_91)))+# , group = def_stage, fill = def_stage))+
  geom_boxplot()+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"))+
  scale_x_discrete(labels = c("Initial", "Intermediate", "Advanced"))+
  labs(x = "Deforestation stage", y = "Def. rate (%)")+
  theme_classic(base_size = 22)+
  theme(legend.position="none")

dbcap1_rma%>%
  group_by(def.stage_91)%>%
  summarize(max.def = max(nvc.perc_91),
            min.def = min (nvc.perc_91))%>%
  glimpse()

TukeyHSD(aov(data = dbcap1_rma, tx.desmat.perc_91 ~ as.factor(def.stage_91)))

##frontier 2000 - classifying by grouping####
dbcap1_rma$def.stage_2000<- 
  if_else(condition = dbcap1_rma$nvc.perc_00 < 100 & dbcap1_rma$nvc.perc_00 > 66,
          true = 1,
          false = if_else(condition = dbcap1_rma$nvc.perc_00 <= 66 & dbcap1_rma$nvc.perc_00 > 33,
                          true = 2,
                          false = if_else(condition = dbcap1_rma$nvc.perc_00 <= 33 & dbcap1_rma$nvc.perc_00 >= 0,
                                          true = 3,
                                          false = 0)))
dbcap1_rma%>%
  group_by(def.stage_2000)%>%
  summarize(max.def = max(nvc.perc_00),
            min.def = min (nvc.perc_00))%>%
  glimpse()

TukeyHSD(aov(data = dbcap1_rma, tx.desmat.perc_00 ~ as.factor(def.stage_2000)))

ggplot(data = dbcap1_rma, aes(x = as.factor(def.stage_2000), y = tx.desmat.perc_00,
                              fill = as.factor(def.stage_2000)))+# , group = def_stage, fill = def_stage))+
  geom_boxplot()+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"))+
  scale_x_discrete(labels = c("Initial", "Intermediate", "Advanced"))+
  labs(x = "Deforestation stage", y = "Def. rate (%)")+
  theme_classic(base_size = 22)+
  theme(legend.position="none")

#frontier 2010 - classifying by grouping####
dbcap1_rma$def.stage_10<- 
  if_else(condition = dbcap1_rma$nvc.perc_10 < 100 & dbcap1_rma$nvc.perc_10 > 66,
          true = 1,
          false = if_else(condition = dbcap1_rma$nvc.perc_10 <= 66 & dbcap1_rma$nvc.perc_10 > 33,
                          true = 2,
                          false = if_else(condition = dbcap1_rma$nvc.perc_10 <= 33 & dbcap1_rma$nvc.perc_10 >= 0,
                                          true = 3,
                                          false = 0)))

ggplot(data = dbcap1_rma, aes(x = as.factor(def.stage_10), y = nvc.perc_10,
                              fill = as.factor(def.stage_10)))+ 
  geom_boxplot()+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"))+
  #scale_x_discrete(labels = c("Initial", "Intermediate", "Advanced"))+
  labs(y = "NVC (%) in 2010")+
  theme_classic(base_size = 12)+
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(vjust = -1.5),
        plot.margin = margin(5,6,5,-4)
        )->nvc.defStage_2010

ggplot(data = dbcap1_rma, aes(x = as.factor(def.stage_10), y = tx.desmat.perc_10,
                              fill = as.factor(def.stage_10)))+# , group = def_stage, fill = def_stage))+
  geom_boxplot()+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"))+
  scale_x_discrete(labels = c("Initial", "Intermediate", "Advanced"))+
  labs(x = "Deforestation stage", y = "Def. rate (%)")+
  theme_classic(base_size = 22)+
  theme(legend.position="none")
TukeyHSD(aov(data = dbcap1_rma, tx.desmat.perc_10 ~ as.factor(def.stage_10)))

# Repeate-measures ANOVA####
dbcap1_rma%>% 
  select(code_muni, nvc.perc_91, nvc.perc_00, nvc.perc_10, def.stage_91,
         def.stage_2000, def.stage_10, IDHM_E_1991, IDHM_E_2000, IDHM_E_2010,
         IDHM_L_1991, IDHM_L_2000, IDHM_L_2010, IDHM_R_1991, IDHM_R_2000,
         IDHM_R_2010, expov_1991, expov_2000, expov_2010, gini_1991, gini_2000,
         gini_2010, u5mort_1991, u5mort_2000, U5mort_2010)%>% 
  dplyr::rename(nvc.perc_1991 = nvc.perc_91,
         nvc.perc_2000 = nvc.perc_00,
         nvc.perc_2010 = nvc.perc_10,
         def.stage_1991 = def.stage_91,
         def.stage_2010 = def.stage_10,
         u5mort_2010 = U5mort_2010)%>% 
  pivot_longer(cols = -code_muni,
               names_to = c(".value", "year"),
               names_pattern = "(.+)_(.+)") %>%
  glimpse() -> pl_nvcs

plF_nvcs<- pl_nvcs
plF_nvcs$code_muni<- as.factor(plF_nvcs$code_muni)
plF_nvcs$year<- as.factor(plF_nvcs$year)
plF_nvcs$def.stage<- as.factor(plF_nvcs$def.stage)
glimpse(plF_nvcs)

##IDHM_E####
lmer.idhE<- lmer(data = plF_nvcs, formula = IDHM_E ~ def.stage*year + (1 | code_muni))
tab.idhE<- car::Anova(lmer.idhE, type = "II")
summary(lmer.idhE)
pw.idhE<- emmeans(lmer.idhE, pairwise ~ def.stage*year, pbkrtest.limit = 3621)

ggplot(data = plF_nvcs, aes(x = year, y = IDHM_E, fill = def.stage)) +
  geom_boxplot(aes(middle = mean(IDHM_E)))+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"), labels = c("Initial",
                                                                            "Intermediate",
                                                                            "Advanced"),
                    name = "Deforestation stage")+
  labs(x = "Year", y = "HDI - Education")+
  #scale_y_continuous(limits = c(0, 0.85))+
  geom_signif(y_position = c(0.43, 0.43, 0.43, 0.58, 0.58, 0.58, 0.77, 0.77, 0.77), 
              xmin = c(0.7, 0.95, 1.2, 1.7, 1.95, 2.2, 2.7, 2.95, 3.2),
              xmax = c(0.8, 1.05, 1.3, 1.8, 2.05, 2.3, 2.8, 3.05, 3.3),
              annotations = c("a","b","b", "c", "d", "d", "e", "e", "f"),
              tip_length = 0)+
  theme_classic(base_size = 14) -> rma_idhE
ggsave(filename = "HDI-E_defstage.png")

##IDHM_L####
ggplot(data = plF_nvcs, aes(x = year, y = IDHM_L, fill = def.stage)) +
  geom_boxplot(aes(middle = mean(IDHM_L)))+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"), labels = c("Initial",
                                                                            "Intermediate",
                                                                            "Advanced"),
                    name = "Deforestation stage")+
  labs(x = "Year", y = "HDI - Longevity")+
  #scale_y_continuous(limits = c(0, 0.85))+
  geom_signif(y_position = c(0.76, 0.76, 0.76, 0.83, 0.83, 0.83, 0.89, 0.89, 0.89), 
              xmin = c(0.7, 0.95, 1.2, 1.7, 1.95, 2.2, 2.7, 2.95, 3.2),
              xmax = c(0.8, 1.05, 1.3, 1.8, 2.05, 2.3, 2.8, 3.05, 3.3),
              annotations = c("a","a","b", "c", "c", "c", "d", "d", "d"),
              tip_length = 0)+
  theme_classic(base_size = 14) -> rma_idhL
ggsave(filename = "HDI-L_defstage.png")

lmer.idhL<- lmer(data = plF_nvcs, formula = IDHM_L ~ def.stage*year + (1 | code_muni))
tab.idhL<- car::Anova(lmer.idhL, type = "II")
summary(lmer.idhL)
pw.idhL<- emmeans(lmer.idhL, pairwise ~ def.stage*year, pbkrtest.limit = 3621)

##IDHM_R####
ggplot(data = plF_nvcs, aes(x = year, y = IDHM_R, fill = def.stage)) +
  geom_boxplot(aes(middle = mean(IDHM_R)))+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"), labels = c("Initial",
                                                                            "Intermediate",
                                                                            "Advanced"),
                    name = "Deforestation stage")+
  labs(x = "Year", y = "HDI - Income")+
  #scale_y_continuous(limits = c(0, 0.85))+
  geom_signif(y_position = c(0.69, 0.69, 0.69, 0.74, 0.74, 0.74, 0.79, 0.79, 0.79), 
              xmin = c(0.7, 0.95, 1.2, 1.7, 1.95, 2.2, 2.7, 2.95, 3.2),
              xmax = c(0.8, 1.05, 1.3, 1.8, 2.05, 2.3, 2.8, 3.05, 3.3),
              annotations = c("a","b","c", "d", "e", "de", "f", "f", "f"),
              tip_length = 0)+
  theme_classic(base_size = 14) -> rma_idhR
ggsave(filename = "HDI-R_defstage.png")

lmer.idhR<- lmer(data = plF_nvcs, formula = IDHM_R ~ def.stage*year + (1 | code_muni))
tab.idhR<- car::Anova(lmer.idhR, type = "II")
summary(lmer.idhR)
pw.idhR<- emmeans(lmer.idhR, pairwise ~ def.stage*year, pbkrtest.limit = 3621)

##expov####
ggplot(data = plF_nvcs, aes(x = year, y = expov, fill = def.stage)) +
  geom_boxplot(aes(middle = mean(expov)))+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"), labels = c("Initial",
                                                                            "Intermediate",
                                                                            "Advanced"),
                    name = "Deforestation stage")+
  labs(x = "Year", y = "Extreme poverty")+
  #scale_y_continuous(limits = c(0, 0.85))+
  geom_signif(y_position = c(96, 96, 96, 76, 76, 76, 51, 51, 51), 
              xmin = c(0.7, 0.95, 1.2, 1.7, 1.95, 2.2, 2.7, 2.95, 3.2),
              xmax = c(0.8, 1.05, 1.3, 1.8, 2.05, 2.3, 2.8, 3.05, 3.3),
              annotations = c("a","b","c", "d", "e", "de", "f", "g", "fg"),
              tip_length = 0)+
  theme_classic(base_size = 14) -> rma_expov
ggsave(filename = "expov_defstage.png")

lmer.expov<- lmer(data = plF_nvcs, formula = expov ~ def.stage*year + (1 | code_muni))
tab.expov<- car::Anova(lmer.expov, type = "II")
summary(lmer.expov)
pw.expov<- emmeans(lmer.expov, pairwise ~ def.stage*year, pbkrtest.limit = 3621)

##gini####
ggplot(data = plF_nvcs, aes(x = year, y = gini, fill = def.stage)) +
  geom_boxplot(aes(middle = mean(gini)))+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"), labels = c("Initial",
                                                                            "Intermediate",
                                                                            "Advanced"),
                    name = "Deforestation stage")+
  labs(x = "Year", y = "Gini Index")+
  geom_signif(y_position = c(0.94, 0.94, 0.94, 0.84, 0.84, 0.84, 0.81, 0.81, 0.81), 
              xmin = c(0.7, 0.95, 1.2, 1.7, 1.95, 2.2, 2.7, 2.95, 3.2),
              xmax = c(0.8, 1.05, 1.3, 1.8, 2.05, 2.3, 2.8, 3.05, 3.3),
              annotations = c("a","a","a", "b", "c", "bc", "a", "d", "d"),
              tip_length = 0)+
  theme_classic(base_size = 14) -> rma_gini
ggsave(filename = "gini_defstage.png")

lmer.gini<- lmer(data = plF_nvcs, formula = gini ~ def.stage*year + (1 | code_muni))
tab.gini<- car::Anova(lmer.gini, type = "II")
summary(lmer.gini)
pw.gini<- emmeans(lmer.gini, pairwise ~ def.stage*year, pbkrtest.limit = 3621)

##u5mort####
ggplot(data = plF_nvcs, aes(x = year, y = u5mort, fill = def.stage)) +
  geom_boxplot(aes(middle = mean(u5mort)))+
  scale_fill_manual(values = c("#313695", "#E6E600", "#A50026"), labels = c("Initial",
                                                                            "Intermediate",
                                                                            "Advanced"),
                    name = "Deforestation stage")+
  labs(x = "Year", y = "Under five mortality")+
  geom_signif(y_position = c(155, 155, 155, 110, 110, 110, 55, 55,55), 
              xmin = c(0.7, 0.95, 1.2, 1.7, 1.95, 2.2, 2.7, 2.95, 3.2),
              xmax = c(0.8, 1.05, 1.3, 1.8, 2.05, 2.3, 2.8, 3.05, 3.3),
              annotations = c("a","a","b", "c", "c", "c", "d", "d", "d"),
              tip_length = 0)+
  theme_classic(base_size = 14) -> rma_u5mort
ggsave(filename = "u5mort_defstage.png")

lmer.u5mort<- lmer(data = plF_nvcs, formula = u5mort ~ def.stage*year + (1 | code_muni))
tab.u5mort<- car::Anova(lmer.u5mort, type = "II")
summary(lmer.u5mort)
pw.u5mort<- emmeans(lmer.u5mort, pairwise ~ def.stage*year, pbkrtest.limit = 3621)

#Figure 2####
ggarrange(rma_idhE, rma_idhL, rma_idhR, rma_expov, rma_gini, rma_u5mort,
          labels = c("a", "b", "c", "d", "e", "f"),
          common.legend = T,
          legend = "bottom",
          )->fig2
ggsave(plot = fig2,
       filename = "/home/lucas/Documentos/Doutorado/tese/cap1/Manuscript/fig2.png",
       width = 11,
       height = 8)
