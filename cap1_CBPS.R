# Mon Mar  1 14:59:22 2021 ------------------------------
#Propensity scores analysis
#Analysis made following Walter Leite's tutorial at http://www.practicalpropensityscore.com/continuous.html
#and Christian Fong, Chad Hazlett, and Kosuke Imai, 2018 - .

#Libraries
library(readxl)
library(dplyr)
library(GGally)
library(CBPS)
library(sf)
library(spdep)
library(spatialreg)
library(ggplot2)


read_xlsx("/home/lucas/Documentos/Doutorado/tese/cap1/dbcap1_full_clean.xlsx")-> data

#data exploration
glimpse(data)
data$code_state<- as.factor(data$code_state)
hist(data$nvcPerc_2010)

as.data.frame(scale(data[,12:39]))-> data[,12:39]

#data analysis####

#spatial neighborhood matrix ####
read_sf("/home/lucas/Documentos/Doutorado/Dados/muncat_2020.shp")->mun_cat
data$code_muni<-as.character(data$code_muni)
inner_join(mun_cat, data, by = c("CD_MUN" = "code_muni"))-> mun_cat_data
poly2nb(mun_cat_data[-c(36, 600, 786),], queen=TRUE)-> mat_dist2
nb2listw(mat_dist2)->mat_dist_list

##Propensity scores for 2010 model####
### Create CBPS for each outcome
#IDHM_E####
##CBPS ####
ggpairs(data = data, columns = c(12, 13, 15, 17, 18, 19, 20, 21, 25, 26, 27, 28, 29, 32, 33, 36, 37, 38, 39 ))      

mod.cbps.idhmE<- CBPS(data = data[-c(38,600, 786),], 
              nvcPerc_2010 ~      
              area_mun +
              perc_urb_10+
              rain_var +
              percProp_S+
              pibIndPC_2010+
              pibServpubPC_2010+
              popDens_10+
              MeanSlopefor2004analysis +
              MeanElevationfor2004analysis +
              capMed_06 + 
              bovMed_06 +
              carvVeg_10 +
              lenha_10 +
              irrigPerc_06 +
              riosProt_06,
              method = "exact",
              ATT=0
              )
summary(mod.cbps.idhmE)

### Evaluate CBPS ####
bal.covar.idhme<- balance(mod.cbps.idhmE)
bal.covar.idhme<- data.frame(original=bal.covar$unweighted, 
                                weighted=bal.covar$balanced)
names(bal.covar.idhme)<- c("Pearson r unweighted", "Pearson r IPW")
summary(bal.covar.idhme)
boxplot(bal.covar.idhme)

## outcome IDHM_E####
model.idhmE_2010<- glm(data = data[-c(38,600, 786),], 
                       IDHM_E_2010 ~
                         nvcPerc_2010 +
                         I(nvcPerc_2010^2),
                       weights = mod.cbps.idhmE$weights
)
summary(model.idhmE_2010)

lm.morantest(model.idhmE_2010,mat_dist_list, alternative = "two.sided")->moran.idhmE

sacsarlm(IDHM_E_2010 ~
           nvcPerc_2010 +
           I(nvcPerc_2010^2)+
           area_mun +
           perc_urb_10+
           rain_var+
           percProp_S+
           pibIndPC_2010+
           pibServpubPC_2010+
           popDens_10+
           MeanElevationfor2004analysis+
           bovMed_06+
           carvVeg_10+
           lenha_10,
         data = data[-c(38,600,786),],
         listw = mat_dist_list, 
         type = "sac")->m.idhmE_spat
summary(m.idhmE_spat)
summary(impacts(m.idhmE_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_idhmE

errorsarlm(IDHM_E_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2)+
             area_mun +
             #p_agro_10 +
             perc_urb_10+
             #prodAss_06+
             #nascProt_06+
             riosProt_06+
             irrigPerc_06+
             rain_var+
             percProp_S+
             #pibAgroPC_2010+
             #pibIndPC_2010+
             pibServpubPC_2010,
           #carvVeg_10+
           #lenha_10,
           data= data[-c(38,616),], listw = mat_dist_list, etype="emixed")->m.idhmE_spat2
summary(m.idhmE_spat)

# IDHM_R####
## CBPS####
mod.cbps.idhmR<- CBPS(data = data[-c(38,600, 786),], 
                      nvcPerc_2010 ~      
                        area_mun +
                        p_agro_10+
                        perc_urb_10 +
                        irrigPerc_06 +
                        rain_var +
                        percProp_S +
                        pibAgroPC_2010 +
                        carvVeg_10 +
                        wood_10 +
                        pibServpubPC_2010+
                        capMed_06 + 
                        bovMed_06 +
                        popDens_10+
                        Sumkm2_ProtectedArea2013
                        ,
                      method = "exact",
                      ATT=0
                     )
summary(mod.cbps.idhmR)

model.idhmR_2010<- glm(data = data[-c(38,616),], 
                    IDHM_R_2010 ~
                      nvcPerc_2010 +
                      I(nvcPerc_2010^2),
                    weights = modelnvc$weights
)
summary(model.idhmR_2010)

lm.morantest(model.idhmR_2010,mat_dist_list, alternative = "two.sided")->moran.idhmR

sacsarlm(IDHM_R_2010 ~
           nvcPerc_2010 +
           I(nvcPerc_2010^2)+
           area_mun +
           #p_agro_10 +
           perc_urb_10+
           #prodAss_06+
           #nascProt_06+
           riosProt_06+
           irrigPerc_06+
           rain_var+
           percProp_S+
           #pibAgroPC_2010+
           #pibIndPC_2010+
           pibServpubPC_2010,
           #carvVeg_10+
           #lenha_10,
       data = data[-c(38,616),],
       listw = mat_dist_list, 
         type = "sac")->m.idhmR_spat
summary(m.idhmR_spat)
summary(impacts(m.idhmR_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_idhmR

errorsarlm(IDHM_R_2010 ~
           nvcPerc_2010 +
             I(nvcPerc_2010^2)+
             area_mun +
             #p_agro_10 +
             perc_urb_10+
             #prodAss_06+
             #nascProt_06+
             riosProt_06+
             irrigPerc_06+
             rain_var+
             percProp_S+
             #pibAgroPC_2010+
             #pibIndPC_2010+
             pibServpubPC_2010,
           #carvVeg_10+
           #lenha_10,
           data= data[-c(38,616),], listw = mat_dist_list, etype="emixed")->m.idhmR_spat2
summary(m.idhmR_spat)

#outcome IDHM_L####
model.idhmL_2010<- glm(data = data[-c(38,616),], 
                        IDHM_L_2010 ~
                          nvcPerc_2010 +
                          I(nvcPerc_2010^2),
                        weights = modelnvc$weights
)
summary(model.idhmL_2010)

lm.morantest(model.idhmL_2010,mat_dist_list, alternative = "two.sided")->moran.idhmL

sacsarlm(IDHM_L_2010 ~
           nvcPerc_2010 +
           I(nvcPerc_2010^2)+
           area_mun +
           #p_agro_10 +
           perc_urb_10+
           #prodAss_06+
           #nascProt_06+
           riosProt_06+
           irrigPerc_06+
           rain_var+
           percProp_S+
           #pibAgroPC_2010+
           #pibIndPC_2010+
           pibServpubPC_2010,
         #carvVeg_10+
         #lenha_10,
         data = data[-c(38,616),],
         listw = mat_dist_list, 
         type = "sac")->m.idhmL_spat
summary(m.idhmL_spat)
summary(impacts(m.idhmL_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_idhmL

errorsarlm(IDHM_L_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2)+
             area_mun +
             #p_agro_10 +
             perc_urb_10+
             #prodAss_06+
             #nascProt_06+
             riosProt_06+
             irrigPerc_06+
             rain_var+
             percProp_S+
             #pibAgroPC_2010+
             #pibIndPC_2010+
             pibServpubPC_2010,
           #carvVeg_10+
           #lenha_10,
           data= data[-c(38,616),], listw = mat_dist_list, etype="emixed")->m.idhmL_spat2
summary(m.idhmR_spat)



#outcome expov####
model.expov10<- glm(data = data[-c(38,616),], 
                    expov_2010 ~
                      nvcPerc_2010 +
                      I(nvcPerc_2010^2),
                    weights = modelnvc$weights
)
summary(model.expov10)
lm.morantest(model.expov10, mat_dist_list, alternative = "two.sided")->moran.expov

sacsarlm(expov_2010 ~
           nvcPerc_2010 +
           I(nvcPerc_2010^2)+
           area_mun +
           #p_agro_10 +
           perc_urb_10+
           #prodAss_06+
           #nascProt_06+
           riosProt_06+
           irrigPerc_06+
           rain_var+
           percProp_S+
           #pibAgroPC_2010+
           #pibIndPC_2010+
           pibServpubPC_2010,
         #carvVeg_10+
         #lenha_10,
         data = data[-c(38,616),],
         listw = mat_dist_list, 
         type = "sac")->m.expov_spat
summary(m.expov_spat)
summary(impacts(m.expov_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_expov

errorsarlm(expov_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2)+
             area_mun +
             #p_agro_10 +
             perc_urb_10+
             #prodAss_06+
             #nascProt_06+
             riosProt_06+
             irrigPerc_06+
             rain_var+
             percProp_S+
             #pibAgroPC_2010+
             #pibIndPC_2010+
             pibServpubPC_2010,
           #carvVeg_10+
           #lenha_10,
           data= data[-c(38,616),], listw = mat_dist_list, etype="emixed")->m.expov_spat2
summary(m.expov_spat2)
LR.sarlm(m.expov_spat, m.expov_spat2)

#outcome gini####
model.gini_2010<- glm(data = data[-c(38,616),], 
                      gini_2010 ~
                        nvcPerc_2010 +
                        I(nvcPerc_2010^2),
                      weights = modelnvc$weights
)
summary(model.gini_2010)

lm.morantest(model.gini_2010,mat_dist_list, alternative = "two.sided")->moran.gini

sacsarlm(gini_2010 ~
           nvcPerc_2010 +
           I(nvcPerc_2010^2)+
           area_mun +
           #p_agro_10 +
           perc_urb_10+
           #prodAss_06+
           #nascProt_06+
           riosProt_06+
           irrigPerc_06+
           rain_var+
           percProp_S+
           #pibAgroPC_2010+
           #pibIndPC_2010+
           pibServpubPC_2010,
         #carvVeg_10+
         #lenha_10,
         data = data[-c(38,616),],
         listw = mat_dist_list, 
         type = "sac")->m.gini_spat
summary(m.gini_spat)
summary(impacts(m.gini_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_gini

errorsarlm(gini_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2)+
             area_mun +
             #p_agro_10 +
             perc_urb_10+
             #prodAss_06+
             #nascProt_06+
             riosProt_06+
             irrigPerc_06+
             rain_var+
             percProp_S+
             #pibAgroPC_2010+
             #pibIndPC_2010+
             pibServpubPC_2010,
           #carvVeg_10+
           #lenha_10,
           data= data[-c(38,616),], listw = mat_dist_list, etype="emixed")->m.gini_spat2
summary(m.gini_spat)

#outcome u5mort####
model.u5mort_2010<- glm(data = data[-c(38,616),], 
                      u5mort_2010 ~
                        nvcPerc_2010 +
                        I(nvcPerc_2010^2),
                      weights = modelnvc$weights
)
summary(model.u5mort_2010)

lm.morantest(model.u5mort_2010,mat_dist_list, alternative = "two.sided")->moran.u5mort

sacsarlm(u5mort_2010 ~
           nvcPerc_2010 +
           I(nvcPerc_2010^2)+
           area_mun +
           #p_agro_10 +
           perc_urb_10+
           #prodAss_06+
           #nascProt_06+
           riosProt_06+
           irrigPerc_06+
           rain_var+
           percProp_S+
           #pibAgroPC_2010+
           #pibIndPC_2010+
           pibServpubPC_2010,
         #carvVeg_10+
         #lenha_10,
         data = data[-c(38,616),],
         listw = mat_dist_list, 
         type = "sac")->m.u5mort_spat
summary(m.u5mort_spat)
summary(impacts(m.u5mort_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_u5mort

errorsarlm(u5mort_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2)+
             area_mun +
             #p_agro_10 +
             perc_urb_10+
             #prodAss_06+
             #nascProt_06+
             riosProt_06+
             irrigPerc_06+
             rain_var+
             percProp_S+
             #pibAgroPC_2010+
             #pibIndPC_2010+
             pibServpubPC_2010,
           #carvVeg_10+
           #lenha_10,
           data= data[-c(38,616),], listw = mat_dist_list, etype="emixed")->m.u5mort_spat2
summary(m.u5mort_spat)

#Figures ####
#IDHM_E
ggplot(data = data[-c(38,616),], aes(x = nvcPerc_2010, y = IDHM_E_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("IDHM_E")->fig.IDHM_E_10

#IDHM_L
ggplot(data = data[-c(38,616),], aes(x = nvcPerc_2010, y = IDHM_L_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("IDHM_L")->fig.IDHM_L_10

#IDHM_R
ggplot(data = data[-c(38,616),], aes(x = nvcPerc_2010, y = IDHM_R_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("IDHM_R")->fig.IDHM_R_10

#Extreme poverty
ggplot(data = data[-c(38,616),], aes(x = nvcPerc_2010, y = expov_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("Extrem Poverty")->fig.expov_10

#gini
ggplot(data = data[-c(38,616),], aes(x = nvcPerc_2010, y = gini_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("Gini Index")->fig.gini_10

#u5mort
ggplot(data = data[-c(38,616),], aes(x = nvcPerc_2010, y = u5mort_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("Under five mortality")->fig.u5mort_10


