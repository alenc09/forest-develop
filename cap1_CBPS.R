# Mon Mar  1 14:59:22 2021 ------------------------------
#Propensity scores analysis
#Analysis made following Walter Leite's tutorial at http://www.practicalpropensityscore.com/continuous.html
#and Christian Fong, Chad Hazlett, and Kosuke Imai, 2018 - .

setwd("/home/lucas/Documents/Doutorado/tese/cap1/")

#Libraries
library(readxl)
library(dplyr)
library(GGally)
library(CBPS)
library(sf)
library(spdep)
library(spatialreg)
library(ggplot2)


read_xlsx("/home/lucas/Documentos/Doutorado/tese/cap1/db2cap1_cbps_clean.xlsx")-> data

#data exploration
glimpse(data)
data$code_state<- as.factor(data$code_state)
hist(data$nvcPerc_2010)
glimpse(data)
ggpairs(data[,12:29])
as.data.frame(scale(data[,12:31]))-> data[,12:31]

#data analysis####
##Propensity scores for 2010 model
### Create CBPS ####
modelnvc<- CBPS(data = data[-c(38,616),], #remove varibales with correlation higher than 0.4 in both directions
              nvcPerc_2010 ~      
              area_mun +
              p_agro_10 +
              #perc_rur_10 +
              perc_urb_10+
              prodAss_06+
              nascProt_06+
              riosProt_06+
              irrigPerc_06+
              rain_var+
              percProp_S+
              #percProp_M+
              #percProp_L+
              pibAgroPC_2010+
              pibIndPC_2010+
              pibServpubPC_2010+
              carvVeg_10+
              lenha_10,
              #wood_10,
              method = "exact",
              ATT=0
              )
summary(modelnvc)

### Evaluate CBPS ####
bal.covar<- balance(modelnvc)
bal.covar<- data.frame(original=bal.covar$unweighted, 
                                weighted=bal.covar$balanced)
names(bal.covar)<- c("Pearson r unweighted", "Pearson r IPW")
summary(bal.covar)
boxplot(bal.covar)

### check spatial correlation in CBPS stage ####
read_sf("/home/lucas/Documentos/Doutorado/Dados/muncat_2020.shp")->mun_cat
data$code_muni<-as.character(data$code_muni)
inner_join(mun_cat, data, by = c("CD_MUN" = "code_muni"))-> mun_cat_data
poly2nb(mun_cat_data[-c(38,616),], queen=TRUE)-> mat_dist2
nb2listw(mat_dist2)->mat_dist_list

#não consegui encontrar uma forma de testar o resíduos do CBPS

###Average Treatment Effect####
####Fong et al, 2018 method
#outcome expov####
model.expov10<- glm(data = data[-c(38,616),], 
                    expov_2010 ~
                      nvcPerc_2010 +
                      I(nvcPerc_2010^2),
                    weights = modelnvc$weights
                    )
summary(model.expov10)
lm.morantest(model.expov10, mat_dist_list, alternative = "two.sided")

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
             p_agro_10 +
             #perc_rur_10 +
             perc_urb_10+
             prodAss_06+
             nascProt_06+
             riosProt_06+
             irrigPerc_06+
             rain_var+
             percProp_S+
             #percProp_M+
             #percProp_L+
             pibAgroPC_2010+
             pibIndPC_2010+
             pibServpubPC_2010+
             carvVeg_10+
             lenha_10,
           #wood_10,, 
           data= data[-c(38,616),], listw = mat_dist_list)->m.expov_spat2
summary(m.expov_spat2)
LR.sarlm(m.expov_spat, m.expov_spat2)

#outcome IDHM_R####
model.idhmR_2010<- glm(data = data[-c(38,616),], 
                    IDHM_R_2010 ~
                      nvcPerc_2010 +
                      I(nvcPerc_2010^2),
                    weights = modelnvc$weights
)
summary(model.idhmR_2010)

lm.morantest(model.idhmR_2010,mat_dist_list, alternative = "two.sided")

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

errorsarlm(data = data[-c(38,616),], #Spatial error model with Inverse probabilit weights
           IDHM_R_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2),
           weights = modelnvc$weights,
           listw =mat_dist_list
)->m.idhmR_spat
summary(m.idhmR_spat)

#outcome IDHM_L####
model.idhmL_2010<- glm(data = data[-c(38,616),], 
                        IDHM_L_2010 ~
                          nvcPerc_2010 +
                          I(nvcPerc_2010^2),
                        weights = modelnvc$weights
)
summary(model.idhmL_2010)

lm.morantest(model.idhmL_2010,mat_dist_list, alternative = "two.sided")

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

errorsarlm(data = data[-c(38,616),], #Spatial error model with Inverse probabilit weights
           IDHM_L_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2),
           weights = modelnvc$weights,
           listw =mat_dist_list
)->m.idhmR_spat
summary(m.idhmR_spat)

#outcome IDHM_E####
model.idhmE_2010<- glm(data = data[-c(38,616),], 
                       IDHM_E_2010 ~
                         nvcPerc_2010 +
                         I(nvcPerc_2010^2),
                       weights = modelnvc$weights
)
summary(model.idhmE_2010)

lm.morantest(model.idhmE_2010,mat_dist_list, alternative = "two.sided")

sacsarlm(IDHM_E_2010 ~
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
         type = "sac")->m.idhmE_spat
summary(m.idhmE_spat)
summary(impacts(m.idhmE_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_idhmE

errorsarlm(data = data[-c(38,616),], #Spatial error model with Inverse probabilit weights
           IDHM_E_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2),
           weights = modelnvc$weights,
           listw =mat_dist_list
)->m.idhmE_spat
summary(m.idhmE_spat)

#outcome gini####
model.gini_2010<- glm(data = data[-c(38,616),], 
                      gini_2010 ~
                        nvcPerc_2010 +
                        I(nvcPerc_2010^2),
                      weights = modelnvc$weights
)
summary(model.gini_2010)

lm.morantest(model.gini_2010,mat_dist_list, alternative = "two.sided")

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

errorsarlm(data = data[-c(38,616),], #Spatial error model with Inverse probabilit weights
           gini_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2),
           weights = modelnvc$weights,
           listw =mat_dist_list
)->m.gini_spat
summary(m.gini_spat)

#outcome u5mort####
model.u5mort_2010<- glm(data = data[-c(38,616),], 
                      u5mort_2010 ~
                        nvcPerc_2010 +
                        I(nvcPerc_2010^2),
                      weights = modelnvc$weights
)
summary(model.u5mort_2010)

lm.morantest(model.u5mort_2010,mat_dist_list, alternative = "two.sided")

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

errorsarlm(data = data[-c(38,616),], #Spatial error model with Inverse probabilit weights
           u5mort_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2),
           weights = modelnvc$weights,
           listw =mat_dist_list
)->m.u5mort_spat
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

#gini
ggplot(data = data[-c(38,616),], aes(x = nvcPerc_2010, y = gini_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("Gini Index")->fig.gini_10

#Extreme poverty
ggplot(data = data[-c(38,616),], aes(x = nvcPerc_2010, y = expov_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("Extrem Poverty")->fig.expov_10

#u5mort
ggplot(data = data[-c(38,616),], aes(x = nvcPerc_2010, y = u5mort_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("Under five mortality")->fig.u5mort_10


