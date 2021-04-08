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

#data exploration####
hist(data$nvcPerc_2010)
hist(data$nvcPerc_2010_norm)#parece menos normal que sem ser transformado...
glimpse(data)
ggpairs(data[,12:29])
as.data.frame(scale(data[,12:29]))-> data[,12:29]

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
model.expov10<- glm(data = data[-c(38,616),], 
                    expov_2010 ~
                      nvcPerc_2010 +
                      I(nvcPerc_2010^2),
                    weights = modelnvc$weights
                    )
summary(model.expov10)
lm.morantest(model.expov10, mat_dist_list, alternative = "two.sided")

moran.test(mun_cat_data[-c(38,616),]$expov_2010, mat_dist_list, alternative = "two.sided") # tentativa de identificar qual variável carrega a dependencia espacial
moran.test(mun_cat_data[-c(38,616),]$gini_2010, mat_dist_list, alternative = "two.sided")
moran.test(mun_cat_data[-c(38,616),]$IDHM_E_2010, mat_dist_list, alternative = "two.sided")
moran.test(mun_cat_data[-c(38,616),]$nvcPerc_2010, mat_dist_list, alternative = "two.sided")

errorsarlm(data = data[-c(38,616),], #Spatial error model with Inverse probabilit weights
         expov_2010 ~
           nvcPerc_2010 +
           I(nvcPerc_2010^2),
         weights = modelnvc$weights,
         listw =mat_dist_list
        )->m.expov_spat
summary(m.expov_spat)

#outcome IDHM_R
model.idhmR_2010<- glm(data = data[-c(38,616),], 
                    IDHM_R_2010 ~
                      nvcPerc_2010 +
                      I(nvcPerc_2010^2),
                    weights = modelnvc$weights
)
summary(model.idhmR_2010)

lm.morantest(model.idhmR_2010,mat_dist_list, alternative = "two.sided")
errorsarlm(data = data[-c(38,616),], #Spatial error model with Inverse probabilit weights
           IDHM_R_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2),
           weights = modelnvc$weights,
           listw =mat_dist_list
)->m.idhmR_spat
summary(m.idhmR_spat)

#outcome IDHM_L
model.idhmL_2010<- glm(data = data[-c(38,616),], 
                        IDHM_L_2010 ~
                          nvcPerc_2010 +
                          I(nvcPerc_2010^2),
                        weights = modelnvc$weights
)
summary(model.idhmL_2010)

lm.morantest(model.idhmL_2010,mat_dist_list, alternative = "two.sided")
errorsarlm(data = data[-c(38,616),], #Spatial error model with Inverse probabilit weights
           IDHM_L_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2),
           weights = modelnvc$weights,
           listw =mat_dist_list
)->m.idhmR_spat
summary(m.idhmR_spat)

#outcome IDHM_E
model.idhmE_2010<- glm(data = data[-c(38,616),], 
                       IDHM_E_2010 ~
                         nvcPerc_2010 +
                         I(nvcPerc_2010^2),
                       weights = modelnvc$weights
)
summary(model.idhmE_2010)

lm.morantest(model.idhmE_2010,mat_dist_list, alternative = "two.sided")
errorsarlm(data = data[-c(38,616),], #Spatial error model with Inverse probabilit weights
           IDHM_E_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2),
           weights = modelnvc$weights,
           listw =mat_dist_list
)->m.idhmE_spat
summary(m.idhmE_spat)

#outcome gini
model.gini_2010<- glm(data = data[-c(38,616),], 
                      gini_2010 ~
                        nvcPerc_2010 +
                        I(nvcPerc_2010^2),
                      weights = modelnvc$weights
)
summary(model.gini_2010)

lm.morantest(model.gini_2010,mat_dist_list, alternative = "two.sided")
errorsarlm(data = data[-c(38,616),], #Spatial error model with Inverse probabilit weights
           gini_2010 ~
             nvcPerc_2010 +
             I(nvcPerc_2010^2),
           weights = modelnvc$weights,
           listw =mat_dist_list
)->m.gini_spat
summary(m.gini_spat)

#outcome u5mort
model.u5mort_2010<- glm(data = data[-c(38,616),], 
                      u5mort_2010 ~
                        nvcPerc_2010 +
                        I(nvcPerc_2010^2),
                      weights = modelnvc$weights
)
summary(model.u5mort_2010)

lm.morantest(model.u5mort_2010,mat_dist_list, alternative = "two.sided")
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


