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
library(here)
library(geobr)
library(sandwich)
library(lmtest)

read_xlsx(here("data/db2cap1_cbps_clean.xlsx"))-> data

#data exploration
glimpse(data)
data$code_state<- as.factor(data$code_state)
hist(data$nvcPerc_2010)
glimpse(data)
#ggpairs(data[,12:29])
as.data.frame(scale(data[,12:29]))-> data[,12:29]

data %>% 
  mutate(defPerc_2010 = 100-nvcPerc_2010) %>% 
  glimpse() -> data

data %>% #removing empty neighbours for spatial analysis
  filter(code_muni != "2203206",
         code_muni != "2515005",
         code_muni != "2804607") %>%
  glimpse()-> data

data[-142,] -> data
#data analysis----
##Propensity scores for 2010 model
### Create CBPS----

CBPS(data = data, #remove variables with correlation higher than 0.4 in 
              defPerc_2010 ~  #both directions and one municipality without neighbours    
              area_mun +
              p_agro_10 +
              perc_urb_10+
              prodAss_06+
              nascProt_06+
              riosProt_06+
              irrigPerc_06+
              rain_var+
              percProp_S+
              pibAgroPC_2010+
              pibIndPC_2010+
              pibServpubPC_2010+
              carvVeg_10+
              lenha_10,
              method = "exact",
              ATT=0
              ) -> modeldef
summary(modeldef)

#### Evaluate CBPS----
bal.covar<- balance(modeldef)
bal.covar<- data.frame(original=bal.covar$unweighted, 
                                weighted=bal.covar$balanced)
names(bal.covar)<- c("Unweighted", "CBPS weighted")
summary(bal.covar)
boxplot(bal.covar)


###Average Treatment Effect----
####Fong et al, 2018 method
####outcome expov----
glm(data = data, 
                    expov_2010 ~
                      defPerc_2010 +
                      I(defPerc_2010^2),
                    weights = modeldef$weights
                    ) -> model.expov10
summary(model.expov10)

#####clustered standard errors----
clval <- sandwich::vcovCL(model.expov10, data$code_muni)
coeftest(x = model.expov10, vcov. = clval, cluster = "code_muni")

#####Spatial autocorrelation----
read_municipality(simplified = F)->mun_cat
data$code_muni<-as.double(data$code_muni)
inner_join(x = mun_cat, y = data, by = "code_muni")-> mun_cat_data
poly2nb(mun_cat_data[-142,], queen=TRUE)-> mat_dist2
nb2listw(mat_dist2)->mat_dist_list

lm.morantest(model.expov10, mat_dist_list, alternative = "two.sided")

sacsarlm(
  expov_2010 ~
    defPerc_2010 +
    I(defPerc_2010 ^ 2) +
    area_mun +
    perc_urb_10 +
    riosProt_06 +
    irrigPerc_06 +
    rain_var +
    percProp_S +
    pibServpubPC_2010,
  data = data[-142, ],
  listw = mat_dist_list,
  type = "sac"
) -> m.expov_spat
summary(m.expov_spat)
summary(impacts(m.expov_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_expov


####outcome IDHM_R----
model.idhmR_2010<- glm(data = data, 
                    IDHM_R_2010 ~
                      defPerc_2010 +
                      I(defPerc_2010^2),
                    weights = modeldef$weights
)
summary(model.idhmR_2010)

#####clustered standard errors----
clval <- sandwich::vcovCL(model.idhmR_2010, data$code_muni)
coeftest(x = model.idhmR_2010, vcov. = clval, cluster = "code_muni")

#####spatial autocorrelation----
lm.morantest(model.idhmR_2010,mat_dist_list, alternative = "two.sided")

sacsarlm(
  IDHM_R_2010 ~
    defPerc_2010 +
    I(defPerc_2010 ^ 2) +
    area_mun +
    #p_agro_10 +
    perc_urb_10 +
    #prodAss_06+
    #nascProt_06+
    riosProt_06 +
    irrigPerc_06 +
    rain_var +
    percProp_S +
    #pibAgroPC_2010+
    #pibIndPC_2010+
    pibServpubPC_2010,
  #carvVeg_10+
  #lenha_10,
  data = data[-142, ],
  listw = mat_dist_list,
  type = "sac"
) -> m.idhmR_spat
summary(m.idhmR_spat)
summary(impacts(m.idhmR_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_idhmR


####outcome IDHM_L####
model.idhmL_2010<- glm(data = data, 
                        IDHM_L_2010 ~
                          defPerc_2010 +
                          I(defPerc_2010^2),
                        weights = modeldef$weights
)
summary(model.idhmL_2010)

#####clustered standard errors----
clval <- sandwich::vcovCL(model.idhmL_2010, data$code_muni)
coeftest(x = model.idhmL_2010, vcov. = clval, cluster = "code_muni")

#####spatial autocorrelation----
lm.morantest(model.idhmL_2010,mat_dist_list, alternative = "two.sided")

sacsarlm(
  IDHM_L_2010 ~
    defPerc_2010 +
    I(defPerc_2010 ^ 2) +
    area_mun +
    #p_agro_10 +
    perc_urb_10 +
    #prodAss_06+
    #nascProt_06+
    riosProt_06 +
    irrigPerc_06 +
    rain_var +
    percProp_S +
    #pibAgroPC_2010+
    #pibIndPC_2010+
    pibServpubPC_2010,
  #carvVeg_10+
  #lenha_10,
  data = data[-142, ],
  listw = mat_dist_list,
  type = "sac"
) -> m.idhmL_spat

summary(m.idhmL_spat)
summary(impacts(m.idhmL_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_idhmL


####outcome IDHM_E####
model.idhmE_2010<- glm(data = data, 
                       IDHM_E_2010 ~
                         defPerc_2010 +
                         I(defPerc_2010^2),
                       weights = modeldef$weights
)
summary(model.idhmE_2010)

#####clustered standard errors----
clval <- sandwich::vcovCL(model.idhmE_2010, data$code_muni)
coeftest(x = model.idhmE_2010, vcov. = clval, cluster = "code_muni")

#####Spatial autocorrelation----
lm.morantest(model.idhmE_2010,mat_dist_list, alternative = "two.sided")

sacsarlm(
  IDHM_E_2010 ~
    defPerc_2010 +
    I(defPerc_2010 ^ 2) +
    area_mun +
    perc_urb_10 +
    riosProt_06 +
    irrigPerc_06 +
    rain_var +
    percProp_S +
    pibServpubPC_2010,
  data = data[-142, ],
  listw = mat_dist_list,
  type = "sac"
) -> m.idhmE_spat

summary(m.idhmE_spat)
summary(impacts(m.idhmE_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_idhmE


####outcome gini####
model.gini_2010<- glm(data = data, 
                      gini_2010 ~
                        defPerc_2010 +
                        I(defPerc_2010^2),
                      weights = modeldef$weights
)
summary(model.gini_2010)

#####clustered standard errors----
clval <- sandwich::vcovCL(model.gini_2010, data$code_muni)
coeftest(x = model.gini_2010, vcov. = clval, cluster = "code_muni")

#####Spatial autocorrelation----
lm.morantest(model.gini_2010,mat_dist_list, alternative = "two.sided")

sacsarlm(
  gini_2010 ~
    defPerc_2010 +
    I(defPerc_2010 ^ 2) +
    area_mun +
    perc_urb_10 +
    riosProt_06 +
    irrigPerc_06 +
    rain_var +
    percProp_S +
    pibServpubPC_2010,
  data = data[-142,],
  listw = mat_dist_list,
  type = "sac"
) -> m.gini_spat
summary(m.gini_spat)
summary(impacts(m.gini_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_gini


####outcome u5mort####
model.u5mort_2010<- glm(data = data, 
                      u5mort_2010 ~
                        defPerc_2010 +
                        I(defPerc_2010^2),
                      weights = modeldef$weights
)
summary(model.u5mort_2010)

#####clustered standard errors----
clval <- sandwich::vcovCL(model.u5mort_2010, data$code_muni)
coeftest(x = model.u5mort_2010, vcov. = clval, cluster = "code_muni")

#####spatial autocorrelation----
lm.morantest(model.u5mort_2010,mat_dist_list, alternative = "two.sided")

sacsarlm(
  u5mort_2010 ~
    defPerc_2010 +
    I(defPerc_2010 ^ 2) +
    area_mun +
    perc_urb_10 +
    riosProt_06 +
    irrigPerc_06 +
    rain_var +
    percProp_S +
    pibServpubPC_2010,
  data = data[-142, ],
  listw = mat_dist_list,
  type = "sac"
) -> m.u5mort_spat
summary(m.u5mort_spat)
summary(impacts(m.u5mort_spat,
                listw = mat_dist_list,
                R = 500),
        zstats = TRUE)->summ.sac_u5mort


#Figures ####
#IDHM_E
ggplot(data = data[-142,], aes(x = defPerc_2010, y = IDHM_E_2010))+
  geom_point(alpha  = 0.2, color = "#5c3811")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.8, fill = "grey80", color = "#ffa600")+
  ylab("HDI - Education")+
  theme_classic()+
  theme(legend.position = "none"
        ,axis.title.x = element_blank()) ->fig.IDHM_E_10

#IDHM_L
ggplot(data = data[-142,], aes(x = defPerc_2010, y = IDHM_L_2010))+
  geom_point(alpha  = 0.2, color = "#5c3811")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.8, fill = "grey80", color = "#ffa600")+
  ylab("HDI - Longevity")+
  theme_classic()+
  theme(legend.position = "none",axis.title.x = element_blank())->fig.IDHM_L_10

#IDHM_R
ggplot(data = data[-142,], aes(x = defPerc_2010, y = IDHM_R_2010))+
  geom_point(alpha  = 0.2, color = "#5c3811")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.8, fill = "grey80", color = "#ffa600")+
  ylab("HDI - Income")+
  theme_classic()+
  theme(legend.position = "none"
        ,axis.title.x = element_blank())->fig.IDHM_R_10

#Extreme poverty
ggplot(data = data[-142,], aes(x = defPerc_2010, y = expov_2010))+
  geom_point(alpha  = 0.2, color = "#5c3811")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.8, fill = "grey80", color = "#ffa600")+
  xlab("Deforestation (%)")+
  ylab("Extreme poverty rate (%)")+
  theme_classic()+
  theme(legend.position = "none")->fig.expov_10

#gini
ggplot(data = data[-142,], aes(x = defPerc_2010, y = gini_2010))+
  geom_point(alpha  = 0.2, color = "#5c3811")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.8, fill = "grey80", color = "#ffa600")+
  xlab("Deforestation (%)")+
  ylab("Gini Index")+
  theme_classic()+
  theme(legend.position = "none")->fig.gini_10

#u5mort
ggplot(data = data[-142,], aes(x = defPerc_2010, y = u5mort_2010))+
  geom_point(alpha  = 0.2, color = "#5c3811")+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.8, fill = "grey80", color = "#ffa600")+
  xlab("Deforestation (%)")+
  ylab("Under five mortality")+
  theme_classic()+
  theme(legend.position = "none")->fig.u5mort_10

##figure 3####
ggarrange(fig.IDHM_E_10, fig.IDHM_L_10, fig.IDHM_R_10, fig.expov_10, fig.gini_10, fig.u5mort_10,
          labels = c("a","b", "c", "d", "e", "f"))-> fig3
ggsave(plot = fig3,
       filename = "/Users/user/OneDrive - The University of Manchester/outros_trampos/Manuscritos/boom-bust_caat/Manuscript/NatSus/fig3.jpg",
       dpi = 300,
       width = 10,
       height = 6)
