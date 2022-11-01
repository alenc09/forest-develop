# Tue Nov  1 15:50:21 2022 ------------------------------
#Script to build new CBPS models using lagged deforestation

#Libraries----
library(readxl)
library(dplyr)
library(CBPS)
library(geobr)
library(spdep)

#data----
# read_xlsx(path = "/home/alenc/Documents/Doutorado/Dados/mapb5_cobertura_selec.xlsx") ->mapb5_cover
# read_xlsx(path = "data/db2cap1_cbps_clean.xlsx") -> data
read_xlsx(path = "data/db3cap1_cbps_clean.xlsx") -> data

##data manipulation----
# mapb5_cover %>%
#   filter(level_1 == "1 - Forest") %>%
#   rename(code_muni = territory_id,
#          nvcHa_2005 = '2005',
#          nvcHa_2008 = '2008') %>%
#   group_by(code_muni) %>%
#   summarise(code_muni = code_muni,
#             nvcHa_2005 = sum(nvcHa_2005),
#             nvcHa_2008 = sum(nvcHa_2008),
#             .groups = "drop") %>%
#   distinct() %>%
#   right_join(x = ., y = data, by = "code_muni") %>%
#   mutate(nvcPerc_2005 = (nvcHa_2005*100)/area_mun,
#          nvcPerc_2008 = (nvcHa_2008*100)/area_mun,
#          defPerc_2005 = 100 - nvcPerc_2005,
#          defPerc_2008 = 100 - nvcPerc_2008) %>%
#   glimpse -> data2
writexl::write_xlsx(x= data2, path = "data/db3cap1_cbps_clean.xlsx")

data %>% 
  mutate(defPerc_2010 = 100-nvcPerc_2010) %>% 
  glimpse() -> data
#Analysis----
##deforestation data 2005----
data %>% #removing empty neighbours for spatial analysis
  filter(code_muni != "2203206",
         code_muni != "2515005",
         code_muni != "2804607") %>%
  glimpse()->data

#data analysis----
##Propensity scores for 2010 model
### Create CBPS----

CBPS(data = data[-142,], #remove variables with correlation higher than 0.4 in 
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
glm(data = data[-142,], 
    expov_2010 ~
      defPerc_2005 +
      I(defPerc_2005^2),
    weights = modeldef$weights
) -> model.expov10
summary(model.expov10)

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


####outcome IDHM_R####
model.idhmR_2010<- glm(data = data[-142,], 
                       IDHM_R_2010 ~
                         defPerc_2010 +
                         I(defPerc_2010^2),
                       weights = modeldef$weights
)
summary(model.idhmR_2010)

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
model.idhmL_2010<- glm(data = data[-142,], 
                       IDHM_L_2010 ~
                         defPerc_2010 +
                         I(defPerc_2010^2),
                       weights = modeldef$weights
)
summary(model.idhmL_2010)

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
model.idhmE_2010<- glm(data = data[-142,], 
                       IDHM_E_2010 ~
                         defPerc_2010 +
                         I(defPerc_2010^2),
                       weights = modeldef$weights
)
summary(model.idhmE_2010)

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
model.gini_2010<- glm(data = data[-142,], 
                      gini_2010 ~
                        defPerc_2010 +
                        I(defPerc_2010^2),
                      weights = modeldef$weights
)
summary(model.gini_2010)

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
model.u5mort_2010<- glm(data = data[-142,], 
                        u5mort_2010 ~
                          defPerc_2010 +
                          I(defPerc_2010^2),
                        weights = modeldef$weights
)
summary(model.u5mort_2010)

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
##deforestation data 2008----

