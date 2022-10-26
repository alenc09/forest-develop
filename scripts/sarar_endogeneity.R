# Wed Oct 26 14:00:03 2022 ------------------------------
# Semi-formal test of endogeneity in SARAR models

#library----
library(readxl)
library(dplyr)

#data----
read_xlsx(here("data/db2cap1_cbps_clean.xlsx"))-> data

## data manipulation----
data %>% 
  mutate(defPerc_2010 = 100-nvcPerc_2010) %>% 
  glimpse() -> data

data %>% #removing empty neighbours for spatial analysis
  filter(code_muni != "2203206",
         code_muni != "2515005",
         code_muni != "2804607") %>%
  glimpse()->data

#analysis----
## Spatial matrix----
read_municipality(simplified = F)->mun_cat
data$code_muni<-as.double(data$code_muni)
inner_join(x = mun_cat, y = data, by = "code_muni")-> mun_cat_data
poly2nb(mun_cat_data[-142,], queen=TRUE)-> mat_dist2
nb2listw(mat_dist2)->mat_dist_list

##SARAR models----
### Model HDI-E----
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

###endogeneity test (Dyngeland et al., 2020)----
cor.test(x = m.idhmE_spat$residuals, y = data[-142,]$defPerc_2010, method = "spearman")

###Model HDI-L----
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
###endogeneity test (Dyngeland et al., 2020)----
cor.test(x = m.idhmL_spat$residuals, y = data[-142,]$defPerc_2010, method = "spearman")

###Model HDI-R----
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
###endogeneity test (Dyngeland et al., 2020)----
cor.test(x = m.idhmR_spat$residuals, y = data[-142,]$defPerc_2010, method = "spearman")

###Model extreme poverty----
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
###endogeneity test (Dyngeland et al., 2020)----
cor.test(x = m.expov_spat$residuals, y = data[-142,]$defPerc_2010, method = "spearman")

###Model gini index----
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
###endogeneity test (Dyngeland et al., 2020)----
cor.test(x = m.gini_spat$residuals, y = data[-142,]$defPerc_2010, method = "spearman")

###Model under five mortality----
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
###endogeneity test (Dyngeland et al., 2020)----
cor.test(x = m.u5mort_spat$residuals, y = data[-142,]$defPerc_2010, method = "spearman")
