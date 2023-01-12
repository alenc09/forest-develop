# Wed Nov  2 10:45:42 2022 ------------------------------
#script to test SARAR models with lagged deforestation

#Analysis----
##HDI - Education----
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

## HDI - Longevity----

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

## HDI - income----


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

## extreme poverty----
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

##Gini index ----

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

##Under five mortality----
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