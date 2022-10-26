# Wed Oct 26 12:07:34 2022 ------------------------------
# Semi-formal test of endogeneity in cbps models

#library----
library(readxl)
library(dplyr)
library(CBPS)

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
##propensity scores----
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
summary(modeldef) #propensity scores were evaluated in a previous script "CBPS_analysis.R"

## Model HDI-E----
model.idhmE_2010<- glm(data = data[-142,], 
                       IDHM_E_2010 ~
                         defPerc_2010 +
                         I(defPerc_2010^2),
                       weights = modeldef$weights)

###endogeneity test (Dyngeland et al., 2020)----
cor.test(x = model.idhmE_2010$residuals, y = data[-142,]$defPerc_2010, method = "spearman")

##Model HDI-L----
model.idhmL_2010<- glm(data = data[-142,], 
                       IDHM_L_2010 ~
                         defPerc_2010 +
                         I(defPerc_2010^2),
                       weights = modeldef$weights)
####endogeneity test (Dyngeland et al., 2020)----
cor.test(x = model.idhmL_2010$residuals, y = data[-142,]$defPerc_2010, method = "spearman")

###Model HDI-I----
model.idhmR_2010<- glm(data = data[-142,], 
                       IDHM_R_2010 ~
                         defPerc_2010 +
                         I(defPerc_2010^2),
                       weights = modeldef$weights)
####endogeneity test (Dyngeland et al., 2020)----
cor.test(x = model.idhmR_2010$residuals, y = data[-142,]$defPerc_2010, method = "spearman")

###Model extreme poverty----
glm(data = data[-142,], 
    expov_2010 ~
      defPerc_2010 +
      I(defPerc_2010^2),
    weights = modeldef$weights
) -> model.expov10
####endogeneity test (Dyngeland et al., 2020)----
cor.test(x = model.expov10$residuals, y = data[-142,]$defPerc_2010, method = "spearman")

###Model gini----
glm(
  data = data[-142, ],
  gini_2010 ~
    defPerc_2010 +
    I(defPerc_2010 ^ 2),
  weights = modeldef$weights
) -> model.gini_2010
####endogeneity test (Dyngeland et al., 2020)----
cor.test(x = model.gini_2010$residuals, y = data[-142,]$defPerc_2010, method = "spearman")

###Model under five mortality----
glm(
  data = data[-142, ],
  u5mort_2010 ~
    defPerc_2010 +
    I(defPerc_2010 ^ 2),
  weights = modeldef$weights
) -> model.u5mort_2010
####endogeneity test (Dyngeland et al., 2020)----
cor.test(x = model.u5mort_2010$residuals, y = data[-142,]$defPerc_2010, method = "spearman")
