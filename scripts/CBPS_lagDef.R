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
#          nvcHa_2009 = '2009') %>%
#   group_by(code_muni) %>%
#   summarise(code_muni = code_muni,
#             nvcHa_2005 = sum(nvcHa_2005),
#             nvcHa_2009 = sum(nvcHa_2009),
#             .groups = "drop") %>%
#   distinct() %>%
#   right_join(x = ., y = data, by = "code_muni") %>%
#   mutate(nvcPerc_2005 = (nvcHa_2005*100)/area_mun,
#          nvcPerc_2009 = (nvcHa_2009*100)/area_mun,
#          defPerc_2005 = 100 - nvcPerc_2005,
#          defPerc_2009 = 100 - nvcPerc_2009) %>%
#   glimpse -> data2
# writexl::write_xlsx(x= data2, path = "data/db3cap1_cbps_clean.xlsx")

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
####outcome IDHM_E####
model.idhmE_2005<- glm(data = data[-142,], 
                       IDHM_E_2010 ~
                         defPerc_2005 +
                         I(defPerc_2005^2),
                       weights = modeldef$weights
)
summary(model.idhmE_2005)

####outcome IDHM_L####
model.idhmL_2005<- glm(data = data[-142,], 
                       IDHM_L_2010 ~
                         defPerc_2005 +
                         I(defPerc_2005^2),
                       weights = modeldef$weights
)
summary(model.idhmL_2005)

####outcome IDHM_R####
model.idhmR_2005<- glm(data = data[-142,], 
                       IDHM_R_2010 ~
                         defPerc_2005 +
                         I(defPerc_2005^2),
                       weights = modeldef$weights
)
summary(model.idhmR_2005)

####outcome expov----
glm(data = data[-142,], 
    expov_2010 ~
      defPerc_2005 +
      I(defPerc_2005^2),
    weights = modeldef$weights
) -> model.expov05
summary(model.expov05)

####outcome gini####
model.gini_2005<- glm(data = data[-142,], 
                      gini_2010 ~
                        defPerc_2005 +
                        I(defPerc_2005^2),
                      weights = modeldef$weights
)
summary(model.gini_2005)


####outcome u5mort####
model.u5mort_2005<- glm(data = data[-142,], 
                        u5mort_2010 ~
                          defPerc_2005 +
                          I(defPerc_2005^2),
                        weights = modeldef$weights
)
summary(model.u5mort_2005)


##deforestation data 2009----
####outcome IDHM_E####
model.idhmE_2009<- glm(data = data[-142,], 
                       IDHM_E_2010 ~
                         defPerc_2009 +
                         I(defPerc_2009^2),
                       weights = modeldef$weights
)
summary(model.idhmE_2009)

####outcome IDHM_L####
model.idhmL_2009<- glm(data = data[-142,], 
                       IDHM_L_2010 ~
                         defPerc_2009 +
                         I(defPerc_2009^2),
                       weights = modeldef$weights
)
summary(model.idhmL_2009)

####outcome IDHM_R####
model.idhmR_2009<- glm(data = data[-142,], 
                       IDHM_R_2010 ~
                         defPerc_2009 +
                         I(defPerc_2009^2),
                       weights = modeldef$weights
)
summary(model.idhmR_2009)

####outcome expov----
glm(data = data[-142,], 
    expov_2010 ~
      defPerc_2009 +
      I(defPerc_2009^2),
    weights = modeldef$weights
) -> model.expov09
summary(model.expov09)

####outcome gini####
model.gini_2009<- glm(data = data[-142,], 
                      gini_2010 ~
                        defPerc_2009 +
                        I(defPerc_2009^2),
                      weights = modeldef$weights
)
summary(model.gini_2009)


####outcome u5mort####
model.u5mort_2009<- glm(data = data[-142,], 
                        u5mort_2010 ~
                          defPerc_2009 +
                          I(defPerc_2009^2),
                        weights = modeldef$weights
)
summary(model.u5mort_2009)
