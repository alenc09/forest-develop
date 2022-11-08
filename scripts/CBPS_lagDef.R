# Tue Nov  1 15:50:21 2022 ------------------------------
#Script to build new CBPS models using lagged deforestation

#Libraries----
library(readxl)
library(dplyr)
library(CBPS)
library(geobr)
library(spdep)
library(stringr)
library(performance)

#data----
# read_xlsx(path = "/home/alenc/Documents/Doutorado/Dados/mapb5_cobertura_selec.xlsx") ->mapb5_cover
# read_xlsx(path = "data/db2cap1_cbps_clean.xlsx") -> data
read_xlsx(path = "data/db3cap1_cbps_clean.xlsx") -> data

##data manipulation----
# mapb5_cover %>% 
#   filter(level_1 == "1 - Forest") %>%
#   rename(code_muni = territory_id) %>%
#   group_by(code_muni) %>%
#   summarise(across(.cols = `2001`:`2009`,
#                    .fns = sum,
#                    .names = "nvcHa_{.col}")) %>%
#   right_join(x = ., y = data, by = "code_muni") %>%
#   mutate(across(.cols = starts_with("nvcHa"),
#                  .fns = ~ . *100)/area_mun) %>% 
#   mutate(across(.cols = starts_with("nvcHa"),
#                 .fns = ~100 - . )) %>% 
#   rename_with(.fn = ~ str_replace(string = . ,
#               pattern = "nvcHa",
#               replacement = "defPerc")) %>% 
#   glimpse -> data2
# writexl::write_xlsx(x= data2, path = "data/db3cap1_cbps_clean.xlsx")

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

glm_lag_IDHE <- function(i){
  glm(data = data[-142,], 
      IDHM_E_2010 ~
        i + I(i ^ 2),
      weights = modeldef$weights)} 

lapply(dplyr:::select(.data = data[-142, ], starts_with("defPerc")),
       FUN = glm_lag_IDHE)-> l_lag_idhE
compare_performance(l_lag_idhE, metrics = "common",rank = T) -> compare_lag_idhE


####outcome IDHM_L####
glm_lag_IDHL <- function(i){
  glm(data = data[-142,], 
      IDHM_L_2010 ~
        i + I(i ^ 2),
      weights = modeldef$weights)} 

lapply(dplyr:::select(.data = data[-142, ], starts_with("defPerc")),
       FUN = glm_lag_IDHL)-> l_lag_idhL
compare_performance(l_lag_idhL, metrics = "common",rank = T) -> compare_lag_idhL

####outcome IDHM_R####
glm_lag_IDHR <- function(i){
  glm(data = data[-142,], 
      IDHM_R_2010 ~
        i + I(i ^ 2),
      weights = modeldef$weights)} 

lapply(dplyr:::select(.data = data[-142, ], starts_with("defPerc")),
       FUN = glm_lag_IDHR)-> l_lag_idhR
compare_performance(l_lag_idhR, metrics = "common",rank = T) -> compare_lag_idhR

####outcome expov----
glm_lag_expov <- function(i){
  glm(data = data[-142,], 
      expov_2010 ~
        i + I(i ^ 2),
      weights = modeldef$weights)} 

lapply(dplyr:::select(.data = data[-142, ], starts_with("defPerc")),
       FUN = glm_lag_expov)-> l_lag_expov
compare_performance(l_lag_expov, metrics = "common",rank = T) -> compare_lag_expov

####outcome gini####
glm_lag_gini <- function(i){
  glm(data = data[-142,], 
      gini_2010 ~
        i + I(i ^ 2),
      weights = modeldef$weights)} 

lapply(dplyr:::select(.data = data[-142, ], starts_with("defPerc")),
       FUN = glm_lag_gini)-> l_lag_gini
compare_performance(l_lag_gini, metrics = "common",rank = T) -> compare_lag_gini



####outcome u5mort####
glm_lag_u5mort <- function(i){
  glm(data = data[-142,], 
      u5mort_2010 ~
        i + I(i ^ 2),
      weights = modeldef$weights)} 

lapply(dplyr:::select(.data = data[-142, ], starts_with("defPerc")),
       FUN = glm_lag_u5mort)-> l_lag_u5mort
compare_performance(l_lag_u5mort, metrics = "common",rank = T) -> compare_lag_u5mort

#export----
write.table(x = compare_lag_idhE, file = "data/comp_lag_idhE.csv", sep = "|", dec = ".")
write.table(x = compare_lag_idhL, file = "data/comp_lag_idhL.csv", sep = "|", dec = ".")
write.table(x = compare_lag_idhR, file = "data/comp_lag_idhR.csv", sep = "|", dec = ".")
write.table(x = compare_lag_expov, file = "data/comp_lag_expov.csv", sep = "|", dec = ".")
write.table(x = compare_lag_gini, file = "data/comp_lag_gini.csv", sep = "|", dec = ".")
write.table(x = compare_lag_u5mort, file = "data/comp_lag_u5mort.csv", sep = "|", dec = ".")
