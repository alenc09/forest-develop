# Wed Nov  9 14:23:00 2022 ------------------------------
#script for lagged rmANOVA deforestation analysis

#library----
library(readxl)
library(here)
library(dplyr)
library(stringr)
library(tidyr)
library(lme4)
library(emmeans)
library(MuMIn)
library(cAIC4)

#data----
read_xlsx(here("data/dbcap1_rma.xlsx"))-> db_rma
read_xlsx(path = "/home/alenc/Documents/Doutorado/Dados/mapb5_cobertura_selec.xlsx") ->mapb5_cover
read_xlsx("/home/alenc/Documents/Doutorado/tese/base_geral_1.xlsx")-> base_geral


##manipultion----
mapb5_cover %>% 
  filter(level_1 == "1 - Forest") %>%
  rename(code_muni = territory_id) %>%
  group_by(code_muni) %>%
  summarise(across(.cols = `1985`:`2010`,
                   .fns = sum,
                   .names = "nvcHa_{.col}")) %>%
  right_join(x = ., y = db_rma, by = "code_muni") %>%
  select(-c(49:57)) %>% 
  right_join(x = ., y = select(base_geral, code_muni, area_mun),
             by = "code_muni") %>%
  mutate(across(.cols = starts_with("nvcHa"),
                 .fns = ~ . *100)/area_mun) %>%
  mutate(across(.cols = starts_with("nvcHa"),
                .fns = ~100 - . )) %>%
  rename_with(.fn = ~ str_replace(string = . ,
              pattern = "nvcHa",
              replacement = "defPerc")) %>%
  rename(u5mort_2010 = U5mort_2010) %>% 
  pivot_longer(cols = -code_muni,
               names_to = c(".value", "year"),
               names_pattern = "(.+)_(.+)") %>% 
  mutate(code_muni = as.factor(code_muni),
         year = as.double(year)
         ) %>% 
  select(-area) %>% 
  filter(year != "mun") %>% 
  mutate(defStage = if_else(condition = defPerc >= 0 & defPerc <= 33 ,
                            true = 1,
                            false = if_else(condition = defPerc > 33 & defPerc <= 66,
                                            true = 2,
                                            false = if_else(condition = defPerc > 66 & defPerc <= 100,
                                                            true = 3,
                                                            false = 0)))) %>%
  glimpse -> db_rma_lag

db_rma_lag %>% 
  #filter(year <= 1991) %>% 
  pivot_wider(names_from = year,
              names_sep = "_",
              values_from = 3:11) %>% 
  #mutate(db_lag_na[,c(9:14)] = gini_1991) %>% 
  glimpse -> db_lag_na

writexl::write_xlsx(x = db_lag_na, path = "data/db_lag_na.xlsx")

#Need to fill the NA's from lagged deforestation with the reference year for
#socioeconomic data. Don't know how to do this in R :( so I cheated and used
#Calc for that. Then I imported the filled data into R.

read_xlsx(path = "data/db_lag_full.xlsx")-> db_lag_full

rma_lag_fun <- function(year1, year2, year3){
  db_lag_full %>% 
    pivot_longer(cols = -code_muni,
                 names_to = c(".value", "year"),
                 names_pattern = "(.+)_(.+)") %>%
    na.omit %>% 
    mutate(code_muni = as.factor(code_muni),
           year = as.factor(year),
           defStage = as.factor(defStage)) %>% 
    filter(year == year1 | year == year2 | year == year3) %>%  
    glimpse
}

#ma_lag_fun(year1 = 1985, year2 = 1994, year3 = 2004) -> rma_lag_6
rma_lag_fun(1986, 1995, 2005) -> rma_lag_5
rma_lag_fun(1987, 1996, 2006) -> rma_lag_4
rma_lag_fun(1988, 1997, 2007) -> rma_lag_3
rma_lag_fun(1989, 1998, 2008) -> rma_lag_2
rma_lag_fun(1990, 1999, 2009) -> rma_lag_1
rma_lag_fun(1991, 2000, 2010) -> rma_lag_0
list(rma_lag_0, rma_lag_1, rma_lag_2, rma_lag_3, rma_lag_4, rma_lag_5) -> list_lags

#Analysis----
##effect size with lagged data---- 
c()-> list_lmer

for (i in colnames(rma_lag_0[, 4:10])) {
  lapply(list_lags,
         lmer,
         formula = paste(i, "~ defStage*year + (1 | code_muni)"),
         REML = F) -> list_lmer[[i]]
} 

lapply(unlist(list_lmer),
       emmeans,
       pairwise ~ defStage * year,
       pbkrtest.limit = 3621) -> list_pw

lapply(list_pw[1:6], summary)

##Model averaging----
###HDI - Education----
unlist(list_lmer) -> unlist_lmer
   unlist_lmer[25:30] -> rmaLag_mods_idhE

model.avg(rmaLag_mods_idhE) %>% 
  #get.models(subset = delta < 2) %>% 
  #model.avg() %>% 
  #coefTable()
  summary


###HDI - Longevity----
unlist_lmer[31:36] -> rmaLag_mods_idhL

model.avg(rmaLag_mods_idhL) %>% 
summary

###HDI - Income----
unlist_lmer[37:42] -> rmaLag_mods_idhI

model.avg(rmaLag_mods_idhI) %>%
  get.models(subset = delta < 2) %>%
  model.avg %>%
  summary

list_pw[["IDHM_R7"]][["contrasts"]] %>% 
  as_tibble %>% 
  mutate(weight = 0.68,
         code = c(1:36)) %>% 
  glimpse -> pw_idhr_0


c("1 1991 - 1 2000",
  "1 2000 - 1 2010",
  "1 1991 - 1 2010",
  "2 1991 - 2 2000",
  "2 2000 - 2 2010",
  "2 1991 - 2 2010",
  "3 1991 - 3 2000",
  "3 2000 - 3 2010",
  "3 1991 - 3 2010",
  "1 1991 - 2 1991",
  "1 2000 - 2 2000",
  "1 2010 - 2 2010",
  "1 1991 - 3 1991",
  "1 2000 - 3 2000",
  "1 2010 - 3 2010",
  "2 1991 - 3 1991",
  "2 2000 - 3 2000",
  "2 2010 - 3 2010") -> contrast.x
as_tibble(contrast.x) -> contrast.x
colnames(contrast.x)<- "contrast.x"

list_pw[["IDHM_R6"]][["contrasts"]] %>% 
  as_tibble %>% 
  mutate(weight = 0.32,
         code = c(1:36)) %>% 
  left_join(x = pw_idhr_0, y = ., by = "code") %>% 
  mutate(avg_coef = (estimate.x*weight.x + estimate.y*weight.y)/(weight.x + weight.y),
         avg_se = (SE.x*weight.x + SE.y*weight.y)/(weight.x+weight.y) ) %>% 
  select(contrast.x, avg_coef, avg_se) %>% 
  left_join(x = as_tibble(contrast.x), y = .) %>% 
  glimpse() -> pw_idhr_weights



###Extreme poverty ----
unlist_lmer[13:18] -> rmaLag_mods_expov

model.avg(rmaLag_mods_expov) %>%
  get.models(subset = delta < 2) %>%
  model.avg %>%
  summary

list_pw[["expov7"]][["contrasts"]] %>% 
  as_tibble %>% 
  mutate(weight = 0.57,
         code = c(1:36)) %>% 
  glimpse -> pw_expov_lag0

list_pw[["expov6"]][["contrasts"]] %>% 
  as_tibble %>% 
  mutate(weight = 0.43,
         code = c(1:36)) %>% 
   left_join(x = pw_expov_lag0, y = ., by = "code") %>%
  mutate(avg_coef = (estimate.x*weight.x + estimate.y*weight.y)/(weight.x + weight.y),
         avg_se = (SE.x*weight.x + SE.y*weight.y)/(weight.x+weight.y) ) %>%
  select(contrast.x, avg_coef, avg_se) %>%
  left_join(x = as_tibble(contrast.x), y = .) %>%
  glimpse() -> pw_expov_weights

###Gini index ----
unlist_lmer[1:6] -> rmaLag_mods_gini

model.avg(rmaLag_mods_gini) %>% 
  get.models(subset = delta < 2) %>% 
  model.avg() %>% 
  summary

list_pw[["gini7"]][["contrasts"]] %>% 
  as_tibble %>% 
  mutate(weight = 0.51,
         code = c(1:36)) %>% 
  glimpse -> pw_gini_lag0

list_pw[["gini6"]][["contrasts"]] %>% 
  as_tibble %>% 
  mutate(weight = 0.27,
         code = c(1:36)) %>% 
  glimpse -> pw_gini_lag1

list_pw[["gini5"]][["contrasts"]] %>% 
  as_tibble %>% 
  mutate(weight = 0.21,
         code = c(1:36)) %>% 
  glimpse -> pw_gini_lag2

pw_gini_lag0 %>% 
  left_join(pw_gini_lag1, by = "code") %>% 
  left_join(pw_gini_lag2, by = "code") %>% 
  mutate(avg_coef = (estimate.x*weight.x + estimate.y*weight.y +estimate*weight)/(weight.x + weight.y + weight),
         avg_SE = (SE.x*weight.x + SE.y*weight.y + SE*weight)/(weight.x + weight.y + weight)) %>% 
  select(contrast.x, avg_coef, avg_SE) %>%
  left_join(x = contrast.x, y = .) %>%
  glimpse -> pw_gini_weights

###Underf five mortality----
unlist_lmer[7:12] -> rmaLag_mods_u5mort

model.avg(rmaLag_mods_u5mort) %>% 
  summary

list_pw[["u5mort4"]][["contrasts"]] %>% 
  as_tibble %>% 
  select(contrast, estimate, SE, p.value) %>%
  glimpse -> pw_u5mort_lag3

##change in number of municipalities per deforestation stage----
db_lag_full %>% 
  mutate(across(.cols = defStage_1985:defStage_2010,.fns = as.factor)) %>%
  pivot_longer(cols = defStage_1985:defStage_2010,
               names_to = "year",
               names_prefix = "defStage_",
               values_to = "defStage"
               ) %>%
  group_by(year, defStage) %>%
  summarise(n_mun = n()) %>%
  glimpse -> tab_mun_n

db_lag_full%>%
  filter(defStage_2010 == 3 & defStage_2004 == 2) %>% #change the values to change what groups to compare
  nrow() 
  