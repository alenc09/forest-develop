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

rma_lag_fun(year1 = 1985, year2 = 1994, year3 = 2004) -> rma_lag_6
rma_lag_fun(1986, 1995, 2005) -> rma_lag_5
rma_lag_fun(1987, 1996, 2006) -> rma_lag_4
rma_lag_fun(1988, 1997, 2007) -> rma_lag_3
rma_lag_fun(1989, 1998, 2008) -> rma_lag_2
rma_lag_fun(1990, 1999, 2009) -> rma_lag_1
rma_lag_fun(1991, 2000, 2010) -> rma_lag_0
list(rma_lag_6, rma_lag_5, rma_lag_4, rma_lag_3,rma_lag_2, rma_lag_1, rma_lag_0) -> list_lags

#Analysis----

c()-> list_lmer

for (i in colnames(rma_lag_0[,4:10])) {
  lapply(list_lags, lmer, formula = paste(i, "~ defStage*year + (1 | code_muni)")) -> list_lmer[[i]]
  } 

lapply(unlist(list_lmer), emmeans, pairwise ~ defStage*year, pbkrtest.limit = 3621) -> list_pw
