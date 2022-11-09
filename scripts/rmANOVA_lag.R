# Wed Nov  9 14:23:00 2022 ------------------------------
#script for lagged rmANOVA deforestation analysis

#library----
library(readxl)
library(here)
library(dplyr)
library(stringr)
library(tidyr)

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
  mutate(year = as.factor(year),
         code_muni = as.factor(code_muni)) %>% 
  select(-area) %>% 
  mutate(defStage = if_else(condition = defPerc > 0 & defPerc <= 33 ,
                            true = 1,
                            false = if_else(condition = defPerc > 33 & defPerc <= 66,
                                            true = 2,
                                            false = if_else(condition = defPerc > 66 & defPerc <= 100,
                                                            true = 3,
                                                            false = 0)))) %>%
  glimpse -> db_rma_lag

#Analysis----
#defStage change----

##IDHM_E----
lmer(data = , formula = IDHM_E ~ def.stage*year + (1 | code_muni)) -> lmer.idhE
tab.idhE<- car::Anova(lmer.idhE, type = "II")
summary(lmer.idhE)
pw.idhE<- emmeans(lmer.idhE, pairwise ~ def.stage*year, pbkrtest.limit = 3621)
