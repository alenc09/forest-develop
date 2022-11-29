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
library(ggplot2)
library(ggpubr)
library(MuMIn)
library(dotwhisker)
library(ggforce)
library(ggbreak)

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

lapply(dplyr:::select(.data = data[-142, ],
                      defPerc_2010, defPerc_2009:defPerc_2005),
       FUN = glm_lag_IDHE)-> l_lag_idhE

lapply(l_lag_idhE, summary)

model.avg(l_lag_idhE) %>% 
  summary() -> avg_lag_idhE 

####outcome IDHM_L####
glm_lag_IDHL <- function(i){
  glm(data = data[-142,], 
      IDHM_L_2010 ~
        i + I(i ^ 2),
      weights = modeldef$weights)} 

lapply(dplyr:::select(.data = data[-142, ], 
                      defPerc_2010, defPerc_2009:defPerc_2005),
       FUN = glm_lag_IDHL)-> l_lag_idhL
lapply(l_lag_idhL, summary)

model.avg(l_lag_idhL) -> avg_lag_idhL
summary(avg_lag_idhL)

avg_lag_idhL %>%   
get.models(subset = delta < 2) %>%
  model.avg() %>% 
  summary -> sub_avg_lag_idhL

# plot(x = avg_lag_idhL,
#           labels = c("def", "def_sqr"),
#           main = NULL,
#           intercept = F, horizontal = F)

coefTab_avg_lag_idhL_full %>%
   as_tibble() %>%
  # mutate(var = as.factor(c("def", "def^2")),
  #        mod = as.factor(c("idhL", "idhL"))) %>%
  # rbind(tab_avg_mods) %>% 
  glimpse #-> tab_avg_mods

####outcome IDHM_R####
glm_lag_IDHR <- function(i){
  glm(data = data[-142,], 
      IDHM_R_2010 ~
        i + I(i ^ 2),
      weights = modeldef$weights)} 

lapply(dplyr:::select(.data = data[-142, ], 
                      defPerc_2010, defPerc_2009:defPerc_2005),
       FUN = glm_lag_IDHR)-> l_lag_idhR
lapply(l_lag_idhR, summary)

model.avg(l_lag_idhR) -> avg_lag_idhR
summary(avg_lag_idhR)

avg_lag_idhR %>% 
  get.models(subset = delta < 2) %>% 
  model.avg() %>% 
  summary -> sub_avg_lag_idhR


coefTable(avg_lag_idhR, full = TRUE)-> coefTab_avg_lag_idhR
# plot(x = avg_lag_idhL,
#           labels = c("def", "def_sqr"),
#           main = NULL,
#           intercept = F, horizontal = F)

coefTab_avg_lag_idhR[-1,-3] %>%
  as_tibble() %>%
  mutate(var = as.factor(c("def", "def^2")),
         mod = as.factor(c("idhR", "idhR"))) %>%
  rbind(tab_avg_mods) %>% 
  glimpse -> tab_avg_mods

####outcome expov----
glm_lag_expov <- function(i){
  glm(data = data[-142,], 
      expov_2010 ~
        i + I(i ^ 2),
        weights = modeldef$weights)}

lapply(dplyr:::select(.data = data[-142, ], 
                      defPerc_2010, defPerc_2009:defPerc_2005),
       FUN = glm_lag_expov)-> l_lag_expov
lapply(l_lag_expov, summary)

model.avg(l_lag_expov) -> avg_lag_expov
summary(avg_lag_expov)

avg_lag_expov %>% 
  get.models(subset = delta < 2) %>% 
  model.avg() %>% 
  summary -> sub_avg_lag_expov
coefTable(avg_lag_expov, full = TRUE)-> coefTab_avg_lag_expov

# plot(x = avg_lag_idhL,
#           labels = c("def", "def_sqr"),
#           main = NULL,
#           intercept = F, horizontal = F)

coefTab_avg_lag_expov[-1,-3] %>%
  as_tibble() %>%
  mutate(var = as.factor(c("def", "def^2")),
         mod = as.factor(c("expov", "expov"))) %>%
  rbind(tab_avg_mods) %>% 
  glimpse -> tab_avg_mods

####outcome gini####
glm_lag_gini <- function(i){
  glm(data = data[-142,], 
      gini_2010 ~
        i + I(i ^ 2),
      weights = modeldef$weights)}

lapply(dplyr:::select(.data = data[-142, ], 
                      defPerc_2010, defPerc_2009:defPerc_2005),
       FUN = glm_lag_gini)-> l_lag_gini
lapply(l_lag_gini, summary)

model.avg(l_lag_gini) -> avg_lag_gini 
summary(avg_lag_gini)

coefTable(avg_lag_gini, full = TRUE)-> coefTab_avg_lag_gini

# plot(x = avg_lag_idhL,
#           labels = c("def", "def_sqr"),
#           main = NULL,
#           intercept = F, horizontal = F)

coefTab_avg_lag_gini[-1,-3] %>%
  as_tibble() %>%
  mutate(var = as.factor(c("def", "def^2")),
         mod = as.factor(c("gini", "gini"))) %>%
  rbind(tab_avg_mods) %>% 
  glimpse -> tab_avg_mods


####outcome u5mort####
glm_lag_u5mort <- function(i){
  glm(data = data[-142,], 
      u5mort_2010 ~
        i + I(i ^ 2),
      weights = modeldef$weights)}

lapply(dplyr:::select(.data = data[-142, ], 
                      defPerc_2010, defPerc_2009:defPerc_2005),
       FUN = glm_lag_u5mort)-> l_lag_u5mort
lapply(l_lag_u5mort, summary)

model.avg(l_lag_u5mort) -> avg_lag_u5mort 
summary(avg_lag_u5mort)

avg_lag_u5mort %>% 
get.models(subset = delta < 2) %>% 
  model.avg() %>% 
  summary() -> sub_avg_lag_u5mort



coefTable(avg_lag_u5mort, full = TRUE)-> coefTab_avg_lag_u5mort

# plot(x = avg_lag_idhL,
#           labels = c("def", "def_sqr"),
#           main = NULL,
#           intercept = F, horizontal = F)

coefTab_avg_lag_u5mort[-1,-3] %>%
  as_tibble() %>%
  mutate(var = as.factor(c("def", "def^2")),
         mod = as.factor(c("u5mort", "u5mort"))) %>%
  rbind(tab_avg_mods) %>% 
  glimpse -> tab_avg_mods

#Figures----
##IDHM_E----
lapply(l_lag_idhE, summary) %>% 
  lapply(coef) %>% 
  .$defPerc_2005 %>% 
  as_tibble() %>% 
  slice(-1) %>% 
  mutate(var = as.factor(c("def", "def^2"))) %>% 
  ggplot(aes(x = var, y = Estimate, colour = var))+ 
  geom_hline(yintercept = 0, linewidth = 0.1, color = "red")+
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`),
                linewidth = 0.3,
                width = 0.1,
                colour = "#5c3811")+
  geom_point()+
  scale_y_break(breaks = c(0.000002, 0.0002), scales = 10, expand = expansion(mult = 0.22))+
  scale_x_discrete(label = c("DEF","DEF²"))+
  scale_color_manual(values = c("#ffa600", "#ffa600"))+
  labs(x = "HDI - Education", y = "Averaged coefficient and standard error")+
  theme(panel.background = element_blank(),
        axis.line.x.bottom = element_line(colour = "black",linewidth = 0.2),
        axis.line.y.left = element_line(colour = "black",linewidth = 0.2),
        legend.position = "none",
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_line(linewidth = 0),
        plot.title = element_text(hjust = 0.5, size = 10))-> avg_fig_IDHE
  
ggsave(plot = avg_fig_IDHE,
       filename = "/home/alenc/Documents/Doutorado/tese/cap1/Manuscript/figures/avg_fig_IDHE.png")

##IDHM_L----
sub_avg_lag_idhL %>% 
  coefTable() %>% 
  as_tibble() %>% 
  slice(-1) %>% 
  mutate(var = as.factor(c("def", "def^2"))) %>% 
  glimpse %>% 
  ggplot(aes(x = var, y = Estimate, colour = var))+
  geom_hline(yintercept = 0, linewidth = 0.1, color = "red")+
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`),
                linewidth = 0.3,
                width = 0.1,
                colour = "#5c3811")+
  geom_point()+
  scale_y_break(breaks = c(0.000002, 0.0002), scales = 10, expand = expansion(mult = 0.22))+
  scale_x_discrete(label = c("DEF","DEF²"))+
  scale_color_manual(values = c("#ffa600", "#ffa600"))+
  labs(x = "HDI - Longevity", y = "Averaged coefficient and standard error")+
  theme(panel.background = element_blank(),
        axis.line.x.bottom = element_line(colour = "black",linewidth = 0.2),
        axis.line.y.left = element_line(colour = "black",linewidth = 0.2),
        legend.position = "none",
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_line(linewidth = 0),
        plot.title = element_text(hjust = 0.5, size = 10)) -> avg_fig_IDHL

ggsave(plot = avg_fig_IDHL,
       filename = "/home/alenc/Documents/Doutorado/tese/cap1/Manuscript/figures/avg_fig_IDHL.png")

##IDHM_R----
tab_avg_mods %>%
  filter(mod == "idhR") %>% 
  ggplot(aes(x = var, y = Estimate, colour = var))+
  geom_hline(yintercept = 0, linewidth = 0.1, color = "red")+
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`),
                linewidth = 0.3,
                width = 0.1,
                colour = "#5c3811")+
  geom_point()+
  scale_x_discrete(label = c("DEF","DEF²"))+
  scale_color_manual(values = c("#ffa600", "#ffa600"))+
  labs(x = "HDI - Income", y = "Averaged coefficient and standard error")+
  theme(panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10))+
  facet_zoom(ylim= c(-0.00005, 0.00003)) -> avg_fig_IDHR

ggsave(plot = avg_fig_IDHR,
       filename = "/home/alenc/Documents/Doutorado/tese/cap1/Manuscript/figures/avg_fig_IDHR.png")

##Extreme poverty----
tab_avg_mods %>%
  filter(mod == "expov") %>% 
  ggplot(aes(x = var, y = Estimate, colour = var))+
  geom_hline(yintercept = 0, linewidth = 0.1, color = "red")+
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`),
                linewidth = 0.3,
                width = 0.1,
                colour = "#5c3811")+
  geom_point()+
  scale_x_discrete(label = c("DEF","DEF²"))+
  scale_color_manual(values = c("#ffa600", "#ffa600"))+
  labs(x = "Extreme poverty", y = "Averaged coefficient and standard error")+
  theme(panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10))+
  facet_zoom(ylim= c(-0.005, 0.003)) -> avg_fig_expov

ggsave(plot = avg_fig_expov,
       filename = "/home/alenc/Documents/Doutorado/tese/cap1/Manuscript/figures/avg_fig_expov.png")

##gini----
tab_avg_mods %>%
  filter(mod == "gini") %>% 
  ggplot(aes(x = var, y = Estimate, colour = var))+
  geom_hline(yintercept = 0, linewidth = 0.1, color = "red")+
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`),
                linewidth = 0.3,
                width = 0.1,
                colour = "#5c3811")+
  geom_point()+
  scale_x_discrete(label = c("DEF","DEF²"))+
  scale_color_manual(values = c("#ffa600", "#ffa600"))+
  labs(x = "Income inequality", y = "Averaged coefficient and standard error")+
  theme(panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10))+
  facet_zoom(ylim= c(-0.00005, 0.00003)) -> avg_fig_gini

ggsave(plot = avg_fig_gini,
       filename = "/home/alenc/Documents/Doutorado/tese/cap1/Manuscript/figures/avg_fig_gini.png")

##u5mort----
tab_avg_mods %>%
  filter(mod == "u5mort") %>% 
  ggplot(aes(x = var, y = Estimate, colour = var))+
  geom_hline(yintercept = 0, linewidth = 0.1, color = "red")+
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`),
                linewidth = 0.3,
                width = 0.1,
                colour = "#5c3811")+
  geom_point()+
  scale_x_discrete(label = c("DEF","DEF²"))+
  scale_color_manual(values = c("#ffa600", "#ffa600"))+
  labs(x = "Under five mortality", y = "Averaged coefficient and standard error")+
  theme(panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10))+
  facet_zoom(ylim= c(-0.005, 0.003)) -> avg_fig_u5mort

ggsave(plot = avg_fig_u5mort,
       filename = "/home/alenc/Documents/Doutorado/tese/cap1/Manuscript/figures/avg_fig_u5mort.png")