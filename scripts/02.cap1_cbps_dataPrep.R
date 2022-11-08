# Tue Nov  8 11:49:32 2022 ------------------------------
#Script to build the data table for propensity score analysis

#Libraries----
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(maptools)

#Data preparation####

dbtese_17<- read_xlsx("/home/lucas/Documents/Doutorado/tese/dbtese_17.xlsx")
dbcap2_1<- read_xlsx("/home/lucas/Documents/Doutorado/tese/cap2/dbcap2_1.xlsx")
zh_data<- read.csv("/home/lucas/Documents/Doutorado/Disciplinas/Quasi-experimental_approach/ZH_analysis/Data/ZHdf/ZeroHungerDF_2020.csv")
atlasbr_data<- read_xlsx("/home/lucas/Documents/Doutorado/Dados/atlas_dadosbrutos_00_10.xlsx", sheet = 2)

dbcap1_PS <-
  left_join(
    x = dplyr:::select(
      .data = dbtese_17,
      code_muni,
      area_mun,
      nvc_2000,
      nvc_2010,
      gini_2000,
      gini_2010,
      u5mort_2000,
      u5mort_2010.x,
      expov_2000,
      expov_2010
    ),
    y = dplyr:::select(
      .data = dbcap2_1,
      code_muni,
      txDesemp_00,
      txDesemp_10,
      finanPerc_06,
      finanPerc_17,
      bovMed_06,
      bovMed_17,
      capMed_06,
      capMed_17,
      beanMed_06,
      beanMed_17,
      cornMed_06,
      cornMed_17
    ),
    by = "code_muni"
  )

dbcap1_PS %>%
  left_join(
    y = dplyr:::select(
      .data = zh_data,
      IBGECode7digit,
      Pop2000,
      Pop2010,
      GDPPerCapPublicrealN2000for2000Analysis1000,
      MeanSlopefor2004analysis,
      MeanElevationfor2004analysis,
      Sumkm2_ProtectedArea2004,
      Sumkm2_ProtectedArea2013
    ),
    by = c("code_muni" = "IBGECode7digit")
  ) -> dbcap1_PS

atlasbr_data %>%
  dplyr:::select(Codmun7, i, IDHM, IDHM_E, IDHM_L, IDHM_R) %>%
  pivot_wider(
    id_cols = Codmun7,
    names_from = i,
    values_from = c(IDHM, IDHM_E, IDHM_L, IDHM_R),
    names_sep = "_"
  ) %>%
  left_join(x = dbcap1_PS,
            y = .,
            by = c("code_muni" = "Codmun7")) %>%
  
  mutate(
    dbcap1_PS,
    area_munKm2 = area_mun / 100,
    nvc_00Km2 = nvc_2000 / 100,
    nvc_10Km2 = nvc_2010 / 100,
    nvcPerc_00 = (nvc_00Km2 / area_munKm2) * 100,
    nvcPerc_10 = (nvc_10Km2 / area_munKm2) * 100,
    popDens_00 = Pop2000 / area_munKm2,
    popDens_10 = Pop2010 / area_munKm2,
    .keep = 'unused'
  ) %>%
  
  dplyr:::select(
    code_muni,
    nvcPerc_00,
    nvcPerc_10,
    IDHM_2000,
    IDHM_2010,
    IDHM_E_2000,
    IDHM_E_2010,
    IDHM_L_2000,
    IDHM_L_2010,
    IDHM_R_2000,
    IDHM_R_2010,
    expov_2000,
    expov_2010,
    gini_2000,
    gini_2010,
    u5mort_2000,
    u5mort_2010.x,
    txDesemp_00,
    txDesemp_10,
    finanPerc_06,
    finanPerc_17,
    bovMed_06,
    bovMed_17,
    capMed_06,
    capMed_17,
    beanMed_06,
    beanMed_17,
    cornMed_06,
    cornMed_17,
    popDens_00,
    popDens_10,
    GDPPerCapPublicrealN2000for2000Analysis1000,
    MeanSlopefor2004analysis,
    MeanElevationfor2004analysis,
    Sumkm2_ProtectedArea2004,
    Sumkm2_ProtectedArea2013
  ) %>%
  
  glimpse() -> dbcap1_PS
writexl::write_xlsx(dbcap1_PS,path = "/home/lucas/Documents/Doutorado/tese/cap1/forest-develop/dbcap1_clean.xlsx")

##################################################################
#arquivo para análise com novas variárveis - db2cap1_clean####

read_xlsx("/home/lucas/Documentos/Doutorado/Dados/mapb5_cobertura_selec.xlsx")->mapbiomas5_cobertura
mapbiomas5_cobertura%>%
  dplyr::select(territory_id, `2010`)%>%
  group_by(territory_id)%>%
  summarise(nvcHa_2010 = sum(`2010`))%>%
  glimpse()-> nvcHa_2010

read_xlsx("/home/lucas/Documentos/Doutorado/tese/dbtese_17.xlsx")->dbtese_17
read_xlsx("/home/lucas/Documentos/Doutorado/tese/cap1/forest-develop/dbcap1_clean.xlsx")-> dbcap1_clean

dbcap1_clean %>%
  dplyr::select(
    code_muni,
    IDHM_2010,
    IDHM_E_2010,
    IDHM_L_2010,
    IDHM_R_2010,
    expov_2010,
    gini_2010,
    u5mort_2010.x
  ) %>%
  left_join(y = nvcHa_2010, by = c("code_muni" = "territory_id")) %>%
  glimpse() -> db2cap1_cbps

dbtese_17 %>%
  dplyr::select(code_muni,
         code_state,
         area_mun,
         CODIBGE_short) %>%
  left_join(x = db2cap1_cbps, y = .) %>%
  glimpse() -> db2cap1_cbps

db2cap1_cbps %>%
  mutate(nvcPerc_2010 = (nvcHa_2010 / area_mun) * 100) %>%
  glimpse()->db2cap1_cbps

dbcap2_1<- read_xlsx("/home/lucas/Documentos/Doutorado/tese/cap_2/dbcap2_clean.xlsx")

dbcap2_1%>%
  dplyr::select(code_muni,
         p_agro_10,
         perc_rur_10,
         perc_urb_10,
         prodAss_06,
         nascProt_06,
         riosProt_06,
         irrigPerc_06)%>%
  left_join(x = db2cap1_cbps, y=.)%>%
  glimpse()-> db2cap1_cbps

var_prec<- read_xlsx("/home/lucas/Documentos/Doutorado/projetos_paralelos/artigo_wivi/Water Insecurity and Vulnerability Index - old database.xlsx")
var_prec%>%
  dplyr::select(code_short,
         rain_var)%>%
  left_join(x = db2cap1_cbps, y=., by = c("CODIBGE_short" = "code_short"))%>%
  glimpse()-> db2cap1_cbps

area_props<- read_xlsx("/home/lucas/Documentos/Doutorado/Dados/area_propriedades.xlsx")
area_props%>%
  mutate(areaProps_total = areaProp_S + areaProp_M + areaProp_L,
         percProp_L = (areaProp_L/areaProps_total)*100,
         percProp_M = (areaProp_M/areaProps_total)*100,
         percProp_S = (areaProp_S/areaProps_total)*100)%>%
  dplyr::select(mun_cod,
         percProp_S,
         percProp_M,
         percProp_L)%>%
  left_join(x = db2cap1_cbps, y=., by = c("code_muni" = "mun_cod"))%>%
  glimpse() ->db2cap1_cbps

pibAgro_mun<- read_xlsx("/home/lucas/Documentos/Doutorado/Dados/pib_mun.xlsx",  sheet = 3)
pibInd_mun<- read_xlsx("/home/lucas/Documentos/Doutorado/Dados/pib_mun.xlsx",  sheet = 4)
pibserv_mun<- read_xlsx("/home/lucas/Documentos/Doutorado/Dados/pib_mun.xlsx",  sheet = 6)
readODS::read_ods("/home/lucas/Documentos/Doutorado/Dados/pop-total.ods")-> poptotal


pibAgro_mun%>%
  left_join(y=pibInd_mun)%>%
  left_join(y=pibserv_mun)%>%
  filter(code_muni != 1)%>%
  dplyr::select(-nome_mun, -name_muni)%>%
  mutate(across(.fns = as.double))%>%
  left_join(y = dplyr::select(poptotal,
                       code_muni,
                       popTotal_10))%>%
  mutate(pibAgroPC_2010 = pibAgro_2010*1000/popTotal_10,
         pibIndPC_2010 = pibInd_2010*1000/popTotal_10,
         pibServpubPC_2010 = pibServpub_2010*1000/popTotal_10,
        )%>%
  glimpse()->pibs_mun

pibs_mun%>%
  dplyr::select(code_muni,
          pibAgroPC_2010,
          pibIndPC_2010,
          pibServpubPC_2010
          )%>%
  left_join(x = db2cap1_cbps, y=.)%>%
  glimpse()-> db2cap1_cbps

read_xlsx("/home/lucas/Documentos/Doutorado/Dados/pevs.xlsx")->pevs
pevs%>%
  dplyr::select(code_muni,
         `7.1 - Carvão vegetal (Toneladas)_2010`,
         `7.2 - Lenha (Metros cúbicos)_2010`,
         `7.3 - Madeira em tora (Metros cúbicos)_2010`
         )%>%
  rename(carvVeg_10 = `7.1 - Carvão vegetal (Toneladas)_2010`,
         lenha_10 = `7.2 - Lenha (Metros cúbicos)_2010`,
         wood_10 = `7.3 - Madeira em tora (Metros cúbicos)_2010`)%>%
  mutate(code_muni = as.double(code_muni))%>%
  left_join(x = db2cap1_cbps, y=.)%>%
  glimpse()->db2cap1_cbps

db2cap1_cbps%>%
  na.omit()%>%
  rename(u5mort_2010 = u5mort_2010.x)%>%
  glimpse()->db2cap1_cbps_clean

sum(is.na(db2cap1_cbps_clean$nvcPerc_2010))

write_xlsx(x = db2cap1_cbps_clean, path = "/home/lucas/Documentos/Doutorado/tese/cap1/db2cap1_cbps_clean.xlsx")

read_xlsx("/home/lucas/Documentos/Doutorado/tese/cap1/db2cap1_cbps_clean.xlsx")->p
p%>%
dplyr::select(code_muni,
       CODIBGE_short,
       code_state,
       IDHM_E_2010,
       IDHM_L_2010,
       IDHM_R_2010,
       expov_2010,
       gini_2010,
       u5mort_2010,
       nvcHa_2010,
       nvcPerc_2010,
       area_mun,
       p_agro_10,
       perc_rur_10,
       perc_urb_10,
       prodAss_06,
       nascProt_06,
       riosProt_06,
       irrigPerc_06,
       rain_var,
       percProp_S,
       percProp_M,
       percProp_L,
       pibAgroPC_2010,
       pibIndPC_2010,
       pibServpubPC_2010,
       carvVeg_10,
       lenha_10,
       wood_10,
      )%>%
  glimpse()->p

  write_xlsx(x = p, path = "/home/lucas/Documentos/Doutorado/tese/cap1/db2cap1_cbps_clean.xlsx")

#adição de variáveis de distância####
read.csv("/home/lucas/Documentos/Doutorado/Dados/sede-dist-100mil.csv", sep = ";", dec = ",")-> sede.dist.100
sede.dist.100%>%
  select(CD_MUN.C.7,
         HubDist.N.24.15)%>%
  rename(code_muni = CD_MUN.C.7,
         dist_100 = HubDist.N.24.15)%>%
  glimpse()-> sede.dist.100

read.csv("/home/lucas/Documentos/Doutorado/Dados/sede-dist-capital.csv", sep = ";", dec = ",")-> sede.dist.cap
sede.dist.cap%>%
  select(CD_MUN.C.7,
         HubDist.N.24.15)%>%
  rename(code_muni = CD_MUN.C.7,
         dist_cap = HubDist.N.24.15)%>%
  glimpse()-> sede.dist.cap

read_xlsx("/home/lucas/Documentos/Doutorado/tese/cap1/db2cap1_cbps_clean.xlsx")%>%
  left_join(x = ., y = sede.dist.100)%>%
  left_join(y = sede.dist.cap)%>%
  glimpse()%>%
  write_xlsx(x = ., path = "/home/lucas/Documentos/Doutorado/tese/cap1/db2cap1_cbps_clean.xlsx")

#database for chapter 1 with all variables####
read_xlsx(path = "/home/lucas/Documentos/Doutorado/tese/cap1/forest-develop/dbcap1_clean.xlsx")->data2
#as.numeric(data$code_muni)->data$code_muni
data2%>%
  select(code_muni,
         txDesemp_10,
         finanPerc_06,
         bovMed_06,
         capMed_06,
         cornMed_06,
         beanMed_06,
         popDens_10,
         MeanSlopefor2004analysis,
         MeanElevationfor2004analysis,
         Sumkm2_ProtectedArea2013)%>%
  left_join(x=p, y=.)%>%
  glimpse()%>%
  write_xlsx(x = ., path = "/home/lucas/Documentos/Doutorado/tese/cap1/dbcap1_full_clean.xlsx")
