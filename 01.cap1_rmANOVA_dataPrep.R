# Tue May 25 14:56:59 2021 ------------------------------
#data preparation for rmANOVA - Chapter one

#Library ####
library(dplyr)
library(readxl)
library(tidyr)
library(foreign)
library(writexl)

#data ####
read_xlsx("/home/lucas/Documentos/Doutorado/Dados/mapb5_cobertura_selec.xlsx")->mapbiomas5_cobertura
read_xlsx("/home/lucas/Documentos/Doutorado/tese/dbtese_17.xlsx")->dbtese_17
read_xlsx("/home/lucas/Documentos/Doutorado/Dados/atlas2013_dadosbrutos_pt.xlsx", sheet = 2)->atlas_br
read_xlsx(path = "/home/lucas/Documentos/Doutorado/Dados/mapb5_transicao_selec.xlsx")-> desmat_anual
read.dbf("/home/lucas/Documentos/Doutorado/Dados/shapes/BR_Municipios_2020/BR_Municipios_2020.dbf")-> muns_br
  
mapbiomas5_cobertura%>%
  select(territory_id, `1991`, `2000`, `2010`)%>%
  group_by(territory_id)%>%
  summarise(nvcHa_1991 = sum(`1991`),
            nvcHa_2000 = sum(`2000`),
            nvcHa_2010 = sum(`2010`)
            )%>%
  glimpse-> nvcHa

dbtese_17%>%
  select(code_muni, area_mun, gini_1991, gini_2000, gini_2010, u5mort_1991,
         u5mort_2000, U5mort_2010, expov_1991, expov_2000, expov_2010
        )%>%
  left_join(y= nvcHa, by = c("code_muni" = "territory_id")
            )%>%
  glimpse->dbcap1_rma

atlas_br%>%
  select(Codmun7, i, IDHM, IDHM_E, IDHM_L, IDHM_R)%>%
  pivot_wider(
    id_cols = Codmun7,
    names_from = i,
    values_from = c(IDHM, IDHM_E, IDHM_L, IDHM_R),
    names_sep = "_"
              )%>%
  left_join(x = dbcap1_rma, y= ., by = c("code_muni"="Codmun7")
            )%>%
    glimpse-> dbcap1_rma

desmat_anual%>%
  group_by(territory_id)%>%
  summarize(desmat_85 = sum(`1985 to 1986`),
            desmat_86 = sum(`1986 to 1987`),
            desmat_87 = sum(`1987 to 1988`),
            desmat_88 = sum(`1988 to 1989`),
            desmat_89 = sum(`1989 to 1990`),
            desmat_90 = sum(`1990 to 1991`),
            desmat_91 = sum(`1991 to 1992`),
            desmat_92 = sum(`1992 to 1993`),
            desmat_93 = sum(`1993 to 1994`),
            desmat_94 = sum(`1994 to 1995`),
            desmat_95 = sum(`1995 to 1996`),
            desmat_96 = sum(`1996 to 1997`),
            desmat_97 = sum(`1997 to 1998`),
            desmat_98 = sum(`1998 to 1999`),
            desmat_99 = sum(`1999 to 2000`),
            desmat_00 = sum(`2000 to 2001`),
            desmat_01 = sum(`2001 to 2002`),
            desmat_02 = sum(`2002 to 2003`),
            desmat_03 = sum(`2003 to 2004`),
            desmat_04 = sum(`2004 to 2005`),
            desmat_05 = sum(`2005 to 2006`),
            desmat_06 = sum(`2006 to 2007`),
            desmat_07 = sum(`2007 to 2008`),
            desmat_08 = sum(`2008 to 2009`),
            desmat_09 = sum(`2009 to 2010`),
            desmat_10 = sum(`2010 to 2011`),
  )%>%
  left_join(x = dbcap1_rma, y=., by = c("code_muni" = "territory_id"))%>%
  glimpse ->dbcap1_rma_full

dbcap1_rma_full%>%
  mutate(nvc.perc_91 = nvcHa_1991*100/area_mun,
         nvc.perc_00 = nvcHa_2000*100/area_mun, 
         nvc.perc_10 = nvcHa_2010*100/area_mun,
         tx.desmat.perc_91 = (((desmat_87 + desmat_88 + desmat_89 + desmat_90 + desmat_91)/5)*100)/area_mun,
         tx.desmat.perc_00 = (((desmat_96 + desmat_97 + desmat_98 + desmat_99 + desmat_00)/5)*100)/area_mun,
         tx.desmat.perc_10 = (((desmat_06 + desmat_07 + desmat_08 + desmat_09 + desmat_10)/5)*100)/area_mun,
         )%>%
  filter(!is.na(nvc.perc_91),
         nvc.perc_00 < 100)%>%
  glimpse -> dbcap1_rma

write_xlsx(x = dbcap1_rma, "/home/lucas/Documentos/Doutorado/tese/cap1/dbcap1_rma.xlsx")

#data exploration####
summary(dbcap1_rma[,23:28])
which(dbcap1_rma[,23:25] > 100)

