# Mon Mar  1 14:59:22 2021 ------------------------------
#Propensity scores analysis
#Analysis made following Walter Leite's tutorial at http://www.practicalpropensityscore.com/continuous.html
#and Christian Fong, Chad Hazlett, and Kosuke Imai, 2018 - .

setwd("/home/lucas/Documents/Doutorado/tese/cap1/")

#Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(CBPS)
library(ggplot2)

read_xlsx("/home/lucas/Documents/Doutorado/tese/cap1/forest-develop/dbcap1_clean.xlsx")-> dbcap1_PS
read_xlsx("")

##Standardize continuous covariates####
as.data.frame(scale(dbcap1_PS [,18:36]))-> dbcap1_PS [,18:36]
#Define covariate names####
paste0(names(dbcap1_PS [,18:36]),"_scaled")-> covariateNames
names(dbcap1_PS)[18:36] <- covariateNames
glimpse(dbcap1_PS)
na.omit(dbcap1_PS)-> dbcap1_PS
## treatment normalization####
#Find the Box-Cox transformation of the treatment
# that makes its distribution closest to normal
#max.norm.cor<-cor(qqnorm(dbcap1_PS$nvcPerc_10)$x,qqnorm(dbcap1_PS$nvcPerc_10)$y)
#best.bc<-dbcap1_PS$nvcPerc_10
#best.lambda<-1
#for (lambda in seq(-2,2,0.01)){
#  if (lambda != 0)
#  {
#    bc.nvcPerc_10<-((dbcap1_PS$nvcPerc_10+1)^lambda - 1)/lambda
#  }
#  else
#  {
#    bc.nvcPerc_10<-log(dbcap1_PS$nvcPerc_10+1)
#  }
#  norm.cor<-cor(qqnorm(bc.nvcPerc_10)$x,qqnorm(bc.nvcPerc_10)$y)
#  if (norm.cor > max.norm.cor)
#  {
#    max.norm.cor<-norm.cor
#    best.bc<-bc.nvcPerc_10
#    best.lambda<-lambda
#  }
#}

#dbcap1_PS$nvcPerc_10_norm<-best.bc



#data exploration####
#hist(dbcap1_PS$nvcPerc_10)
#hist(dbcap1_PS$nvcPerc_10_norm)
#GGally:::ggpairs(dbcap1_PS [,18:36])

#data analysis####
##Propensity scores for 2010 model
### Create CBPS ####
modelnvc<- CBPS(data = dbcap1_PS,
              nvcPerc_10 ~      
                txDesemp_10_scaled +  
                finanPerc_17_scaled +
                bovMed_17_scaled +
                capMed_17_scaled +
                beanMed_17_scaled +
                cornMed_17_scaled +
                popDens_10_scaled +
                MeanSlopefor2004analysis_scaled +                   
                MeanElevationfor2004analysis_scaled +              
                Sumkm2_ProtectedArea2013_scaled,
              method = "exact",
              )
summary(modelnvc)

### Evaluate CBPS ####
bal.covar<- balance(modelnvc)
bal.covar<- data.frame(original=bal.covar$unweighted, 
                                weighted=bal.covar$balanced)
names(bal.covar)<- c("Pearson r unweighted", "Pearson r IPW")
summary(bal.covar)
boxplot(bal.covar)

### Create npCBPS 
modelNPnvc<- npCBPS(data = dbcap1_PS,
                nvcPerc_10 ~      
                  txDesemp_10_scaled +  
                  finanPerc_17_scaled +
                  bovMed_17_scaled +
                  capMed_17_scaled +
                  beanMed_17_scaled +
                  cornMed_17_scaled +
                  popDens_10_scaled +
                  MeanSlopefor2004analysis_scaled +                   
                  MeanElevationfor2004analysis_scaled +              
                  Sumkm2_ProtectedArea2013_scaled,
                method = "exact",
                standardize = T,
                ATT = 0)
summary(modelNPnvc)

### Evaluate npCBPS
balNP.covar<- balance(modelNPnvc)
balNP.covar<- data.frame(original=balNP.covar$unweighted, 
                       weighted=balNP.covar$balanced)
names(balNP.covar)<- c("Pearson r baseline", "npCBPS")
summary(balNP.covar)

cbind(bal.covar, balNP.covar$npCBPS)->bal.check
names(bal.check)<- c("Unweighted", "CBPS", "npCBPS")
boxplot(bal.check, ylab= "Pearson correlations")

###Average Treatment Effect####
####Fong et al, 2018 method
model.expov10<- glm(data = dbcap1_PS, 
                    expov_2010 ~
                      nvcPerc_10 +
                      I(nvcPerc_10^2),
                    weights = modelnvc$weights
                    )
summary(model.expov10)


#outcome IDHM_R
model.idhmR_2010<- glm(data = dbcap1_PS, 
                    IDHM_R_2010 ~
                      nvcPerc_10 +
                      I(nvcPerc_10^2),
                    weights = modelnvc$weights
)
summary(model.idhmR_2010)


#outcome IDHM_L
model.idhmL_2010<- glm(data = dbcap1_PS, 
                        IDHM_L_2010 ~
                          nvcPerc_10 +
                          I(nvcPerc_10^2),
                        weights = modelnvc$weights
)
summary(model.idhmL_2010)


#outcome IDHM
model.idhm_2010<- glm(data = dbcap1_PS, 
                      IDHM_2010 ~
                        nvcPerc_10 +
                        I(nvcPerc_10^2),
                      weights = modelnvc$weights
)
summary(model.idhm_2010)


#outcome IDHM_E
model.idhmE_2010<- glm(data = dbcap1_PS, 
                       IDHM_E_2010 ~
                         nvcPerc_10 +
                         I(nvcPerc_10^2),
                       weights = modelnvc$weights
)
summary(model.idhmE_2010)


#outcome gini
model.gini_2010<- glm(data = dbcap1_PS, 
                      gini_2010 ~
                        nvcPerc_10 +
                        I(nvcPerc_10^2),
                      weights = modelnvc$weights
)
summary(model.gini_2010)


#outcome u5mort
model.u5mort_2010<- glm(data = dbcap1_PS, 
                        u5mort_2010.x ~
                          nvcPerc_10 +
                          I(nvcPerc_10^2),
                        weights = modelnvc$weights
)
summary(model.u5mort_2010)


coef(model.idhm_2010)
coef(model.idhmE_2010)
coef(model.idhmR_2010)
coef(model.idhmL_2010)
coef(model.gini_2010)
coef(model.expov10)
coef(model.u5mort_2010)

#### Non-parametric weightning
####Fong et al, 2018 method
model.expov10NP<- glm(data = dbcap1_PS, 
                    expov_2010 ~
                      nvcPerc_10 +
                      I(nvcPerc_10^2),
                    weights = modelNPnvc$weights
)
summary(model.expov10NP)

#outcome IDHM_R
model.idhmR_2010NP<- glm(data = dbcap1_PS, 
                       IDHM_R_2010 ~
                         nvcPerc_10 +
                         I(nvcPerc_10^2),
                       weights = modelNPnvc$weights
)
summary(model.idhmR_2010NP)


#outcome IDHM_L
model.idhmL_2010NP<- glm(data = dbcap1_PS, 
                       IDHM_L_2010 ~
                         nvcPerc_10 +
                         I(nvcPerc_10^2),
                       weights = modelNPnvc$weights
)
summary(model.idhmL_2010NP)


#outcome IDHM
model.idhm_2010NP<- glm(data = dbcap1_PS, 
                      IDHM_2010 ~
                        nvcPerc_10 +
                        I(nvcPerc_10^2),
                      weights = modelNPnvc$weights
)
summary(model.idhm_2010NP)


#outcome IDHM_E
model.idhmE_2010NP<- glm(data = dbcap1_PS, 
                       IDHM_E_2010 ~
                         nvcPerc_10 +
                         I(nvcPerc_10^2),
                       weights = modelNPnvc$weights
)
summary(model.idhmE_2010NP)


#outcome gini
model.gini_2010NP<- glm(data = dbcap1_PS, 
                      gini_2010 ~
                        nvcPerc_10 +
                        I(nvcPerc_10^2),
                      weights = modelNPnvc$weights
)
summary(model.gini_2010NP)


#outcome u5mort
model.u5mort_2010NP<- glm(data = dbcap1_PS, 
                        u5mort_2010.x ~
                          nvcPerc_10 +
                          I(nvcPerc_10^2),
                        weights = modelNPnvc$weights
)
summary(model.u5mort_2010NP)

coef(model.idhm_2010NP)
coef(model.idhmE_2010NP)
coef(model.idhmR_2010NP)
coef(model.idhmL_2010NP)
coef(model.gini_2010NP)
coef(model.expov10NP)
coef(model.u5mort_2010NP)

#Figures ####
#IDHM
ggplot(data = dbcap1_PS, aes(x = nvcPerc_10, y = IDHM_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("IDHM")->fig.IDHM_10

#IDHM_E
ggplot(data = dbcap1_PS, aes(x = nvcPerc_10, y = IDHM_E_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("IDHM_E")->fig.IDHM_E_10

#IDHM_L
ggplot(data = dbcap1_PS, aes(x = nvcPerc_10, y = IDHM_L_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("IDHM_L")->fig.IDHM_L_10

#IDHM_R
ggplot(data = dbcap1_PS, aes(x = nvcPerc_10, y = IDHM_R_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("IDHM_R")->fig.IDHM_R_10

#gini
ggplot(data = dbcap1_PS, aes(x = nvcPerc_10, y = gini_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("Gini Index")->fig.gini_10

#Extreme poverty
ggplot(data = dbcap1_PS, aes(x = nvcPerc_10, y = expov_2010))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("Extrem Poverty")->fig.expov_10

#u5mort
ggplot(data = dbcap1_PS, aes(x = nvcPerc_10, y = u5mort_2010.x))+
  geom_point(alpha  = 0.5)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), lwd = 0.5, fill = "grey20")+
  theme_classic()+
  xlab("NVC (%)")+
  ylab("Under five mortality")->fig.u5mort_10


