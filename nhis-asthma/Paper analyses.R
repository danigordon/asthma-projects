library(survey)
library(car)

setwd("C:/Users/Danielle/Google Drive/Dem coursework/Mortality (JSparks)/")

#Load data file for person-level
#nhis12<-load("C:/Users/Danielle/Google Drive/Dem coursework/Mortality (JSparks)/NHIS data/2012/personsx.rda")
#108,131
nhis12p<-read.csv("C:/Users/Danielle/Google Drive/Dem coursework/Mortality (JSparks)/NHIS data/2012/personsx.csv", na.strings="")
nhis12p<-nhis12p[order(nhis12p$srvy_yr, nhis12p$hhx, nhis12p$fmx, nhis12p$fpx),]


nhis12h<-read.csv("C:/Users/Danielle/Google Drive/Dem coursework/Mortality (JSparks)/NHIS data/2012/househld.csv", na.strings="")
#nhis12h<-nhis12h[order(nhis12h$srvy_yr, nhis12h$hhx),]
attach(nhis12h)
#Variable recodes
psu<-psu_p
strat<-strat_p
pwt<-wtfa
survyr<-srvy_yr
detach(nhis12h)

nhis12f<-read.csv("C:/Users/Danielle/Google Drive/Dem coursework/Mortality (JSparks)/NHIS data/2012/familyxx.csv", na.strings="")

attach(nhis12f)
#SES measures
#Missing 3888
faminc1<-ifelse(incgrp2==1, 1,
                ifelse(incgrp2==2, 2,
                       ifelse(incgrp2==3, 3,
                              ifelse(incgrp2==4, 4, 
                                     ifelse(incgrp2==5, 5, NA)))))
#Missing 3888
faminc2<-ifelse(incgrp3==1, 1,
                ifelse(incgrp3==2, 2,
                       ifelse(incgrp3==3, 3,
                              ifelse(incgrp3==4, 4, NA))))
famsize<-fm_size
numkids<-fm_kids

#Different on 41-43 only
table(fm_strcp)
table(fm_strp)

#Remake this to capture everyone
famstr.sin<-ifelse(fm_type==3, 1, ifelse(fm_type==4, 0, 99))

#famstr<-ifelse(FM_STRCP %in% c('31', '32', '33'), 1, 
#               ifelse(FM_STRCP %in% c('41', '42', '43')))

famedu.nohs<-ifelse(fm_educ1 %in% c(1,2), 1, ifelse(fm_educ1 %in% c(97,98,99), NA, 0))
famedu.hs<-ifelse(fm_educ1 %in% c(3,4), 1, ifelse(fm_educ1 %in% c(97,98,99), NA, 0))
famedu.somcol<-ifelse(fm_educ1 %in% c(8,9), 1, ifelse(fm_educ1 %in% c(97,98,99), NA, 0))
famedu.deg<-ifelse(fm_educ1 %in% c(8,9), 1, ifelse(fm_educ1 %in% c(97,98,99), NA, 0))
#Tenure status
rent<-ifelse(houseown==2, 1, ifelse(houseown==1, 0, NA))

detach(nhis12f)

nhis12f<-subset(nhis12f, c(famedu.nohs, famedu.hs, famedu.somcol, famedu.deg,
                           famstr.sin, fm_strcp, fm_strp, famsize, numkids,
                           faminc2, faminc1, rent))

#Create fam identifier in person file
nhis12p$fident<-paste0(nhis12p$srvy_yr, nhis12p$hhx, nhis12p$fmx)
#Create fam identifier in fam file
nhis12f$fident<-paste0(nhis12f$srvy_yr, nhis12f$hhx, nhis12f$fmx)
#Merge pers & fam files
nhis12fp<-merge(nhis12f, nhis12p, by="fident")

#Create HH identifer in fam/person file
nhis12fp$hident<-paste0(nhis12fp$srvy_yr.y, nhis12fp$hhx.y)
#Create HH identifier in HH file
nhis12h$hident<-paste0(nhis12h$srvy_yr, nhis12h$hhx)
nhis12hfp<-merge(nhis12h, nhis12fp, by="hident")

#Create pers identifier in total file
#nhis12hfp$pident<-paste0(srvy_yr, hhx, fmx, fpx)

#28,016
nhis12.sub<-subset(nhis12hfp, age_p < 18) 




noins<-ifelse(HINOTYR=='01', 1, ifelse(HINOTYR=='02', 0, NA))

#Child asthma or breathing problems limitations
asthlim<-ifelse(LAHCC4=='01', 1, ifelse(LAHCC4=='02', 0, NA))
asthchron<-ifelse(LCCHRC4=='01', 1, ifelse(LCCHRC4=='02', 0, NA))

asthdur<-ifelse(lcdura4>18, NA, lcdura4)
table(asthdur)

asthage<-ifelse(asthlim==1, age-asthdur, 99)
table(asthage)
asth12<-ifelse(asthage<2, 1, 99)
table(asth12)

#Incidence of asthma in past year
asthinc<-ifelse(asthlim==1 & asth12==1, 1, ifelse(asthlim==0, 0, NA))
table(asthinc)

#Demographics
male<-ifelse(SEX=='01', 1, 0)
cage<-ifelse(AGE_P %in% c('00', '01'))
hisp<-ifelse(HISCODI3=='01', 1, 0)
white<-ifelse(HISCODI3=='02', 1, 0)
black<-ifelse(HISCODI3=='03', 1, 0)
asian<-ifelse(HISCODI3=='04', 1, 0)
other<-ifelse(HISCODI3=='05', 1, 0)
other2<-ifelse(HISCODI3 %in% c('04', '05'), 1, 0)

#
nhis12.sub<-na.omit(subset(nhis12.sub, select=c()))



#rm(dat1)

options(survey.adjust.domain.lonely=T, survey.lonely.psu="remove")

nhis12.design<-svydesign(ids=~psu, strata=~strat, weights=~awt, nest=T, data=sdat1) #Used nested b/c docs said psu only unique within stratum
summary(brfss.design)

###################FULL SAMPLE#############################

#Controls
#(male + hisp + black + other2 + noins)

#Null model
fit.0<-svyglm(asthlim~1, family=binomial, design=nhis.design)
summary(fit.0)

fit.00<-svyglm(asthlim~cage + ???, family=binomial, design=nhis.design)
summary(fit.00)
exp(coef(fit.00))

#Model with SES predicting asthma, controls
fit.1<-svyglm(asthlim~??? + (), family=binomial, design=nhis.design) 
summary(fit.1) 
exp(coef(fit.1))

#Model adding age
fit.2<-svyglm(asthlim~??? + cage + (), 
               family=binomial, design=nhis.design) 
summary(fit.2) 
exp(coef(fit.2))

#Model adding SES x age interaction
fit.3<-svyglm(asthlim~??? + cage + (??? * cage) + (), 
              family=binomial, design=nhis.design)

