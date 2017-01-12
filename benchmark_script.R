library(RODBC)
library(tidyverse)
library(codemogAPI)
library(codemog)
source("J:/Estimates/ConstructionData/LocalUpdateData/v2015ResidentialConstructionSurvey/v2015MasterUpdates/pw.r") # creates a connection to Oracle using a document not under version control
#### Input Tables ####

# OIS.PLACE provides a LGID to PLACEFIPS crosswalk
oisplace=sqlFetch(dolaprod, "OIS.PLACE")

# DLG.LIM_LEVY provides assessed values
lim_levy=sqlFetch(dolaprod, "DLG.LIM_LEVY")

# DLG.LGBASIC provides basic local government data
lgbasic=sqlFetch(dolaprod, "DLG.LGBASIC")

# Total Housing Units
munis=as.numeric(c("760", "925", "1090", "1530", "2355", "3235", "3455", "3620", "3950", "4000", "4110", "4935", "5265", "6090", "6255", "6530", "7025", "7190", "7410", "7571", "7795", "7850", "8070", "8345", "8400", "8675", "9115", "9280", "9555", "10105", "10600", "11260", "11645", "11810", "12045", "12387", "12415", "12635", "12815", "12855", "12910", "13460", "13845", "14175", "14765", "15330", "15550", "15605", "16000", "16385", "16495", "17375", "17760", "17925", "18310", "18420", "18530", "18640", "18750", "19080", "19355", "19630", "19795", "19850", "20000", "20440", "20495", "20770", "21265", "22035", "22145", "22200", "22860", "23025", "23135", "23740", "24620", "24785", "24950", "25115", "25280", "25610", "26270", "26600", "26765", "26875", "27040", "27425", "27700", "27810", "27865", "27975", "28105", "28305", "28360", "28690", "28745", "29185", "29680", "29735", "29955", "30340", "30780", "30835", "31550", "31605", "31660", "31715", "32155", "32650", "33035", "33310", "33640", "33695", "34520", "34740", "34960", "35070", "36610", "37215", "37270", "37380", "37545", "37600", "37820", "37875", "38370", "38535", "38590", "39195", "39855", "39965", "40185", "40515", "40570", "40790", "41010", "41560", "41835", "42055", "42110", "42330", "42495", "43000", "43110", "43550", "43605", "43660", "44100", "44320", "44980", "45255", "45530", "45695", "45955", "45970", "46355", "46465", "47070", "48060", "48115", "48445", "48500", "48555", "49600", "49875", "50040", "50480", "50920", "51250", "51635", "51690", "51745", "51800", "52075", "52350", "52550", "52570", "53120", "53175", "53395", "54330", "54880", "54935", "55045", "55155", "55540", "55705", "55870", "55980", "56145", "56365", "56420", "56475", "56860", "56970", "57025", "57245", "57300", "57400", "57630", "58235", "59005", "59830", "60160", "60600", "61315", "62000", "62660", "62880", "63045", "63265", "64090", "64200", "64255", "64970", "65190", "65740", "66895", "67005", "67280", "67830", "68105", "68655", "68930", "69040", "69150", "69645", "69700", "70195", "70250", "70360", "70525", "70580", "70635", "71755", "72395", "73330", "73715", "73825", "73935", "74485", "74815", "75640", "75970", "76795", "77290", "77510", "78610", "79270", "80040", "80865", "81030", "81690", "82130", "82350", "82460", "82735", "83230", "83450", "83835", "84440", "84770", "85045", "85155", "85485", "85705", "86090", "86310", "86475", "86750", "99990"))
housingunits=muni_est(munis, 2010:2015, vars=c("totalhousingunits", "totalpopulation"))%>%
  select(-totalpopulation)%>%
  inner_join(oisplace, by= c("placefips"="PLACE"))
carbonate=data.frame(municipality="Carbonate", year=c(2010:2015), placefips=NA,totalhousingunits="0",
                     FIPS_CLASS_CODE=NA, PLACECE=NA, LG_ID=66636, DOR_CODE=NA, CREATED_ON=NA,
                     CREATED_BY=NA, UPDATED_ON=NA, UPDATED_BY=NA, FEATURE_ID=NA)
housingunits=bind_rows(housingunits, carbonate)

# DLG.CM_AUDIT
cm_audit=sqlFetch(dolaprod, "DLG.CM_AUDIT")%>%
  select(LG_ID, CM_AUDIT_ID, AUDIT_YEAR)

# DLG.CM_FUND
cm_fund=sqlFetch(dolaprod, "DLG.CM_FUND")

#### S3 Assessed Value / Household ####

av=lgbasic%>%
  select(-CREATED_BY:-OSA)%>%
  inner_join(select(lim_levy, -CREATED_BY:-UPDATED_BY), by="LG_ID")%>%
  filter(SUBDIST_NUM==1 | SUBDIST_NUM==2, LGTYPE_ID%in%c(2,3,4,5, 61, 70), BUDGET_YEAR>=2012 & BUDGET_YEAR<=2016)%>%
  select(LG_ID, NAME, BUDGET_YEAR, ASSESSED_VALUE)%>%
  group_by(LG_ID,NAME)%>%
  summarize(ASSESSED_VALUE=mean(ASSESSED_VALUE))

hu3=housingunits%>%
  filter(year>2010)%>%
  select(LG_ID, year, totalhousingunits)%>%
  group_by(LG_ID)%>%
  summarize(housingunits=mean(as.numeric(totalhousingunits)))

#Generates Full Dataset

s3_data=inner_join(av, hu3, by="LG_ID")%>%
  mutate(s3metric=ASSESSED_VALUE/housingunits)

#Generates Benchmark

s3_benchmark=data.frame(s3_benchmark=median(s3_data$s3metric))

#### S4 Current and Projected System Debt/Tap/MHV
## Need to redo this to be run for S+W S and W.
## Need to sum S and W for those with separate before creating W+s Median
## Could do this by spreading the values and using LG-ID as the key
## Separate MEdians for all sets

s4debt_ws=lgbasic%>%
  select(-CREATED_BY:-OSA)%>%
  inner_join(select(cm_audit, -UPDATED_ON:-UPDATED_BY), by="LG_ID")%>%
  inner_join(select(cm_fund, -CREATED_ON:-UPDATED_BY), by="CM_AUDIT_ID")%>%
  filter(CM_FUND_TYPE_ID%in%c(1,2,3), LGTYPE_ID%in%c(2,3,4,5), LG_ID!=16002, LG_ID!=64030, AUDIT_YEAR>=2010, AUDIT_YEAR<=2014)%>%
  select(LG_ID, NAME, CM_FUND_TYPE_ID, AUDIT_YEAR, GO_DEBT,REVENUE_DEBT,OTHER_DEBT)%>%
  mutate(debt=GO_DEBT+REVENUE_DEBT+OTHER_DEBT)%>%
  group_by(LG_ID, NAME, AUDIT_YEAR)%>%
  summarize(debt=sum(debt))%>%
  ungroup()%>%
  group_by(LG_ID, NAME)%>%
  summarize(debt=mean(debt))

s4debt_sep=lgbasic%>%
  select(-CREATED_BY:-OSA)%>%
  inner_join(select(cm_audit, -UPDATED_ON:-UPDATED_BY), by="LG_ID")%>%
  inner_join(select(cm_fund, -CREATED_ON:-UPDATED_BY), by="CM_AUDIT_ID")%>%
  filter(CM_FUND_TYPE_ID%in%c(2,3), LGTYPE_ID%in%c(2,3,4,5), LG_ID!=16002, LG_ID!=64030, AUDIT_YEAR>=2010, AUDIT_YEAR<=2014)%>%
  select(LG_ID, NAME, CM_FUND_TYPE_ID, AUDIT_YEAR, GO_DEBT,REVENUE_DEBT,OTHER_DEBT)%>%
  mutate(debt=GO_DEBT+REVENUE_DEBT+OTHER_DEBT)%>%
  group_by(LG_ID, NAME, CM_FUND_TYPE_ID)%>%
  summarize(debt=sum(debt))%>%
  select(LG_ID, NAME, CM_FUND_TYPE_ID, debt)

## WHY AM I GETTING ONLY 234 - There are some without either
##LIST OF PLACE NOT IN THERE
# y=anti_join(s3_data, s4debt, by="LG_ID")

hu4=housingunits%>%
  filter(year>=2010, year<=2014)%>%
  select(LG_ID, year, totalhousingunits)%>%
  group_by(LG_ID)%>%
  summarize(housingunits=mean(as.numeric(totalhousingunits)))

mhv4=codemog_api(data="b25077", db="acs1115", sumlev = "160", geography = "sumlev")%>%
  mutate(place=as.numeric(place), 
         mhv=as.numeric(b25077001))%>%
  left_join(oisplace, c("place"= "PLACE"))

s4_data_ws=hu4%>%
  inner_join(mhv4, by="LG_ID")%>%
  filter(!is.na(LG_ID))%>%
  inner_join(s4debt_ws, by="LG_ID")%>%
  mutate(s4metric=debt/housingunits/mhv,
         s4metric=ifelse(is.na(s4metric), 0,s4metric))

s4_data_sep=hu4%>%
  inner_join(mhv4, by="LG_ID")%>%
  filter(!is.na(LG_ID))%>%
  inner_join(s4debt_sep, by="LG_ID")%>%
  mutate(s4metric=debt/housingunits/mhv,
         s4metric=ifelse(is.na(s4metric), 0,s4metric))

s4_benchmark_ws=data.frame(s4_benchmark_ws=median(s4_data_ws$s4metric))
s4_benchmark_sep=s4_data_sep%>%
  mutate(fundID=ifelse(CM_FUND_TYPE_ID==2, "w", "s"),
         name=paste0("s4_benchmark_", fundID))%>%
  group_by(name)%>%
  summarize(s4_benchmark_sep=median(s4metric))%>%
  spread(name, s4_benchmark_sep)


#### S5a System Full-Cost/Tap/MHI ####

s5acost_ws=lgbasic%>%
  select(-CREATED_BY:-OSA)%>%
  inner_join(cm_audit, by="LG_ID")%>%
  inner_join(select(cm_fund, -CREATED_ON:-UPDATED_BY), by="CM_AUDIT_ID")%>%
  filter(CM_FUND_TYPE_ID%in%c(1,2,3), LGTYPE_ID%in%c(2,3,4,5), LG_ID!=16002, LG_ID!=64030, AUDIT_YEAR>=2010, AUDIT_YEAR<=2014)%>%
  mutate(cost=EXP_OPERATING+EXP_TRANSFER_OUT+DEPRECIATION)%>%
  group_by(LG_ID, NAME, AUDIT_YEAR)%>%
  summarize(cost=mean(cost))%>%
  ungroup()%>%
  group_by(LG_ID, NAME)%>%
  summarize(cost=mean(cost))

s5acost_sep=lgbasic%>%
  select(-CREATED_BY:-OSA)%>%
  inner_join(cm_audit, by="LG_ID")%>%
  inner_join(select(cm_fund, -CREATED_ON:-UPDATED_BY), by="CM_AUDIT_ID")%>%
  filter(CM_FUND_TYPE_ID%in%c(2,3), LGTYPE_ID%in%c(2,3,4,5), LG_ID!=16002, LG_ID!=64030, AUDIT_YEAR>=2010, AUDIT_YEAR<=2014)%>%
  select(LG_ID, NAME, CM_FUND_TYPE_ID, AUDIT_YEAR, EXP_OPERATING,EXP_TRANSFER_OUT,DEPRECIATION)%>%
  mutate(cost=EXP_OPERATING+EXP_TRANSFER_OUT+DEPRECIATION)%>%
  group_by(LG_ID, NAME, CM_FUND_TYPE_ID)%>%
  summarize(cost=sum(cost))%>%
  select(LG_ID, NAME, CM_FUND_TYPE_ID, cost)


hu5=housingunits%>%
  filter(year>=2010, year<=2014)%>%
  select(LG_ID, year, totalhousingunits)%>%
  group_by(LG_ID)%>%
  summarize(housingunits=mean(as.numeric(totalhousingunits)))

mhi5=codemog_api(data="b19013", db="acs1115", sumlev = "160", geography = "sumlev")%>%
  mutate(place=as.numeric(place), 
         mhi=as.numeric(b19013001))%>%
  left_join(oisplace, c("place"= "PLACE"))%>%
  filter(!is.na(LG_ID))

s5a_data_ws=hu5%>%
  inner_join(mhi5, by="LG_ID")%>%
  filter(!is.na(LG_ID))%>%
  inner_join(s5acost_ws, by="LG_ID")%>%
  mutate(s5metric=cost/housingunits/mhi,
         s5metric=ifelse(is.na(s5metric), 0,s5metric))

s5a_data_sep=hu5%>%
  inner_join(mhi5, by="LG_ID")%>%
  filter(!is.na(LG_ID))%>%
  inner_join(s5acost_sep, by="LG_ID")%>%
  mutate(s5metric=cost/housingunits/mhi,
         s5metric=ifelse(is.na(s5metric), 0,s5metric))

s5a_benchmark_ws=data.frame(s5a_benchmark_ws=median(s5a_data_ws$s5metric))
s5a_benchmark_sep=s5a_data_sep%>%
  mutate(fundID=ifelse(CM_FUND_TYPE_ID==2, "w", "s"),
    name=paste0("s5a_benchmark_", fundID))%>%
  group_by(name)%>%
  summarize(s5a_benchmark_sep=median(s5metric))%>%
  spread(name, s5a_benchmark_sep)


#### S5b System Required Revenue/Tap/MHI (@110% Coverage) ####

s5brev_sep=lgbasic%>%
  select(-CREATED_BY:-OSA)%>%
  inner_join(cm_audit, by="LG_ID")%>%
  inner_join(select(cm_fund, -CREATED_ON:-UPDATED_BY), by="CM_AUDIT_ID")%>%
  filter(CM_FUND_TYPE_ID%in%c(2,3), LGTYPE_ID%in%c(2,3,4,5), LG_ID!=16002, LG_ID!=64030, AUDIT_YEAR>=2010, AUDIT_YEAR<=2014)%>%
  


s5b_data_ws=hu5%>%
  inner_join(mhi5, by="LG_ID")%>%
  filter(!is.na(LG_ID))%>%
  inner_join(s5brev_ws, by="LG_ID")%>%
  mutate(s5metric=cost/housingunits/mhi,
         s5metric=ifelse(is.na(s5metric), 0,s5metric))

s5b_data_sep=hu5%>%
  inner_join(mhi5, by="LG_ID")%>%
  filter(!is.na(LG_ID))%>%
  inner_join(s5brev_sep, by="LG_ID")%>%
  mutate(s5metric=cost/housingunits/mhi,
         s5metric=ifelse(is.na(s5metric), 0,s5metric))

s5b_benchmark_ws=data.frame(s5b_benchmark_ws=median(s5b_data_ws$s5metric))
s5b_benchmark_sep=s5b_data_sep%>%
  mutate(fundID=ifelse(CM_FUND_TYPE_ID==2, "w", "s"),
         name=paste0("s5b_benchmark_", fundID))%>%
  group_by(name)%>%
  summarize(s5b_benchmark_sep=median(s5metric))%>%
  spread(name, s5b_benchmark_sep)



