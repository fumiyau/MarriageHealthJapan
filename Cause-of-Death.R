#===============================================================================
# 2020/06/20
# Estat data extraction - cause of death
# Marriage and Health in Japan
# Fumiya Uchikoshi, uchikoshi@princeton.edu
#===============================================================================
library(estatapi)
#https://cran.r-project.org/web/packages/estatapi/README.html
library(tidyverse)
library(memisc)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(gdata)
#library(foreign)
#library(margins)
#library(gdata)
#library(ggeffects)
#library(lme4)
#library(ggsci)
#library(viridis)

######################################################################
# Change Working Directory / Set parameter
######################################################################
rm(list = ls())
setwd("/Users/fumiyau/Dropbox (Princeton)/10.MarriageHealthJapan/1.Git/MarriageHealthJapan") 
source("appid.R")
######################################################################
# Read data: Census mid year population
######################################################################

#2010
meta2010 <- estat_getMetaInfo(appId = appid_fu, statsDataId = "0003038593")
print(meta2010$cat03, n = nrow(meta2010$cat03) )

pop2010 <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003038593",
  cdArea = c("00000")) %>% #using cdArea is really important
  filter(cat01_code =="00710") %>% #全域 総数（外国人含む）
  filter(cat02_code =="001" | cat02_code == "002") %>% #男性、女性
  filter(cat04_code =="001" | cat04_code =="002" |cat04_code =="004" |cat04_code =="005" | cat04_code =="999") %>% #Never-married,Married,Widowed,Divorced,不詳 
  filter(cat05_code =="000" ) %>% #総数（外国人含む）
  filter(area_code =="00000"  ) %>% #総数（外国人含む）
  mutate(marital=case_when(
    .$cat04_code == "001" ~ "Never-married",
    .$cat04_code == "002" ~ "Married",
    .$cat04_code == "004" ~ "Widowed",
    .$cat04_code == "005" ~ "Divorced",
    .$cat04_code == "999" ~ "Unstated"),
    sex=case_when(
      .$cat02_code == "001" ~ "Male",
      .$cat02_code == "002" ~ "Female"
    ),
    age=case_when(
      .$cat03_code == "000" ~ "Total",
      .$cat03_code == "203" ~ "15",
      .$cat03_code == "204" ~ "20",
      .$cat03_code == "205" ~ "25",
      .$cat03_code == "206" ~ "30",
      .$cat03_code == "207" ~ "35",
      .$cat03_code == "208" ~ "40",
      .$cat03_code == "209" ~ "45",
      .$cat03_code == "210" ~ "50",
      .$cat03_code == "211" ~ "55",
      .$cat03_code == "212" ~ "60",
      .$cat03_code == "213" ~ "65",
      .$cat03_code == "214" ~ "70",
      .$cat03_code == "215" ~ "75",
      .$cat03_code == "216" ~ "80",
      .$cat03_code == "217" ~ "85",
      .$cat03_code == "218" ~ "90",
      .$cat03_code == "219" ~ "95",
      .$cat03_code == "600" ~ "100")) %>% 
  filter(age !="Total") %>% 
  mutate(age=as.numeric(paste(age))) %>% 
  dplyr::select(sex, marital, value,age) %>% 
  mutate(year=2010) %>% 
  na.omit()

######################################################################
# Read data: vital stats cause-of-death
# 15歳以上の死亡数，性・年齢(特定階級)・配偶関係・死因(選択死因分類)別
######################################################################
datalist <- estat_getStatsList(appId = appid_fu, searchWord = "人口動態調査 人口動態統計 確定数")

#2018
meta2018 <- estat_getMetaInfo(appId = appid_fu, statsDataId = "0003412001")

uni <- data.frame(cat03_code = meta2018$cat03[-1,]$`@code`, id = 1:length(meta2018$cat03[-1,]$`@code`))

death2018 <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003412001",
  cdCat02 = c("00110","00120"), # male and female
  cdCat01 = c("00110","00120","00130","00140","00150")) %>% #有配偶、未婚、死別、離別, others
  filter(cat03_code !="00100") %>% #omit total
  filter(cat04_code !="Se00") %>% 
  left_join(uni, by = "cat03_code") %>% 
  mutate(age=id*5 + 10, 
         marital=case_when(
           .$cat02_code == "00110" ~ "Married",
           .$cat02_code == "00120" ~ "Never-married",
           .$cat02_code == "00130" ~ "Widowed",
           .$cat02_code == "00140" ~ "Divorced",
           .$cat02_code == "00150" ~ "Unstated"),
         sex=case_when(
           .$cat02_code == "00110" ~ "Male",
           .$cat02_code == "00120" ~ "Female"),
         year=2018,
         death_code = substr(`選択死因分類`,1,4)) %>% 
  dplyr::select(age,marital,sex,death=value,death_code,death_lab=`選択死因分類`,year)

#2016, 2017
meta2017 <- estat_getMetaInfo(appId = appid_fu, statsDataId = "0003214980")
uni <- data.frame(cat03_code = meta2017$cat03[-1,]$`@code`, id = 1:length(meta2017$cat03[-1,]$`@code`))

death2016_2017 <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003214980",
  cdCat02 = c("00110","00120"), # male and female
  cdCat01 = c("00110","00120","00130","00140","00150")) %>% #有配偶、未婚、死別、離別, others
  filter(cat03_code !="00100") %>% #omit total
  filter(cat04_code !="Se00") %>% 
  left_join(uni, by = "cat03_code") %>% 
  mutate(age=id*5 + 10, 
         marital=case_when(
           .$cat02_code == "00110" ~ "Married",
           .$cat02_code == "00120" ~ "Never-married",
           .$cat02_code == "00130" ~ "Widowed",
           .$cat02_code == "00140" ~ "Divorced",
           .$cat02_code == "00150" ~ "Unstated"),
         sex=case_when(
           .$cat02_code == "00110" ~ "Male",
           .$cat02_code == "00120" ~ "Female"),
         year=as.numeric(substr(time_code,1,4)),
         death_code=substr(`選択死因分類`,1,4)) %>% 
  dplyr::select(age,marital,sex,death=value,death_code,death_lab=`選択死因分類`,year)

#2010-2015
death2010_2015 <- read_csv("0.Data/Cause-of-Death/2010.csv",skip=3) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2011.csv",skip=3)) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2012.csv",skip=3)) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2013.csv",skip=3)) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2014.csv",skip=3)) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2015.csv",skip=3)) %>% 
  mutate(death_lab = X1,
         year = sort(rep(2010:2015,nrow(read_csv("0.Data/Cause-of-Death/2015.csv",skip=3)))))
for (i in 1:length(unique(death2010_2015$X2))) {              
  death2010_2015 <- death2010_2015 %>%  mutate(death_lab=ifelse(is.na(death_lab), lag(death_lab), death_lab))               
}
death2010_2015 <- death2010_2015 %>% 
  filter(X2 != "総数") %>% 
  filter(death_lab !="　　死　亡　総　数　　　　　") %>% 
  filter(death_lab != "　　死　亡　総　数　　　　　　　　　　　　　　") %>% 
  mutate(age=as.numeric(paste(substr(X2,1,3))),
         death_code = substr(death_lab,1,4)) %>% 
  dplyr::select(-(1:8),-9,-15)

unique(death2010_2015$death_code)

death2010_2015_m <- death2010_2015 %>% 
  dplyr::select(-(6:10)) %>% 
  pivot_longer(
    cols = ends_with("_1"),
    names_to = "marital",
    values_to = "death",
    values_drop_na = TRUE
  ) %>% 
  mutate(sex="Male")

death2010_2015_f <- death2010_2015 %>% 
  dplyr::select(-(1:5)) %>% 
  pivot_longer(
    cols = ends_with("_2"),
    names_to = "marital",
    values_to = "death",
    values_drop_na = TRUE
  ) %>% 
  mutate(sex="Female")

death2010_2015 <- rbind(death2010_2015_m,death2010_2015_f) %>% 
  mutate(death=as.numeric(death),
         marital=substr(marital,1,3)) %>% 
  mutate(marital=case_when(
           .$marital == "有配偶" ~ "Married",
           .$marital == "未婚_" ~ "Never-married",
           .$marital == "死別_" ~ "Widowed",
           .$marital == "離別_" ~ "Divorced",
           .$marital == "不詳_" ~ "Unstated"))

#2009
meta2009 <- estat_getMetaInfo(appId = appid_fu, statsDataId = "0003030208")

death2009 <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003030208",
  cdCat01 = c("2","3"),
  cdCat02 = c("2","3","4","5","6")) %>% #有配偶、未婚、死別、離別, others
  filter(cat03_code !="1") %>% #omit total
  filter(cat04_code !="1") %>% #omit total
  mutate(age=as.numeric(paste(cat04_code))*5 + 5,
         marital=case_when(
           .$cat02_code == "2" ~ "Married",
           .$cat02_code == "3" ~ "Never-married",
           .$cat02_code == "4" ~ "Widowed",
           .$cat02_code == "5" ~ "Divorced",
           .$cat02_code == "6" ~ "Unstated"),
         sex=case_when(
           .$cat01_code == "2" ~ "Male",
           .$cat01_code == "3" ~ "Female"),
         year=2009,
         death_code = substr(`死因_011`,1,4)) %>% 
  dplyr::select(death_lab=`死因_011`,year,age,death_code,marital,death=value,sex)

#1999-2008
death1999_2008 <- read_csv("0.Data/Cause-of-Death/1999.csv",skip=3) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2000.csv",skip=3)) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2001.csv",skip=3)) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2002.csv",skip=3)) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2003.csv",skip=3)) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2004.csv",skip=3)) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2005.csv",skip=3)) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2006.csv",skip=3)) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2007.csv",skip=3)) %>% 
  rbind(read_csv("0.Data/Cause-of-Death/2008.csv",skip=3)) %>% 
  mutate(death_lab = X1,
         year = sort(rep(1999:2008,665)))
for (i in 1:length(unique(death1999_2008$X2))) {              
  death1999_2008 <- death1999_2008 %>%  mutate(death_lab=ifelse(is.na(death_lab), lag(death_lab), death_lab))               
}

death1999_2008 <- death1999_2008 %>% 
  filter(X2 != "総数") %>% 
  filter(death_lab !="死 亡 総 数") %>% 
  filter(death_lab != "　　死　亡　総　数　　　　　") %>% 
  mutate(age=as.numeric(paste(substr(X2,1,3))),
         death_code = substr(death_lab,1,4)) %>% 
  dplyr::select(-(1:8),-9,-15)

unique(death1999_2008$death_lab)

death1999_2008_m <- death1999_2008 %>% 
  dplyr::select(-(6:10)) %>% 
  pivot_longer(
    cols = ends_with("_1"),
    names_to = "marital",
    values_to = "death",
    values_drop_na = TRUE
  ) %>% 
  mutate(sex="Male")

death1999_2008_f <- death1999_2008 %>% 
  dplyr::select(-(1:5)) %>% 
  pivot_longer(
    cols = ends_with("_2"),
    names_to = "marital",
    values_to = "death",
    values_drop_na = TRUE
  ) %>% 
  mutate(sex="Female")

death1999_2008 <- rbind(death1999_2008_m,death1999_2008_f) %>% 
  mutate(death=as.numeric(death),
         marital=substr(marital,1,3)) %>% 
  mutate(marital=case_when(
    .$marital == "有配偶" ~ "Married",
    .$marital == "未婚_" ~ "Never-married",
    .$marital == "死別_" ~ "Widowed",
    .$marital == "離別_" ~ "Divorced",
    .$marital == "不詳_" ~ "Unstated"))

######################################################################
# Merge and output
# 5 marital status, 35 causes of death, 18 age categories,2 sex = 6300!
######################################################################

death1999_2018 <- bind_rows(death1999_2008,death2009,death2010_2015,death2016_2017,death2018) %>% 
  arrange(year,sex,death_code,age) %>% 
  dplyr::select(year,sex,death_code,death_lab,age,marital,death) %>% 
  mutate(cause=case_when(
    .$death_code == "Se01" ~ "Tuberculosis",
    .$death_code == "Se02" ~ "Cancer",
    .$death_code == "Se15" ~ "Diabetes",
    .$death_code == "Se16" ~ "Hypertensive",
    .$death_code == "Se17" ~ "Heart",
    .$death_code == "Se22" ~ "Cerebrovascular",
    .$death_code == "Se25" ~ "Aortic aneurysm",
    .$death_code == "Se26" ~ "Pneumonia",
    .$death_code == "Se27" ~ "Chronic obstructive pulmonary",
    .$death_code == "Se28" ~ "Asthma",
    .$death_code == "Se29" ~ "Liver",
    .$death_code == "Se30" ~ "Kidney failure",
    .$death_code == "Se31" ~ "Old age",
    .$death_code == "Se32" ~ "Accidents",
    .$death_code == "Se34" ~ "Suicide")) %>% 
  na.omit(cause) 
write.csv(death1999_2018,"0.Data/Share/Cause-of-death_1999_2018.csv")

####################
### Merge  #########
####################
death <- death1999_2018 %>% 
  filter(marital == "Never-married"| marital=="Married") %>% 
  mutate(year=case_when(
    .$year >= 2000 & .$year < 2005  ~ 2000,
    .$year >= 2005 & .$year < 2010  ~ 2005,
    .$year >= 2010 & .$year < 2015  ~ 2010,
    .$year >= 2015 & .$year < 2020  ~ 2015)) %>% # four years
  na.omit(year) %>% 
  group_by(year,sex, death_code, death_lab, age, cause, marital) %>% 
  summarize(death = sum(death)) 

pop <- bind_rows(pop2000,pop2005,pop2010,pop2015) %>% 
  filter(marital == "Never-married"| marital=="Married") %>% 
  filter(age != "Total") %>% 
  mutate(age=as.numeric(age))

df <- death %>% 
  left_join(pop,by=c("year","age","sex","marital")) %>% 
  mutate(value=if_else(year<2015,value*5,value*4)) %>%  # times by years
  mutate(rate=round((death/value)*1000,2))

df_single <- df %>% 
  filter(marital=="Never-married")

dfx <- df %>% 
  filter(marital=="Married") %>% 
  left_join(df_single,by=c("year","sex","age","death_code","death_lab","cause")) %>% 
  dplyr::select(year,age,sex,death_code,death_lab,cause,rate_m=rate.x,rate_s=rate.y) %>% 
  mutate(excess=round(rate_s/rate_m,1)) %>% 
#  filter(death_code==c("2","3","15","16","17","22","26","27","28","29","30","31") | death_code==c("32","33","35")) %>% 
  dplyr::select(year,age,sex,excess,cause) %>% 
  ungroup() %>% 
  dplyr::select(year,age,sex,excess,cause) %>% 
  mutate(birth=year-age) %>% 
  na.omit(excess) %>% 
  filter(excess != Inf) %>% 
  filter(excess < 15) #excessが大きすぎるのを削除

### heat map ###
dfm <- dfx %>% 
  filter(sex=="Male")

dff <- dfx %>% 
  filter(sex=="Female")

ggplot(dfm, aes(y=-age, x=birth, fill=excess))+
  geom_tile() + theme_minimal()+
  theme_few()+facet_wrap(~cause)+
  scale_fill_viridis(option="magma") +ggtitle("Relative mortality ratio for men")+
  labs(x="", y="", fill="") + theme(legend.title= element_blank()) + 
  ggsave(height=8,width=10,dpi=200, filename="3.Results/excess_cause_men.pdf",  family = "Helvetica")

ggplot(dff, aes(y=-age, x=birth, fill=excess))+
  geom_tile() + theme_minimal()+
  theme_few()+facet_wrap(~cause)+
  scale_fill_viridis(option="magma") +ggtitle("Relative mortality ratio for women")+
  labs(x="", y="", fill="") + theme(legend.title= element_blank()) + 
ggsave(height=8,width=10,dpi=200, filename="3.Results/excess_cause_women.pdf",  family = "Helvetica")
