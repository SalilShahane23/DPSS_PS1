#ASSIGNMENT 1
#Due Date: 30 June, 2019
#Student_Name: Salil Shahane (salils@uchicago.edu)

########################################################################

library(tidyverse) #Loading tidyverse packages

#SET1

#1.1
x<-(c(1:25))
x[-19]

#1.2
y<-as.Date("2019/06/30")
class(y)

#1.3
z <- list("dog","octopus",3,NA,TRUE,FALSE,y)

#1.4
z[!is.na(z)]

#1.5
output<- (z>0|z=="dog")
sum(output,na.rm = TRUE)

#1.6
pov <- read.csv("poverty.csv")

#1.7
#pov_list<-list(pov[1:5,3],pov[1:5,5])
pov[1:5,c(3,5)]
#pov_list

#1.8
pov[pov$Region %in% c("Europe Mostly", "Middle East", "Eastern Europe"),]%>%select_if(is.numeric)

#################################################################################

#SET2

#2.1
library(tidyverse) #library(dplyr), library(tidyr), library(stringr), library(readr)
library(lubridate)

#2.2
dpss_data<-read_csv("dpss_data.csv")
dpss<-head(dpss_data,30)

#2.3
dpss%>%
  filter(country=="Mexico"|country== "China"|country== "Japan")%>%
  filter(op_system=="Apple/Mac OS")

#2.4
dpss%>%
  select(c(3,4,6,7,8))

#2.5
dpss%>%
  filter(country=="United States")%>%
  pull(height)%>%
  sd()

#2.6
dpss <- dpss%>%
  mutate(international=country!="United States")

#2.7
dpss%>%
  summarise_if(is_numeric,na.rm=TRUE, mean)


#?summarize_if

#2.8
dpss%>%
  group_by(country,op_system)%>%
  mutate(os_users=n())%>%
  group_by(op_system,country)%>%
  mutate(tot_user_per_cnty_per_os=sum(os_users))%>%
  group_by(country)%>%
  mutate(total_usr_per_cnty=sum(tot_user_per_cnty_per_os))%>%
  mutate(pct=((tot_user_per_cnty_per_os)/(total_usr_per_cnty)))%>%
  group_by(country,op_system)%>%
  summarise(pct_user_wrt_os=sum(pct))


###############################################################################

#SET3

#3.1
dpss<-rename(dpss,Country=country)
pov_joined<-pov%>%
  left_join(dpss,by="Country")

#3.2
pov%>% 
  semi_join(dpss,by="Country")

#3.3
table4a_long<-table4a%>%
  gather(key=year,value=value)

#3.4
pov_joined%>%count(is.na(op_system))


#3.5
str_sub("I LOVE CODING IN R", 8, 13)%>%str_to_title()

#3.6
births<-read_csv("births.csv")
births_new <- mutate(births,date=make_date(year=year,month=month,day=day))

#3.7
with(births_new,difftime(max(date), min(date)))

#3.8
births_new%>%
  group_by(month,day)%>%
  mutate(is_leap_day=(month==2&day==29))%>%
  filter(is_leap_day==TRUE)%>%
  group_by(year)%>%
  summarise(leap_day_births=sum(births))

###########################################################

#SET4

acc2014<-read.csv("ACCIDENT.csv")
acc2015<-read.csv("ACCIDENT.csv")
acc2015<-read_sas("accident.sas7bdat")
acc2014<-acc2014%>%
  mutate(TWAY_ID2=na_if(TWAY_ID2,""))

table(is.na(acc2014$TWAY_ID2))

acc<- bind_rows(acc2014,acc2015)

fips<-read_csv("fips.csv")
acc%>% as.character("COUNTY", "STATE")
acc%>% str

  
