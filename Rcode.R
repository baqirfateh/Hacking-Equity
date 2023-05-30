###############################################################################
# Project: Hackaton 
# Team: 'Fighting Magnolias'
# Date: Oct 2022
##############################################################################


rm(list=ls())

#install.packages(c("tidyverse","data.table","readr","lubridate","ggplot"))

library(tidyverse)
library(data.table)
library(readr)
library(lubridate)
library(ggplot2)

## Downloading Louisiana (LA) Data directly from the website 

# LA Data 
temp <- tempfile()
download.file("https://stacks.stanford.edu/file/druid:yg821jf8611/yg821jf8611_la_new_orleans_2020_04_01.csv.zip",temp)
la_neworlean <- read.csv(unzip(temp,"la_new_orleans_2020_04_01.csv"))

la_neworlean%>%group_by(subject_race)%>%summarise(n=n())%>%mutate(mean=n/sum(n))
la_neworlean%>%group_by(reason_for_stop)%>%summarise(n=n())%>%mutate(mean=n/sum(n))
la_neworlean%>%group_by(outcome)%>%summarise(n=n())%>%mutate(mean=n/sum(n))

## Downloading Georgia data from the website 

#GA data
data2 <- read_rds("https://stacks.stanford.edu/file/druid:yg821jf8611/yg821jf8611_ga_statewide_2020_04_01.rds")

data2%>%group_by(subject_race)%>%summarise(n=n())%>%mutate(mean=n/sum(n))
#data2%>%group_by(violation)%>%summarise(n=n())%>%mutate(mean=n/sum(n))
data2%>%group_by(outcome)%>%summarise(n=n())%>%mutate(mean=n/sum(n))


# MD data 
#data_md <- read_rds("https://stacks.stanford.edu/file/druid:yg821jf8611/yg821jf8611_ma_statewide_2020_04_01.rds")

## Creating new variables - hour, day, month and weekday 

# LA
la_neworlean <- la_neworlean%>%mutate(day = day(date), month = month(date),
                                      hour = hour(hms(time)),
                                    week_day = wday(date,label = TRUE,abbr = FALSE),
                                    year = year(date))

# GA
data_ga <- data2%>%mutate(day = day(date), month = month(date),
                                      hour = hour(hms(time)),
                                      week_day = wday(date,label = TRUE,abbr = FALSE))

## Frequency of disciplinary and no disciplinary action 
freq_la <- la_neworlean%>%group_by(outcome)%>%
  summarise(n = n())%>%mutate(mean = n/sum(n), state = rep("LA"))
  
freq_ga <- data_ga%>%group_by(outcome)%>%
  summarise(n = n())%>%mutate(mean = n/sum(n), state = rep("GA"))

freq_la_ga <- freq_la%>%rbind(freq_ga)%>%filter(outcome!="NA")


freq_la_ga


## stops volume by hour of day

#LA
la_neworlean%>%count(hour)%>%mutate(mean=n/sum(n))

la_neworlean%>%count(hour)%>% 
  ggplot(aes(hour,log(n)))+geom_line()+geom_point()+
  scale_x_continuous(breaks = seq(0,23,1))+xlab("Hour of Day")+ylab("Frequency (log)")+
  ggtitle("Figure 1: Stop Volumes by Hour of Day, LA")+
  theme_classic()

# GA
data_ga%>%count(hour)%>%
  ggplot(aes(hour,log(n)))+geom_line()+geom_point()+
  scale_x_continuous(breaks = seq(0,23,1))+xlab("Hour of Day")+ylab("Frequency (log)")+
  ggtitle("Figure 2: Stop Volumes by Hour of Day, GA")+
  theme_classic()

# Combined 
la <- la_neworlean%>%count(hour)%>%mutate(state = rep("LA",time=24), mean = n/sum(n))
ga <- data_ga%>%count(hour)%>%mutate(state = rep("GA", time = 24), mean = n/sum(n))

combined <- la%>%rbind(ga)

combined%>%
  ggplot(aes(hour,mean,group = state))+geom_line(aes(color = state))+geom_point(aes(color = state))+
  scale_x_continuous(breaks = seq(0,23,1))+xlab("Hour of Day")+ylab("Proportion")+
  scale_color_discrete(name="")+
  ggtitle("Figure 3: Stop Volumes by Hour of Day, LA and GA")+
  theme_classic()

### Stop volume by week 

# LA data
la_neworlean%>%filter(week_day!="NA")%>%
  ggplot(aes(week_day,fill = week_day))+geom_bar()+xlab("")+ylab("Frequency")+
  guides(fill = "none")+
  ggtitle("Figure 4: Stop Volumes by Week Day, LA")+
  theme_classic()

# GA data 
data_ga%>%filter(week_day!="NA")%>%
  ggplot(aes(week_day, fill = week_day))+geom_bar()+xlab("")+ylab("Frequency")+
  guides(fill = "none")+
  ggtitle("Figure 5: Stop Volumes by Week Day, GA")+
  theme_classic()

## by day of month 
# LA
la_neworlean%>%count(day)%>%mutate(mean=n/sum(n))%>%
  ggplot(aes(day,mean))+geom_line()+geom_point()+
  scale_x_continuous(breaks = seq(1,31,1))+xlab("Day of Month")+ylab("Proportion")+
  ggtitle("Figure 6: Stop Volumes by Day of Month, LA")+
  theme_classic()
  
# GA
data_ga%>%count(day)%>%mutate(mean=n/sum(n))%>%
  ggplot(aes(day,mean))+geom_line()+geom_point()+
  scale_x_continuous(breaks = seq(1,31,1))+xlab("Day of Month")+ylab("Proporation")+
  ggtitle("Figure 7: Stop Volumes by Day of Month, GA")+
  theme_classic()  
  
 # Distribution of stop volume by disciplinary action 

la_neworlean%>%filter(outcome!="NA"&week_day!="NA")%>%
  group_by(week_day,outcome)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
  ggplot(aes(week_day,log(n),fill = outcome))+geom_col()+
  xlab("")+ylab("Frequency (log)")+
  facet_wrap(~outcome, ncol = 1)+
  guides(fill = "none")+
  ggtitle("Figure 8: Disciplinary Actions by Week Day, LA")+
  theme_classic()


la_month <- la_neworlean%>%count(day)%>%mutate(mean=n/sum(n), state = rep("LA"))

ga_month <- data_ga%>%count(day)%>%mutate(mean=n/sum(n), state = rep("GA"))

la_ga_month <- la_month%>%rbind(ga_month)

la_ga_month%>%
  ggplot(aes(day,mean,group = state))+geom_line(aes(color = state))+geom_point(aes(color = state))+
  scale_x_continuous(breaks = seq(0,31,1))+xlab("Day of Month")+ylab("Monthly Mean")+
  scale_color_discrete(name="")+
  ggtitle("Figure 9: Stop Volumes by Day of Month, LA and GA")+
  theme_classic()

## Disagregation by Age 


# LA
la_age <- la_neworlean%>%count(subject_age)%>%mutate(mean=n/sum(n), state = rep("LA"))%>%
  filter(subject_age!="NA"&n>2000)
la_age


la_age%>%
  ggplot(aes(subject_age, mean))+geom_line()+geom_point()+
  xlab("Driver's Age")+ylab("Mean")+
  guides(fill = "none")+
  ggtitle("Figure 10: Stop Volume by Age ,LA")+
  theme_classic()


### Disaggregation by sex

# LA
la_sex <- la_neworlean%>%count(subject_sex)%>%mutate(mean=n/sum(n), state = rep("LA"))%>%
  filter(subject_sex!="NA")
la_sex
# GA
ga_sex <- data_ga%>%count(subject_sex)%>%mutate(mean=n/sum(n), state = rep("GA"))%>%
  filter(subject_sex!="NA")
ga_sex

la_ga_sex <- la_sex%>%rbind(ga_sex)

la_ga_sex%>%
  ggplot(aes(subject_sex, mean, fill = state))+geom_col()+
  xlab("")+ylab("Mean")+
  guides(fill = "none")+
  ggtitle("Figure 11: Stop Volume by Sex , GA and LA")+
  geom_text(aes(label = round(mean,2)), vjust = -0.2)+
  facet_wrap(~state,ncol = 2)+
  theme_classic()

### Disaggregation by sex and race 

#LA

la_neworlean%>%group_by(subject_sex,subject_race)%>%summarise(n=n())%>%
  mutate(mean=n/sum(n))%>%
  filter(subject_race%in%c("black", "white","hispanic", "asian/pacific islander"))



# GA

data_ga%>%group_by(subject_sex,subject_race)%>%summarise(n=n())%>%
  mutate(mean=n/sum(n))%>%
  filter(subject_race%in%c("black", "white","hispanic", "asian/pacific islander")&subject_sex!="NA")



### Disaggregation by Race

#LA data 
traffic_stop_race <- la_neworlean%>%count(subject_race)%>%
  mutate(mean=n/sum(n))%>%rename(race=subject_race)%>%
  filter(race%in%c("black", "white","hispanic"))%>%
  select(race, mean)%>%mutate(type = rep("Traffic Stop"))

# New Orlean census data 

race_new_orl <- c("white", "black", "hispanic")
census_la <- c(0.33,0.59, 0.06 )

data_census_la <- data.frame(cbind(race_new_orl,census_la))
colnames(data_census_la) <- c("race","mean")

data_census_la <- data_census_la%>%mutate(type = rep("Census"))

data_combined_la <- traffic_stop_race%>%rbind(data_census_la)%>%
  mutate(mean=as.numeric(mean))


data_combined_la%>%
  ggplot(aes(race,round(mean,2),fill = race))+
  geom_col()+xlab("")+ylab("Proportion")+
  ggtitle("Figure 12: Stop vs Census data, LA")+
  guides(fill = "none")+
  geom_text(aes(label = round(mean,2)), vjust = -0.2)+
  facet_wrap(~type, ncol = 2)+
  theme_classic()

# GA traffic stop data
traffic_stop_race_ga <- data_ga%>%count(subject_race)%>%
  mutate(mean=n/sum(n))%>%rename(race=subject_race)%>%
  filter(race%in%c("black", "white","hispanic"))%>%
  select(race, mean)%>%mutate(type = rep("Traffic Stop"))

# GA census data 


race_ga <- c("white", "black", "hispanic")
census_ga <- c(0.59,0.33, 0.1 )

data_census_ga <- data.frame(cbind(race_ga,census_ga))
colnames(data_census_ga) <- c("race","mean")

data_census_ga <- data_census_ga%>%mutate(type = rep("Census"))

data_combined_ga <- traffic_stop_race_ga%>%rbind(data_census_ga)%>%
  mutate(mean = as.numeric(mean))


data_combined_ga%>%
  ggplot(aes(race,round(mean,2),fill = race))+
  geom_col()+xlab("")+ylab("Proportion")+
  ggtitle("Figure 13: Stop vs Census Data, GA")+
  guides(fill = "none")+
  geom_text(aes(label = round(mean,2)), vjust = -0.2)+
  facet_wrap(~type, ncol = 2)+
  theme_classic()


## detailed disaggregation by race and hour of day 


data1_race <- la_neworlean%>%group_by(subject_race,hour)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
  rename(race=subject_race)%>%
  filter(race%in%c("black", "white","hispanic", "asian/pacific islander"))

data1_race%>%
ggplot(aes(hour,mean,group=race))+geom_line(aes(color = race))+geom_point(aes(color=race))+
  scale_x_continuous(breaks = seq(0,23,1))+xlab("Hour of Day")+ylab("Proportion")+
  scale_colour_discrete(name = "")+
  ggtitle("Figure 14: Stop Volumes by Hour of Day and Race, LA")+
  theme_classic()

# GA data
data2_race <- data_ga%>%group_by(subject_race,hour)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
  rename(race=subject_race)%>%
  filter(race%in%c("black", "white","hispanic"))

data2_race%>%
ggplot(aes(hour,mean,group=race))+geom_line(aes(color = race))+geom_point(aes(color=race))+
  scale_x_continuous(breaks = seq(0,23,1))+xlab("Hour")+ylab("Proportion")+
  scale_colour_discrete(name = "")+
  ggtitle("Figure 15: Stop Volumes by Hour of Day and Race, GA")+
  theme_classic()

# Combined 

la_race <- la_neworlean%>%group_by(subject_race,hour)%>%summarise(n=n())%>%
  filter(subject_race%in%c("black", "white","hispanic","asian/pacific islander"))%>%
mutate(mean = n/sum(n), state = rep("LA"))

ga_race <- data_ga%>%group_by(subject_race,hour)%>%summarise(n=n())%>%
  filter(subject_race%in%c("black", "white","hispanic","asian/pacific islander"))%>%
  mutate(mean = n/sum(n), state = rep("GA"))

combined_race <- la_race%>%rbind(ga_race)%>%rename(race=subject_race)

combined_race%>%
  ggplot(aes(hour,mean,group=race))+geom_line(aes(color = race))+geom_point(aes(color=race))+
  scale_x_continuous(breaks = seq(0,23,1))+xlab("Hour of Day")+ylab("Proportion")+
  scale_color_discrete(name="")+
  ggtitle("Figure 16: Stop Volumes by Hour of Day and Race, GA and LA")+
  facet_wrap(~state,ncol = 1)+
  theme_classic()


# Type of Violation 
la_neworlean%>%count(reason_for_stop)%>%mutate(mean=n/sum(n))%>%filter(n>1000)

# Type of violation and time of day
data_type_la <- la_neworlean%>%count(hour, reason_for_stop)%>%mutate(mean=n/sum(n))%>%filter(n>1000)

data_type_la%>%
  ggplot(aes(hour,mean,group=reason_for_stop))+geom_line(aes(color = reason_for_stop))+
  geom_point(aes(color = reason_for_stop))+
  scale_x_continuous(breaks = seq(0,23,1))+xlab("Hour of Day")+ylab("Proportion")+
  scale_color_discrete(name = "")+
  ggtitle("Figure 17: Type of Violation by Hour of Day and Race, LA")+
  theme_classic()

#### hour of day and type of disciplinary action 


## LA data 

la_neworlean%>%filter(outcome!="NA")%>%
  group_by(outcome,hour)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
  ggplot(aes(hour,mean,group=outcome))+geom_line(aes(color = outcome))+
  geom_point(aes(color = outcome))+
  scale_x_continuous(breaks = seq(0,23,1))+xlab("Hour of Day")+ylab("Proportion")+
  scale_color_discrete(name = "")+
  ggtitle("Figure 18: Types of disciplinary action by Hour of Day, LA")+
  theme_classic()

## Frequency of Arrest Made 
nrow(la_neworlean%>%filter(outcome=="arrest"))/nrow(la_neworlean)

# Approximatly, out of five stops, one is arrested 

la_arrest <- la_neworlean%>%filter(outcome=="arrest")

# Disaggregating arrest by race 
la_arrest%>%group_by(subject_race)%>%summarise(n=n())%>%mutate(mean=n/sum(n))

la_arrest%>%group_by(subject_race)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
filter(subject_race%in%c("black", "white","hispanic"))%>%
ggplot(aes(subject_race,mean, fill = subject_race))+
  geom_col()+xlab("")+ylab("Proportion")+
  ggtitle("Figure 19: Arrests Made by Race, LA")+
  geom_text(aes(label = round(mean,2)), vjust = -0.2)+
  guides(fill = "none")+
  theme_classic()

#Disaggregating arrest by race and gender 

la_arrest%>%group_by(subject_sex, subject_race)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
  filter(subject_race%in%c("black", "white","hispanic"))%>%
  ggplot(aes(subject_race,mean,fill=subject_race))+
  geom_col()+xlab("")+ylab("Proportion")+
  ggtitle("Figure 20: Arrests Made by Race and Sex, LA")+
  guides(fill="none")+
  geom_text(aes(label = round(mean,2)), vjust = -0.2)+
  facet_wrap(~subject_sex,ncol = 2)+
  theme_classic()

## GA data - Disciplinary Action 
# By Race 

data_ga%>%filter(outcome!="NA")%>%
  group_by(subject_race)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
  filter(subject_race%in%c("black", "white","hispanic"))

data_ga%>%filter(outcome!="NA")%>%
  group_by(subject_race)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
  filter(subject_race%in%c("black", "white","hispanic"))%>%
ggplot(aes(subject_race,mean, fill = subject_race))+
  geom_col()+xlab("")+ylab("Proportion")+
  guides(fill = "none")+
  ggtitle("Figure 21: Disciplinary Action by Race, GA")+
  geom_text(aes(label = round(mean,2)), vjust = -0.2)+
  theme_classic()

# By race and and sex 
data_ga%>%filter(outcome!="NA"&subject_sex!="NA")%>%
  group_by(subject_sex, subject_race)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
  filter(subject_race%in%c("black", "white","hispanic"))


data_ga%>%filter(outcome!="NA"&subject_sex!="NA")%>%
  group_by(subject_sex, subject_race)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
  filter(subject_race%in%c("black", "white","hispanic"))%>%
  ggplot(aes(subject_race,mean,fill=subject_race))+
  geom_col()+xlab("")+ylab("Proportion")+
  guides(fill="none")+
  facet_wrap(~subject_sex,ncol = 2)+
  ggtitle("Figure 22: Disciplinary Action by Race and Sex, GA")+
  geom_text(aes(label = round(mean,2)), vjust = -0.2)+
  theme_classic()

# Disagrgregated by race and hour of day 

la_neworlean%>%filter(outcome!="NA")%>%
  group_by(subject_race, outcome)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
  filter(subject_race%in%c("black", "white","hispanic", "asian/pacific islander"))


la_race_dis_ac <- la_neworlean%>%filter(outcome!="NA")%>%
  group_by(subject_race, outcome ,hour)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
  filter(subject_race%in%c("black", "white","hispanic", "asian/pacific islander"))

  la_race_dis_ac%>%
  ggplot(aes(hour,mean,group=outcome))+geom_line(aes(color = outcome))+
  geom_point(aes(color = outcome))+
  scale_x_continuous(breaks = seq(0,23,1))+xlab("Hour of Day")+ylab("Proportion")+
  ggtitle("Figure 23: Types of Disciplinary Action by Hour of Day and Race, LA")+
    scale_color_discrete(name = "")+
    facet_wrap(~subject_race,ncol = 2)+
  theme_classic()
  
  
##  Disciplinary action by sex
  
 # LA 
  
    la_neworlean%>%filter(outcome!="NA")%>%
    group_by(subject_sex, outcome )%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
    filter(subject_sex%in%c("male","female"))
  

   la_sex_discip_ac <- la_neworlean%>%filter(outcome!="NA")%>%
    group_by(subject_sex, outcome,hour)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
    filter(subject_sex%in%c("male","female"))
  
   la_sex_discip_ac%>%
     ggplot(aes(hour,mean,group=outcome))+geom_line(aes(color = outcome))+
     geom_point(aes(color = outcome))+
     scale_x_continuous(breaks = seq(0,23,1))+xlab("Hour of Day")+ylab("Proportion")+
     ggtitle("Figure 24: Types of Disciplinary Action by Hour of Day and Sex, LA")+
     scale_color_discrete(name="")+
     facet_wrap(~subject_sex,ncol = 2)+
     theme_classic()
  
  
## The distribution of those who have been stopped but faced no disciplinary action 
 la_race_no_dis_ac <- la_neworlean%>%filter(arrest_made=="FALSE"&citation_issued=="FALSE"&warning_issued=="FALSE")%>%
    group_by(subject_race, subject_sex)%>%summarise(n=n())%>%mutate(mean=n/sum(n))%>%
    filter(subject_race%in%c("black", "white","hispanic"))
 la_race_no_dis_ac
  
la_race_no_dis_ac%>%
  ggplot(aes(subject_race,mean, fill=subject_race))+geom_col()+
  xlab("")+ylab("Proportion")+
  ggtitle("Figure 25: No Disciplinary Action by Race and Sex, LA")+
  guides(fill = "none")+
  facet_wrap(~subject_sex,ncol = 2)+
  geom_text(aes(label = round(mean,2)), vjust = -0.2)+
  theme_classic()

### Distribution of stop volume by types of vehicle 

## GA 
#year

year_ga <- data_ga%>%group_by(vehicle_year)%>%
  summarise(n=n())%>%mutate(mean=n/sum(n),state = rep("GA"))%>%
  filter(n>1000)%>%arrange(desc(n))
year_ga
year_ga%>%
ggplot(aes(vehicle_year,n))+geom_line()+
  geom_point()+
  xlab("Vehicle Year")+ylab("Count")+
  ggtitle("Figure 26: Distribution of Stop Volume by Vehicle Year, LA")+
  #scale_color_discrete(name="")+
  #facet_wrap(~subject_sex,ncol = 2)+

  theme_classic()

# By Make 

make_ga <- data_ga%>%group_by(vehicle_make)%>%
  summarise(n=n())%>%mutate(mean=n/sum(n))%>%filter(n>2000)%>%
  arrange(desc(n))
make_ga

# By model 
model_ga <- data_ga%>%group_by(vehicle_model)%>%
  summarise(n=n())%>%mutate(mean=n/sum(n))%>%filter(n>1000)%>%
  arrange(desc(n))

model_ga

# By color 
color_ga <- data_ga%>%group_by(vehicle_color)%>%
  summarise(n=n())%>%mutate(mean=n/sum(n))%>%filter(n>1000)%>%
  arrange(desc(n))

color_ga

## LA 
#year

year_la <- la_neworlean%>%group_by(vehicle_year)%>%filter(vehicle_year!="NA")%>%
  summarise(n=n())%>%mutate(mean=n/sum(n),state = rep("LA"))%>%
  filter(n>1000)%>%arrange(desc(n))
year_la%>%
  ggplot(aes(vehicle_year,n))+geom_line()+
  geom_point()+
  xlab("Vehicle Year")+ylab("Count")+
  ggtitle("Figure 27: Distribution of Stop Volume by Vehicle Year, LA")+
  #scale_color_discrete(name="")+
  #facet_wrap(~subject_sex,ncol = 2)+
  theme_classic()

# By Make 

make_la <- la_neworlean%>%group_by(vehicle_make)%>%
  summarise(n=n())%>%mutate(mean=n/sum(n))%>%filter(n>2000)%>%
  arrange(desc(n))
make_la

# By model 
model_la <- la_neworlean%>%group_by(vehicle_model)%>%
  summarise(n=n())%>%mutate(mean=n/sum(n))%>%filter(n>1000)%>%
  arrange(desc(n))

model_la

# By color 
color_la <- la_neworlean%>%group_by(vehicle_color)%>%
  summarise(n=n())%>%mutate(mean=n/sum(n))%>%filter(n>1000)%>%
  arrange(desc(n))

color_la


year_la_ga <- year_la%>%rbind(year_ga)

year_la_ga%>%
  ggplot(aes(vehicle_year,log(n),color = state))+geom_line()+
  geom_point()+
  xlab("Vehicle Year")+ylab("Frequency (log)")+
  ggtitle("Figure 28: Distribution of Stop Volume by Vehicle Year, LA and GA")+
  scale_color_discrete(name="")+
  theme_classic()

## Distribution stop volume county and location 

# LA 

la_zone <- la_neworlean%>%count(zone)%>%mutate(mean = n/sum(n))%>%filter(n>1000)
la_zone

la_zone%>%
  ggplot(aes(zone,mean))+geom_col()+
  ggtitle("Stope Volume by Zone, LA")+
  theme_classic()

la_neworlean%>%count(subject_race, zone)%>%mutate(mean = n/sum(n))%>%
  filter(n>1000&subject_race%in%c("black","white"))%>%
  ggplot(aes(zone,mean,fill = subject_race))+geom_col()+
  ggtitle("Figure 29: Stope Volume by Zone and Race, LA")+
  guides(fill = "none")+
  facet_wrap(~subject_race, ncol = 1)+
  theme_classic()

la_dist <- la_neworlean%>%count(district)%>%mutate(mean = n/sum(n))%>%filter(n>1000)
la_dist

la_dist%>%
  ggplot(aes(district,mean))+geom_col()+
  xlab("District Number")+ ylab("Proportion")+
  ggtitle("Figure 30: Stope Volume by District, LA")+
  geom_text(aes(label = round(mean,2)), vjust = -0.2)+
  theme_classic()

la_neworlean%>%count(subject_race, district)%>%mutate(mean = n/sum(n))%>%
  filter(n>1000&subject_race%in%c("black","white"))%>%
  ggplot(aes(district,mean,fill = subject_race))+geom_col()+
  ggtitle("Figure 31: Stope Volume by District and Race, LA")+
  guides(fill = "none")+
  facet_wrap(~subject_race, ncol = 1)+
  theme_classic()






