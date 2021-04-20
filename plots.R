library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)

cumulative_covid_vaccinations <- read_csv("cumulative-covid-vaccinations.csv")
owid_covid_data <- read_csv("owid-covid-data.csv")

country<-"United States"

death_data<-owid_covid_data%>%filter(location==country)%>%
  select(date,location,new_deaths)%>%
  set_names(c("Date","Country","New_Deaths"))

vaxx_data<-cumulative_covid_vaccinations%>%
  set_names(c("Country","Code","Date","total_vaxx"))%>%
  filter(Country==country)

case_data<-owid_covid_data%>%filter(location==country)%>%select(date,new_cases)%>%
  set_names(c("Date","new_cases"))

custom_table<-left_join(case_data,vaxx_data, by = "Date")%>%
  select(Date,new_cases,total_vaxx)

custom_table<-left_join(death_data,custom_table,by = "Date")

custom_table%>%
  ggplot()+
  geom_point(aes(Date,new_cases),col="red")+
  geom_point(aes(Date,New_Deaths))+
  geom_vline(xintercept=custom_table$Date[which.min(custom_table$total_vaxx)])+
  ylim(-100, (max(custom_table$new_cases))*1.15)+
  xlab("Date") +
  ylab("New Daily Cases") +
  ggtitle("Comparing new cases to first Vaccines administered")
 
  


  


 