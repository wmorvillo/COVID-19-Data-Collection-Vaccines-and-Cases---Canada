library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)



country<-"Brazil"

vaxx_data<-cumulative_covid_vaccinations%>%set_names(c("Country","Code","Date","total_vaxx"))%>%filter(Country==country)

case_data<-owid_covid_data%>%filter(location==country)%>%select(date,new_cases)%>%
  set_names(c("Date","new_cases"))

table<-left_join(case_data,vaxx_data,by="Date")%>%
  select(Date,new_cases,total_vaxx)%>%mutate(vaxx_per_thousand=total_vaxx/100)

first_vax<-table$Date[which.min(table$total_vaxx)]


table%>%
  ggplot()+
  geom_point(aes(Date,new_cases),col="red")+
  geom_vline(xintercept=first_vax)+
  ylim(-100, (max(table$new_cases))*1.15)+
  xlab("Date") +
  ylab("New Daily Cases") +
  ggtitle("Comparing new cases to first Vaccines administered")
 
  


  


 