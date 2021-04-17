library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)


country<-"Canada"

canada_vaxx_data<-cumulative_covid_vaccinations%>%set_names(c("Country","Code","Date","total_vaxx"))%>%filter(Country==country)

canada_case_data<-owid_covid_data%>%filter(location==country)%>%select(date,new_cases)%>%
  set_names(c("Date","new_cases"))

table<-left_join(canada_case_data,canada_vaxx_data,by="Date")%>%
  select(Date,new_cases,total_vaxx)%>%mutate(vaxx_per_thousand=total_vaxx/1000)


table%>%
  ggplot()+
  geom_point(aes(Date,new_cases),col="red")+
  geom_point(aes(Date,vaxx_per_thousand))+
  xlab("Date") +
  ylab("New Daily Cases") +
  ggtitle("Comparing new cases to vaccine cases/1000")
  


  


 