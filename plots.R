library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(rvest)

cumulative_covid_vaccinations <- read_csv("cumulative-covid-vaccinations.csv")
owid_covid_data <- read_csv("owid-covid-data.csv")
url<-"https://www.worldometers.info/world-population/population-by-country/"

#set.seed(20)

number_of_countries<-3
country_index<-0
for(i in 1:number_of_countries)
{
   country_index[i]<-sample(nrow(owid_covid_data),1)
}
country_df<-data.frame(my_countries=unique(owid_covid_data$location[data.frame(Country=country_index)$Country]))
country_df<-intersect(country_df$my_countries,owid_covid_data$location)
country_df<-data.frame(Country=country_df)%>%filter(Country != "International")
number_of_countries<-nrow(country_df)



#######
pop<-read_html(url)%>%
  html_nodes("table")%>%
  html_table()%>%
  .[[1]]%>%setNames(c("#","Country","Population"))%>%
  select(Country, Population)%>%
  filter(Country %in% country_df)%>%
  mutate_at(2,parse_number)


#######  
death_data<-owid_covid_data%>%filter(location%in%country_df$Country & !is.na(continent))%>%
  select(date,location,new_deaths)%>%
  set_names(c("Date","Country","New_Deaths"))%>%na.omit(New_Deaths)
  

vaxx_data<-cumulative_covid_vaccinations%>%
  set_names(c("Country","Code","Date","total_vaxx"))%>%
  filter(Country%in%country_df$Country)%>%
  select(Date,Country,total_vaxx)

test<-setdiff(country_df$Country,vaxx_data$Country)
temp_df<-data.frame(Date=NA,Country=NA, total_vaxx=NA)
if(is.null(test)){
  temp_df<-data.frame(Date=NA, Country = test,total_vaxx=NA)
}


first_vaxx_date<-0


for(j in 1:number_of_countries)
{
  first_vaxx<-vaxx_data%>%filter(total_vaxx != 0)%>%rbind(temp_df)%>%
    spread(Country,total_vaxx)%>%
    select(Date,country_df[j,])%>%na.omit(country_df[j,])
  first_vaxx_date[j]<-first_vaxx[1,1]
}
first_vaxx_date<-data.frame(Date=first_vaxx_date)
first_vaxx_date<-gather(first_vaxx_date,key=data,value = Date)%>%select(Date)
first_vaxx_date<-bind_cols(first_vaxx_date,country_df)


case_data<-owid_covid_data%>%filter(location%in%country_df$Country)%>%select(date, location, new_cases)%>%
  set_names(c("Date","Country","new_cases"))%>%na.omit()

Full_Data<-left_join(case_data,death_data,by=c("Date","Country"))
  

#custom_table<-left_join(case_data,vaxx_data, by = "Date")#%>%
#  select(Date,Country,new_cases,total_vaxx)%>%left_join(death_data,by="Date")%>%
#  select(Date,new_cases,New_Deaths,total_vaxx)

#test<-data.frame(test=which(is.na(owid_covid_data$continent)))

#######
#number_vaccinated<-max(vaxx_data$total_vaxx)
#num_vax_percentage<-(number_vaccinated/pop)*100


#######
Full_Data%>%group_by(Country)%>%
  ggplot()+
  geom_point(aes(Date,New_Deaths,col=Country))+
  geom_vline(data = first_vaxx_date, aes(xintercept = Date))+
  facet_grid(Country~., scales="free")+
  theme(legend.position = "none")+
  xlab("Date") +
  ylab("New Daily Deaths") +
  ggtitle("Comparing new cases to first Vaccines administered for different Countries")
 
  


  


 