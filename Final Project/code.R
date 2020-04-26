
library(tidyverse)
library(ggplot2)
library(lubridate)
library(car)

Sys.setlocale("LC_TIME", "English")

AllData<-read_csv("us_states_covid19_daily.csv")

dim(AllData)

AllData<-AllData%>% mutate(date= ymd(date))

AllIncrease<-AllData %>% 
  select(date, positiveIncrease, deathIncrease, positive, recovered, death) %>%
  group_by(date) %>% summarise(AllDeathIncrease = sum(deathIncrease, na.rm=TRUE), 
                               AllPositiveIncrease = sum(positiveIncrease, na.rm=TRUE),
                               AllPositive=sum(positive, na.rm=TRUE), 
                               AllRecovered=sum(recovered, na.rm=TRUE), 
                               AllDeath=sum(death, na.rm=TRUE)
                               )

Figure1<-AllIncrease%>%select(date,AllPositive,AllRecovered, AllDeath)%>%
  gather(key = flag, value = value, AllDeath, AllPositive,AllRecovered)


Figure1%>%ggplot(aes(x=date, y=value))+
  geom_line(aes(color = flag), alpha = 0.7, lwd=1.5)+ ylab("number")+
  labs(title="All Positive, Death and Recovered") + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


Figure2<-AllIncrease%>%
  select(date,AllDeathIncrease,AllPositiveIncrease)%>%
  gather(key = flag, value = value, AllDeathIncrease, AllPositiveIncrease)

Figure2%>%
  ggplot(aes(x=date, y=value))+
  geom_line(aes(color = flag), alpha = 0.7, lwd=1.5)+ ylab("number")+
  labs(title="Daily Increase of Positive and Death") + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


AllData<-AllData %>% rename(StateCode = state)

States=data.frame(StateCode=state.abb, State=state.name)
States$StateCode=as.character(States$StateCode)
States$State=as.character(States$State)

AllDataTable<-
  AllData %>%  inner_join(States, by = c("StateCode" = "StateCode")) 


Figure3<-AllDataTable %>% filter(date=="2020-04-23")%>% 
  select(State, death, positive)

Figure3%>%
  ggplot(aes(x=State, y=positive))+geom_bar(stat = "identity")+
  labs(title="Number of positive in different US States")+
  theme_bw()+theme(axis.text.x = element_text(angle = 80, hjust=1),
                   plot.title = element_text(hjust = 0.5)) 

Figure3%>%
  ggplot(aes(x=State, y=death))+geom_bar(stat = "identity")+
  labs(title="Number of death in different US States")+
  theme_bw()+theme(axis.text.x = element_text(angle = 80, hjust=1),
                   plot.title = element_text(hjust = 0.5)) 

######linear regression method to predict the death

AllData2=AllDataTable %>% select(death, positive, totalTestResults, 
                                 positiveIncrease, totalTestResultsIncrease)%>%
  filter(death>50,positive>0, totalTestResults>0, positiveIncrease>0, totalTestResultsIncrease>0)
        

modData=na.omit(AllData2)

fit1=lm(death~positive+totalTestResults+positiveIncrease+totalTestResultsIncrease ,data=modData)

summary(fit1)
par(mfrow=c(2,2))
plot(fit1)

shapiro.test(residuals(fit1))

 

