library(tidyverse)
library(gridExtra)
x<-read.csv('trend_w_demo.csv')
x %>% glimpse()

#몇몇 변수들이 한글로 되어있기 때문에 분석의 편리함을 위해 영어로 rename
x<-x %>% rename(Gender=성별구분, Age_Group=연령대, 
                Marriage=기혼스코어, Infant_Child=유아자녀스코어,
                Elementary_Child=초등학생자녀스코어, 
                MiddleHigh_Child=중고생자녀스코어, 
                University_Child=대학생자녀스코어, 
                House_Wife=전업주부스코어)

#year, month 별 observation수가 다르기때문에 비율로 분석
x %>% group_by(YM) %>% summarize(n=n())

#각 연월 당 카테고리별 차지비율 YM_Category로 저장
YM_Category<-x %>% group_by(YM ,Category) %>% 
  summarize(n=n()) %>% mutate(prop=n/sum(n)) 
YM_Category 


YM_Category %>% ggplot(aes(x=factor(YM),y=prop,col=Category,group=Category))+
  geom_point()+geom_line()+
  xlab('year_month')+ylab('proportion of category')

#사실 이번년도 코로나의 영향에 대한 분석이기에 201904와 201905, 202004와 202005를 묶어서 보는 것이 나아보임

Y_Category<- x%>% mutate(year=ifelse(YM %in% c(201904,201905),2019,2020)) %>%
  group_by(year,Category) %>% summarize(n=n()) %>% mutate(prop=n/sum(n))


Y_Category %>% ggplot(aes(factor(year),prop,col=Category,group=Category))+geom_point()+
  geom_line()+xlab('year')+ylab('proportion of category')



#카테고리별 odd ratio 계산

Category2019 <-Y_Category %>% ungroup %>% slice(1:10)
Category2020 <-Y_Category %>% ungroup %>% slice(11:20)
Category2019<-Category2019 %>% select(-year,-prop) %>% rename(n2019=n)
Category2020<-Category2020 %>% select(-year,-prop) %>% rename(n2020=n)

Category_or<-Category_or %>% mutate(odds_ratio=(n2019/(sum(n2019)-n2019))/
                                      (n2020/(sum(n2020)-n2020))) %>%
  arrange(-odds_ratio)
Category_or

#These differences may occur just due to chance. So for each Category, we can compute confidence interval.
log_or<-Category_or %>% mutate(log_or=log(odds_ratio)) %>%
  mutate(se=sqrt(1/n2019+1/(sum(n2019)-n2019)+
                   1/n2020+1/(sum(n2020)-n2020))) %>%
  mutate(conf.low=log_or-qnorm(0.975)*se,
         conf.high=log_or+qnorm(0.975)*se)
log_or
log_or %>%
  mutate(Category=reorder(Category,log_or)) %>%
  ggplot(aes(x=Category, ymin=conf.low,ymax=conf.high))+
  geom_errorbar()+
  geom_point(aes(Category,log_or)) + coord_flip()+
  ylab('Log odds ratio between Category and year')
#All Categories are significant since error bars does not capture 0.




#By Gender



#Overview
Y_Category_Gender<-x %>% mutate(year=ifelse(YM %in% c(201904,201905),2019,2020)) %>%
  group_by(Gender,year, Category) %>% summarize(n=n()) %>% mutate(prop=n/sum(n))

Y_Category_Gender %>% ungroup %>% ggplot(aes(factor(year),prop,col=Category, group=Category))+
  geom_line()+xlab('year')+ylab('proportion of category')+
  facet_grid(.~Gender)+geom_point()


#odds ratio
x_gen0<-x %>% filter(Gender==0)
x_gen1<-x %>% filter(Gender==1)

x_gen0<-x_gen0 %>% mutate(year=ifelse(YM %in% c(201904,201905),2019,2020)) %>% 
  group_by(year,Category) %>% summarize(n=n()) %>% mutate(prop=n/sum(n))
x_gen1<-x_gen1 %>% mutate(year=ifelse(YM %in% c(201904,201905),2019,2020)) %>% 
  group_by(year,Category) %>% summarize(n=n()) %>% mutate(prop=n/sum(n))


#gen0
x_gen0_2019<-x_gen0 %>% ungroup %>% slice(1:10)
x_gen0_2020<-x_gen0 %>% ungroup %>% slice(11:20)

x_gen0_2019 <-x_gen0_2019 %>% select(-year,-prop) %>% rename(n2019=n)
x_gen0_2020 <-x_gen0_2020 %>% select(-year,-prop) %>% rename(n2020=n)
x_gen0_or<- x_gen0_2019 %>% left_join(x_gen0_2020)

gen0_log_or<-x_gen0_or %>% mutate(log_or=log((n2019/(sum(n2019)-n2019))/
                                    (n2020/(sum(n2020)-n2020)))) %>%
  mutate(se=sqrt(1/n2019+1/(sum(n2019)-n2019)+
                   1/n2020+1/(sum(n2020)-n2020))) %>%
  mutate(conf.low=log_or-qnorm(0.975)*se,
         conf.high=log_or+qnorm(0.975)*se) %>%
  select(Category,log_or,conf.low,conf.high) %>%
  arrange(-log_or)
plot0<-

#gen1
x_gen1_2019<-x_gen1 %>% ungroup %>% slice(1:10)
x_gen1_2020<-x_gen1 %>% ungroup %>% slice(11:20)

x_gen1_2019 <-x_gen1_2019 %>% select(-year,-prop) %>% rename(n2019=n)
x_gen1_2020 <-x_gen1_2020 %>% select(-year,-prop) %>% rename(n2020=n)

x_gen1_or<- x_gen1_2019 %>% left_join(x_gen0_2020)

gen1_log_or<-x_gen1_or %>% mutate(log_or=log((n2019/(sum(n2019)-n2019))/
                                               (n2020/(sum(n2020)-n2020)))) %>%
  mutate(se=sqrt(1/n2019+1/(sum(n2019)-n2019)+
                   1/n2020+1/(sum(n2020)-n2020))) %>%
  mutate(conf.low=log_or-qnorm(0.975)*se,
         conf.high=log_or+qnorm(0.975)*se) %>%
  select(Category,log_or,conf.low,conf.high) %>%
  arrange(-log_or)
gen1_log_or


#plot
plot0<-gen0_log_or %>%
  mutate(Category=reorder(Category,log_or)) %>%
  ggplot(aes(x=Category, ymin=conf.low,ymax=conf.high))+
  geom_errorbar()+
  geom_point(aes(Category,log_or)) + coord_flip()+
  ylab('Log odds ratio where gender=0')


plot1<-gen1_log_or %>%
  mutate(Category=reorder(Category,log_or)) %>%
  ggplot(aes(x=Category, ymin=conf.low,ymax=conf.high))+
  geom_errorbar()+
  geom_point(aes(Category,log_or)) + coord_flip()+
  ylab('Log odds ratio where gender=1')

grid.arrange(plot0,plot1,nrow=2)








#by age
x %>% group_by(Age_Group) %>% summarize(n=n()) %>% mutate(prop=n/sum(n)) %>% arrange(-prop)


x %>% group_by(year,Category,Age_Group) %>% 
  summarize(n=n()) %>% mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=as.character(year),y=prop,col=Age_Group,
             group=Category))+geom_point()+
  geom_line(aes(group=Age_Group),size=1)+facet_wrap(.~Category)


