rm(list=ls(all=TRUE))
#install.packages("haven")
library(haven)
library(ggplot2)
library(dplyr)
library(plm)
guns = read_dta("guns.dta")
guns1 = read.csv("3years.csv")
colnames(guns1)[1]="year"
guns
summary(guns)
?ggplot
?dplyr
#data grouped by year
a1 <- guns %>% select(year,vio,mur,rob,incarc_rate,pb1064,pw1064,pm1029,pop,avginc,density,stateid,shall) %>% arrange(year)
#average of different variables grouped by year
a2 <- a1 %>% group_by(year) %>% summarise(avg_vio=mean(vio),avg_mur=mean(mur),avg_rob=mean(rob),avg_incarc_rate=mean(incarc_rate),avg_pb1064=mean(pb1064), avg_pw1064=mean(pw1064),avg_pm1029=mean(pm1029),avg_pop =mean(pop),avg_avginc=mean(avginc),avg_density=mean(density) )  #average mur,vio and rob by year
#average of different variables grouped by state
a2_1 <- a1 %>% group_by(stateid) %>% summarise(avg_vio=mean(vio),avg_mur=mean(mur),avg_rob=mean(rob),avg_incarc_rate=mean(incarc_rate),avg_pb1064=mean(pb1064), avg_pw1064=mean(pw1064),avg_pm1029=mean(pm1029),avg_pop =mean(pop),avg_avginc=mean(avginc),avg_density=mean(density) )
#data where shall=0
a21 <- a1 %>% filter(shall==0)
#data where shall=1
a22 <- a1 %>% filter(shall==1)
#average of variables where shall=0 grouped by year
a21 <- a21 %>% group_by(year) %>% summarise(avg_vio=mean(vio),avg_mur=mean(mur),avg_rob=mean(rob),avg_incarc_rate=mean(incarc_rate),avg_pb1064=mean(pb1064), avg_pw1064=mean(pw1064),avg_pm1029=mean(pm1029),avg_pop =mean(pop),avg_avginc=mean(avginc),avg_density=mean(density) ) #average mur,vio and rob by year before shall law
#average of varaibles where shall=1 grouped by state 
a22 <- a22 %>% group_by(year) %>% summarise(avg_vio=mean(vio),avg_mur=mean(mur),avg_rob=mean(rob),avg_incarc_rate=mean(incarc_rate),avg_pb1064=mean(pb1064), avg_pw1064=mean(pw1064),avg_pm1029=mean(pm1029),avg_pop =mean(pop),avg_avginc=mean(avginc),avg_density=mean(density) ) #average mur,vio and rob by year after shall law

?plot
plot(a2)
plot(a21)
plot(a22)

ggplot(data=a2, aes(x=year,y=avg_mur))+ geom_smooth()+labs(title="Average murder rate across years")
ggplot(data=a2, aes(x=year,y=avg_vio))+ geom_smooth()+labs(title="Average vio rate across years")
ggplot(data=a2, aes(x=year,y=avg_rob))+ geom_smooth()+labs(title="Average rob rate across years")

ggplot(data=a21, aes(x=year,y=avg_mur))+ geom_smooth()+labs(title="Average murder rate across years before shall law")
ggplot(data=a21, aes(x=year,y=avg_vio))+ geom_smooth()+labs(title="Average vio rate across years before shall law")
ggplot(data=a21, aes(x=year,y=avg_rob))+ geom_smooth()+labs(title="Average rob rate across years before shall law")

ggplot(data=a22, aes(x=year,y=avg_mur))+ geom_smooth()+labs(title="Average murder rate across years after shall law")
ggplot(data=a22, aes(x=year,y=avg_vio))+ geom_smooth()+labs(title="Average vio rate across years after shall law")
ggplot(data=a22, aes(x=year,y=avg_rob))+ geom_smooth()+labs(title="Average rob rate across years after shall law")


ggplot(data=a2, aes(x=avg_pb1064,y=avg_mur))+ geom_smooth()+labs(title="plot of  agv_pb1064 vs avg_murder rate across years")
ggplot(data=a2, aes(x=avg_pm1029,y=avg_mur))+ geom_smooth()+labs(title="plot of  agv_pm1029 vs avg_murder rate across years")
ggplot(data=a2, aes(x=avg_avginc,y=avg_mur))+ geom_smooth()+labs(title="plot of  agv_avginc vs avg_murder rate across years")
ggplot(data=a2, aes(x=avg_pop,y=avg_mur))+ geom_smooth()+labs(title="plot of  agv_pop vs avg_murder rate across years")

ggplot(data=a2, aes(x=avg_avginc,y=avg_incarc_rate))+ geom_smooth()+labs(title="plot of  agv_avginc vs avg_incarc rate across years")
ggplot(data=a2, aes(x=avg_avginc,y=avg_mur))+ geom_smooth()+labs(title="plot of  agv_avginc vs avg_mur rate across years")
ggplot(data=a2, aes(x=avg_avginc,y=avg_vio))+ geom_smooth()+labs(title="plot of  agv_avginc vs avg_vio rate across years")
ggplot(data=a2, aes(x=avg_avginc,y=avg_rob))+ geom_smooth()+labs(title="plot of  agv_avginc vs avg_rob rate across years")


#why dip at 85 for every variable?
#guns 1 has data across 3 years: the year before shall law is introduced, the year it was introduced and the year after it was introduced grouped by states
head(guns1)
summary(guns1)
guns1_stateid <- unique(guns1$stateid)
par(mfrow=c(4,3))
#plot of mur rate vs year for 3 years across states
for(i in guns1_stateid[1:24]){
  temp <- guns1 %>% filter(stateid==i)
  print(plot(temp$year,temp$mur, main=i))
}

#plot of vio rate vs year for 3 years across states
for(i in guns1_stateid[1:24]){
  temp <- guns1 %>% filter(stateid==i)
  print(plot(temp$year,temp$vio, main=i))
}

#plot of  rate vs year for 3 years across states
for(i in guns1_stateid[1:24]){rob
  temp <- guns1 %>% filter(stateid==i)
  print(plot(temp$year,temp$rob, main=i))
}




############################models###########################
guns    <- plm.data(guns,index=c("stateid","year"))


model12 <- plm(log(vio)~shall,model = "pooling",data = guns)
summary(model12)


model1      <- plm(log(vio)~shall+log(pb1064)+pw1064+pm1029+pop+avginc+log(density),model="pooling",data=guns)
summary(model1)
summary(model1, vcov=vcovHC(model1, method = "white1"))

model2      <- plm(log(vio)~shall+log(pb1064)+pw1064+pm1029+pop+avginc+log(density),model="within",data=guns)
summary(model2)
summary(model2, vcov=vcovHC(model2, method = "white1"))

model3      <- plm(log(vio)~shall+log(pb1064)+pw1064+pm1029+pop+avginc+log(density),model="between",data=guns)
summary(model3)


phtest(model2,model3)


##### Robbery
model5      <- plm(log(rob)~shall+log(pb1064)+pw1064+pm1029+pop+avginc+log(density),model="pooling",data=guns)
summary(model5)
summary(model5, vcov=vcovHC(model5, method = "white1"))

model6      <- plm(log(rob)~shall+log(pb1064)+pw1064+pm1029+pop+avginc+log(density),model="within",data=guns)
summary(model6)
summary(model6, vcov=vcovHC(model6, method = "white1"))



##### Murder
model8     <- plm(log(mur)~shall+log(pb1064)+pw1064+pm1029+pop+avginc+log(density),model="pooling",data=guns)
summary(model8)
summary(model8, vcov=vcovHC(model8, method = "white1"))

model9      <- plm(log(mur)~shall+log(pb1064)+pw1064+pm1029+pop+avginc+log(density),model="within",data=guns)
summary(model9)
summary(model9, vcov=vcovHC(model9, method = "white1"))


