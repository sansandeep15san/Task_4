# THE SPARKS FOUNDATION
# GRIP JULY 2022
# SANDEEP KUMAR SINGH
# DATA SCIENCE AND BUSINESS ANALYTICS interaction
# TASK 4 : EXPLANATORY DATA ANALYSIS ON GLOBAL TERRORISM
# LEVEL : INTERMEDIATE
----------------------------------------------------------------------
  install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")

library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

globalterror <- read.csv("C:/Users/sansa/OneDrive/Desktop/GRIP/globalterrorismdb_0718dist.csv")
terror <- globalterror %>% select(eventid,iyear,country_txt,region_txt,city,
                                  latitude,longitude,attacktype1_txt,targtype1_txt,
                                  weaptype1_txt,gname,nkill,nwound,nkillter,propvalue)
names(terror) <- c("eventid","year","country","region","city","latitude","longitude",
                   "attacktype","target","weapon","group","killed",
                   "wounded","killedtr","loss")

colSums(is.na(terror))
terror$killed[is.na(terror$killed)] <- 0
terror$wounded [is.na(terror$wounded)] <- 0
terror$killedtr [is.na(terror$killedtr)] <- 0
terror$loss [is.na(terror$loss)] <- 0
terror$casuality <- terror$killed+terror$wounded
head(terror)

# Country wise distribution
terror1 <- terror %>% group_by(country) %>% summarise(attacks = n())
terror1 <-terror1[rev(order(terror1$attacks)),]
terror1 <- head(terror1,10)
country <- terror1$country
attacks <- terror1$attacks
ggplot()+ geom_bar(aes(x=reorder(country,-attacks),y=attacks,fill=country), stat="identity")+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  xlab("")+ylab("Number of Terror Attacks")  


# Plotting on world Map (NO OF ATTACKS)

m=map_data("world")
names(m) <- c("long","lat","group","order","country","subrigeon")
m= left_join(m,terror1,by="country")
map1= ggplot(m,aes(x=long,y= lat,group=group))+
  geom_polygon(aes(fill=attacks),colour="black")+scale_fill_gradient(name="attacks",low="yellow",
                                                                     high="darkred",na.value = "white")
plot(map1) 

#COUNTRIES WITH MOST LOSSES BORNE
terror2 <- terror %>% group_by(country) %>% summarise(lossborne=sum(loss/1000000))
terror2 <-terror2[rev(order(terror2$lossborne)),]
country <- terror2$country
terror2 <-head (terror2,10)
lossborne <- terror2$lossborne
ggplot()+ geom_bar(aes(x=reorder(country,-lossborne),y=lossborne,fill=country), stat="identity")+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  xlab("")+ylab("Loss (in mill. $)")  

#COUNTRIES WITH MOST CASUALITIES(DEATH +WOUNDED)

terror2 <- terror %>% group_by(country) %>% summarise(casuality=sum(casuality))
terror2 <- terror2[rev(order(terror2$casuality)),]
country <- terror2$country
terror2 <-head (terror2,10)
casuality <- terror2$casuality
ggplot()+ geom_bar(aes(x=reorder(country,-casuality),y=casuality,fill=country), stat="identity")+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  xlab("")+ylab("Death + Wounded")

---------------------------------------------------------------------
  
  # REGION WISE ATTACKS
  
  terror1 <- terror %>% group_by(region) %>% summarise(attacks = n())
terror1 <-terror1[rev(order(terror1$attacks)),]
terror1 <- head(terror1,10)
region <- terror1$region
attacks <- terror1$attacks
ggplot()+ geom_bar(aes(x=reorder(region,-attacks),y=attacks,fill=region), stat="identity")+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  xlab("Regions")+ylab("Number of Terror Attacks")  

#REGIONS WITH MOST LOSSES BORNE
terror1 <- terror %>% group_by(region) %>% summarise(loss=sum(loss/1000000))
terror1 <-terror1[rev(order(terror1$loss)),]
terror1 <- head(terror1,10)
region <- terror1$region
loss <- terror1$loss
ggplot()+ geom_bar(aes(x=reorder(region,-loss),y=loss,fill=region), stat="identity")+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  xlab("Regions")+ylab("Loss (in mill.$)")  


#REGIONS WITH MOST CASUALITIES(DEATH +WOUNDED)
terror3 <- terror %>% group_by(region) %>% summarise(casuality=sum(casuality/1000))
terror3 <-terror3[rev(order(terror3$casuality)),]
terror3 <- head(terror3,10)
region <- terror3$region
casuality <- terror3$casuality
ggplot()+ geom_bar(aes(x=reorder(region,-casuality),y=casuality,fill=region), stat="identity")+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  xlab("Regions")+ylab("Deaths (in 1000)") 


# MOST ATTACKED CITIES
terror1 <- terror %>% group_by(city) %>% summarise(attacks = n())
terror1 <-terror1[rev(order(terror1$attacks)),]
terror1 <-terror1[-c(1),]
terror1 <- head(terror1,10)
city <- terror1$city
attacks <- terror1$attacks
ggplot()+ geom_bar(aes(x=reorder(city,-attacks),y=attacks,fill=city), stat="identity")+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  xlab("Cities")+ylab("Number of Terror Attacks")  

# MOST ACTIVE TERROR GROUPS
terror1 <- terror %>% group_by(group) %>% summarise(attacks = n())
terror1 <-terror1[rev(order(terror1$attacks)),]
terror1 <-terror1[-c(1),]
terror1 <- head(terror1,10)
group <- terror1$group
attacks <- terror1$attacks
ggplot()+ geom_bar(aes(x=reorder(group,-attacks),y=attacks,fill=group), stat="identity")+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  xlab("")+ylab("Number of Terror Attacks")  

# MOST DEADLY GROUPS
terror1 <- terror %>% group_by(group) %>% summarise(casuality=sum(casuality))
terror1 <-terror1[rev(order(terror1$casuality)),]
terror1 <-terror1[-c(1),]
terror1 <- head(terror1,10)
group <- terror1$group
casuality <- terror1$casuality
ggplot()+ geom_bar(aes(x=reorder(group,-casuality),y=casuality,fill=group), stat="identity")+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  xlab("")+ylab("Number of Terror Attacks")

# TYPES OF ATTACKS
terror1 <- terror %>% group_by(attacktype) %>% summarise(attacks = n())
terror1 <-terror1[rev(order(terror1$attacks)),]
attacktype <- terror1$attacktype
attacks <- terror1$attacks
ggplot()+ geom_bar(aes(x=reorder(attacktype,-attacks),y=attacks,fill=attacktype), stat="identity")+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  xlab("Type of Attack")+ylab("Number of Terror Attacks")  


# TARGETS OF TERROR ATTACKS
terror1 <- terror %>% group_by(target) %>% summarise(attacks = n())
terror1 <-terror1[rev(order(terror1$attacks)),]
#terror1 <-terror1[-c(1),]
terror1 <- head(terror1,10)
target <- terror1$target
attacks <- terror1$attacks
ggplot()+ geom_bar(aes(x=reorder(target,-attacks),y=attacks,fill=target), stat="identity")+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  xlab("Victims")+ylab("Number of Terror Attacks") 

# YEARLY TREND OF TERROR ATTACKS
terror1 <- terror %>% group_by(year) %>% summarise(attacks = n())
year <- terror1$year
attacks <- terror1$attacks
ggplot()+ geom_line(aes(x=year,y=attacks), stat="identity")+
  xlab("year")+ylab("Number of Terror Attacks")+
  geom_point(aes(x=year,y=attacks),stat="identity")
#=============xxxxxxx============#

#    CASE STUDY   #

# YEARLY TREND OF  ATTACKS IN INDIA,UK, USA 
indter <-filter(terror,terror[,"country"]=="India",preserve=TRUE)
indter <- indter %>% group_by(year) %>% summarise(attacks = n())
indyear <- indter$year
indattacks <- indter$attacks
uster <- filter (terror,terror[,"country"]=="United States",preserve=TRUE)
uster <- uster %>% group_by(year) %>% summarise(attacks = n())
usyear <- uster$year
usattacks <- uster$attacks
ukter <- filter (terror,terror[,"country"]=="United Kingdom",preserve=TRUE)
ukter <- ukter %>% group_by(year) %>% summarise(attacks = n())
ukyear <- ukter$year
ukattacks <- ukter$attacks


plot(indyear,indattacks,type="l",xlab="Years",ylab="Attacks",col="blue")
lines(usyear,usattacks)
lines(ukyear,ukattacks,col="orange")
legend("topleft",legend=c("INDIA","USA","UK"),
       col=c("blue","black","orange"),lty=1,cex=0.5,
       horiz=T)



