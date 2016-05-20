levels(raw.data$Country)

#Transform into long form and break into separate measures
rd.long <- melt(raw.data,id.vars=c(1:9),measure.vars = c(10:15),variable.name = "measure",value.name = "value")
rd.long2 <- rd.long %>% separate(measure,into = c("measure","run"),sep = -2)
wt <- rd.long2 %>% filter(measure =="Weight")
ht <- rd.long2 %>% filter(measure == "Height")
muac <- rd.long2 %>% filter(measure == "MUAC")

# find bad measurements
bad.wt <- wt %>% filter(value < 1 | value > 35)
bad.wt$Training <- as.character(bad.wt$Training)
bad.wt$Training[bad.wt$Training=="Nigeria Northern States Nutrition Survey Jul11"]<-"Nigeria NNS Jul11"
bad.wt$Training[bad.wt$Training=="Nigeria Northern States Nutrition Survey Jul10"]<-"Nigeria NNS Jul10"
bad.wt$Training <- as.factor(bad.wt$Training)
bad.wt$Date <- as.character(bad.wt$Date)
bad.wt$Date[bad.wt$Date=="10sep2015"]<-"2015-09-10"
bad.wt$Date[bad.wt$Date=="11sep2015"]<-"2015-09-11"
bad.wt$Date[bad.wt$Date=="12sep2015"]<-"2015-09-12"
bad.wt$Date[bad.wt$Date=="15feb2014"]<-"2014-02-15"
bad.wt$Date[bad.wt$Date=="07feb2014"]<-"2014-02-07"
bad.wt$Date[bad.wt$Date=="18jul2011"]<-"2011-07-18"
bad.wt$Date[bad.wt$Date=="10jul2010"]<-"2010-07-10"
bad.wtCount <- count_(bad.wt,c("Country", "Date"))
bad.wtCount <- arrange(bad.wtCount,Country,Date)

bad.ht <- ht %>% filter(value <40 | value >135)
bad.ht$Training <- as.character(bad.ht$Training)
bad.ht$Training[bad.ht$Training=="Nigeria Northern States Nutrition Survey Jul13"]<-"Nigeria NNS Jul13"
bad.ht$Training[bad.ht$Training=="Nigeria Northern States Nutrition Survey Feb12"]<-"Nigeria NNS Feb12"
bad.ht$Training[bad.ht$Training=="Nigeria Northern States Nutrition Survey Jul10"]<-"Nigeria NNS Jul10"
bad.ht$Training <- as.factor(bad.ht$Training)
bad.htCount <- count_(bad.ht,c("Country", "Date"))
bad.htCount$Date <- as.character(bad.htCount$Date)
bad.htCount$Date[bad.htCount$Date=="13sep2015"]<-"2015-09-13"
bad.htCount$Date[bad.htCount$Date=="12sep2015"]<-"2015-09-12"
bad.htCount$Date[bad.htCount$Date=="11sep2015"]<-"2015-09-11"
bad.htCount$Date[bad.htCount$Date=="10sep2015"]<-"2015-09-10"
bad.htCount$Date[bad.htCount$Date=="22oct2015"]<-"2015-10-22"
bad.htCount$Date[bad.htCount$Date=="15oct2015"]<-"2015-10-15"
bad.htCount$Date[bad.htCount$Date=="07feb2014"]<-"2014-02-07"
bad.htCount$Date[bad.htCount$Date=="15feb2014"]<-"2014-02-15"
bad.htCount$Date[bad.htCount$Date=="17feb2014"]<-"2014-02-17"
bad.htCount$Date[bad.htCount$Date=="07jul2013"]<-"2013-07-07"
bad.htCount$Date[bad.htCount$Date=="08jul2013"]<-"2013-07-08"
bad.htCount$Date[bad.htCount$Date=="07jul2013"]<-"2013-07-07"
bad.htCount$Date[bad.htCount$Date=="22feb2012"]<-"2012-02-22"
bad.htCount$Date[bad.htCount$Date=="24feb2012"]<-"2012-02-24"
bad.htCount$Date[bad.htCount$Date=="10jul2010"]<-"2010-07-10"
bad.htCount$Date[bad.htCount$Date=="11jul2010"]<-"2010-07-11"
bad.htCount <- arrange(bad.htCount,Country,Date)

bad.muac <- muac %>% filter(value < 80 | value > 235)
bad.muac$Training <- as.character(bad.muac$Training)
bad.muac$Training[bad.muac$Training=="Nigeria Northern States Nutrition Survey Feb12"]<-"Nigeria NNS Feb12"
bad.muac$Training[bad.muac$Training=="Nigeria Northern States Nutrition Survey Jul10"]<-"Nigeria NNS Jul10"
bad.muac$Training[bad.muac$Training=="Nigeria Northern States Nutrition Survey Jul13"]<-"Nigeria NNS Jul13"
bad.muac$Training <- as.factor(bad.muac$Training)
bad.muacCount <- count_(bad.muac,c("Country", "Date"))
bad.muacCount$Date <- as.character(bad.muacCount$Date)
bad.muacCount$Date[bad.muacCount$Date=="10jul2010"] <- "2010-07-10"
bad.muacCount$Date[bad.muacCount$Date=="22feb2012"] <- "2012-02-22"
bad.muacCount$Date[bad.muacCount$Date=="08jul2013"] <- "2013-07-08"
bad.muacCount$Date[bad.muacCount$Date=="15feb2014"] <- "2014-02-15"
bad.muacCount$Date[bad.muacCount$Date=="17feb2014"] <- "2014-02-17"
bad.muacCount$Date[bad.muacCount$Date=="19dec2014"] <- "2014-12-19"
bad.muacCount$Date[bad.muacCount$Date=="15feb2014"] <- "2014-02-15"
bad.muacCount$Date[bad.muacCount$Date=="11sep2015"] <- "2015-09-11"
bad.muacCount$Date[bad.muacCount$Date=="12sep2015"] <- "2015-09-12"
bad.muacCount$Date[bad.muacCount$Date=="13sep2015"] <- "2015-09-13"

#Find Good Measurements
good.wt <- wt %>% filter(value >=1 | value <= 35)
good.ht <- ht %>% filter(value >= 40| value <=135)
good.muac <- muac %>% filter(value >= 80 | value <= 235)


write.csv(wt, "Weight.csv", row.names=FALSE)
write.csv(ht, "Height.csv", row.names=FALSE)
write.csv(muac, "muac.csv", row.names=FALSE)
write.csv(bad.ht, "BadHt.csv", row.names=FALSE)
write.csv(good.ht, "GoodHT.csv", row.names=FALSE)
write.csv(bad.wt, "BadWt.csv", row.names=FALSE)
write.csv(good.wt, "GoodWt.csv", row.names=FALSE)
write.csv(bad.muac, "BadMuac.csv", row.names=FALSE)
write.csv(good.muac, "GoodMuac.csv", row.names=FALSE)



#Break data sets by country
#Senegal
sen <- filter(rd.long,Country=="SEN")
sen.ht <- filter(good.ht,Country=="SEN")
  senHt1 <- filter(sen.ht,Session==1)
  senHt2 <- filter(sen.ht,Session==2)
  senHt3 <- filter(sen.ht,Session==3)
  senHt4 <- filter(sen.ht,Session==4)
  senHt5 <- filter(sen.ht,Session==5)
  senHt6 <- filter(sen.ht,Session==6)
  
sen.wt <- filter(good.wt,Country=="SEN")
  senWt1 <- filter(sen.wt,Session==1)
  senWt2 <- filter(sen.wt,Session==2)
  senWt3 <- filter(sen.wt,Session==3)
  senWt4 <- filter(sen.wt,Session==4)
  senWt5 <- filter(sen.wt,Session==5)
  senWt6 <- filter(sen.wt,Session==6)
  
sen.muac <- filter(good.muac,Country=="SEN")
  senMuac1 <- filter(sen.muac,Session==1)
  senMuac2 <- filter(sen.muac,Session==2)
  senMuac3 <- filter(sen.muac,Session==3)
  senMuac4 <- filter(sen.muac,Session==4)
  senMuac5 <- filter(sen.muac,Session==5)
  senMuac6 <- filter(sen.muac,Session==6)

#chad
  chad <- filter(rd.long,Country=="TCD")
  chad.ht <- filter(good.ht,Country=="TCD")
  chadHt1 <- filter(chad.ht,Session==1)
  chadHt2 <- filter(chad.ht,Session==2)
  chadHt3 <- filter(chad.ht,Session==3)
  chadHt4 <- filter(chad.ht,Session==4)
  
  chad.wt <- filter(good.wt,Country=="TCD")
  chadWt1 <- filter(chad.wt,Session==1)
  chadWt2 <- filter(chad.wt,Session==2)
  chadWt3 <- filter(chad.wt,Session==3)
  chadWt4 <- filter(chad.wt,Session==4)
  
  chad.muac <- filter(good.muac,Country=="TCD")
  chadMuac1 <- filter(chad.muac,Session==1)
  chadMuac2 <- filter(chad.muac,Session==2)
  chadMuac3 <- filter(chad.muac,Session==3)
  chadMuac4 <- filter(chad.muac,Session==4)
  
#Congo
congo <- filter(raw.data,Country=="DRC")
congo$Session <- as.factor(congo$Session)
levels(congo$Session)
congo.weight<- congo %>% 
               select(Country:Weight2) %>% 
               gather(measure,Value,c(10:11))
write.csv(congo.weight, "CongoWeight.csv", row.names=FALSE)

congo.height<- congo %>%
               select(Country:AgeinMonths,Height1:Height2)%>%
               gather(measure,value,c(10:11))
write.csv(congo.height, "CongoHeight.csv", row.names=FALSE)

congo.MUAC <- congo %>%
              select(Country:AgeinMonths,MUAC1:MUAC2)%>%
              gather(measure,value,c(10:11))
write.csv(congo.MUAC, "CongoMUAC.csv", row.names=FALSE)

#Mauritania
mrt <- filter(rd.long,Country=="MRT")

mrt.ht <- filter(good.ht,Country=="MRT")
mrtHt1 <- filter(mrt.ht,Session=="1")
mrtHt2 <- filter(mrt.ht,Session=="2")
mrtHt3 <- filter(mrt.ht,Session=="3")

mrt.wt <-filter(good.wt,Country=="MRT")
mrtWt1 <- filter(mrt.wt,Session=="1")
mrtWt2 <- filter(mrt.wt,Session=="2")
mrtWt3 <- filter(mrt.wt,Session=="3")

mrt.muac <- filter(good.muac,Country=="MRT")
mrtMuac1 <- filter(mrt.muac,Session=="1")
mrtMuac2 <- filter(mrt.muac,Session=="2")
mrtMuac3 <- filter(mrt.muac,Session=="3")


#Niger
niger <- filter(rd.long,Country=="NER")

#Niger Height data
ner.ht <- filter(good.ht,Country == "NER")
ner.ht$Date<-as.character(ner.ht$Date)
nerHt2014<-filter(ner.ht,Date=="19dec2014")
nerHt2014s1 <-filter(nerHt2014,Session =="1")
nerHt2014S2 <- filter(nerHt2014, Session =="2")
nerHt2015 <- filter(ner.ht,Date =="02sep2015")
nerHt2015s1 <-filter(nerHt2015,Session =="1")
nerHt2015s2 <- filter(nerHt2015, Session =="2")
#Niger Weight Data
ner.wt <- filter(good.wt,Country == "NER")
ner.wt$Date<-as.character(ner.wt$Date)
nerWt2014<-filter(ner.wt,Date=="19dec2014")
nerWt2014s1 <-filter(nerWt2014,Session =="1")
nerWt2014S2 <- filter(nerWt2014, Session =="2")
nerWt2015 <- filter(ner.wt,Date =="02sep2015")
nerWt2015s1 <-filter(nerWt2015,Session =="1")
nerWt2015s2 <- filter(nerWt2015, Session =="2")

#Niger MUAC Data
ner.muac <- filter(good.muac,Country == "NER")
ner.muac$Date<-as.character(ner.muac$Date)
nerMuac2014<-filter(ner.muac,Date=="19dec2014")
nerMuac2014s1 <-filter(nerMuac2014,Session =="1")
nerMuac2014S2 <- filter(nerMuac2014, Session =="2")
nerMuac2015 <- filter(ner.muac,Date =="02sep2015")
nerMuac2015s1 <-filter(nerMuac2015,Session =="1")
nerMuac2015s2 <- filter(nerMuac2015, Session =="2")


niger1 <- filter(niger,Date=="19dec2014")
niger2 <- filter(niger,Date=="02sep2015")

#Nigeria
nga <- filter(raw.data,Country=="NGA")

ngaHt <- filter(good.ht,Country=="NGA")
ngaHt20140207<-filter(ngaHt,Date=="07feb2014")
ngaHt20140215 <- filter(ngaHt,Date == "15feb2014")
ngaHt20140217 <- filter(ngaHt,Date == "17feb2014")
ngaHt20130707 <- filter(ngaHt,Date =="07jul2013")
ngaHt20130708 <- filter(ngaHt,Date =="08jul2013")
ngaHt20130710 <- filter(ngaHt,Date =="10jul2013")
ngaHt20140207<-filter(ngaHt,Date=="07feb2014")

ngaWt <-filter(good.wt,Country =="NGA")
ngaMuac <- filter(good.muac,Country =="NGA")

