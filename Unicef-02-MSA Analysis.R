library(SixSigma)
library(ggplot2)

ggplot(wt, aes(x=value)) +
  geom_histogram(binwidth = 0.3, colour="black", fill="white")
ggplot(wt,aes(sample = value))+stat_qq()
gstem(wt$value)

ggplot(ht,aes(sample = value))+stat_qq()
ggplot(ht, aes(x=value)) +
  geom_histogram(binwidth = 0.3, colour="black", fill="white")


qqnorm(congo.weight$Value)
qqline(congo.weight$Value)

ggplot(bad.htCount,aes(x=Date, y=n))+geom_bar(position ="dodge",stat="identity")+
  theme_bw()+
  theme(strip.text.y = element_text(angle=0))+ 
  labs(x="Date", y = "Number of Out of Spec Heights")+
  facet_grid(Country~.,scales = "free_x")+coord_flip()+scale_y_continuous(breaks=seq(0,12,1))

ggplot(bad.wtCount,aes(x=Date, y=n))+geom_bar(position ="dodge",stat="identity")+
  theme_bw()+
  theme(strip.text.y = element_text(angle=0))+ 
  labs(x="Date", y = "Number of Out of Spec Weights")+
  facet_grid(Country~.,scales = "free_x")+coord_flip()

ggplot(bad.muacCount,aes(x=Date, y=n))+geom_bar(position ="dodge",stat="identity")+
  theme_bw()+
  theme(strip.text.y = element_text(angle=0))+ 
  labs(x="Date", y = "Number of Out of Spec MUAC")+
  facet_grid(Country~.,scales = "free_x")+coord_flip()

#Senegal data MSA
#Height
sink('Senegal_Height_MSA.txt')
cat("Senegal Session 1\n")
ss.rr(value,ChildNumber,Position,data=senHt1,main="Senegal Session 1 Height MSA Study")
cat("Senegal Session 2\n")
ss.rr(value,ChildNumber,Position,data=senHt2,main="Senegal Session 2 Height MSA Study")
cat("Senegal Session 3\n")
ss.rr(value,ChildNumber,Position,data=senHt3,main="Senegal Session 3 Height MSA Study")
cat("Senegal Session 4\n")
ss.rr(value,ChildNumber,Position,data=senHt4,main="Senegal Session 4 Height MSA Study")
cat("Senegal Session 5\n")
ss.rr(value,ChildNumber,Position,data=senHt5,main="Senegal Session 5 Height MSA Study")
cat("Senegal Session 6\n")
ss.rr(value,ChildNumber,Position,data=senHt6,main="Senegal Session 6 Height MSA Study")
sink()

#Weight
sink('Senegal_Weight_MSA.txt')
cat("Senegal Session 1\n")
ss.rr(value,ChildNumber,Position,data=senWt1,main="Senegal Session 1 Weight MSA Study")
cat("Senegal Session 2\n")
ss.rr(value,ChildNumber,Position,data=senWt2,main="Senegal Session 2 Weight MSA Study")
cat("Senegal Session 3\n")
ss.rr(value,ChildNumber,Position,data=senWt3,main="Senegal Session 3 Weight MSA Study")
cat("Senegal Session 4\n")
ss.rr(value,ChildNumber,Position,data=senWt4,main="Senegal Session 4 Weight MSA Study")
cat("Senegal Session 5\n")
ss.rr(value,ChildNumber,Position,data=senWt5,main="Senegal Session 5 Weight MSA Study")
cat("Senegal Session 6\n")
ss.rr(value,ChildNumber,Position,data=senWt6,main="Senegal Session 6 Weight MSA Study")
sink()

#Muac
sink('Senegal_MUAC_MSA.txt')
cat("Senegal Session 1\n")
ss.rr(value,ChildNumber,Position,data=senMuac1,main="Senegal Session 1 MUAC MSA Study")
cat("Senegal Session 2\n")
ss.rr(value,ChildNumber,Position,data=senMuac2,main="Senegal Session 2 MUAC MSA Study")
cat("Senegal Session 3\n")
ss.rr(value,ChildNumber,Position,data=senMuac3,main="Senegal Session 3 MUAC MSA Study")
cat("Senegal Session 4\n")
ss.rr(value,ChildNumber,Position,data=senMuac4,main="Senegal Session 4 MUAC MSA Study")
cat("Senegal Session 5\n")
ss.rr(value,ChildNumber,Position,data=senMuac5,main="Senegal Session 5 MUAC MSA Study")
cat("Senegal Session 6\n")
ss.rr(value,ChildNumber,Position,data=senMuac6,main="Senegal Session 6 MUAC MSA Study")
sink()

# Congo data MSA
sink('Congo_Weight_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data=congo.weight,main="Congo Weight MSA Study")
sink()

sink('Congo_Height_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data=congo.height,main="Congo height MSA Study")
sink()

sink('Congo_MUAC_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data=congo.MUAC,main="Congo MUAC MSA Study")
sink()

# Mauritania data MSA
sink('Mauritania_S1_Weight_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data=mrtWt1,main="Mauritania Session 1 Weight MSA Study")
sink()

sink('Mauritania_21_Weight_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data=mrtWt2,main="Mauritania Session 2 Weight MSA Study")
sink()

sink('Mauritania_S3_Weight_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data=mrtWt3,main="Mauritania Session 3 Weight MSA Study")
sink()

sink('Mauritania_S1_Height_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data=mrtHt1,main="Mauritania Session 1 Height MSA Study")
sink()

sink('Mauritania_S2_Height_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data=mrtHt2,main="Mauritania Session 2 Height MSA Study")
sink()

sink('Mauritania_S3_Height_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data=mrtHt3,main="Mauritania Session 3 Height MSA Study")
sink()

sink('Mauritania_S1_Muac_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data=mrtMuac1,main="Mauritania Session 1 MUAC MSA Study")
sink()

sink('Mauritania_S2_Muac_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data=mrtMuac2,main="Mauritania Session 2 MUAC MSA Study")
sink()

sink('Mauritania_S3_Height_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data=mrtMuac3,main="Mauritania Session 3 MUAC MSA Study")
sink()

#Niger Data MSA
sink('Niger_2014S1_Height_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data= nerHt2014s1,main="Niger 2014 Session 1 Height MSA Study")
sink()

sink('Niger_2014S2_Height_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data= nerHt2014S2,main="Niger 2014 Session 2 Height MSA Study")
sink()

sink('Niger_2015s1_Height_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data= nerHt2015s1,main="Niger 2015 Session 1 Height MSA Study")
sink()

sink('Niger_2015s2_Height_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data= nerHt2015s2,main="Niger 2015 Session 2 Height MSA Study")
sink()


sink('Niger_2014S1_Weight_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data= nerWt2014s1,main="Niger 2014 Session 1 Weight MSA Study")
sink()

sink('Niger_2014S2_Weight_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data= nerWt2014S2,main="Niger 2014 Session 2 Weight MSA Study")
sink()

sink('Niger_2015s1_Weight_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data= nerWt2015s1,main="Niger 2015 Session 1 Weight MSA Study")
sink()

sink('Niger_2015s2_Weight_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data= nerWt2015s2,main="Niger 2015 Session 2 Weight MSA Study")
sink()

sink('Niger_2014S1_MUAC_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data= nerMuac2014s1,main="Niger 2014 Session 1 MUAC MSA Study")
sink()

sink('Niger_2014S2_MUAC_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data= nerMuac2014S2,main="Niger 2014 Session 2 MUAC MSA Study")
sink()

sink('Niger_2015s1_MUAC_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data= nerMuac2015s1,main="Niger 2015 Session 1 MUAC MSA Study")
sink()

sink('Niger_2015s2_MUAC_MSA.txt\n')
ss.rr(value,ChildNumber,Position,data= nerMuac2015s2,main="Niger 2015 Session 2 MuAC MSA Study")
sink()

