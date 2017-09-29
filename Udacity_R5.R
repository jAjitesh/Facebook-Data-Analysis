facebook<- read.csv("/Users/madhavsarma/Desktop/pseudo_facebook.csv")
facebook
head(facebook)
str(facebook)
table(facebook$gender)
library(ggplot2)


qplot(x=dob_day,data=facebook)+
scale_x_continuous(breaks=1:31)+
 facet_wrap(~dob_month,ncol = 3)

qplot(data=facebook,x=friend_count,xlim=c(0,1000))
#OR TEH BELOW METHOD CAN ALSO BE USED
qplot(x=friend_count,data=subset(facebook,!is.na(gender)),binwidth=25)+
  scale_x_continuous(limits=c(0,1000), breaks= seq(0,1000,50)) +
  facet_wrap(~gender)
#TO not see the NA, we remove all the coloums which has NA check above

table(facebook$gender)
by(facebook$friend_count,facebook$gender,summary)

qplot(data=facebook,x=tenure,binwidth=30, color=I("black"), fill= I("#099DD9"))
head(facebook)
#to get tenure in years
qplot(data=facebook,x=tenure/365,binwidth=0.1, color=I("black"), fill= I("#099DD9"))+
scale_x_continuous(limits=c(0,7),breaks=seq(1,7,1))
#TO label
qplot(data=facebook,x=tenure/365,binwidth=0.1, color=I("black"), fill= I("#099DD9"),
      xlab="tenure in years",ylab="number of fb users")+
  scale_x_continuous(limits=c(0,7),breaks=seq(1,7,1))

#Create histogram based on the age 
head(facebook)
qplot(data=facebook,x=age,color=I("black"),fill=I("#099DD9"),binwidth=1,
      xlab="age",ylab="no of fb users")+
  scale_x_continuous(breaks=seq(1,120,5))

#transforming data for better visuals
summary(facebook$friend_count)
summary(log10(facebook$friend_count+1))
summary(sqrt(facebook$friend_count))

p1=qplot(x=friend_count,data=facebook,binwidth=25,color=I("black"),fill=I("#099DD9"))+
  scale_x_continuous(limits=c(0,1000), breaks= seq(0,1000,50))
p2=qplot(x=log10(friend_count+1),data=facebook,binwidth=0.1,color=I("black"),fill=I("#099DD9"))+
  scale_x_continuous(limits=c(0,3.692), breaks= seq(0,4,0.5))
p3=qplot(x=sqrt(friend_count),data=facebook,binwidth=1,color=I("black"),fill=I("#099DD9"))+
  scale_x_continuous(limits=c(0,70.160), breaks= seq(0,71,5))
p3
#multiple plots
install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1,p2,p3,ncol=1)
#We can also do this using ggplots
p1<- ggplot(aes(x=friend_count),data=facebook)+geom_histogram()
p2<-p1+scale_x_log10()
p3<-p1+scale_x_sqrt()
grid.arrange(p1,p2,p3,ncol=1)

#Transformations and comparisions
ggplot(aes(x=friend_count, y = ..count../sum(..count..)),data=subset(facebook,!is.na(gender)))+
  geom_freqpoly(aes(color = gender), binwidth=10)+
  scale_x_continuous(limits=c(0,1000), breaks= seq(0,1000,50)+
                     xlab="Friend Count",
                    ylab="Percentage of users with that friend count"
                    
#FReqpolys exploration
head(facebook)
qplot(x=www_likes,data=subset(facebook,!is.na(gender)),geom="freqpoly",color=gender)+
scale_x_continuous()+
scale_x_log10()

by(facebook$www_likes,facebook$gender,sum)

#Doubt to explore
#ggplot(data = pf,aes(x=dob_day))+
#geom_bar()+
#scale_x_discrete(breaks = 1:31)+
#facet_wrap(~dob_month,ncol = 3)
 
  
 