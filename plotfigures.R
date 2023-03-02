# plot figures 1, 2 and 3 power law scaling paper
library(ggplot2)

### OLD
dat<- read.csv('C:/Users/jwest/OneDrive - Bureau of Meteorology/Documents/Research/Papers/Power Law Scaling/fig1.csv')
str(dat)

ggplot(data=dat,aes(x=Year, y=kcal, linetype=Type, shape=Type))+
  geom_line(aes(colour=Source)) +
  geom_point(size=2, aes(colour=Source))

### NEW
dat<- read.csv('C:/Users/jwest/OneDrive - Bureau of Meteorology/Documents/Research/Papers/Power Law Scaling/fig1a.csv')
str(dat)
dat$Source_f<-factor(dat$Source, levels = c("Animal","Vegetable","Total"))

ggplot(data=dat,aes(x=Year, y=kcal, linetype=Type, shape=Type))+
  geom_line(aes(colour=Source)) +
  geom_point(size=2.5, aes(colour=Source)) +
  theme(text = element_text(size=11),
        axis.text.x = element_text(angle=45, hjust=1)) +
  facet_grid(.~Source_f)

ggplot(data=dat,aes(x=Year, y=kcal, linetype=Type, shape=Type))+
  geom_line(size=0.58,aes(colour=Source)) +
  geom_point(size=2,aes(colour=Source)) +
  theme(text = element_text(size=11),
        axis.text.x = element_text(angle=45, hjust=1)) +
  facet_grid(.~Source_f) + 
  scale_colour_discrete(name  ="Type",
                        breaks=c("Developing countries","Industrialized countries","Transition countries"),
                        labels=c("Developing countries","Industrialized countries","Transition countries")) +
  scale_shape_discrete(name  ="Type",
                       breaks=c("Developing countries","Industrialized countries","Transition countries"),
                       labels=c("Developing countries","Industrialized countries","Transition countries")) +
  ylab("Consumption (kcal per person pa)")



### KG CONSUMPTION CHINA URBAN V RURAL
dat<- read.csv('C:/Users/jwest/OneDrive - Bureau of Meteorology/Documents/Research/Papers/Power Law Scaling/fig2.csv')
str(dat)
dat$Location_f<-factor(dat$Location, levels = c("Rural","Urban"))
dat$Year<-as.integer(dat$Year)

ggplot(data=dat,aes(x=Year, y=Consumption, fill=Source))+
  geom_bar(stat="identity") +
  scale_fill_brewer() +
  facet_grid(.~Location_f) +
  scale_x_continuous(breaks= pretty_breaks()) +
  ylab("Consumption (kg per person pa)")



### FORECASTS
library(scales)
dat<- read.csv('C:/Users/jwest/OneDrive - Bureau of Meteorology/Documents/Research/Papers/Power Law Scaling/figFinal.csv')
str(dat)
dat$Source_f<-factor(dat$Source, levels = c("Animal","Vegetable","Total"))

ggplot(data=dat,aes(x=Year, y=kcal, linetype=Source, shape=Source))+
  geom_line(aes(colour=Source)) +
  geom_point(size=2.5, aes(colour=Source)) +
  theme(text = element_text(size=11),
        axis.text.x = element_text(angle=45, hjust=1))
