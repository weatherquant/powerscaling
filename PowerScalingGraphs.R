## Graphs for China Food paper

library(data.table)
library(ggplot2)
library(reshape2)
library(ggrepel)

# CHINA
pop<-read.csv('C:/Users/jwest/OneDrive - Bureau of Meteorology/Documents/Research/Papers/Power Law Scaling/China_Population.csv')
pop<-as.data.table(pop)
pop_num<-pop[,.(Year, Urban_Population/1000000, Rural_Population/1000000)]
setnames(pop_num,"V2","Urban")
setnames(pop_num,"V3","Rural")


pop_long <- melt(pop_num, id = "Year")
setnames(pop_long,"variable","Location")
pop_long<-as.data.table(pop_long)
pop_long<-pop_long[Year>1977,]
pop_plot <- ggplot(pop_long,aes(x = Year,y = value,group = Location)) +  
                       geom_line(aes(linetype=Location))+
                       scale_linetype_manual(values=c("solid", "longdash")) +
                       labs(title="Urban vs Rural Population - China 1978-2020",
                       x ="Year", y = "Population (mil)")

pop_plot

###################################################################################
# POPULATION GRAPHS
###################################################################################

pop_gr<-pop[,.(Year, Urban_Population_Growth, Rural_Population_Growth)]
setnames(pop_gr,"Urban_Population_Growth","Urban")
setnames(pop_gr,"Rural_Population_Growth","Rural")
popgr_long <- melt(pop_gr, id = "Year")
setnames(popgr_long,"variable","Location")
popgr_long<-as.data.table(popgr_long)
popgr_long<-popgr_long[Year>1977,]
yrs<-c("1978", "1983", "1987", "1992", "1997", "2002",
       "2007", "2012", "2017", "2020")

# Use position = position_dodge() 
popgr_plot <- ggplot(popgr_long, aes(x=as.factor(Year),y=value)) +
  geom_col(aes(color = Location, fill = Location), position = position_dodge(0.8), width = 0.7) +
  scale_color_manual(values = c("#555555", "#999999"))+
  scale_fill_manual(values = c("#555555", "#999999")) +
  labs(title="Urban vs Rural Population Growth Rates - China 1978-2020",
       x ="Year", y = "Population Growth (%)") + 
       scale_x_discrete(limits = yrs) +
       scale_y_continuous(limits = c(-7.5, 7.5)) +
       guides(x = guide_axis(angle = 90))

popgr_plot

###################################################################################
# FOOD CONSUMPTION GRAPHS
###################################################################################
food<-read.csv('G:/My Drive/Work Files/QAAFI/Papers/Power Law Scaling/ggplot_data.csv')
food<-as.data.table(food)

x1<-lm(Grain_Urban~Log_Pop_Urban, data=food)
x2<-lm(Grain_Rural~Log_Pop_Rural, data=food)
summary(x1)
summary(x2)
confint(x1)
confint(x2)

x3<-lm(Vegetables_Urban~Log_Pop_Urban, data=food)
x4<-lm(Vegetables_Rural~Log_Pop_Rural, data=food)
summary(x3)
summary(x4)
confint(x3)
confint(x4)

x5<-lm(Beef_Mutton_Urban~Log_Pop_Urban, data=food)
x6<-lm(Beef_Mutton_Rural~Log_Pop_Rural, data=food)
summary(x5)
summary(x6)
confint(x5)
confint(x6)

x7<-lm(Eggs_Urban~Log_Pop_Urban, data=food)
x8<-lm(Eggs_Rural~Log_Pop_Rural, data=food)
summary(x7)
summary(x8)
confint(x7)
confint(x8)

x9<-lm(Cotton_Wool_Urban~Log_Pop_Urban, data=food)
x10<-lm(Cotton_Wool_Rural~Log_Pop_Rural, data=food)
summary(x9)
summary(x10)
confint(x9)
confint(x10)

x11<-lm(Fish_Urban~Log_Pop_Urban, data=food)
x12<-lm(Fish_Rural~Log_Pop_Rural, data=food)
summary(x11)
summary(x12)
confint(x11)
confint(x12)

x13<-lm(Oils_Urban~Log_Pop_Urban, data=food)
x14<-lm(Oils_Rural~Log_Pop_Rural, data=food)
summary(x13)
summary(x14)
confint(x13)
confint(x14)


# Grain
p<-ggplot(food, aes(x=Log_Pop_Rural, y=Grain_Rural, label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=70,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  #geom_text(hjust=0.5, vjust=1.5, size=3)+
  geom_smooth(method=lm)+
  labs(title="Rural grain consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (rural population)", limits=c(20.0, 21.2)) +
  scale_y_continuous(name="Log (grain consumption)", limits=c(25.3, 26.2))+
  geom_text(x=20.75, y=25.7, label="Slope = 2.912 (95% CI [2.700, 3.123] \n R-squared 0.987")
p + theme_classic()  
  
q<-ggplot(food, aes(x=Log_Pop_Urban, y=Grain_Urban,label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=150,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  geom_smooth(method=lm)+
  labs(title="Urban grain consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (urban population)", limits=c(19.3, 20.5)) +
  scale_y_continuous(name="Log (grain consumption)", limits=c(24.0, 24.8))+
  geom_text(x=19.5, y=24.7, label="Slope = 0.439 (95% CI [0.274, 0.604]) \n	R-squared 0.802")
q + theme_classic()  


# Vegetables
p<-ggplot(food, aes(x=Log_Pop_Rural, y=Vegetables_Rural, label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=70,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  #geom_text(hjust=0.5, vjust=1.5, size=3)+
  geom_smooth(method=lm)+
  labs(title="Rural vegetable consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (rural population)", limits=c(20.0, 21.2)) +
  scale_y_continuous(name="Log (vegetable consumption)", limits=c(24.5, 25.5))+
  geom_text(x=20.75, y=25.7, label="Slope = 2.272	(95% CI [1.412, 3.018]) \n R-squared 0.770")
p + theme_classic()  

q<-ggplot(food, aes(x=Log_Pop_Urban, y=Vegetables_Urban,label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=150,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  geom_smooth(method=lm)+
  labs(title="Urban vegetable consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (urban population)", limits=c(19.3, 20.5)) +
  scale_y_continuous(name="Log (vegetable consumption)", limits=c(24.3, 25.0))+
  geom_text(x=19.5, y=24.7, label="Slope = 0.816 (95% CI [0.701, 0.931] \n	R-squared 0.966")
q + theme_classic()  

# Beef+Mutton
p<-ggplot(food, aes(x=Log_Pop_Rural, y=Beef_Mutton_Rural, label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=70,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  #geom_text(hjust=0.5, vjust=1.5, size=3)+
  geom_smooth(method=lm)+
  labs(title="Rural beef / mutton consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (rural population)", limits=c(20.0, 21.2)) +
  scale_y_continuous(name="Log (beef / mutton consumption)", limits=c(22.5, 24.0))+
  geom_text(x=20.75, y=23.5, label="Slope = 1.262 (95% CI [0.506, 3.031]) \n R-squared 0.183")
p + theme_classic()  

q<-ggplot(food, aes(x=Log_Pop_Urban, y=Beef_Mutton_Urban,label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=150,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  geom_smooth(method=lm)+
  labs(title="Urban beef / mutton consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (urban population)", limits=c(19.3, 20.5)) +
  scale_y_continuous(name="Log (beef / mutton consumption)", limits=c(22.3, 23.0))+
  geom_text(x=19.75, y=22.6, label="Slope = 1.474	(95% CI [1.341, 1.606])	\n R-squared 0.986")
q + theme_classic()  

# Eggs
p<-ggplot(food, aes(x=Log_Pop_Rural, y=Eggs_Rural, label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=70,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  #geom_text(hjust=0.5, vjust=1.5, size=3)+
  geom_smooth(method=lm)+
  labs(title="Rural egg consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (rural population)", limits=c(20.0, 21.2)) +
  scale_y_continuous(name="Log (egg consumption)", limits=c(21.4, 22.2))+
  geom_text(x=20.75, y=22.0, label="Slope = 1.716 (95% CI [0.487, 2.944])	\n R-squared 0.565")
p + theme_classic()  

q<-ggplot(food, aes(x=Log_Pop_Urban, y=Eggs_Urban,label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=150,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  geom_smooth(method=lm)+
  labs(title="Urban egg consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (urban population)", limits=c(19.3, 20.5)) +
  scale_y_continuous(name="Log (egg consumption)", limits=c(21.1, 23.0))+
  geom_text(x=19.75, y=22.6, label="Slope = 1.327	(95% CI [1.118, 1.535]) \n R-squared 0.958")
q + theme_classic()  

# Wool and Cotton
p<-ggplot(food, aes(x=Log_Pop_Rural, y=Cotton_Wool_Rural, label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=70,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  #geom_text(hjust=0.5, vjust=1.5, size=3)+
  geom_smooth(method=lm)+
  labs(title="Rural cotton / wool consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (rural population)", limits=c(19.0, 22.5)) +
  scale_y_continuous(name="Log (cotton / wool consumption)", limits=c(20.0, 22.2))+
  geom_text(x=19.75, y=21.0, label="Slope = 28.920 (95% CI [16.737, 41.103]) \n R-squared 0.849")
p + theme_classic()  

q<-ggplot(food, aes(x=Log_Pop_Urban, y=Cotton_Wool_Urban,label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=150,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  geom_smooth(method=lm)+
  labs(title="Urban cotton / wool consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (urban population)", limits=c(19.3, 20.5)) +
  scale_y_continuous(name="Log (cotton / wool consumption)", limits=c(18.8, 20.5))+
  geom_text(x=19.75, y=20.0, label="Slope = 0.562	(95% CI [-0.681, 1.804]) \n R-squared 0.654")
q + theme_classic()  


# Fish
p<-ggplot(food, aes(x=Log_Pop_Rural, y=Fish_Rural, label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=70,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  #geom_text(hjust=0.5, vjust=1.5, size=3)+
  geom_smooth(method=lm)+
  labs(title="Rural fish consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (rural population)", limits=c(19.0, 22.5)) +
  scale_y_continuous(name="Log (fish consumption)", limits=c(21.0, 22.0))+
  geom_text(x=19.75, y=21.6, label="Slope = 2.091 (95% CI	[0.741, 3.442]) \n R-squared 0.615")
p + theme_classic()  

q<-ggplot(food, aes(x=Log_Pop_Urban, y=Fish_Urban,label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=150,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  geom_smooth(method=lm)+
  labs(title="Urban fish consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (urban population)", limits=c(19.3, 20.5)) +
  scale_y_continuous(name="Log (fish consumption)", limits=c(21.2, 23.2))+
  geom_text(x=19.6, y=22.5, label="Slope = 1.808 (95% CI	[1.703, 1.913]) \n R-squared 0.994")
q + theme_classic()  


# Oils
p<-ggplot(food, aes(x=Log_Pop_Rural, y=Oils_Rural, label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=70,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  #geom_text(hjust=0.5, vjust=1.5, size=3)+
  geom_smooth(method=lm)+
  labs(title="Rural oils consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (rural population)", limits=c(19.0, 22.5)) +
  scale_y_continuous(name="Log (oils consumption)", limits=c(22.2, 22.4))+
  geom_text(x=19.6, y=22.3, label="Slope = 0.049 (95% CI [-0.705, 0.607]) \n R-squared 0.004")
p + theme_classic()  

q<-ggplot(food, aes(x=Log_Pop_Urban, y=Oils_Urban,label=Year)) + 
  geom_point()+
  geom_label_repel(aes(label = Year),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   force=150,
                   vjust=0.25,
                   hjust=0.25,
                   size=3) +
  geom_smooth(method=lm)+
  labs(title="Urban oils consumption (with std error)", subtitle = "China 1978-2015")+
  scale_x_continuous(name="Log (urban population)", limits=c(19.3, 20.5)) +
  scale_y_continuous(name="Log (oils consumption)", limits=c(21.0, 23.0))+
  geom_text(x=19.6, y=22.5, label="Slope = 1.443 (95% CI [1.356, 1.530]) \n R-squared 0.994")
q + theme_classic()  

########################################################
# Loess method
ggplot(food, aes(x=Log_Pop_Rural, y=Grain_Rural)) + 
  geom_point()+
  geom_smooth()



# Remove the confidence interval
# ggplot(food, aes(x=Log_Pop_Rural, y=Grain_Rural)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)

