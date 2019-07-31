
# Install and Load Packages --------------------------------------------------------
#change
install.packages('tidyverse'); install.packages('vegan')
install.packages('MASS');install.packages("devtools"); devtools::install_github("gavinsimpson/ggvegan")
install.packages("ggrepel"); install.packages('ggplot2');install.packages('dplyr'); install.packages('scales')
install.packages('viridis'); install.packages('lme4')
install.packages('lmerTest'); install.packages('ggplot2')
install.packages('colorspace'); install.packages('backports'); install.packages('stringi')
install.packages('gridExtra')
library('backports'); library('stringi')
library('vegan'); library('tidyverse'); library('ggplot2')
library('MASS');  library('scales')
library('lmerTest');
library('viridis'); library('lme4'); library('dplyr')
library('gridExtra')


setwd("C:\\Users\\Julia Michaels\\Google Drive\\Dissertation Chapter 2\\Chapter_2_Analysis")


# Load data ---------------------------------------------------------------
calibration<-read.csv("2018 Inundation_Stopping 2018-03-19.csv")
data_loggers_2018<-read.csv('2018_Levelloggers.csv')
staff_gauge_2018<-read.csv('2017-2018 Staff Gauges All.csv')
precip_2018<-read.csv('precipitation data_2018.csv')#in mm?
#precip_2017<-read.csv('precipitation data_2017.csv')#in mm
#precip_2016<-read.csv('precipitation data_2016.csv')#in mm
#all_precip<-read.csv('Total precip 1997-2019.csv')
days_2018<-read.csv("2018_Master_List.csv")
data_loggers_2017<-read.csv('2017_Levelloggers.csv')
staff_gauge_2017<-read.csv('2016-2017 Staff Gauges.csv') 
data_loggers_2016<-read.csv('2016_Levelloggers.csv')
staff_gauge_2016<-read.csv('2015-2016 Staff Gauges All.csv')
days_2016<-read.csv("2016_Master_List.csv")
all_years_combined<-read.csv("All_Years_Master_List.csv") 
transect_data<-read.csv("2016-2018-Vegetation-Transects_Cleaned.csv") 
days_2017<-read.csv("2017_Master_List.csv")
qdata<-read.csv("2017-2018 Vegetation Quadrats.csv",fileEncoding="UTF-8-BOM")



# Calculate total precipitation -------------------------------------------------

#2018
precip_2018_by_day<-precip_2018 %>%
  group_by(Date) %>% 
  summarize(rainfall=sum(Precip, na.rm = TRUE)) %>% 
  slice(2:n()) %>% 
  mutate(rainfall_cm=rainfall/10)
  
#2017- 
precip_2017_by_day<-precip_2017 %>%
  group_by(Date) %>% 
  summarize(rainfall=sum(Precip, na.rm = TRUE)) %>% 
  slice(2:n())%>% 
  mutate(rainfall_cm=rainfall/10)
#2016- 
precip_2016_by_day<-precip_2016 %>%
  group_by(Date) %>% 
  summarize(rainfall=sum(Precip, na.rm = TRUE)) %>% 
  slice(2:n())%>% 
  mutate(rainfall_cm=rainfall/10)

precip_by_year<-all_precip %>% 
  group_by(Water_Year) %>% 
  summarize(rainfall=sum(Precip, na.rm = TRUE)) %>% 
  mutate(avg=mean(rainfall)) %>% 
  mutate(pct_of_avg=rainfall/mean(rainfall))
  

# 2018 Calculate total days of inundation from data loggers --------------------

DL2018<-data_loggers_2018%>%              #Average hourly datalogger data by day
  group_by(Date) %>% 
  summarise_all(funs(median)) 


dl_days_2018<-c()
for(i in 2:ncol(DL2018)){
  dl_days_2018[i]<-sum(DL2018[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}


dl_days_2018<-tibble(dl_days_2018, Pool.ID=colnames(DL2018))

dl_days_2018<-dl_days_2018[-15,]

# 2018 Pull out subset of dates from dataaloggers when staff gauges were also taken----------------------------

dl_subset_2018<-data_loggers_2018 %>% 
  group_by(Date) %>%
  summarise_all(funs(median)) %>% 
  filter(Date %in% c('2017-11-06','2017-11-20', '2017-11-30', '2017-12-07','2017-12-07', '2017-12-15', '2017-12-22', '2017-12-29', '2018-01-07', '2018-01-14', '2018-01-21', '2018-01-29', '2018-02-05', '2018-02-13', '2018-02-19','2018-02-26', '2018-03-05', '2018-03-12', '2018-03-19','2018-03-23', '2018-04-13','2018-04-22', '2018-04-29', '2018-05-01', '2018-06-01'))

#Combine these with data taken from pools with only staff gauge
subset_days_2018<-full_join(dl_subset_2018, staff_gauge_2018, by="Date") #.x=data logger, .y=staff gauge


#Calculate days of inundation from subsetted dates
subset_2018<-data.frame(subset_days_2018[, 2:ncol(subset_days_2018)])
subset_2018[subset_2018 < 0] = 0
subset_2018$Date<-subset_days_2018$Date
#subset_2018<-subset_2018[-23,]
subset_2018[is.na(subset_2018)] = 1 #make all na vals (days we didnt check staff gauge) 1 because we cant assume it was dry
hydrograph_2018<-subset_2018 #set aside for hydrograph
subset_2018[subset_2018 > 0] = 1
interval_days<-c(1,14,10,7,8,7,7,9,7,7,8,7,8,6,7,7,7,7,4,21,9,7,7,31)
subset_2018$Interval_Days<-interval_days

subset_temp<-subset_2018[,1:89] * subset_2018[,91]


total_days_2018<-c()
for(i in 1:ncol(subset_temp)){
  total_days_2018[i]<-sum(subset_temp[,i], na.rm=TRUE)
}
subset<-tibble(Pool.ID=colnames(subset_temp), total_days_2018) 
inundation_days_2018<-subset[-7,]%>% 
  separate(Pool.ID, c("Pool", "ID"), extra='drop') %>% 
  unite("Pool.ID", Pool, ID, sep=".") %>% 
  group_by(Pool.ID) %>% 
  summarize(days=mean(total_days_2018))

Inundation_2018<-full_join(inundation_days_2018, all_years_combined, by="Pool.ID")


subset_inundation_days_2018<-Inundation_2018 %>% 
  filter(Pair.Paper.2 %in% c(1:17)) %>% 
  filter(Year=='2018')

#Visualize
ggplot(subset_inundation_days_2018, aes(x=Treatment, y=days))+
  geom_boxplot()


anova<-aov(days~Treatment, subset_inundation_days_2018)
summary(anova)
TukeyHSD(anova)

# Compare data logger and staff gauge 

compare_2018<-full_join(dl_days_2018, subset_inundation_days_2018)%>% 
  mutate(difference=dl_days_2018-days)



# 2018 Hydrograph by Treatment --------------------------------------------
hydrograph_2018<-gather(hydrograph_2018, "Pool.ID", "Level", 1:89)
hydrograph_2018<-hydrograph_2018 %>% 
  separate(Pool.ID, c("Pool", "ID"), extra='drop') %>% 
  unite("Pool.ID", Pool, ID, sep=".") %>% 
  group_by(Pool.ID, Date) %>% 
  summarize(Level=mean(Level))

SG<-left_join(hydrograph_2018, days_2018) %>% 
  filter(Pair.Paper.2%in% c(1:17)) %>% 
  dplyr::select(Pool.ID, Date, Level, Treatment)

#average by grazing treatment

SG_graph<-SG %>% 
  group_by(Date, Treatment) %>% 
  summarize(Level=mean(Level))

#graph
ggplot(data=SG_graph, mapping=aes(x=Date, y=Level, group=Treatment))+
  geom_line(aes(color=Treatment), size=1)+
  geom_point(aes(color=Treatment))+
  scale_color_manual(name = "", 
                     values = c("Ungrazed" = "blue", "New Grazed" = "turquoise", "Grazed"="maroon"))+
  geom_line(data=precip_2018_by_day, mapping=aes(x=Date, y=rainfall_cm, linetype="Precipitation",group=1), color='grey35')+
  scale_linetype_manual(name="", 
                     values=c("Precipitation"="dashed"))+
  theme(plot.title=element_text(hjust=.5))+
  theme(axis.text.y=element_text(size=10))+
  scale_y_continuous(name="Level (cm)")+
  theme(axis.text.x = element_text(angle=25, size=10))+
  scale_x_discrete(breaks = SG_graph$Date[seq(1, length(SG_graph$Date), by = 7)])+
  labs(title="Average Vernal Pool Depth by Grazing, 2017-2018", y="Pool Depth", x="Date")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text.x =element_text(size=20))+
  theme(axis.text.y =element_text(size=20))


#Data logger hydrographs for ungrazed vs newly grazed (no continuously grazed)
DL<-DL2018 %>% 
  group_by(Date) %>% 
  summarise_all(funs(median)) 
#DL$Date<-as.Date(DL$Date)

#ungrazed
ungrazed <- DL[,colnames(DL) %in% days_2018$Pool.ID[days_2018$Treatment == 'Ungrazed']] 
ungrazed_days<-cbind(Date=DL$Date, ungrazed)
#average each row
ug_days<-c()
for(i in 1:nrow(ungrazed_days)){
  ug_days[i]<-rowMeans(ungrazed_days[i,2:ncol(ungrazed_days)],na.rm = TRUE)
}
ungrazed<-mean<-tibble (
  Date=ungrazed_days$Date, 
  Level=ug_days)
#newly grazed
newgrazed <- DL[,colnames(DL) %in% days_2018$Pool.ID[days_2018$Treatment == 'New Grazed']]
newgrazed_days<-cbind(Date=DL$Date, newgrazed)
#average each row
ng_days<-c()
for(i in 1:nrow(newgrazed_days)){
  ng_days[i]<-rowMeans(newgrazed_days[i,2:ncol(newgrazed_days)], na.rm = TRUE)
}
newgrazed<-mean<-tibble (
  Date=newgrazed_days$Date, 
  Level=ng_days)
#grazed
grazed <- DL[,colnames(DL) %in% days_2018$Pool.ID[days_2018$Treatment == 'Grazed']]
grazed_days<-cbind(Date=DL$Date, grazed)

#average each row
g_days<-c()
for(i in 1:nrow(grazed_days)){
  g_days[i]<-rowMeans(grazed_days[i,2:ncol(grazed_days)])
}
grazed<-mean<-tibble (
  Date=grazed_days$Date, 
  Level=g_days)
#graph
ggplot(data=ungrazed, mapping=aes(x=Date, y=Level))+
  geom_line(aes(x=Date, y=Level, color="Ungrazed", group=1), size=.75)+
  geom_line(data=newgrazed, aes(x=Date, y=Level, color="Newly Grazed", group=1), size=.75)+
 # geom_line(data=grazed, aes(x=Date, y=Level, color='Continuously Grazed', group=1), size=.75)+
  geom_line(data=precip_2018_by_day, mapping=aes(x=Date, y=rainfall_cm, linetype="Precipitation",group=1))+
  scale_linetype_manual(name="", 
                        values=c("Precipitation"="dashed"))+
  scale_color_manual(name = "", 
                     values = c("Ungrazed" = "blue", "Newly Grazed" = "turquoise", "Continuously Grazed"="maroon"))+
  geom_hline(yintercept=0, color='grey16')+
  theme(plot.title=element_text(hjust=.5))+
  theme(axis.text.y=element_text(size=10))+
  scale_y_continuous(name="Level (cm)")+
  theme(axis.text.x = element_text(angle=25, size=10))+
  scale_x_discrete(breaks = ungrazed$Date[seq(1, length(ungrazed$Date), by = 30)])+
  labs(title="Average Vernal Pool Depth by Grazing, 2017-2018", y="Pool Depth", x="Date")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text.x =element_text(size=20))+
  theme(axis.text.y =element_text(size=20))



# 2017 Calculate total days of inundation from data loggers --------------------

DL2017<-data_loggers_2017%>%              #Average hourly datalogger data by day
  group_by(Date) %>% 
  summarise_all(funs(median)) 


dl_days_2017<-c()
for(i in 2:ncol(DL2017)){
  dl_days_2017[i]<-sum(DL2017[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}


dl_days_2017<-tibble(dl_days_2017, Pool.ID=colnames(DL2017))


# 2017 Pull out subset of dates from dataaloggers when staff gauges were also taken----------------------------

dl_subset_2017<-data_loggers_2017 %>% 
  group_by(Date) %>%
  summarise_all(funs(median)) %>% 
  filter(Date %in% staff_gauge_2017$Date)
         
         
#Combine these with data taken from pools with only staff gauge
subset_days_2017<-full_join(dl_subset_2017, staff_gauge_2017, by="Date") #.x=data logger, .y=staff gauge


#Calculate days of inundation from subsetted dates
subset_2017<-data.frame(subset_days_2017[, 2:ncol(subset_days_2017)])
subset_2017[subset_2017 < 0] = 0
subset_2017$Date<-subset_days_2017$Date
#subset_2017<-subset_2017[-23,]
subset_2017[is.na(subset_2017)] = 1 #make all na vals (days we didnt check staff gauge) 1 because we cant assume it was dry
hydrograph_2017<-subset_2017 #set aside for hydrograph
subset_2017[subset_2017 > 0] = 1
interval_days<-c(1,10,7,7,8,12,9,7,10,13,19,17,7,7,8,5,11,14,13,7) 
subset_2017$Interval_Days<-interval_days


subset_temp<-subset_2017[,1:101] * subset_2017[,103]

total_days_2017<-c()
for(i in 1:ncol(subset_temp)){
  total_days_2017[i]<-sum(subset_temp[,i], na.rm=TRUE)
}
subset<-tibble(Pool.ID=colnames(subset_temp), total_days_2017) 
inundation_days_2017<-subset[-7,]%>% 
  separate(Pool.ID, c("Pool", "ID"), extra='drop') %>% 
  unite("Pool.ID", Pool, ID, sep=".") %>% 
  group_by(Pool.ID) %>% 
  summarize(days=mean(total_days_2017))

Inundation_2017<-full_join(inundation_days_2017, all_years_combined, by="Pool.ID")


subset_inundation_days_2017<-Inundation_2017 %>% 
  filter(Pair.Paper.2 %in% c(1:17)) %>% 
  filter(Year=='2017')

#Visualize
ggplot(subset_inundation_days_2017, aes(x=Treatment, y=days))+
  geom_boxplot()


anova<-aov(days~Treatment, subset_inundation_days_2017)
summary(anova)
TukeyHSD(anova)

# Compare data logger and staff gauge 

compare_2017<-full_join(dl_days_2017, subset_inundation_days_2017, by='Pool.ID')%>% 
  mutate(difference=dl_days_2017-days)


# 2017 Hydrograph by Treatment --------------------------------------------
hydrograph_2017<-gather(hydrograph_2017, "Pool.ID", "Level", 1:101)
hydrograph_2017<-hydrograph_2017 %>% 
  separate(Pool.ID, c("Pool", "ID"), extra='drop') %>% 
  unite("Pool.ID", Pool, ID, sep=".") %>% 
  group_by(Pool.ID, Date) %>% 
  summarize(Level=mean(Level))

SG<-left_join(hydrograph_2017, days_2017) %>% 
  filter(Pair.Paper.2%in% c(1:17)) %>% 
  dplyr::select(Pool.ID, Date, Level, Treatment)

#average by grazing treatment

SG_graph<-SG %>% 
  group_by(Date, Treatment) %>% 
  summarize(Level=mean(Level))

#graph
ggplot(data=SG_graph, mapping=aes(x=Date, y=Level, group=Treatment))+
  geom_line(aes(color=Treatment), size=1)+
  geom_point(aes(color=Treatment))+
  scale_color_manual(name = "", 
                     values = c("Ungrazed" = "blue", "New Grazed" = "turquoise", "Grazed"="maroon"))+
 # geom_line(data=precip_2017_by_day, mapping=aes(x=Date, y=rainfall_cm, linetype="Precipitation",group=1), color='grey35')+
  #scale_linetype_manual(name="", 
   #                     values=c("Precipitation"="dashed"))+
  theme(plot.title=element_text(hjust=.5))+
  theme(axis.text.y=element_text(size=10))+
  scale_y_continuous(name="Level (cm)")+
  theme(axis.text.x = element_text(angle=25, size=10))+
  scale_x_discrete(breaks = SG_graph$Date[seq(1, length(SG_graph$Date), by = 7)])+
  labs(title="Average Vernal Pool Depth by Grazing, 2016-2017", y="Pool Depth", x="Date")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text.x =element_text(size=20))+
  theme(axis.text.y =element_text(size=20))

# 2017 DL New grazed/ungrazed hydrograph

#divide into grazed/newly grazed/ungrazed
#ungrazed
DL<-Calibrated_2017 %>% 
  group_by(Date) %>% 
  summarise_all(funs(median)) 
#DL$Date<-as.Date(DL$Date)
ungrazed <- DL[,colnames(DL) %in% days_2017$Pool.ID[days_2017$Treatment == 'Ungrazed']]
ungrazed_days<-cbind(Date=DL$Date, ungrazed)
#average each row
ug_days<-c()
for(i in 1:nrow(ungrazed_days)){
  ug_days[i]<-rowMeans(ungrazed_days[i,2:ncol(ungrazed_days)],na.rm = TRUE)
}
ungrazed<-mean<-tibble (
  Date=ungrazed_days$Date, 
  Level=ug_days)

#newly grazed
newgrazed <- DL[,colnames(DL) %in% days_2017$Pool.ID[days_2017$Treatment == 'New Grazed']]
newgrazed_days<-cbind(Date=DL$Date, newgrazed)
#average each row
ng_days<-c()
for(i in 1:nrow(newgrazed_days)){
  ng_days[i]<-rowMeans(newgrazed_days[i,2:ncol(newgrazed_days)], na.rm = TRUE)
}
newgrazed<-mean<-tibble (
  Date=newgrazed_days$Date, 
  Level=ng_days)

#graph
ggplot(data=ungrazed, mapping=aes(x=Date, y=Level))+
  geom_line(aes(x=Date, y=Level, color="Ungrazed", group=1), size=.75)+
  geom_line(data=newgrazed, aes(x=Date, y=Level, color="Newly Grazed", group=1), size=.75)+
 # geom_line(data=grazed, aes(x=Date, y=Level, color='Continuously Grazed', group=1), size=.75)+
  #geom_line(data=precip_2017_by_day, mapping=aes(x=Date, y=rainfall_cm, linetype="Precipitation",group=1))+
  #scale_linetype_manual(name="", 
   #                     values=c("Precipitation"="dashed"))+
  scale_color_manual(name = "", 
                     values = c("Ungrazed" = "blue", "Newly Grazed" = "turquoise", "Continuously Grazed"="maroon"))+
  geom_hline(yintercept=0, color='grey16')+
  theme(plot.title=element_text(hjust=.5))+
  theme(axis.text.y=element_text(size=10))+
  scale_y_continuous(name="Level (cm)")+
  theme(axis.text.x = element_text(angle=25, size=10))+
  scale_x_discrete(breaks = ungrazed$Date[seq(1, length(grazed$Date), by = 30)])+
  labs(title="Average Vernal Pool Depth by Grazing, 2016-2017", y="Pool Depth", x="Date")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text.x =element_text(size=20))+
  theme(axis.text.y =element_text(size=20))





# 2016 Calculate total days of inundation from data loggers --------------------

DL2016<-data_loggers_2016%>%              #Average hourly datalogger data by day
  group_by(Date) %>% 
  summarise_all(funs(median)) 


dl_days_2016<-c()
for(i in 2:ncol(DL2016)){
  dl_days_2016[i]<-sum(DL2016[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}


dl_days_2016<-tibble(dl_days_2016, Pool.ID=colnames(DL2016))


# 2016 Pull out subset of dates from dataaloggers when staff gauges were also taken----------------------------

dl_subset_2016<-data_loggers_2016 %>% 
  group_by(Date) %>%
  summarise_all(funs(median)) %>% 
  filter(Date %in% staff_gauge_2016$Date)


#Combine these with data taken from pools with only staff gauge
subset_days_2016<-full_join(dl_subset_2016, staff_gauge_2016, by="Date") #.x=data logger, .y=staff gauge


#Calculate days of inundation from subsetted dates
subset_2016<-data.frame(subset_days_2016[, 2:ncol(subset_days_2016)])
subset_2016[subset_2016 < 0] = 0
subset_2016$Date<-subset_days_2016$Date
#subset_2016<-subset_2016[-23,]
subset_2016[is.na(subset_2016)] = 1 #make all na vals (days we didnt check staff gauge) 1 because we cant assume it was dry
hydrograph_2016<-subset_2016 #set aside for hydrograph
subset_2016[subset_2016 > 0] = 1
interval_days<-c(1,68,10,8,7,11,16,5,7,7,7) 
subset_2016$Interval_Days<-interval_days


subset_temp<-subset_2016[,1:87] * subset_2016[,89]

total_days_2016<-c()
for(i in 1:ncol(subset_temp)){
  total_days_2016[i]<-sum(subset_temp[,i], na.rm=TRUE)
}
subset<-tibble(Pool.ID=colnames(subset_temp), total_days_2016) 
inundation_days_2016<-subset[-7,]%>% 
  separate(Pool.ID, c("Pool", "ID"), extra='drop') %>% 
  unite("Pool.ID", Pool, ID, sep=".") %>% 
  group_by(Pool.ID) %>% 
  summarize(days=mean(total_days_2016))

Inundation_2016<-full_join(inundation_days_2016, all_years_combined, by="Pool.ID")


subset_inundation_days_2016<-Inundation_2016 %>% 
  filter(Pair.Paper.2 %in% c(1:17)) %>% 
  filter(Year=='2016')

#Visualize
ggplot(subset_inundation_days_2016, aes(x=Treatment, y=days))+
  geom_boxplot()


anova<-aov(days~Treatment, subset_inundation_days_2016)
summary(anova)
TukeyHSD(anova)

# Compare data logger and staff gauge 

compare_2016<-full_join(dl_days_2016, subset_inundation_days_2016, by='Pool.ID')%>% 
  mutate(difference=dl_days_2016-days)



# 2016 Hydrograph by Treatment --------------------------------------------
hydrograph_2016<-gather(hydrograph_2016, "Pool.ID", "Level", 1:87)
hydrograph_2016<-hydrograph_2016 %>% 
  separate(Pool.ID, c("Pool", "ID"), extra='drop') %>% 
  unite("Pool.ID", Pool, ID, sep=".") %>% 
  group_by(Pool.ID, Date) %>% 
  summarize(Level=mean(Level))

SG<-left_join(hydrograph_2016, days_2016) %>% 
  filter(Pair.Paper.2%in% c(1:17)) %>% 
  dplyr::select(Pool.ID, Date, Level, Treatment)

#average by grazing treatment

SG_graph<-SG %>% 
  group_by(Date, Treatment) %>% 
  summarize(Level=mean(Level))

#graph
ggplot(data=SG_graph, mapping=aes(x=Date, y=Level, group=Treatment))+
  geom_line(aes(color=Treatment), size=1)+
  geom_point(aes(color=Treatment))+
  scale_color_manual(name = "", 
                     values = c("Ungrazed" = "blue", "Newly Grazed" = "turquoise", "Grazed"="maroon"))+
 # geom_line(data=precip_2016_by_day, mapping=aes(x=Date, y=rainfall_cm, linetype="Precipitation",group=1), color='grey35')+
  #scale_linetype_manual(name="", 
   #                     values=c("Precipitation"="dashed"))+
  theme(plot.title=element_text(hjust=.5))+
  theme(axis.text.y=element_text(size=10))+
  scale_y_continuous(name="Level (cm)")+
  theme(axis.text.x = element_text(angle=25, size=10))+
  scale_x_discrete(breaks = SG_graph$Date[seq(1, length(SG_graph$Date), by = 1)])+
  labs(title="Average Vernal Pool Depth by Grazing, 2015-2016", y="Pool Depth", x="Date")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text.x =element_text(size=20))+
  theme(axis.text.y =element_text(size=20))

###2016 DL hydrograph for grazed/ungrazed
DL<-Calibrated_2016 %>% 
  group_by(Date) %>% 
  summarise_all(funs(median)) 

#DL$Date<-as.Date(DL$Date)
ungrazed <- DL[,colnames(DL) %in% days_2016$Pool.ID[days_2016$Treatment == 'Ungrazed']]
ungrazed_days<-cbind(Date=DL$Date, ungrazed)

#average each row
ug_days<-c()
for(i in 1:nrow(ungrazed_days)){
  ug_days[i]<-rowMeans(ungrazed_days[i,2:ncol(ungrazed_days)],na.rm = TRUE)
}

ungrazed<-mean<-tibble (
  Date=ungrazed_days$Date, 
  Level=ug_days)

#newly grazed
newgrazed <- DL[,colnames(DL) %in% days_2016$Pool.ID[days_2016$Treatment == 'Newly Grazed']]
newgrazed_days<-cbind(Date=DL$Date, newgrazed)

#average each row
ng_days<-c()
for(i in 1:nrow(newgrazed_days)){
  ng_days[i]<-rowMeans(newgrazed_days[i,2:ncol(newgrazed_days)], na.rm = TRUE)
}

newgrazed<-mean<-tibble (
  Date=newgrazed_days$Date, 
  Level=ng_days)


#grazed
grazed <- DL[,colnames(DL) %in% days_2016$Pool.ID[days_2016$Treatment == 'Grazed']]
grazed_days<-cbind(Date=DL$Date, grazed) 

grazed_days<-grazed_days%>% 
  dplyr::select(-D6.59)

#average each row
g_days<-c()
for(i in 1:nrow(grazed_days)){
  g_days[i]<-rowMeans(grazed_days[i,2:ncol(grazed_days)], na.rm=TRUE)
}

grazed<-mean<-tibble (
  Date=grazed_days$Date, 
  Level=g_days)


#graph
ggplot(data=ungrazed, mapping=aes(x=Date, y=Level))+
  geom_line(aes(x=Date, y=Level, color="Ungrazed", group=1), size=.75)+
  #geom_line(data=newgrazed, aes(x=Date, y=Level, color="Newly Grazed", group=1), size=.75)+
  geom_line(data=grazed, aes(x=Date, y=Level, color='Continuously Grazed', group=1), size=.75)+
  #geom_line(data=precip_2016_by_day, mapping=aes(x=Date, y=rainfall_cm, linetype="Precipitation",group=1))+
 # scale_linetype_manual(name="", 
               #         values=c("Precipitation"="dashed"))+
  scale_color_manual(name = "", 
                     values = c("Ungrazed" = "blue", "Newly Grazed" = "turquoise", "Continuously Grazed"="maroon"))+
  geom_hline(yintercept=0, color='grey16')+
  theme(plot.title=element_text(hjust=.5))+
  theme(axis.text.y=element_text(size=10))+
  scale_y_continuous(name="Level (cm)")+
  theme(axis.text.x = element_text(angle=25, size=10))+
  scale_x_discrete(breaks = ungrazed$Date[seq(1, length(ungrazed$Date), by = 30)])+
  labs(title="Average Vernal Pool Depth by Grazing, 2015-2016", y="Pool Depth", x="Date")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text.x =element_text(size=20))+
  theme(axis.text.y =element_text(size=20))




# Compare all years inundation ----------------------------

test<-full_join(subset_inundation_days_2017, subset_inundation_days_2018, by=c("Pool.ID", "days","Treatment", "Year", "Shape","Soil.Type", "Size", "Topography", "Calibrated.Days", "Pair.Paper.2"))
test$Year<-as.factor(test$Year)
subset_inundation_days_2016$Year<-as.factor(subset_inundation_days_2016$Year)
test2<-full_join(test, subset_inundation_days_2016, by=c("Pool.ID", "days","Treatment", "Year", "Shape","Soil.Type", "Size", "Topography", "Calibrated.Days", "Pair.Paper.2"))



test2<-test2 %>% 
  filter(!is.na(days))
all_years<-test2%>%
  filter(Pair.Paper.2 %in% c(1:17)) %>% 
  group_by(Year, Treatment) %>% 
  summarize(days1=mean(days, na.rm = TRUE), sd1=sd(days, na.rm = TRUE), 
            sem1 = sd(days, na.rm = TRUE)/sqrt(length(days)))
all_years$Year<-as.factor(all_years$Year)

pd <- position_dodge(0.01)


plot<-ggplot(data=all_years, aes(x=Year, y=days1, group=Treatment))+
  geom_line(aes(color=Treatment), size=1.5)+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_errorbar(aes(ymin=days1-sem1, ymax=days1+sem1), width=.025, position=pd)+
  geom_point(size=2, position=pd)+
  scale_x_discrete(labels=c("2015-2016", "2016-2017", "2017-2018"))+
  labs(title="Average Days of Innundation by Treatment, 2015-2018", y="Days of Inundation", x="Year")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))+
  geom_vline(aes(xintercept=2), colour="#990000", linetype="dashed")+
  annotate("text", x = 2, y =55, label = "Grazing introduced", size=7)+
  annotate("text", x = 2, y =50, label = "to 'New Grazed'", size=7)+
  annotate("text", x = 1, y =185, label = "43.60 cm precip
           87.3% of 21-year avg", size=7)+
  annotate("text", x = 2, y =185, label = "93.96 cm precip
           188.00% of 21-year avg",size=7)+
  annotate("text", x = 3, y =185, label = "52.29 cm precip
           105.7% of 21-year avg", size=7)
plot



#Inundation period by grazing and year (all three years)
anova<-aov(days~Treatment+Year, test2)
summary(anova) 
TukeyHSD(anova)

m2<-lmer(days~Treatment+(1|Pool.ID)+(1|Pool.ID), data=test2)
anova(m2)
TukeyHSD(m2)

inundation_days <- lm(days ~ Treatment+Year+Size+Soil.Type, data=test2) 
summary(aov(inundation_days))
hist(residuals(inundation_days))

#Inundation period by grazing and year (2017 and 2018 only)
anova<-aov(days~Treatment+Year, test)
summary(anova)
TukeyHSD(anova)

m3<-lmer(days~Treatment*Year+(1|Pool.ID), data=test)
anova(m3)
TukeyHSD(m3)

inundation_days <- lm(days ~ Treatment+Year+Size+Soil.Type, data=test) 
summary(aov(inundation_days))
hist(residuals(inundation_days))



#RDM by grazing
ggplot(data=qdata, aes(x=Grazing,y=RDM, fill=Grazing))+
  geom_boxplot()+
  labs(title="Residual Dry Matter (RDM) by Grazing", y="RDM", x="Grazing")+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_blank())+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

summary(aov(RDM~Grazing, qdata))

#Pool size by grazing
ggplot(data=qdata, aes(x=Grazing,y=Size, fill=Grazing))+
  geom_boxplot()+
  labs(title="Pool Size by Grazing", y="Size", x="Grazing")+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

#catchment area by grazingclass(qdata$Catchment)
qdata$Catchment<-as.character(qdata$Catchment)
qdata$Catchment<-as.numeric(qdata$Catchment)
ggplot(data=qdata, aes(x=Grazing,y=Catchment, fill=Grazing))+
  geom_boxplot()+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  labs(title="Catchment Size by Grazing", y="Catchment Size (m2)", x="Grazing")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))


summary(aov(Catchment~Grazing, qdata))


inundation_days2 <- lm(Calibrated.Total.Days~Grazing+Size+Catchment+SoilType, data=qdata) 
summary(inundation_days2)
hist(residuals(inundation_days2))



all_years_combined$Year<-as.factor(all_years_combined$Year)
all_years<-all_years_combined%>%
  filter(Pair.Paper.2 %in% c(1:17)) %>% 
  group_by(Year, Treatment) %>% 
  summarize(days=mean(Calibrated.Days, na.rm = TRUE), sd=sd(Calibrated.Days, na.rm = TRUE), 
            sem = sd(Calibrated.Days, na.rm = TRUE)/sqrt(length(Calibrated.Days)))
all_years$Year<-as.factor(all_years$Year)


plot<-ggplot(data=all_years, aes(x=Year, y=days, group=Treatment))+
  geom_line(aes(color=Treatment), size=1.5)+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_errorbar(aes(ymin=days-sem, ymax=days+sem), width=.025)+
  geom_point(size=2)+
  scale_x_discrete(labels=c("2015-2016", "2016-2017", "2017-2018"))+
  labs(title="Average Days of Innundation by Treatment, 2015-2018", y="Days of Inundation", x="Year")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))+
  geom_vline(aes(xintercept=2), colour="#990000", linetype="dashed")+
  annotate("text", x = 2, y =55, label = "Grazing introduced")+
  annotate("text", x = 2, y =50, label = "to 'New Grazed'")+
  annotate("text", x = 1, y =185, label = "43.60 cm precip
  87.3% of 21-year avg")+
  annotate("text", x = 2, y =185, label = "93.96 cm precip
  188.00% of 21-year avg")+
  annotate("text", x = 3, y =185, label = "52.29 cm precip
  105.7% of 21-year avg")
plot



#Inundation period by grazing and year
anova<-aov(Calibrated.Days~Treatment+Year, all_years_combined)
summary(anova)
TukeyHSD(anova)

m2<-lmer(Calibrated.Days~Treatment*Year+(1|Pool.ID), data=all_years_combined)
summary(anova2)
TukeyHSD(anova2)

inundation_days <- lm(Calibrated.Days ~ Treatment+Year+Size+Soil.Type, data=all_years_combined) 
summary(aov(inundation_days))
hist(residuals(inundation_days))


#RDM by grazing
ggplot(data=qdata, aes(x=Grazing,y=RDM, fill=Grazing))+
  geom_boxplot()+
  labs(title="Residual Dry Matter (RDM) by Grazing", y="RDM", x="Grazing")+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

summary(aov(RDM~Grazing, qdata))

#Pool size by grazing
ggplot(data=qdata, aes(x=Grazing,y=Size, fill=Grazing))+
  geom_boxplot()+
  labs(title="Pool Size by Grazing", y="Size", x="Grazing")+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

#catchment area by grazingclass(qdata$Catchment)
qdata$Catchment<-as.character(qdata$Catchment)
qdata$Catchment<-as.numeric(qdata$Catchment)
ggplot(data=qdata, aes(x=Grazing,y=Catchment, fill=Grazing))+
  geom_boxplot()+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  labs(title="Catchment Size by Grazing", y="Catchment Size (m2)", x="Grazing")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))


summary(aov(Catchment~Grazing, qdata))


inundation_days2 <- lm(Calibrated.Total.Days~Grazing+Size+Catchment+SoilType, data=qdata) 
summary(inundation_days2)
hist(residuals(inundation_days2))



# Transect data -----------------------------------------------------------

transect_data<-transect_data%>% 
  filter(Pair.P2 %in% c(1:11)) 
#Trans_2018<-filter(transect_data, Pair.P2 %in% c(1:12), Year=="2018")
#head(Trans_2018)

##########2016-2018 Transect Top Hit Only########
#2016 was top hit, 2017 and 2018 were multiple hits per point, so have to filter out first row

Transect_TopHit<-transect_data%>%
  group_by(Year, Point, Pool.ID, Transect., Zone)%>%
  #mutate(Speciesnew = first(Species))%>%
  filter(row_number()==1)%>%
  #select(-Point)%>%
  group_by(Species, Pool.ID, Grazing, Zone, Year, Transect.)%>%
  tally()%>%
  arrange(desc(Pool.ID))%>%
  group_by(Pool.ID, Zone, Year, Transect.)%>%
  mutate(trans_length=sum(n),
         Rel_abun=n/trans_length,#calculate relative abundance for each species per pool
         specrich=n_distinct(Species),#calculate species richness for each pool
         shannon=diversity(Rel_abun))%>%
  arrange(Pool.ID, Year, Zone, Transect.)

#calculate diversity metrics

Transect_diversity<-
  Transect_TopHit %>% 
  dplyr::select(-Species) %>% 
  group_by(Pool.ID, Grazing, Zone, Year) %>% 
  summarize_all(funs(mean)) #average the two transects
Transect_diversity$Year<-as.factor(Transect_diversity$Year)

#####ANOVAS for specrich and shannon
summary(aov(specrich~Grazing*Year, Transect_diversity))
summary(aov(shannon~Grazing*Year, Transect_diversity)) 


##Calculate native relative cover
specid<-read.csv("SpecID.csv", stringsAsFactors=FALSE)

Transect_TopHit_Summed<-Transect_TopHit %>% 
  dplyr::select(Species,Pool.ID, Transect., Year, Grazing, Zone, Rel_abun) %>% 
  group_by(Species,Pool.ID, Year, Zone, Grazing) %>% 
  summarize_all(funs(sum))

st<-t(Transect_TopHit_Summed)
colnames(st) <- as.character(unlist(st[1,]))
st1= st[-1, ]
stnative<-as.data.frame(st1[,colnames(st1) %in% specid$SpeciesIDCode[specid$Status == 'Native']])
stnative1<-t(stnative)
stnative2<-as.data.frame(stnative1, row.names = TRUE)

native_abundance<-stnative2%>%
  dplyr::select(-Transect.)


native_abundance$Rel_abun<-as.character(native_abundance$Rel_abun)
native_abundance$Rel_abun<-as.numeric(native_abundance$Rel_abun)

native_cover<-native_abundance%>% 
  group_by(Pool.ID, Zone, Grazing, Year) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(native=Rel_abun/2)

###ANOVA for native cover
#####ANOVAS
summary(aov(native~Grazing*Year, native_cover))


############Plot species richness by grazing treatment

#boxplot

ggplot(data=Transect_diversity, aes(x=Year,y=specrich, fill=Grazing))+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  geom_boxplot()+
  facet_wrap(~Zone)+
  labs(title="Species Richness by Habitat Zone and Grazing Treatment", y="Species Richness", x="")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(strip.text =element_text(size=30))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))


#line graph

Transect_specrich<-Transect_diversity %>% 
  group_by(Year, Grazing, Zone) %>% 
  summarize(mean=mean(specrich), sd1=sd(specrich), sem1 = sd(specrich, na.rm = TRUE)/sqrt(length(specrich))) #average the two transects

pd <- position_dodge(0.04)
ggplot(data=Transect_specrich, aes(x=Year,y=mean, group=Grazing))+
  geom_point(aes(color=Grazing), size=2, position=pd)+
  geom_line(aes(color=Grazing), size=1.5)+
  facet_wrap(~Zone)+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  labs(title="Species Richness by Habitat Zone and Grazing Treatment", y="Species Richness", x="")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(strip.text =element_text(size=30))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title=element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))+
  geom_errorbar(aes(ymin=mean-sem1, ymax=mean+sem1), width=.025, position=pd)




###########Plot shannon by grazing treatment
#boxplot
ggplot(data=Transect_diversity, aes(x=Year,y=shannon, fill=Grazing))+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  geom_boxplot()+
  facet_wrap(~Zone)+
  labs(title="Shannon Weiner Diversity by Habitat Zone and Grazing Treatment", y="Shannon Weiner Diversity", x="")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(strip.text =element_text(size=30))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))


#line graph

Transect_shannon<-Transect_diversity %>% 
  group_by(Year, Grazing, Zone) %>% 
  summarize(mean=mean(shannon), sd1=sd(specrich), sem1 = sd(shannon, na.rm = TRUE)/sqrt(length(shannon))) #average the two transects

pd <- position_dodge(0.04)
ggplot(data=Transect_shannon, aes(x=Year,y=mean, group=Grazing))+
  geom_point(aes(color=Grazing), size=2, position=pd)+
  geom_line(aes(color=Grazing), size=1.5)+
  facet_wrap(~Zone)+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  labs(title="Shannon Weiner Diversity by Habitat Zone and Grazing Treatment", y="Shannon Weiner", x="")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(strip.text =element_text(size=30))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title=element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))+
  geom_errorbar(aes(ymin=mean-sem1, ymax=mean+sem1), width=.025, position=pd)


##Plot native cover by grazing treatment

#boxplot
ggplot(data=native_cover, aes(x=Year,y=native, fill=Grazing))+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  geom_boxplot()+
  facet_wrap(~Zone)+
  labs(title="Native Cover by Habitat Zone and Grazing Treatment", y="Shannon Weiner Diversity", x="")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(strip.text =element_text(size=30))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))


#line graph

Transect_shannon<-native_cover %>% 
  group_by(Year, Grazing, Zone) %>% 
  summarize(mean=mean(Rel_abun), sd1=sd(Rel_abun), sem1 = sd(Rel_abun, na.rm = TRUE)/sqrt(length(Rel_abun))) #average the two transects

pd <- position_dodge(0.04)
ggplot(data=Transect_shannon, aes(x=Year,y=mean, group=Grazing))+
  geom_point(aes(color=Grazing), size=2, position=pd)+
  geom_line(aes(color=Grazing), size=1.5)+
  facet_wrap(~Zone)+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  labs(title="Shannon Weiner by Habitat Zone and Grazing Treatment", y="% Native Cover", x="")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(strip.text =element_text(size=30))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title=element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))+
  geom_errorbar(aes(ymin=mean-sem1, ymax=mean+sem1), width=.025, position=pd)



# 2018 Quadrats in Transition Zones ----------------------------------------------------------------

#2018 Quadrats in Transition Zones
qdata$Catchment<-as.character(qdata$Catchment)
qdata$Catchment<-as.numeric(qdata$Catchment)
specid<-read.csv("SpecID.csv", fileEncoding="UTF-8-BOM")
qdata_separate<-qdata[-39,] #remove 39 if lookng at samples separately

species<-dplyr::select(qdata_separate, -(Quadrat:Inundation.Type)) %>%  #average the three quadrat samples
  group_by(Pool.ID)%>%
  summarise_all(funs(mean))

pool_info<-qdata_separate %>% group_by(Pool.ID) %>% 
  filter(row_number()==1) %>% 
  dplyr::select(Pool.ID:Inundation.Type, -Quadrat)

qdata<-right_join(pool_info, species)#combine community data with pool characteristics


#Calculate diversity indices

#species richness
spec_rich<-c()
for(i in 1:nrow(qdata)){
  spec_rich[i]<-sum(qdata[i,-(1:15)]>0)
}

#shannon weiner
shannon <- c()
for(i in 1:nrow(qdata)){
  temp <- as.numeric(qdata[i,-(1:15)])
  shannon[i] <- diversity(temp)
}

#Calculate the relative cover of natives
natives <- qdata[,colnames(qdata) %in% specid$SpeciesIDCode[specid$Status == 'Native']]

total_cov_nat<-c()
for(i in 1:nrow(natives)){
  total_cov_nat[i]<-sum(natives[i,1:ncol(natives)])
}

total_cov <- c()
for(i in 1:nrow(qdata)){
  temp <- as.numeric(qdata[i,-(1:13)])
  total_cov[i] <- sum(temp)
}

rel_cov_nat<-c()
for(i in 1:nrow(qdata)){
  rel_cov_nat[i]<-total_cov_nat[i]/total_cov[i]
}


#create data frame with all community metrics
transition_diversity <- data.frame(qdata$Pool.ID, qdata$Grazing, spec_rich,shannon, rel_cov_nat)
trans_diversity<-gather(transition_diversity, "Metric", "Value", 3:5)

# plot all 
transition_diversity_plot<- trans_diversity %>%
  group_by(qdata.Pool.ID, Metric, qdata.Grazing, Value) %>% 
  summarize(days=mean(Value, na.rm = TRUE))
transition_diversity_plot

anova1<-aov(spec_rich~qdata.Grazing, transition_diversity)
summary(anova1)

TukeyHSD(anova1)
TukeyHSD(aov(shannon~qdata.Grazing, transition_diversity))
TukeyHSD(aov(rel_cov_nat~qdata.Grazing, transition_diversity))

plot<-transition_diversity_plot%>% 
  ggplot(aes(x=qdata.Grazing, y=Value, fill=qdata.Grazing))+
  geom_boxplot()+
  facet_wrap(~Metric, scales="free_y")+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=20))+
  theme(strip.text = element_text(size=17))+
  theme(axis.title.y  = element_blank())+
  theme(axis.title.x  = element_blank())+
  theme(axis.text.x  = element_blank())+
  theme(axis.text.y  = element_text(size=30))

plot



#Plot Species richness by days of inundation and treatment 
ggplot(data=transition_diversity, aes(x=Calibrated.Total.Days,y=spec_rich, colour=Grazing))+
  #geom_jitter()+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(mapping=aes(group=cut_width(Calibrated.Total.Days, 15)), size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Species Richness by Days of Inundation and Grazing", y="Species Richness", x="Days of Inundation")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

#Plot Species richness by days of hoofprint count and treatment 
ggplot(data=transition_diversity, aes(x=Hoofprint,y=spec_rich, colour=Grazing))+
  #geom_jitter()+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(mapping=aes(group=cut_width(Calibrated.Total.Days, 15)), size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Species Richness by Hoofprint Count and Grazing", y="Species Richness", x="Hoofprint Count")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))


summary(aov(spec_rich~Hoofprint, transition_diversity))
summary(aov(lm(spec_rich~Hoofprint, data=transition_diversity)))

#Plot shannon by grazing
ggplot(data=transition_diversity, aes(x=Grazing,y=shannon))+
  geom_boxplot()+
  ggtitle("Shannon Weiner Diversity by Grazing")

summary(aov(shannon~Grazing, transition_diversity))

#Plot Shannon by days of inundation and treatment 
ggplot(data=transition_diversity, aes(x=Calibrated.Total.Days,y=shannon, colour=Grazing))+
  #geom_jitter()+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(mapping=aes(group=cut_width(Calibrated.Total.Days, 15)), size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Shannon Weiner Diversity by Days of Inundation and Grazing", y="Shannon Weiner", x="Days of Inundation")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

#Plot native cover by grazing
ggplot(data=transition_diversity, aes(x=Grazing,y=rel_cov_nat))+
  geom_boxplot()+
  ggtitle("Relative Native Cover by Grazing")

summary(aov(rel_cov_nat~Grazing, transition_diversity))

#Plot native cover by days of inundation and treatment 
ggplot(data=transition_diversity, aes(x=Calibrated.Total.Days,y=rel_cov_nat, colour=Grazing))+
  #geom_jitter()+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(mapping=aes(group=cut_width(Calibrated.Total.Days, 15)), size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Native Relative Cover by Days of Inundation and Grazing", y="Relative Cover Natives", x="Days of Inundation")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))




###Linear model: native cover by grazing and inundation (Staff gauge)
linearMod1 <- lm(rel_cov_nat ~ Calibrated.Total.Days*Grazing, data=transition_diversity) 
summary(linearMod1)#individual coeffeicients fitted to the model
summary(aov(linearMod1))#asking if the grazing treatments are different

hist(residuals(linearMod1))


###Linear model: specrich by grazing and inundation

linearMod3 <- lm(spec_rich ~ Calibrated.Total.Days*Grazing, data=transition_diversity) 
summary(linearMod3)
hist(residuals(linearMod3))

linearMod4 <- lm(shannon ~ Calibrated.Total.Days*Grazing, data=transition_diversity) 
summary(linearMod4)
hist(residuals(linearMod4))



# 2018 Quadrat Ordination -------------------------------------------------

qdata1<-qdata[,-c(1:14)]

env_quad1<-
  tibble(
    Pool.ID=qdata$Pool.ID,
    Grazing=qdata$Grazing,
    In_Days=qdata$Calibrated.Total.Days,
    HoofCount=qdata$Hoofprint,
    RDM=qdata$RDM,
    # Catchment=qdata$Catchment, can't do because of NAs
    Soil=qdata$SoilType,
    Size=qdata$Size
  )

#the total inertia is decomposed into constrained and unconstrained components
#'proportion of inertia' doesn't really have a clear meaning in CCA (not lioke in RDA)
#test the significance of the model by permuting the data randomly and refitting the model
#when the constrained inertia in #Permutations is always lower than the oberved constrained inertia, the constraints are significant


quad<-cca(qdata1~In_Days+HoofCount+Size+Soil+RDM, env_quad1)
plot(quad)
anova(quad, by='margin', perm=500)
#don't pay attention to F ratio
#test significance of the variables
anova(quad, by='axis', perm=500)# Only first axis is significant
##Try conditioning the CCA on days of inundation and hoofprint to look separately
quad2<-cca(qdata1~In_Days+Size+Soil+ Condition(HoofCount), env_quad1)
anova(quad2, by='margin', perm=500)

quad3<-cca(qdata1~HoofCount+Size+Soil+ Condition(In_Days), env_quad1)
anova(quad3, by='margin', perm=500)

#constrained-- inertia explained by your variables
#compare the constrained inertia in the conditioned vs. non-conditioned model
#Plot CCA#
p1<-ordiplot(quad)
identify(p1, "species")
p1
p2<-autoplot(quad)+
  geom_point(data = cbind(subset(fmod, Score == "sites"), Grazing = env_quad$Grazing),
             aes(x = CCA1, y = CCA2, colour = Grazing), size = 2)


fmod <- fortify(quad)
size <- 1.8


p4<-ggplot(fmod, aes(x = CCA1, y = CCA2, label=Label)) +
  geom_text(data = subset(fmod, Score == "species"),
            colour = 'grey66', size = 6) +
  geom_point(data = cbind(subset(fmod, Score == "sites"), Grazing = env_quad1$Grazing),
             aes(colour = Grazing), size = 4) +
  scale_colour_brewer("Score", palette = "Set2") +
  coord_fixed() +
  theme(legend.position = "top") +
  geom_segment(data = subset(fmod, Score == "biplot")[c(1:5),],
               aes(x = 0, y = 0, xend = CCA1 * 3, yend = CCA2 * 3), arrow = arrow(length = unit(1/2, 'picas')), colour = "gray24", size = 1) +
  geom_text(data = subset(fmod, Score == "biplot")[c(1:5),], 
            aes(x=CCA1 * 3,y=CCA2 *3,label=c("Hydroperiod", "Hoofprints", "Pool Area", "Redding Soil", "RDM")), size=5, colour = "gray24") + xlab("CCA1") + ylab("CCA2")+
  
  geom_segment(data = subset(fmod, Score == "centroids")[1,],
               aes(x = 0, y = 0, xend = CCA1, yend = CCA2), arrow = arrow(length = unit(1/2, 'picas')), colour = "gray24", size = 1) +
  geom_text(data = subset(fmod, Score == "centroids")[1,], 
            aes(x=CCA1,y=CCA2,label="Corning Soil"), size=5, colour = "gray24") + xlab("CCA1") + ylab("CCA2")+
  
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))


p4


# END ---------------------------------------------------------------------





#####PCA#####
qdata1<-qdata[,-c(1:14)]
qdata1<-(!is.na(qdata1))
quad.pca<-rda(qdata1)
quad.pca
ordiplot(quad.pca)
biplot(quad.pca)
dim(qdata1)

quad.ca<-cca(qdata1[-93,])
quad.ca
chisq.test(qdata1/sum(qdata1))

plot(quad.ca, display=c('sites', 'sp')) #'sp' only displays labels, why can't both be displayed at the same time?
p0<-plot(quad.ca, choices = c(1, 2), display = c("sp", "wa"),
         scaling = 2)
identify(p0, "species")

p1<-plot(quad.ca, dis='sp', type='n')

mod<-decorana(qdata1)
stems<-colSums(qdata1)
plot(mod, dis='sp', type='n')
sel<-orditorp(mod, dis='sp', priority=stems, pcol='grey', pch='+' )


###fitting environmental variables####

env_quad1<-
  tibble(
    Pool.ID=qdata$Pool.ID,
    Grazing=qdata$Grazing,
    In_Days=qdata$Calibrated.Total.Days,
    HoofCount=qdata$Hoofprint,
    RDM=qdata$RDM,
    # Catchment=qdata$Catchment, can't do because of NAs
    Soil=qdata$SoilType,
    Size=qdata$Size
  )

#env_quad1<-full_join(env_quad1, pool_depth, by="Pool.ID")#stuck random values in for C3-17 and D5-08, measure soon
#env_quad<-env_quad[-c(112:nrow(env_quad)),]
ef<-envfit(quad.mds2, env_quad1, permu=999, na.rm=TRUE)

ef
plot(quad.mds2, display='sites')
plot(ef, p.max=0.1)

###CCA####
install.packages('ggfortify'); library('ggfortify')
install.packages('ggvegan', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('dplyr',  dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('tidyverse'); library('tidyverse')
library('ggvegan')

quad<-cca(qdata1~In_Days+HoofCount+Grazing+Size+Soil+RDM, env_quad1)#will want to condition on year for transects
quad
plot(quad)
#the total inertia is decomposed into constrained and unconstrained components
#'proportion of inertia' doesn't really have a clear meaning in CCA (not lioke in RDA)

#test the significance of the model by permuting the data randomly and refitting the model
#when the constrained inertia in permutations is always lower than the oberved constrained inertia, the constraints are significant
anova(quad)
#don't pay attention to F ratio
#test significance of the variables
anova(quad, by='term', step=200)
anova(quad, by='margin', perm=500)
anova(quad, by='axis', perm=1000)# Only first axis is significant
##Try conditioning the CCA on days of inundation, looking just at grazing
quad2<-cca(qdata1~Grazing+ Condition(HoofCount), env_quad1)
anova(quad2)
anova(quad2, by='margin', perm=500)

quad3<-cca(qdata1~Grazing, env_quad1)

#the effect of grazing is independent from inundation days

#constrained-- inertia explained by your variables
#compare the constrained inertia in the conditioned vs. non-conditioned model

quad3<-cca(qdata1~Grazing, env_quad1)#Grazing alone
anova(quad3)
#can sub in grazing treatment for hoofprint
#Grazing has an effect even when when variation due to inundation days is removed
#butttt, the variables of hoofprint and days of inundation are linearly dependent
with(env_quad, anova(quad, strata=In_Days))


#Plot CCA#
fmod <- fortify(quad)
size <- 1.8

p4<-ggplot(fmod, aes(x = CCA1, y = CCA2, label=Label)) +
  geom_text(data = subset(fmod, Score == "species"),
            colour = 'grey66', size = 6) +
  geom_point(data = cbind(subset(fmod, Score == "sites"), Grazing = env_quad1$Grazing),
             aes(colour = Grazing), size = 6) +
  scale_colour_brewer("Score", palette = "Set2") +
  coord_fixed() +
  theme(legend.position = "top") +
  geom_segment(data = subset(fmod, Score == "biplot")[c(1:5),],
               aes(x = 0, y = 0, xend = CCA1 * 3, yend = CCA2 * 3), arrow = arrow(length = unit(1/2, 'picas')), colour = "gray24", size = 1) +
  geom_text(data = subset(fmod, Score == "biplot")[c(1:5),], 
            aes(x=CCA1 * 3,y=CCA2 *3,label=c("Hydroperiod", "Hoofprints", "Pool Area", "Redding Soil", "RDM")), size=6, colour = "gray24") + xlab("CCA1") + ylab("CCA2")+

  geom_segment(data = subset(fmod, Score == "centroids")[1,],
               aes(x = 0, y = 0, xend = CCA1, yend = CCA2), arrow = arrow(length = unit(1/2, 'picas')), colour = "gray24", size = 1) +
  geom_text(data = subset(fmod, Score == "centroids")[1,], 
            aes(x=CCA1,y=CCA2,label="Corning Soil"), size=6, colour = "gray24") + xlab("CCA1") + ylab("CCA2")+
  
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

















