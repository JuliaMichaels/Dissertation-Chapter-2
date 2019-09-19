#Load packages
install.packages('tidyverse'); install.packages('vegan'); install.packages("ggrepel"); install.packages('ggplot2');install.packages('dplyr'); install.packages('scales')
install.packages('viridis'); install.packages('lme4'); install.packages('lmerTest'); install.packages('colorspace'); install.packages('backports'); install.packages('stringi')
install.packages('gridExtra'); install.packages('ggvegan'); install.packages('MuMIn')
install.packages("rpart"); install.packages("party"); install.packages("partykit")
library("rpart"); library("party"); library("partykit")
library('backports'); library('stringi'); library('vegan'); library('tidyverse'); library('ggplot2'); library('MuMIn')

#Load data
data_loggers_2018<-read.csv('2018_Levelloggers.csv')
staff_gauge_2018<-read.csv('2017-2018 Staff Gauges All.csv')
data_loggers_2017<-read.csv('2017_Levelloggers.csv')
staff_gauge_2017<-read.csv('2016-2017 Staff Gauges.csv') 
data_loggers_2016<-read.csv('2016_Levelloggers.csv')
staff_gauge_2016<-read.csv('2015-2016 Staff Gauges All.csv')
qdata<-read.csv("2017-2018 Vegetation Quadrats.csv",fileEncoding="UTF-8-BOM")
specid<-read.csv("SpecID.csv", fileEncoding="UTF-8-BOM")

# Calculate Total Inundation days ---------------------------------------------------------
# 2018 Calculate total days of inundation from data loggers 
DL2018<-data_loggers_2018%>%              #Average hourly datalogger data by day
  group_by(Date) %>% 
  summarise_all(funs(median)) 

dl_days_2018<-c()
for(i in 2:ncol(DL2018)){
  dl_days_2018[i]<-sum(DL2018[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}

dl_days_2018<-tibble(dl_days_2018, Pool.ID=colnames(DL2018))

# 2018 Pull out subset of dates from dataloggers when staff gauges were also taken

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
subset_2018[is.na(subset_2018)] = 1 #make all na vals (days we didnt check staff gauge) 1 because we cant assume it was dry
#hydrograph_2018<-subset_2018 #set aside for hydrograph
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
  summarize(days=mean(total_days_2018)) #if there is staff gauge and data logger, take the mean of the two measurements 

inundation_days_2018$Year<-'2018'


# 2017 Calculate total days of inundation from data loggers

# 2017 Pull out subset of dates from dataloggers when staff gauges were also taken
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
inundation_days_2017$Year<-'2017'


# 2016 Calculate total days of inundation from data loggers
DL2016<-data_loggers_2016%>%              #Average hourly datalogger data by day
  group_by(Date) %>% 
  summarise_all(funs(median)) 

dl_days_2016<-c()
for(i in 2:ncol(DL2016)){
  dl_days_2016[i]<-sum(DL2016[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}


dl_days_2016<-tibble(dl_days_2016, Pool.ID=colnames(DL2016))


# 2016 Pull out subset of dates from dataaloggers when staff gauges were also taken

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
subset_2016[is.na(subset_2016)] = 1 #make all na vals (days we didnt check staff gauge) 1 because we cant assume it was dry
#hydrograph_2016<-subset_2016 #set aside for hydrograph
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
inundation_days_2016$Year<-'2016'


# combine all years inundation 
all_years<-full_join(inundation_days_2016, inundation_days_2017)
all_years<-full_join(all_years, inundation_days_2018)
all_years$Year<-as.factor(all_years$Year)

all_data<-full_join(all_years, qdata) %>% 
  filter(Pair %in% c(1:12))

#pull out just year 2018 and add to 2018 quadrat data
all_data_2018<-all_data %>% 
  filter(Year==2018)

#Calculate diversity indices 

#species richness
spec_rich<-c()
for(i in 1:nrow(all_data_2018)){
  spec_rich[i]<-sum(all_data_2018[i,-(1:15)]>0)
}

#shannon weiner
shannon <- c()
for(i in 1:nrow(all_data_2018)){
  temp <- as.numeric(all_data_2018[i,-(1:15)])
  shannon[i] <- diversity(temp)
}


#Calculate the relative cover of natives
natives <- all_data_2018[,colnames(all_data_2018) %in% specid$SpeciesIDCode[specid$Status == 'Native']]

total_cov_nat<-c()
for(i in 1:nrow(natives)){
  total_cov_nat[i]<-sum(natives[i,1:ncol(natives)])
}

total_cov <- c()
for(i in 1:nrow(all_data_2018)){
  temp <- as.numeric(all_data_2018[i,-(1:15)])
  total_cov[i] <- sum(temp)
}

rel_cov_nat<-c()
for(i in 1:nrow(all_data_2018)){
  rel_cov_nat[i]<-total_cov_nat[i]/total_cov[i]
}

rel_cov_nat[is.nan(rel_cov_nat)] <- 0

all_data_2018_info<-all_data_2018 %>% 
  dplyr::select(Pool.ID:Inundation.Type)


#create data frame with all community metrics
transition_diversity <- data.frame(all_data_2018_info, spec_rich,shannon, rel_cov_nat)
colnames(transition_diversity)[colnames(transition_diversity)=="spec_rich"]<- "Species Richness"
colnames(transition_diversity)[colnames(transition_diversity)=="shannon"]<- "Shannon Diversity"
colnames(transition_diversity)[colnames(transition_diversity)=="rel_cov_nat"]<- "Relative Cover of Natives"


# Plot specrich, shannon and native cover by grazing ----------------------

#change data frame format to make it easier to graph
trans_diversity<-gather(transition_diversity, "Metric", "Value", 16:18)

transition_diversity_plot<- trans_diversity %>%
  group_by(Pool.ID, Metric, Grazing, days) %>% 
  summarize(Value=mean(Value))

plot<-transition_diversity_plot%>% 
  ggplot(aes(x=Grazing, y=Value, fill=Grazing))+
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



# Plot inundation over all three years ------------------------------------

all_years<-all_data%>% 
  group_by(Pool.ID, Grazing, Year) %>% 
  summarize(days=mean(days))

all_years<-all_years%>% 
  group_by(Grazing, Year) %>% 
  summarize(days1=mean(days, na.rm = TRUE), sd1=sd(days, na.rm = TRUE), 
            sem1 = sd(days, na.rm = TRUE)/sqrt(length(days)))

all_years$Year<-as.factor(all_years$Year)

pd <- position_dodge(0.01)

plot<-ggplot(data=all_years, aes(x=Year, y=days1, group=Grazing))+
  geom_line(aes(color=Grazing), size=1.5)+
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
  annotate("text", x = 1, y =30, label = "43.60 cm precip
           87.3% of 21-year avg", size=5)+
  annotate("text", x = 2, y =30, label = "93.96 cm precip
           188.00% of 21-year avg",size=5)+
  annotate("text", x = 3, y =30, label = "52.29 cm precip
           105.7% of 21-year avg", size=5)
plot



# Plot diversity by grazing and hoofprints -------------------------
#species richness
ggplot(data=transition_diversity, aes(x=Hoofprint,y=`Species Richness`, colour=Grazing))+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Species Richness by Hoofprint Count and Grazing Duration", y="Species Richness", x="Hoofprint Count")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))


#shannon
ggplot(data=transition_diversity, aes(x=Hoofprint,y=`Shannon Diversity`, colour=Grazing))+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Species Richness by Hoofprint Count and Grazing Duration", y="Shannon Diversity", x="Hoofprint Count")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

#relative cover of natives
ggplot(data=transition_diversity, aes(x=Hoofprint,y=`Relative Cover of Natives`, colour=Grazing))+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Relative Cover of Natives by Hoofprint Count and Grazing Duration", y="Relative Cover of Natives", x="Hoofprint Count")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

# Plot diversity by grazing and inundation -------------------------
transition_diversity_avg<-transition_diversity %>% 
  group_by(Pool.ID, Grazing) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

#species richness
ggplot(data=transition_diversity_avg, aes(x=days,y=`Species Richness`, colour=Grazing))+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Species Richness by Grazing and Inundation length", y="Species Richness", x="Days of Inundation")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

#Shannon Diversity
ggplot(data=transition_diversity_avg, aes(x=days,y=`Shannon Diversity`, colour=Grazing))+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Shannon Diversity by Grazing and Inundation length", y="Shannon Diversity", x="Days of Inundation")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))


#Relative Cover of Natives
ggplot(data=transition_diversity_avg, aes(x=days,y=`Relative Cover of Natives`, colour=Grazing))+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Relative Cover of Natives by Grazing and Inundation length", y="Shannon Diversity", x="Days of Inundation")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))



# Check for a size difference between treatments ---------------------------------------------------------
ggplot(transition_diversity, aes(x=Grazing, y=Size))+
  geom_boxplot()
summary(aov(Size~Grazing, transition_diversity))


# CIT test ----------------------------------------------------------------

#set model to univariate
univ<-ctree_control(testtype="Univariate")

#species richness
CIT.test.specrich<-ctree(spec_rich ~ Grazing+ days+ Hoofprint + SoilType+ Size+ Depth+Catchment, data=transition_diversity, control=univ)
plot(CIT.test.specrich)

#shannon
CIT.test.shannon<-ctree(shannon ~ Grazing+ days+ Hoofprint + SoilType+ Size+ Depth+Catchment, data=transition_diversity, control=univ)
plot(CIT.test.shannon)

#relative native cover
CIT.test.native<-ctree(rel_cov_nat ~ Grazing+ days+ Hoofprint + SoilType+ Size+ Depth+Catchment, data=transition_diversity, control=univ)
plot(CIT.test.native)



# Linear mixed effects models ---------------------------------------------

#species richness
m1<-lmer(spec_rich ~ Grazing*Hoofprint+ Size+ (1|Pool.ID), data=transition_diversity)#fixed-effect model matrix is rank deficient so dropping 1 column / coefficient
m2<-lmer(spec_rich ~ Grazing+ Hoofprint+ Size+ (1|Pool.ID), data=transition_diversity)#removing interaction terms
anova(m1) #interaction terms didn't really make a difference
anova(m2)
AIC(m1,m2)#m2 is lower AIC

#shannon
m1<-lmer(shannon ~ Grazing*Size+Hoofprint+Catchment+ (1|Pool.ID), data=transition_diversity)
m2<-lmer(shannon ~ Grazing+Hoofprint+ Size+ Catchment+ (1|Pool.ID), data=transition_diversity)
anova(m1) #interaction terms didn't really make a difference
anova(m2)
AIC(m1,m2)#m2 is lower AIC

#native cover
m1<-lmer(rel_cov_nat ~ days*Hoofprint* Size+ (1|Pool.ID), data=transition_diversity)#Some predictor variables are on very different scales: consider rescaling 
m2<-lmer(rel_cov_nat ~ days+Hoofprint+ Size+ (1|Pool.ID), data=transition_diversity)
anova(m1) #interaction terms didn't really make a difference
anova(m2)
AIC(m1,m2)#m2 is lower AIC

