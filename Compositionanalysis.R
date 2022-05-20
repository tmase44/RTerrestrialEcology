library(tidyverse)
library(dplyr)

changi<-read.csv("Composition.csv")
ls(changi)
unique(changi$Object)
view(changi)

changi$Object<-factor(changi$Object)

#detections
# https://www.frontiersin.org/articles/10.3389/fevo.2021.671492/full

# detection rates----

#prepare data
a<-changi %>% 
  select(id,Object) %>% # these fields 
  group_by(id,Object) %>% # grouping
  summarise(n = n_distinct(Object)) # for identification
view(a)

#calculate rates
b<-a %>% 
  spread(key=id,value=n) %>% # spread data set
  replace(is.na(.), 0) %>%  #replace NA
  mutate(detections = rowSums(across(where(is.numeric)))) %>% 
    mutate(detection_rate =detections/6) %>% 
  arrange(desc(detection_rate)) %>% 
  relocate(detection_rate, .after = Object)
print(b)

b %>% 
  ggplot(aes(detection_rate,Object,color=detection_rate))+
  geom_point()

# total species / species richness

unique(changi$Object) # by name
n_distinct(changi$Object) # by qty
# 29 is the species richness

# proportions----

porp<-changi %>%
  group_by(id,Object)%>%
  summarize(n=n())%>%
  mutate(freq=n/sum(n)*100)
view(porp)

porp2<-porp %>% 
  group_by(Object) %>% 
  summarize(average_obs=mean(n),
            average_freq=mean(freq)) %>% 
  arrange(desc(average_obs))

# changing the above for vegan analysis
counts<-porp %>% 
  select(-freq) %>%  
  spread(key=Object,value=n) %>% 
  replace(is.na(.), 0)# spread data set
view(counts)

# average daily population----

counts_2<-porp%>% 
  select(-freq) %>%  
  spread(key=id,value=n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total_count = rowSums(across(where(is.numeric)))) %>% 
  mutate(avg_count = total_count/6) %>% 
  arrange(desc(avg_count))
view(counts_2)

counts_2.1<-porp%>% 
  select(-freq) %>%  
  spread(key=Object,value=n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total_count = rowSums(across(where(is.numeric)))) %>% 
  mutate(avg_count = total_count/6)
view(counts_2.1)

freq<-porp%>% 
  select(-n) %>%  
  spread(key=Object,value=freq) %>% 
  replace(is.na(.), 0)
view(freq)

# chart all species count and propoertion
ggplot(porp,aes(freq,n,color=Object))+
  geom_point()+
  facet_wrap(~Object,ncol = 5)+
  theme(legend.position = "none")+
  labs(x="proportion",y="n observations")
        
# aggregated
ggplot(porp2,aes(average_freq,average_obs))+
         geom_point()+
  geom_text(data=subset(porp2, average_freq > 5),
            aes(average_freq,average_obs,label=Object))+
  theme(legend.position = "none")
  

#ALPHA biodiversity----
library(vegan)
#use dataset: counts and convert plots to row names
# I have to rename the rows to be the plots
counts3<-counts %>% 
  remove_rownames %>% 
  column_to_rownames(var="id")
view(counts3)

#
#Richness----
fun.1<-function(x){sum(x>0)}
ch_richness<-apply(counts3, 1, FUN=fun.1)
richness<-data.frame(ch_richness)
colnames(richness)<-"Richness"
view(richness)

#Shannon index----
for (counts3.row in 1:6)
{shannon<- matrix(diversity(counts3[,], index = "shannon"))}
shannon<-round(shannon,3)
#Adjusting output names of rows and columns
row.names(shannon)<-row.names(counts3)
colnames(shannon)<-"Shannon score"
view(shannon)

#Simpson index----
for (counts3.row in 1:6)
{simpson<- matrix(diversity(counts3[,], index = "simpson"))}
simpson<-round(simpson,3)
#Adjusting the names of rows and columns
row.names(simpson)<-row.names(counts3)
colnames(simpson)<-"Simpson"
view(simpson)

#Putting together all indices
indices<-cbind(richness, shannon, simpson)
indices<-data.frame(indices)
View(indices)
indices$Richness<-factor(indices$Richness)


library(ggplot2)
p<-ggplot(data=indices,aes(x=Simpson,y=Shannon.score,
                           label=row.names(indices))) +
  geom_point(aes(color=Richness), size=4) +
  geom_text(hjust=-0.2,vjust=0.1)
p  
p<-p+ylim(1.5,2.5)+xlim(0.72, 0.88)             

