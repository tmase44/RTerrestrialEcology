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

# porps

porp<-changi %>%
  group_by(id,Object)%>%
  summarize(n=n())%>%
  mutate(freq=n/sum(n)*100)
view(porp)

# changing the above for vegan analysis
counts<-porp %>% 
  select(-freq) %>%  
  spread(key=Object,value=n) %>% 
  replace(is.na(.), 0)# spread data set
view(counts)

#basic ecological analysis----
library(vegan)

