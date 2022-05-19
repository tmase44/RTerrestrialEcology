changi<-read.csv("Composition.csv")
ls(changi)
unique(changi$Object)
view(changi)

changi$Object<-factor(changi$Object)

#detections
# https://www.frontiersin.org/articles/10.3389/fevo.2021.671492/full

# detection rates----
a<-changi %>% 
  select(id,Object) %>% # these fields 
  group_by(id,Object) %>% # grouping
  summarise(n = n_distinct(Object)) # for identification
view(a)

b<-a %>% 
  spread(key=id,value=n) %>% # spread data set
  replace(is.na(.), 0) #replace NA
view(b)

# add detections rate
b<-b %>%
  mutate(detections = rowSums(across(where(is.numeric))))
# and detection rate
b<-b %>% 
  mutate(detection_rate =detections/6) %>% 
  arrange(desc(detection_rate)) %>% 
  relocate(detection_rate, .after = Object)
print(b)

b %>% 
  ggplot(aes(detection_rate,Object,color=detection_rate))+
  geom_point()

#basic ecological analysis----
library(vegan)
