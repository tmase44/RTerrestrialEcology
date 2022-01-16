#Setup---
# https://librarysearch.stir.ac.uk/discovery/fulldisplay?docid=alma991004705879706861&context=L&vid=44UST_INST:VU1&lang=en&search_scope=ALMA_NO_JOURNALS&adaptor=Local%20Search%20Engine&tab=LibraryCatalog&query=any,contains,r%20ecology&offset=0
# file:///C:/Users/tmaso/OneDrive/Msc%20Environmental%20Management/R/Lakicevic2020_Book_IntroductionToRForTerrestrialE.pdf

library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape)
library(vegan)
install.packages("vegan")

#Creating vectors----
id<-1:12
species<-paste("Species", 1:12)
size<-c(50,25,30,45,2,70,22,20,10,45,22,56)
altitude<-c(rep("0-500",2), rep("501-1000",3), rep("1001-1500",5), rep("0-500",2))
protection<-c(rep(T, each=5), rep(F, each=7))

# create DF
df<-data.frame(id, species, size, altitude, protection)
df
View(df)

# subsetting data----
df_subset<-subset(df,size>=50)
View(df_subset)

# multiple criteria subset
subset(df, size>10 &(altitude=="0-500"|altitude=="501-1000")&protection==T)

#removing elements from a dataframe
  #remove a row
df[-12,] # removes 12th row
df[-c(1:7,10),] #removes 1 to 7 and 10

#remove cols based on name
subset(df,select = -c(altitude,protection))

#remove cols and rows at the same time
df[-(2:9),-c(1,3)] #removes rows 2 to 9 and rows 1 and 3df
# to keep only cols 1 and 3, just remove the "-" before "c"

#Adding elements to a data frame----
# This will add the year 2018, 2019 repeated 6x
year<-c(rep(2018:2019,6))

#append to the DF
cbind(df,year) #or
data.frame(df,year)

#Merge DF's----

# COLUMNS
  #create new df to merge 
df.category<-data.frame(id=1:12,category=c("a","b","c"))
df.category
  # merge
merge(df,df.category,by="id")

# ROWS
df.row<-apply(df,2,min) #this row contains the lowest value in each column
df.row
#this creates 1 row, it can be added using rbind
rbind(df,df.row)

# or as a list, create the list
row.list<-list(15,"species 8",28,"501-1000",T)
row.list
#bind it
rbind(df[1:3,],row.list) #this binds it with only entries 1,3 for example

# OPERATIONS----

#cut function
size.group<-cut(df$size,breaks=3,
                labels = c("small","medium","large"))
size.group
cbind(df,size.group)
# in this example R auto-defines small, med and large
  # but we can define our own thresholds
size.group2<-cut(df$size,breaks=c(0,30,100), #2 groups are make <=30 and bwteen 30-100.
                 labels=c("regular","remarkable"))
size.group2
cbind(df,size.group2)

#Manipulating the dataframe----

  #find max population size for protected species
protected<-subset(df,protection==T)
max(protected$size)
protected

  #count species with protected status
length(which(df$protection==T))
    # and in a specific range
length(which(df$protection==T & df$altitude=="0-500"))

  #max pop size in each range
df3<-aggregate(df$size,by=list(altitude),FUN=max)
df3
   #name the columns
names(df3)<-c("altitude","max pop size")
   #reorder the data
df3[order(df3$`max pop size`),]

#calculate avg pop size by protection status
df4<-aggregate(df$size,by=list(protection),FUN=mean)
names(df4)<-c("protection","avg pop size")
df4

#MISSING DATA----
  # for demo, replace pop sizes of 45 with NA
df$size[df$size==45]<-NA
df.na<-df
df.na

# NA data means tha certain sums will not work, for example
sum(df$size)
# NA values need to be replaced
sum(df$size,na.rm=T)
  # na remove sums up all the values excluding NA

# in order to list all rows with NA (missing data)
df[!complete.cases(df),]

#Replacing missing values----
#Creating a vector with complete cases
size.complete<-df$size[complete.cases(df$size)]
size.complete
#Finding a mode value for complete cases
size.complete<-factor(size.complete)
mode<-rev(sort(summary(size.complete)))
mode<-mode[1]
mode
#Filling missing elements with the mode value
df$size[!complete.cases(df$size)]<-22
df$size
df # NA's have been placed with 22

#Finding the median value of complete cases
median<-median(df$size, na.rm=T)
median
#Filling missing elements with the median value
df$size[!complete.cases(df$size)]<-median
df$size
df # NA's have been placed with 22

# "Safe" NA threshold----
  #There is a conventional threshold regarding the number 
  #of missing values in a row or column that should be taken 
  #into account. If we have large datasets, a threshold of 5% 
  #missing values is considered “safe.” 
  #For cases in which there are more than 5% missing values, 
  #it is recommended to discard that column or row from the data

#Checking percentage of missing elements in a column
per.missing<-function(x){sum(is.na(x))/length(x)*100}
apply(df.na, 2, per.missing)
# we have 16% data NA. This a big issue because this is a small dataset

#Checking percentage of missing elements in a row
miss.rows<-apply(df.na, 1, per.missing)
cbind(id, miss.rows)
  # shows that 20% of elements have missing data in THE ROW
   # 1 column out of 5


#OUTLIER DATA----
# transforming outliers----

#Measured height of poplar trees [meters]
tree.height.meters<-c(27,30,32,28,35,42,27,150)
  # here we have outlier data of 150
boxplot(tree.height.meters, ylab = "height [m]")
  # if we think the outlier is invalid we can just remove it, but if it valid
  # we can log-transform the data
#Transforming values using log funtion
log.tree.height<-log(tree.height.meters)
log.tree.height<-round(log.tree.height,2)
log.tree.height
#Plotting transformed values
plot(log.tree.height, xlab="measurement")

# We can ise this ^ to compare the inital and logtransformed data to show the effect
#Creating a matrix (initial and transformed values)
it<-matrix(c(tree.height.meters,log.tree.height), ncol=2, byrow=F)
colnames(it)<-c("initial", "transformed")
it
#Defining multiple functions
multi.fun <- function(x) {c(mean = mean(x),
            median = median(x), sd = sd(x))}
#Apply multiple functions
apply(it,2, multi.fun)

# removing outliers----
initial<-tree.height.meters # first, rename the dataset in question
rm.out<-initial[!initial%in%boxplot(initial)$out]
rm.out
# now the boxplot is reasonable
boxplot(rm.out, ylab="height [m]")
# with SUMMARY i can pull descriptive data
summary(rm.out)

# BASIC ECOLOGICAL ANALYSIS----
getwd()
list.files()
pdata<-read.csv("pdtest.csv")
pdata$Type1<-pdata$Type.1
pdata<-select(pdata,-Type.1)
View(pdata)
dim(pdata)
str(pdata)
unique(pdata$`Type1`)

# create a new DF subset for all Fire Types
fire<-subset(pdata,`Type1`=="Fire")
View(fire)

# back to pdata, make Type 1 a factor
pdata$`Type1`<-factor(pdata$`Type1`)
summary(pdata$`Type1`) # see totals per Type1

#calculate biological spectrum
biospec<-prop.table(summary(pdata$'Type1'))*100
biospec# results are as a %
biospec[rev(order(biospec))] # this orders HI-LOW
# transform the results into a matrix!
biospec<-as.matrix(biospec)
labels<-paste(c(rownames(biospec)),":",
              biospec[,1])
labels<-paste(labels,"%",sep="")
#pie
pie(biospec,labels=labels,col=topo.colors(18))

typechart<-pdata %>% 
  count(Type1) %>% 
  mutate(freq=n/sum(n)*100) %>% 
  arrange(desc(freq))

# DPLYR CAN ALSO WORK OUR PROPORTION WITHOUT SUBSETTING----
pdata %>%
  group_by(Type1)%>%
  summarize(n=n())%>%
  mutate(freq=n/sum(n)*100)
# or BETTER BECAUSE DECIMALS ARE EQUAL!!!

View(typechart)
# Then chart
ggplot(typechart,aes(Type1,freq))+
  geom_col()
# order and formatting
ggplot(typechart,aes(reorder(Type1,-freq,sum),freq,fill=Type1))+
  geom_col()+
  geom_text(aes(label=freq),vjust=-0.5)+
  labs(x="frequency(%)",
       y = "Power Type",
       title = "Population Distribution by Type")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45),
        panel.background = element_blank(),
        panel.grid = element_line(colour="grey92"))

#YOY proportional changes----
  # expand the data set with count data for 2 years
pdata2<-pdata
dim(pdata2)
pdata2$Y2020<-rnorm(800,50,10)
pdata2$Y2020<-round(pdata2$Y2020,0)
pdata2$Y2021<-rnorm(800,60,10)
pdata2$Y2021<-round(pdata2$Y2021,0)

# now mark the changes using MUTATE + IFELSE
pdata3<-pdata2 %>% 
  mutate(change = ifelse(pdata2$Y2020==pdata2$Y2021,"no change",
                         ifelse(pdata2$Y2020<pdata2$Y2021,"increase",
                                "decrease")))
View(pdata3)

# count the types of change
pdata3 %>% count(change)
# stacked bar to compare YOY
#To graphically present the number of species in 2014 and 2019, we create two different plots: 
#stacked and grouped bar. When defining values to be placed on the x and y axes, we need to have 
#unique values, so we cannot specify two different columns to be printed on the y axis. Therefore, we 
#need to make some adjustments to the data frame df before printing the results. In fact, we need to 
#create a new data frame with one column showing the number of individuals in which the first half of 
#rows show the number of individuals in 2014 and then the second half the number in 2019. This transformation is easily done with the function “melt” from the package “reshape”:

#DPLYR gather and spread can be used----
pdatagather<-pdata2 %>% 
  gather(key="Year",value="Sightings",3:4)

# summary stats (individual pokemon too many data)
pdsum<-pdatagather %>% 
  group_by(Type1,Year) %>% 
  summarize(Total_count = sum(Sightings))
#chart
pdsum %>% 
  ggplot(aes(Year,Total_count,fill=Type1))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=Total_count),size=4,
            position=position_stack(vjust=0.5))
  
# now to show the % share of each Type per year
pdsum<-pdsum %>% 
group_by(Year) %>% 
  mutate(percent = Total_count/sum(Total_count)*100)
pdsum$percent<-round(pdsum$percent,2)
# chart
pdsum %>% 
  ggplot(aes(Year,percent,fill=Type1))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=percent),size=4,
            position=position_stack(vjust=0.5))

# data can be shown as a grouped bar
pdsum %>% 
  ggplot(aes(Type1,Total_count,fill=Year))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(label=Total_count),
            position=position_dodge(width=0.9),vjust=-0.5,size=3)+
  scale_y_continuous(expand = expansion(mult = c(0, .1))) # Tip: remove x-ax gap----
  # and the percentage
pdsum %>% 
  ggplot(aes(reorder(Type1,-percent,sum),percent,fill=Year))+ #ordered by hi-low-2021
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(label=percent),
            position=position_dodge(width=0.9),vjust=-0.5,size=3)+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  labs(x="Type",y="Percentage of all types in given year")

#facet wrapping
  # split by species YOY
pdsum %>% 
  ggplot(aes(Year,percent,fill=Year))+
  geom_bar(stat="identity")+
  facet_wrap(~Type1)

# split by year species
pdsum %>%
  ggplot(aes(Type1,percent,fill=Type1))+
  geom_bar(stat="identity")+
  facet_wrap(~Year)

# that is a bit crowded, we can subset in the ggplot to make it nicer
ggplot(subset(pdsum,Type1 %in% c("Dark","Dragon","Ghost","Poison","Psychic")),aes(Type1,percent,fill=Type1))+
  geom_bar(stat="identity")+
  facet_wrap(~Year)

# BIODIVERSITY INDICES----
  # alpha indices measure species in a particular area
  # beta indices compare similarities between different areas

# Shannon and Simpson indices
  # calculating in base R----

# ! SHANNON INDEX----
  # p.i = proportion  of species (i)n the community
  # s = total number of species in the community
# calculate  p
p<-round(prop.table(typechart$n),3)
# add p to the dataframe
typechart<-cbind(typechart,p)
# calculate In(p)
in.p<-round(log(x=p),3)
# add in(p) to the dataframe
typechart<-cbind(typechart,in.p)
# multiply p * in.p, add column to the dataframe
typechart<-cbind(typechart, multipl=round(p*in.p,3))
typechart
# To calculate the Shannon index: 
  # sum the values in the column “multipl” 
    #and multiply it by -1
shannon<-sum(typechart$multipl)*(-1)
  # result = 2.716
# index usually ranges between 1.5-3.5
  # index above 3.5 and 4 suggests high biodiversity

# ! SIMPSON INDEX----
  # p.i = proportion  of species (i)n the community
  # s = total number of species in the community
# calculate  p
p<-round(prop.table(typechart$n),3)
# calculate p * p
p2<-round(p*p,3)
# To calculate Simpson index:
simpson<-1-sum(p2)
simpson
# index ranges 0 - 1 (1 is higher biodiversity)

  # calculating using VEGAN package----

# data prep
  # here i have done SUBSET & SPREAD to transform the data
pdsum2<-pdsum %>% 
  select(Type1,Year,Total_count) %>% 
  filter(Type1 %in%c("Dark","Dragon","Ghost","Poison","Psychic")) %>% 
  group_by(Year) %>% 
  spread(key="Type1",value="Total_count")
# I have to rename the rows to be the years
pdsum2<-pdsum2 %>% 
  remove_rownames %>% 
  column_to_rownames(var="Year")

# ! RICHNESS----
  # is an ALPHA biodiversity index (Fedor and Zvaríková 2019)
#Calculating the richness
fun.1<-function(x){sum(x>0)} 
richness<-apply(pdsum2, 1, FUN=fun.1)
richness
# result shows that there are 5 species found in each group (in this case, each year)

# ! VEGAN SHANNON INDEX----
  # command "for" makes a loop
#Calculating the Shannon index
for (pdsum2.row in 1:2)
{shannon<- matrix(diversity(pdsum2[,], index = "shannon"))} 
shannon<-round(shannon,3)
shannon
#rename rows and cols
row.names(shannon)<-row.names(pdsum2) 
colnames(shannon)<-"Shannon"

# ! VEGAN SIMPSON INDEX----
#Calculating the Simpson index
for (pdsum2.row in 1:2)
{simpson<- matrix(diversity(pdsum2[,], index = "simpson"))} 
simpson<-round(simpson,3)
simpson
#rename rows and cols
row.names(simpson)<-row.names(pdsum2) 
colnames(simpson)<-"Simpson" 

# All 3 can be assembled
indices<-cbind(shannon,simpson,richness)
indices<-data.frame(indices)
indices

# TASK----
  # create the 3 indices for all the pokemon types
    # new DF with unique count of pokemon per type
      #use pdata2 as the base