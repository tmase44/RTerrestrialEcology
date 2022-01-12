#Setup---
# https://librarysearch.stir.ac.uk/discovery/fulldisplay?docid=alma991004705879706861&context=L&vid=44UST_INST:VU1&lang=en&search_scope=ALMA_NO_JOURNALS&adaptor=Local%20Search%20Engine&tab=LibraryCatalog&query=any,contains,r%20ecology&offset=0
# file:///C:/Users/tmaso/OneDrive/Msc%20Environmental%20Management/R/Lakicevic2020_Book_IntroductionToRForTerrestrialE.pdf

library(dplyr)
library(ggplot2)

#Creating vectors----
id<-1:12
species<-paste("Species", 1:12)
size<-c(50,25,30,45,2,70,22,20,10,45,22,56)
altitude<-c(rep("0-500",2), rep("501-1000",3), rep("1001-1500",5), rep("0-500",2))
protection<-c(rep(T, each=5), rep(F, each=7))

# create DF----
df<-data.frame(id, species, size, altitude, protection)
df
View(df)

# subsetting data (and making a separate DF)----
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
View(pdata)
dim(pdata)
str(pdata)
unique(`Type 1`)

# create a new DF subset for all Fire Types
fire<-subset(pdata,`Type 1`=="Fire")
View(fire)

# back to pdata, make Type 1 a factor
pdata$`Type 1`<-factor(pdata$`Type 1`)
summary(pdata$`Type 1`) # see totals per Type1

#calculate biological spectrum
biospec<-prop.table(summary(pdata$'Type 1'))*100
biospec# results are as a %
biospec[rev(order(biospec))] # this orders HI-LOW
