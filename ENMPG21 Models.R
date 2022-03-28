#ENMPG21 logistic growth model and harvesting
#Nils Bunnefeld

#Simple logistic growth model
Nt<-10 # population at current time
r<-0.5 # pop. growth rate
K<-100 # carrying capacity of the population - limit of pop. growh
years<-100

for(i in 1:years){
  Nt[i+1]<-Nt[i]+Nt[i]*r*(1-Nt[i]/K)
}

plot(Nt,ylab="Population size",xlab="Time")


Nt<-10
r<-0.5
K<-100
years<-100
h<-0.1 # 

for(i in 1:years){
  Nt[i+1]<-Nt[i]+Nt[i]*r*(1-Nt[i]/K)-h*Nt[i]
}

plot(Nt,ylab="Population size",xlab="Time")

