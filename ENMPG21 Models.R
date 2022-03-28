#ENMPG21 logistic growth model and harvesting
#Nils Bunnefeld

#Simple logistic growth model----
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
h<-0.44 # control, e.g. harvest/hunt rate, 10% in this case

for(i in 1:years){
  Nt[i+1]<-Nt[i]+Nt[i]*r*(1-Nt[i]/K)-h*Nt[i]
}

plot(Nt,ylab="Population size",xlab="Time")

# max sustainable harvest----
  # r 0.5 
  # h 0.45 no pop growth
  # h 0.44 K at 12


# carrying capacity
  # before this calc.
    # max. density per ha / or per sqkm

# https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.1822



