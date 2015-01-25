setwd(getwd()); require (dplyr)
yogurt<-read.csv("Yogurt100N.csv")

# Select Indep and response variables
Xf<-yogurt[,grepl("Feature",names(yogurt))]
Xp<-yogurt[,grepl("Price",names(yogurt))]
Y<-yogurt[,grepl("Brand",names(yogurt))]

# Miscellany needed for creating model matrix
on <- rep(1,nrow(yogurt))
off <- rep(0,nrow(yogurt))
Xa <- cbind(c(on,off,off,off),
            c(off,on,off,off),
            c(off,off,on,off))
index=rep(1:nrow(yogurt),4)

# ---------------------------
# WEEK 3 SEGMENTS
# ---------------------------

pan<-yogurt[,grepl("Pan.I.D.",names(yogurt))]
pan<-rep(pan,4)
# Create log likelihood function to be minimized
logLik <- function(param,Xf,Xp,Y) {  
  m.1<-cbind(Xa, unlist(Xf, use.names=F), unlist(Xp, use.names=F)) #condense indep variables into one column
  ev.1<-exp(m.1 %*% param.1)  
  den.1<-tapply(ev.1,index,sum)
  den.1<-rep(den.1,4)
  p.1<-ev.1/den.1
  pc.1<-p.1*unlist(Y,use.names=F);pc.1=tapply(pc.1,index,sum)
  
  temp<-data.frame(cbind(pan,pc.1)) #create matrix so you can product over HH
  ll.1<- temp %>%
    group_by(pan) %>%
    summarise(ll=prod(pc.1))
  
### stopped here
  lpc<-log(pc)
  ll<-sum(lpc) 
  return(-ll)
}

param.1<-c(0,0,2,0.4,1); param.2<-param.1
logLik(param,Xf,Xp,Y)



# ---------------------------
# WEEK 2 PRODUCT CHARACTERISTICS With Lag 
# ---------------------------

# Create lagged values
lags<-data.frame(cbind(yogurt$Pan.I.D.,Y))
lags <- lags %>%
  group_by(yogurt$Pan.I.D.) %>%
  mutate_each(funs(lag=lag(.,1,default=0)), contains("Brand"))    
lags<-data.frame(lags) # ToDo: Why is DPLYR changing object type
Xl<-select(lags,Brand.1:Brand.4)




# Create log likelihood function to minimize
logLik <- function(param,Xf,Xp,Xl,Y) {  
  m<-cbind(Xa, unlist(Xf, use.names=F), unlist(Xp, use.names=F), unlist(Xl,use.names=F)) #condense indep variables into one column
  ev<-exp(m %*% param)  
  den<-tapply(ev,index,sum)
  den<-rep(den,4)
  p<-ev/den
  pc<-p*unlist(Y,use.names=F);pc=tapply(pc,index,sum)
  lpc<-log(pc)
  ll<-sum(lpc) 
  return(-ll)
}

param<-c(0,0,2,0.4,1,1)
logLik(param,Xf,Xp,Xl,Y)

model=nlm(f = logLik,p=param,Xf=Xf, Xp=Xp, Xl=Xl, Y=Y)
BIC <- model$minimum + length(param)*log(nrow(yogurt))

# ---------------------------
# WEEK 1 PRODUCT CHARACTERISTICS ONLY 
# ---------------------------

# Create log likelihood function to be minimized
logLik <- function(param,Xf,Xp,Y) {  
  m<-cbind(Xa, unlist(Xf, use.names=F), unlist(Xp, use.names=F)) #condense indep variables into one column
  ev<-exp(m %*% param)  
  den<-tapply(ev,index,sum)
  den<-rep(den,4)
  p<-ev/den
  pc<-p*unlist(Y,use.names=F);pc=tapply(pc,index,sum)
  lpc<-log(pc)
  ll<-sum(lpc) 
  return(-ll)
}

param<-c(0,0,2,0.4,1)
logLik(param,Xf,Xp,Y)

nlm(f = logLik,p=param,Xf=Xf, Xp=Xp,Y=Y)
# ev=exp(mapply(`*`,Xp,param)))