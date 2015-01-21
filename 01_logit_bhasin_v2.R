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

# Create lagged values
lags<-data.frame(cbind(yogurt$Pan.I.D.,Y))
lags <- lags %>%
  group_by(yogurt$Pan.I.D.) %>%
  mutate_each(funs(lag=lag(.,1,default=0)), contains("Brand"))    
lags<-data.frame(lags) # ToDo: Why is DPLYR changing object type
Xl<-select(lags,Brand.1:Brand.4)



# ---------------------------
# WEEK 2 PRODUCT CHARACTERISTICS With Lag 
# ---------------------------

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