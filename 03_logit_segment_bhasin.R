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

# Create log likelihood function to be maximized
logLik <- function(param,Xf,Xp,Y) {  
  param.1<-param[1:5]; param.2<-param[6:10]; lambda<-param[11]
  
  ll.n <- function(param,Xf,Xp,Y){
    m<-cbind(Xa, unlist(Xf, use.names=F), unlist(Xp, use.names=F)) #condense indep variables into one column
    ev<-exp(m %*% param)  #all 4 evs stacked on top of each other
    den<-tapply(ev,index,sum) #sum where index is shared
    den<-rep(den,4) 
    p<-ev/den #4X the length with each p stacked on top of each other
    pc<-p*unlist(Y,use.names=F) #still 4X the length
    pc<-tapply(pc,index,sum) #no longer 4X
    
    temp<-data.frame(cbind(pan,pc)) #create matrix so you can product over HH
    ll<- temp %>%
      group_by(pan) %>%
      summarise(ll=prod(pc))  
    return(ll[,2]) #return segmented ll
  }
  
  ll.1<- ll.n(param=param.1,Xf=Xf,Xp=Xp,Y=Y) 
  ll.2<- ll.n(param=param.2,Xf=Xf,Xp=Xp,Y=Y)
  pi<-exp(lambda)/(1+exp(lambda))
  ll<-log(pi*ll.1+(1-pi)*ll.2)
  ll<-sum(ll) 
  return(-ll)
}

param=c(2,3,-0.5,1,-10,2,3,-0.5,1,-10,0.1)
logLik(param,Xf,Xp,Y)

fit_segment<-nlm(f = logLik,p=param,Xf=Xf, Xp=Xp,Y=Y)

solution<-fit_segment$estimate
names(solution) <- c("a11","a21","a31", "bf1","bp1",
                  "a12","a22","a32", "bf2","bp2",
                  "lambda")
BIC_segment <- 2*fit_segment$minimum + length(param)*log(nrow(yogurt))

# Calculate Brand Specific Elasticities 
elasticity <-function(param,Xf,Xp){
  param.1<-param[1:5]; param.2<-param[6:10]; lambda<-param[11]
  
  # This function is used often
  pp.n <- function(param,Xf,Xp){ 
    m<-cbind(Xa, unlist(Xf, use.names=F), unlist(Xp, use.names=F)) 
    ev<-exp(m %*% param)  
    den<-tapply(ev,index,sum) 
    den<-rep(den,4) 
    pp<-ev/den   
    return(pp)
  }
  
  #To Do: make this extensible to 2+ segments
  pp.1<- pp.n(param=param.1,Xf=Xf,Xp=Xp) 
  pp.2<- pp.n(param=param.2,Xf=Xf,Xp=Xp)
  pi<-exp(lambda)/(1+exp(lambda))
  ep<-pi*pp.1+(1-pi)*pp.2
  el<- (param.1["bp1"]*pp.1*(1-pp.1)*pi + param.2["bp2"]*pp.2*(1-pp.2)*(1-pi))*Xp/ep
  el<-colMeans(el)
  return(el)
}

elasticity(param=solution, Xf=Xf, Xp=Xp)

# ---------------------------
# WEEK 2 PRODUCT CHARACTERISTICS WITH LAG
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