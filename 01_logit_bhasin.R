setwd(getwd())
yogurt<-read.csv("Yogurt100N.csv")

param<-c(0,0,2,0.4,1)

Xf=yogurt[,grepl("Feature",names(yogurt))]
Xp=yogurt[,grepl("Price",names(yogurt))]
Y=yogurt[,grepl("Brand",names(yogurt))]


logLik <- function(param,Xf,Xp,Y) {
  a1=param[1];a2=param[2];a3=param[3];bf=param[4];bp=param[5];
  ev1=exp(a1+bf*Xf[,1]+bp*Xp[,1])
  ev2=exp(a2+bf*Xf[,2]+bp*Xp[,2])
  ev3=exp(a3+bf*Xf[,3]+bp*Xp[,3])
  ev4=exp(0+bf*Xf[,4]+bp*Xp[,4])
  den=ev1+ev2+ev3+ev4       
  p=data.frame(p1=ev1/den,p2=ev2/den,p3=ev3/den,p4=ev4/den)       
  pc=rowSums(Y*p)
  lpc=log(pc)
  ll=sum(lpc) 
  return(-ll)
}

logLik(param,Xf,Xp,Y)

nlm(f = logLik,p=param,Xf=Xf, Xp=Xp,Y=Y)
# ev=exp(mapply(`*`,Xp,param)))