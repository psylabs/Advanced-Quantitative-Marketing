# ---------------------------
# ADVANCED QUANTITATIVE MARKETING FINAL PROJECT - BEER
# ---------------------------

require(dplyr)
BEER <- read.csv("beer.csv")

Xl<-BEER[,grepl("local",names(BEER))]
Xn<-BEER[,grepl("Net",names(BEER))]
Y<-BEER$brand_choice

Y2 <- mat.or.vec(nrow(BEER),10)
for (i in 1:length(Y)){Y2[i,Y[i]] <- 1}
Y <- as.data.frame(Y2)

brands <- 10
coeffs <- 11
Xa <- list()
for (i in 1:(brands-1)){
   Xa[[i]] <- rep(0,brands*nrow(BEER))
   Xa[[i]][ ((i-1)*nrow(BEER)+1):(i*nrow(BEER))] <- 1 
}
Xa <- do.call(cbind,Xa)
index <- rep(1:nrow(BEER),brands)

param <- rep(1, coeffs)

# -----
# SIMPLE LOGIT NO HETROGENIETY
# -----

# Create log likelihood function to be maximized
logLik <- function(param,Xl,Xn,Y) {  
  m <-cbind(Xa, unlist(Xl, use.names=F), unlist(Xn, use.names=F)) #condense indep variables into one column
  ev <-exp(m %*% param)    
  den <- data.frame(index=index,ev=ev) # create matrix so you can sum over index
  den <- den %>% group_by(index) %>% summarise(den=sum(ev))
  den <- den[,2]
  den <- rep(den[[1]],brands) 
  p   <- ev/den  # 10x the length with each p stacked on top of each other
  pc  <- p*unlist(Y,use.names=F)  # still 10X the length    
  pc  <- data.frame(index=index,pc=pc) # create matrix so you can sum over index
  pc  <- pc %>% group_by(index) %>% summarise(pc=sum(pc))
  pc  <- pc[,2]      
  ll <- sum(log(pc))
  return(-ll)
}

logLik(param,Xl,Xn,Y)

fit <- nlm(f = logLik,p=param,Xl=Xl, Xn=Xn,Y=Y)
solution <- fit$estimate
print(solution)
