setwd(getwd()); require (dplyr)
yogurt<-read.csv("Yogurt100N.csv")

# Select Indep and response variables
Xf <- yogurt[,grepl("Feature",names(yogurt))]
Xp <- yogurt[,grepl("Price",names(yogurt))]
Y <- yogurt[,grepl("Brand",names(yogurt))]

# Miscellany needed for creating model matrix
on <- rep(1,nrow(yogurt))
off <- rep(0,nrow(yogurt))
Xa <- cbind(c(on,off,off,off),
            c(off,on,off,off),
            c(off,off,on,off))
index <- rep(1:nrow(yogurt),4)

# ---------------------------
# WEEK 3 SEGMENTS
# ---------------------------
pan <- yogurt[,grepl("Pan.I.D.",names(yogurt))]

# Instantiate model criteria
brands <- ncol(Y) 
coeffs <- 5 
segments <- 2

# params=c(2,3,-0.5,1,-10,2,3,-0.5,1,-10,0.1)
params <- c(rep(rnorm(coeffs, mean=0, sd=3), segments), rep(0.1,segments-1))

# Create log likelihood function to be maximized
LogLik <- function(params,Xf,Xp,Y) {  
  beta <- list()
  for (s in 1:segments) beta[[s]] <- params[((s-1)*coeffs+1):(s*coeffs)]
  
  SegLik <- function(params,Xf,Xp,Y){
    m   <- cbind(Xa, unlist(Xf, use.names=F), unlist(Xp, use.names=F))  # condense indep variables into one column
    ev  <- exp(m %*% params)   # all 4 evs stacked on top of each other
    den <- tapply(ev,index,sum)  # sum where index is shared
    den <- rep(den,4) 
    p   <- ev/den  # 4X the length with each p stacked on top of each other
    pc  <- p*unlist(Y,use.names=F)  # still 4X the length
    pc  <- tapply(pc,index,sum)  # no longer 4X
    
    temp <- data.frame(cbind(pan,pc))  # create matrix so you can product over HH
    ll <- temp %>%
      group_by(pan) %>%  # pan obtained from global scope
      summarise(ll=prod(pc))  
    return(ll[,2])  # return segmented ll
  }
  
  lambda <- params[-(1:(coeffs*segments))] 
  pi <- exp(lambda)/(1+sum(exp(lambda)))
  pi <- c(pi,1-sum(pi))
  
  ll <- lapply(beta, SegLik, Xf=Xf, Xp=Xp, Y=Y)
  ll <- Map(`*`, ll, pi)
  ll <- unlist(ll)
  ll <- log(tapply(ll,names(ll),sum))  # should be a better way to find weighted sums
  ll <- sum(-ll) 
  return(ll)
}

params <-c (1,3,-0.5,1,-10,2,3,-0.5,1,-10,0.1)
LogLik(params,Xf,Xp,Y)

fit_segment <- nlm(f = LogLik,p=params,Xf=Xf, Xp=Xp,Y=Y)
solution <- fit_segment$estimate
names(solution) <- c("a11","a21","a31", "bf1","bp1",
                  "a12","a22","a32", "bf2","bp2",
                  "lambda")
BIC_segment <- 2*fit_segment$minimum + length(params)*log(nrow(yogurt))

# ----
# Calculate Brand Specific Elasticities 
# ----
Elasticity <- function(params,Xf,Xp){
  beta <- list()
  for (s in 1:segments) beta[[s]]<- params[((s-1)*coeffs+1):(s*coeffs)]
  
  # This function is used often
  SegPp <- function(params,Xf,Xp){ 
    m   <- cbind(Xa, unlist(Xf, use.names=F), unlist(Xp, use.names=F)) 
    ev  <- exp(m %*% params)  
    den <- tapply(ev,index,sum) 
    den <- rep(den,4) 
    pp  <- ev/den   
    return(pp)
  }
  
  lambda <- params[-(1:(coeffs*segments))] 
  pi <- exp(lambda)/(1+sum(exp(lambda)))
  pi <- c(pi,1-sum(pi))
  
  pp <- lapply(beta, SegPp, Xf=Xf, Xp=Xp)  # stacked
  ep <- Map(`*`, pp, pi)  # weighted eps 
  ep <- do.call('+',ep)
  ipp <- lapply(pp, function(x) 1-x)
  bp <- params[seq(coeffs,s*coeffs,coeffs)]  # since we know Bp is the last element 
  temp <- Map(function(pp, ipp, pi,bp) pp*ipp*pi*bp, pp, ipp, pi, bp)
  temp <- do.call('+',temp)
  temp <- temp/ep  
  temp <- matrix(temp,ncol = 4)      
  es <- colMeans(temp *Xp); 
  names(es) <- NULL
  return(es)
}

Elasticity(params=solution, Xf=Xf, Xp=Xp)