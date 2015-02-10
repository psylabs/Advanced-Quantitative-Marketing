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
# WEEK 4 Random Coefficients Logit
# ---------------------------
require(miscTools)
pan <- yogurt[,grepl("Pan.I.D.",names(yogurt))]

# Instantiate model criteria
brands <- ncol(Y) 
coeffs <- 5 
draws <- 10

beta_seed <- c(1,-2,1,-1,-5,2,0,-2,0,2,1,0,0,1,1,0,-1,1,0,2)
# beta_seed <- as.numeric(readClipboard())

#### Now params refers to the 5 betas per draws not the beta_seed (called params earlier) #####

# Create log likelihood function to be maximized
LogLik <- function(beta_seed,Xf,Xp,Y) {  
  
  # Starting Criteria
  root <- triang(tail(beta_seed,-coeffs),coeffs)  # choleski root based on random start values
  params <- list () # BAD NAME SELECTION
  set.seed(123)
  for (s in 1:draws) {
    w <-  c(rnorm(5, mean=0, sd=1))  
    params [[s]] <- matrix(beta_seed[1:coeffs] + w%*%root)  # converted univariate draws to multivariate draws
  }
  params <- unlist(params)
  
  beta <- list()
  for (s in 1:draws) beta[[s]] <- params[((s-1)*coeffs+1):(s*coeffs)]
  
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
    return(ll[,2])  # not really log yet
  }
  
  ll <- lapply(beta, SegLik, Xf=Xf, Xp=Xp, Y=Y)  
  ll <- unlist(ll)
  ll <- log(tapply(ll,names(ll),mean))  # still looking for a better way to do this
  ll <- sum(-ll) 
  return(ll)
}

LogLik(beta_seed,Xf,Xp,Y)

system.time(fit_segment <- nlm(f = LogLik,p=beta_seed,Xf=Xf, Xp=Xp,Y=Y))  # steptol=1e-4,gradtol=1e-4
solution <- fit_segment$estimate
BIC_segment <- 2*fit_segment$minimum + length(params)*log(nrow(yogurt))
write.csv(c(fit_segment, solution, BIC_segment), file = "output.csv")
