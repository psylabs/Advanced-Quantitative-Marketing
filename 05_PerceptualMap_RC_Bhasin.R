require (dplyr)
OJ <- read.csv("OJ.csv")

Xp <- OJ[,grepl("price",names(OJ))]
Y  <- OJ[,grepl("br",names(OJ))]
pan <- OJ[,grepl("panid",names(OJ))]

# ---------------------------
# WEEK 5 Perceptual Map RC Cts
# ---------------------------

# Instantiate model criteria
brands <- ncol(Y) 
draws <- 10   
Adim <-2  # not currently built to scale beyond 2 dim

beta_mean <- c(-3,-4,-2,0,0,-1,2)
params <- c(beta_mean,rep(0.1,(brands-1)*Adim-1),-1)

# Create log likelihood function to be maximized
LogLik <- function(params,Xf,Xp,Y,draws) {  
  
  #Extract seed values
  beta_mean <- params[1:brands-1]
  A <- c(params[(length(beta_mean)+1):(length(params)-1)],0)  # fix rotation here
  A <- matrix(data=A, ncol=Adim)
  bp <- params[length(params)]
  
  pref <- list () 
  set.seed(123)
  for (s in 1:draws) {
     w <-  matrix(rnorm(2, mean=0, sd=1))  # BiVariate draws
    pref [[s]] <- matrix(beta_mean) + A%*%w  # converted univariate draws to bivariate draws
    pref [[s]] <- rbind(pref[[s]],0)  
  }
  pref <- lapply(pref,t)
  
  EV <- function(pref,bp,Xp){
    ev <- bp*Xp
    ev <- exp(sweep(ev,2,pref,'+'))
    ev <- log(rowSums((ev*Y)/rowSums(ev)))
  }
  
  ev <- lapply(pref, EV ,bp=bp, Xp=Xp )
  ev <- do.call(cbind,ev)
  ll <- data.frame(pan=pan,ev=ev) 
  ll <- ll %>% group_by(pan) %>% summarise_each(funs(sum))  
  ll <- ll[,-1]
  ll <- log(rowMeans(exp(ll)))
  ll <- sum(-ll)
  return(ll)
}

LogLik(params,Xf,Xp,Y,draws)

fit.10 <- nlm(f = LogLik,p=params,Xf=Xf, Xp=Xp,Y=Y, draws=draws)
saveRDS(fit.10, "PreceptualMap_fit.Rds")

#### plot brands
param.fit <- fit.10$estimate
A <- c(param.fit[(length(beta_mean)+1):(length(param.fit)-1)],0)
A <- matrix(data=A, ncol=Adim)
plot(x=A[,1], y=A[,2], type="n")
text(x=A[,1], y=A[,2], labels=1:7)

# ---------------------------
# DEBUGGING
# ---------------------------
beta_mean <- as.numeric(readClipboard())
a.x <- as.numeric(readClipboard())
a.y <- as.numeric(readClipboard())
A <- matrix (c(a.x,a.y), ncol=2)

pref <- beta_mean + A%*%t(w)
pref <- rbind(pref,rep(0,ncol(pref)))
pref <- t(pref)
pref <- split(pref,row(pref)) # This is what I will actually be dealing with
