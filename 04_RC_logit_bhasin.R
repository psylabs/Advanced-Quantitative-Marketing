require (dplyr)
yogurt <- read.csv("Yogurt100N.csv")

# Select Indep and response variables
Xf <- yogurt[,grepl("Feature",names(yogurt))]
Xp <- yogurt[,grepl("Price",names(yogurt))]
Y  <- yogurt[,grepl("Brand",names(yogurt))]

# Miscellany needed for creating model matrix
on  <- rep(1,nrow(yogurt))
off <- rep(0,nrow(yogurt))
Xa  <- cbind(c(on,off,off,off),
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

beta_seed <- c(2,2,-1,1,-50,2,0,-2,0,2,1,0,0,1,1,0,-1,1,0,-20)
params <- as.numeric(readClipboard())

#### Now params refers to the 5 betas per draw not the beta_seed (called params earlier) #####

# Create log likelihood function to be maximized
LogLik <- function(beta_seed,Xf,Xp,Y,draws,coeffs) {  
  
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
    den <- data.frame(index=index,ev=ev) # create matrix so you can sum over index
    den <- den %>% group_by(index) %>% summarise(den=sum(ev))
    den <- den[,2]
    den <- rep(den[[1]],4) 
    p   <- ev/den  # 4X the length with each p stacked on top of each other
    pc  <- p*unlist(Y,use.names=F)  # still 4X the length    
    pc  <- data.frame(index=index,pc=pc) # create matrix so you can sum over index
    pc  <- pc %>% group_by(index) %>% summarise(pc=sum(pc))
    pc  <- pc[,2]      
    ll  <- data.frame(pan=pan,pc=pc)  # create matrix so you can product over HH
    ll  <- ll %>% group_by(pan) %>% summarise(ll=prod(pc))  
    return(ll[,2])  # not really log yet
  }
  
  ll <- lapply(beta, SegLik, Xf=Xf, Xp=Xp, Y=Y)  
  ll <- unlist(ll)
  ll <- do.call(cbind,ll)
  
  # Calculate Posteriors
  weights <- ll/rowSums(ll)
  beta <- do.call(rbind, beta)
  posterior <- as.matrix(weights) %*% beta
  # End  TODO: You need to expose this
  
  ll <- log(tapply(ll,names(ll),mean))  # still looking for a better way to do this
  ll <- sum(-ll) 
  return(ll)
}

# LogLik(beta_seed,Xf,Xp,Y,draws,coeffs)
fit <- list(); solution <-list()
draws <- c(30,50,100)
i <- 1 #counter
for (d in draws) {
  fit[[i]] <- nlm(f = LogLik,p=beta_seed,Xf=Xf, Xp=Xp,Y=Y, draws=d, coeffs=coeffs)  # steptol=1e-4,gradtol=1e-4
  solution[[i]] <- fit[[i]]$estimate
  print(i <- i+1)
}

write.csv(c(solution), file = "RC.csv")



# ---------------------------
# WEEK 5 Random Coefficients Logit Posteriors
# ---------------------------

# Calculate posteriors for new draws given converged betas
Posterior <- function(beta_seed,Xf,Xp,Y,draws,coeffs) {  # identical to LogLik save for the values returned
  
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
    den <- data.frame(index=index,ev=ev) # create matrix so you can sum over index
    den <- den %>% group_by(index) %>% summarise(den=sum(ev))
    den <- den[,2]
    den <- rep(den[[1]],4) 
    p   <- ev/den  # 4X the length with each p stacked on top of each other
    pc  <- p*unlist(Y,use.names=F)  # still 4X the length    
    pc  <- data.frame(index=index,pc=pc) # create matrix so you can sum over index
    pc  <- pc %>% group_by(index) %>% summarise(pc=sum(pc))
    pc  <- pc[,2]      
    ll  <- data.frame(pan=pan,pc=pc)  # create matrix so you can product over HH
    ll  <- ll %>% group_by(pan) %>% summarise(ll=prod(pc))  
    return(ll[,2])  # not really log yet
  }
  
  ll <- lapply(beta, SegLik, Xf=Xf, Xp=Xp, Y=Y)  
  ll <- do.call(cbind,ll)
  
  # Calculate Posteriors
  weights <- ll/rowSums(ll)
  beta <- do.call(rbind, beta)
  posterior <- as.matrix(weights) %*% beta
  
  return(posterior)
}


hh_beta <- Posterior(solution,Xf,Xp,Y,draws,coeffs)
colnames(hh_beta) <- c("a1","a2","a3","bf","bp")
par(mfrow=c(2,3))
for (p in 1:coeffs) {hist(hh_beta[,p],main=NULL, xlab=colnames(hh_beta)[p])}
# pairs(hh_beta)