# These functions support the other codefiles. 

extract.samples <- function (object, n = 10000, clean.names = TRUE, ...)  { ### From McElreath Rethinking
  p <- rstan::extract(object, ...)
  for (i in 1:length(p)) {
    attr(p[[i]], "dimnames") <- NULL
  }
  return(p)
}

dordlogit <- function (x, phi, a, log = FALSE) ### From McElreath Rethinking
{
  a <- c(as.numeric(a), Inf)
  p <- logistic(a[x] - phi)
  na <- c(-Inf, a)
  np <- logistic(na[x] - phi)
  p <- p - np
  if (log == TRUE) 
    p <- log(p)
  p
}

pordlogit <- function (x, phi, a, log = FALSE) ### From McElreath Rethinking
{
  a <- c(as.numeric(a), Inf)
  if (length(phi) == 1) {
    p <- logistic(a[x] - phi)
  }
  else {
    p <- matrix(NA, ncol = length(x), nrow = length(phi))
    for (i in 1:length(phi)) {
      p[i, ] <- logistic(a[x] - phi[i])
    }
  }
  if (log == TRUE) 
    p <- log(p)
  p
}

logistic <- function (x) ### From McElreath Rethinking
{
  p <- 1/(1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  p
}

rordlogit <- function (n, phi = 0, a) ### From McElreath Rethinking
{
  a <- c(as.numeric(a), Inf)
  k <- 1:length(a)
  if (length(phi) == 1) {
    p <- dordlogit(k, a = a, phi = phi, log = FALSE)
    y <- sample(k, size = n, replace = TRUE, prob = p)
  }
  else {
    y <- rep(NA, n)
    if (n > length(phi)) {
      phi <- rep(phi, ceiling(n/length(phi)))
    }
    for (i in 1:n) {
      p <- dordlogit(k, a = a, phi = phi[i], log = FALSE)
      y[i] <- sample(k, size = 1, replace = TRUE, prob = p)
    }
  }
  y
}

## Violinplot functions

diamondfun <- function(input) {
  counts <- seq(1,500,length.out=10)[c(1:10,9:1)] %>% round
  storvec <- numeric(0)
  for (i in 1:19) storvec <- (pull(input[2]) - ((10-i)/9)*pull(input[3])) %>% rep(.,times = counts[i]) %>% c(storvec,.)
  data.frame(input[1],storvec)
}

logiter <- function(cp,alpha,eta,beta,ti1,ti2,gdh = 2) {
  phi <- gdh* alpha / (1 + exp(-beta - cp*eta))
  rordlogit(50, a=c(ti1,ti2), phi = phi)
}

logiter_fieldsim <- function(cp,alpha,eta,beta,ti1,ti2,gdh) {
  phi <-gdh* alpha / (1 + exp(-beta - cp*eta))
  1 - pordlogit(1, a=c(ti1,ti2), phi = phi)
}

generate_datesims <- function(df, variety_in){
  load(paste0("./modelfits/",variety_in,"_3p_updated"))
  post <- posterior_samples(fit.test)
  post <- as.vector(apply(post[,c(1,2,3,31,32)],2,median))
  apply(df,1,function(x) logiter_fieldsim(as.numeric(x[3]),post[1],post[2],post[3],post[4],post[5],as.numeric(x[4])))
}

generate_sims <- function(post, rn){
  post <- post[,c(1,2,3,31,32)]
  test_CP <- seq(0,4.5,length.out=testlen)
  
  stor_mat <- matrix(NA,2000,testlen)
  
  for(i in 1:length(test_CP)){
    stor_mat[,i] <-  apply(post,1,function(x) logiter(test_CP[i],x[1],x[2],x[3],x[4],x[5])) %>% 
      apply(2,function(x) length(which(x > 1))/50) 
    print(i)
  }
  apply(stor_mat,1,function(x) rn[min(which(x >.5))])
}

ysat_ext <- function(post){
  post %$% apply(Ysat,1,mean)*14.3562 %>% as.vector()
}

ysat_quant <- function(filename){
  load(filename)
  fit.test %>% extract.samples() %$% apply(Ysat,1,mean) %>% quantile(probs=c(.05,.5,.95))*14.3562 %>% as.vector()
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
