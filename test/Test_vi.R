
tempbr <- d %>% 
  mutate(treesub = paste0(tree,sub),
         vartree = as.integer(as.factor(paste0(variety,tree)))) %>%
  filter(!(treesub %in% c("61", "62"))) %>%
  melt(measure=c(6:20)) %>%           
  filter(!is.na(CP_c), variety == variety_in) %>%                 
  mutate(value = as.integer(value + 1)) %>%              
  make_standata(bf(value ~ GDH_c*(Asym/(1+exp((xmid-CP_c)/scal))),
                   scal + xmid ~ variety, 
                   Asym ~ variety +  (1|vartree/sub), 
                   nl = TRUE),
                data=., 
                family = gaussian,
                chains=4, 
                cores=4,
                prior = c(prior(uniform(.5, 10), nlpar = "Asym"),
                          prior(uniform(0, 10), nlpar = "xmid"),
                          prior(uniform(0, 10), nlpar = "scal")))


d <- read.csv("./datafiles/Raw_combined_excel.csv")
varlist <- unique(d$variety)

tempbr <- d %>% 
  mutate(treesub = paste0(tree,sub)) %>%
  filter(!(treesub %in% c("61", "62"))) %>%
  melt(measure=c(6:20)) %>%           
  filter(!is.na(CP_c), variety == variety_in) %>%                 
  mutate(value = as.integer(value + 1)) %>%              
  make_standata(bf(value ~ GDH_c*(Asym/(1+exp((xmid-CP_c)/scal))),
                   scal + xmid~ 1, 
                   Asym ~ (1|tree/sub), 
                   nl = TRUE),
                data=., 
                family = gaussian,
                chains=4, 
                cores=4,
                prior = c(prior(uniform(.5, 10), nlpar = "Asym"),
                          prior(uniform(0, 10), nlpar = "xmid"),
                          prior(uniform(0, 10), nlpar = "scal")))

tempbr$CP_Y <- rep(seq(0,4.5,length.out=10),10)
tempbr$GDH_Y <- rep(seq(0,4.5,length.out=10),each=10)
tempbr$N_Y <- 100

fit.test <- stan(file="./stanfiles/NL_cerezos_3p_vi.stan", data = tempbr, #### Sampler command
                 iter=1000, warmup = 200, chains=1, cores=1,
                 control = list(adapt_delta = 0.95))


Yreps <- apply(post$Yrep,2,mean)
data.frame(Yreps,GDH_Y, CP_Y) %>% ggplot(aes(GDH_Y,Yreps,color=as.factor(CP_Y))) + geom_line()




post <- extract(fit.test)

  rn <- seq(0,64.35,length.out=10000)
  
  fdiffer <- function(y,K,r,t){
    m <- K/(1+exp((y-t)/r))
    
    d1 <- rep(NA,9999)
    d2 <- rep(NA,9998)
    d3 <- rep(NA,9997)
    d4 <- rep(NA,9996)
    d1 <-  m[1:9999] - m[2:10000]
    d2 <-  d1[1:9998] - d1[2:9999]
    d3 <-  d2[1:9997] - d2[2:9998]
    d4 <-  d3[1:9996] - d3[2:9997]
    d1 <- d1/max(abs(d1),na.rm=T)
    d2 <- d2/max(abs(d2),na.rm=T)
    d3 <- d3/max(abs(d3),na.rm=T)
    d4 <- d4/max(abs(d4),na.rm=T)
    
    c1 <- which(d4 == max(d4,na.rm=T))[1]
    c2 <- c1 + which(d4[c1:10000] == min(d4[c1:10000],na.rm=T))[1]
    ap <- c1 + which(abs(d4[c1:c2]) == min(abs(d4[c1:c2]),na.rm=T))[1]
    
    return(rn[ap])
  }
  t <- seq(0,4.5,length.out=10000)
  
  post %$% 
    data.frame(b_xmid,b_Asym,b_scal) %>% 
    apply(1,function(x) fdiffer(x[2],x[1],x[3],t)) %>%
    hist

  
  
  
  
  
  
  
  
  
  d <- read.csv("./datafiles/Raw_combined_excel.csv")
  varlist <- unique(d$variety)
  
  tempbr <- d %>% 
    mutate(treesub = paste0(tree,sub),
           vartree = as.integer(as.factor(paste0(variety,tree)))) %>%
    filter(!(treesub %in% c("31","32","41","42","51","52","61","62"))) %>%
    melt(measure=c(6:20)) %>%           
    filter(!is.na(CP_c), variety == variety_in) %>%                 
    mutate(value = as.integer(value + 1)) %>%              
    make_standata(bf(value ~ GDH_c*(Asym/(1+exp((xmid-CP_c)/scal))),
                     scal + xmid ~ variety, 
                     Asym ~ variety +  (1|vartree/sub), 
                     nl = TRUE),
                  data=., 
                  family = gaussian,
                  chains=4, 
                  cores=4,
                  prior = c(prior(uniform(.5, 10), nlpar = "Asym"),
                            prior(uniform(0, 10), nlpar = "xmid"),
                            prior(uniform(0, 10), nlpar = "scal")))
  
    fit.test <- stan(file="stanfiles/NL_cerezos_3p_form.stan", data = tempbr, #### Sampler command
                     iter=2800, warmup = 800, chains=1, cores=1,
                     control = list(adapt_delta = 0.95))
    
    save(fit.test, file=paste("./modelfits/",variety_in,filename,sep="")) #### Save model fit
  }


-(log(5-2*sqrt(6)) + -3.65)/2.45

-[ln(5-2rt6) + β]/γ

y = a / (1 + exp(-B-γx))




