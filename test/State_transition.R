load("./modelfits/Lapins_3p_updated")

post <- extract.samples(fit.test)

starray <- array(dim=c(2000,10,3))

sat_growth <- post$mu_rep[,seq(10,100,by=10)]

for(i in 1:10){
  for(j in 1:2000){
    starray[j,i,]<- dordlogit(c(1,2,3),sat_growth[j,i],post$temp_Intercept[j,])
  }
}

dorm <- starray[,,1] %>% melt
dorm$type <- "dorm"
gtip <- starray[,,2] %>% melt
gtip$type <- "gtip"
lout <- starray[,,3] %>% melt
lout$type <- "lout"

glob <- dorm %>% rbind(gtip) %>% rbind(lout) %>% select(value,type)

ggplot(glob,aes(Var2,value, color=type)) + 
  stat_smooth()
