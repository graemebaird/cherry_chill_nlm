# Estimate marginal GDH req for 50%


n_out <- 1000
CP <- rep(10,times=n_out) # way above saturation for all models
GDH <- seq(0,5,length.out=n_out)
df <- data.frame(CP=CP,GDH=GDH)

logiter_fieldsim_GDHest <- function(cp,alpha,eta,beta,ti1,ti2,gdh) {
  phi <-gdh* alpha / (1 + exp(-beta - cp*eta))
  1 - pordlogit(1, a=c(ti1,ti2), phi = phi)
}

generate_datesims_GDHest <- function(df, variety_in){
  load(paste0("./modelfits/",variety_in,"_3p_updated"))
  post <- posterior_samples(fit.test)
  post <- post[,c(1,2,3,31,32)]
  act_v <- apply(df,1, function(y) {
    apply(post,1,function(x) logiter_fieldsim(as.numeric(y[1]),x[1],
                                          x[2],x[3],x[4],
                                          x[5],as.numeric(y[2])))
  })
  df[apply(act_v,1,function(x) which(x > .5) %>% min),2]*4648
}


GDH_samples <- matrix(NA,2000,9) %>% as.data.frame

GDH_samples[,1] <- generate_datesims_GDHest(df, "Bing")
GDH_samples[,2] <- generate_datesims_GDHest(df, "Sweetheart")
GDH_samples[,3] <- generate_datesims_GDHest(df, "Rainier")
GDH_samples[,4] <- generate_datesims_GDHest(df, "Regina")
GDH_samples[,5] <- generate_datesims_GDHest(df, "Santina")
GDH_samples[,6] <- generate_datesims_GDHest(df, "Kordia (C )")
GDH_samples[,7] <- generate_datesims_GDHest(df, "Kordia (M)")
GDH_samples[,8] <- generate_datesims_GDHest(df, "Skeena")
GDH_samples[,9] <- generate_datesims_GDHest(df, "Lapins")
colnames(GDH_samples) <- c("Bing", "Sweetheart", "Rainier", "Regina", "Santina", "Kordia (C)", 
                           "Kordia (M)", "Skeena", "Lapins")

save(GDH_samples, file = "datafiles/GDH_model_estimation")

gdh_plot <- GDH_samples %>% 
  melt() %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value), 
            sdh = mean + sd(value), 
            sdl = mean - sd(value)) %>% 
  ggplot() + 
  geom_pointrange(aes(x = reorder(variable,mean,fun=median), y = mean, ymin = sdl, ymax = sdh)) + 
  xlab("Variety") +
  ylab("Growing Degree Hours") + 
  ggtitle("Heat requirements to reach 50% bud activity")





d <- read.csv("datafiles/Raw_combined_excel.csv")
varlist <- unique(d$variety)

post_samples_3p <- matrix(NA,2000,8) %>% as.data.frame

for(i in 1:9) {
  load(paste0("./modelfits/",varlist[i],"_3p_updated"))
  post <- extract.samples(fit.test) 
  post_samples_3p[,i] <- post$Ysat %>% apply(1,mean)*unique(d$CP_scale)
  print(paste("3p import",i))
}


colnames(post_samples_3p) <- varlist
colnames(post_samples_3p)[8] <- "Kordia C"

cp_plot <- post_samples_3p %>% 
  melt() %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value), 
            sdh = mean + sd(value), 
            sdl = mean - sd(value)) %>% 
  ggplot() + 
  geom_pointrange(aes(x = reorder(variable,mean,fun=median), y = mean, ymin = sdl, ymax = sdh)) + 
  xlab("Variety") +
  ylab("Chill Portions") + 
  ggtitle("Chilling sufficiency requirement")


grid.arrange(gdh_plot,cp_plot)

