d <- read.csv("./datafiles/Raw_combined_excel.csv")
varlist <- unique(d$variety)

##########################################################

post_samples_3p <- matrix(NA,2000,9) %>% as.data.frame

for(i in 1:9) {
  load(paste0("./modelfits/",varlist[i],"_3p_updated"))
  post <- extract.samples(fit.test) 
  post_samples_3p[,i] <- post$Ysat %>% apply(1,mean)*unique(d$CP_scale)
  print(paste("3p import",i))
}

colnames(post_samples_3p) <- varlist

post_samples_3p %>%
  melt %>%
  group_by(variable) %>%
  summarise(mean = mean(value), 
            L = quantile(value,.025),
            H = quantile(value,.975))

