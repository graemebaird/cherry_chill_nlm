d <- read.csv("datafiles/Raw_combined_excel.csv")
varlist <- unique(d$variety)
varlist <- as.character(varlist[varlist != "Kordia (M)"])

post_samples_3p <- matrix(NA,4000,8) %>% as.data.frame
post_samples_alpha <- matrix(NA,4000,8) %>% as.data.frame

for(i in 1:8) {
  load(paste0("modelfits/",varlist[i],"_3p_updated"))
  post <- extract.samples(fit.test) 
  post_samples_3p[,i] <- ysat_ext(post) %>% as.vector()
  alpha <- post$b_alpha %>% as.vector()
  tempInt <- post$temp_Intercept[1] %>% as.vector()
  post_samples_alpha[,i] <- alpha/tempInt
  print(paste("3p import",i))
}

varlist[varlist == "Kordia (C )"] <- "Kordia"

colnames(post_samples_3p) <- varlist
comp_3p <- post_samples_3p %>% melt
post_samples_3p$model <- "Logistic"

colnames(post_samples_alpha) <- varlist
comp_alpha <- post_samples_alpha %>% melt 
post_samples_alpha$model <- "alpha"


comp_tot <- comp_alpha
comp_tot$value_3p <- comp_3p$value


temp_table <- comp_tot %>% 
  group_by(variable) %>% 
  summarise(CP_h = quantile(value_3p, .975), 
            CP_l = quantile(value_3p, .025), 
            alpha_h = quantile(value, .975), 
            alpha_l = quantile(value, .025))

colnames(comp_tot)[1] <- "Variety"

temp_img <-  comp_tot %>%
  ggplot(aes(value,value_3p, color=Variety)) +
  stat_ellipse(type="t", level=.9,size=1.2) +
  theme_classic() + 
  xlab("Alpha-Ratio") +
  ylab("Asymptotic Deceleration Point")

  
ggsave("figures/contour_plot_3p.png", plot = temp_img,width = 5,height=5)

