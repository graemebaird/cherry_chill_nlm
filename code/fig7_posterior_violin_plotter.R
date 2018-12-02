d <- read.csv("datafiles/Raw_combined_excel.csv")
varlist <- unique(d$variety)
varlist <- varlist[varlist != "Kordia (M)"]

##########################################################
# 
# post_samples_plat <- matrix(NA,2000,9) %>% as.data.frame
# 
# for(i in 1:9) {
#   load(paste0("./modelfits/",varlist[i],"_plat"))
#   post <- extract.samples(fit.test) 
#   post_samples_plat[,i] <- sample(16*post$b_k/post$b_umax,2000,replace=FALSE)
# }
# 
# colnames(post_samples_plat) <- varlist
# post_samples_plat$model <- "Plateau"

##########################################################

post_samples_3p <- matrix(NA,2000,8) %>% as.data.frame

for(i in 1:8) {
  load(paste0("modelfits/",varlist[i],"_3p_updated"))
  post <- extract.samples(fit.test) 
  post_samples_3p[,i] <- post$Ysat %>% apply(1,mean)*unique(d$CP_scale)
  print(paste("3p import",i))
}


colnames(post_samples_3p) <- varlist
colnames(post_samples_3p)[8] <- "Kordia"
post_samples_3p$Model <- "Logistic"

##########################################################

post_samples_fifty <- read.csv("datafiles/Raw_combined_excel.csv") %>% 
  filter(week==3 & prop >= .5) %>% 
  group_by(variety, tree,sub) %>% 
  summarise(CP = min(CP)) %>%
  group_by(variety) %>%
  summarise(mean = mean(CP), sdv = sd(CP), l = mean -  1.96*sdv/2, h = mean +  1.96*sdv/2) %>%
  ungroup() %>%
  select(variety,mean,sdv) %>%
  filter(variety != "Kordia (M)") %>%
  mutate(variety = as.character(variety), 
         variety = ifelse(variety == "Kordia (C )", "Kordia", variety))

# 
# post_samples_fifty_exp <- data.frame(variable = character(0), storvec = numeric(0))
# for(i in 1:8) post_samples_fifty_exp <- diamondfun(post_samples_fifty[i,]) %>% rbind(post_samples_fifty_exp)
# post_samples_fifty_exp$Model <- "Fifty"
# colnames(post_samples_fifty_exp) <- c("variable", "value", "Model")

##########################################################
# 
# testlen <- 500
# rn <- seq(0,4.5,length.out=testlen)*unique(d$CP_scale)
# 
# post_samples_3p_dsim <- matrix(NA,2000,9) %>% as.data.frame
# 
# for(i in 1:9) {
#   load(paste0("./modelfits/",varlist[i],"_3p_updated"))
#   post <- posterior_samples(fit.test)
#   post_samples_3p_dsim[,i] <- generate_sims(post, rn)
#   print(i)
# }
# colnames(post_samples_3p_dsim) <- varlist
# post_samples_3p_dsim$model <- "Fifty (simulated)"

##########################################################

#post_samples <- rbind(post_samples_3p) %>% melt %>% rbind(post_samples_fifty_exp)
#post_samples <- rbind(post_samples_plat,post_samples_3p,post_samples_3p_95,post_samples_3p_dsim) %>% melt %>% rbind(post_samples_fifty_exp)
post_samples <- post_samples_3p %>% melt

temp_img <- ggplot(data = post_samples,aes(x = value, y = variable, height=..density..)) +
  geom_joy(scale=0.85, alpha = .8) +
  geom_errorbarh(data = post_samples_fifty, 
                 aes(x=mean, xmin = mean-sdv, xmax = mean + sdv, y = variety ), 
                 inherit.aes=F, position=position_nudge(y = .2), height = .4)  +
  geom_point(data = post_samples_fifty, 
                   aes(x=mean, y = variety ), 
                   inherit.aes=F, position=position_nudge(y = .2)) +
  theme_classic() + 
  ylab("Variety") + 
  xlab("Estimated critical chilling requirement (CP)")

ggsave("figures/fig7_violin_plot_3p.png", plot = temp_img, width=5, height=5)
