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
  theme(legend.position="none") +
  xlab("Alpha-Ratio") +
  ylab("Asymptotic Deceleration Point") +
  scale_y_continuous(limits=c(30,44), breaks=c(30,32,34,36,38,40,42,44)) +
  scale_x_continuous(limits=c(.35,.65), breaks = c(.35,.40,.45,.50,.55,.60,.65)) +
  scale_color_brewer(palette="Dark2")


a <- comp_tot %>% ggplot(aes(value, y = ..scaled.., fill=Variety)) + geom_density(alpha=.5) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_text(colour="white"),
        axis.text.y=element_text(colour="white"),
        axis.ticks.y=element_line(colour="white"),
        axis.line.y=element_line(colour="white")) +
  scale_x_continuous(limits=c(.35,.65))+
  scale_fill_brewer(palette="Dark2")



b <- comp_tot %>% ggplot(aes(value_3p, y = ..scaled.., fill=Variety)) + geom_density(alpha=.5) + 
  scale_x_continuous(limits=c(30,44)) +
  coord_flip()+
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x=element_text(colour="white"),
        axis.text.x=element_text(colour="white"),
        axis.ticks.x=element_line(colour="white"),
        axis.line.x=element_line(colour="white"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_fill_brewer(palette="Dark2")



leg <- comp_tot %>% ggplot(aes(value_3p, y = ..scaled.., fill=Variety)) + 
  geom_density(alpha=.5) + 
  theme(legend.position = "right",
        legend.text=element_text(size=8)) + 
  guides(fill=guide_legend(ncol=2)) + 
  scale_x_continuous(limits=c(30,44)) +
  coord_flip() +
  scale_fill_brewer(palette="Dark2")

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend <- g_legend(leg)
library(grid)
grid.draw(mylegend)


grid.arrange(a,temp_img,b,mylegend,
             layout_matrix = rbind(c(1,1,1,1,4), 
                                   c(2,2,2,2,3),
                                   c(2,2,2,2,3),
                                   c(2,2,2,2,3),
                                   c(2,2,2,2,3)),padding=0) %>%
  ggsave("figures/fig8_contour_plot_3p.png", plot = .,width = 8,height=8)

