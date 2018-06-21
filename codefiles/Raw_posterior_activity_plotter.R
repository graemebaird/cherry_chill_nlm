d  <- read.csv("datafiles/Raw_combined_excel.csv") 
varlist <- unique(d$variety)
varlist <- varlist[varlist != "Kordia (M)"]

storframe <- data.frame(variety = character(0), CP_c = numeric(0), GDH_c = numeric(0), budstate = numeric(0))

for(i in 1:8) {
  load(paste0("modelfits/",varlist[i],"_3p_updated"))
  post <- extract.samples(fit.test) 
  tempbr <- d %>% 
    mutate(treesub = paste0(chill,sub),
           vartree = as.integer(as.factor(paste0(variety,tree)))) %>%
    filter(!(tree %in% c("6"))) %>%
    melt(measure=c(6:20)) %>%           
    filter(!is.na(CP_c), variety == varlist[i]) %>%                 
    mutate(value = as.integer(value + 1)) %>%              
    make_standata(bf(value ~ GDH_c*(alpha/(1+exp(-beta-CP_c*eta))),
                     beta + eta~ 1, 
                     alpha ~ 1 + (1|vartree/treesub), 
                     nl = TRUE),
                  data=., 
                  family = gaussian,
                  chains=4, 
                  cores=4,
                  prior = c(prior(normal(0, 10), nlpar = "alpha"),
                            prior(normal(0, 10), nlpar = "beta"),
                            prior(normal(0, 10), nlpar = "eta")))
  storframe <- data.frame(variety = varlist[i], CP_c = tempbr$C_2*14.3, GDH_c = tempbr$C_1*4848, budstate = (apply(post$Yrep,2,getmode))) %>% rbind(storframe)
  print(i)
}

mod2  <- storframe %>%
  filter(variety != "Kordia (M)") %>%
  mutate(variety = as.character(variety), 
         variety = ifelse(variety == "Kordia (C )", "Kordia", variety)) %>%
  group_by(variety,CP_c) %>% 
  filter(budstate == 1) %>%
  summarise(min = max(GDH_c), 
            count = length(GDH_c)) 



mod  <- read.csv("datafiles/Raw_combined_excel.csv") %>%
  melt(measure=c(6:20)) %>% 
  mutate(bud_u = as.factor(paste0(tree_u,variable))) %>%
  group_by(variety,CP, bud_u) %>% 
  filter(value == 0) %>%
  summarise(min = max(GDH), 
            count = length(GDH)) %>% 
  group_by(variety,CP) %>% 
  summarise(mean = mean(min), 
            sd = sd(min),
            meanl = mean(ifelse(count==5,0,1))*100,
            sdl = sd(ifelse(count==5,0,1))*100)  %>%
  ungroup() %>%
  filter(variety != "Kordia (M)") %>%
  mutate(variety = as.character(variety), 
         variety = ifelse(variety == "Kordia (C )", "Kordia", variety))

intervals <- data.frame(variety = character(8),
                        l = numeric(8),
                        me = numeric(8),
                        h = numeric(8))

intervals$variety <- c("Rainier", 
                       "Lapins",
                       "Sweetheart",
                       "Bing", 
                       "Kordia (C )", 
                       "Regina",
                       "Skeena", 
                       "Santina")

for(i in 1:8) intervals[i,2:4] <- ysat_quant(paste0("modelfits/",intervals[i,1],"_3p_updated"))

intervals$variety[5] <- "Kordia"

temp_img <- ggplot(mod, aes(x=CP)) + 
  geom_line(aes(y=mean)) + 
  geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd), alpha=.1) +
  geom_line(stat="identity", aes(y=meanl*50), linetype="dashed") +
  geom_ribbon(aes(ymin=(meanl-sdl)*50, ymax=(meanl+sdl)*50), alpha=.1) + 
  geom_vline(data=intervals, aes(xintercept = me), color="red") +
  geom_vline(data=intervals, aes(xintercept = l), linetype="dotted",color="red") +
  geom_vline(data=intervals, aes(xintercept = h), linetype="dotted",color="red") +
  scale_y_continuous(limits=c(-3000,22000),sec.axis = sec_axis(~./50, breaks=c(0,100)))+
  facet_wrap(~variety,ncol=2) +
  geom_line(data = mod2,aes(x=CP_c, y=min, color = "red")) +
  labs(x="Chill accumulation (CP)", y="Heat requirement (GDH)") + 
  theme_classic() +
  guides(color = FALSE)

ggsave("figures/posterior_activity.png", plot = temp_img, width=7,height=5)
