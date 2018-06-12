
require(pander)
require(tidybayes)

load("./modelfits/Lapins_3p_updated")
post <- posterior_samples(fit.test)

fit.test %>%
  gather_samples(b_alpha[1], b_beta, b_eta, temp_Intercept[1], temp_Intercept[2]) %>% 
  mean_qi() %>%
  pander
