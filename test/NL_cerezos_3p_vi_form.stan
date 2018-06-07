data { 
  int<lower=1> N;  // total number of observations 
  int Y[N];  // response variable 
  int<lower=1> K_Asym;  // number of population-level effects 
  matrix[N, K_Asym] X_Asym;  // population-level design matrix 
  int<lower=1> K_xmid;  // number of population-level effects 
  matrix[N, K_xmid] X_xmid;  // population-level design matrix 
  int<lower=1> K_scal;  // number of population-level effects 
  matrix[N, K_scal] X_scal;  // population-level design matrix 
  vector[N] C_1;
  vector[N] C_2;
  int<lower=1> N_Y;
  vector[N_Y] CP_Y;
  vector[N_Y] GDH_Y;
  // data for group-level effects of ID 1 
  int<lower=1> J_1[N]; 
  int<lower=1> N_1; 
  int<lower=1> M_1; 
  vector[N] Z_1_Asym_1; 
  // data for group-level effects of ID 2 
  int<lower=1> J_2[N]; 
  int<lower=1> N_2; 
  int<lower=1> M_2; 
  vector[N] Z_2_Asym_1; 
} 
transformed data { 
} 
parameters { 
  vector<lower=0>[K_Asym] b_Asym;  // population-level effects 
  vector<lower=0>[K_scal] b_scal;  // population-level effects 
  vector<upper=0>[K_xmid] b_xmid;  // population-level effects 
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations 
  vector[N_1] z_1[M_1];  // unscaled group-level effects 
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations 
  vector[N_2] z_2[M_2];  // unscaled group-level effects 
  ordered[2] temp_Intercept;  // temporary thresholds 
} 
transformed parameters { 
  // group-level effects 
  vector[N_1] r_1_Asym_1 = sd_1[1] * (z_1[1]); 
  // group-level effects 
  vector[N_2] r_2_Asym_1 = sd_2[1] * (z_2[1]); 
} 
model { 
  vector[N] mu_Asym = X_Asym * b_Asym; 
  vector[N] mu_xmid = X_xmid * b_xmid; 
  vector[N] mu_scal = X_scal * b_scal; 
  vector[N] mu; 
  for (n in 1:N) { 
    mu_Asym[n] = mu_Asym[n] + (r_1_Asym_1[J_1[n]]) * Z_1_Asym_1[n] + (r_2_Asym_1[J_2[n]])               * Z_2_Asym_1[n]; 
      mu[n] = C_1[n] * (mu_Asym[n] / (1 + exp(-mu_xmid[n] - C_2[n]*mu_scal[n])));
    target += ordered_logistic_lpmf(Y[n] | mu[n], temp_Intercept);
  } 
  
  target += uniform_lpdf(b_scal | 0, 10); 
  target += uniform_lpdf(b_xmid | -10, 0); 
  target += uniform_lpdf(b_Asym | 0, 10); 
  target += student_t_lpdf(sd_1 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10); 
  target += normal_lpdf(z_1[1] | 0, 1); 
  target += student_t_lpdf(sd_2 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10); 
  target += normal_lpdf(z_2[1] | 0, 1); 
  
} 


generated quantities { 
  // Posterior predictive block
  vector[N] mu_Asym = X_Asym * b_Asym; 
  vector[N] mu_xmid = X_xmid * b_xmid; 
  vector[N] mu_scal = X_scal * b_scal; 
  vector[N] mu; 
  int Yhat[N];
  int Yrep[N_Y];
  
  for (n in 1:N) { 
      mu[n] = C_1[n] * (mu_Asym[n] / (1 + exp(-mu_xmid[n] - C_2[n]*mu_scal[n])));
    Yhat[n] = ordered_logistic_rng(mu[n], temp_Intercept);
  } 
  
  
  for (n in 1:N_Y) { 
      mu[n] = GDH_Y[n] * (mu_Asym[n] / (1 + exp(-mu_xmid[n] - C_2[n]*mu_scal[n])));
    Yrep[n] = ordered_logistic_rng(mu[n], temp_Intercept);
  } 
  
} 
