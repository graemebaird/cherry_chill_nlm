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
  int prior_only;  // should the likelihood be ignored? 
} 
transformed data { 
} 
parameters { 
  vector<lower=0>[K_Asym] b_Asym;  // population-level effects 
  vector<upper=0>[K_xmid] b_xmid;  // population-level effects 
  vector<lower=0>[K_scal] b_scal;  // population-level effects 
  ordered[2] temp_Intercept;  // temporary thresholds 
} 
model { 
  vector[N] mu_Asym = X_Asym * b_Asym; 
  vector[N] mu_xmid = X_xmid * b_xmid; 
  vector[N] mu_scal = X_scal * b_scal; 
  vector[N] mu; 
  // prior specifications 
  b_Asym ~ normal(0.5, 1); 
  b_xmid ~ normal(0, 1); 
  b_scal ~ normal(0, 1); 
  
  for (n in 1:N) { 
    // compute non-linear predictor 
    mu[n] = C_1[n] * (mu_Asym[n] / (1 + exp(-mu_xmid[n] - C_2[n]*mu_scal[n])));
    target += ordered_logistic_lpmf(Y[n] | mu[n], temp_Intercept);
  } 
  
} 
generated quantities { 
} 
