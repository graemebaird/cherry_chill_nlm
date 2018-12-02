data { 
  int<lower=1> N;  // total number of observations 
  int Y[N];  // response variable 
  int<lower=1> K_umax;  // number of population-level effects 
  matrix[N, K_umax] X_umax;  // population-level design matrix 
  int<lower=1> K_k;  // number of population-level effects 
  matrix[N, K_k] X_k;  // population-level design matrix 
  vector[N] C_1;
  vector[N] C_2;
  int prior_only;  // should the likelihood be ignored? 
} 
transformed data { 
} 
parameters { 
  vector<lower=0>[K_umax] b_umax;  // population-level effects 
  vector<lower=0>[K_k] b_k;  // population-level effects 
  ordered[2] temp_Intercept;  // temporary thresholds 
} 
model { 
  vector[N] mu_umax = X_umax * b_umax; 
  vector[N] mu_k = X_k * b_k; 
  vector[N] mu; 
  // prior specifications 
  b_umax ~ normal(0, 1); 
  b_k ~ normal(2, 1); 
  
  for (n in 1:N) { 
    // compute non-linear predictor 
    mu[n] = C_1[n]*mu_umax[n]*C_2[n]/(mu_k[n]+C_2[n]);
    target += ordered_logistic_lpmf(Y[n] | mu[n], temp_Intercept);
  } 
  
} 
generated quantities { 
} 
