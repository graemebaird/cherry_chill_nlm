data { 
  int<lower=1> N;  // total number of observations 
  int Y[N];  // response variable 
  int<lower=1> K_alpha;  // number of population-level effects 
  matrix[N, K_alpha] X_alpha;  // population-level design matrix 
  int<lower=1> K_beta;  // number of population-level effects 
  matrix[N, K_beta] X_beta;  // population-level design matrix 
  // covariate vectors 
  vector[N] C_1;
  vector[N] C_2;
  // data for group-level effects of ID 1 
  int<lower=1> J_1[N]; 
  int<lower=1> N_1; 
  int<lower=1> M_1; 
  vector[N] Z_1_alpha_1; 
  // data for group-level effects of ID 2 
  int<lower=1> J_2[N]; 
  int<lower=1> N_2; 
  int<lower=1> M_2; 
  vector[N] Z_2_alpha_1; 
  // data for group-level effects of ID 3 
  int<lower=1> J_3[N]; 
  int<lower=1> N_3; 
  int<lower=1> M_3; 
  vector[N] Z_3_beta_1; 
  // data for group-level effects of ID 4 
  int<lower=1> J_4[N]; 
  int<lower=1> N_4; 
  int<lower=1> M_4; 
  vector[N] Z_4_beta_1; 
} 
transformed data { 
} 
parameters { 
  vector[K_alpha] b_alpha;  // population-level effects 
  vector[K_beta] b_beta;  // population-level effects 
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations 
  vector[N_1] z_1[M_1];  // unscaled group-level effects 
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations 
  vector[N_2] z_2[M_2];  // unscaled group-level effects 
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations 
  vector[N_3] z_3[M_3];  // unscaled group-level effects 
  vector<lower=0>[M_4] sd_4;  // group-level standard deviations 
  vector[N_4] z_4[M_4];  // unscaled group-level effects 
  ordered[2] temp_Intercept;  // temporary thresholds 
} 
transformed parameters { 
  // group-level effects 
  vector[N_1] r_1_alpha_1 = sd_1[1] * (z_1[1]); 
  // group-level effects 
  vector[N_2] r_2_alpha_1 = sd_2[1] * (z_2[1]); 
  // group-level effects 
  vector[N_3] r_3_beta_1 = sd_3[1] * (z_3[1]); 
  // group-level effects 
  vector[N_4] r_4_beta_1 = sd_4[1] * (z_4[1]); 
} 
model { 
  vector[N] mu_alpha = X_alpha * b_alpha; 
  vector[N] mu_beta = X_beta * b_beta; 
  vector[N] mu; 
  
  for (n in 1:N) { 
    mu_alpha[n] = mu_alpha[n] + (r_1_alpha_1[J_1[n]]) * Z_1_alpha_1[n] + (r_2_alpha_1[J_2[n]]) * Z_2_alpha_1[n]; 
    mu_beta[n] = mu_beta[n] + (r_3_beta_1[J_3[n]]) * Z_3_beta_1[n] + (r_4_beta_1[J_4[n]]) * Z_4_beta_1[n]; 
    // compute non-linear predictor 
    mu[n] = C_1[n] * mu_alpha[n] * C_2[n] * mu_beta[n]; 
    target += ordered_logistic_lpmf(Y[n] | mu[n], temp_Intercept);
  } 
  
  target += normal_lpdf(b_alpha | 0, 10); 
  target += normal_lpdf(b_beta | 0, 10); 
  target += student_t_lpdf(sd_1 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10); 
  target += normal_lpdf(z_1[1] | 0, 1); 
  target += student_t_lpdf(sd_2 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10); 
  target += normal_lpdf(z_2[1] | 0, 1); 
  target += student_t_lpdf(sd_3 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10); 
  target += normal_lpdf(z_3[1] | 0, 1); 
  target += student_t_lpdf(sd_4 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10); 
  target += normal_lpdf(z_4[1] | 0, 1);
  
} 
generated quantities { 
} 
