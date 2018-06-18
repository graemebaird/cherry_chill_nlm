data { 
  int<lower=1> N;  // total number of observations 
  int Y[N];  // response variable 
  int<lower=1> K_alpha;  // number of population-level effects 
  matrix[N, K_alpha] X_alpha;  // population-level design matrix 
  int<lower=1> K_beta;  // number of population-level effects 
  matrix[N, K_beta] X_beta;  // population-level design matrix 
  int<lower=1> K_eta;  // number of population-level effects 
  matrix[N, K_eta] X_eta;  // population-level design matrix 
  vector[N] C_1;
  vector[N] C_2;
  int<lower=1> N_Y;
  vector[N_Y] CP_Y;
  vector[N_Y] GDH_Y;
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
} 
transformed data { 
} 
parameters { 
  vector[K_alpha] b_alpha;  // population-level effects 
  real<lower=0> b_eta;  // population-level effects 
  real<upper=0> b_beta;  // population-level effects 
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations 
  vector[N_1] z_1[M_1];  // unscaled group-level effects 
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations 
  vector[N_2] z_2[M_2];  // unscaled group-level effects 
  ordered[2] temp_Intercept;  // temporary thresholds 
} 
transformed parameters { 
  // group-level effects 
  vector[N_1] r_1_alpha_1 = sd_1[1] * (z_1[1]); 
  // group-level effects 
  vector[N_2] r_2_alpha_1 = sd_2[1] * (z_2[1]); 
} 
model { 
  vector[N] mu_alpha = X_alpha * b_alpha; 
  real mu_beta = b_beta; 
  real mu_eta = b_eta; 
  vector[N] mu; 
  for (n in 1:N) { 
    mu_alpha[n] = mu_alpha[n] + (r_1_alpha_1[J_1[n]]) * Z_1_alpha_1[n] + (r_2_alpha_1[J_2[n]]) * Z_2_alpha_1[n]; 
    mu[n] = C_1[n] * (mu_alpha[n] / (1 + exp(-mu_beta - C_2[n]*mu_eta)));
    target += ordered_logistic_lpmf(Y[n] | mu[n], temp_Intercept);
  } 
  
  target += normal_lpdf(b_eta | 2, 1); 
  target += normal_lpdf(b_beta | -3, 1); 
  target += normal_lpdf(b_alpha | 0, 2); 
  target += student_t_lpdf(sd_1 | 3, 0, 10)
  - 1 * student_t_lccdf(0 | 3, 0, 10); 
  target += normal_lpdf(z_1[1] | 0, 1); 
  target += student_t_lpdf(sd_2 | 3, 0, 10)
  - 1 * student_t_lccdf(0 | 3, 0, 10); 
  target += normal_lpdf(z_2[1] | 0, 1); 
  
} 
// 

generated quantities {
  // Posterior predictive block
  vector[N] mu_alpha = X_alpha * b_alpha;
  real mu_beta =  b_beta;
  real mu_eta =  b_eta;
  real mu;
  real mu_rep;
  int Yrep[N];
  int Yhat[N_Y];
  real Ysat;

  for (n in 1:N) {
    mu_alpha[n] = mu_alpha[n] + (r_1_alpha_1[J_1[n]]) * Z_1_alpha_1[n] + (r_2_alpha_1[J_2[n]]) * Z_2_alpha_1[n];
    mu = C_1[n] * (mu_alpha[n] / (1 + exp(-mu_beta - C_2[n]*mu_eta)));
    Yrep[n] = ordered_logistic_rng(mu, temp_Intercept);
    Ysat = -(mu_beta - 2.292432)/mu_eta;
  }

  for (n in 1:N_Y) {
    mu_rep = GDH_Y[n] * (mu_alpha[n] / (1 + exp(-mu_beta - CP_Y[n]*mu_eta)));
    Yhat[n] = ordered_logistic_rng(mu_rep, temp_Intercept);
  }
}
