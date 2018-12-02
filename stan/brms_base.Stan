
functions { 
} 
data { 
  int<lower=1> N;  // total number of observations 
  vector[N] Y;  // response variable 
  int<lower=1> K_alpha;  // number of population-level effects 
  matrix[N, K_alpha] X_alpha;  // population-level design matrix 
  int<lower=1> K_beta;  // number of population-level effects 
  matrix[N, K_beta] X_beta;  // population-level design matrix 
  int<lower=1> K_eta;  // number of population-level effects 
  matrix[N, K_eta] X_eta;  // population-level design matrix 
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
  // data for group-level effects of ID 5
  int<lower=1> J_5[N];
  int<lower=1> N_5;
  int<lower=1> M_5;
  vector[N] Z_5_eta_1;
  // data for group-level effects of ID 6
  int<lower=1> J_6[N];
  int<lower=1> N_6;
  int<lower=1> M_6;
  vector[N] Z_6_eta_1;
  int prior_only;  // should the likelihood be ignored? 
} 
transformed data { 
} 
parameters { 
  vector[K_alpha] b_alpha;  // population-level effects 
  vector[K_beta] b_beta;  // population-level effects 
  vector[K_eta] b_eta;  // population-level effects 
  real<lower=0> sigma;  // residual SD 
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // unscaled group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // unscaled group-level effects
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  vector[N_3] z_3[M_3];  // unscaled group-level effects
  vector<lower=0>[M_4] sd_4;  // group-level standard deviations
  vector[N_4] z_4[M_4];  // unscaled group-level effects
  vector<lower=0>[M_5] sd_5;  // group-level standard deviations
  vector[N_5] z_5[M_5];  // unscaled group-level effects
  vector<lower=0>[M_6] sd_6;  // group-level standard deviations
  vector[N_6] z_6[M_6];  // unscaled group-level effects
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
  // group-level effects 
  vector[N_5] r_5_eta_1 = sd_5[1] * (z_5[1]);
  // group-level effects 
  vector[N_6] r_6_eta_1 = sd_6[1] * (z_6[1]);
} 
model { 
  vector[N] nlp_alpha = X_alpha * b_alpha;
  vector[N] nlp_beta = X_beta * b_beta;
  vector[N] nlp_eta = X_eta * b_eta;
  vector[N] mu;
  for (n in 1:N) { 
    nlp_alpha[n] += r_1_alpha_1[J_1[n]] * Z_1_alpha_1[n] + r_2_alpha_1[J_2[n]] * Z_2_alpha_1[n];
    nlp_beta[n] += r_3_beta_1[J_3[n]] * Z_3_beta_1[n] + r_4_beta_1[J_4[n]] * Z_4_beta_1[n];
    nlp_eta[n] += r_5_eta_1[J_5[n]] * Z_5_eta_1[n] + r_6_eta_1[J_6[n]] * Z_6_eta_1[n];
    // compute non-linear predictor 
    mu[n] = C_1[n] * (nlp_alpha[n] / (1 + exp( - nlp_beta[n] - C_2[n] * nlp_eta[n])));
  } 
  // priors including all constants 
  target += normal_lpdf(b_alpha | 0, 10); 
  target += normal_lpdf(b_beta | 0, 10); 
  target += normal_lpdf(b_eta | 0, 10); 
  target += student_t_lpdf(sigma | 3, 0, 10)
  - 1 * student_t_lccdf(0 | 3, 0, 10); 
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
  target += student_t_lpdf(sd_5 | 3, 0, 10)
  - 1 * student_t_lccdf(0 | 3, 0, 10); 
  target += normal_lpdf(z_5[1] | 0, 1);
  target += student_t_lpdf(sd_6 | 3, 0, 10)
  - 1 * student_t_lccdf(0 | 3, 0, 10); 
  target += normal_lpdf(z_6[1] | 0, 1);
  // likelihood including all constants 
  if (!prior_only) { 
    target += normal_lpdf(Y | mu, sigma);
  } 
} 
generated quantities { 
} 