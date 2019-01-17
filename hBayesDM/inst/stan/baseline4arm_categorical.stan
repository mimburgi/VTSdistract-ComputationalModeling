data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1, upper=T> Tsubj[N];
  real outcome[N, T];
  int choice[N, T];
}
transformed data {
  vector[4] initV;
  initV  = rep_vector(0.0, 4);
}
parameters {
# Declare all parameters as vectors for vectorizing
  # Hyper(group)-parameters  
  vector[4] mu_p;  
  vector<lower=0>[4] sigma;
    
  # Subject-level raw parameters (for Matt trick)
  vector[N] C1_pr;
  vector[N] C2_pr;
  vector[N] C3_pr;
  vector[N] C4_pr;
  
}
transformed parameters {
  # Transform subject-level raw parameters
  
  vector[N] C1;
  vector[N] C2;
  vector[N] C3;
  vector[N] C4;
  
  

  
  C1 = mu_p[1] + sigma[1] * C1_pr;
  C2 = mu_p[2] + sigma[2] * C2_pr;
  C3 = mu_p[3] + sigma[3] * C3_pr;
  C4 = mu_p[4] + sigma[4] * C4_pr;  
}
model {
  # Hyperparameters
   
  mu_p[1]  ~ normal(0, 10.0);
  mu_p[2]  ~ normal(0, 10.0);
  mu_p[3]  ~ normal(0, 10.0);
  mu_p[4]  ~ normal(0, 10.0);  
   
  sigma ~ cauchy(0, 5);
  
  # individual parameters
  
  C1_pr    ~ normal(0, 1.0);
  C2_pr    ~ normal(0, 1.0);
  C3_pr    ~ normal(0, 1.0);
  C4_pr    ~ normal(0, 1.0);  
  

  for (i in 1:N) {
    # Define values
    
    vector[4] V;   # weighted sum of ev and pers

    real curUtil;     # utility of curFb
    real theta;       # theta = 3^c - 1
    
    # Initialize values
    
    V     = initV;
    V[1] = C1[i];
    V[2] = C2[i];
    V[3] = C3[i];
    V[4] = C4[i];    

    for (t in 1:Tsubj[i]) {
      # softmax choice
      choice[i, t] ~ categorical_logit( V );
      
      
    }
  }
}

generated quantities {
  # For group level parameters
  
  real mu_C1;
  real mu_C2;
  real mu_C3;
  real mu_C4;

  
  # For log likelihood calculation
  real log_lik[N];

  
  mu_C1    = mu_p[1];
  mu_C2    = mu_p[2];
  mu_C3    = mu_p[3];
  mu_C4    = mu_p[4];  
  
  
  { # local section, this saves time and space
    for (i in 1:N) {
      # Define values
      
      vector[4] V;   # weighted sum of ev and pers
  
     
      
      # Initialize values 
      log_lik[i] = 0;
      
      V     = initV;
      V[1] = C1[i];
      V[2] = C2[i];
      V[3] = C3[i];
      V[4] = C4[i];
  
      for (t in 1:Tsubj[i]) {
        # softmax choice
        log_lik[i] = log_lik[i] + categorical_logit_lpmf( choice[i, t] | V );
        

      }
    }
  }  
}
