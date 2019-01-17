data {
  int<lower=1> N;
  int<lower=1> T;               
  int<lower=1,upper=T> Tsubj[N];                 
  int<lower=1,upper=2> choice[N,T];     
  real outcome[N,T];  # no lower and upper bounds   
}

transformed data {
  vector[2] initV;  # initial values for EV
  vector[2] initET;  # initial values for EV
  initV = rep_vector(60.0, 2); //changed starting point to 60
  initET = rep_vector(0.0, 2); //starting for ET traces is zero
}

parameters {
# Declare all parameters as vectors for vectorizing
  # Hyper(group)-parameters  
  vector[2] mu_p;  
  vector<lower=0>[2] sigma;
    
  # Subject-level raw parameters (for Matt trick)
  vector[N] A_pr;    # learning rate
  vector[N] tau_pr;  # inverse temperature
}

transformed parameters {
  # subject-level parameters
  vector<lower=0,upper=1>[N] A;
  vector<lower=0,upper=5>[N] tau;
  
  for (i in 1:N) {
    A[i]   = Phi_approx( mu_p[1]  + sigma[1]  * A_pr[i] );
    tau[i] = Phi_approx( mu_p[2] + sigma[2] * tau_pr[i] ) * 5;
  }
}

model {
  # Hyperparameters
  mu_p  ~ normal(0, 1); 
  sigma ~ cauchy(0, 5);  
  
  # individual parameters
  A_pr   ~ normal(0,1);
  tau_pr ~ normal(0,1);
  
  # subject loop and trial loop
  for (i in 1:N) {
     vector[2] ev; # expected value
    vector[2] et; # expected value
    vector[2] normet; # expected value
    real PE;      # prediction error
    real sumet;

    ev = initV;
    et = initET;

    for (t in 1:(Tsubj[i])) {        
      # compute action probabilities
      choice[i,t] ~ categorical_logit( tau[i] * ev );

      # prediction error 
      PE = outcome[i,t] - ev[choice[i,t]];
      
      # increment trace for chosen option 
      et[choice[i,t]] = et[choice[i,t]] + 1; 
      sumet = et[1]+et[2];
      
      for (j in 1:2) {
      # value updating (learning)
      normet[j]=et[j]/sumet;
      ev[j] = ev[j] + normet[j]*(outcome[i,t] - ev[j]); //reward based on credit
      et[j] = et[j]*A[i]; //traces decay
      
      }
    }
  }
}

generated quantities {
  # For group level parameters
  real<lower=0,upper=1> mu_A; 
  real<lower=0,upper=5> mu_tau;
  
  # For log likelihood calculation
  real log_lik[N]; 

  mu_A   = Phi_approx(mu_p[1]);
  mu_tau = Phi_approx(mu_p[2]) * 5;

  { # local section, this saves time and space
    for (i in 1:N) {
     vector[2] ev; # expected value
    vector[2] et; # expected value
    vector[2] normet; # expected value
    real PE;      # prediction error
    real sumet;

    ev = initV;
    et = initET;
      
      log_lik[i] = 0;
      
      for (t in 1:(Tsubj[i])) {
        # compute action probabilities
        log_lik[i] = log_lik[i] + categorical_logit_lpmf(choice[i,t] | tau[i] * ev);
        
      # prediction error 
      PE = outcome[i,t] - ev[choice[i,t]];
      
      # increment trace for chosen option 
      et[choice[i,t]] = et[choice[i,t]] + 1; 
      sumet = et[1]+et[2];
      
      for (j in 1:2) {
      # value updating (learning)
      normet[j]=et[j]/sumet;
      ev[j] = ev[j] + normet[j]*(outcome[i,t] - ev[j]); //reward based on credit
      et[j] = et[j]*A[i]; //traces decay
      }
      }
    }   
  }
}
