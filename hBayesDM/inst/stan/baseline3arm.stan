data {
  int<lower=1> N;
  int<lower=1> T;               
  int<lower=1,upper=T> Tsubj[N];                 
  int<lower=1,upper=4> choice[N,T];     
  real outcome[N,T];  # no lower and upper bounds
  #real<lower=0,upper=1> theta;
}

transformed data {
  vector[4] initV;  # initial values for EV
  initV = rep_vector(0.0, 4);
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
  vector<lower=0,upper=1>[N] tau;
  
  for (i in 1:N) {
    A[i]   = Phi_approx( mu_p[1]  + sigma[1]  * A_pr[i] );
    tau[i] = Phi_approx( mu_p[2] + sigma[2] * tau_pr[i] );
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
    vector[4] ev; # expected value
    real PE;      # prediction error
    real theta;

    ev = initV;

    for (t in 1:(Tsubj[i])) {
      if ( choice[i,t] == 1) {  # x(t) >= 0
        theta = A[i];
      } 
      if ( choice[i,t] == 2) {  # x(t) >= 0
        theta = tau[i];
      }   
      if ( choice[i,t] > 2) {  # x(t) >= 0
        theta = 1-tau[i]-A[i];
      } 
      
      # compute action probabilities
      choice[i,t] ~ bernoulli( theta );

      # prediction error 
      PE = outcome[i,t] - ev[choice[i,t]];
      
       
      # decay-RI
      ev = ev * A[i];            
      ev[ choice[i, t] ] = ev[ choice[i, t] ] + outcome[i,t];      
      
    }
  }
}

generated quantities {
  # For group level parameters
  real<lower=0,upper=1> mu_A; 
  real<lower=0,upper=1> mu_tau;
  
  # For log likelihood calculation
  real log_lik[N]; 

  mu_A   = Phi_approx(mu_p[1]);
  mu_tau = Phi_approx(mu_p[2]);

  { # local section, this saves time and space
    for (i in 1:N) {
      vector[4] ev; # expected value
      real PE;      # prediction error
      real theta;

      # Initialize values
      ev = initV;
      
      log_lik[i] = 0;
      
      for (t in 1:(Tsubj[i])) {
        # compute action probabilities
      if ( choice[i,t] == 1) {  # x(t) >= 0
        theta = A[i];
      } 
      if ( choice[i,t] == 2) {  # x(t) >= 0
        theta = tau[i];
      }   
      if ( choice[i,t] > 2) {  # x(t) >= 0
        theta = 1-tau[i]-A[i];
      } 
      
      # compute action probabilities
            
        
        log_lik[i] = log_lik[i] + bernoulli_logit_lpmf(choice[i,t] | theta);
        
        # prediction error 
        PE = outcome[i,t] - ev[choice[i,t]];

        # value updating (learning) 
        ev[choice[i,t]] = ev[choice[i,t]] + A[i] * PE; 
      }
    }   
  }
}
