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
  //vector[2] initet;  # initial values for EV
  //initet <- [0 0];
  //vector[2] initet;  # initial values for EV
  //initet = rep_vector(0.0, 0.01);  
}

parameters {
# Declare all parameters as vectors for vectorizing
  # Hyper(group)-parameters  
  vector[4] mu_p;  
  vector<lower=0>[4] sigma;
    
  # Subject-level raw parameters (for Matt trick)
  vector[N] A_pr;    # learning rate
  vector[N] tau_pr;  # inverse temperature
  vector[N] gam_pr;  # inverse temperature
  vector[N] P_pr;  # inverse temperature
}

transformed parameters {
  # subject-level parameters
  vector<lower=0,upper=1>[N] A;
  vector<lower=0,upper=5>[N] tau;
  vector<lower=0,upper=1>[N] gam;
  vector<lower=0,upper=1>[N] P;
  
  for (i in 1:N) {
    A[i]   = Phi_approx( mu_p[1]  + sigma[1]  * A_pr[i] );
    tau[i] = Phi_approx( mu_p[2] + sigma[2] * tau_pr[i] ) * 5;
    gam[i] = Phi_approx( mu_p[3] + sigma[3]  * gam_pr[i] );
    P[i] = Phi_approx( mu_p[4] + sigma[4]  * P_pr[i] );
  }
}

model {
  # Hyperparameters
  mu_p  ~ normal(0, 1); 
  sigma ~ cauchy(0, 5);  
  
  # individual parameters
  A_pr   ~ normal(0,1);
  tau_pr ~ normal(0,1);
  gam_pr   ~ normal(0,1);
  P_pr   ~ normal(0,1);
  
  # subject loop and trial loop
  for (i in 1:N) {
    vector[2] ev; # expected value
    vector[2] et; # expected value
    vector[2] normet; # expected value
    vector[2] pv;
    vector[2] V;
    real PE;      # prediction error
    real sumet;

    ev = initV;
    et = initET;
    pv = initET;
    V = initET;

    for (t in 1:(Tsubj[i])) {        
      # compute action probabilities
      choice[i,t] ~ categorical_logit( tau[i] * V );

      # prediction error 
      PE = outcome[i,t] - ev[choice[i,t]];
      
      
      # increment perseveration for chosen option 
      pv[choice[i,t]] = pv[choice[i,t]] + 1;      
      sumet = et[1]+et[2];
      
      for (j in 1:2) {
      # value updating (learning)
      normet[j]=et[j]/sumet;
      ev[j] = ev[j] + A[i] * et[j]*(outcome[i,t] - ev[j]); //reward based on credit
      # increment trace for chosen option 
      et[choice[i,t]] =  1;
      et[j] = et[j]*gam[i]; //traces decay
      pv[j] = pv[j]*P[i]; //traces decay
      
      }
      V = ev + pv;
    }
    

  }
}

generated quantities {
  # For group level parameters
  real<lower=0,upper=1> mu_A; 
  real<lower=0,upper=5> mu_tau;
  real<lower=0,upper=1> mu_gam;
  real<lower=0,upper=1> mu_P;
  
  # For log likelihood calculation
  real log_lik[N]; 

  mu_A   = Phi_approx(mu_p[1]);
  mu_tau = Phi_approx(mu_p[2]) * 5;
  mu_gam   = Phi_approx(mu_p[3]);
  mu_P   = Phi_approx(mu_p[4]);

  { # local section, this saves time and space
    for (i in 1:N) {
      vector[2] ev; # expected value
      real PE;      # prediction error
      vector[2] et; #eligibility trace
      vector[2] normet; # expected value
      vector[2] pv;
      vector[2] V;
      real sumet;

      # Initialize values
      ev = initV;
      et = initET;
      pv = initET;
      V = initET;
      
      log_lik[i] = 0;
      
      for (t in 1:(Tsubj[i])) {
        # compute action probabilities
        log_lik[i] = log_lik[i] + categorical_logit_lpmf(choice[i,t] | tau[i] * V);
        
      # prediction error 
      PE = outcome[i,t] - ev[choice[i,t]];
      
      
      # increment perseveration for chosen option 
      pv[choice[i,t]] = pv[choice[i,t]] + 1;      
      sumet = et[1]+et[2];
      
      for (j in 1:2) {
      # value updating (learning)
      normet[j]=et[j]/sumet;
      ev[j] = ev[j] + A[i] * et[j]*(outcome[i,t] - ev[j]); //reward based on credit
      # increment trace for chosen option 
      et[choice[i,t]] =  1;
      et[j] = et[j]*gam[i]; //traces decay
      pv[j] = pv[j]*P[i]; //traces decay
      
      }
      V = ev + pv;
      }
    }   
  }
}
