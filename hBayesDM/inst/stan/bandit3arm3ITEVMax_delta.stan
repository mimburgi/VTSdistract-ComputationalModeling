data {
  int<lower=1> N;
  int<lower=1> T;               
  int<lower=1,upper=T> Tsubj[N];                 
  int<lower=1,upper=4> choice[N,T];     
  real outcome[N,T];  // no lower and upper bounds   
}

transformed data {
  vector[4] initV;  // initial values for EV
  initV = rep_vector(0.0, 4);
}

parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters  
  vector[4] mu_p;  
  vector<lower=0>[4] sigma;
    
  // Subject-level raw parameters (for Matt trick)
  vector[N] A_pr;    // learning rate
  vector[N] tauR_pr;  // inverse temperature
  vector[N] tauF_pr;  // inverse temperature for features
  vector[N] tauN_pr;  // inverse temperature for features
}

transformed parameters {
  // subject-level parameters
  vector<lower=0,upper=1>[N] A;
  vector<lower=0,upper=5>[N] tauR;
  vector<lower=0,upper=5>[N] tauF;
  vector<lower=0,upper=5>[N] tauN;
  
  for (i in 1:N) {
    A[i]   = Phi_approx( mu_p[1]  + sigma[1]  * A_pr[i] );
    tauR[i] = Phi_approx( mu_p[2] + sigma[2] * tauR_pr[i] ) * 5;
    tauF[i] = Phi_approx( mu_p[3] + sigma[3] * tauF_pr[i] ) * 5;
    tauN[i] = Phi_approx( mu_p[4] + sigma[4] * tauN_pr[i] ) * 5;
  }
}

model {
  // Hyperparameters
  mu_p  ~ normal(0, 1); 
  sigma ~ cauchy(0, 5);  
  
  // individual parameters
  A_pr   ~ normal(0,1);
  tauR_pr ~ normal(0,1);
  tauF_pr ~ normal(0,1);
  tauN_pr ~ normal(0,1);
  
  
  // subject loop and trial loop
  for (i in 1:N) {
    vector[4] ev; // expected value
    real PE;      // prediction error
    real maxchoice;

    ev = initV;
    
      if (ev[1] >= ev[2] && ev[1] >= ev[3] && ev[1] >= ev[4]) {
      maxchoice = ev[1];
      }
      else if (ev[2] >= ev[1] && ev[2] >= ev[3] && ev[2] >= ev[4]) {
        maxchoice = ev[2];
      }
      else if (ev[3] >= ev[1] && ev[3] >= ev[2] && ev[3] >= ev[4]) {
        maxchoice = ev[3];
      }
      else {
        maxchoice = ev[4];
      }

    for (t in 1:(Tsubj[i])) {        
      // compute action probabilities
      if ( maxchoice == ev[1]) { 
        choice[i,t] ~ categorical_logit( tauR[i] * ev );
      } else if (maxchoice == ev[2]) {                  // x(t) < 0
        choice[i,t] ~ categorical_logit( tauF[i] * ev );
      } 
        else if (maxchoice == ev[3]) {
         choice[i,t] ~ categorical_logit( tauN[i] * ev ); 
      }
        else {
         choice[i,t] ~ categorical_logit( ev ); 
      }      
      
      

      // prediction error 
      PE = outcome[i,t] - ev[choice[i,t]];
            
      // value updating (learning) 
      ev[choice[i,t]] = ev[choice[i,t]] + A[i] * PE; 
    }
  }
}

generated quantities {
  // For group level parameters
  real<lower=0,upper=1> mu_A; 
  real<lower=0,upper=5> mu_tauR;
  real<lower=0,upper=5> mu_tauF;
  real<lower=0,upper=5> mu_tauN;
  
  // For log likelihood calculation
  real log_lik[N]; 

  mu_A   = Phi_approx(mu_p[1]);
  mu_tauR = Phi_approx(mu_p[2]) * 5;
  mu_tauF = Phi_approx(mu_p[3]) * 5;
  mu_tauN = Phi_approx(mu_p[4]) * 5;

  { // local section, this saves time and space
    for (i in 1:N) {
      vector[4] ev; // expected value
      real PE;      // prediction error
      real maxchoice;
      
      // Initialize values
      ev = initV;
      
      if (ev[1] >= ev[2] && ev[1] >= ev[3] && ev[1] >= ev[4]) {
      maxchoice = ev[1];
      }
      else if (ev[2] >= ev[1] && ev[2] >= ev[3] && ev[2] >= ev[4]) {
        maxchoice = ev[2];
      }
      else if (ev[3] >= ev[1] && ev[3] >= ev[2] && ev[3] >= ev[4]) {
        maxchoice = ev[3];
      }
      else {
        maxchoice = ev[4];
      }
      
      
      
      log_lik[i] = 0;
      
      for (t in 1:(Tsubj[i])) {
        // compute action probabilities
      if ( maxchoice == ev[1]) { 
        log_lik[i] = log_lik[i] + categorical_logit_lpmf(choice[i,t] | tauR[i] * ev);
      } else if (maxchoice == ev[2]) {                  // x(t) < 0
        log_lik[i] = log_lik[i] + categorical_logit_lpmf(choice[i,t] | tauF[i] * ev);
      } 
        else if (maxchoice == ev[3] ) {
         log_lik[i] = log_lik[i] + categorical_logit_lpmf(choice[i,t] | tauN[i] * ev);
      }    
      else {
         log_lik[i] = log_lik[i] + categorical_logit_lpmf(choice[i,t] | ev);
      }   
        
        
        // prediction error 
        PE = outcome[i,t] - ev[choice[i,t]];

        // value updating (learning) 
        ev[choice[i,t]] = ev[choice[i,t]] + A[i] * PE; 
      }
    }   
  }
}
