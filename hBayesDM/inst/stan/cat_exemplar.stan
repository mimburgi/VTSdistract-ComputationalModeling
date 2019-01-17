data {
  int<lower=1> N;
  int<lower=1> T;               
  int<lower=1,upper=T> Tsubj[N];                 
  int<lower=1,upper=2> choice[N,T]; //label this as the cat choice    
  real outcome[N,T];  // the outcome or correct category 
  real dim1[N,T];
  real dim2[N,T];
}

transformed data {
  vector[2] initSim;  // initial values for EV
  //vector[2] initET;  // initial values for EV
  initSim = rep_vector(1.0, 2); //starting point for similarity is 1,   
  //initET = rep_vector(0.0, 2); //starting for ET traces is zero
  //vector[2] initet;  // initial values for EV
  //initet <- [0 0];
  //vector[2] initet;  // initial values for EV
  //initet = rep_vector(0.0, 0.01);  
}

parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters  
  vector[2] mu_p;  
  vector<lower=0>[2] sigma;
    
  // Subject-level raw parameters (for Matt trick)
  vector[N] wdim1_pr;    //attention to dimension 1 
  vector[N] c_pr;  // sensitivity
  
  //vector[N] gam_pr;  // gamma
}

transformed parameters {
  // subject-level parameters
  vector<lower=0,upper=1>[N] wdim1;
  vector<lower=0,upper=5>[N] c;
  
  //vector<lower=0,upper=5>[N] gam;
  
  for (i in 1:N) {
    wdim1[i]   = Phi_approx( mu_p[1]  + sigma[1]  * wdim1_pr[i] );
    c[i] = Phi_approx( mu_p[2] + sigma[2] * c_pr[i] ) * 5;
    //gam[i] = Phi_approx( mu_p[3] + sigma[3]  * gam_pr[i] );
    //gam[i] = Phi_approx( mu_p[3] + sigma[3] * gam_pr[i] ) * 5;
  }
}

model {
  // Hyperparameters
  mu_p  ~ normal(0, 1); 
  sigma ~ cauchy(0, 5);  
  
  // individual parameters
  wdim1_pr   ~ normal(0,1);
  c_pr ~ normal(0,1);
  //gam_pr   ~ normal(0,1);
 
  
  // subject loop and trial loop
  for (i in 1:N) {
    vector[2] Sim; // summed similarity
    vector[2] p; //
    vector[2] dummy; //
    
    real dist1;      // distance for dimension 1
    real dist2;
    //real sumet;

    Sim = initSim;
    
    
   
       // Trial 1
    for (d in 1 : 2)  // loop over choice
    {  
      p[d] = .5;
      

    }  
    choice[i,1] ~ categorical(p);
    //reamining trials
    for (t in 1:(Tsubj[i]-1)) { 
      Sim = initSim;//initalize the similarities values on each trial
      // get sumSim
      
      for (j in 1:(t-1)) {
      // value updating (learning)
      //normet[j]=et[j]/sumet;
      if (outcome[i,j] == 1)
        Sim[1]=Sim[1] + exp(-c[i]*(wdim1[i]*abs(dim1[i,t]-dim1[i,j]) + (1-wdim1[i])*abs(dim2[i,t]-dim2[i,j])));
      else
        Sim[2] = Sim[2] + exp(-c[i]*(wdim1[i]*abs(dim1[i,t]-dim1[i,j]) + (1-wdim1[i])*abs(dim2[i,t]-dim2[i,j])));
     
      
      }
      
            for (d in 1 : 2)  // loop over decks
        dummy[d] = Sim[d];       
      
      for (d in 1 : 2)  // loop over decks
        p[d] = dummy[d] / sum(dummy); 
          
      choice[i,t] ~ categorical(p);
      // compute action probabilities
      //choice[i,t] ~ categorical_logit( tau[i] * V );

        
      
    }
    

  }
}

generated quantities {
  // For group level parameters
  real<lower=0,upper=1> mu_wdim1; 
  real<lower=0,upper=5> mu_c;
  //real<lower=0,upper=1> mu_gam;
  //real<lower=0,upper=5> mu_gam;
  
  // For log likelihood calculation
  real log_lik[N]; 

  mu_wdim1   = Phi_approx(mu_p[1]);
  mu_c = Phi_approx(mu_p[2]) * 5;
  //mu_gam   = Phi_approx(mu_p[3]);
  //mu_gam   = Phi_approx(mu_p[3]) * 5;

  { // local section, this saves time and space
    for (i in 1:N) {
    vector[2] Sim; // expected value
    vector[2] p;
    vector[2] dummy;
    real PE;
    
    
    //real sumet;

    Sim = initSim;
      //real sumet;

      
      
      log_lik[i] = 0;
      
      
             // Trial 1
    for (d in 1 : 2)  // loop over choice
    {  
      p[d] = .5;
      

    } 
        // compute action probabilities
        log_lik[i] = log_lik[i] + categorical_lpmf(choice[i,1] | p);    
    
    
    //remaining trials
    for (t in 1:(Tsubj[i]-1)) { 
      Sim = initSim;//initalize the similarities values on each trial
      // get sumSim
      
      for (j in 1:(t-1)) {
      // value updating (learning)
      //normet[j]=et[j]/sumet;
      if (outcome[i,j] == 1)
        Sim[1]=Sim[1] + exp(-c[i]*(wdim1[i]*abs(dim1[i,t]-dim1[i,j]) + (1-wdim1[i])*abs(dim2[i,t]-dim2[i,j])));
      else
        Sim[2] = Sim[2] + exp(-c[i]*(wdim1[i]*abs(dim1[i,t]-dim1[i,j]) + (1-wdim1[i])*abs(dim2[i,t]-dim2[i,j])));
     
      
      }
      
            for (d in 1 : 2)  // loop over decks
        dummy[d] = Sim[d];       
      
      for (d in 1 : 2)  // loop over decks
        p[d] = dummy[d] / sum(dummy); 
          
      
        
        // compute action probabilities
        log_lik[i] = log_lik[i] + categorical_lpmf(choice[i,t] | p);
        
      // prediction error 
      PE = 1 - p[choice[i,t]];
      
      
      }
    }   
  }
}
