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
  matrix[250,3] initStim;
  //vector[2] initET;  // initial values for EV
  initSim = rep_vector(1.0, 2); //starting point for similarity is 1, 
  
  initStim = rep_matrix(0, 250, 3);
  //initET = rep_vector(0.0, 2); //starting for ET traces is zero
  //vector[2] initet;  // initial values for EV
  //initet <- [0 0];
  //vector[2] initet;  // initial values for EV
  //initet = rep_vector(0.0, 0.01);  
}

parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters  
  vector[3] mu_p;  
  vector<lower=0>[3] sigma;
    
  // Subject-level raw parameters (for Matt trick)
  vector[N] wdim1_pr;    //attention to dimension 1 
  vector[N] c_pr;  // sensitivity
  
  vector[N] gam_pr;  // gamma
}

transformed parameters {
  // subject-level parameters
  vector<lower=0,upper=1>[N] wdim1;
  vector<lower=0,upper=5>[N] c;
  //vector<lower=0,upper=1>[N] gam;
  vector<lower=0,upper=5>[N] gam;
  
  for (i in 1:N) {
    wdim1[i]   = Phi_approx( mu_p[1]  + sigma[1]  * wdim1_pr[i] );
    c[i] = Phi_approx( mu_p[2] + sigma[2] * c_pr[i] ) * 5;
    //gam[i] = Phi_approx( mu_p[3] + sigma[3]  * gam_pr[i] );
    gam[i] = Phi_approx( mu_p[3] + sigma[3] * gam_pr[i] ) * 5;
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
  gam_pr   ~ normal(0,1);
  
  // subject loop and trial loop
  for (i in 1:N) {
    vector[2] Sim; // summed similarity
    vector[2] p; //
    vector[2] dummy; //
    matrix[250,3] Stim1;
    matrix[250,3] Stim2;
    real dist1;      // distance for dimension 1
    real dist2;
    int nstim1;
    int nstim2;
    real shiftdim1;
    real shiftdim2;
    nstim1=0;
    nstim2=0;
    //real sumet;

    Sim = initSim;
    Stim1= initStim;//initialize the storage banks for stimuli
    Stim2= initStim;
    
    
   
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
      
      
      if (nstim1 >= 1){
        for (j in 1 : nstim1) {
        Sim[1]=Sim[1] + exp(-c[i]*(wdim1[i]*fabs(dim1[i,t]-Stim1[j,1]) + (1-wdim1[i])*fabs(dim2[i,t]-Stim1[j,2])));
        }
      }
        
      if (nstim2 >= 1){
        for (j in 1 : nstim2) {
        Sim[2] = Sim[2] + exp(-c[i]*(wdim1[i]*fabs(dim1[i,t]-Stim2[j,1]) + (1-wdim1[i])*fabs(dim2[i,t]-Stim2[j,2])));
     
      
      }
      }
      
            for (d in 1 : 2)  // loop over decks
        dummy[d] = Sim[d];       
      
      for (d in 1 : 2)  // loop over decks
        p[d] = dummy[d] / sum(dummy); 
          
      choice[i,t] ~ categorical(p);
      
      
      //now learn from feedback
      shiftdim1=0;
      shiftdim2=0;
      // store and shift
      if (outcome[i,t]==1){
        //Assimilation
        if (nstim1>=1){
          for (k in 1:nstim1) {
            shiftdim1=shiftdim1 + (Stim1[k,1]-dim1[i,t]);
            shiftdim2=shiftdim2 + (Stim1[k,2]-dim2[i,t]);
          }
        }
        //Contrast//
        if (nstim2>=1){
          for (k in 1:nstim2) {
            shiftdim1=shiftdim1 + (dim1[i,t]-Stim2[k,1]);
            shiftdim2=shiftdim2 + (dim2[i,t]-Stim2[k,2]);
          }
        }
        nstim1=nstim1+1;
        Stim1[nstim1,1]=dim1[i,t]+gam[i]*shiftdim1;
        Stim1[nstim1,2]=dim2[i,t]+gam[i]*shiftdim2;
      }

      if (outcome[i,t]==2){
        //Assimilation
        if (nstim2>=1){
          for (k in 1:nstim2) {
            shiftdim1=shiftdim1 + (Stim2[k,1]-dim1[i,t]);
            shiftdim2=shiftdim2 + (Stim2[k,2]-dim2[i,t]);
          }
        }
        //Contrast//
        if (nstim1>=1){
          for (k in 1:nstim1) {
            shiftdim1=shiftdim1 + (dim1[i,t]-Stim1[k,1]);
            shiftdim2=shiftdim2 + (dim2[i,t]-Stim1[k,2]);
          }
        }
        nstim2=nstim2+1;
        Stim2[nstim2,1]=dim1[i,t]+gam[i]*shiftdim1;
        Stim2[nstim2,2]=dim2[i,t]+gam[i]*shiftdim2;
      }             

        
      
    }
    

  }
}

generated quantities {
  // For group level parameters
  real<lower=0,upper=1> mu_wdim1; 
  real<lower=0,upper=5> mu_c;
  //real<lower=0,upper=1> mu_gam;
  real<lower=0,upper=5> mu_gam;
  
  // For log likelihood calculation
  real log_lik[N]; 

  mu_wdim1   = Phi_approx(mu_p[1]);
  mu_c = Phi_approx(mu_p[2]) * 5;
  //mu_gam   = Phi_approx(mu_p[3]);
  mu_gam   = Phi_approx(mu_p[3]) * 5;

  { // local section, this saves time and space
    for (i in 1:N) {
    vector[2] Sim; // summed similarity
    vector[2] p; //
    vector[2] dummy; //
    matrix[250,3] Stim1;
    matrix[250,3] Stim2;
    real dist1;      // distance for dimension 1
    real dist2;
    int nstim1;
    int nstim2;
    real shiftdim1;
    real shiftdim2;
    nstim1=0;
    nstim2=0;
    //real sumet;

    Sim = initSim;
    Stim1= initStim;//initialize the storage banks for stimuli
    Stim2= initStim;

      
      
      log_lik[i] = 0;
      
      
             // Trial 1
    for (d in 1 : 2)  // loop over choice
    {  
      p[d] = .5;
      

    } 
        // compute action probabilities
        log_lik[i] = log_lik[i] + categorical_lpmf(choice[i,1] | p);    
    
    
    for (t in 1:(Tsubj[i]-1)) { 
      Sim = initSim;//initalize the similarities values on each trial
      // get sumSim
      
      
      if (nstim1 >= 1){
        for (j in 1 : nstim1) {
        Sim[1]=Sim[1] + exp(-c[i]*(wdim1[i]*fabs(dim1[i,t]-Stim1[j,1]) + (1-wdim1[i])*fabs(dim2[i,t]-Stim1[j,2])));
        }
      }
        
      if (nstim2 >= 1){
        for (j in 1 : nstim2) {
        Sim[2] = Sim[2] + exp(-c[i]*(wdim1[i]*fabs(dim1[i,t]-Stim2[j,1]) + (1-wdim1[i])*fabs(dim2[i,t]-Stim2[j,2])));
     
      
      }
      }
      
            for (d in 1 : 2)  // loop over decks
        dummy[d] = Sim[d];       
      
      for (d in 1 : 2)  // loop over decks
        p[d] = dummy[d] / sum(dummy); 
          
      //choice[i,t] ~ categorical(p);
      // compute action probabilities
      //choice[i,t] ~ categorical_logit( tau[i] * V );
      shiftdim1=0;
      shiftdim2=0;
      // store and shift
      if (outcome[i,t]==1){
        //Assimilation
        if (nstim1>=1){
          for (k in 1:nstim1) {
            shiftdim1=shiftdim1 + (Stim1[k,1]-dim1[i,t]);
            shiftdim2=shiftdim2 + (Stim1[k,2]-dim2[i,t]);
          }
        }
        //Contrast//
        if (nstim2>=1){
          for (k in 1:nstim2) {
            shiftdim1=shiftdim1 + (dim1[i,t]-Stim2[k,1]);
            shiftdim2=shiftdim2 + (dim2[i,t]-Stim2[k,2]);
          }
        }
        nstim1=nstim1+1;
        Stim1[nstim1,1]=dim1[i,t]+gam[i]*shiftdim1;
        Stim1[nstim1,2]=dim2[i,t]+gam[i]*shiftdim2;
      }

      if (outcome[i,t]==2){
        //Assimilation
        if (nstim2>=1){
          for (k in 1:nstim2) {
            shiftdim1=shiftdim1 + (Stim2[k,1]-dim1[i,t]);
            shiftdim2=shiftdim2 + (Stim2[k,2]-dim2[i,t]);
          }
        }
        //Contrast//
        if (nstim1>=1){
          for (k in 1:nstim1) {
            shiftdim1=shiftdim1 + (dim1[i,t]-Stim1[k,1]);
            shiftdim2=shiftdim2 + (dim2[i,t]-Stim1[k,2]);
          }
        }
        nstim2=nstim2+1;
        Stim2[nstim2,1]=dim1[i,t]+gam[i]*shiftdim1;
        Stim2[nstim2,2]=dim2[i,t]+gam[i]*shiftdim2;
      }             

        // compute action probabilities
        log_lik[i] = log_lik[i] + categorical_lpmf(choice[i,t] | p);
        
             
      
    }
          
      
        

      
      
      
    }   
  }
}
