/* Inference for antibody samples that had been taken from students before and after       */
/* a mumps epidemics in the Netherlands. Data are (with minor modifications) taken from    */
/* Gouma S et al (2014) Open Forum Infect Dis 1; doi: 10.1093/ofid/ofu101                  */
/* Code below comprises a random effects binary mixture model where p(infection) is        */
/* modeled with a logistic function, with dependent variable the ratio of log2 Ab titers   */
/* Copyright by Michiel van Boven and licensed under BSD (3-clause) 2019, 2020             */ 

data {
  int<lower=0> N;									                   // total number of subjects; notice that samples are paired
  real Titer_Pre[N];                                 // log2 IgG Jeryl-Lynn antibody titers in pre-epidemic samples
  real Titer_Post[N];                                // log2 IgG Jeryl-Lynn antibody titers in post-epidemic samples 
  real Prior_Mu_Pre;                                 // mu of Pre distribution (assumed uninfected)
  real Prior_Sigma_Pre;                              // sigma of Pre distribution (assumed uninfected)
  int<lower=0, upper=1> Mode;                        // 0 = regular sampling, 1 = sampling to compute WBIC
}

transformed data {
  real<lower=0, upper=1> watanabe_beta;              // defines WBIC sampling temperature (if mode <> 0)

  /* sampling temperature */
  if ( Mode == 0 ) { // normal sampling
  watanabe_beta = 1.0;
  }
  else { // WBIC sampling
  watanabe_beta = 1.0/log(2.0 * N);                  // number of likelihood contributions is 2N
  }
}

parameters {
  real Mu_Uninf;                                     // mean of antibody distribution in pre-epidemic sample
  real<lower = 0> Sigma_Uninf;                       // standard deviation of the antibody distribution in the pre-epidemic sample
  vector[N] Ind_Titer_Level;                         // random effect for uninfected persons	
  real<lower = 0> Sigma_Random;                      // random noise for uninfected persons
  real Mu_Inf;                                       // mean of antibody distribution in the post-epidemic sample
  real<lower = 0> Sigma_Inf;                         // standard deviations of the antibody distribution in the post-epidemic sample
  real<lower = 0, upper = 1> ProbInf1;               // probability of infection when no titer increase is observed
  real<lower = 0, upper = 1> ProbInf4;               // probability of infection at fourfold titer increase 
}

transformed parameters {
  real kpar;
  real x0;
  vector[N] pjump;
  
  /* reparameterisation of ProbInf1/ProbInf4 to standard parameters of logistic function */
  kpar = 0.5 * ((log((1.0 / ProbInf1) - 1.0)) - (log((1.0 / ProbInf4) - 1.0)));
  x0 = -2 * ((log((1.0 / ProbInf1) - 1.0)) / ((log((1.0 / ProbInf4) - 1.0)) - (log((1.0 / ProbInf1) - 1.0))));
  
  for ( i in 1:N ) {
    pjump[i] = 1 / (1 + exp(-kpar * ((Titer_Post[i] - Titer_Pre[i]) - x0))); 
  }
  
}

model {
  /* prior distributions */    
  Mu_Uninf ~ normal(Prior_Mu_Pre, 0.1);              // random effect 
  Sigma_Uninf ~ normal(Prior_Sigma_Pre, 0.25);       // random effect
  Ind_Titer_Level ~ normal(Mu_Uninf, Sigma_Uninf);   // individual titer levels of uninfected persons
  //Sigma_Random ~ normal(0, 1);                     // random noise for individual titers
  //Mu_Inf ~ normal(10, 0.5);                        // mu of infected persons
  //Sigma_Inf ~ normal(0, 2);                        // sigma of infected persons
  ProbInf1 ~ beta(1,9); //1.1, 1.1);                 // prior on p(infection) when no increase is observed; anything goes
  ProbInf4 ~ beta(9,1); //1.1, 1.1);                 // prior on p(infection) when fourfold increase is observed; anything goes

  /* likelihood contributions */
  target += watanabe_beta * normal_lpdf(Titer_Pre | Ind_Titer_Level, Sigma_Random); 
  for ( i in 1:N ) { 
      target += watanabe_beta * log((1-pjump[i])*exp(normal_lpdf(Titer_Post[i] | Ind_Titer_Level[i], Sigma_Random)) 
      + pjump[i]*exp(normal_lpdf(Titer_Post[i] | Mu_Inf, Sigma_Inf)));
  }
}

generated quantities {
  vector[2*N] log_lik;                               // log-likelihood contributions
  vector[N] prob_inf_pred;                           // predicted probabilities of infection
  real<lower = 0, upper = 1> ProbInf2;               // probability of infection at twofold titer increase 
  real intraclass_corr;                              // intraclass correlation
  
  /* likelihood contributions for LOOIC/WBIC calculation  */
  /* and calculation of p(infection) for each participant */
  for ( i in 1:N ) {
    log_lik[i] = normal_lpdf(Titer_Pre[i] | Ind_Titer_Level[i], Sigma_Random);
    log_lik[N+i] = log((1-pjump[i])*exp(normal_lpdf(Titer_Post[i] | Ind_Titer_Level[i], Sigma_Random)) + pjump[i] * exp(normal_lpdf(Titer_Post[i] | Mu_Inf, Sigma_Inf)));
    prob_inf_pred[i] = (pjump[i] * exp(normal_lpdf(Titer_Post[i] | Mu_Inf, Sigma_Inf))) / ((pjump[i] * exp(normal_lpdf(Titer_Post[i] | Mu_Inf, Sigma_Inf))) + ((1 - pjump[i]) * exp(normal_lpdf(Titer_Post[i] | Ind_Titer_Level[i], Sigma_Uninf))));
  }
  
  /* calculation of probability of infection at twofold titer increase */
  ProbInf2 =  1 / (1 + exp(-kpar * (1.0 - x0)));
  
  /* intraclass correlation */
  intraclass_corr = Sigma_Uninf^2 / (Sigma_Uninf^2 + Sigma_Random^2);      
} 
