data {
  int<lower=1> T; //the number of days since the 2024 general election
  int<lower=1> J; //Number of parties (including reference party)
  int<lower=1> P; //the number of pollsters
  int<lower=1> N; //the number of polls
  array[N] int poll_time; //the date the poll was conducted on (taking latest date)
  array[N] int pollster; //the identity of the pollster
  array[N] int n; //sample size of each poll
  array[N,J] int y; //observed vote count from polling
}

parameters {
  matrix[J-1, T] a_raw; //support for non-reference parties, from T=1,...K
  matrix[J-1, P] delta_raw; //house effect, one for each party
}  

transformed parameters{
  matrix[J,T] a; //latent support including reference party
  matrix[J,P] delta; //house effect including reference party
  
  //Set latent support of the first party to 0 for all parties as per the referenced paper
  for(t in 1:T){
    a[1, t] = 0;
  }
  
  //Otherwise, the latent support of the party is the estimated parameter above
  for(j in 2:J){
    for(t in 1:T){
      a[j,t] = a_raw[j-1,t];
    }
  }
  
  // Set pollster house effect to 0 for the reference party
  for(p in 1:P){
    delta[1,p] = 0;
  }
  
  //Otherwise, constrain the pollster house effect to have a zero sum
  for(j in 2:J){
    for(p in 1:P){
      delta[j,p] = delta_raw[j-1,p] - mean(delta_raw[j-1]);
    }
  }
}

model {
  
  // Define initial support for non-reference parties
  for(j in 2:J){
    a_raw[j-1,1] ~ uniform(-10,10);
  }
  
  // Then, model latent support as a random walk. Prior variance is set to 0.05
  for(j in 2:J){
    for(t in 2:T){
      a_raw[j-1,t] ~ normal(a_raw[j-1,t-1], 0.05);
    }
  }
  
  //Set the house effect prior to a normal distribution with a variance of 0.1
  for(j in 2:J){
    for(p in 1:P){
      delta_raw[j-1,p] ~ normal(0, 0.1);
    }
  }
  
  //Likelihood as defined on the paper
  for(i in 1:N){
    vector[J] support;
    for(j in 1:J){
      support[j] = a[j, poll_time[i]] + delta[j, pollster[i]];
    }
    y[i] ~ multinomial(softmax(support));
  }
}

