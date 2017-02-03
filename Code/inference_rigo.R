rm(list=ls())
library('R2jags')
setwd("~/Documents/Luis/Research Projects/CowLogRigo/Data")
load('misconceptions.Rdata')

misconceptions_binary <- array(dim=dim(misconceptions))
misconceptions_binary[misconceptions==1|misconceptions==2] <- 0
misconceptions_binary[misconceptions==3|misconceptions==4] <- 1
n_participants <- nrow(misconceptions_binary)
n_questions <- ncol(misconceptions_binary)

data_jags <- list('misconceptions_binary','n_participants','n_questions')

# m1_MC: 'model 1, misconceptions'
write('model{
      # Observed level
      for(p in 1:n_participants){
      for(q in 1:n_questions){
      misconceptions_binary[p,q]~dbern(theta[p,q])
      predictive[p,q]~dbern(theta[p,q])
      
      # No Rasch:      
      # theta[p,q] <- lat_par[p]*lat_ques[q]
      
      # Rasch model:
      theta[p,q] <- exp(lat_par[p]-lat_ques[q])/(1+exp(lat_par[p]-lat_ques[q]))
      }
      }
      
      # Priors
      for(p in 1:n_participants){
      lat_par[p]~dunif(-6,6)
      }
      for(q in 1:n_questions){
      lat_ques[q]~dunif(-6,6)
      }
      }','m1_MC.bug')

nodes <- c('theta','lat_par','lat_ques','predictive')

start <- list(list(lat_par=runif(n_participants,-1,1),
                   lat_ques=runif(n_questions,-1,1)),
              list(lat_par=runif(n_participants,-1,1),
                   lat_ques=runif(n_questions,-1,1)),
              list(lat_par=runif(n_participants,-1,1),
                   lat_ques=runif(n_questions,-1,1)))

posteriors <- jags(
  data=data_jags,
  inits=start,
  parameters.to.save=nodes,
  model.file='m1_MC.bug',
  n.chains=3,
  n.iter=6000,
  n.burnin=5000,
  n.thin=1)
posteriors$observed_data <- misconceptions_binary
summary(posteriors$BUGSoutput$summary)



