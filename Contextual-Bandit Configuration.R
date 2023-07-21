####################################################################
##################### Section 0: note
####################################################################





####################################################################
##################### Section 1: configurations
####################################################################

#####settings that can be changed
action_name <- 'a'#should match whatever in the formula defined after this
beta_name <- 'b'
true_beta_name <- 'mu'
context_name <- 'c'
name_list <-  c(action_name,beta_name,true_beta_name,context_name)


##############   list of tunning parameters
#1. 'n', sample size,
#2. 'contextual_matrix_large', a matrix describing the dsitribution of contexts. Each columns denotes a differnt context (by default it's c1, c2,...). Can modify to be more flexible
#3. 'mu': true beta parameters generating rewards;
#4. name_list <- c(action_name,beta_name,context_name). Name identifiers
#5. true_reward <- 'b0+b1*a1+b2*c1+b3*a1*c1'
#   reward_model <- 'b0+b1*a1+b2*c1+b3*a1*c1'
#6. 'sig_err', variance of reward error
#7. 'priors', (a0=2,b0=1,mu0=rep(0,k),L0=0.01*diag(k))

n <- 560
batch_size <- 4
burnin <- 8

############# study B2 
##################

#Rating = intercept + BetaRationale*GetRationale + BetaRationaleTimeOfDay*GetRationale*TimeOfDay
#b0: Intercept
#b1: BetaRationale
#b2: BetaRationaleTimeOfDay
#a1: GetRationale
#a2: NoRationale
#c1: TimeOfDay

#DidActivity = intercept + BetaRationale*GetRationale + BetaRationaleTimeOfDay*GetRationale*TimeOfDay
#b0: Intercept
#b1: BetaRationale
#b2: BetaRationaleTimeOfDay
#a1: GetRationale
#c1: TimeOfDay
true_reward <- 'mu0+mu1*a1+mu2*c1+mu3*c2*a1'
reward_model <- 'match' 
mu <- c(0.4,0.1,0.1,-0.15) #c(0.3,0.1,-0.05), c(0.6,-0.1,0.05)
contextual_distribution_table <- data.frame(c1=c(0.1,0.2,0.3,0.4),
                                            c2=c(0.4,0.1,0.1,0.4))
mu_prior <- c(0,0,0,0) #c(0.6,0.1,0), c(0.6,0.2,0)
var_diag <- 5 #inverse.  500, 0.05


#study C1
#true_reward <- 'mu0+mu1*a1+mu2*a2+mu3*c1+mu4*a1*c1+mu5*a2*c1'

#Study C2
#true_reward <- 'mu0+mu1*a1+mu2*a2+mu3*a3+mu4*a1*c1+mu5*a2*c1+mu6*a3*c1'

#Study C3
#true_reward <- 'mu0+mu1*a1+mu2*c1*a1'


#test: can we get rid of it??
sig_err <- 1/36 #variance of reward error
reward_setting <- list(mu=mu,sig_err=sig_err)
reward_setting$reward_generation <- function(mean_reward){
  #y <- median(c(0,1,round(y*4)/4))
  #y <- rnorm(1,mean=mean_reward,sd=sqrt(sig_err))
  #y <- median(c(0,1,y))
  y <- rbernoulli(1,p=mean_reward)
  return(y)
}


############ fixed setting
#######################

if (reward_model=='match'){
  reward_model <- paste(str_split(true_reward,'mu')[[1]],collapse = 'b')
}

num_context <- length(unique(get_indices(paste0(true_reward,reward_model),'c')))



Var_cov_prior <- var_diag*diag(length(mu_prior))
para_priors <- list(a=2,b=1,mu=mu_prior,L=Var_cov_prior)
process_reward_model(true_reward,reward_model,name_list)
