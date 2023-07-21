####################################################################
##################### Section 0: note
####################################################################

#standardize your input/output
#also, if output is standardized, you can use it without too much review!


#We used this R code to simulate:
# 1. in three arm case, performance of Uniform Random v.s. Top two TS
# 2. in two arm, one contextual varaible (high/low accuracy) case, perfroa


####################################################################
##################### Section 1: package
####################################################################
library(invgamma)
library(pracma)
library(stringr)
library(mvtnorm)
library(tidyverse)
library(readr)
library(rebus)
library(dplyr)
library(progress)
options(dplyr.summarise.inform = FALSE)


####################################################################
##################### Section 2: helper function
####################################################################

get_indices <- function(expr,var_name,uniq=F){
  #get index numbers for a variable in a string
  #expr is the full string of the expression (e.g. reward model etc)
  
  ## Example: get_indices('b0+b1*a1+b2*c1+b3*a1*c1','c') -> c(1, 1)
  indices <- parse_number(str_split(expr,char_class(var_name))[[1]])[-1]
  indices <- indices[!is.na(indices)]
  if(uniq){
    #get unique, ordered indices
    indices <- unique(indices)
    indices <- indices[order(indices)]
  }
  return(indices)
}

get_IndexMatrix <- function(expr,index_list,sep='+'){
  
  ## example:get_IndexMatrix('b0+b1*a1+b2*c1+b3*a1*c1',index_list) -> 
  #    [,1] [,2] [,3] [,4]
  #a    NA    1   NA    1
  #b     0    1    2    3
  #mu   NA   NA   NA   NA
  #c    NA   NA    1    1
  
  s <- str_split(expr,char_class(sep))[[1]]
  m <- matrix(nrow=length(index_list),ncol=length(s))
  row.names(m) <-names(index_list) 
  for (i in name_list){
    for (j in 1:length(s)){
      ind <- parse_number(str_split(s[j],i)[[1]])[-1]
      if(length(ind>0)){
        m[i,j] <- ind
      }
    }
  }
  return(m)
}

index_mapping <- function(im,index_list){
  for(i in row.names(im)){
    for (j in 1:ncol(im)){
      if(!is.na(im[i,j])){
        im[i,j] <- which(index_list[[i]]==im[i,j])
      }
    }
  }
  return(im)
}

m2v <- function(m,v){
  #m is the matrix
  #v is the value list
  #will skip NAN values.
  #can also be viewed as the reverse of 'index_mapping' function
  for(i in row.names(m)){
    for (j in 1:ncol(m)){
      if(!is.na(m[i,j])){
        m[i,j] <- v[[i]][m[i,j]]
      }
    }
  }
  return(m)
}


####################################################################
##################### Section 3: Algorithm-level function
####################################################################

UR_sample_actions <- function(num_act,onehot=T){
  if (onehot){
    action_draw <- sample(1:(1+num_act),size=1)
    action_vector <- rep(0,num_act) #need to confirm same length as actual action array
    if (action_draw<=num_act){
      action_vector[action_draw] <- 1
    }
  } else {
    stop("need to re-work on UR_sample_actions function, no onehot=F option")
  }
  return(action_vector=action_vector)
}

blinear_update <- function(X,y,priors){
  a0 <- priors$a
  b0 <- priors$b
  mu0 <- priors$mu
  L0 <- priors$L
  n <- dim(X)[1]
  H <- t(X)%*%X
  #b_hat <- inv(H)%*%(t(X)%*%y)
  Ln <- H+L0
  mun <- inv(Ln) %*% ((t(X)%*%y)+L0%*%mu0)
  an <- a0+n/2
  bn <- b0+(t(y)%*%y+t(mu0)%*%L0%*%mu0-t(mun)%*%Ln%*%mun)/2
  return(list(a=an,b=bn,mu=mun,L=Ln))
}

#I think each action variable should be binary. Can explain more.
mat2exp <- function(mat){
  #input: a matrix, with rows corresponding to variable names, and entries corresponding to indices
  #output: a text of expression of everything collasped
  #return 1 if everything is NA.
  var_names <- row.names(mat)
  formula_pieces <- c()
  mat_copy <- mat #I think that may lead to bugs.. Better if just create an NA matrix with same row names
  for(i in var_names){
    for(j in 1:ncol(mat)){
      if(!is.na(mat[i,j])){
        mat_copy[i,j] <- paste0(i,'[',mat[i,j],']')#can do -1 for Python
      }
    }
  }
  for(j in 1:ncol(mat)){
    formula_pieces[j] <-paste(mat_copy[!is.na(mat_copy[,j]),j],collapse = '*') 
  }
  full_formula <- paste(formula_pieces,collapse='+')
  if(str_count(full_formula)==0){
    full_formula <- '1'
  }
  return(full_formula)
}

mat2coef <- function(mat,para_name){
  #mat2coef(regression_im_sort_ind,'a')
  #input: 'mat': a matrix, with rows corresponding to variable names, and entries corresponding to indices
  #input: 'para_name': the parameter you look for coef
  #output: a vector of texts of coefs
  coefs <- c()
  for (i in unique(mat[para_name,])){
    if(!is.na(i)){
      coefs[i] <- mat2exp(mat[rownames(mat) !=para_name,which(mat[para_name,]==i),drop = F])
    }
  }
  return(coefs)
}

process_reward_model <- function(true_reward,reward_model,name_list){
  #process_reward_model(true_reward,reward_model,name_list)
  #produce action_indicator function, input must be b= and c=
  #produce reward_eval function, input must be a, c and mu
  #produce get_design_matrix 
  index_list <<- list()
  for (i in name_list){
    index_list[[i]] <<- get_indices(paste0(true_reward,reward_model),i,uniq=T)
  }
  
  #act_indices <- get_indices(paste0(true_reward,reward_model),action_name,uniq=T)
  num_act <<- length(index_list[[action_name]])
  
  #beta_indices <- get_indices(paste0(true_reward,reward_model),beta_name,uniq=T)
  num_beta <<- length(index_list[[beta_name]])
  
  regression_im_ori_ind <- get_IndexMatrix(reward_model,index_list) #align with original index in formula
  regression_im_sort_ind <- index_mapping(regression_im_ori_ind,index_list)
  
  true_im_ori_ind <- get_IndexMatrix(true_reward,index_list) #align with original index in formula
  true_im_sort_ind <- index_mapping(true_im_ori_ind,index_list)
  
  coefs <- mat2coef(regression_im_sort_ind,action_name)
  reward_eval_text <- mat2exp(true_im_sort_ind)
  dm <- mat2coef(regression_im_sort_ind,beta_name)
  eval(parse(text= paste0('action_indicator<<-function(b,c,onehot=T){ ifelse(onehot, order(-c(',paste(coefs,collapse=','),',0))[1] ,  c(',paste(coefs,collapse=','),')>0) }') ))
  eval(parse(text= paste0('reward_eval<<-function(a,c,mu){return(',reward_eval_text,')}') ))
  eval(parse(text= paste0('get_design_matrix<<-function(a,c){c(',paste(dm,collapse=','),')>0}') ))
  return(list(action_indicator=coefs,get_design_matrix=dm,reward_eval=reward_eval_text))
  #return(list(num_act=num_act,coef_list=coef_list,act_list=act_list))
}

sample_arms <- function(para_post,num_act,context,epsilon=0){
  #July 10th notes: I guess need future edit. e.g. what if we change Bayesian model here? (also Bayesian model part should be one func, instaed of duplicating.)
  sigma_n <- rinvgamma(1,para_post$a,para_post$b) #sigma here is actually sigma square
  beta_n <- rmvnorm(1,mean=para_post$mu,sigma = sigma_n*inv(para_post$L))
  
  sample_return <- rep(0,num_act)
  if(runif(1)<epsilon){
    return(UR_sample_actions(num_act = num_act))
  } else {
    #if onehot =T, only return a single arm each time. Otherwise, a vector...
    #someone can change it
    arm_ind <- action_indicator(b=beta_n,c=context,onehot=T)
    
    if (arm_ind<=num_act){
      sample_return[arm_ind] <- 1
    }
    return(sample_return)
  }
  
}



sample_arms1 <- function(para_post,num_act,context,epsilon=0){
  #July 10th notes: I guess need future edit. e.g. what if we change Bayesian model here? (also Bayesian model part should be one func, instaed of duplicating.)
  sigma_n <- rinvgamma(1,para_post$a,para_post$b) #sigma here is actually sigma square
  beta_n <- rmvnorm(1,mean=para_post$mu,sigma = sigma_n*inv(para_post$L))
  
  sample_return <- rep(0,num_act)
  if(runif(1)<epsilon){
    return(UR_sample_actions(num_act = num_act))
  } else {
    #if onehot =T, only return a single arm each time. Otherwise, a vector...
    #someone can change it
    arm_ind <- action_indicator(b=beta_n,c=context,onehot=T)
    
    if (arm_ind<=num_act){
      sample_return[arm_ind] <- 1
    }
    return(list(sample_return=sample_return,sigma=sigma_n,beta=beta_n))
  }
  
}
#here is where I don't like. it's doing too many things.... entangled.
#too many inputs/outputs. doesn't follow any structure
#should name some variables. e.g. para_priors and reward_setting. 
#put some variables together (e.g. all those indicators in a list. and test if None. Better compatibility)
#wirte breakdown of this function

Contextual_Bandit <- function(para_priors,reward_setting,n,contextual_matrix,burnin,batch_size,epsilon=0){
  #it's quite clear what this does if you look at return. 
  
  #try to only output history (reward, context, and action)
  #maybe some other things if general, useful, no additional cost
  #### plans/ideas
  #1. save everythign as a vectore (instead of an array), change dim at the end
  #2, separate step-wise-useful results with other things, save the mseperately
  #how to track batch and colnames??
  
  #goal: I create something I want to track, and put all info (col name, dim, maybe batch) in one place. Then I don't need to worry
  
  #### Section 1: Input checking
  ## 1.1 check contextual matrix
  if (dim(contextual_matrix)[2]!=length(index_list[[context_name]])){
    stop("Incorrect number of columns in contextual_matrix")
  }
  
  #### Section 2: Initialization
  para_post <- para_priors
  
  #Question: what's a framework for naming columns, figure out dims, and assign values in loops.
  
  #2.1 create data frames/matrices to save step-wise result later
  #Content: act_hist, X, y
  act_hist <- array(NA,dim=c(n,num_act))
  
  X <- matrix(nrow=n,ncol=length(para_priors$mu))
  heading <- c()
  for (b in 1:length(para_priors$mu)){
    heading[b] <- paste0('X',b)
  }
  colnames(X) <- heading
  
  y <- matrix(nrow=n,ncol=1)
  value_list <- list()
  
  colnames(act_hist) <- paste(action_name, index_list[[action_name]], sep="")
  colnames(contextual_matrix)<- paste(context_name, index_list[[context_name]], sep="")
  colnames(y) <- 'reward'
  
  #burinin phase
  i <- 0
  while (i<burnin){
    i <- i+1
    #notes:
    #for arm, if we have k arms, than we need k-1 action variable. (think of regression equation).
    #their sum need to be <=1
    
    #original UR
    #act_hist[i,] <- 1*(runif(num_act)>1/2)
    
    #new design, sum <=1
    act_hist[i,] <- UR_sample_actions(num_act = num_act)
    
    #three arm_> 2 action variable, 11 resample 
    X[i,] <- 1*get_design_matrix(a=act_hist[i,],c=contextual_matrix[i,])
    mean_reward <- reward_eval(a=act_hist[i,],c=contextual_matrix[i,],mu=reward_setting$mu)
    y[i] <- reward_setting$reward_generation(mean_reward)
  }
  
  
  #should I do update here?
  if(i>0){
    para_post <- blinear_update(X[1:i,,drop = F],y[1:i],para_priors)
  }
  
  batch_ind <- 0
  while(i < n){
    i <- i+1
    batch_ind <- batch_ind+1
    #sample beta and sigma
    act_hist[i,] <- sample_arms(para_post,num_act,context=contextual_matrix[i,],epsilon=epsilon)
    
    X[i,] <- 1*get_design_matrix(a=act_hist[i,],c=contextual_matrix[i,])
    mean_reward <- reward_eval(a=act_hist[i,],c=contextual_matrix[i,],mu=reward_setting$mu)
    y[i] <- reward_setting$reward_generation(mean_reward)
    
    if (batch_ind==batch_size){
      batch_ind <- 0
      para_post <- blinear_update(X[1:i,,drop = F],y[1:i],para_priors)
    }
    
  }
  
  
  
  #return(list(para_post=para_post,X=X,y=y,his=his,AP_hist=AP_hist,lm_coef_hist=lm_coef_hist))
  return(list(X=X,reward_hist=y,action_hist=act_hist,contextual_hist=contextual_matrix[1:n,,drop = F]))
}

#for simulation on a single trial, calculating AP, Linear regression etc.

Contextual_Bandit_backup <- function(para_priors,reward_setting,n,contextual_matrix,burnin,batch_size,epsilon,record_AP=F,AP_b=50,record_coef=F,reduce_computation=F){
  #it's quite clear what this does if you look at return. 
  
  #try to only output history (reward, context, and action)
  #maybe some other things if general, useful, no additional cost
  #### plans/ideas
  #1. save everythign as a vectore (instead of an array), change dim at the end
  #2, separate step-wise-useful results with other things, save the mseperately
  #how to track batch and colnames??
  
  #goal: I create something I want to track, and put all info (col name, dim, maybe batch) in one place. Then I don't need to worry
  
  #### Section 1: Input checking
  ## 1.1 check contextual matrix
  if (dim(contextual_matrix)[2]!=length(index_list[[context_name]])){
    stop("Incorrect number of columns in contextual_matrix")
  }
  
  #### Section 2: Initialization
  para_post <- para_priors
  
  #Question: what's a framework for naming columns, figure out dims, and assign values in loops.
  
  #2.1 create data frames/matrices to save step-wise result later
  #Content: act_hist, AP_hist, lm_coef_hist, X, y
  act_hist <- array(NA,dim=c(n,num_act))
  
  AP_hist <- as.matrix(array(NA, dim=c(n,3)))#should be 2*(num context * num)+1 something
  #heading <- c()
  #for (b in 1:((num_act)+1)){
  #  heading[b] <- paste0('AP to arm',b)
  #}
  colnames(AP_hist) <- c('AP when c1=0','AP when c1=1','AP overall')# need to change...
  
  #column 1-> arm1 etc...
  lm_coef_hist <- array(NA,dim=c(n,length(para_priors$mu)))
  heading <- c()
  for (b in 1:length(para_priors$mu)){
    heading[b] <- paste0('Est. Coef of X',b)
  }
  colnames(lm_coef_hist) <- heading
  
  
  X <- matrix(nrow=n,ncol=length(para_priors$mu))
  heading <- c()
  for (b in 1:length(para_priors$mu)){
    heading[b] <- paste0('X',b)
  }
  colnames(X) <- heading
  
  y <- matrix(nrow=n,ncol=1)
  value_list <- list()
  
  colnames(act_hist) <- paste(action_name, index_list[[action_name]], sep="")
  colnames(contextual_matrix)<- paste(context_name, index_list[[context_name]], sep="")
  colnames(y) <- 'reward'
  
  #burinin phase
  i <- 0
  while (i<burnin){
    i <- i+1
    #notes:
    #for arm, if we have k arms, than we need k-1 action variable. (think of regression equation).
    #their sum need to be <=1
    
    #original UR
    #act_hist[i,] <- 1*(runif(num_act)>1/2)
    
    #new design, sum <=1
    act_hist[i,] <- UR_sample_actions(num_act = num_act)
    AP_hist[i,] <- 1/(num_act+1)
    
    
    #three arm_> 2 action variable, 11 resample 
    X[i,] <- 1*get_design_matrix(a=act_hist[i,],c=contextual_matrix[i,])
    mean_reward <- reward_eval(a=act_hist[i,],c=contextual_matrix[i,],mu=reward_setting$mu)
    y[i] <- reward_setting$reward_generation(mean_reward)
  }
  
  
  #should I do update here?
  if(i>0){
    para_post <- blinear_update(X[1:i,,drop = F],y[1:i],para_priors)
  }
  
  batch_ind <- 0
  batch_computation <- 1 #reduce computation cost for e.g. allocation probability etc
  batch_computation_ind <- 0
  while(i < n){
    i <- i+1
    batch_ind <- batch_ind+1
    batch_computation_ind <- batch_computation_ind+1
    #sample beta and sigma
    act_hist[i,] <- sample_arms(para_post,num_act,context=contextual_matrix[i,],epsilon=epsilon)
    
    
    X[i,] <- 1*get_design_matrix(a=act_hist[i,],c=contextual_matrix[i,])
    mean_reward <- reward_eval(a=act_hist[i,],c=contextual_matrix[i,],mu=reward_setting$mu)
    y[i] <- reward_setting$reward_generation(mean_reward)
    #
    
    if (batch_ind==batch_size){
      batch_ind <- 0
      para_post <- blinear_update(X[1:i,,drop = F],y[1:i],para_priors)
      
    }
    
    if (batch_computation_ind==batch_computation){
      batch_computation_ind <- 0
      if(reduce_computation){
        batch_computation <- batch_computation+1 #each time, increase batch size.
      }
      
      if (record_AP){
        
        
        act_temp <- as.matrix(array(dim=c(AP_b,num_act)))
        colnames(act_temp) <- colnames(act_hist)
        for (j in 1:AP_b){
          act_temp[j,] <- sample_arms(para_post,num_act,context=contextual_matrix[j,],epsilon=epsilon)
        }
        df <- as.data.frame(cbind(act_temp,contextual_matrix[1:AP_b,,drop = F]))
        
        
        AP_hist[i,] <- c(summarize(group_by(df,c1),mean=mean(a1))[,2][[1]],mean(act_temp)) #apply(act_temp,2,mean)
      }
      
      
      
      if (record_coef){
        det_X <- determinant(t(X[1:i,])%*%(X[1:i,]))
        if( det_X$modulus[1]!= -Inf){
          lm_setpwise <- summary(lm(y[1:i]~X[1:i,-1]))
          lm_coef_hist[i,] <- lm_setpwise$coefficients[,1]
        }
        
      }
    }
  }
  
  
  para_post <- blinear_update(X[1:i,,drop = F],y[1:i],para_priors)
  
  
  his <- cbind(act_hist,contextual_matrix[1:n,,drop = F],y)
  AP_hist[,(num_act+1)] <- 1-apply(AP_hist[,1:num_act,drop = F],1,sum)
  #return(list(para_post=para_post,X=X,y=y,his=his,AP_hist=AP_hist,lm_coef_hist=lm_coef_hist))
  return(list(X=X,reward=y,his=his,AP_hist=AP_hist,lm_coef_hist=lm_coef_hist))
}


process_result <- function(res,B=1000,inds,names){
  y <- res$y
  betas <- array(dim=c(B,length(res$para_post$mu)))
  his <- as_tibble(res$his)
  col <- c('all')
  reward <- mean(y)
  for(i in 1:num_context){
    col <- c(col,paste0('c',i,'=0'),paste0('c',i,'=1'))
    reward <- c(reward ,summarize(group_by_at(his,paste0('c',i)),reward=mean(reward))$reward)
  }
  reward <- data.frame(t(reward))
  colnames(reward) <- col
  mse <- summarize(group_by(his,a1),mse=sd(reward)/sqrt(n()))$mse
  for (i in 1:B){
    sigma_n <- rinvgamma(1,res$para_post$a,res$para_post$b) #sigma here is actually sigma square
    betas[i,] <- rmvnorm(1,mean=res$para_post$mu,sigma = sigma_n*inv(res$para_post$L))
  }
  
  return( list(reward=reward,
               MSE=c(mse,sd(y)/sqrt(length(y))),
               CI=apply(betas,2,quantile,probs=c(0.025,0.5,0.975))))
  
  #betas <- array(dim=c(B,length(mun)))
  #his <- as_tibble(his)
  #his_sum <- summarize(group_by(his,a1,c1),p_hat=mean(reward),sd=sd(reward),n=n())
  #Wald <- Wald_for_cont(his_sum)
  #his_sum <- summarize(group_by(his,a1),reward=mean(reward))
  #for (i in 1:B){
  #  sigma_n <- rinvgamma(1,an,bn) #sigma here is actually sigma square
  #  betas[i,] <- rmvnorm(1,mean=mun,sigma = sigma_n*inv(Ln))
  #}
  #nx <- dim(X)[2]-1
  #ind <- 2^(c(1:nx)-1)
  #xy <-as_tibble(cbind(X,y))
  #colnames(xy)[inds] <- names
  #X_group <- summarise(group_by_at(xy,inds),m=mean(y),con=sum(y)/mean(y))
  #return(list(CI=apply(betas,2,quantile,probs=c(0.025,0.5,0.975)),
  #            reward=X_group,Wald=Wald))
}





####################################################################
##################### Section 4: Simulation-level function
####################################################################

Bandit_sim <- function(B=100,cont_mat_size=10000,AP_track=F,Bayes_track=T,sampling_density=2){
  #simulate in a fixed setting
  k <- str_count(reward_model,'b') #by default the dim 'k' is the number of 'b_i' in reward model
  contextual_matrix_large <- array(dim=c(cont_mat_size,num_context))
  colnames(contextual_matrix_large)<- paste(context_name, index_list[[context_name]], sep="")
  
  magic_for(silent = T,progress = T)
  
  for (j in 1:num_context){
    contextual_matrix_large[,j] <- sample(1:length(contextual_distribution_table[,j]),
                                          size = cont_mat_size,
                                          prob = contextual_distribution_table[,j],
                                          replace=T)
    contextual_matrix_large[,j] <-(contextual_matrix_large[,j]-1 )/(sum(contextual_distribution_table[,j]>0)-1)
  }
  #AP_hist_full <- array(dim=c(n,(1+2*num_context),B))
  #Bayes.est_hist_full <- array(dim=c(n,k,B))
  #coef.est_hist_full <- array(dim=c(n,k,B))
  #p.value_hist_full <- array(dim=c(n,k,B))
  #lm_hist_full <- array(dim=c(n,4,B))
  
  reward_hist <- c()
  for (i in 1:B){
    #probar$tick()
    set.seed(i)
    contextual_matrix <- contextual_matrix_large[sample(1:cont_mat_size,size=n,replace = T),,drop=F]
    res <- Contextual_Bandit(para_priors,reward_setting,n=n,contextual_matrix=contextual_matrix,burnin=burnin,batch_size=batch_size)
    if (AP_track){
      AP_hist_full[,,i] <- get_AP_hist(prior=para_priors,contextual_matrix_large,n=n,X=res$X,y=res$reward_hist,AP_b=1000,sampling_density=sampling_density)
    }
    if (Bayes_track){
      Bayes.est_hist_full[,,i] <- get_Bayes_hist(res$X,res$reward_hist,para_priors,sampling_density=sampling_density)
    }
    #re <- get_lm_hist(res$X,res$reward_hist)
    #coef.est_hist_full[,,i] <- re$est.coef_hist
    #p.value_hist_full[,,i] <- re$p.value_hist
    #reward_hist[i] <- mean(res$reward_hist)
    put(mean(res$reward_hist))
  }
  #return(list(Bayes.est_hist=Bayes.est_hist_full,AP_hist=AP_hist_full,coef.est_hist=coef.est_hist_full,p.value_hist=p.value_hist_full,reward_hist=reward_hist))
  return(magic_result())
}

stepwise_AP <- function(para_post,contextual_matrix_large,AP_b,context_var='c2'){
  #this only works for normal Bayesian model
  #maybe use Stan in the future
  #how to make contextual matrix more standardized?
  #change input to hist! now there's too many, and repeated input
  
  cont_temp <- contextual_matrix_large[sample(1:cont_mat_size,size=AP_b,replace = T),,drop=F]
  act_temp <- as.matrix(array(dim=c(AP_b,num_act)))
  colnames(act_temp) <- paste(action_name, index_list[[action_name]], sep="")
  for (j in 1:AP_b){
    act_temp[j,] <- sample_arms(para_post,num_act,context=cont_temp[j,])
  }
  df <- as.data.frame(cbind(act_temp,cont_temp))
  
  #for i in 1:numcontext
  
  return(c(summarize(group_by_at(df,context_var),a1=mean(a1))[,2][[1]],mean(act_temp)))
  #AP_hist[i,] <- c(summarize(group_by(df,c1),mean=mean(a1))[,2][[1]],mean(act_temp)) #apply(act_temp,2,mean)
}

stepwise_lm <- function(X,y,i){
  det_X <- determinant(t(X[1:i,])%*%(X[1:i,]))
  coef <- NA
  pvalue <- NA
  if((det_X$modulus[1] > -3)&(i>1)){
    lm_setpwise <- summary(lm(y[1:i]~X[1:i,-1]))
    coef <- lm_setpwise$coefficients[,1]
    pvalue <- lm_setpwise$coefficients[,4]
  }
  return(list(coef=coef,pvalue=pvalue))
}

get_Bayes_hist <- function(X,y,para_priors,sampling_density=2){
  #sampling_density is roughly 'how often you do a sample', it's a log rate anyway (sample less and less)
  n <- dim(X)[1]
  Bayes.est_hist <- array(dim=c(n,dim(X)[2]))
  i <- 1
  imp <- 0 #importance, accumulate as time increases
  while(i<=n){
    imp <- imp+1/i
    if( imp>1/sampling_density){
      re <- blinear_update(X[1:i,,drop=F],y[1:i,drop=F],para_priors)
      Bayes.est_hist[i,] <- re$mu
      imp <- imp-1/sampling_density
    }
    i <- i+1
  }
  return(Bayes.est_hist)
}
get_lm_hist <- function(X,y,sampling_density=2){
  n <- dim(X)[1]
  est.coef_hist <- array(dim=c(n,dim(X)[2]))
  p.value_hist <- array(dim=c(n,dim(X)[2]))
  i <- 1
  imp <- 0 #importance, accumulate as time increases
  while(i<=n){
    imp <- imp+1/i
    if( imp>1/sampling_density){
      re <- stepwise_lm(X,y,i)
      est.coef_hist[i,] <- re$coef
      p.value_hist[i,] <- re$pvalue
      imp <- imp- 1/sampling_density
    }
    
    i <- i+1
    
  }
  
  return(list(est.coef_hist=est.coef_hist,p.value_hist=p.value_hist))
}
get_AP_hist <- function(priors,contextual_matrix_large,n,X,y,AP_b=3000,sampling_density=2){
  #this only works for normal Bayesian model
  #maybe use Stan in the future
  #how to make contextual matrix more standardized?
  
  #force match dim
  para_post <- blinear_update(X[1,,drop=F],y[1,drop=F],priors=priors)
  AP_hist <- array(dim=c(n,length(stepwise_AP(para_post,contextual_matrix_large,AP_b))))
  i <- 1
  imp <- 0 #importance, accumulate as time increases
  while(i<=n){
    
    imp <- imp+1/i
    if( imp>1/sampling_density){
      para_post <- blinear_update(X[1:i,,drop=F],y[1:i,drop=F],priors=priors)
      AP_hist[i,] <- stepwise_AP(para_post,contextual_matrix_large,AP_b)
      imp <- imp- 1/sampling_density
    }
    
    i <- i+1
    
  }
  
  return(AP_hist)
}



Bandit_sim_unisetting <- function(bb=10000){
  #simulate in a fixed setting
  k <- str_count(reward_model,'b') #by default the dim 'k' is the number of 'b_i' in reward model
  bayesian_test <- array(dim=c(bb,k)) #array to save results
  bayesian_est <-  array(dim=c(bb,k)) 
  rewards <- array(dim=c(bb, (1+2*num_context) ))
  ap_df <- array(dim=c(bb,n,3 ))
  probar <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                             total = bb,
                             complete = "=",   # Completion bar character
                             incomplete = "-", # Incomplete bar character
                             current = ">",    # Current bar character
                             clear = FALSE,    # If TRUE, clears the bar when finish
                             width = 100)      # Width of the progress bar
  
  for (i in 1:bb){
    probar$tick()
    set.seed(i)
    #n sample size
    #k number of betas
    
    #need to change contextual matrix each time
    res <- Contextual_Bandit(para_priors,reward_setting,n=n,contextual_matrix=contextual_matrix,epsilon=0,burnin=burnin,batch_size=batch_size,record_AP=T)
    
    
    
    process_result(res)
    res1 <- process_result(res)
    rewards[i,] <- as.matrix(res1$reward)
    bayesian_test[i,] <- res1$CI[1,]*res1$CI[3,]>0
    bayesian_est[i,] <- res1$CI[2,]
    ap_df[i,,] <- res$AP_hist
  }
  reward_output <- data.frame(apply(rewards,2,quantile,probs=c(0.025,0.5,0.975)))
  reward_sd <- apply(rewards,2,sd)
  #colnames(reward_output) <- c('a1=0','a1=1','all')
  colnames(reward_output) <- colnames(res1$reward)
  output2 <- data.frame(apply(bayesian_test,2,mean))
  output3 <- data.frame(apply(bayesian_est,2,quantile,probs=c(0.025,0.5,0.975)))
  
  return(list(rewards=reward_output,reward_sd=reward_sd,bayesian_test=output2,bayesian_est=output3,ap_df=ap_df))
}



AP_prior_sim <- function(B=10000,epsilon=0){
  para_post <- para_priors
  
  contextual_matrix <- array(dim=c(B,num_context))
  act_hist <- as.data.frame(array(NA,dim=c(B,num_act)))
  for (j in 1:num_context){
    contextual_matrix[,j] <- sample(1:length(contextual_distribution_table[,j]),
                                    size = B,
                                    prob = contextual_distribution_table[,j],
                                    replace=T)
    contextual_matrix[,j] <-(contextual_matrix[,j]-1 )/(sum(contextual_distribution_table[,j]>0)-1)
  }
  for (i in 1:B){
    sigma_n <- rinvgamma(1,para_post$a,para_post$b) #sigma here is actually sigma square
    beta_n <- rmvnorm(1,mean=para_post$mu,sigma = sigma_n*inv(para_post$L))
    if(runif(1)<epsilon){
      act_hist[i,] <- UR_sample_actions(num_act = num_act)
    } else {
      #if onehot =T, only return a single arm each time. Otherwise, a vector...
      #someone can change it
      arm_ind <- action_indicator(b=beta_n,c=contextual_matrix[i,],onehot=T)
      
      act_hist[i,] <- 0
      if (arm_ind<=num_act){
        act_hist[i,arm_ind] <- 1
      }
    }
  }
  if(dim(act_hist)[2]>1){
    return(apply(act_hist,2,mean))#allocation to arm1 (GetRationale)
  } else{
    return(mean(act_hist[,1]))#allocation to arm1 (GetRationale)
  }
  
}

