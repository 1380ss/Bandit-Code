source("Contextual configration.R")
#note July 20th
# reward calculation
# sampling density: should include e.g. 560, 785 etc
#progress bar
#arm numbering. from 0? UR_sample XXX

#note July 19th
#maybe globale_AP_b or something. A list of global things
#or how to create a df whenever needed? auto figure out dim?
#change sample arm to vector com
#note July 18th
#1. estimation of intercept, main effect etc over time
#reward
#3. allocation probability
#change inverse

#note to myself June 27th
#what are some barriers?
#1. Sim/Simulation similar files. what I can copy from sim?
#2. stick to local
#3. how to split the code. One file only for functions, the other only for settings?
#4. there's some fixed settings. How to deal with it?
#5. what else 

# commit message:
#1. need to rename file, revise, and edit AP_prior_sim and Bandit_sim function



res <- Bandit_sim(B=100,AP_track = F, Bayes_track = F,sampling_density=2)



#res_var5 <- res
res <- res_var5$AP_hist
i <- 5
df <- apply(res[,i,],1,quantile,probs=c(0.025,0.5,0.975),na.rm=T)
ind <- !is.na(df[1,])
df <- df[,ind]
plot((1:n)[ind],df[2,],type='l',
     main='Average allocation probability (across all) \n over time with 95% CI',
     xlab='Time Step',
     ylab='Allocation Prob',
     ylim=c(0,1))
lines((1:n)[ind],df[1,],type='l',col='red')
lines((1:n)[ind],df[3,],type='l',col='red')









instance=1
i <- 2
x <- !is.na(res$Bayes.est_hist[,1,1])
plot(c(1:n)[x],res$Bayes.est_hist[x,i,instance],type='l',
     ylim=c(0,0.5),
     xlab = 'time step',
     ylab='estimation of main effect (Bayes)',
     main = paste0('Bayes Estimation of main effect over time, \nwith true main effect =',mu[2], ', variance =',1/var_diag))
abline(h=0.2,col='red')
