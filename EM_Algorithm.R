rm(list = ls())
library(readxl)
library(stringr)

gamma_mle_fn = function(p, data_stat)
{
  data_stat[data_stat==0] = 0.01
  
  if(p[1]>0 && p[2]>0)
  {
    
    like = sum(dgamma(x = data_stat ,shape = p[1],scale = p[2],log = T))
    
  }else{
    like = -10000000
  }
  
  return(-like)
}


gamma_likelihood_censoring = function(p, runs, notout)
{
  
  out_runs = runs[notout==0]
  out_runs[out_runs==0] = 0.01
  notout_runs = runs[notout==1]
  notout_runs[notout_runs==0] = 0.01
  
  if(p[1]>0 && p[2]>0)
  {
    likelihood = sum(dgamma(x = out_runs,shape = p[1],scale = p[2],log = T),na.rm = T)+
      
                 sum(log(1-pgamma(q = notout_runs,shape = p[1],scale = p[2])),na.rm = T)
  }else
  {
    likelihood = -100000000
  }
  
  
  return(-likelihood)
  
}


gamma_e_fun = function(yi,alpha_i,beta_i, upper_bound = Inf, upper_bound_adjustment=100)
{
  #e_ti = ((1-pgamma(q = yi, shape = alpha_i+1,scale = beta_i))*beta_i)/(1-pgamma(yi, shape = alpha_i, scale = beta_i))
  

  
  gamma_pdf = function(x, a, b) {
    dgamma(x, shape = alpha_i, scale = beta_i)
  }
  
  integrand = function(x) {
          x * gamma_pdf(x)
  }
  
  numerator = c()
  for(i1 in 1:length(yi) ){
    if(yi[i1]<1){upper_bound=upper_bound_adjustment}
    numerator[i1] = integrate(integrand, lower = yi[i1], upper = upper_bound)$value
    }
  
  
  denom = 1 - pgamma(yi, shape = alpha_i, scale = beta_i)
  
  e_ti = numerator/denom
  
  return(e_ti)
}

gamma_e_log_fun = function(yi,alpha_i,beta_i, upper_bound=Inf, upper_bound_adjustment=200)
{
  gamma_pdf = function(x, a, b) {
    dgamma(x, shape = a, scale = b)
  }
  
  e_log_x_fn = function(x, a, b){
    log(x) * gamma_pdf(x = x, a = a, b = b)
  }
  
  numerator = c()
  

  
  for(i1 in 1:length(yi) )
  {
    if(yi[i1]<1){upper_bound=upper_bound_adjustment}
    numerator[i1] = integrate(e_log_x_fn, lower = yi[i1], 
                                                   upper = upper_bound, a = alpha_i, b = beta_i)$value
  }
  
  denom = 1-pgamma(yi, shape = alpha_i, scale = beta_i)
  
  e_log_ti = numerator/denom
  
  return(e_log_ti)
  
}



gamma_e_step = function(notout_x, est_p, upper_bound_adjustment=150)
{
    alpha_i = est_p[1]
    beta_i  = est_p[2]
    
    notout_x[notout_x==0] = 0.01
    
    e_x = gamma_e_fun(yi = notout_x,alpha_i = alpha_i, beta_i = beta_i,upper_bound_adjustment = upper_bound_adjustment)
    e_log_x = gamma_e_log_fun(yi = notout_x,alpha_i = alpha_i, beta_i = beta_i,upper_bound_adjustment = upper_bound_adjustment)

    return(data.frame(e_x,e_log_x))
    
}



gamma_m_step = function(p, x, notout,alpha_i,beta_i, est_x, est_log_x)
{

  out_x = x[notout==0]
  out_x[out_x==0] = 0.01
  out_x = out_x[!is.na(out_x)]
  
  notout_x = x[notout==1]
  notout_x[notout_x==0] = 0.01
  
  if(p[1]>0 && p[2]>0)
  {

    Q = -length(notout_x)*log(gamma(p[1])) - 
       length(notout_x)*p[1]*log(p[2]) + 
       (p[1]-1)*sum(est_log_x,na.rm = T) - 
        sum(est_x,na.rm = T)/p[2]
    
    likelihood = sum(dgamma(x = out_x,shape = p[1],scale = p[2],log = T),na.rm = T) + Q
    
  }else
  {
    likelihood = -100000000
  }
  
  
  return(-likelihood)
  
  
}

cricket_em = function(path = "Dataset.xlsx", em_sims = 10, print_em=F)
{

  batter_names = excel_sheets(path)
  
  summary_df = data.frame()
  
  for(j1 in 1:length(batter_names))
  {

    bat_df = read_xlsx(path = path ,sheet = batter_names[j1])
    
    ## Process ###
    bat_df$Runs = as.numeric(str_replace(bat_df$Runs,"\\*",replacement = ""))
    bat_df$BF   = as.numeric(str_replace(bat_df$BF,"\\*",replacement = ""))
    
    bat_df$notout = 0
    bat_df$notout[bat_df$Dismissal =="not out"] = 1
    
    n_matches = length(bat_df$Runs) - sum(is.na(bat_df$Runs))
    n_notouts = sum(bat_df$notout)
    cricket_avg = sum(bat_df$Runs,na.rm = T)/(n_matches-n_notouts)
    
    not_bat = is.na(bat_df$Runs)
    runs   = bat_df$Runs[!not_bat]
    notout = bat_df$notout[!not_bat]
    out_runs = runs[notout==0]
    out_runs[out_runs==0] = 0.01
    
    notout_runs = runs[notout==1]
    notout_runs[notout_runs==0] = 0.01
    
    balls_faced = bat_df$BF[!not_bat]
    balls_faced[balls_faced==0] = 0.01
    
    beta_init = (var(out_runs,na.rm = T)/mean(out_runs,na.rm = T))
    alpha_init = mean(out_runs,na.rm = T)/beta_init
    
    ## Runs ##
    
    gamma_mle = nlm(f = gamma_likelihood_censoring,p = c(alpha_init,beta_init), runs = runs, notout = notout)$estimate
    gamma_censor_avg = gamma_mle[1]*gamma_mle[2]
    
    alpha_censor = gamma_mle[1]
    beta_censor = gamma_mle[2]
    
    runs_em_est0 = c(0.1, 60)
    
    for(i1 in 1:em_sims)
    {
      
      exp_gamma = gamma_e_step(notout_x = notout_runs,est_p = runs_em_est0,upper_bound_adjustment = 500)
      est_runs = exp_gamma$e_x
      est_log_runs = exp_gamma$e_log_x
      
      runs_em_est = nlm(f = gamma_m_step, p = c(runs_em_est0[1], runs_em_est0[2]), 
                        x=runs, notout=notout, alpha_i=runs_em_est0[1], beta_i=runs_em_est0[2], 
                        est_x=est_runs, est_log_x = est_log_runs)$estimate
      
      runs_em_est0 = runs_em_est
      
      #print(runs_em_est0)
      
    }
    
    runs_em_avg = runs_em_est[1]*runs_em_est[2]
    
    ## Balls Faced ##
    beta_init = (var(balls_faced,na.rm = T)/mean(balls_faced,na.rm = T))
    alpha_init = mean(balls_faced,na.rm = T)/beta_init
    
    gamma_bf_mle = nlm(f = gamma_likelihood_censoring, p = c(alpha_init,beta_init), runs = bat_df$BF, notout=bat_df$notout)$estimate
    gamma_bf_censor_avg = gamma_mle[1]*gamma_mle[2]
    
    alpha_bf_censor = gamma_bf_mle[1]
    beta_bf_censor = gamma_bf_mle[2]
    
    balls_faced = bat_df$BF[!not_bat]
    not_out_balls_faced = balls_faced[notout==1]
    
    bf_em_est0 = c(alpha_init, beta_init)
    
    for(i1 in 1:em_sims)
    {
      exp_gamma = gamma_e_step(notout_x = not_out_balls_faced,est_p = bf_em_est0)
      est_bf = exp_gamma$e_x
      est_log_bf = exp_gamma$e_log_x
      
      bf_em_est = nlm(f = gamma_m_step, p = c(bf_em_est0[1], bf_em_est0[2]), 
                        x=balls_faced, notout=notout, alpha_i=bf_em_est0[1], beta_i=bf_em_est0[2], 
                        est_x=est_bf, est_log_x = est_log_bf)$estimate
      
      bf_em_est0 = bf_em_est
      
      #print(bf_em_est0)
    }
    
    ## Estimation ##
    runs_updated = bat_df$Runs[!not_bat]
    runs_updated[notout==1] = est_runs
    
    bf_updated = bat_df$BF[!not_bat]
    bf_updated[notout==1] = est_bf
    
    strike_rate = mean(bat_df$Runs/bat_df$BF, na.rm = T)*100
    em_strike_rate = mean(runs_updated/bf_updated, na.rm = T)*100
    
    summary_df = rbind(summary_df,
                       data.frame(batter_names[j1], matches_batted = length(runs), no_not_outs = sum(notout),
                                  runs_scored = sum(runs, na.rm =T), balls_faced = sum(balls_faced, na.rm = T),
                                  runs_censor_alpha = alpha_censor, runs_censor_beta = beta_censor, 
                                  bf_censor_alpha = alpha_bf_censor, bf_censor_beta = beta_bf_censor,
                                  runs_em_alpha = runs_em_est[1], runs_em_beta = runs_em_est[2], 
                                  bf_em_alpha = bf_em_est[1], bf_em_beta = bf_em_est[2],
                                  cricket_avg, gamma_censor_avg, runs_em_avg, 
                                  strike_rate, em_strike_rate))
    
    
  }
  
  return(summary_df)
  
}

cricket_em()

cricket_em_seq = function(path = "Dataset.xlsx", em_sims = 10, print_em=F, start_inn = 10)
{
  
  batter_names = excel_sheets(path)
  
  summary_df = data.frame()
  
  all_results = list()
  em_exp_values = data.frame()
  
  for(j1 in 1:length(batter_names))
  {
    
    bat_df = read_xlsx(path = path ,sheet = batter_names[j1])
    
    print(batter_names[j1])
    
    ## Process ###
    bat_df$Runs = as.numeric(str_replace(bat_df$Runs,"\\*",replacement = ""))
    bat_df$BF   = as.numeric(str_replace(bat_df$BF,"\\*",replacement = ""))
    
    bat_df$notout = 0
    bat_df$notout[bat_df$Dismissal =="not out"] = 1
    
    n_matches = length(bat_df$Runs) - sum(is.na(bat_df$Runs))
    n_notouts = sum(bat_df$notout)
    cricket_avg = sum(bat_df$Runs,na.rm = T)/(n_matches-n_notouts)
    
    not_bat = is.na(bat_df$Runs)
    runs   = bat_df$Runs[!not_bat]
    notout = bat_df$notout[!not_bat]
    out_runs = runs[notout==0]
    out_runs[out_runs==0] = 0.01
    
    notout_runs = runs[notout==1]
    notout_runs[notout_runs==0] = 0.01
    
    balls_faced = bat_df$BF[!not_bat]
    balls_faced[balls_faced==0] = 0.01
    
    beta_init = (var(out_runs,na.rm = T)/mean(out_runs,na.rm = T))
    alpha_init = mean(out_runs,na.rm = T)/beta_init
    
    ## Runs Gamma Censor##
    
    gamma_mle = nlm(f = gamma_likelihood_censoring,p = c(alpha_init,beta_init), runs = runs, notout = notout)$estimate
    gamma_censor_avg = gamma_mle[1]*gamma_mle[2]
    
    alpha_censor = gamma_mle[1]
    beta_censor = gamma_mle[2]
    
    beta_init = (var(balls_faced,na.rm = T)/mean(balls_faced,na.rm = T))
    alpha_init = mean(balls_faced,na.rm = T)/beta_init
      
    gamma_bf_mle = nlm(f = gamma_likelihood_censoring, p = c(alpha_init,beta_init), runs = balls_faced, notout=notout)$estimate
    gamma_bf_censor_avg = gamma_bf_mle[1]*gamma_bf_mle[2]
      
    alpha_bf_censor = gamma_bf_mle[1]
    beta_bf_censor = gamma_bf_mle[2]
    
    ## EM Algorithm ##
    runs_updated = bat_df$Runs[!not_bat]
    log_runs_updated = log(bat_df$Runs[!not_bat])
    
    balls_faced_updated = bat_df$BF[!not_bat]
    log_balls_faced_updated = log(bat_df$BF[!not_bat])
    
    not_out_positions = which(notout==1)
    not_out_positions[not_out_positions<start_inn] = start_inn
    not_out_positions = unique(not_out_positions)
    if(not_out_positions[length(not_out_positions)] != length(runs)){not_out_positions[length(not_out_positions)] = length(runs)}
    
    notout_sim_full = notout
    
    est_runs_sim = c()
    est_log_runs_sim = c()
    
    est_bf_sim = c()
    est_log_bf_sim = c()
    
    for(k1 in not_out_positions)
    {
      beta_init = (var(runs[1:k1],na.rm = T)/mean(runs[1:k1],na.rm = T))
      alpha_init = mean(runs[1:k1],na.rm = T)/beta_init
      
      runs_em_est0 = c(alpha_init, beta_init)
      
      runs_sim = runs[1:k1]
      notout_sim = notout_sim_full[1:k1]
    
      for(i1 in 1:em_sims)
      {
        
        exp_gamma = gamma_e_step(notout_x = runs_sim[notout_sim==1],est_p = runs_em_est0,upper_bound_adjustment = 500)
        est_runs = exp_gamma$e_x
        est_log_runs = exp_gamma$e_log_x
      
        runs_em_est = nlm(f = gamma_m_step, p = c(runs_em_est0[1], runs_em_est0[2]), 
                          x=runs_sim, notout=notout[1:k1], alpha_i=runs_em_est0[1], beta_i=runs_em_est0[2], 
                          est_x=est_runs_sim, est_log_x = est_log_runs_sim)$estimate
        
        runs_em_est0 = runs_em_est
        
        #print(runs_em_est)
        #if(print_em){print(runs_em_est)}
        
      }
      
      est_runs_sim = c(est_runs_sim, est_runs)
      est_log_runs_sim = c(est_log_runs_sim, est_log_runs)
      
      runs_updated[which(notout_sim==1)] = est_runs
      log_runs_updated[which(notout_sim==1)] = est_log_runs
      
      ## Balls Faced ##
      
      bf_sim = balls_faced[1:k1]
      beta_init = (var(balls_faced[1:k1],na.rm = T)/mean(balls_faced[1:k1],na.rm = T))
      alpha_init = mean(balls_faced[1:k1],na.rm = T)/beta_init
      
      bf_em_est0 = c(alpha_init, beta_init)
      
      balls_faced_sim = balls_faced[1:k1]
      
      for(i1 in 1:em_sims)
      {
        exp_gamma = gamma_e_step(notout_x = bf_sim[notout_sim==1],est_p = bf_em_est0)
        est_bf = exp_gamma$e_x
        est_log_bf = exp_gamma$e_log_x
      
        bf_em_est = nlm(f = gamma_m_step, p = c(runs_em_est0[1], runs_em_est0[2]), 
                          x=bf_sim, notout=notout[1:k1], alpha_i=runs_em_est0[1], beta_i=runs_em_est0[2], 
                          est_x=est_bf_sim, est_log_x = est_log_bf_sim)$estimate
      }
      
      
      est_bf_sim = c(est_bf_sim, est_bf)
      est_log_bf_sim = c(est_log_bf_sim, est_log_bf)
      
      balls_faced_updated[which(notout_sim==1)] = est_bf
      log_balls_faced_updated[which(notout_sim==1)] = est_log_bf
      
      notout_sim_full[1:k1] = 0
      
    }
    
    runs_updated_cop = runs_updated
    runs_updated_cop[runs_updated_cop==0] = 0.01
    balls_faced_updated_cop = balls_faced_updated
    balls_faced_updated_cop[balls_faced_updated_cop==0] = 0.01
    
    strike_rate = sum(bat_df$Runs,na.rm = T)/sum(bat_df$BF,na.rm=T)*100
    
    runs_para_em_avg = runs_em_est[1]*runs_em_est[2]
    runs_em_avg = mean(runs_updated)
    em_strike_rate = sum(runs_updated)/sum(balls_faced_updated)*100
    
    summary_df = rbind(summary_df,
                       data.frame(batter_names[j1], matches_batted = length(runs), no_not_outs = sum(notout),
                                  runs_scored = sum(runs, na.rm =T), balls_faced = sum(balls_faced, na.rm = T),
                                  runs_censor_alpha = alpha_censor, runs_censor_beta = beta_censor, 
                                  bf_censor_alpha = alpha_bf_censor, bf_censor_beta = beta_bf_censor, 
                                  runs_em_alpha = runs_em_est[1], runs_em_beta = runs_em_est[2], 
                                  bf_alpha = bf_em_est[1], bf_beta = bf_em_est[2], 
                                  cricket_avg, gamma_censor_avg, runs_em_avg, runs_para_em_avg, 
                                  strike_rate, em_strike_rate)
                       )
    
    
    df_sim = data.frame(batter_names[j1], runs, balls_faced, notout, runs_updated, balls_faced_updated)
    all_results[[j1]] = df_sim
    
    em_exp_values = rbind(em_exp_values,data.frame(batter_names[j1], notout, runs, runs_updated, 
                               balls_faced, balls_faced_updated,log_runs_updated, log_balls_faced_updated))
    
  }
  
  
  return(list(summary_df, all_results,em_exp_values))
  
}

results = cricket_em_seq()
results[1]
