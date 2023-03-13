


############ Intialize values before running each country using intialize.R ##################
#############################################################################################
#############################################################################################



# optim function to get iE,iR,iN when incarc prev, number of admissions, and percent of recidivists are known
optim_for_iE_iR_perct_grow <- function(params, timeunit = seq(0,finaltime,.5),
                                       unvarying_params, 
                                       xstart = c(P=0, S=0, R=0, N=100000,E=0, Ishadow=0, Eshadow=0),years_ip,ip_obs, years_ad, ad_obs,
                                       recid_perct_known,recid_perct_year_known, 
                                       start.incr, stop.incr,
                                       time.to.now=time.passed) 
  
  
{
  
  if(country == "Peru"){
    params[3] = 0.5034239 # release rate is fixed
    params[4] = 0 # stop growth in first interval
    params[6] = 0 # two growth intervals
  }
  
  if(country == "Argentina"){
    params[5] = 0 # halted growth
  
  }
  
  if(country == "Brazil"){
    params[5] = 0 # one growth interval
    params[6] = 0 # one growth interval
    params[7] = 1 # don't reduce admissions during Covid
    params[8] = 1 # don't reduce admissions during Covid
  }
  
  all_params <- c(iR = params[1], iE=params[1]*params[2], iN=params[1]*params[2], r=params[3], k=params[4],k1=params[5],k2=params[6],
                  gf=params[7], gf2=params[8],
                  muP=unvarying_params[1], muR=unvarying_params[2], muE=unvarying_params[3], muS=unvarying_params[4],
                  muN=unvarying_params[5], a=unvarying_params[6])
  
  output <- ode(
    func=prison.model.with.growth,
    y=xstart,
    times=timeunit,
    parms=all_params,
    intrvn.start=start.incr, 
    intrvn.end=start.incr + time.passed,
    change.r.start1 = change.r.start1.input,
    change.r.factor = change.r.factor.input,
    change.r.end1=change.r.end1.input,
    cint = cint.input,
    cintstart = cintstart.input,
    interval_one_years = interval_one_years.input,
    interval_two_years = interval_two_years.input
  )
  

  
  ######
  
  output_dt <- data.table(output)
  
  
  
  
  ip_obs <-vector() # observed incarceration prevalence
  
  for(i in 1:length(years_ip)){
    ip_obs<-append(ip_obs, output_dt[time == start.incr + years_ip[i] + .5, S + P] ) # Find IP halfway through year
    
    
  }
  
  ad_obs<-vector() # observed admissions rate
  
  for(i in 1:length(years_ad)){
    ad_obs<-append(ad_obs, output_dt[time == start.incr + years_ad[i] + 1, Ishadow] - output_dt[time == (start.incr + (years_ad[i])), Ishadow] )
    
  }
  
  
  
  recid_percent_obs <- output_dt[time == start.incr +recid_perct_year_known, S] / (output_dt[time == start.incr + recid_perct_year_known, S] + output_dt[time == start.incr + recid_perct_year_known, P]) # percentage with prior incarceration in 2013)
  
  
  ip_error = ((ip_obs[2:(length(ip_obs) - 1)] - as.numeric(ip_known[2:(length(ip_known) - 1)])) / as.numeric(ip_known[2:(length(ip_known) - 1)]))^2 
  # incarceration prevalence squared error excluding first and last year
  
  ip_error_start = ((ip_obs[1] - ip_known[1])/ip_known[1])^2
  
  
  ip_error_end = ((ip_obs[length(ip_obs)] - ip_known[length(ip_known)])/ip_known[length(ip_known)])^2
  
  ad_error = ((ad_obs - as.numeric(ad_known)) / as.numeric(ad_known))^2 # admissions squared error
  
  rec_error = ((recid_percent_obs - recid_perct_known)/recid_perct_known)^2 
  
  
  if(country == "Peru"){ # don't use admissions rate for errors
    
    error = (mean(ip_error) +  rec_error + ip_error_start + ip_error_end) / 4 # average incarceration prevalence and recidivism error
   
    
  } else {
    
    error = (mean(ip_error) + mean(ad_error) + rec_error + ip_error_start + ip_error_end) / 5 # average incarceration prevalence, admissions, and recidivism error
    
    
  }
  
  
  if(error<smallest_error){
    cat("\n")
    print(paste0('Incarc Prev Now = ', ip_obs[length(ip_obs)]))
    print(paste0('Recidivist Percentage = ', recid_percent_obs))
    print(paste0('Final number of admissions = ', ad_obs[length(ad_obs)]))
    
    smallest_error <<- error
  }
  
  return(error)
}


# parscale (upper and lower bound average)
scale <-c(0.25005000, 0.10005000, 1.10000000, 0.005000005, 0.005000005, 0.005000005, .55, .55) 

get_iE_iR_grow <- function(
    recid_perct_known, # percentage of recidivists in prison population
    recid_perct_year_known, # year recidivism data is taken
    recid_prob_known, # probability of people released who are convicted of a crime within a given amount of years
    recid_prob_year_known, # amount of years that recidivism probability is calculated over
    years_ip,ip_known, years_ad, ad_known,
    start.incr, stop.incr,
    time.to.now=time.passed,
    muP, # mortality rate
    muS,
    muR,
    muE,
    muN,
    a= 0.1428571, # rate of transition from post-release back to ex-prisoners
    param_start, # starting value for IR, ratio of IE:IR, r,  k, k1, k2, cf, cf2
    param_lower_bounds=c(0.0001, 0.0001, 0.2, 0.00000001, 0.00000001, 0.00000001,.1,.1), 
    param_upper_bounds=c(.5,.2, 2, 0.01,0.01,0.01, 1,1)
){
  unvarying_params <- c(muP, muR,muE,muS, muN, a)
  param_start <- param_start
  param_lower_bounds <- param_lower_bounds
  param_upper_bounds <- param_upper_bounds
  
  if(recid_prob_known == 0 & recid_prob_year_known == 0){ # using recidivism percentage
    optim_res <- optim(param_start, optim_for_iE_iR_perct_grow, method='L-BFGS-B', lower=param_lower_bounds, upper=param_upper_bounds,
                       control=list(trace=T, maxit=200000, parscale = scale), #100000, 7
                       years_ip=years_ip,ip_obs=ip_obs, years_ad=years_ad, ad_obs=ad_obs,
                       recid_perct_known=recid_perct_known, 
                       recid_perct_year_known = recid_perct_year_known,
                       unvarying_params = unvarying_params,
                       start.incr=start.incr, stop.incr=stop.incr,
                       time.to.now=time.to.now)
  }
  
  
  
  iR <- optim_res$par[1]
  iE <- optim_res$par[1]*optim_res$par[2]
  iN <- iE
  r <- optim_res$par[3]
  k <- optim_res$par[4]
  k1 <- optim_res$par[5]
  k2 <- optim_res$par[6]
  gf <- optim_res$par[7]
  gf2 <- optim_res$par[8]
  error <- optim_res$value
  
  
  
  return(list(iR=iR, iE=iE, r=r, k=k,k1=k1, k2=k2, gf=gf,gf2=gf2, error =error))
}








########################## Run Brazil ###############################

smallest_error <- Inf

country_incarc_rates <-  get_iE_iR_grow(recid_perct_known = .49, recid_prob_known = 0, recid_perct_year_known = 23.5, recid_prob_year_known = 0,
                                        years_ip = years_ip ,ip_known = ip_known, years_ad = years_ad, 
                                        ad_known=ad_known,start.incr = start.incr, stop.incr = start.incr + time.passed,
                                        param_start = c(0.09865996, 0.00167854, .2, 5.700556e-06, 5.700556e-06, 5.700556e-06,1,1),
                                        muP =0.001104304, 
                                        muS = 0.001104304,
                                        muR = 0.01783875, 
                                        muE = 0.01698929,
                                        muN = 0.01698929) # using 1/LE for muN and .065 and 1.05 for muR and muP ratios



######################### Run Peru ###################################


smallest_error <- Inf


country_incarc_rates <-  get_iE_iR_grow(recid_perct_known = .2471, recid_prob_known = 0, recid_perct_year_known = 31,
                                        years_ip = years_ip ,recid_prob_year_known = 0,
                                        ip_known = ip_known, years_ad = years_ad, 
                                        ad_known=ad_known,start.incr = start.incr, stop.incr = start.incr + time.passed,
                                        param_start = c(0.09865996, 0.00167854, 0.5034239, 5.700556e-06, 5.700556e-06, 5.700556e-06,.5,.5),
                                        muP = 0.01091325, 
                                        muS =  0.01091325,
                                        muR = 0.0176291, 
                                        muE = 0.01678962,
                                        muN = 0.01678962)





######################### Run Argentina ###################################


smallest_error <- Inf


country_incarc_rates <-  get_iE_iR_grow(recid_perct_known = .28, recid_prob_known = 0, recid_perct_year_known = 29,
                                        years_ip = years_ip ,recid_prob_year_known = 0,
                                        ip_known = ip_known, years_ad = years_ad, 
                                        ad_known=ad_known,start.incr = start.incr, stop.incr = start.incr + time.passed,
                                        param_start = c(0.09865996, 0.00167854, 0.5034239, 5.700556e-06, 5.700556e-06, 5.700556e-06,.5,.5),
                                        muP = 0.01091325, 
                                        muS =  0.01091325,
                                        muR = 0.0176291, 
                                        muE = 0.01678962,
                                        muN = 0.01678962)








