

## optim_for_iE_iR_perct_grow: Helper function for get_iE_iR_grow used to obtain incarceration 
                                ## parameters given data. Used when recidivism percentage is known.
## Input parameters:
      ## params: calibrated prison model rates (iR, IE, iN, r, k ..., covid_a, covid_r)
      ## timeunit: time unit for prison model
      ## unvarying_params: prison model rates that do not need to be calibrated (muP, muR ...)
      ## xstart: starting parameters for compartments in prison model
      ## years_ip: Years of incarceration prevalence data (recorded as years since 1990) (eg. 1,2,3, ...30)
      ## ip_known: Known incarceration prevalence data
      ## years_ad: Years of admissions data (recorded as years since 1990) (eg. 1,2,3, ...30)
      ## ad_known: Known admissions data
      ## recid_perct_known: Known recidivism percentage data
      ## recid_perct_year_known: Year corresponding to known recidivism percentage data
      ## start.incr: Time to start prison model (eg. 500)
      ## stop.incr: Time to end prison model
      ## time.to.now: Time duration of the prison model

## Output parameters:
      ## error: error from optim function

optim_for_iE_iR_perct_grow <- function(params, timeunit = seq(0,finaltime,.5),
                                       unvarying_params, 
                                       xstart = c(P=0, S=0, R=0, N=100000,E=0, Ishadow=0, 
                                        Eshadow=0),years_ip, ip_known, years_ad, ad_known,
                                       recid_perct_known,recid_perct_year_known, 
                                       start.incr, stop.incr,
                                       time.to.now=time.passed) 
  
  
{
  
  if(country == "Peru"){
    params[3] = 0.5034239 # initial release rate is fixed
    params[4] = 0 # stop growth in first interval
    params[6] = 0 # two growth intervals
  }
  
  if(country == "Argentina"){
    params[5] = 0 # halted growth
  
  }
  
  if(country == "Brazil"){
    params[5] = 0 # one growth interval
    params[6] = 0 # one growth interval

  }
  
  if(country == "Colombia"){
    #params[3] = 0.5034239 # release rate is fixed and sampled
    params[6] =  0 # two growth intervals only
    params[11] = 0 # two intervals for release rates
    params[7] = .43 # fixed change in admissions rate due to covid
    #params[8] = 1.84 # fixed change in release rate due to covid
  }
  
  all_params <- c(iR = params[1], iE=params[1]*params[2], iN=params[1]*params[2], r=params[3],
                  k=params[4],k1=params[5],k2=params[6], covid_a=params[7], covid_r=params[8], 
                  l1 = params[9], l2 = params[10], l3 = params[11], 
                  muN=unvarying_params[5], a=unvarying_params[6]) 
  

  
  output <- ode(
    func=prison.model.with.growth,
    y=xstart,
    times=timeunit,
    parms=all_params,
    intrvn.start=start.incr, 
    intrvn.end=start.incr + total_time,
    change.r.start = change.r.start.input,
    change.r.factor.1 = change.r.factor.1.input,
    change.r.factor.2 = change.r.factor.2.input,
    change.r.factor.3 = change.r.factor.3.input,
    change.r.end=change.r.end.input,
    interval_one_years = interval_one_years.input,
    interval_two_years = interval_two_years.input,
    rel_one_years = rel_one_years.input,
    rel_two_years = rel_two_years.input,
    covid.start = covid.start.input,
    covid.end = covid.end.input
  )
  

  ############### Compute error ###############
  
  output_dt <- data.table(output)

  
  offset <- 500 # start of model
  
  
  ip_obs <-vector() # observed incarceration prevalence
  
  for(i in 1:length(years_ip)){
    ip_obs<-append(ip_obs, output_dt[time == offset + years_ip[i] + .5, S + P] ) 
    # Find IP halfway through year
    
    
  }
  
  ad_obs<-vector() # observed admissions rate
  
  
  for(i in 1:length(years_ad)){
    ad_obs<-append(ad_obs, output_dt[time == offset + years_ad[i] + 1, Ishadow]
                   - output_dt[time == (offset + (years_ad[i])), Ishadow] )
    
  }
  
  
  
  recid_percent_obs <- output_dt[time == offset +recid_perct_year_known, S] / 
    (output_dt[time == offset + recid_perct_year_known, S] + 
       output_dt[time == offset + recid_perct_year_known, P]) 
  # percentage with prior incarceration 
  
  
  ip_error = ((ip_obs[2:(length(ip_obs) - 1)] - 
  as.numeric(ip_known[2:(length(ip_known) - 1)])) / as.numeric(ip_known[2:(length(ip_known) - 1)]))^2 
  # incarceration prevalence squared error excluding first and last year
  
  ip_error_start = ((ip_obs[1] - ip_known[1])/ip_known[1])^2
  
  if(country == "Colombia"){ # use spline data
    ip_error_start = ((ip_obs[1] -  spline_ip)/spline_ip)^2
    
  }
  
  
  
  ip_error_end = ((ip_obs[length(ip_obs)] - ip_known[length(ip_known)])
                  /ip_known[length(ip_known)])^2
  
  
  ad_error = ((ad_obs - as.numeric(ad_known)) / as.numeric(ad_known))^2 
  # admissions squared error
  
  
  
  
  rec_error = ((recid_percent_obs - recid_perct_known)/recid_perct_known)^2 
  
  
  if(country == "Peru"){ # don't use admissions rate for errors
    
    error = (mean(ip_error) +  rec_error + ip_error_start + ip_error_end) / 4 
    # average incarceration prevalence and recidivism error
    
    
    
    
  } else {
    
    error = (mean(ip_error) + mean(ad_error) + rec_error + ip_error_start + ip_error_end) / 5 
    # average incarceration prevalence, admissions, and recidivism error
    
    
  }
  
  
  
  if(error<smallest_error){
    cat("\n")
    print(paste0('Incarc Prev Now = ', ip_obs[length(ip_obs)]))
    print(paste0('Recidivist Percentage = ', recid_percent_obs))
    print(paste0('Final number of admissions = ', ad_obs[length(ad_obs)]))
    
    print(paste0('r=', params[3]))
    print(paste0('iR=', params[1]))
    print(paste0('iE=', params[1]*params[2]))
    print(paste0('k=', params[4]))
    print(paste0('k1=', params[5]))
    print(paste0('k2=', params[6]))
    print(paste0('covida=', params[7]))
    print(paste0('covidr=', params[8]))
    print(paste0('l1=', params[9]))
    print(paste0('l2=', params[10]))
    print(paste0('error=', error))
    
    smallest_error <<- error
  }
  
  return(error)
}






## optim_for_iE_iR_prob: Helper function for get_iE_iR_grow used to obtain incarceration 
## parameters given data. Used when recidivism probability is known.
## Input parameters:
## params: calibrated prison model rates (iR, IE, iN, r, k ..., covid_a, covid_r)
## timeunit: time unit for prison model
## unvarying_params: prison model rates that do not need to be calibrated (muP, muR ...)
## xstart: starting parameters for compartments in prison model
## years_ip: Years of incarceration prevalence data (recorded as years since 1990) (eg. 1,2,3, ...30)
## ip_known: Known incarceration prevalence data
## years_ad: Years of admissions data (recorded as years since 1990) (eg. 1,2,3, ...30)
## ad_known: Known admissions data
## recid_prob_known: Known recidivism probability data
## recid_prob_year_known: Year corresponding to known recidivism probability data
## start.incr: Time to start prison model (eg. 500)
## stop.incr: Time to end prison model
## time.to.now: Time duration of the prison model

## Output parameters:
## error: error from optim function

optim_for_iE_iR_prob <- function(params, timeunit=seq(0,finaltime,1),
                                 unvarying_params, 
                                 xstart = c(P=0, S=0, R=0, N=100000,E=0, Ishadow=0, Eshadow=0),
                                 years_ip, ip_known, years_ad, ad_known,
                                 recid_prob_known,recid_prob_year_known, 
                                 start.incr, stop.incr,
                                 time.to.now=time.passed)
{
  
  if(country == "Peru"){
    params[3] = 0.5034239 # initial release rate is fixed
    params[4] = 0 # stop growth in first interval
    params[6] = 0 # two growth intervals
  }
  
  if(country == "Argentina"){
    params[5] = 0 # halted growth
    
  }
  
  if(country == "Brazil"){
    params[5] = 0 # one growth interval
    params[6] = 0 # one growth interval
    
  }
  
  if(country == "Colombia"){
    #params[3] = 0.5034239 # release rate is fixed and sampled
    params[6] =  0 # two growth intervals only
    params[11] = 0 # two intervals for release rates
    params[7] = .43 # fixed change in admissions rate due to covid
    #params[8] = 1.84 # fixed change in release rate due to covid
  }
  
  all_params <- c(iR = params[1], iE=params[1]*params[2], iN=params[1]*params[2], r=params[3],
                  k=params[4],k1=params[5],k2=params[6], covid_a=params[7], covid_r=params[8], 
                  l1 = params[9], l2 = params[10], l3 = params[11], 
                  muN=unvarying_params[5], a=unvarying_params[6]) 
  
  
  
  output <- ode(
    func=prison.model.with.growth,
    y=xstart,
    times=timeunit,
    parms=all_params,
    intrvn.start=start.incr, 
    intrvn.end=start.incr + total_time,
    change.r.start = change.r.start.input,
    change.r.factor.1 = change.r.factor.1.input,
    change.r.factor.2 = change.r.factor.2.input,
    change.r.factor.3 = change.r.factor.3.input,
    change.r.end=change.r.end.input,
    interval_one_years = interval_one_years.input,
    interval_two_years = interval_two_years.input,
    rel_one_years = rel_one_years.input,
    rel_two_years = rel_two_years.input,
    covid.start = covid.start.input,
    covid.end = covid.end.input
  )
  
  

  output_dt <- data.table(output)
  
  
  offset <- 500 # start of model
  
  
  ip_obs <-vector() # observed incarceration prevalence
  
  for(i in 1:length(years_ip)){
    ip_obs<-append(ip_obs, output_dt[time == offset + years_ip[i] + .5, S + P] ) 
    # Find IP halfway through year
    
    
  }
  
  ad_obs<-vector() # observed admissions rate
  
  
  for(i in 1:length(years_ad)){
    ad_obs<-append(ad_obs, output_dt[time == offset + years_ad[i] + 1, Ishadow]
                   - output_dt[time == (offset + (years_ad[i])), Ishadow] )
    
  }
  
  
  
  ip_error = ((ip_obs[2:(length(ip_obs) - 1)] - 
  as.numeric(ip_known[2:(length(ip_known) - 1)])) / as.numeric(ip_known[2:(length(ip_known) - 1)]))^2 
  # incarceration prevalence squared error excluding first and last year
  
  ip_error_start = ((ip_obs[1] - ip_known[1])/ip_known[1])^2
  
  if(country == "Colombia"){ # use spline data
    ip_error_start = ((ip_obs[1] -  spline_ip)/spline_ip)^2
    
  }
  
  
  
  ip_error_end = ((ip_obs[length(ip_obs)] - ip_known[length(ip_known)])
                  /ip_known[length(ip_known)])^2
  
  
  ad_error = ((ad_obs - as.numeric(ad_known)) / as.numeric(ad_known))^2 
  # admissions squared error
  
  
  output <- ode( # recidivism sub-model
    func=sub_prison.model,
    y=c(R=100000, Y=0, E=0),
    times=timeunit,
    parms=c(iR = params[1] , iE=params[2], a=1/7) # TO DO: Update parameters
  )
  
  sub_output_dt <- data.table(output)
  recid_prob_obs <- sub_output_dt[time == recidivism_yrs_known, Y]/100000
  
  rec_error <- (abs(recid_prob_obs - recid_prob_known) / recid_prob_known)^2
  
  
  if(country == "Peru"){ # don't use admissions rate for errors
    
    error = (mean(ip_error) +  rec_error + ip_error_start + ip_error_end) / 4 
    # average incarceration prevalence and recidivism error
    
    
    
    
  } else {
    
    error = (mean(ip_error) + mean(ad_error) + rec_error + ip_error_start + ip_error_end) / 5 
    # average incarceration prevalence, admissions, and recidivism error
    
    
  }
  
  
  #################################################################
  
  
  if(error<smallest_error){
    cat("\n")
    print(paste0('Incarc Prev Now = ', ip_obs[length(ip_obs)]))
    print(paste0('Recidivist Probablity = ', recid_prob_obs))
    print(paste0('Final number of admissions = ', ad_obs[length(ad_obs)]))
    
    print(paste0('r=', params[3]))
    print(paste0('iR=', params[1]))
    print(paste0('iE=', params[1]*params[2]))
    print(paste0('k=', params[4]))
    print(paste0('k1=', params[5]))
    print(paste0('k2=', params[6]))
    print(paste0('covida=', params[7]))
    print(paste0('covidr=', params[8]))
    print(paste0('l1=', params[9]))
    print(paste0('l2=', params[10]))
    print(paste0('error=', error))
    
    smallest_error <<- error
  }
  
  return(error)
}








## get_iE_iR_grow: Function returns calibrated parameters for incarceration model and
                   ## respective error
## Input parameters:
      ## recid_perct_known: Known recidivism percentage data
      ## recid_perct_year_known: Year corresponding to known recidivism percentage data
      ## recid_prob_known: Known recidivism probability data
      ## recid_prob_year_known: Number of years recidivism probability data is calculated over
      ## years_ip: Years of incarceration prevalence data (recorded as years since 1990) (eg. 1,2,3, ...30)
      ## ip_known: Known incarceration prevalence data
      ## years_ad: Years of admissions data (recorded as years since 1990) (eg. 1,2,3, ...30)
      ## ad_known: Known admissions data
      ## start.incr: Time to start prison model (eg. 500)
      ## stop.incr: Time to end prison model
      ## time.to.now: Time duration of the prison model
      ## muP: Mortality rate for first time incarcerated
      ## muS: Mortality rate for second time incarcerated
      ## muR: Mortality rate for released
      ## muE: Mortaility rate for ex-prisoners
      ## muN: Mortality rate for never incarcerated
      ## a: Rate of transition from post-release back to ex-prisoners (1/7)
      ## param_start: Starting value for IR, ratio of IE:IR, r,  k, k1, k2, covid_a, covid_r, l1,l2,l3
      ## param_lower_bounds: Lower bounds for parameters
      ## param_upper_bounds: Upper bounds for parameters
      ## scale: Average of lower and upper bounds

## Output parameters: List of calibrated parameters
      ## iR: Incarceration rate (released)
      ## iE: Incarceration rate (ex-prisoners) 
      ## r: Release rate
      ## k, k1, k2: Incarceration growth rates for interval 1, 2, and 3
      ## covid_a: Covid-19 admissions decrease factor
      ## covid_r: Covid-19 releases increase factor
      ## l1, l2, l3: Release growth rate for interval 1, 2, 3
      ## error: Optim error


get_iE_iR_grow <- function(
    recid_perct_known, 
    recid_perct_year_known, 
    recid_prob_known, 
    recid_prob_year_known, 
    years_ip,ip_known, years_ad, ad_known,
    start.incr, stop.incr,
    time.to.now=time.passed,
    muP, 
    muS,
    muR,
    muE,
    muN,
    a= 0.1428571, 
    param_start, 
    param_lower_bounds, 
    param_upper_bounds,
    scale
){
  unvarying_params <- c(muP, muR,muE,muS, muN, a)
  param_start <- param_start
  param_lower_bounds <- param_lower_bounds
  param_upper_bounds <- param_upper_bounds
  
  if(recid_prob_known == 0 & recid_prob_year_known == 0){ # using recidivism percentage
    optim_res <- optim(param_start, optim_for_iE_iR_perct_grow, method='L-BFGS-B', 
                       lower=param_lower_bounds, upper=param_upper_bounds,
                       control=list(trace=T, maxit=200000, parscale = scale, factr = 1e-8), 
                       years_ip=years_ip, ip_known= ip_known, years_ad=years_ad, ad_known = ad_known,
                       recid_perct_known=recid_perct_known, 
                       recid_perct_year_known = recid_perct_year_known,
                       unvarying_params = unvarying_params,
                       start.incr=start.incr, stop.incr=stop.incr,
                       time.to.now=time.to.now)
  } else{
    
    optim_res <- optim(param_start, optim_for_iE_iR_prob_grow, method='L-BFGS-B', 
                       lower=param_lower_bounds, upper=param_upper_bounds,
                       control=list(trace=T, maxit=200000, parscale = scale, factr = 1e-8), 
                       years_ip=years_ip, ip_known= ip_known, years_ad=years_ad, ad_known = ad_known,
                       recid_prob_known=recid_prob_known, 
                       recid_prob_year_known = recid_prob_year_known,
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
  covid_a <- optim_res$par[7]
  covid_r <- optim_res$par[8]
  l1 <- optim_res$par[9]
  l2 <- optim_res$par[10]
  l3 <- optim_res$par[11]
  error <- optim_res$value
  
  
  
  return(list(iR=iR, iE=iE, r=r, k=k,k1=k1, k2=k2, covid_a=covid_a,
              covid_r=covid_r,l1=l1,l2=l2,l3=l3, error =error)) 
}








########################## Run Brazil ###############################
# 
# smallest_error <- Inf
# 
# country_incarc_rates <-  get_iE_iR_grow(recid_perct_known = .49, recid_prob_known = 0, recid_perct_year_known = 23.5, recid_prob_year_known = 0,
#                                         years_ip = years_ip ,ip_known = ip_known, years_ad = years_ad, 
#                                         ad_known=ad_known,start.incr = start.incr, stop.incr = start.incr + time.passed,
#                                         param_start = c(0.09865996, 0.00167854, .2, 5.700556e-06, 5.700556e-06, 5.700556e-06,1,1),
#                                         muP =0.001104304, 
#                                         muS = 0.001104304,
#                                         muR = 0.01783875, 
#                                         muE = 0.01698929,
#                                         muN = 0.01698929) # using 1/LE for muN and .065 and 1.05 for muR and muP ratios
# 


######################### Run Peru ###################################



smallest_error <- Inf


country_incarc_rates <-  get_iE_iR_grow(recid_perct_known = .2471, recid_prob_known = 0, recid_perct_year_known = 31,
                                        years_ip = years_ip ,recid_prob_year_known = 0,
                                        ip_known = ip_known, years_ad = years_ad,
                                        ad_known=ad_known,start.incr = start.incr, stop.incr = start.incr + time.passed,
                                        param_start = c(0.09865996, 0.00167854, 0.5034239, 5.700556e-06, 5.700556e-06, 5.700556e-06,.5,1.5),
                                        param_lower_bounds=c(0.0001, 0.0001, 0.2, 0.00000001, 0.00000001, 0.00000001,.1,1), #,0,-.056,0), 
                                        param_upper_bounds=c(.5,.2, 2, 0.01,.01,0.01, 1,4), #,.14,0,.14),
                                        scale =c(0.25005000, 0.10005000, 1.10000000, 0.005000005, 0.005000005, 0.005000005, .55, .55),
                                        muP = 0.01091325,
                                        muS =  0.01091325,
                                        muR = 0.0176291,
                                        muE = 0.01678962,
                                        muN = 0.01678962)





# ######################### Run Argentina ###################################
# 
# 
# smallest_error <- Inf
# 
# 
# country_incarc_rates <-  get_iE_iR_grow(recid_perct_known = .28, recid_prob_known = 0, recid_perct_year_known = 29,
#                                         years_ip = years_ip ,recid_prob_year_known = 0,
#                                         ip_known = ip_known, years_ad = years_ad, 
#                                         ad_known=ad_known,start.incr = start.incr, stop.incr = start.incr + time.passed,
#                                         param_start = c(0.09865996, 0.00167854, 0.5034239, 5.700556e-06, 5.700556e-06, 5.700556e-06,.5,.5),
#                                         muP = 0.01091325, 
#                                         muS =  0.01091325,
#                                         muR = 0.0176291, 
#                                         muE = 0.01678962,
#                                         muN = 0.01678962)
# 
# 
# 
# 

######################### Run Colombia ###################################




smallest_error <- Inf


country_incarc_rates <-  get_iE_iR_grow(recid_perct_known = .223, recid_prob_known = 0, recid_perct_year_known = 29,
                                        years_ip = years_ip ,recid_prob_year_known = 0,
                                        ip_known = ip_known, years_ad = years_ad, 
                                        ad_known=ad_known,start.incr = start.incr, stop.incr = start.incr + time.passed,
                                        param_start = c(0.05511821, 0.008507921, .5034239, 1e-06, 0, 0,.43,1.84,-0.016,0.007710299, 0.007710299),
                                        param_lower_bounds=c(0.0001, 0.0001, 0.2, 0.000001, -0.00001, -0.00001,.1,1,-.016,0,0), 
                                        param_upper_bounds=c(.5,.2, 2, .01,0,0, 1,4,0,.16,.16),
                                        scale =c(0.2499500, 0.0999500, 0.9000000, 0.0049995, 0.0000050, 0.0000050,0.4500000, 1.5,0.0080000,0.0800000,0.0800000),
                                        muP = 0.0111943, 
                                        muS = 0.0111943,
                                        muR = 0.0180831, 
                                        muE = 0.017222,
                                        muN = 0.017222)

debug(optim_for_iE_iR_perct_grow)
debug(get_iE_iR_grow)

