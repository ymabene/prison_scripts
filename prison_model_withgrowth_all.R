library(deSolve)
library(ggplot2)
library(data.table)
library(SciViews)
library(readxl)


#####################################################################################################################
######################################################################################################################




## prison.model.with.growth: Function modeling incarceration trends from 1990-present

## Input parameters: 
      ## t: time
      ## x: model compartments (P, S, R, N, E, Ishadow, Eshadow)
      ## params: specified prison model rates (iR, IE, iN, r, k ..., muP ..., ... covid_a, covid_r)
      ## intrvn.start: model start time
      ## intrvn.end: model end time
      ## change.r.start: start of release rate intervention period
      ## change.r.end: end of release rate intervention period
      ## change.r.factor.1: proportion of the release rate at start of interval 1 that is left at end of interval 1
      ## change.r.factor.2: proportion of the release rate at start of interval 2 that is left at end of interval 2
      ## change.r.factor.3: proportion of the release rate at start of interval 3 that is left at end of interval 3
      ## interval_one_years: number of years of interval 1 for incarceration rate growth (eg. 11 for 1990-2001)
      ## interval_two_years: number of years of interval 2 for incarceration rate growth (eg. 10 for 2010-2020)
      ## rel_one_years: number of years in interval 1 for release rate trends (eg. 11 for 1990-2001)
      ## rel_two_years: number of years in interval 2 for release rate trends (eg. 10 for 2010-2020)
      ## covid.start = year offset to start decreasing admissions rate and increasing release rates due to Covid-19
         ## (eg. 30 for 2020 since 1990 + 30 = 2020)
      ## covid.end = year admissions/release rates returns to pre-pandemic level

## Output parameters: List with the following parameters ::
      ## dPdt, dSdt, dRdt, dNdt,dEdt (Number of people in model compartments)
      ## dIshadowdt, dEshadowdt (Total number of prison entries and exists in model)


## Details
  ## There are 3 total intervals over the time period that the model is run during which the admissions
  ## and release rate trends can be modified. 
  ## If the trends for the admissions rate is the same throughout
  ## the entire period, then do not specify "interval_two_years" and "interval_one_years" (default = 0). 
  ## If there is only one interval of incarceration growth, then only specify
  ## "interval_one_years = intrvn.end - intrvn.start". ("interval_two_years" is set to 0 by default). 
  ## If there are only 2 intervals, specify "interval_one_years" and "interval_two_years". The second
  ## interval should end at "intrvn.end".
  ## The number of years in the third interval is the remaining years after interval 2 ends until "intrvn.end".
  ##
  ## If there is no change in the release rates over the entire intervention period, then "change.r.start",
  ## "change.r.end", "rel_one_years, and "rel_two_years" do not need to be specified. 
  ## If there is only one interval of time where the release rate changes, then specify only 
  ## "change.r.factor1" and "rel_one_years= change.r.end - intrvn.start". 
  ## ("change.r.factor.2" and "change.r.factor3" will default to NA. "rel_two_years" will default to 0.)
  ## If there are only two intervals, specify "change.r.factor1", "change.r.factor2", "rel_one_years", and
  ## "rel_two_years." The second interval should end at "change.r.end." 
  ## The number of years in the third interval is the remaining years after interval 2 ends until "change.r.end".
  ##
  ## If there are no changes due to covid, do not specify either "covid.start" or "covid.end" (default to 0).





prison.model.with.growth <- function(t, x, params, intrvn.start=Inf, intrvn.end=Inf, change.r.start=Inf, change.r.end=Inf, 
                                     change.r.factor.1 = NA, change.r.factor.2 = NA, change.r.factor.3 = NA,
                                     interval_one_years = 0, interval_two_years=0, rel_one_years=0, rel_two_years = 0,
                                     covid.start=0,covid.end=0)
  

{
  with(as.list(c(x, params)),{
    
    offset <- 500 # start of model
    ########### Incarceration Rates ##############
    
    if (t >= intrvn.start){ # first interval of growth
      if(t < intrvn.start + interval_one_years){
        iR <- iR+k*(t-intrvn.start)
        iE <- iE+k*(t-intrvn.start)
        iN <- iN+k*(t-intrvn.start)

      } else{
        
        iR <- iR + k*(interval_one_years)
        iE <- iE + k*(interval_one_years)
        iN <- iN + k*(interval_one_years)
      
      }
     
    }
    
    if (t >= intrvn.start + interval_one_years){ # second interval of growth
      if(t <intrvn.start + interval_one_years + interval_two_years){
        iR <- iR+k1*(t-(intrvn.start + interval_one_years)) 
        iE <- iE+k1*(t-(intrvn.start + interval_one_years))
        iN <- iN+k1*(t-(intrvn.start + interval_one_years)) 
       
        
      } else{
        
        iR <- iR + k1*(interval_two_years)
        iE <- iE + k1*(interval_two_years)
        iN <- iN + k1*(interval_two_years)
        
        
        
        
      }
    
     
    }
    
    
    if (t >= intrvn.start + interval_one_years + interval_two_years){ # third interval of growth
      if(t < intrvn.end){
        
      }
      iR <- iR+ k2*(t-(intrvn.start + interval_one_years + interval_two_years)) 
      iE <- iE+ k2*(t-(intrvn.start + interval_one_years + interval_two_years)) 
      iN <- iN+ k2*(t-(intrvn.start + interval_one_years+ interval_two_years)) 
      
    } else{
      
      iR <- iR+ k2*((intrvn.end - interval_two_years - interval_one_years - intrvn.start ))
      iE <- iE + k2*((intrvn.end - interval_two_years - interval_one_years - intrvn.start ))
      iN <- iN + k2*((intrvn.end - interval_two_years - interval_one_years - intrvn.start ))
      
    }
    
    ################# Release Rates #####################
    
    if(country != "Peru") { # half the release rate over 1990-2022
      
      # Notes: r changes at a rate of l1. r can change in two ways:
      # 1. user specifies the factor by which r changes (ie 0.5 if you want r to halve over the period); l1 is automatically calculated
      # 2. user specifies the proportion of prison growth resulting from changes in r (vs changes in admissions); l1 is calibrated during optimization
      
      if (t >= change.r.start){
        if (t < intrvn.end){
          if (!is.na(change.r.factor.1)){ # l1 is only calculated if r is to change via method 1 above, otherwise, l1 is fed-in during optimization
            l1 <- ((r * change.r.factor.1) - r) / (rel_one_years) # calculate the annual rate of change needed to get to the ultimate r
            # print(r)
            # print(rel_one_years)
            # print(change.r.factor.1)
          
           
          }  
          
          if (!is.na(change.r.factor.2)){ # l2 is only calculated if r is to change via method 1 above, otherwise, l2 is fed-in during optimization
            if(rel_two_years == 0){
              l2 <- l1 # same rate
              
            } else{
              
              l2 <- ((r * change.r.factor.2) - r) / (rel_two_years) 
         
             
              
            }
            
            
          }  
          
          if (!is.na(change.r.factor.3)){ # l3 is only calculated if r is to change via method 1 above, otherwise, l3 is fed-in during optimization
            # if there is only 1 or 2 intervals for release rate trends, set l3 = l2
            if(rel_two_years == 0 || rel_two_years + rel_one_years+ change.r.start == change.r.end){
              l3<-l2
             
             
            } else{
              
              l3 <- ((r * change.r.factor.3) - r) / (change.r.end-(rel_two_years + rel_one_years+ change.r.start) ) 
              
            }
            
            
          }
          
        }
          
          
          if(t >= change.r.start){ # decrease/increase/maintain release rate
            if(t < (change.r.start + rel_one_years)){
              r <- r+l1*(t-change.r.start)
             # print(r)
              
            } else{
              
              r <- r + l1*(rel_one_years)
              #print(r)
            }
              
             
            
          } 
          
          if(t >= (change.r.start + rel_one_years)){ # # decrease/increase/maintain release rate
            if(t < (change.r.start + rel_one_years+ rel_two_years)){
              r <- r + l2*(t - (change.r.start + rel_one_years)) 
              
            } else{
              
              r<- r + l2*(rel_two_years)
              #print(r)
              
            }
            
            
          } 
          
          if(t >= (change.r.start + rel_one_years + rel_two_years)){ # decrease/increase/maintain release rate
            if(t < (change.r.end)){
              
              r <- r + l3*(t- (change.r.start + rel_one_years + rel_two_years)) 
              #print("l3")
              #print(l3)
              #print((change.r.start + rel_one_years + rel_two_years))
              
            } else{
              
              r <- r + l3*(change.r.end- (change.r.start + rel_one_years + rel_two_years))
              #print("l3")
              #print(l3)
              #print(change.r.end- (change.r.start + rel_one_years + rel_two_years))
              
            }
          }
          
          
        
        
      }
      
      #print(t)
      #print(r)
      #print(l1)
    } else { # use actual release rate data for Peru only
      
      if ((t >= intrvn.start) & (t < intrvn.end) ){ # 1990-2024
        r <- intercept + (slope *(t - intrvn.start)) # incorporate predicted release rates
        
        
      }
      
    }
    
    ############### Covid-19 changes #####################
    
    #print(r)
    
    if (t >= offset + covid.start){
      
      if((t >= (offset + covid.start)) & (t < (offset + covid.end))){ # Covid-19 intervention period
        iR <- iR*covid_a
        iE <- iE*covid_a
        iN <- iE*covid_a
        r <- r*covid_r
        
        #print(t)
        #print(r)
  
      }
      
  
    }
    
    
    
    
    
    
    
    
    ##########################################################################################################
    #          MODEL EQUATIONS
    ##########################################################################################################
    
    # incarcerated first time
    dPdt <- iN*N - r*P - muP*P
    
    # incarcerated repeated (subsequent)
    dSdt<- iR*R + iE*E  - r*S - muS*S
    
    # released
    dRdt <- -iR*R + r*(P + S) - muR*R - a*R   # the release rate is the same for first and subsequent incarcerations
    
    # never incarcerated
    dNdt <- muP*P + muS*S + muR*R + muN*N + muE*E - iN*N  - muN*N     
    
    # ex prisoners
    dEdt <- a*R -iE*E -muE*E
    
    # to keep track of total admissions 
    dIshadowdt <- iN*N + iR*R + iE*E                            
    
    # to keep track of total exits
    dEshadowdt <- r*(P+S)
    
    
    list(c(dPdt, dSdt, dRdt, dNdt, dEdt, dIshadowdt, dEshadowdt))
  })
}
