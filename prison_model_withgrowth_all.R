library(deSolve)
library(ggplot2)
library(data.table)
library(SciViews)
library(readxl)



##### NOTE: Initialize values for selected country before running using intialize.R. Parameter definitions are in initialize.R


#####################################################################################################################
######################################################################################################################


# constants of regression model for fitting Peru (check initialize.R for regression model)
intercept = 0.503423912234061
slope = -0.00999950168014751

prison.model.with.growth <- function(t, x, params, intrvn.start=Inf, intrvn.end=Inf, change.r.start1=Inf, change.r.end1=Inf, 
                                     change.r.factor = NA, interval_one_years = NA, interval_two_years=NA, cint = NA, cintstart = NA)
  

{
  with(as.list(c(x, params)),{
    
    
    if ((t >= intrvn.start) & (t < intrvn.start + interval_one_years) ){ # first interval of growth
      iR <- iR+k*(t-intrvn.start)
      iE <- iE+k*(t-intrvn.start)
      iN <- iN+k*(t-intrvn.start)
      
    }
    
    if ((t >= intrvn.start + interval_one_years) & (t <intrvn.start + interval_two_years)){ # second interval of growth
      iR <- iR+k1*(t-(intrvn.start + interval_one_years))+ k*(interval_one_years)
      iE <- iE+k1*(t-(intrvn.start + interval_one_years))+ k*(interval_one_years)
      iN <- iN+k1*(t-(intrvn.start + interval_one_years))+ k*(interval_one_years)
     
    }
    
    
    if ((t >= intrvn.start + interval_two_years) & (t <intrvn.end)){ # third interval of growth
      iR <- iR+ k2*(t-(intrvn.start + interval_two_years))+ k1*(interval_two_years - interval_one_years) + k*(interval_one_years)
      iE <- iE+ k2*(t-(intrvn.start + interval_two_years)) + k1*(interval_two_years - interval_one_years) + k*(interval_one_years)
      iN <- iN+ k2*(t-(intrvn.start + interval_two_years))+ k1*(interval_two_years - interval_one_years) + k*(interval_one_years)
      
    }
    
    
    if (t >= intrvn.end){
      
      
      if((t >= intrvn.end) & (t < intrvn.start +30)){ # 2019-2020
        iR <- (iR+ k2*(intrvn.end-(intrvn.start + interval_two_years))+ k1*(interval_two_years - interval_one_years) + k*(interval_one_years))*gf
        iE <- (iE+ k2*(intrvn.end-(intrvn.start + interval_two_years)) + k1*(interval_two_years - interval_one_years) + k*(interval_one_years))*gf
        iN <- (iN+ k2*(intrvn.end-(intrvn.start + interval_two_years))+ k1*(interval_two_years - interval_one_years) + k*(interval_one_years))*gf
      }
      
      else { #2021-2022
        
        iR <- (iR+ k2*(intrvn.end-(intrvn.start + interval_two_years))+ k1*(interval_two_years - interval_one_years) + k*(interval_one_years))*gf2
        iE <- (iE+ k2*(intrvn.end-(intrvn.start + interval_two_years)) + k1*(interval_two_years - interval_one_years) + k*(interval_one_years))*gf2
        iN <- (iN+ k2*(intrvn.end-(intrvn.start + interval_two_years))+ k1*(interval_two_years - interval_one_years) + k*(interval_one_years))*gf2
        
      }
      
    }
    
    if(country != "Peru") { # half the release rate over 1990-2022
      
      # Notes: r changes at a rate of l1. r can change in two ways:
      # 1. user specifies the factor by which r changes (ie 0.5 if you want r to halve over the period); l1 is automatically calculated
      # 2. user specifies the proportion of prison growth resulting from changes in r (vs changes in admissions); l1 is calibrated during optimization
      
      if (t > change.r.start1){
        if (t <= change.r.end1){
          if (!is.na(change.r.factor)){ # l1 is only calculated if r is to change via method 1 above, otherwise, l1 is fed-in during optimization
            l1 <- ((r * change.r.factor) - r) / (change.r.end1-change.r.start1 - cint) # calculate the annual rate of change needed to get to the ultimate r
            if(t <= change.r.start1 + cintstart){ # decrease release rate
              r <- r+l1*(t-change.r.start1)
            }
            if(t > change.r.start1 + cintstart & t <= change.r.start1 + cintstart + cint){ # stop decreasing release rate
              r <- r+l1*(cintstart)
            }
            
            if(t > change.r.start1 + cintstart + cint & t <= change.r.end1){ # decrease release rate
              r <- r+l1*(t-change.r.start1 - cint)
            
            }
            
          }
          
          
        } else {
          r <- ifelse(!is.na(change.r.factor), r*change.r.factor, r+l1*(change.r.end1-change.r.start1 -cint))
        }
      }
      
    } else { # use actual release rate data for Peru only
      
      if ((t >= intrvn.start) & (t <= intrvn.start + total_time) ){ # 1990-2022
        r <- intercept + (slope *(t - intrvn.start)) # incorporate predicted release rates
        
        
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



################################### Optimal Parameters generated by optim ############################

######################################### Run Peru ###################################################


# error: .000381452
all_params <- c(iR = 0.07434591, iE= 0.0004730968, iN= 0.0004730968, r =  0.5034239, k =  1e-08, k1 = 5.427281e-05, k2 = 0, gf=0.4999813, 
                gf2=0.4999752,
                muP= 0.01091325, muR= 0.0176291, muE=0.01678962, muS= 0.01091325,
                muN=0.01678962, a=0.142857) # using 1/LE for general mortality and .65,1.05 for ratios 

########################################  Run Brazil #################################################

# error: .002479
all_params <- c(iR = 0.1882389, iE=0.0006993193, iN=0.0006993193, r= 1.615509, k =  5.186631e-05,k1 = 0, k2 = 0, gf=1, gf2 = 1,
                muP =0.001104304, 
                muS = 0.001104304,
                muR = 0.01783875, 
                muE = 0.01698929,
                muN = 0.01698929, a=0.142857) # using 1/LE for general mortality and .65,1.05 for ratios

######################################### Run Argentina  #################################################

# error: .003

all_params <- c(iR = 0.08068844, iE=  0.0009807887, iN=  0.0009807887, r =  1.855909, k =  5.788708e-05, k1 =  0, k2 = 0.0003932904,
                gf=0.5086949, gf2=0.7201509,
                muP = 0.001058423, 
                muS =  0.001058423,
                muR = 0.0170976, 
                muE = 0.01628343,
                muN = 0.01628343, a=0.142857) 



############################################################################################################
###########################################################################################################


xstart <- c(P=0, S=0, R=0, N=100000, E=0, Ishadow=0, Eshadow=0)
timeunit<-seq(0,finaltime,.5) # years

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
  ad_obs<-append(ad_obs, output_dt[time == start.incr + years_ad[i], Ishadow] - output_dt[time == (start.incr + (years_ad[i] - 1)), Ishadow] )
  
}



recid_percent_obs <- output_dt[time == start.incr +recid_perct_year_known, S] / (output_dt[time == start.incr + recid_perct_year_known, S] + output_dt[time == start.incr + recid_perct_year_known, P]) # percentage with prior incarceration in 2013)


ip_error = ((ip_obs[2:(length(ip_obs) - 1)] - as.numeric(ip_known[2:(length(ip_known) - 1)])) / as.numeric(ip_known[2:(length(ip_known) - 1)]))^2 
# incarceration prevalence squared error excluding first and last year

ip_error_start = ((ip_obs[1] - ip_known[1])/ip_known[1])^2


ip_error_end = ((ip_obs[length(ip_obs)] - ip_known[length(ip_known)])/ip_known[length(ip_known)])^2

ad_error = ((ad_obs - as.numeric(ad_known)) / as.numeric(ad_known))^2 # admissions squared error

rec_error = ((recid_percent_obs - recid_perct_known)/recid_perct_known)^2 


if(country == "Peru"){ # don't use admissions rate for errors
  
  error = (mean(ip_error) +  rec_error + ip_error_start + ip_error_start) / 4 # average incarceration prevalence, admissions, and recidivism error
  
  
  
} else {
  
  error = (mean(ip_error) + mean(ad_error) + rec_error + ip_error_start + ip_error_end) / 5 # average incarceration prevalence, admissions, and recidivism error
  
  
}



#### Start of Additional Information

ad_obs_all<-vector() # observed admissions 

for(i in 0:total_time){ #1990-2022
  ad_obs_all<-append(ad_obs_all, output_dt[time == start.incr + i, Ishadow] - output_dt[time == (start.incr + i  -1), Ishadow] )
  
}

rel_obs_all<-vector() # observed exits

for(i in 0:total_time){ #1990-2022
  rel_obs_all<-append(rel_obs_all, output_dt[time == start.incr + i, Eshadow] - output_dt[time == (start.incr + i -1), Eshadow] )
  
}

rel_rate_obs_all<-vector() # observed release rate

for(i in 0:total_time){ #1990-2022
  rel_rate_obs_all<-append(rel_rate_obs_all, (output_dt[time == start.incr + i, Eshadow] - output_dt[time == (start.incr + i -1), Eshadow])
                           /output_dt[time == (start.incr + i -1), S + P] )
  
}

#### End of Additional Information



########################################### Plot #########################################


output_dt[,Total := P+S+R+N+E]

output_dt_s = output_dt[1001:1067,]

output_melt <- melt(output_dt_s, id.vars = 'time', variable.name = 'Population', value.name = 'Count')
output_melt$Population <- factor(output_melt$Population, levels = c('P','S','R', 'N', 'E', 'Total','Ishadow','Eshadow'),
                                 labels = c('First Time Incarcerated', 'Repeated Incarcerated', 'Released','Never Incarcerated', 'Ex-Prisoner', 'Total','Admissions','Exits'))
ggplot(output_melt[!Population %in% c('Total','Admissions','Exits')], aes(x=time, y=Count, color=Population)) + geom_line() + theme_bw() +
  scale_y_log10()

library(ggplot2)


##################### Plot observed and actual incarceration prevalence #####################
df<-data.frame(years_ip, ip_obs )

if(country == 'Brazil'){ # Set true values
  df2 <-data.frame(year =(as.numeric(brazil_ip$Year)), aip =as.numeric(brazil_ip$`Adjusted Incarceration Prevalence`))
}

if(country == 'Peru'){ # Set true values
  df2 <-data.frame(year =(as.numeric(peru_ip$Year)), aip =as.numeric(peru_ip$`Adjusted Incarceration Prevalence`))
}

if(country == 'Argentina'){ # Set true values
  df2 <-data.frame(year =(as.numeric(argentina_ip$Year)), aip =as.numeric(argentina_ip$`Adjusted Incarceration Prevalence`))
}



g<-ggplot( df, aes(x=years_ip + 1990)) + 
  
  geom_line(aes(y = ip_obs), color = "orange")+
  geom_point(data = df2, 
             mapping = aes(x = year, y = aip),colour = "red")
  
 

print(g + labs(x = "Years", y = "Incarceration Prevalence"))


################## Plot observed and actual admissions rate ##################################

df<-data.frame(years_ad, ad_obs )

if(country == 'Brazil'){ # Set true values
  df2 <-data.frame(year =(as.numeric(brazil_ad$Year)), 
                   ad =as.numeric(brazil_ad$`Admissions Number`) / as.numeric(brazil_ad$`Population Size 15+`)*100000)
}

if(country == 'Peru'){ # Set true values
  df2 <-data.frame(year =(as.numeric(peru_ad$Year)), 
                   ad =as.numeric(peru_ad$`Admissions Number`) / as.numeric(peru_ad$`Population Size 15+`)*100000)
}

if(country == 'Argentina'){ # Set true values
  df2 <-data.frame(year =(as.numeric(argentina_ad$Year)), 
                   ad =as.numeric(argentina_ad$`Admissions Number`) / as.numeric(argentina_ad$`Population Size 15+`)*100000)
}


g<-ggplot( df, aes(x=years_ad + 1990)) + 
  
  geom_line(aes(y = ad_obs), color = "blue")+
  geom_point(data = df2, 
             mapping = aes(x = year, y = ad),colour = "red")

print(g + labs(x = "Years Since 1990", y = "Admissions"))


##################### Plot admissions and exit rate per 100k #######################################

df<-data.frame(x = seq(1990,2022,1), y = ad_obs_all, y1 = rel_obs_all)

g<-ggplot(df, aes(x)) +                    # basic graphical object
  geom_line(aes(y=y), colour="green") +  # admissions
  geom_line(aes(y=y1), colour="red")  # exits

print(g + labs(x = "Years", y = "(Green:Admissions), (Red:Exits)"))


########################## Plot net admissions #########################
plot(seq(1990,2022,1), ad_obs_all-rel_obs_all, xlab = "Years", ylab = "Net Admissions")


########################## Plot release rates################################

plot(seq(1990,2022,1), rel_rate_obs_all, xlab = "Years", ylab = "Release Rate")









