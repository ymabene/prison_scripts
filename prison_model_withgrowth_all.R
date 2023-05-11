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
                                     change.r.factor.1 = NA, change.r.factor.2 = NA, change.r.factor.3 = NA,
                                     interval_one_years = NA, interval_two_years=NA, rel_one_years=NA, rel_two_years = NA,
                                     covid.start=NA,covid.end=NA)
  

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
      if(t < offset + covid.start){
        
      }
      iR <- iR+ k2*(t-(intrvn.start + interval_one_years + interval_two_years)) 
      iE <- iE+ k2*(t-(intrvn.start + interval_one_years + interval_two_years)) 
      iN <- iN+ k2*(t-(intrvn.start + interval_one_years+ interval_two_years)) 
      
    } else{
      
      iR <- iR+ k2*((covid.start - interval_two_years - interval_one_years - intrvn.start + offset))
      iE <- iE + k2*((covid.start - interval_two_years - interval_one_years - intrvn.start + offset))
      iN <- iN + k2*((covid.start - interval_two_years - interval_one_years - intrvn.start + offset))
      
    }
    
    ################# Release Rates #####################
    
    if(country != "Peru") { # half the release rate over 1990-2022
      
      # Notes: r changes at a rate of l1. r can change in two ways:
      # 1. user specifies the factor by which r changes (ie 0.5 if you want r to halve over the period); l1 is automatically calculated
      # 2. user specifies the proportion of prison growth resulting from changes in r (vs changes in admissions); l1 is calibrated during optimization
      
      if (t >= change.r.start1){
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
            if(rel_two_years == 0){
              l3<-l2
             
            } else{
              
              l3 <- ((r * change.r.factor.3) - r) / (change.r.end1-(rel_two_years + rel_one_years+ change.r.start1) ) 
              
            }
            
            
          }
          
        }
          
          
          if(t >= change.r.start1){ # decrease/increase/maintain release rate
            if(t < (change.r.start1 + rel_one_years)){
              r <- r+l1*(t-change.r.start1)
             # print(r)
              
            } else{
              
              r <- r + l1*(rel_one_years)
              #print(r)
            }
              
             
            
          } 
          
          if(t >= (change.r.start1 + rel_one_years)){ # # decrease/increase/maintain release rate
            if(t < (change.r.start1 + rel_one_years+ rel_two_years)){
              r <- r + l2*(t - (change.r.start1 + rel_one_years)) 
              
            } else{
              
              r<- r + l2*(rel_two_years)
              #print(r)
              
            }
            
            
          } 
          
          if(t >= (change.r.start1 + rel_one_years + rel_two_years)){ # # decrease/increase/maintain release rate
            if(t < (change.r.end1)){
              
              r <- r + l3*(t- (change.r.start1 + rel_one_years + rel_two_years)) #+ l2*(rel_two_years - rel_one_years) +l1*(rel_one_years)
              
            } else{
              
              r <- r + l3*(change.r.end1- (change.r.start1 + rel_one_years + rel_two_years))
              #print(r)
              
            }
          }
          
          
        
        
      }
      
      #print(t)
      #print(r)
      #print(l1)
    } else { # use actual release rate data for Peru only
      
      if ((t >= intrvn.start) & (t < intrvn.start + total_time + 1) ){ # 1990-2023
        r <- intercept + (slope *(t - intrvn.start)) # incorporate predicted release rates
        
        
      }
      
    }
    
    ############### Covid-19 changes #####################
    
    #print(r)
    
    if (t >= offset + covid.start){
      
      
      if((t >= (offset + covid.start)) & (t < (offset + covid.end))){ # 2020-
        iR <- iR*covid_a
        iE <- iE*covid_a
        iN <- iN*covid_a
        r <- r*covid_r
  
      }
      
  
      if((t >=  (offset + covid.end)) & (t < intrvn.end)) { # returns to pre-pandemic incarceration rate
       

        
        iR <- iR+ k2*(t- (covid.end + intrvn.start)) #+ covid.start-(intrvn.start + interval_two_years))+ k1*(interval_two_years - interval_one_years) + k*(interval_one_years))
        iE <- iE+ k2*(t-(covid.end + intrvn.start)) #+ covid.start-(intrvn.start + interval_two_years)) + k1*(interval_two_years - interval_one_years) + k*(interval_one_years))
        iN <- iN+ k2*(t-(covid.end + intrvn.start)) #+ covid.start-(intrvn.start + interval_two_years))+ k1*(interval_two_years - interval_one_years) + k*(interval_one_years))
       
        r <- r + l3*(t-(covid.end + intrvn.start)) #+ l3*(change.r.end1- (change.r.start1 + rel_two_years)) #+ l2*(rel_two_years - rel_one_years) + l1*(rel_one_years)
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


# error:0.0004544431
all_params <- c(iR =   0.07170899, iE=   0.0004772377, iN=    0.0004772377, r =  0.5034239, k =  0, k1 =5.70424e-05, k2 = 0, gf= 0.4973802, gf2= 0.4993039,
                muP= 0.01091325, muR= 0.0176291, muE=0.01678962, muS= 0.01091325,
                muN=0.01678962, a=0.142857) # using 1/LE for general mortality and .65,1.05 for ratios 

all_params <- c(iR =   0.07170899, iE=   0.0004772377, iN=    0.0004772377, r =  0.5034239, k =  0, k1 =5.70424e-05, k2 = 0, covid_a= 0.4973802, covid_r= 0.4993039,
                l1=0,l2=0,l3=0,muP= 0.01091325, muR= 0.0176291, muE=0.01678962, muS= 0.01091325,
                muN=0.01678962, a=0.142857)


# CURRENT
all_params <- c(iR =  0.07106578, iE= 0.0004784638, iN= 0.0004784638, r =  0.5034239, k =  0, k1 =5.746305e-05, k2 = 0, covid_a=  0.6116575, covid_r= 1.435199,
                l1=0,l2=0,l3=0,muP= 0.01091325, muR= 0.0176291, muE=0.01678962, muS= 0.01091325,
                muN=0.01678962, a=0.142857)



########################################  Run Brazil #################################################

# error: 0.00220949
all_params <- c(iR = 0.1908153, iE=0.0006971622, iN=0.0006971622, r= 1.633183, k =  5.734617e-05,k1 = 0, k2 = 0, gf=0.9279631, gf2 = 0.9808103,
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

######################################### Run Colombia  #################################################

# error: .001

all_params <- c(iR = 0.04642193, iE= 0.0005503423, iN= 0.0005503423, r =0.5034239, k = 1.929627e-05, k1 =  1e-08, k2 = 1e-08,
                covid_a= 0.6539033, covid_r=1.364841,l1=0,l2=  -0.01502336,l3=  0.01564111,
                muP = 0.0111943, 
                muS =  0.0111943,
                muR = 0.0180831, 
                muE = 0.017222,
                muN = 0.017222, a=0.142857) 

# r = .5034239



all_params <- c(iR = 0.05189814, iE= 0.0005656261, iN=  0.0005656261, r =0.5034239, k =  1e-08, k1 = 0, k2 =  0,
                covid_a=  0.6902856, covid_r=1.347039,l1=0,l2=-0.03281143,l3=  0.02587123,
                muP = 0.0111943, 
                muS = 0.0111943,
                muR = 0.0180831, 
                muE = 0.017222,
                muN = 0.017222, a=0.142857) # not fitting admissions


all_params <- c(iR = 0.05511821, iE= 0.0004689414, iN= 0.0004689414, r =0.5034239, k =  1e-08, k1 = 0, k2 =  0,
                covid_a=  0.6902856, covid_r=1.345376,l1=05,l2=-0.03435735,l3= 0.01664506,
                muP = 0.0111943, 
                muS = 0.0111943,
                muR = 0.0180831, 
                muE = 0.017222,
                muN = 0.017222, a=0.142857) # fitting admissions

all_params <- c(iR =  0.0525709, iE= 0.0005317931, iN=0.0005317931, r =0.5034239, k = 1e-08, k1 = 0, k2 =  0,
                covid_a= 0.7104978, covid_r=1.338682,l1=0,l2= -0.03074574,l3= 0.01028386,
                muP = 0.0111943, 
                muS = 0.0111943,
                muR = 0.0180831, 
                muE = 0.017222,
                muN = 0.017222, a=0.142857)



# r = 0.9595281

all_params <- c(iR =  0.03638664, iE= 0.001094897, iN=  0.001094897, r =0.9595281, k = 2.741851e-05, k1 =1.24786e-05, k2 = 1.24786e-05,
                covid_a= 0.6874559, covid_r= 1.348388,l1=0,l2=-0.03306269,l3=  0.02538081,
                muP = 0.0111943, 
                muS = 0.0111943,
                muR = 0.0180831, 
                muE = 0.017222,
                muN = 0.017222, a=0.142857) 




############################################################################################################
###########################################################################################################



xstart <- c(P=0, S=0, R=0, N=100000, E=0, Ishadow=0, Eshadow=0)
timeunit<-seq(0,finaltime,.5) # years

# output <- ode(
#   func=prison.model.with.growth,
#   y=xstart,
#   times=timeunit,
#   parms=all_params,
#   intrvn.start=start.incr, 
#   intrvn.end=start.incr + time.passed,
#   change.r.start1 = change.r.start1.input,
#   change.r.factor = change.r.factor.input,
#   change.r.end1=change.r.end1.input,
#   cint = cint.input,
#   cintstart = cintstart.input,
#   interval_one_years = interval_one_years.input,
#   interval_two_years = interval_two_years.input
# )


output <- ode(
  func=prison.model.with.growth,
  y=xstart,
  times=timeunit,
  parms=all_params,
  intrvn.start=start.incr, 
  intrvn.end=start.incr + total_time,
  change.r.start1 = change.r.start1.input,
  change.r.factor.1 = change.r.factor.1.input,
  change.r.factor.2 = change.r.factor.2.input,
  change.r.factor.3 = change.r.factor.3.input,
  change.r.end1=change.r.end1.input,
  interval_one_years = interval_one_years.input,
  interval_two_years = interval_two_years.input,
  rel_one_years = rel_one_years.input,
  rel_two_years = rel_two_years.input,
  covid.start = covid.start.input,
  covid.end = covid.end.input
)





######

output_dt <- data.table(output)


offset <- 500 # start of model


ip_obs <-vector() # observed incarceration prevalence

for(i in 1:length(years_ip)){
  ip_obs<-append(ip_obs, output_dt[time == offset + years_ip[i] + .5, S + P] ) # Find IP halfway through year
  
  
}

ad_obs<-vector() # observed admissions rate


for(i in 1:length(years_ad)){
  ad_obs<-append(ad_obs, output_dt[time == offset + years_ad[i] + 1, Ishadow] - output_dt[time == (offset + (years_ad[i])), Ishadow] )
  
}



recid_percent_obs <- output_dt[time == offset +recid_perct_year_known, S] / (output_dt[time == offset + recid_perct_year_known, S] + output_dt[time == offset + recid_perct_year_known, P]) # percentage with prior incarceration in 2013)


ip_error = ((ip_obs[2:(length(ip_obs) - 1)] - as.numeric(ip_known[2:(length(ip_known) - 1)])) / as.numeric(ip_known[2:(length(ip_known) - 1)]))^2 
# incarceration prevalence squared error excluding first and last year

ip_error_start = ((ip_obs[1] - ip_known[1])/ip_known[1])^2

if(country == "Colombia"){ # use spline data
  ip_error_start = ((ip_obs[1] -  138.8909 )/ip_known[1])^2
  
}



ip_error_end = ((ip_obs[length(ip_obs)] - ip_known[length(ip_known)])/ip_known[length(ip_known)])^2


ad_error = ((ad_obs - as.numeric(ad_known)) / as.numeric(ad_known))^2 # admissions squared error




rec_error = ((recid_percent_obs - recid_perct_known)/recid_perct_known)^2 


if(country == "Peru"){ # don't use admissions rate for errors
  
  error = (mean(ip_error) +  rec_error + ip_error_start + ip_error_end) / 4 # average incarceration prevalence and recidivism error
  
  
  
  
} else {
  
  error = (mean(ip_error) + mean(ad_error) + rec_error + ip_error_start + ip_error_end) / 5 # average incarceration prevalence, admissions, and recidivism error
  
  
}




#### Start of Additional Information

ad_obs_all<-vector() # observed admissions 

for(i in 0:total_time){ #1990-2022
  ad_obs_all<-append(ad_obs_all, output_dt[time == (offset + i + 1), Ishadow] - output_dt[time == (offset + i ), Ishadow] )
  
  
}

rel_obs_all<-vector() # observed exits

for(i in 0:total_time){ #1990-2022
  rel_obs_all<-append(rel_obs_all, output_dt[time == (offset + i + 1), Eshadow] - output_dt[time == (offset + i), Eshadow] )
  
}

rel_rate_obs_all<-vector() # observed release rate

for(i in 0:total_time){ #1990-2022
  rel_rate_obs_all<-append(rel_rate_obs_all, (output_dt[time == (offset + i + 1), Eshadow] - output_dt[time == (offset + i ), Eshadow])
                           /output_dt[time == (offset + i + .5 ), S + P] )
  
}

#### End of Additional Information



########################################### Plot #########################################


output_dt[,Total := P+S+R+N+E]

output_dt_s = output_dt[1001:1067,]

output_melt <- melt(output_dt_s, id.vars = 'time', variable.name = 'Population', value.name = 'Count')
output_melt$Population <- factor(output_melt$Population, levels = c('P','S','R', 'N', 'E', 'Total','Ishadow','Eshadow'),
                                 labels = c('First Time Incarcerated', 'Repeated Incarcerated', 'Released','Never Incarcerated', 'Ex-Prisoner', 'Total','Admissions','Exits'))
ggplot(output_melt[!Population %in% c('Total','Admissions','Exits')], aes(x=time + 1490, y=Count, color=Population)) + geom_line() + theme_bw() +
  scale_y_log10() + labs(x = "Years", y = "Count")

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

if(country == 'Colombia'){ # Set true values
  df2 <-data.frame(year =(as.numeric(colombia_ip$Year)), aip =as.numeric(colombia_ip$`Adjusted Incarceration Prevalence`))
}



g<-ggplot( df, aes(x=years_ip + 1990)) + 
  
  geom_line(aes(y = ip_obs), color = "purple")+
  geom_point(data = df2, 
             mapping = aes(x = year, y = aip),colour = "black", size=3, shape=20)+
  theme_bw() +
  
  ggtitle(label = 'Colombia Incarceration Prevalence Over Time') +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(plot.title = element_text(size = 18))  
  
 

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

if(country == 'Colombia'){ # Set true values
  df2 <-data.frame(year =(as.numeric(colombia_ad$Year)), 
                   ad =as.numeric(colombia_ad$`Admissions Number`) / as.numeric(colombia_ad$`Population Size 15+`)*100000)
}


g<-ggplot( df, aes(x=years_ad + 1990)) + 
  
  geom_line(aes(y = ad_obs), color = "purple")+
  geom_point(data = df2, 
             mapping = aes(x = year, y = ad),colour = "red", size=3, shape=20)+
  theme_bw() +
  ggtitle(label = 'Colombia Admissions Rates Over Time') +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(plot.title = element_text(size = 18))  

print(g + labs(x = "Years", y = "Admissions"))


##################### Plot admissions and exit rate per 100k #######################################

df<-data.frame(x = seq(1990,2022,1), y = ad_obs_all, y1 = rel_obs_all)

g<-ggplot(df, aes(x)) +                    # basic graphical object
  geom_line(aes(y=y), colour="green") +  # admissions
  geom_line(aes(y=y1), colour="red")  # exits

print(g + labs(x = "Years", y = "(Green:Admissions), (Red:Exits)"))


########################## Plot net admissions #########################
plot(seq(1990,2023,1), ad_obs_all-rel_obs_all, xlab = "Years", ylab = "Net Admissions")


########################## Plot release rates################################

plot(seq(1990,2022,1), rel_rate_obs_all[1:33], xlab = "Years", ylab = "Release Rate",pch=19, col="blue", bg="blue")

########################## Plot Admissions  rates################################
plot(seq(1990,2022,1), ad_obs_all[1:33], xlab = "Years", ylab = "Admissions Rate",pch=19, col="red", bg="red")


