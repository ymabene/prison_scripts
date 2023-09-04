################### Script for Colombia ##################

source("optim_incarc_withgrowth_all.R")
source("prison_model_withgrowth_all.R")


################## Initialize variables #################################
#########################################################################
#########################################################################

library(readxl)
library(deSolve)
library(ggplot2)
library(data.table)
library(SciViews)
library(readxl)
library(ggplot2)

# Upload data 

prison_data <- read.csv("https://raw.githubusercontent.com/ymabene/prison_scripts/main/prison_data.csv",
                        header=TRUE)
prison_data <- as.data.frame(prison_data)


## The incarceration rates grow from [1992-2012). The incarceration 
## rates decrease from [2012-end). The release rates decrease from [1992-2012)
## and increase from [2012-end).
## The incarceration rates decrease and release rates
## increase in 2020 due to Covid.

## For variable definitions see ("prison_model_withgrowth_all.R")


samples<-runif(100, 1/3,1)
factor_one = sample(samples,1,replace=TRUE)

factor_one = 0.3745025 # comment out to change sample

samples<-runif(100, 1,3)
factor_two = sample(samples,1,replace=TRUE)

factor_two =  1.126807 # comment out to change sample

country = "Colombia"

finaltime <- 600
start.incr <-502 # start changes in 1992
stop.incr <- 533 # 2023

interval_one_years.input = 20
interval_two_years.input = 11
change.r.start.input=502
change.r.end.input = stop.incr
change.r.factor.1.input = factor_one #  0.7891789, 0.3745025
change.r.factor.2.input = factor_two # 1.570884,  1.126807
change.r.factor.3.input = factor_two 
recid_perct_known = .223
recid_perct_year_known = 29
covid.start.input = 30
covid.end.input = 33 
rel_one_years.input = 20
rel_two_years.input = 11


colombia <- subset(prison_data, Country == "Colombia")
colombia<- replace(colombia, colombia =='NA', NA) # fix format from excel to r
colombia_ip <- subset(colombia, select=c('Year', 'Adjusted.Incarceration.Prevalence'))
colombia_ip <- colombia_ip[complete.cases(colombia_ip), ] 
# Table with True incarceration prevalence data with corresponding years

colombia_ad <- subset(colombia,select=c('Year', 'Admissions.Number', 'Population.Size.15.'))
colombia_ad <- colombia_ad[complete.cases(colombia_ad), ] 
# Table with True admissions number data with corresponding years


years_ip<-colombia_ip$Year - 1990 # Years of incarceration rate data written as years since 1990
ip_known<-colombia_ip$`Adjusted.Incarceration.Prevalence` # Adjusted true incarceration prevalence
ip_known<-as.numeric(ip_known)

years_ad<-colombia_ad$Year - 1990 # Years of admissions data written as years since 1990
ad_known<-(as.numeric(colombia_ad$`Admissions.Number`)/as.numeric(colombia_ad$`Population.Size.15.`))*100000 
# Adjusted true admissions

### Fit spline for Colombia Incarceration Prevalence

spline = smooth.spline(years_ip + 1990,ip_known, spar = .55)
plot(years_ip + 1990, ip_known, xlab= "Years", 
     ylab = "Incarceration Prevalence", pch=19, col="blue", bg="blue")
lines(spline)

spline_ip = 138.688 # Fitted IP for 1990



################## Calibrate Parameters Using Optim #####################
#########################################################################
#########################################################################


smallest_error <- Inf

country_incarc_rates <-  get_iE_iR_grow(recid_perct_known = .223, recid_prob_known = 0, recid_perct_year_known = 29,
                                        years_ip = years_ip ,recid_prob_year_known = 0,
                                        ip_known = ip_known, years_ad = years_ad,
                                        ad_known=ad_known,start.incr = start.incr, stop.incr = stop.incr,
                                        param_start = c(0.05511821, 0.008507921, .5034239, 1e-06, 0, 0,.43,1.84),
                                        param_lower_bounds=c(0.0001, 0.0001, 0.2, 0.000001, -0.00001, -0.00001,.1,1),
                                        param_upper_bounds=c(.5,.2, 2, .01,0,0, 1,4),
                                        scale =c(0.2499500, 0.0999500, 0.9000000, 0.0049995, 0.0000050, 0.0000050,0.4500000, 1.5),
                                        muP = 0.0111943,
                                        muS = 0.0111943,
                                        muR = 0.0180831,
                                        muE = 0.017222,
                                        muN = 0.017222)
# error = .003





################## Run Calibrated Parameters in Prison Model and Plot ########
##############################################################################
##############################################################################


# calibrated values from optim function

# *** Note: These parameters are hard coded to avoid having to re-run 
# the optimization function. If you modify the optimization function
# or its parameters, comment out these values***

country_incarc_rates <-list(iR = 0.06751577, iE= 0.0002649274, iN= 0.0002649274, r = 0.2476413,
                            k = 3.962651e-06, k1 = 0 , k2 =0,
                            covid_a= 0.4621881, covid_r = 1.616103)


######################################################################

all_params <- c(iR =  country_incarc_rates$iR, iE= country_incarc_rates$iE, 
                iN= country_incarc_rates$iE, r =  country_incarc_rates$r, 
                k = country_incarc_rates$k, k1 =country_incarc_rates$k1, 
                k2 = 0, covid_a=  country_incarc_rates$covid_a, 
                covid_r= country_incarc_rates$covid_r,l1=0,l2=0,l3=0,
                muP= 0.01091325, muR= 0.0176291, muE=0.01678962, muS= 0.01091325,
                muN=0.01678962, a=0.142857)


## Note: k2 is set to 0 since there are only 2 growth intervals.

## The l parameters are set to 0 since they are calculated in the prison model function.






xstart <- c(P=0, S=0, R=0, N=100000, E=0, Ishadow=0, Eshadow=0)
timeunit<-seq(0,finaltime,.5) # years



output <- ode(
  func=prison.model.with.growth,
  y=xstart,
  times=timeunit,
  parms=all_params,
  intrvn.start=start.incr, 
  intrvn.end=stop.incr,
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



recid_percent_obs <- output_dt[time == offset +recid_perct_year_known, S] / 
  (output_dt[time == offset + recid_perct_year_known, S] + 
     output_dt[time == offset + recid_perct_year_known, P]) 
# percentage with prior incarceration 


ip_error = ((ip_obs[2:(length(ip_obs) - 1)] - as.numeric(ip_known[2:(length(ip_known) - 1)])) / 
              as.numeric(ip_known[2:(length(ip_known) - 1)]))^2 
# incarceration prevalence squared error excluding first and last year

ip_error_start = ((ip_obs[1] - ip_known[1])/ip_known[1])^2
# first year incarceration prevalence error



ip_error_end = ((ip_obs[length(ip_obs)] - ip_known[length(ip_known)])/ip_known[length(ip_known)])^2
# final year incarceration prevalence error

ad_error = ((ad_obs - as.numeric(ad_known)) / as.numeric(ad_known))^2 
# admissions squared error


rec_error = ((recid_percent_obs - recid_perct_known)/recid_perct_known)^2 
# recidivism error


error = (mean(ip_error)  + rec_error + ip_error_start + ip_error_end) / 4 
# average incarceration prevalence, admissions, and recidivism error



#### Start of Additional Information

ad_obs_all<-vector() # observed admissions 

for(i in 0:(stop.incr - offset)){ #1990-2024
  ad_obs_all<-append(ad_obs_all, output_dt[time == (offset + i + 1), Ishadow] - output_dt[time == (offset + i ), Ishadow] )
  
  
}

rel_obs_all<-vector() # observed exits

for(i in 0:(stop.incr - offset)){ #1990-2024
  rel_obs_all<-append(rel_obs_all, output_dt[time == (offset + i + 1), Eshadow] - output_dt[time == (offset + i), Eshadow] )
  
}

rel_rate_obs_all<-vector() # observed release rate

for(i in 0:(stop.incr - offset)){ #1990-2024
  rel_rate_obs_all<-append(rel_rate_obs_all, (output_dt[time == (offset + i + 1), Eshadow] - output_dt[time == (offset + i ), Eshadow])
                           /output_dt[time == (offset + i + .5 ), S + P] )
  
}

################### Plot compartment population over time ###############

output_dt[,Total := P+S+R+N+E]

output_dt_s = output_dt[1001:1067,]

output_melt <- melt(output_dt_s, id.vars = 'time', variable.name = 'Population', value.name = 'Count')
output_melt$Population <- factor(output_melt$Population, levels = c('P','S','R', 'N', 'E', 'Total',
                                                                    'Ishadow','Eshadow'),
                                 labels = c('First Time Incarcerated', 'Repeated Incarcerated',
                                            'Released','Never Incarcerated', 'Ex-Prisoner', 
                                            'Total','Admissions','Exits'))
ggplot(output_melt[!Population %in% c('Total','Admissions','Exits')], aes(x=time + 1490,
                                                                          y=Count, color=Population)) + geom_line() + theme_bw() +
  scale_y_log10() + labs(x = "Years", y = "Count")



##################### Plot observed and actual incarceration prevalence #####################
df<-data.frame(years_ip, ip_obs )

# Set true values
df2 <-data.frame(year =(as.numeric(colombia_ip$Year)), 
                 aip =as.numeric(colombia_ip$`Adjusted.Incarceration.Prevalence`))


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


# Set true values
df2 <-data.frame(year =(as.numeric(colombia_ad$Year)), 
                 ad =as.numeric(colombia_ad$`Admissions.Number`) / as.numeric(colombia_ad$`Population.Size.15.`)*100000)

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

df<-data.frame(x = seq(1990,2023,1), y = ad_obs_all, y1 = rel_obs_all)

g <- ggplot(df, aes(x)) +
  geom_line(aes(y = y, color = "Admissions"), linetype = "solid") +
  geom_line(aes(y = y1, color = "Exits"), linetype = "solid") +
  theme_bw() +
  labs(x = "Years", y = "Admissions and Release Rates") +
  scale_color_manual(values = c("Admissions" = "green", "Exits" = "red")) +
  labs(color = "Rates") 

print(g)







