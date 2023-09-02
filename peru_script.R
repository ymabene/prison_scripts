
################### Script for Peru ##################

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


# Upload data 

prison_data <- read.csv("https://raw.githubusercontent.com/ymabene/prison_scripts/main/prison_data.csv",
                        header=TRUE)
prison_data <- as.data.frame(prison_data)


### There are two intervals of growth for the incarceration rates : [1990-2006) 
## and [2006-2012). There is no growth in the first interval but growth in the second.
## Incarceration rate growth stops again from [2012-end). The incarceration
## rates are reduced in 2020 after. The release rates for Peru are determined using a regression model
## fitt to actual release rate data. 


## For variable definitions see ("prison_model_withgrowth_all.R")

country = "Peru"

finaltime <- 600
start.incr <-500 # 1990
stop.incr <- 534 # 2024
interval_one_years.input = 16 # Interval 1: [1990-2006)
interval_two_years.input = 6 # Interval 2: [2006 -2012)

# Decrease release rate for entire time period
change.r.start.input=start.incr
change.r.end.input = stop.incr
change.r.factor.1.input = NA # fit release rate to regression model
change.r.factor.2.input = NA
change.r.factor.3.input = NA
recid_perct_known = .2471
recid_perct_year_known = 31 # 2021
covid.start.input = 30 # 2020
covid.end.input = 32  # 2022
rel_one_years.input = 0
rel_two_years.input = 0


peru <- subset(prison_data, Country == "Peru")
peru <- replace(peru, peru =='NA', NA) # fix format from excel to r

# Table with True incarceration prevalence data with corresponding years
peru_ip <- subset(peru, select=c('Year', 'Adjusted.Incarceration.Prevalence'))
peru_ip <- peru_ip[complete.cases(peru_ip), ] 

# Table with True admissions number data with corresponding years
peru_ad <- subset(peru, select=c('Year', 'Admissions.Number', 'Population.Size.15.'))
peru_ad <- peru_ad[complete.cases(peru_ad), ] 

# Table with True release rate data with corresponding years
peru_rel <-  subset(peru, select=c('Year','Release.Rate'))
peru_rel <- peru_rel[complete.cases(peru_rel), ]  

years_rel <- peru_rel$Year - 1990 # Years of release rate data written as years since 1990
rel_known<- as.numeric(peru_rel$`Release.Rate`) # True release rate data


years_ip<-peru_ip$Year - 1990 #  Years of incarceration prev data written as years since 1990
ip_known<-peru_ip$`Adjusted.Incarceration.Prevalence` 
ip_known<-as.numeric(ip_known) # True population adjusted incarceration prevalence

years_ad<-peru_ad$Year - 1990 # Years of admissions data written as years since 1990
ad_known<-(as.numeric(peru_ad$`Admissions.Number`)/as.numeric(peru_ad$`Population.Size.15.`))*100000 
# True admissions data adjusted for population


## Release rates prediction using linear regression
my_mod <- lm(rel_known ~ years_rel)  

plot(years_rel,                       # Regression line plot
     rel_known,
     type = "l",
     xlab= "Years",
     ylab= "Release Rate")
lines(years_rel,
      predict(my_mod),
      col = "blue",
      lwd = 2
)
my_coef <- coef(my_mod) 

my_equation <- paste("y =",        # Regression equation
                     coef(my_mod)[[1]],
                     "+",
                     coef(my_mod)[[2]],
                     "* x")
my_equation   

predicted_rel <- ((peru$Year - 1990) * coef(my_mod)[[2]] ) + coef(my_mod)[[1]] 
# predicted release rates based on linear regression

# constants of regression model for fitting Peru 
intercept = 0.503423912234061
slope = -0.00999950168014751


################## Calibrate Parameters Using Optim #####################
#########################################################################
#########################################################################


smallest_error <- Inf


country_incarc_rates <-  get_iE_iR_grow(recid_perct_known = .2471, recid_prob_known = 0, recid_perct_year_known = 31,
                                        years_ip = years_ip ,recid_prob_year_known = 0,
                                        ip_known = ip_known, years_ad = years_ad,
                                        ad_known=ad_known,start.incr = start.incr, stop.incr = stop.incr,
                                        param_start = c(0.09865996, 0.00167854, 0.5034239, 5.700556e-06, 5.700556e-06, 5.700556e-06,.5,1.5),
                                        param_lower_bounds=c(0.0001, 0.0001, 0.2, 0.00000001, 0.00000001, 0.00000001,.1,1), 
                                        param_upper_bounds=c(.5,.2, 2, 0.01,.01,0.01, 1,4), 
                                        scale =c(0.25005000, 0.10005000, 1.10000000, 0.005000005, 0.005000005, 0.005000005, .55, .55),
                                        muP = 0.01091325,
                                        muS =  0.01091325,
                                        muR = 0.0176291,
                                        muE = 0.01678962,
                                        muN = 0.01678962)




################## Run Calibrated Parameters in Prison Model and Plot ########
##############################################################################
##############################################################################


# calibrated values from optim function
country_incarc_rates <-list(iR = 0.0668022, iE= 0.000487048, r = 0.5034239,
                        k = 0, k1 = 5.758476e-05, k2 =0,
                        covid_a= 0.7277991, covid_r =  1.373086)


all_params <- c(iR =  country_incarc_rates$iR, iE= country_incarc_rates$iE, 
                iN= country_incarc_rates$iE, r =  country_incarc_rates$r, 
                k =  0, k1 =country_incarc_rates$k1, 
                k2 = 0, covid_a=  country_incarc_rates$covid_a, 
                covid_r= country_incarc_rates$covid_r,l1=0,l2=0,l3=0,
                muP= 0.01091325, muR= 0.0176291, muE=0.01678962, muS= 0.01091325,
                muN=0.01678962, a=0.142857)


## Note: k1 and k2 are set to 0 since
## we specified no growth in the first interval and there is no third interval.

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

  
error = (mean(ip_error) +  rec_error + ip_error_start + ip_error_end) / 4 
# average incarceration prevalence and recidivism error (do not use admissions error)



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

# Set true values
df2 <-data.frame(year =(as.numeric(peru_ip$Year)), 
                 aip =as.numeric(peru_ip$`Adjusted.Incarceration.Prevalence`))


g<-ggplot( df, aes(x=years_ip + 1990)) + 
  
  geom_line(aes(y = ip_obs), color = "purple")+
  geom_point(data = df2, 
             mapping = aes(x = year, y = aip),colour = "black", size=3, shape=20)+
  theme_bw() +
  
  ggtitle(label = 'Peru Incarceration Prevalence Over Time') +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(plot.title = element_text(size = 18))  



print(g + labs(x = "Years", y = "Incarceration Prevalence"))


################## Plot observed and actual admissions rate ##################################

df<-data.frame(years_ad, ad_obs )


# Set true values
df2 <-data.frame(year =(as.numeric(peru_ad$Year)), 
                   ad =as.numeric(peru_ad$`Admissions.Number`) / as.numeric(peru_ad$`Population.Size.15.`)*100000)

g<-ggplot( df, aes(x=years_ad + 1990)) + 
  
  geom_line(aes(y = ad_obs), color = "purple")+
  geom_point(data = df2, 
             mapping = aes(x = year, y = ad),colour = "red", size=3, shape=20)+
  theme_bw() +
  ggtitle(label = 'Peru Admissions Rates Over Time') +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(plot.title = element_text(size = 18))  

print(g + labs(x = "Years", y = "Admissions"))


##################### Plot admissions and exit rate per 100k #######################################

df<-data.frame(x = seq(1990,2024,1), y = ad_obs_all, y1 = rel_obs_all)

g <- ggplot(df, aes(x)) +
  geom_line(aes(y = y, color = "Admissions"), linetype = "solid") +
  geom_line(aes(y = y1, color = "Exits"), linetype = "solid") +
  theme_bw() +
  labs(x = "Years", y = "Admissions and Release Rates") +
  scale_color_manual(values = c("Admissions" = "green", "Exits" = "red")) +
  labs(color = "Rates") 

print(g)



