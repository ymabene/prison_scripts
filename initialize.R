
library(readxl)


# Upload data from excel sheet

prison_data <- read_excel("C:\\Users\\yasmi\\OneDrive\\My Documents\\Stanford-2021-2022\\Lab\\prison_data.xlsx")



####################################################################################
############ Initialize values for one country at a time ###########################
###################################################################################

# DEFINITIONS:

# finaltime <- final time the model runs at
# start.incr <- start time of model
# time.passed <- time until intervention end
# time.to.now <- time until intervention end
# intrvn.end= start.incr + time.passed
# interval_one_years.input = year that ends the first interval of incarceration rate growth (Default = intrvn.end)
# interval_two_years.input = years the ends the second interval of incarceration rate growth (Default = intrvn.end)
# total_time = total time of intervention
# cint.input = = amount of years the release rate stops decreasing for (Deafult = 0)
# cintstart.input = year that we stop decreasing the release rate (Default = start.incr + total_time)
# change.r.start1.input= Start of intervention
# change.r.end1.input = End of intervention
# change.r.factor.input = Proportion of the initial release rate that we want left in 2022
# recid_perct_known = Known recidivism rate
# recid_perct_year_known = year that known recidivism rate is from

#gf1 and gf2 are the constants we multiply the incarcerations by to decrease admissions due to Covid-19




############ Initialize values below for Peru #####################
###################################################################

### There are two intervals of growth for the incarceration rates : 1990-2006 and 2006-2012. Incarceration rate growth stops from 
### 2013-2019. The incarceration rates are reduced using gf and gf2 in 2020 after.

country = "Peru"

finaltime <- 600
start.incr <-500
time.passed <- 30
time.to.now <-30
intrvn.end= start.incr + time.passed
interval_one_years.input = 16
interval_two_years.input = 22
total_time = 32 
cint.input = 0
cintstart.input = time.passed
change.r.start1.input=NA
change.r.end1.input = NA
change.r.factor.input = NA
recid_perct_known = .2471
recid_perct_year_known = 31

peru <- subset(prison_data, Country == "Peru")
peru <- replace(peru, peru =='NA', NA) # fix format from excel to r
peru_ip <- subset(peru, select=c('Year', 'Adjusted Incarceration Prevalence'))
peru_ip <- peru_ip[complete.cases(peru_ip), ] # Table with True incarceration prevalence data with corresponding years

peru_ad <- subset(peru, select=c('Year', 'Admissions Number', 'Population Size 15+'))
peru_ad <- peru_ad[complete.cases(peru_ad), ] # Table with True admissions number data with corresponding years

peru_rel <-  subset(peru, select=c('Year','Release Rate'))
peru_rel <- peru_rel[complete.cases(peru_rel), ]  # Table with True release rate data with corresponding years

years_rel <- peru_rel$Year - 1990 # years since 1990
rel_known<- as.numeric(peru_rel$`Release Rate`)


years_ip<-peru_ip$Year - 1990 # years since 1990
ip_known<-peru_ip$`Adjusted Incarceration Prevalence` # Adjusted true incarceration prevalence
ip_known<-as.numeric(ip_known)

years_ad<-peru_ad$Year - 1990 # years since 1990
ad_known<-(as.numeric(peru_ad$`Admissions Number`)/as.numeric(peru_ad$`Population Size 15+`))*100000 # Adjusted true admissions



## Release rates prediction using linear regression
my_mod <- lm(rel_known ~ years_rel)  

plot(years_rel,                       # Regression line plot
     rel_known,
     type = "l",
     xlab= "Years",
     ylab= "Release Rate")
lines(years_rel,
      predict(my_mod),
      col = 2,
      lwd = 2
      )
my_coef <- coef(my_mod) 

my_equation <- paste("y =",        # Regression equation
                     coef(my_mod)[[1]],
                     "+",
                     coef(my_mod)[[2]],
                     "* x")
my_equation   

predicted_rel <- ((peru$Year - 1990) * coef(my_mod)[[2]] ) + coef(my_mod)[[1]] # predicted release rates based on linear regression



#############################################################################################

############ Intialize values below for Brazil ##################
#################################################################

## The incarceration rate grows from 1990-2015. Afterwards the incarceration rates stop growing and the release rates stop decreasing.

country = "Brazil"

finaltime <- 600
start.incr <-500
time.passed <- 25
time.to.now <-25
intrvn.end= start.incr + time.passed
total_time <-32
interval_one_years.input = time.passed
interval_two_years.input = time.passed
change.r.start1.input=start.incr
change.r.end1.input = change.r.start1.input + total_time
change.r.factor.input = .5
recid_perct_known = .49
recid_perct_year_known = 23.5
cint.input = 7
cintstart.input = 25

brazil <- subset(prison_data, Country == "Brazil")
brazil<- replace(brazil, brazil =='NA', NA) # fix format from excel to r
brazil_ip <- subset(brazil, select=c('Year', 'Adjusted Incarceration Prevalence'))
brazil_ip <- brazil_ip[complete.cases(brazil_ip), ] # Table with True incarceration prevalence data with corresponding years

brazil_ad <- subset(brazil, select=c('Year', 'Admissions Number', 'Population Size 15+'))
brazil_ad <- brazil_ad[complete.cases(brazil_ad), ] # Table with True admissions number data with corresponding years


years_ip<-brazil_ip$Year - 1990 # years since 1990
ip_known<-brazil_ip$`Adjusted Incarceration Prevalence` # Adjusted true incarceration prevalence
ip_known <- as.numeric(ip_known)

years_ad<-brazil_ad$Year - 1990 # years since 1990
ad_known<-(as.numeric(brazil_ad$`Admissions Number`)/as.numeric(brazil_ad$`Population Size 15+`))*100000 # Adjusted true admissions




########################### Initialize Values for Argentina ###########################################################
######################################################################################################################

## The incarceration rates grow from 1990-2002 and 2015-2019. The incarceration rates stop growing in 2002-2015 and the release rates
## stop decreasing in this time period. The incarceration rates decrease in 2020 after using gf and gf2 due to Covid.

country = "Argentina"

finaltime <- 600
start.incr <-500
intrvn.end= 532
time.passed <- 30
time.to.now <-30
intrvn.end= start.incr + time.passed
total_time <-32

interval_one_years.input = 12
interval_two_years.input = 25
change.r.start1.input=start.incr
change.r.end1.input = change.r.start1.input + total_time
change.r.factor.input = .5
recid_perct_known = .28
recid_perct_year_known = 29
cint.input = 13
cintstart.input = 12


argentina <- subset(prison_data, Country == "Argentina")
argentina<- replace(argentina, argentina =='NA', NA) # fix format from excel to r
argentina_ip <- subset(argentina, select=c('Year', 'Adjusted Incarceration Prevalence'))
argentina_ip <- argentina_ip[complete.cases(argentina_ip), ] # Table with True incarceration prevalence data with corresponding years

argentina_ad <- subset(argentina, select=c('Year', 'Admissions Number', 'Population Size 15+'))
argentina_ad <- argentina_ad[complete.cases(argentina_ad), ] # Table with True admissions number data with corresponding years


years_ip<-argentina_ip$Year - 1990 # years since 1990
ip_known<-argentina_ip$`Adjusted Incarceration Prevalence` # Adjusted true incarceration prevalence
ip_known<-as.numeric(ip_known)

years_ad<-argentina_ad$Year - 1990 # years since 1990
ad_known<-(as.numeric(argentina_ad$`Admissions Number`)/as.numeric(argentina_ad$`Population Size 15+`))*100000 # Adjusted true admissions

