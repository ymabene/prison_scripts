
library(readxl)


# Upload data from excel sheet

prison_data <- read_excel("C:\\Users\\yasmi\\OneDrive\\My Documents\\Stanford-2021-2022\\Lab\\prison_data.xlsx")



####################################################################################
############ Initialize values for one country at a time ###########################
###################################################################################

# DEFINITIONS:

# finaltime <- final time the model runs at
# start.incr <- start time of model
# total_time = total time of intervention (run until 2024 so that we have valid results for all of 2023)
# time.passed <- time until intervention ends for Covid
# time.to.now <- time until intervention ends for Void
# intrvn.end= start.incr + total_time
# interval_one_years.input = number of years of first interval of incarceration rate growth ie. 11 for 1990-2001. (Default = 0)
# interval_two_years.input = number of years of the second interval of incarceration rate growth ie. 11 for 2001-2012 (Default = 0)

# change.r.start1.input= Start of release rate intervention. 
# change.r.end1.input = End of intervention (run until 2024 so that we have valid results for all of 2023)

# change.r.factor.1.input = Proportion of the release rate at start of interval 1 that we want left at end of interval1
# change.r.factor.2.input = Proportion of the initial release rate at start of interval 2 that we want left at end of interval 2
# change.r.factor.3.input = Proportion of the initial release rate at start of interval 2 that we want left at end of interval 3

# recid_perct_known = Known recidivism rate
# recid_perct_year_known = year that known recidivism rate is from (eg. 29 for 2019)
# rel_one_years.input = Number of years in first release rate interval. Default is 0
# rel_two_years.input = Number of years in second release rate interval. Default is 0
# covid.start = year to start decreasing admissions rate and increasing release rates due to Covid-19 (30)
# covid.end = year admissions rate returns to pre-pandemic level






############ Initialize values below for Peru #####################
###################################################################

### There are two intervals of growth for the incarceration rates : 1990-2006 and 2006-2012. Incarceration rate growth stops from 
### 2013-2020. The incarceration rates are reduced using gf and gf2 in 2020 after.

country = "Peru"

finaltime <- 600
start.incr <-500
time.passed <- 30
time.to.now <-30
total_time = 34 
intrvn.end= start.incr + total_time
interval_one_years.input = 16 #2006
interval_two_years.input = 6 # 2012
change.r.start1.input=start.incr
change.r.end1.input = change.r.start1.input + time.passed
change.r.factor.1.input = .5 #1
change.r.factor.2.input = NA
change.r.factor.3.input = NA
recid_perct_known = .2471
recid_perct_year_known = 31
covid.start.input = 30
covid.end.input = 32
rel_one_years.input = 0
rel_two_years.input = 0


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
total_time <-34
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



#############################################################################################

############ Intialize values below for Colombia ##################
#################################################################

country = "Colombia"


finaltime <- 600
start.incr <-502 # start changes in 1992
time.passed <- 30
time.to.now <-30
total_time <-34
intrvn.end= start.incr + total_time

interval_one_years.input = 20
interval_two_years.input = 8
change.r.start1.input=start.incr
change.r.end1.input = change.r.start1.input + time.passed
change.r.factor.1.input = 1 #1
change.r.factor.2.input = NA
change.r.factor.3.input = NA
recid_perct_known = .223
recid_perct_year_known = 29
covid.start.input = 30
covid.end.input = 33 
rel_one_years.input = 8 
rel_two_years.input = 12 


colombia <- subset(prison_data, Country == "Colombia")
colombia<- replace(colombia, colombia =='NA', NA) # fix format from excel to r
colombia_ip <- subset(colombia, select=c('Year', 'Adjusted Incarceration Prevalence'))
colombia_ip <- colombia_ip[complete.cases(colombia_ip), ] # Table with True incarceration prevalence data with corresponding years

colombia_ad <- subset(colombia,select=c('Year', 'Admissions Number', 'Population Size 15+'))
colombia_ad <- colombia_ad[complete.cases(colombia_ad), ] # Table with True admissions number data with corresponding years


years_ip<-colombia_ip$Year - 1990 # years since 1990
ip_known<-colombia_ip$`Adjusted Incarceration Prevalence` # Adjusted true incarceration prevalence
ip_known<-as.numeric(ip_known)

years_ad<-colombia_ad$Year - 1990 # years since 1990
ad_known<-(as.numeric(colombia_ad$`Admissions Number`)/as.numeric(colombia_ad$`Population Size 15+`))*100000 # Adjusted true admissions





#############################################################################################

############ Intialize values below for Venezuela ##################
#################################################################


country = "Venezuela"


venezuela <- subset(prison_data, Country == "Venezuela")
venezuela <- replace(venezuela , venezuela  =='NA', NA) # fix format from excel to r
venezuela_ip <- subset(venezuela , select=c('Year', 'Adjusted Incarceration Prevalence'))
venezuela_ip <- venezuela_ip[complete.cases(venezuela_ip), ] # Table with True incarceration prevalence data with corresponding years

venezuela_ad <- subset(venezuela,select=c('Year', 'Admissions Number', 'Population Size 15+'))
venezuela_ad <- venezuela_ad[complete.cases(venezuela_ad), ] # Table with True admissions number data with corresponding years


years_ip<-venezuela_ip$Year - 1990 # years since 1990
ip_known<-venezuela_ip$`Adjusted Incarceration Prevalence` # Adjusted true incarceration prevalence
ip_known<-as.numeric(ip_known)

years_ad<-venezuela_ad$Year - 1990 # years since 1990
ad_known<-(as.numeric(venezuela_ad$`Admissions Number`)/as.numeric(venezuela_ad$`Population Size 15+`))*100000 # Adjusted true admissions


#############################################################################################

############ Intialize values below for Ecuador ##################
#################################################################


country = "Ecuador"


ecuador <- subset(prison_data, Country == "Ecuador")
ecuador <- replace(ecuador , ecuador  =='NA', NA) # fix format from excel to r
ecuador_ip <- subset(ecuador , select=c('Year', 'Adjusted Incarceration Prevalence'))
ecuador_ip <- ecuador_ip[complete.cases(ecuador_ip), ] # Table with True incarceration prevalence data with corresponding years

ecuador_ad <- subset(ecuador,select=c('Year', 'Admissions Number', 'Population Size 15+'))
ecuador_ad <- ecuador_ad[complete.cases(ecuador_ad), ] # Table with True admissions number data with corresponding years


years_ip<-ecuador_ip$Year - 1990 # years since 1990
ip_known<-ecuador_ip$`Adjusted Incarceration Prevalence` # Adjusted true incarceration prevalence
ip_known<-as.numeric(ip_known)

years_ad<-ecuador_ad$Year - 1990 # years since 1990
ad_known<-(as.numeric(ecuador_ad$`Admissions Number`)/as.numeric(ecuador_ad$`Population Size 15+`))*100000 # Adjusted true admissions



#############################################################################################

############ Intialize values below for Bolivia##################
#################################################################


country = "Bolivia"

bolivia <- subset(prison_data, Country == "Bolivia")
bolivia <- replace(bolivia , bolivia  =='NA', NA) # fix format from excel to r
bolivia_ip <- subset(bolivia, select=c('Year', 'Adjusted Incarceration Prevalence'))
bolivia_ip <- bolivia_ip[complete.cases(bolivia_ip), ] # Table with True incarceration prevalence data with corresponding years

bolivia_ad <- subset(bolivia,select=c('Year', 'Admissions Number', 'Population Size 15+'))
bolivia_ad <- bolivia_ad[complete.cases(bolivia_ad), ] # Table with True admissions number data with corresponding years


years_ip<-bolivia_ip$Year - 1990 # years since 1990
ip_known<-bolivia_ip$`Adjusted Incarceration Prevalence` # Adjusted true incarceration prevalence
ip_known<-as.numeric(ip_known)

years_ad<-bolivia_ad$Year - 1990 # years since 1990
ad_known<-(as.numeric(bolivia_ad$`Admissions Number`)/as.numeric(bolivia_ad$`Population Size 15+`))*100000 # Adjusted true admissions


#############################################################################################

############ Intialize values below for Chile ##################
#################################################################

country = "Chile"

chile <- subset(prison_data, Country == "Chile")
chile <- replace(chile, chile =='NA', NA) # fix format from excel to r
chile_ip <- subset(chile, select=c('Year', 'Adjusted Incarceration Prevalence'))
chile_ip <- chile_ip[complete.cases(chile_ip), ] # Table with True incarceration prevalence data with corresponding years

chile_ad <- subset(chile,select=c('Year', 'Admissions Number', 'Population Size 15+'))
chile_ad <- chile_ad[complete.cases(chile_ad), ] # Table with True admissions number data with corresponding years


years_ip<-chile_ip$Year - 1990 # years since 1990
ip_known<-chile_ip$`Adjusted Incarceration Prevalence` # Adjusted true incarceration prevalence
ip_known<-as.numeric(ip_known)

years_ad<-chile_ad$Year - 1990 # years since 1990
ad_known<-(as.numeric(chile_ad$`Admissions Number`)/as.numeric(chile_ad$`Population Size 15+`))*100000 # Adjusted true admissions



#############################################################################################

############ Intialize values below for El Salvador ##################
#################################################################


country = "El Salvador"

elsal <- subset(prison_data, Country == "El Salvador")
elsal <- replace(elsal, elsal =='NA', NA) # fix format from excel to r
elsal_ip <- subset(elsal, select=c('Year', 'Adjusted Incarceration Prevalence'))
elsal_ip <- elsal_ip[complete.cases(elsal_ip), ] # Table with True incarceration prevalence data with corresponding years

elsal_ad <- subset(elsal,select=c('Year', 'Admissions Number', 'Population Size 15+'))
elsal_ad <- elsal_ad[complete.cases(elsal_ad), ] # Table with True admissions number data with corresponding years


years_ip<-elsal_ip$Year - 1990 # years since 1990
ip_known<-elsal_ip$`Adjusted Incarceration Prevalence` # Adjusted true incarceration prevalence
ip_known<-as.numeric(ip_known)

years_ad<-elsal_ad$Year - 1990 # years since 1990
ad_known<-(as.numeric(elsal_ad$`Admissions Number`)/as.numeric(elsal_ad$`Population Size 15+`))*100000 # Adjusted true admissions


#############################################################################################

############ Intialize values below for Guatemala ##################
#################################################################

country = "Guatemala"

guat <- subset(prison_data, Country == "Guatemala")
guat  <- replace(guat , guat  =='NA', NA) # fix format from excel to r
guat_ip <- subset(guat, select=c('Year', 'Adjusted Incarceration Prevalence'))
guat_ip <- guat_ip[complete.cases(guat_ip), ] # Table with True incarceration prevalence data with corresponding years

guat_ad <- subset(guat,select=c('Year', 'Admissions Number', 'Population Size 15+'))
guat_ad <- guat_ad[complete.cases(guat_ad), ] # Table with True admissions number data with corresponding years


years_ip<-guat_ip$Year - 1990 # years since 1990
ip_known<-guat_ip$`Adjusted Incarceration Prevalence` # Adjusted true incarceration prevalence
ip_known<-as.numeric(ip_known)

years_ad<-guat_ad$Year - 1990 # years since 1990
ad_known<-(as.numeric(guat_ad$`Admissions Number`)/as.numeric(guat_ad$`Population Size 15+`))*100000 # Adjusted true admissions



