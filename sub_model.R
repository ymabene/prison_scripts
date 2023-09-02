library(deSolve)
library(ggplot2)
library(data.table)
library(SciViews)

## sub_prison.model: Function used to calculate recidivism probability error
## Input parameters:
      ## t: time
      ## x: model compartments (R, E, Y)
      ## params: specified prison model rates (iR, IE, iN, r)
## Output parameters:
      ## List with the following parameters:
      ## dRdt, dEdt, dYdt (Number of people in model compartments)


sub_prison.model <- function(t, x, params) 
{
  with(as.list(c(x, params)),{
    
    
    ##########################################################
    #          MODEL EQUATIONS
    ##########################################################
    
    
    # released
    dRdt <- -iR*R - a*R  
    
    # reincarcerated
    dYdt <- iR*R + iE*E   
    
    # ex prisoners
    dEdt <- a*R -iE*E
    
    
    
    list(c(dRdt, dYdt, dEdt))
  })
}



# time=500
# 
# timeunit<-seq(0,time,1) # years
# 
# xstart <- c( R=100000, Y=0, E=0)
# 
# # US
# params <- c(iR=0.244,
#             iE=0.0006,
#             a=0.2)
# 
# 
# 
# 
# output <- ode(
#   func=sub_prison.model,
#   y=xstart,
#   times=timeunit,
#   parms=params
# )


# sub_output_dt <- data.table(output)
# 
# sub_output_melt <- melt(sub_output_dt, id.vars = 'time', variable.name = 'Population', value.name = 'Count')
# sub_output_melt$Population <- factor(sub_output_melt$Population, levels = c('R','Y','E'),
#                                  labels = c('Released', 'Reincarcerated', 'Ex-Prisoner'))
# ggplot(sub_output_melt, aes(x=time, y=Count, color=Population)) + geom_line() + theme_bw() +
#   scale_y_log10()







