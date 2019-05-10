

#########################################################
# Testing with Northern Red Hake

# The following code:
#  1. Create a normal distribution for the survey data (running means) using the final running mean and variance
#  2. Create a normal distribution for the exploitation index using the final running mean and variance of the last three years of the raw exploitation index (not running means)
##### Ask larry: should this variance be of the running means or of the raw exploitation indices????? (He used the raw indices, but I wonder why)


# TO DO:
#  3. OFL Distrib worksheet: Calculate joint probabilities of OFL and actual OFL values
#  4. OFL Summary: Bin OFL

#########################################################


library(here)

rm(list=ls())
ls()


data.dir <- here("data.dir")
surv.fname <- c('Red_Hake_Northern_Spring_Arithmetic_kg_tow.csv') 
aim.fname <- c('Red_Hake_Northern_AIM_Bootstrapped_Output_RelF.csv')

# Number of years used to calculate the moving average
nyr.mov.avg <- 3

# Number of bins used to develop the survey and relative F distributions 
surv.nbins <- 51
exploit.nbins <- 100
ofl.nbins <- 100


# Read in original data and calculate exploitation index
orig.data <- read.csv(file.path(data.dir,surv.fname))
rownames(orig.data) <- orig.data$Year
orig.data$Year <- NULL
orig.data$'Exploit.index' <- orig.data$'Total.catch.tmt'/orig.data$'Survey.kg.tow'  ##### Talk to larry about this  
orig.data$'Survey.stdev'  <- sqrt(orig.data$'Survey.var')

# Calculate moving averages
calc.mov.avg <- function(x,nyr=nyr.mov.avg) {  filter(x, rep((1/nyr),nyr), sides=1, method='convolution')  }  
mov.avg.data <- data.frame( apply(orig.data[,c('Survey.kg.tow','Exploit.index')], 2, calc.mov.avg) )
  rownames(mov.avg.data) <- rownames(orig.data)

# Add survey variance calculation
calc.survey.var <- function(x,nyr=nyr.mov.avg) {  filter(x, rep(1,nyr), sides=1, method='convolution') * (1/nyr.mov.avg)^2  }
# =((1/3)^2*(I30+I31+I32))
mov.avg.data$'Survey.var' <- calc.survey.var(orig.data$'Survey.var')

# Add variance in exploitation index
orig.tmp <- orig.data[is.na(orig.data$Exploit.index)==FALSE,]
yr.tmp <- tail(rownames(orig.tmp),1)
mov.avg.data[yr.tmp,'Exploit.var'] <- var(tail(orig.tmp$Exploit.index, nyr.mov.avg))  
rm(orig.tmp, yr.tmp)

# Final moving average for each time series
final.mov.avgs <- as.data.frame(t( apply( mov.avg.data, 2, function(x){tail(na.omit(x),1)} ) ))
  rownames(final.mov.avgs) <- NULL

# Test to ensure filter function is used correctly
# final.yrs.orig.data <- data.frame(apply(orig.data, 2, function(x){ tail(na.omit(x),nyr.mov.avg) } ))
# final.mov.avg.test <- apply(final.yrs.orig.data, 2, function(x){ mean(x) } )

# Create normal distribution and cdf for the survey
surv.bounds <- c(0.1*final.mov.avgs$'Survey.kg.tow',1.9*final.mov.avgs$'Survey.kg.tow')   
x.surv <- seq(from=surv.bounds[1],to=surv.bounds[2],length=surv.nbins)   
surv.normal <- dnorm(x.surv,mean=final.mov.avgs$'Survey.kg.tow',sd=sqrt(final.mov.avgs$'Survey.var'))
windows()
plot(x.surv, surv.normal, main='Normal distribution of recent 3-year moving average', ylab='Frequency', xlab='Survey (kg/tow)')
# See the mode
x.surv[surv.normal==max(surv.normal)]
# Create the cdfs for the survey
surv.cdf <- pnorm(x.surv,mean=final.mov.avgs$'Survey.kg.tow',sd=sqrt(final.mov.avgs$'Survey.var'))
surv.prob <- rep(NA,length(surv.cdf))
  surv.prob[1] <- surv.cdf[1]
  surv.prob[2:length(surv.cdf)] <- surv.cdf[2:length(surv.cdf)] - surv.cdf[1:(length(surv.cdf)-1)]

# ### Test with survey values from excel
# pop.size.excel <- c(2.333370743,2.618674232,2.742172609,2.826851424,2.893118268,2.948500722,2.996659173,3.03966588,3.078815039,3.114972928,3.148750303,3.180595637,3.21084929,3.239776789,3.267590254,3.294462707,3.320537966,3.345937666,3.370766387,3.395115469,3.419065938,3.44269078,3.466056764,3.489225936,3.512256878,3.5352058,3.558127533,3.581076456,3.604107397,3.627276569,3.650642554,3.674267395,3.698217864,3.722566946,3.747395667,3.772795368,3.798870626,3.82574308,3.853556544,3.882484044,3.912737696,3.944583031,3.978360405,4.014518294,4.053667454,4.096674161,4.144832612,4.200215065,4.266481909,4.351160724,4.474659101,4.75996259)  
# cdf.excel <- pnorm(pop.size.excel,mean=final.mov.avgs$'Survey.kg.tow',sd=sqrt(final.mov.avgs$'Survey.var'))  
# ### Test to manually create the cdf by scaling the normal distribution and then taking the cumsum
# sc.surv.normal <- surv.normal/sum(surv.normal)
# ### Plot to compare the three methods
# windows()
# plot(pop.size.excel,cdf.excel, col='red',cex=1.0)
# lines(x.surv, surv.cdf,type='p',pch=4,col='blue',cex=0.5)
# lines(x.surv, cumsum(sc.surv.normal), type='p',pch=3,col='black',cex=0.5)


### Exploitation index: Two options
# 1) Use output from AIM (red hake)
# 2) Use mean from a selected time period (silver hake used ~1973-1982)


### Option 1:
# Probabilities and cumululative probabilities inputted from bootstrapped AIM output
aim.data <- read.csv(file.path(data.dir,aim.fname))
  colnames(aim.data) <- c('ID','RelF.mt.kg','RelF.kt.kg','Prob','Cum.Prob')
windows()
plot(aim.data$RelF.kt.kg, aim.data$Prob, main='AIM Probability distribution (1982-2008)',ylab='Probability',xlab='Exploitation index')
sum(aim.data$Prob)
  
# ### Extra:
exploit.bounds <- c(0.1*final.mov.avgs$'Exploit.index',1.9*final.mov.avgs$'Exploit.index')  
x.exploit <- seq(from=exploit.bounds[1],to=exploit.bounds[2],length=exploit.nbins)   
# # Create normal distribution for the most recent 3-year average of the exploitation index (relative F)
# exploit.normal <- dnorm(x.exploit, mean=final.mov.avgs$'Exploit.index', sd=sqrt(final.mov.avgs$'Exploit.var'))
# windows()
# plot(x.exploit, exploit.normal, main='Normal distribution of recent 3-year moving average',ylab='Frequency',xlab='Exploitation index')
# # See the mode
# x.exploit[exploit.normal==max(exploit.normal)]
# # Create the cdf for the exploitation index
# exploit.cdf <- pnorm(x.exploit, mean=final.mov.avgs$'Exploit.index', sd=sqrt(final.mov.avgs$'Exploit.var'))


### Option 2: time-series average
##### STILL TO DO
####################################


  
### Use the vector of survey values and vector of relative F's to generate matrix of possible catches
# If using the AIM data (relative F from the bootstrapped relative F estimsates from 1982-2008)
ofl.mat <- as.matrix(aim.data$'RelF.kt.kg') %*% t(as.matrix(x.surv))  
dim(ofl.mat)


### Use associated probabilities for relative F and survey to generate matrix of joint probabilities associated with each possible catch
ofl.probs <- as.matrix(aim.data$'Prob') %*% t(as.matrix(surv.prob))
  dim(ofl.probs)
  
  
### OFL Summary: bin something
min.ofl <- min(ofl.mat)
max.ofl <- max(ofl.mat)
ofl.stepsize <- (max.ofl-min.ofl)/ofl.nbins


#### NOW NEED TO BIN THE OFL VALUES AS IN NORTH_OFL_SUMMARY

 

