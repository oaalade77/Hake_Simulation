rm(list=ls())
ls()

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
data.fname <- c('Red_Hake_Northern_Spring_Arithmetic_kg_tow.csv') 

nyr.mov.avg <- 3

# Read in original data and calculate exploitation index
orig.data <- read.csv(file.path(data.dir,data.fname))
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

# Create normal distribution for the survey
surv.bounds <- c(0.1*final.mov.avgs$'Survey.kg.tow',1.9*final.mov.avgs$'Survey.kg.tow')   
x.surv.normal <- seq(from=surv.bounds[1],to=surv.bounds[2],length=2000)   
surv.normal <- dnorm(x.surv.normal,mean=final.mov.avgs$'Survey.kg.tow',sd=sqrt(final.mov.avgs$'Survey.var'))
windows()
plot(x.surv.normal, surv.normal, main='Normal distribution', ylab='Frequency', xlab='Survey (kg/tow)')
# See the mode
x.surv.normal[surv.normal==max(surv.normal)]


### Exploitation index: Two options
# 1) Create normal distribution from 3-year moving average
# 2) Use output from AIM

### Option 1:
# Create normal distribution for the exploitation index (relative F)
exploit.bounds <- c(0.1*final.mov.avgs$'Exploit.index',1.9*final.mov.avgs$'Exploit.index')  
x.exploit.normal <- seq(from=exploit.bounds[1],to=exploit.bounds[2],length=2000)   
exploit.normal <- dnorm(x.exploit.normal, mean=final.mov.avgs$'Exploit.index', sd=sqrt(final.mov.avgs$'Exploit.var'))
windows()
plot(x.exploit.normal, exploit.normal, main='Normal distribution',ylab='Frequency',xlab='Exploitation index')
# See the mode
x.exploit.normal[exploit.normal==max(exploit.normal)]


### Option 2:



# Create the cdfs for the 1) survey and 2) exploitation index
surv.cdf <- pnorm(x.surv.normal,mean=final.mov.avgs$'Survey.kg.tow',sd=sqrt(final.mov.avgs$'Survey.var'))
exploit.cdf <- pnorm(x.exploit.normal, mean=final.mov.avgs$'Exploit.index', sd=sqrt(final.mov.avgs$'Exploit.var'))


### Test with survey values from excel
# pop.size.excel <- c(2.333370743,2.618674232,2.742172609,2.826851424,2.893118268,2.948500722,2.996659173,3.03966588,3.078815039,3.114972928,3.148750303,3.180595637,3.21084929,3.239776789,3.267590254,3.294462707,3.320537966,3.345937666,3.370766387,3.395115469,3.419065938,3.44269078,3.466056764,3.489225936,3.512256878,3.5352058,3.558127533,3.581076456,3.604107397,3.627276569,3.650642554,3.674267395,3.698217864,3.722566946,3.747395667,3.772795368,3.798870626,3.82574308,3.853556544,3.882484044,3.912737696,3.944583031,3.978360405,4.014518294,4.053667454,4.096674161,4.144832612,4.200215065,4.266481909,4.351160724,4.474659101,4.75996259)  
# cdf.excel <- pnorm(pop.size.excel,mean=final.mov.avgs$'Survey.kg.tow',sd=sqrt(final.mov.avgs$'Survey.var'))  
### Test to manually create the cdf by scaling the normal distribution and then taking the cumsum
# sc.surv.normal <- surv.normal/sum(surv.normal)
### Plot to compare the three methods
# windows()
# plot(pop.size.excel,cdf.excel, col='red',cex=1.0)
# lines(x.surv.normal, surv.cdf,type='p',pch=4,col='blue',cex=0.2)
# lines(x.surv.normal, cumsum(sc.surv.normal), type='p',pch=3,col='black',cex=0.2)





# Calculate joint probability distribution
### Need to confirm what survey probabilities to use



