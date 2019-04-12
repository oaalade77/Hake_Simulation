rm(list=ls())
ls()

#########################################################
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
plot(x.surv.normal, surv.normal)
# See the mode
x.surv.normal[surv.normal==max(surv.normal)]


# Create normal distribution for the exploitation index (relative F)
exploit.bounds <- c(0.1*final.mov.avgs$'Exploit.index',1.9*final.mov.avgs$'Exploit.index')  
x.exploit.normal <- seq(from=exploit.bounds[1],to=exploit.bounds[2],length=2000)   
exploit.normal <- dnorm(x.exploit.normal, mean=final.mov.avgs$'Exploit.index', sd=sqrt(final.mov.avgs$'Exploit.var'))
windows()
plot(x.exploit.normal, exploit.normal)
# See the mode
x.exploit.normal[exploit.normal==max(exploit.normal)]




