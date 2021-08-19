library(tidyverse)
library(here)
library(changepoint)
library(bcp)
library(data.table)
# need to update Cody's code with my own stocks

# Breakpoint fit ---------------------------------------------

pdf("raw_rec_reg.pdf") 
for(x in 2:ncol(use_rec_dat))
{
  recr<-scale(use_rec_dat[,x]) + 10 # this is dumb--scaled and added 10 because changepoint blows up with large numbers. fix later 
  rec_yr<-use_rec_dat[!is.na(recr),1]
  recr<-recr[!is.na(recr)]
  
  #==Fit regime model
  fitPelt	<-cpt.meanvar(recr,method="PELT",test.stat="Normal",penalty="AIC",minseglen=6,pen.value=0.05)
  
  #==look at the different functions in the changepoint library and compare the differences in the output
  #==when you use 'cpt.mean' instead of 'cpt.meanvar' or change minseglen from 6 to 10 (for example)--just play with the knobs!
  
  changes	<- fitPelt@cpts
  totMean	<-rep(0,length(recr))
  totSD	<-rep(0,length(recr))
  for(x in 1:length(changes))
  {
    if(x==1)
      ind1	<-1
    if(x>1)
      ind1	<-changes[x-1]+1
    ind2	<-changes[x]
    data	<-recr[ind1:ind2]
    
    regMean	<-mean(log(data))
    regSD	<-sd(log(data))
    period	<-seq(ind1,ind2)
    
    totMean[period]	<-regMean
    totSD[period]		<-regSD
  }
  par(mfrow=c(1,1),mar=c(4,4,1,1),oma=c(1,1,1,1))
  plot(recr~rec_yr,type='l',las=1,ylab='',xlab='Year')
  lines(exp(totMean)~rec_yr,lty=2,col=2)
  mtext(side=3,colnames(use_rec_dat[x]))
}
dev.off()

# Practice ----------------------------------------------------
stock1 <- takers_rec[,1:2]
stock1 <- cbind(stock1, takers_ssb[,2])
stock1 <- as.tibble(stock1)
stock1 <- stock1 %>%
  rename(
    recruits = "ACADREDGOMGB",
    ssb = "takers_ssb[, 2]"
  ) %>%
  drop_na() %>%
  mutate(logR = log(recruits))


bevholt_starts <- srStarts(logR~ssb, data = stock1, type = "BevertonHolt", param = 1, na.rm = TRUE)
unlist(bevholt_starts)

bevholt <- logR~log((a*ssb)/(1 + b*ssb))

bevholt_fit <- nls(bevholt, data = stock1, start = bevholt_starts)

par(mfrow=c(2,1),mar=c(.1,.1,.1,.1),oma=c(4,7,4,1))
plot(stock1$recruits~stock1$ssb,ylim=c(0,max(stock1$recruits)),xlim=c(0, max(stock1$ssb)),
     las=1,xaxt='n')
axis(side=3)
plot(stock1$recruits~stock1$year,type='l',ylim=c(0,max(stock1$recruits)),las=1)
mtext(outer=T,side=2,"Recruitment",line=4.2)
mtext(outer=T,side=3,"Spawning biomass",line=2.5)
mtext(outer=T,side=1,"Year",line=2.5)
# ----------------------------------------------------------------------------
stock2 <- as_tibble(takers_rec[,c(1,3)])
stock2 <- stock2 %>% rename(recruits = ACMACKSARG)

stock2_ssb <- as_tibble(takers_ssb[,c(1,3)])
stock2_ssb <- stock2_ssb %>% rename(ssb = ACMACKSARG)

stock2 <- stock2 %>% 
  full_join(stock2_ssb, by = "year") %>%
  mutate(logR = log(recruits)) %>%
  drop_na()
  

bevholt_starts <- srStarts(recruits~ssb, data = stock2, type = "BevertonHolt", param = 1, na.rm = TRUE)
unlist(bevholt_starts)

bevholt <- logR~log((a*ssb)/(1 + b*ssb))

bevholt_fit <- nls(bevholt, data = stock2, start = bevholt_starts)

ricker <- logR~log(a*ssb*exp(-b*ssb))
ricker_starts <- srStarts(recruits~ssb, data = stock2, type = "Ricker", param = 1, na.rm = TRUE)
unlist(ricker_starts)
ricker_fit <- nls(ricker, data = stock2, start = ricker_starts)

aic <- AIC(bevholt_fit, ricker_fit)

par(mfrow=c(2,1),mar=c(.1,.1,.1,.1),oma=c(4,7,4,1))
plot(stock2$recruits~stock2$ssb,ylim=c(0,max(stock2$recruits)),xlim=c(0, max(stock2$ssb)),
     las=1,xaxt='n')
axis(side=3)
plot(stock2$recruits~stock2$year,type='l',ylim=c(0,max(stock2$recruits)),las=1)
mtext(outer=T,side=2,"Recruitment",line=4.2)
mtext(outer=T,side=3,"Spawning biomass",line=2.5)
mtext(outer=T,side=1,"Year",line=2.5)

stock2 <- stock2 %>%
  mutate(fits = fitted.values(bevholt_fit)) %>%
  mutate(error = (logR - fits)^2)

RMSE <- sqrt(mean(stock2$error)) # need to find a good threshold value to cut off at (thinking maybe 2?)
# need to find a stock I can write some code to get rid of the second issue, when the model is running up
# so far takers_rec[,7] is a good candidate 19 too
# I think 20 is another to test

stock20 <- takers_rec[, c(1,20)]
mean(stock20$ARFLOUNDPCOAST, na.rm = TRUE) # I think the most effective way to do this would be to take
# the average of the first
# need to get rid of takers_rec[,11] -> something up with filtering method

stock7 <- takers_rec[,c(1,7)]
mean(stock7$ALSKABSAI, na.rm = TRUE)
mean(stock7[c(1:10),2], na.rm = TRUE)
