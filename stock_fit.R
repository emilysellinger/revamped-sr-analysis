# Identify deterministic S-R relationships and remove those stocks from analysis
library(nlmrt)
library(FSA)


# Define S-R models -------------------------------------------------------------------------------------
bevholt <- logR~log((a*sb)/(1 + b*sb))

ricker <- logR~log(a*sb*exp(-b*sb))

# Create usuable stock lists ----------------------------------------------------------------------------
use_stocks <- tibble()
use_stocks <- use_stocks %>%
  add_column(stock_name = "test",
             ricker_a = 1,
             ricker_b = 1,
             bevholt_a = 1,
             bevholt_b = 1,
             ricker_aicc = 1,
             bevholt_aicc = 1,
             min_year = 1,
             max_year = 1)

# Find usuable stocks --------------------------------------------------------------------------------
for(x in 2:ncol(takers_rec)){
  # combine stock data into one data frame
  stock <- data.frame(
    stock_name = colnames(takers_rec[x]),
    year = takers_rec[,1],
    recruits = takers_rec[,x],
    logR = log(takers_rec[,x]),
    sb = takers_ssb[,x])
  
  # remove NAs
  stock <- stock[complete.cases(stock),]
  
  #right now there are some stocks with 0 recruit estimates per year, that needs to be fixed in the initial filtering
  # but for the moment, I'm going to change the inf values that result from taking the log to 0s
  stock <- stock %>%
     mutate(logR = replace(logR, logR == -Inf, 1)) %>%
     mutate(recruits = replace(recruits, recruits == 0, 1)) %>%
     mutate(change = abs((lead(recruits) - recruits)/recruits*100)) %>% # going to remove years that contain model run up
     filter(change > 1)
  
  # make sure there are at least 20 years of data remaining, once removing NAs and model run in time
  if(count(stock) != 0 && count(stock) >= 20 ){
    
    # find starting values for nonlinear regression 
    ricker_starts <- srStarts(recruits~sb, data = stock, type = "Ricker", param = 1)
    bevholt_starts <- srStarts(recruits~sb, data = stock, type = "BevertonHolt", param = 1)
    
    if(ricker_starts[2] < 0){ricker_starts[2] <- 0.000001} # make sure ricker b param is positive
    if(bevholt_starts[1] < 0){bevholt_starts[1] <- 0.000001} # make sure bevholt a param is positive
    if(bevholt_starts[2] < 0){bevholt_starts[2] <- 0.000001} # make sure bevholt b param is positive
    
    # fit ricker and beverton holt models to stock
    ricker_fit <- nlxb(ricker, data = stock, start = ricker_starts)
    bevholt_fit <- nlxb(bevholt, data = stock, start = bevholt_starts)
    
    # calculate RMSE using ricker model (can change to an average of BH and Ricker if needed)
    RMSE <- sqrt(mean((ricker_fit$resid^2)))
    
    # calculate AICc for both models
    params <- 3
    n <- nrow(stock)
    ricker_aicc <- n*log(mean(ricker_fit$resid^2)) + ((2*params*(params+1)) /(n - params - 1))
    bevholt_aicc <- n*log(mean(bevholt_fit$resid^2)) + ((2*params*(params+1)) /(n - params - 1))
    
    # If RMSE is less than 0.3, the S-R relationship is a deterministic estimate (RMSE cutoff can be adjusted)
    if(RMSE > 0.3){
      use_stocks <- use_stocks %>%
        add_row(stock_name = colnames(takers_rec[x]), ricker_a = ricker_fit$coefficients[1],
              ricker_b = ricker_fit$coefficients[2], 
              bevholt_a = bevholt_fit$coefficients[1], bevholt_b = bevholt_fit$coefficients[2],
              ricker_aicc = ricker_aicc, bevholt_aicc = bevholt_aicc,
              min_year = min(stock$year), max_year = max(stock$year))
    }
  }
}

# Plot filtered data -------------------------------------------------------------------
stock_names <- c()
for(i in 1:nrow(use_stocks)){
  stock_names <- c(stock_names, use_stocks[i,1])
}


pdf("filtered_rec_sb.pdf")
for(x in stock_names){
  # get predictions for stock-recruit curve
  row <- which(use_stocks$stock_name == x)
  r_a <- as.numeric(use_stocks[row, 2])
  r_b <- as.numeric(use_stocks[row, 3])
  bh_a <- as.numeric(use_stocks[row, 4])
  bh_b <- as.numeric(use_stocks[row, 5])
  max_sb <- max(takers_ssb[,x], na.rm = TRUE)
  min_sb <- min(takers_ssb[,x], na.rm = TRUE)
  sb_values <- seq(min_sb, max_sb, length.out = 50)
  rec_pred_r <- r_a*sb_values*exp(-r_b*sb_values)
  rec_pred_bh <- (bh_a*sb_values)/(1 + (bh_b*sb_values))
  
  #set margins
  par(mfrow=c(2,1),mar=c(.1,.1,.1,.1),oma=c(4,7,4,1))
  
  # plot stock-recruitment curve w/ observations
  plot(takers_rec[,x]~takers_ssb[,x],ylim=c(0,max(takers_rec[,x],na.rm=T)),xlim=c(0,max(takers_ssb[,x],na.rm=T)),
       las=1,xaxt='n')
  lines(sb_values, rec_pred_r, col = "red") #ricker model
  lines(sb_values, rec_pred_bh, col = "blue")#bevholt model
  
  # axis argument
  axis(side=3)
  
  # plot recruitment time series
  plot(takers_rec[,x]~takers_rec[,1],type='l',xlim = c(as.numeric(use_stocks[row,6]), as.numeric(use_stocks[row,7])),
       ylim=c(0,max(takers_rec[,x],na.rm=T)),las=1)
  
  # axis labels
  mtext(outer=T,side=2,"Recruitment",line=4.2)
  mtext(outer = T, side = 3, x, line = 2.5)
  mtext(outer=T,side=3,"Spawning biomass",line=1.5)
  mtext(outer=T,side=1,"Year",line=2.5)
}
dev.off()

# Split stocks ----------------------------------------------------------------------------------------
# In Szuwalski et al. 2014, the stocks suitable for analysis were split based on minimum AIC value
# different procedures were then applied to each. The correlation analysis will be in a different script.

ricker_stocks <- use_stocks %>%
  filter(ricker_aicc < bevholt_aicc)
bevholt_stocks <- use_stocks %>%
  filter(bevholt_aicc < ricker_aicc)
