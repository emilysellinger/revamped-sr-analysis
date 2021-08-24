# correlation analysis to determine what is driving recruitment

# Ricker stocks ------------------------------------------------------------------------------------------------
# going to add extra columns for rho estimate and p-value from spearman's correlation
ricker_stocks <- ricker_stocks %>%
  add_column(cor_coef = 1,
             cor_pval = 1)

for(x in ricker_stocks$stock_name){
  row <- which(ricker_stocks$stock_name == x)
  
  stock <- tibble(
    year = takers_rec[,1],
    recruits = takers_rec[,x],
    sb = takers_ssb[,x])
  
  # remove model run in time
  min_year <- pull(ricker_stocks[row, "min_year"])
  max_year <- pull(ricker_stocks[row, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  # find max SpBio
  max_sb <- max(stock[, "sb"], na.rm = TRUE)
  
  # find S-R pairs greater than 50% of max & remove from dataset
  for(y in 1:nrow(stock)){
    if(!is.na(stock[y,"sb"]) && stock[y,"sb"] > 0.5*max_sb){
      stock<- stock[-c(y),]
    }
  }
  
  # find spearman's correlation
  corrr <- cor.test(stock$recruits, stock$sb, method = 'spearman', exact = FALSE)
  
  # save rho estimate and p-value to data frame
  ricker_stocks[row, "cor_coef"] <- corrr$estimate
  ricker_stocks[row, "cor_pval"] <- corrr$p.value
}

# BevHolt stocks -----------------------------------------------------------------------------------------
# add extra columns for correlation results
bevholt_stocks <- bevholt_stocks %>%
  add_column(zero_lag = 1,
             neg_lag = 1)

for(x in bevholt_stocks$stock_name){
  row <- which(bevholt_stocks$stock_name == x)
  
  stock <- tibble(
    year = takers_rec[,1],
    recruits = takers_rec[,x],
    sb = takers_ssb[,x])
  
  # remove model run in time
  min_year <- pull(bevholt_stocks[row, "min_year"])
  max_year <- pull(bevholt_stocks[row, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  # find cross correlation values
  stock_ccf <- ccf(stock$sb, stock$recruits, plot = FALSE)
  stock_ccf_df <- data.frame(lag = stock_ccf$lag,
                             acf = stock_ccf$acf)
  # save zero-lagged correlation value
  zero_lag <- stock_ccf_df[11,2]
  bevholt_stocks[row, "zero_lag"] <- zero_lag
  
  # determine if negative lags are <= zero lag value (if yes, spbio driven, if no, either env and/or spbio)
  for(x in 1:10){
    if(stock_ccf_df[x,2] <= zero_lag){
      bevholt_stocks[row, "neg_lag"] <- 1
    }else{
      bevholt_stocks[row, "neg_lag"] <- 0
    }
  }
}

# Combine SB driven stocks -----------------------------------------------------------------------------------------
# for the moment, I'm only going to consider ricker stocks that have a p-value of 0.05 and Beverton Holt stocks that
# have a significant zero lag correlation & negative lagged correlations less than or equal to the zero lag correlation 
sb_driven_stocks <- tibble()
sb_driven_stocks <- sb_driven_stocks %>%
  add_column(stock_name = "test")

# check Ricker stocks first
for(x in 1:nrow(ricker_stocks)){
  if(ricker_stocks[x,"cor_pval"] <= 0.05){
    sb_driven_stocks <- sb_driven_stocks %>%
      add_row(stock_name = pull(ricker_stocks[x,1]))
  }
}

# check BevHolt stocks
for(x in 1:nrow(bevholt_stocks)){
  if(bevholt_stocks[x,"zero_lag"] >= 0.5 && bevholt_stocks[x,"neg_lag"] == 1){
    sb_driven_stocks <- sb_driven_stocks %>%
      add_row(stock_name = pull(bevholt_stocks[x,1]))
  }
}
# Combine environmentally driven stocks ----------------------------------------------------------------------------
env_driven_stocks <- tibble()
env_driven_stocks <- env_driven_stocks %>%
  add_column(stock_name = "test")

# find all ricker stocks with a p-value > 0.05
for(x in 1:nrow(ricker_stocks)){
  if(ricker_stocks[x,"cor_pval"] > 0.05){
    env_driven_stocks <- env_driven_stocks %>%
      add_row(stock_name = pull(ricker_stocks[x,1]))
  }
}

# find all bevholt stocks with a non-significant zero lagged correlation
for(x in 1:nrow(bevholt_stocks)){
  if(bevholt_stocks[x,"zero_lag"] < 0.5){
    env_driven_stocks <- env_driven_stocks %>%
      add_row(stock_name = pull(bevholt_stocks[x,1]))
  }
}

# Edge Cases -----------------------------------------------------------------------------------------------
edge_stocks <- tibble()
edge_stocks <- edge_stocks %>%
  add_column(stock_name = "test")

for(x in 1:nrow(bevholt_stocks)){
  if(bevholt_stocks[x,"zero_lag"] > 0.5 && bevholt_stocks[x,"neg_lag"] == 0){
    edge_stocks <- edge_stocks %>%
      add_row(stock_name = pull(bevholt_stocks[x,1]))
  }
}