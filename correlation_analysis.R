# correlation analysis to determine what is driving recruitment

# Dome-shaped stocks ------------------------------------------------------------------------------------------------
# going to add extra columns for rho estimate and p-value from spearman's correlation
dome_stocks <- dome_stocks %>%
  add_column(cor_coef = 1,
             cor_pval = 1)

for(x in dome_stocks$stock_name){
  row <- which(dome_stocks$stock_name == x)
  row2 <- which(use_stocks$stock_name == x)
  
  stock <- tibble(
    year = takers_rec[,1],
    recruits = takers_rec[,x],
    sb = takers_ssb[,x])
  
  # remove model run in time
  min_year <- pull(use_stocks[row2, "min_year"])
  max_year <- pull(use_stocks[row2, "max_year"])
  
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
  dome_stocks[row, "cor_coef"] <- corrr$estimate
  dome_stocks[row, "cor_pval"] <- corrr$p.value
}

# BevHolt stocks -----------------------------------------------------------------------------------------
# add extra columns for correlation results
monotonic_stocks <- monotonic_stocks %>%
  add_column(zero_lag = 1,
             neg_lag = 1)

for(x in monotonic_stocks$stock_name){
  row <- which(monotonic_stocks$stock_name == x)
  row2 <- which(use_stocks$stock_name == x)
  
  stock <- tibble(
    year = takers_rec[,1],
    recruits = takers_rec[,x],
    sb = takers_ssb[,x])
  
  # remove model run in time
  min_year <- pull(use_stocks[row2, "min_year"])
  max_year <- pull(use_stocks[row2, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  # find cross correlation values
  stock_ccf <- ccf(stock$sb, stock$recruits, plot = FALSE)
  stock_ccf_df <- data.frame(lag = stock_ccf$lag,
                             acf = stock_ccf$acf)
  # save zero-lagged correlation value
  zero_lag <- stock_ccf_df[11,2]
  monotonic_stocks[row, "zero_lag"] <- zero_lag
  
  # determine if negative lags are <= zero lag value (if yes, spbio driven, if no, either env and/or spbio)
  for(x in 1:10){
    if(stock_ccf_df[x,2] <= zero_lag){
      monotonic_stocks[row, "neg_lag"] <- 1
    }else{
      monotonic_stocks[row, "neg_lag"] <- 0
    }
  }
}

# Combine SB driven stocks -----------------------------------------------------------------------------------------
# for the moment, I'm only going to consider ricker stocks that have a p-value of 0.05 and Beverton Holt stocks that
# have a significant zero lag correlation & negative lagged correlations less than or equal to the zero lag correlation 
sb_driven_stocks <- tibble()
sb_driven_stocks <- sb_driven_stocks %>%
  add_column(stock_name = "",
             curve_shape = "")

# check dome-shaped stocks first
for(x in 1:nrow(dome_stocks)){
  if(dome_stocks[x,"cor_pval"] <= 0.05){
    sb_driven_stocks <- sb_driven_stocks %>%
      add_row(stock_name = pull(dome_stocks[x,1]),
              curve_shape = "dome")
  }
}

# check monotonic stocks
for(x in 1:nrow(monotonic_stocks)){
  if(monotonic_stocks[x,"zero_lag"] >= 0.5 && monotonic_stocks[x,"neg_lag"] == 1){
    sb_driven_stocks <- sb_driven_stocks %>%
      add_row(stock_name = pull(monotonic_stocks[x,1]),
              curve_shape = "monotonic")
  }
}
# Combine environmentally driven stocks ----------------------------------------------------------------------------
env_driven_stocks <- tibble()
env_driven_stocks <- env_driven_stocks %>%
  add_column(stock_name = "",
             curve_shape = "")

# find all dome shaped stocks with a p-value > 0.05
for(x in 1:nrow(dome_stocks)){
  if(dome_stocks[x,"cor_pval"] > 0.05){
    env_driven_stocks <- env_driven_stocks %>%
      add_row(stock_name = pull(dome_stocks[x,1]),
              curve_shape = "dome")
  }
}

# find all monotonic stocks with a non-significant zero lagged correlation
for(x in 1:nrow(monotonic_stocks)){
  if(monotonic_stocks[x,"zero_lag"] < 0.5){
    env_driven_stocks <- env_driven_stocks %>%
      add_row(stock_name = pull(monotonic_stocks[x,1]),
              curve_shape = "monotonic")
  }
}

# Edge Cases -----------------------------------------------------------------------------------------------
edge_stocks <- tibble()
edge_stocks <- edge_stocks %>%
  add_column(stock_name = "",
             curve_shape = "")

for(x in 1:nrow(monotonic_stocks)){
  if(monotonic_stocks[x,"zero_lag"] > 0.5 && monotonic_stocks[x,"neg_lag"] == 0){
    edge_stocks <- edge_stocks %>%
      add_row(stock_name = pull(monotonic_stocks[x,1]), curve_shape = "monotonic")
  }
}