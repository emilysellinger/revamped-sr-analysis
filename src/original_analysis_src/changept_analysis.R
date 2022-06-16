

# Collect regime length --------------------------------------------------------------------------------
env_change_pt <- tibble(
  stock_name = "",
  regime_length = numeric())

# will first calculate for strictly environmentally driven stocks
for(x in env_driven_stocks$stock_name)
{
  row <- which(use_stocks$stock_name == x)
  
  stock <- tibble(year = takers_rec[,1],
                  recruits = takers_rec[,x])
  
  # remove model run in time from changepoint
  min_year <- pull(use_stocks[row, "min_year"])
  max_year <- pull(use_stocks[row, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year) %>%
    mutate(recruits = replace(recruits, recruits == 0, 1))
  
  # Fit regime model
  fitPelt	<-cpt.meanvar(log(stock$recruits),method="PELT",test.stat="Normal",penalty="AIC",minseglen=6)
  changes	<- fitPelt@cpts
  
  # calculate regime length
  for(y in 1:length(changes))
  {
    if(y==1)
      ind1	<- 1
    if(y>1)
      ind1	<-changes[y-1]+1
    ind2	<-changes[y]
    regime_length	<-length(stock$recruits[ind1:ind2])
    
    env_change_pt <- env_change_pt %>%
      add_row(stock_name = x,
              regime_length = regime_length)
  }
}

# will now add the 28 stocks where we can't determine if they are environmentally driven or spbio driven (or both)
for(x in edge_stocks$stock_name)
{
  row <- which(use_stocks$stock_name == x)
  
  stock <- tibble(year = takers_rec[,1],
                  recruits = takers_rec[,x])
  
  # remove model run in time from changepoint
  min_year <- pull(use_stocks[row, "min_year"])
  max_year <- pull(use_stocks[row, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year) %>%
    mutate(recruits = replace(recruits, recruits == 0, 1))
  
  # Fit regime model
  fitPelt	<-cpt.meanvar(log(stock$recruits),method="PELT",test.stat="Normal",penalty="AIC",minseglen=6)
  changes	<- fitPelt@cpts
  
  # calculate regime length
  for(y in 1:length(changes))
  {
    if(y==1)
      ind1	<- 1
    if(y>1)
      ind1	<-changes[y-1]+1
    ind2	<-changes[y]
    regime_length	<-length(stock$recruits[ind1:ind2])
    
    env_change_pt <- env_change_pt %>%
      add_row(stock_name = x,
              regime_length = regime_length)
  }
}

# need to remove stocks that don't have any regime changes during the observed series
counts <- env_change_pt %>%
  count(stock_name)


for(x in 1:nrow(counts)){
  y <- pull(counts[x,1])
  nums <- pull(counts[x,2])
  
  
  if(nums < 2){
    env_change_pt <- env_change_pt %>%
      filter(stock_name != y)
  }
}


# want to know the number of environmentally driven stocks with a regime change in the time series
counts %>%
  filter(n > 1)
# 329 stocks have regime changes