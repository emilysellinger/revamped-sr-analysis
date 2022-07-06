# Collect regime length --------------------------------------------------------------------------------
env_change_pt <- tibble(
  stock_name = "",
  regime_length = numeric())

# will first calculate for strictly environmentally driven stocks
for(x in env_driven_stocks$stock_name){
  # get stock and recruitment data
  stock <- retrieve_sr_data(x)
  
  # Fit regime model
  fitPelt	<-cpt.mean(log(stock$recruits),method="PELT",test.stat="Normal",penalty="AIC",minseglen=6)
  changes	<- fitPelt@cpts
  #print(changes)
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
for(x in edge_stocks$stock_name){
  # get stock and recruitment data
  stock <- retrieve_sr_data(x)
  
  # Fit regime model
  fitPelt	<-cpt.mean(log(stock$recruits),method="PELT",test.stat="Normal",penalty="AIC",minseglen=6)
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

# Want to know how many stocks have a recruitment regime shift
counts <- env_change_pt %>%
  count(stock_name) %>% 
  rename(nregimes = n) %>%  # number of regimes in time series
  mutate(nshifts = nregimes - 1) # number of times regime shift (1 regime = 0 shifts)


counts %>% filter(nshifts > 0)
# 172 stocks have regime changes

# Write to CSV ------------------------------------------------------------
# will write this data to a csv file because it will be used to compare methods
write_csv(env_change_pt, here("results/original_analysis/csv_files", "env_change_pt.csv"))

env_change_pt <- read_csv(here("results/original_analysis/csv_files/env_change_pt.csv"))



