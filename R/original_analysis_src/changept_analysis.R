# Collect regime length --------------------------------------------------------------------------------
env_change_pt2 <- tibble(
  stock_name = "",
  regime_length = numeric())

# will first calculate for strictly environmentally driven stocks
for(x in env_driven_stocks$stock_name){
  # get stock and recruitment data
  stock <- retrieve_sr_data(x)
  
  # Fit regime model
  fitPelt	<-cpt.mean(log(stock$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                     pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
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
    
    env_change_pt2 <- env_change_pt2 %>%
      add_row(stock_name = x,
              regime_length = regime_length)
  }
}

# will now add the 28 stocks where we can't determine if they are environmentally driven or spbio driven (or both)
for(x in edge_stocks$stock_name){
  # get stock and recruitment data
  stock <- retrieve_sr_data(x)
  
  # Fit regime model
  fitPelt	<-cpt.mean(log(stock$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                     pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
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
    
    env_change_pt2 <- env_change_pt2 %>%
      add_row(stock_name = x,
              regime_length = regime_length)
  }
}

# Want to know how many stocks have a recruitment regime shift
counts2 <- env_change_pt2 %>%
  count(stock_name) %>% 
  rename(nregimes = n) %>%  # number of regimes in time series
  mutate(nshifts = nregimes - 1) # number of times regime shift (1 regime = 0 shifts)


counts2 %>% filter(nshifts > 0)
# 163 stocks have regime changes when only mean changes


# Write to CSV ------------------------------------------------------------
# will write this data to a csv file because it will be used to compare methods
write_csv(env_change_pt2, here("results/original_analysis/csv_files", "env_change_pt_AICC.csv"))

env_change_pt2 <- read_csv(here("results/original_analysis/csv_files/env_change_pt_AICC.csv"))
