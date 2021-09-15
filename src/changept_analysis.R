
# need to save breakpoint information to a new dataframe for analysis later

# Breakpoint fit ---------------------------------------------
stock_change_pts <- tibble(stock_name = "",
                           change_pt = numeric())
pdf("results/recruitment_change_points.pdf") 
for(x in stock_model_fits$stock_name)
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
  totMean	<-rep(0,nrow(stock))
  totSD	<-rep(0,nrow(stock))
  
  # save change points to tibble
  stock_change_pts <- stock_change_pts %>%
    add_row(stock_name = x,
            change_pt = changes)
  
  # plot regime data
  for(y in 1:length(changes))
  {
    if(y==1)
      ind1	<-1
    if(y>1)
      ind1	<-changes[y-1]+1
    ind2	<-changes[y]
    data	<-stock$recruits[ind1:ind2]
    
    regMean	<-mean(log(data))
    regSD	<-sd(log(data))
    period	<-seq(ind1,ind2)
    
    totMean[period]	<-regMean
    totSD[period]		<-regSD
    
    data <- tibble(year = stock$year,
                   log_reg_mean = totMean,
                   log_reg_sd = totSD)
  }
  print(ggplot(data = stock, aes(x = year, y = recruits)) +
    geom_line(size = 1) +
    geom_line(data = data, aes(x = year, y = exp(totMean)), colour = "#E31A1C", linetype = "dashed") + 
    labs(title = x))
}
dev.off()


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
# 206 stocks have regime changes