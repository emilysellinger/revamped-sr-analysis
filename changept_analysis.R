
# need to save breakpoint information to a new dataframe for analysis later

# Breakpoint fit ---------------------------------------------
stock_change_pts <- tibble(stock_name = "",
                           change_pt = numeric())
pdf("change_point_graphs.pdf") 
for(x in use_stocks$stock_name)
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
  fitPelt	<-cpt.meanvar(log(stock$recruits),method="PELT",test.stat="Normal",penalty="AIC",minseglen=6,pen.value=0.05)
  
  
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

