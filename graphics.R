# Graphs for project report
pdf("final_rec_sb.pdf")
for(x in filtered_stocks$stock_name){
  row <- which(filtered_stocks$stock_name == x)
  
  # pull s-r model parameters
  r_a <- as.numeric(filtered_stocks[row, "ricker_a"])
  r_b <- as.numeric(filtered_stocks[row, "ricker_b"])
  bh_a <- as.numeric(filtered_stocks[row, "bevholt_a"])
  bh_b <- as.numeric(filtered_stocks[row, "bevholt_b"])
  
  # create tibble with s-r data
  stock <- tibble(year = takers_rec[,1],
                  recruits = takers_rec[, x],
                  sb = takers_ssb[, x])
  
  # remove model run in time
  row2 <- which(use_stocks$stock_name == x)
  min_year <- pull(use_stocks[row2, "min_year"])
  max_year <- pull(use_stocks[row2, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year) %>%
    mutate(recruits = replace(recruits, recruits == 0, 1))
  
  # Fit regime model
  fitPelt	<-cpt.mean(log(stock$recruits),method="PELT",test.stat="Normal",penalty="AIC",minseglen=6,pen.value=0.05)
  
  # save change point locations for regimes
  changes	<- fitPelt@cpts
  
  # create tibble for recruitment regime plots
  rectangle_data <- tibble(xmin = numeric(),
                           xmax = numeric(),
                           ymin = numeric(),
                           ymax = numeric(),
                           color_factor = numeric())
  z <- 1
  regime_colors <- rep(0, nrow(stock))
  
  # save regime mean/sd data
  for(y in 1:length(changes))
  {
    if(y==1)
      ind1	<-1
    if(y>1)
      ind1	<-changes[y-1]+1
    ind2	<-changes[y]
    data	<-stock$recruits[ind1:ind2] # retrieve recruit data from each regime
    
    regMean	<-mean(data) # calculate regime average recruitment
    regSD	<-sd(data) # calculate regime standard deviation
    
    period	<-seq(ind1,ind2)
    regime_colors[period] <- z # number each regime, for plotting
    
    xmin <- stock$year[ind1]
    xmax <- stock$year[ind2]
    ymin <- regMean - regSD
    ymax <- regMean + regSD
    
    rectangle_data <- rectangle_data %>%
      add_row(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color_factor = z)
    z <- z + 1
  }
  
  # simulate ricker and beverton holt data
  max_sb <- max(stock$sb, na.rm = TRUE)
  min_sb <- min(stock$sb, na.rm = TRUE)
  sb_values <- seq(min_sb, max_sb, length.out = 50)
  rec_pred_r <- r_a*sb_values*exp(-r_b*sb_values)
  rec_pred_bh <- (bh_a*sb_values)/(1 + (bh_b*sb_values))
  
  # create tibble with preditions
  preds <- tibble(sb = sb_values,
                  r_preds = rec_pred_r,
                  bh_preds = rec_pred_bh)
  preds <- preds %>%
    select(sb, r_preds, bh_preds) %>%
    gather(key = "model", value = "rec_pred", -sb) # put in a more convienent plotting format
  
  # Make string with common name and stock location
  row3 <- which(stock_info$stockid == x)
  common_name <- as.character(stock_info[row3, "commonname"])
  region <- paste("Region:", as.character(stock_info[row3, "region"]))
  stock_id <- paste("Stock ID:", as.character(stock_info[row3, "stockid"]))
  
  # recruitment time series plot
  rec_ts_plot <- ggplot(data = stock, aes(x = year, y = recruits)) +
    geom_line(size = 1) +
    geom_rect(data = rectangle_data, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin,
                                                              ymax = ymax, fill = as.factor(color_factor)), alpha = 0.3) +
    guides(fill = FALSE) +
    labs(x = "Year", y = "Recruits", title = common_name, subtitle = paste(region, stock_id, sep = "\n")) +
    theme_minimal()
  
  # add regime factor to spawning biomass recruitment data
  stock <- stock %>%
    add_column(color_factor = as.factor(regime_colors))
  
  # retrive ricker and bevholt model weights
  min_model <- pull(filtered_stocks[row, "min_model"])
  if(min_model == "bevholt"){
    ricker_lik <- formatC((1 - pull(filtered_stocks[row, "rel_likelihood"])), digits = 3)
    bev_lik <- formatC(pull(filtered_stocks[row, "rel_likelihood"]), digits = 3)
  }else{
    ricker_lik <- formatC(pull(filtered_stocks[row, "rel_likelihood"]), digits = 3)
    bev_lik <- formatC((1 - pull(filtered_stocks[row, "rel_likelihood"])), digits = 3)
  }
  # stock recruitment plot
  sr_plot <- ggplot(data = stock, aes(x = sb, y = recruits))+
    geom_point(aes(color = color_factor), size = 3, show.legend = FALSE) +
    geom_line(data = preds, inherit.aes = FALSE, aes(x = sb, y = rec_pred, linetype = model), size = 1) +
    labs(x = "Spawning Biomass", y = "Recruits") + 
    scale_linetype_discrete(labels = c(paste("Ricker", ricker_lik), paste("BevHolt", bev_lik))) +
    theme_minimal() +
    theme(legend.position = c(0.1, 0.9), legend.background = element_rect(fill = FALSE, color = FALSE),
          legend.title = element_blank())
  
  
  # plot both graphs
  print(grid.arrange(rec_ts_plot, sr_plot, nrow = 2))
  
}
dev.off()
