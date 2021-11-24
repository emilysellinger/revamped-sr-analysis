# Graphs for project report

# Max age histogram -------------------------------------------------------------------------------------------------
age_data <- lifespan %>%
  select(scientific_name, age)
age_data <- age_data %>%
  distinct(scientific_name, .keep_all = TRUE)

svg("results/age_hist.svg")
ggplot(age_data, aes(x = age, res = 120)) +
  geom_histogram(binwidth = 5, alpha = 0.8, color = "#253494", fill = "#253494") +
  labs(x = "Max age reported", y = "Count") + theme_minimal()
dev.off()


# regime length bar chart -------------------------------------------------
df <- env_change_pt %>% drop_na()
df$age_cat <- rep(0, length(df$age))

for(i in 1:nrow(df)){
  if(df[i, "age"] <= 10){
    df[i, "age_cat"] <- 1
  }else if(df[i, "age"] <= 20){
    df[i, "age_cat"] <- 2
  }else if(df[i, "age"] <= 40){
    df[i, "age_cat"] <- 3
  }else{
    df[i, "age_cat"] <- 4
  }
}

df2 <- df %>% group_by(age_cat) %>% summarise(mean = mean(regime_length), sd = sd(regime_length))

svg("results/mean_regime.svg")
ggplot(data = df2, aes(x = age_cat, y = mean)) +
  geom_bar(stat = "identity", fill = "#41B6C4") + 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), stat = "identity", width =.2) +
  ylim(0, 20) +
  labs(x = "age category", y = "mean regime length (yrs)") +
  theme_minimal()
dev.off()
 


# bar chart ---------------------------------------------------------------
df <- tibble(
  analysis = c("2015", "2015", "2015", "current", "current", "current"),
  group = c("Environment", "Spawning Biomass", "SB and/or Environment","Environment", "Spawning Biomass", "SB and/or Environment" ),
  value = c(61, 16, 23, 58, 12, 30),
)

svg("results/barplot.svg")
ggplot(data = df, aes(x = group, y = value, fill = analysis)) +
  geom_bar(stat = "identity", position = position_dodge()) + scale_fill_manual(values = c("#253494", "#41B6C4")) +
  coord_flip() + labs(x = "primary driver", y = "percentage of stocks") +
  theme_minimal()
dev.off()
  



# Map of stocks ----------------------------------------------------------------------------------------------------
library(sf)
library(RColorBrewer)

savepdf <- function(file, width=16, height=10)
{
  fname <- paste("results/",file,".pdf",sep="")
  pdf(fname, width=width/2.54, height=height/2.54,
      pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

fao <- read.csv(here("data", "fao_region.csv"))
fao$stock_name <- str_replace_all(fao$stock_name, "-", "\\.")

lifespan <- lifespan %>%
  left_join(fao)

lifespan %>%
  group_by(primary_FAOarea) %>%
  summarise(n = n())

# going to reassign the inland FAO regions to ocean ones 
lifespan$primary_FAOarea[lifespan$primary_FAOarea == 2] <- 67
lifespan$primary_FAOarea[lifespan$primary_FAOarea == 5] <- 61
lifespan$primary_FAOarea[lifespan$primary_FAOarea == 4] <- 61

fao_regions <- st_read("data/major_fao_region.shp")

fao_regions$num_stocks <- c(0, 29, 133, 4, 82, 21, 0, 1, 9, 2, 17, 9, 34, 1, 0, 0, 17, 5, 48) # stocks in each fao region

svg("results/stock_map.svg")
ggplot() +
  geom_sf(data = fao_regions, aes(fill = num_stocks)) + scale_fill_gradientn("Number of Stocks", colors = rev(brewer.pal(5, "YlGnBu"))) +
  theme_classic() 
dev.off() 

# Stock-recruitment curves -----------------------------------------------------------------------------------------
pdf("results/final_rec_sb.pdf")
for(x in stock_model_fits$stock_name){
  row <- which(stock_model_fits$stock_name == x)
  
  # pull s-r model parameters
  r_a <- as.numeric(stock_model_fits[row, "ricker_a"])
  r_b <- as.numeric(stock_model_fits[row, "ricker_b"])
  bh_a <- as.numeric(stock_model_fits[row, "bevholt_a"])
  bh_b <- as.numeric(stock_model_fits[row, "bevholt_b"])
  
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
  
  # get ranks for recruitment and spawning biomass
  stock <- stock %>%
    mutate(rec_rank = rank(recruits)) %>%
    mutate(sb_rank = rank(sb))
  
  # calculate cross-correlation values
  stock_ccf <- ccf(stock$rec_rank, stock$sb_rank, plot = FALSE, lag.max = 10)
  stock_ccf_df <- data.frame(lag = stock_ccf$lag,
                             ccf = stock_ccf$acf)
  zero_lag <- subset(stock_ccf_df, lag == 0)
  neg_lags <- subset(stock_ccf_df, lag < 0)
  
  # Fit regime model
  fitPelt	<-cpt.meanvar(log(stock$recruits),method="PELT",test.stat="Normal",penalty="AIC",minseglen=6,pen.value=0.05)
  
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
  
  sb_values <- seq(1, max_sb, length.out = 150)
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
  row3 <- which(lifespan$stock_name == x)
  common_name <- as.character(lifespan[row3, "common_name"])
  region <- paste("Region:", as.character(lifespan[row3, "region"]))
  stock_id <- paste("Stock ID:", as.character(lifespan[row3, "stock_name"]))
  
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
  min_model <- pull(stock_model_fits[row, "min_model"])
  if(min_model == "bevholt"){
    ricker_lik <- formatC((1 - pull(stock_model_fits[row, "rel_likelihood"])), digits = 3)
    bev_lik <- formatC(pull(stock_model_fits[row, "rel_likelihood"]), digits = 3)
  }else{
    ricker_lik <- formatC(pull(stock_model_fits[row, "rel_likelihood"]), digits = 3)
    bev_lik <- formatC((1 - pull(stock_model_fits[row, "rel_likelihood"])), digits = 3)
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
  
  # cross-correlation plot
  lim1 <- qnorm((1 + (1 - 0.05))/2)/sqrt(stock_ccf$n.used)
  lim0 <- -lim1
  
  lab_points <- tibble(lag = numeric(),
                       ccf = numeric())
  
  if(zero_lag[,2] > lim1){
    lab_points <-  lab_points %>%
      add_row(lag = 0, ccf = zero_lag[,2])
  }
  
  neg_lags <- neg_lags %>%
    filter(ccf == max(ccf))
  
  if(neg_lags[,2] > lim1){
    lab_points <-  lab_points %>%
      add_row(lag = neg_lags[,1], ccf = neg_lags[,2])
  }
  
  if(count(lab_points) > 0){
    cc_plot <- ggplot(data = stock_ccf_df, aes(x = lag, y = ccf)) +
      geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) +
      geom_hline(aes(yintercept = lim1), linetype = 2, color = 'blue') +
      geom_hline(aes(yintercept = lim0), linetype = 2, color = 'blue') + ylim(-0.5,1) + xlim(-10,10) + 
      theme_minimal() + geom_text(data = lab_points,
                                  aes(label = formatC(ccf, digits = 2)), vjust = -1)
  }else{
    cc_plot <- ggplot(data = stock_ccf_df, aes(x = lag, y = ccf)) +
      geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) +
      geom_hline(aes(yintercept = lim1), linetype = 2, color = 'blue') +
      geom_hline(aes(yintercept = lim0), linetype = 2, color = 'blue') + ylim(-0.5,1) +
      theme_minimal()
  }
  
  # plot both graphs
  print(grid.arrange(rec_ts_plot, sr_plot, cc_plot, nrow = 3))
  
}
dev.off()


# Specific S-R graphs for project report -----------------------------------------------------------------------------
proj_report_stocks <- c("AMPL4T", "ARFLOUNDBSAI", "WHITVIa", "GOPHERSPCOAST", "BLACKROCKCAL", "COD5Zjm", "AUROCKPCOAST")

for(x in proj_report_stocks){
  png(paste("results/",x, "_sb.png", sep = ""))
  row <- which(stock_model_fits$stock_name == x)
  
  # pull s-r model parameters
  r_a <- as.numeric(stock_model_fits[row, "ricker_a"])
  r_b <- as.numeric(stock_model_fits[row, "ricker_b"])
  bh_a <- as.numeric(stock_model_fits[row, "bevholt_a"])
  bh_b <- as.numeric(stock_model_fits[row, "bevholt_b"])
  
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
  
  # get ranks for recruitment and spawning biomass
  stock <- stock %>%
    mutate(rec_rank = rank(recruits)) %>%
    mutate(sb_rank = rank(sb))
  
  # calculate cross-correlation values
  stock_ccf <- ccf(stock$sb_rank, stock$rec_rank, plot = FALSE, lag.max = 10)
  stock_ccf_df <- data.frame(lag = stock_ccf$lag,
                             ccf = stock_ccf$acf)
  zero_lag <- subset(stock_ccf_df, lag == 0)
  neg_lags <- subset(stock_ccf_df, lag < 0)
  
  # Fit regime model
  fitPelt	<-cpt.meanvar(log(stock$recruits),method="PELT",test.stat="Normal",penalty="AIC",minseglen=6,pen.value=0.05)
  
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
  
  sb_values <- seq(1, max_sb, length.out = 150)
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
  row3 <- which(lifespan$stock_name == x)
  common_name <- as.character(lifespan[row3, "common_name"])
  region <- paste("Region:", as.character(lifespan[row3, "region"]))
  stock_id <- paste("Stock ID:", as.character(lifespan[row3, "stock_name"]))
  
  # spawning biomass time series plot
  sb_ts_plot <- ggplot(data = stock, aes(x = year, y = sb)) +
    geom_line(linetype = 2, size = 0.8) + labs(y = "Spawning Biomass" , title = common_name, subtitle = paste(region, stock_id, sep = "\n")) + 
    theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # recruitment time series plot
  rec_ts_plot <- ggplot(data = stock, aes(x = year, y = recruits)) +
    geom_line(size = 1) +
    geom_rect(data = rectangle_data, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin,
                                                              ymax = ymax, fill = as.factor(color_factor)), alpha = 0.3) +
    guides(fill = "none") +
    labs(x = "Year", y = "Recruits") +
    theme_minimal()
  
  # add regime factor to spawning biomass recruitment data
  stock <- stock %>%
    add_column(color_factor = as.factor(regime_colors))
  
  # retrieve ricker and bevholt model weights
  min_model <- pull(stock_model_fits[row, "min_model"])
  if(min_model == "bevholt"){
    ricker_lik <- formatC((1 - pull(stock_model_fits[row, "rel_likelihood"])), digits = 3)
    bev_lik <- formatC(pull(stock_model_fits[row, "rel_likelihood"]), digits = 3)
  }else{
    ricker_lik <- formatC(pull(stock_model_fits[row, "rel_likelihood"]), digits = 3)
    bev_lik <- formatC((1 - pull(stock_model_fits[row, "rel_likelihood"])), digits = 3)
  }
  
  # stock recruitment plot
  sr_plot <- ggplot(data = stock, aes(x = sb, y = recruits))+
    geom_point(aes(color = color_factor), size = 3, show.legend = FALSE) +
    geom_line(data = preds, inherit.aes = FALSE, aes(x = sb, y = rec_pred, linetype = model), size = 1) +
    labs(x = "Spawning Biomass", y = "Recruits") + 
    scale_linetype_discrete(labels = c(paste("Ricker", ricker_lik), paste("BevHolt", bev_lik))) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.background = element_rect(fill = FALSE, color = FALSE),
          legend.title = element_blank())
  
  # cross-correlation plot
  lim1 <- qnorm((1 + (1 - 0.05))/2)/sqrt(stock_ccf$n.used)
  lim0 <- -lim1
  
  lab_points <- tibble(lag = numeric(),
                       ccf = numeric())
  
  if(zero_lag[,2] > lim1){
    lab_points <-  lab_points %>%
      add_row(lag = 0, ccf = zero_lag[,2])
  }
  
  neg_lags <- neg_lags %>%
    filter(ccf == max(ccf))
  
  if(neg_lags[,2] > lim1){
    lab_points <-  lab_points %>%
      add_row(lag = neg_lags[,1], ccf = neg_lags[,2])
  }
  
  if(count(lab_points) > 0){
    cc_plot <- ggplot(data = stock_ccf_df, aes(x = lag, y = ccf)) +
      geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) +
      geom_hline(aes(yintercept = lim1), linetype = 2, color = 'blue') +
      geom_hline(aes(yintercept = lim0), linetype = 2, color = 'blue') + ylim(-0.5,1) + xlim(-10,10) + 
      theme_minimal() + geom_text(data = lab_points,
                                  aes(label = formatC(ccf, digits = 2)), vjust = -1)
  }else{
    cc_plot <- ggplot(data = stock_ccf_df, aes(x = lag, y = ccf)) +
      geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) +
      geom_hline(aes(yintercept = lim1), linetype = 2, color = 'blue') +
      geom_hline(aes(yintercept = lim0), linetype = 2, color = 'blue') + ylim(-0.5,1) +
      theme_minimal()
  }
  
  # plot both graphs
  print(grid.arrange(sb_ts_plot, rec_ts_plot, sr_plot, cc_plot, ncol = 2, nrow = 4, 
                     layout_matrix = rbind(c(1,1), c(2,2), c(3,4), c(3,4))))
  
  dev.off()
}
