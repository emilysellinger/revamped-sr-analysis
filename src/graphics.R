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

fao <- read.csv(here("data/original_analysis", "fao_region.csv"))
fao$stock_name <- str_replace_all(fao$stock_name, "-", "\\.")

# going to redo this so it is without the lifespan issue
env_driven_stocks <- env_driven_stocks %>% 
  left_join(fao)
edge_stocks <- edge_stocks %>% 
  left_join(fao)
sb_driven_stocks <- sb_driven_stocks %>% 
  left_join(fao)

all_stocks <- rbind(env_driven_stocks, edge_stocks, sb_driven_stocks)
fao_num_stocks <- all_stocks %>% 
  group_by(primary_FAOarea) %>% 
  summarise(n = n()) %>% 
  rename(zone = primary_FAOarea) %>% 
  rename(num_stocks = n)

# going to reassign the inland FAO regions to ocean ones 
all_stocks$primary_FAOarea[all_stocks$primary_FAOarea == 2] <- 67
all_stocks$primary_FAOarea[all_stocks$primary_FAOarea == 5] <- 61
all_stocks$primary_FAOarea[all_stocks$primary_FAOarea == 4] <- 61

fao_regions <- st_read("data/original_analysis/World_FAO_Zones.shp")
# stocks in each fao region
fao_regions <- fao_regions %>% 
  left_join(fao_num_stocks) %>% 
  mutate(num_stocks = replace_na(num_stocks, 0))
 

pdf("results/original_analysis/figures/stock_map.pdf")
ggplot() +
  geom_sf(data = fao_regions, aes(fill = num_stocks)) + scale_fill_gradientn("Number of Stocks", colors = natparks.pals("Glacier", 10)) +
  theme_classic() 
dev.off() 
svg("results/original_analysis/figures/stock_map.svg")
ggplot() +
  geom_sf(data = fao_regions, aes(fill = num_stocks)) + scale_fill_gradientn("Number of Stocks", colors = rev(brewer.pal(5, "YlGnBu"))) +
  theme_classic() 
dev.off() 

# Stock-recruitment curves -----------------------------------------------------------------------------------------
stock_info <- read_csv(here("ram_stock_info.csv")) # has area IDs for each stock

# Going to switch back to the stock naming conventions that the rest of the data frames have
stock_info$stockid <- str_replace_all(stock_info$stockid, "-", "\\.")
stock_info <- stock_info %>% 
  rename(stock_name = stockid)


pdf("results/original_analysis/figures/final_rec_sb.pdf")
for(x in stock_model_fits$stock_name){
  row <- which(stock_model_fits$stock_name == x)
  
  # pull s-r model parameters
  r_a <- as.numeric(stock_model_fits[row, "ricker_a"])
  r_b <- as.numeric(stock_model_fits[row, "ricker_b"])
  bh_a <- as.numeric(stock_model_fits[row, "bevholt_a"])
  bh_b <- as.numeric(stock_model_fits[row, "bevholt_b"])
  
  # create tibble with s-r data
  stock <- retrieve_sr_data(x)
  
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
  row3 <- which(stock_info$stock_name == x)
  scientific_name <- as.character(stock_info[row3, "scientificname"])
  region <- paste("Area ID:", as.character(stock_info[row3, "areaid"]))
  common_name <- as.character(stock_info[row3, "commonname"])
  
  # spawning biomass time series plot
  sb_ts_plot <- ggplot(data = stock, aes(x = year, y = sb)) +
    geom_line(linetype = 2, size = 0.8) + labs(y = "Spawning Biomass" ,  title = common_name, subtitle = paste(scientific_name, region, "(a)", sep = "\n")) + 
    theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # recruitment time series plot
  rec_ts_plot <- ggplot(data = stock, aes(x = year, y = recruits)) +
    geom_line(size = 1) +
    geom_rect(data = rectangle_data, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin,
                                                              ymax = ymax, fill = as.factor(color_factor)), alpha = 0.5) +
    guides(fill = "none") +
    labs(x = "Year", y = "Recruits") +
    scale_fill_manual(values = natparks.pals("Banff")) + 
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
    scale_color_manual(values = natparks.pals("Banff")) +
    geom_line(data = preds, inherit.aes = FALSE, aes(x = sb, y = rec_pred, linetype = model), size = 1) +
    labs(x = "Spawning Biomass", y = "Recruits", subtitle = "(b)") + 
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
      geom_hline(aes(yintercept = lim1), linetype = 2, color = "#006475") +
      geom_hline(aes(yintercept = lim0), linetype = 2, color = "#006475") + ylim(-0.5,1) + xlim(-10,10) + 
      labs(subtitle = "(c)", y = "cross correlation") + theme_minimal() + 
      geom_text(data = lab_points, aes(label = formatC(ccf, digits = 2)), vjust = -1)
  }else{
    cc_plot <- ggplot(data = stock_ccf_df, aes(x = lag, y = ccf)) +
      geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) +
      geom_hline(aes(yintercept = lim1), linetype = 2, color = "#006475") +
      geom_hline(aes(yintercept = lim0), linetype = 2, color = "#006475") + ylim(-0.5,1) +
      labs(subtitle = "(c)", y = "cross correlation") + theme_minimal()
  }
  
  # plot graphs
  print(grid.arrange(sb_ts_plot, rec_ts_plot, sr_plot, cc_plot, ncol = 2, nrow = 4, 
                     layout_matrix = rbind(c(1,1), c(2,2), c(3,4), c(3,4))))
  
}
dev.off()


