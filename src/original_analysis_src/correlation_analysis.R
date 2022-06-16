# correlation analysis to determine what is driving recruitment

# Dome-shaped stocks ------------------------------------------------------------------------------------------------
# going to add extra columns for rho estimate and p-value from spearman's correlation
dome_stocks <- dome_stocks %>%
  add_column(zero_lag = 1,
             zero_lag_pval = 1,
             driver = "")

for(x in dome_stocks$stock_name){
  # retrieve spawning biomass and recruitment data for stock
  row <- which(dome_stocks$stock_name == x)
  stock <- retrieve_sr_data(x)
  
  # find max SpBio
  ricker_b <- as.matrix(dome_stocks[row, "ricker_b"])[1]
  max_sb <- 1/ricker_b
  
  
  # find S-R pairs greater than max & remove from dataset
  stock <- stock %>% 
    filter(sb <= 1.5*max_sb)
  # some of the ricker curves don't fit as well, will add a workaround
  # since the correlation will be negative (i.e. environmentally influenced)
  if(nrow(stock) <= 3){
    stock <- retrieve_sr_data(x)
  }
  # find spearman's correlation
  corrr <- cor.test(rank(stock$recruits), rank(stock$sb))
  
  # save rho estimate and p-value to data frame
  dome_stocks[row, "zero_lag"] <- corrr$estimate
  dome_stocks[row, "zero_lag_pval"] <- corrr$p.value
  
  # determine driver - if lag0 is positive and significant, the stock is primarily driven by
  # spawning biomass
  if(corrr$estimate > 0 && corrr$p.value < 0.05){
    dome_stocks[row, "driver"] <- "spawning biomass"
  }else{
    dome_stocks[row, "driver"] <- "environment"
  }
}

# Monotonic stocks -----------------------------------------------------------------------------------------
# add extra columns for correlation results
monotonic_stocks <- monotonic_stocks %>%
  add_column(zero_lag = 1,
             zero_lag_pval = 1,
             driver = "")

for(x in monotonic_stocks$stock_name){
  row <- which(monotonic_stocks$stock_name == x)
  stock <- retrieve_sr_data(x)
  
  # get ranks for recruitment and spawning biomass
  stock <- stock %>%
    mutate(rec_rank = rank(recruits)) %>%
    mutate(sb_rank = rank(sb))
  
  
  # find cross correlation values
  stock_ccf <- ccf(stock$rec_rank, stock$sb_rank, lag.max = 10)
  stock_ccf_df <- data.frame(lag = stock_ccf$lag,
                             ccf = stock_ccf$acf)

  
  
  # save zero-lagged correlation value and threshold significance value
  monotonic_stocks[row, "zero_lag"] <- zero_lag <- stock_ccf_df[11,2]
 
  stock_ccf_df <- stock_ccf_df %>% 
    filter(lag < 0)
  
  # save zero-lagged correlation p value
  corrr <- cor.test(rank(stock$rec_rank), rank(stock$sb_rank))
  monotonic_stocks[row, "zero_lag_pval"] <- corrr$p.value
  
  # check negative lags - if lag0 is significant and positive AND all of the negative lags are 
  # less than lag0, the primary driver of the stock is spawning biomass.
  
  if(zero_lag > 0 && corrr$p.value < 0.05){
    if(all(stock_ccf_df$ccf < zero_lag)){
      monotonic_stocks[row, "driver"] <- "spawning biomass" 
    }else{
      monotonic_stocks[row, "driver"] <- "edge case"
    }
  }else{
    monotonic_stocks[row, "driver"] <- "environment"
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
  if(dome_stocks[x, "driver"] == "spawning biomass"){
    sb_driven_stocks <- sb_driven_stocks %>%
      add_row(stock_name = pull(dome_stocks[x,"stock_name"]), curve_shape = "dome")
  }
}

# check monotonic stocks
for(x in 1:nrow(monotonic_stocks)){
  if(monotonic_stocks[x, "driver"] == "spawning biomass"){
    sb_driven_stocks <- sb_driven_stocks %>%
        add_row(stock_name = pull(monotonic_stocks[x, "stock_name"]), curve_shape = "monotonic")
    }
    
}

# Combine environmentally driven stocks ---------------------------------------------------------
env_driven_stocks <- tibble()
env_driven_stocks <- env_driven_stocks %>%
  add_column(stock_name = "",
             curve_shape = "")

# find all dome shaped stocks with a p-value > 0.05
for(x in 1:nrow(dome_stocks)){
  if(dome_stocks[x,"driver"] == "environment"){
    env_driven_stocks <- env_driven_stocks %>%
      add_row(stock_name = pull(dome_stocks[x, "stock_name"]), curve_shape = "dome")
  }
}

# find all monotonic stocks with a non-significant zero lagged correlation
for(x in 1:nrow(monotonic_stocks)){
  if(monotonic_stocks[x,"driver"] == "environment"){
    env_driven_stocks <- env_driven_stocks %>%
      add_row(stock_name = pull(monotonic_stocks[x,"stock_name"]), curve_shape = "monotonic")
  }
}

# Edge Cases -----------------------------------------------------------------------------------------------
edge_stocks <- tibble()
edge_stocks <- edge_stocks %>%
  add_column(stock_name = "",
             curve_shape = "")

for(x in 1:nrow(monotonic_stocks)){
  if(monotonic_stocks[x,"driver"] == "edge case"){
    edge_stocks <- edge_stocks %>%
      add_row(stock_name = pull(monotonic_stocks[x,"stock_name"]), curve_shape = "monotonic")
  }
}


# Write to CSV files ------------------------------------------------------
write_csv(sb_driven_stocks, here("results/original_analysis/csv_files", "sb_driven_stocks.csv"))
write_csv(env_driven_stocks, here("results/original_analysis/csv_files", "env_driven_stocks.csv"))
write_csv(edge_stocks, here("results/original_analysis/csv_files", "edge_stocks.csv"))

# DCCA comparison ---------------------------------------------------------
# going to calculate the DCCA correlation coefficient for comparison to 
# Spearman's
rho_comparison <- tibble(stock_name = c(dome_stocks$stock_name, monotonic_stocks$stock_name),
                         sp_zero_lag = c(dome_stocks$zero_lag, monotonic_stocks$zero_lag),
                         sp_pval = c(dome_stocks$zero_lag_pval, monotonic_stocks$zero_lag_pval),
                         dcca_rho = rep(NA, 432),
                         driver = c(dome_stocks$driver, monotonic_stocks$driver))


for(x in rho_comparison$stock_name){
  row <- which(rho_comparison$stock_name == x)
  stock <- retrieve_sr_data(x)
  
  # calculate DCCA rho
  dcca <- rhodcca(pull(stock$logR), pull(log(stock$sb)), m = 5, nu = 2)
  rho_comparison[row, "dcca_rho"] <- dcca$rhodcca
}

# going to try to compare the values by primary influence classification

# spawning biomass
rho_comparison_sb <- rho_comparison %>% 
  filter(driver == "spawning biomass")
rho_comparison_sb <- pivot_longer(rho_comparison_sb, !c(stock_name, driver, sp_pval), 
               names_to = "method", values_to = "rho") %>% 
  mutate(paired = rep(1:(n()/2), each = 2))
rho_comparison_sb$method <-  sub("sp_zero_lag", "spearmans", rho_comparison_sb$method)
rho_comparison_sb$method <-  sub("dcca_rho", "dcca", rho_comparison_sb$method)

# environment
rho_comparison_env <- rho_comparison %>% 
  filter(driver == "environment")
rho_comparison_env <- pivot_longer(rho_comparison_env, !c(stock_name, driver, sp_pval), 
               names_to = "method", values_to = "rho") %>% 
  mutate(paired = rep(1:(n()/2), each = 2))
rho_comparison_env$method <-  sub("sp_zero_lag", "spearmans", rho_comparison_env$method)
rho_comparison_env$method <-  sub("dcca_rho", "dcca", rho_comparison_env$method)

# edge cases
rho_comparison_edge <- rho_comparison %>% 
  filter(driver == "edge case")
rho_comparison_edge <- pivot_longer(rho_comparison_edge, !c(stock_name, driver, sp_pval), 
                                   names_to = "method", values_to = "rho") %>% 
  mutate(paired = rep(1:(n()/2), each = 2))
rho_comparison_edge$method <-  sub("sp_zero_lag", "spearmans", rho_comparison_edge$method)
rho_comparison_edge$method <-  sub("dcca_rho", "dcca", rho_comparison_edge$method)


# make dumbbell plots to show differences
# spawning biomass
rho_comparison_sb %>% 
  ggplot(aes(x = rho, y = stock_name)) +
  geom_line(aes(group = paired), color = "grey") +
  geom_point(aes(color = method), size = 4) +
  geom_vline(xintercept = 0, color = "black") +
  labs(x = "correlation coefficient", y = "stock identifier", 
       title = "Stocks with primary influence of spawning biomass on recruitment") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(here("results/original_analysis/figures", "sb_rho_comp_dumbbell.pdf"), height = 10, width = 12)

# environment
rho_comparison_env %>% 
  ggplot(aes(x = rho, y = stock_name)) +
  geom_line(aes(group = paired), color = "grey") +
  geom_point(aes(color = method), size = 4) +
  geom_vline(xintercept = 0, color = "black") +
  xlim(-1,1) +
  labs(x = "correlation coefficient", y = "stock identifier", 
       title = "Stocks with primary influence of environment on recruitment") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(here("results/original_analysis/figures", "env_rho_comp_dumbbell.pdf"), height = 30, width = 12)

rho_comparison_edge %>% 
  ggplot(aes(x = rho, y = stock_name)) +
  geom_line(aes(group = paired), color = "grey") +
  geom_point(aes(color = method), size = 4) +
  geom_vline(xintercept = 0, color = "black") +
  xlim(-1,1) + 
  labs(x = "correlation coefficient", y = "stock identifier", 
       title = "Edge case stocks") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(here("results/original_analysis/figures", "edge_rho_comp_dumbbell.pdf"), height = 15)

# Committee would like scatter plots as well as dumbell plots
rho_comparison %>% 
  filter(driver == "spawning biomass") %>% 
  ggplot(aes(x = sp_zero_lag, y = dcca_rho)) + geom_point() +
  labs(x = "Spearman's correlation coefficient", y = "DCCA correlation coefficient",
       title = "Spawning biomass stocks") + xlim(0, 1) + ylim(-1, 1) +
  theme_minimal()
ggsave(here("results/original_analysis/figures", "sb_case_rho_scatter.pdf"))

rho_comparison %>% 
  filter(driver == "environment") %>% 
  ggplot(aes(x = sp_zero_lag, y = dcca_rho)) + geom_point() +
  labs(x = "Spearman's correlation coefficient", y = "DCCA correlation coefficient",
        title = "Environment stocks") + xlim(-1, 1) + ylim(-1, 1) +
  theme_minimal()
ggsave(here("results/original_analysis/figures", "env_case_rho_scatter.pdf"))


rho_comparison %>% 
  filter(driver == "edge case") %>% 
  ggplot(aes(x = sp_zero_lag, y = dcca_rho)) + geom_point() +
  labs(x = "Spearman's correlation coefficient", y = "DCCA correlation coefficient",
       title = "Edge case stocks") + xlim(0, 1) + ylim(-1, 1) +
  theme_minimal()
ggsave(here("results/original_analysis/figures", "edge_case_rho_scatter.pdf"))


# Supplementary DCCA explanation ------------------------------------------
stock <- retrieve_sr_data("ARFLOUNDBSAI")
row <- which(dome_stocks$stock_name == "ARFLOUNDBSAI")

# plot stock and recruit relationship
ggplot(stock) + geom_point(aes(x = log(sb), y = logR)) +
  labs(x = "spawning biomass", y = "recruits")

# get ranks for recruitment and spawning biomass
stock <- stock %>%
  mutate(rec_rank = rank(recruits)) %>%
  mutate(sb_rank = rank(sb))

ggplot(data = stock) + geom_point(aes(x = sb_rank, y = rec_rank)) + 
  labs(x = "ranked spawning biomass",y = "ranked recruitment", 
       title = "Spearman's correlation", subtitle = "correlation = 0.146") +
  geom_abline(slope = 0.1463, intercept = 17.5)


short_stock1 <- stock %>% 
  filter(year <= 1980)
lm1 <- lm(short_stock1$logR~short_stock1$year)
lm1_resids <- lm1$residuals

c <- stock %>% 
  filter(year <= 1990) %>% 
  add_column(win = c(rep(1,5), rep(0,10))) %>% 
  ggplot() + geom_point(aes(x = year, y = logR, color = as.factor(win)), size = 3) +
  geom_line(aes(x = year, y = logR), linetype = 2) +
  labs(title = "window 1") + 
  geom_abline(slope =  lm1$coefficients[2], intercept = lm1$coefficients[1], color = "#00BFC4", size = 1) +
  theme(legend.position = "none")

c2 <- short_stock1 %>% 
  add_column(resids = lm1_resids) %>% 
  ggplot() + geom_point(aes(x = year, y = resids), color = "#00BFC4", size = 3) +
  geom_hline(yintercept = 0) + 
  labs(x = "year", y = "residuals")

lm2 <- lm(log(short_stock1$sb)~short_stock1$year)
lm2_resids <- lm2$residuals

d <- stock %>% 
  filter(year <= 1990) %>% 
  add_column(win = c(rep(1,5), rep(0,10))) %>% 
  ggplot() + geom_point(aes(x = year, y = log(sb), color = as.factor(win)), size = 3) +
  geom_line(aes(x = year, y = log(sb)), linetype = 2) +
  labs(y = "spawning biomass") + 
  geom_abline(slope = lm2$coefficients[2], intercept = lm2$coefficients[1], color = "#00BFC4", size = 1) +
  theme(legend.position = "none")

d2 <- short_stock1 %>% 
  add_column(resids = lm2_resids) %>% 
  ggplot() + geom_point(aes(x = year, y = resids), color = "#00BFC4", size = 3) +
  geom_hline(yintercept = 0) + 
  labs(x = "year", y = "residuals")

lm3 <- lm(short_stock1$logR~log(short_stock1$sb))
lm3_resids <- lm3$residuals

e <- short_stock1 %>% 
  filter(year <= 1980) %>% 
  add_column(rec_resids = lm1_resids, sb_resids = lm2_resids) %>% 
  ggplot() + geom_point(aes(x = sb_resids, y = rec_resids), size = 3, color = "#00BFC4") +
  labs(x = "detrended spawning biomass", y = "detrended recruits")


grid.arrange(c, c2, d, d2, e, nrow = 3)

# Need to change to log if you want to make a second graph
short_stock1 <- stock %>% 
  filter(year >= 1977) %>% 
  filter(year <= 1981)
lm1 <- lm(short_stock1$recruits~short_stock1$year)
lm1_resids <- lm1$residuals

c <- stock %>% 
  filter(year <= 1990) %>% 
  add_column(win = c(0, rep(1,5), rep(0,9))) %>% 
  ggplot() + geom_point(aes(x = year, y = recruits, color = as.factor(win)), size = 3) +
  geom_line(aes(x = year, y = recruits), linetype = 2) +
  labs(title = "window 2") + 
  geom_abline(slope =  lm1$coefficients[2], intercept = lm1$coefficients[1], color = "#00BFC4", size = 1) +
  theme(legend.position = "none")

c2 <- short_stock1 %>% 
  add_column(resids = lm1_resids) %>% 
  ggplot() + geom_point(aes(x = year, y = resids), color = "#00BFC4", size = 3) +
  geom_hline(yintercept = 0) + 
  labs(x = "year", y = "residuals")

lm2 <- lm(short_stock1$sb~short_stock1$year)
lm2_resids <- lm2$residuals

d <- stock %>% 
  filter(year <= 1990) %>% 
  add_column(win = c(0, rep(1,5), rep(0,9))) %>% 
  ggplot() + geom_point(aes(x = year, y = sb, color = as.factor(win)), size = 3) +
  geom_line(aes(x = year, y = sb), linetype = 2) +
  labs(y = "spawning biomass") + 
  geom_abline(slope =  lm2$coefficients[2], intercept = lm2$coefficients[1], color = "#00BFC4", size = 1) +
  theme(legend.position = "none")

d2 <- short_stock1 %>% 
  add_column(resids = lm2_resids) %>% 
  ggplot() + geom_point(aes(x = year, y = resids), color = "#00BFC4", size = 3) +
  geom_hline(yintercept = 0) + 
  labs(x = "year", y = "residuals")

lm3 <- lm(short_stock1$recruits~short_stock1$sb)
lm3_resids <- lm3$residuals

e <- short_stock1 %>% 
  filter(year >= 1977) %>% 
  filter(year <= 1981) %>% 
  add_column(rec_resids = lm1_resids, sb_resids = lm2_resids) %>% 
  ggplot() + geom_point(aes(x = sb_resids, y = rec_resids), size = 3, color = "#00BFC4") +
  labs(x = "detrended spawning biomass", y = "detrended recruits")


grid.arrange(c, c2, d, d2, e, nrow = 3)
