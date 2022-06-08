# correlation analysis to determine what is driving recruitment

# Dome-shaped stocks ------------------------------------------------------------------------------------------------
# going to add extra columns for rho estimate and p-value from spearman's correlation
dome_stocks <- dome_stocks %>%
  add_column(zero_lag = 1,
             zero_lag_pval = 1,
             driver = "")

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
  ricker_b <- as.matrix(dome_stocks[row, "ricker_b"])[1]
  max_sb <- 1/ricker_b
  
  # find S-R pairs greater than 50% of max & remove from dataset
  for(y in 1:nrow(stock)){
    if(!is.na(stock[y,"sb"]) && stock[y,"sb"] > 0.5*max_sb){
      stock<- stock[-c(y),]
    }
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
  row2 <- which(use_stocks$stock_name == x)
  
  stock <- tibble(
    year = takers_rec[,1],
    recruits = takers_rec[, x],
    sb = takers_ssb[, x])
  
  # remove model run in time
  min_year <- pull(use_stocks[row2, "min_year"])
  max_year <- pull(use_stocks[row2, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
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


# DCCA comparison ---------------------------------------------------------
# going to calculate the DCCA correlation coefficient for comparison to 
# Spearman's
library(DCCA)

rho_comparison <- tibble(stock_name = c(dome_stocks$stock_name, monotonic_stocks$stock_name),
                         sp_zero_lag = c(dome_stocks$zero_lag, monotonic_stocks$zero_lag),
                         sp_pval = c(dome_stocks$zero_lag_pval, monotonic_stocks$zero_lag_pval),
                         dcca_rho = rep(NA, 432),
                         driver = c(dome_stocks$driver, monotonic_stocks$driver))


for(x in rho_comparison$stock_name){
  row <- which(rho_comparison$stock_name == x)
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
  
  # calculate DCCA rho
  dcca <- rhodcca(pull(stock$recruits), pull(stock$sb), m = 5, nu = 2)
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
ggsave(here("dcca_sim_figs", "sb_rho_comp.pdf"), height = 7)

# environment
rho_comparison_env %>% 
  ggplot(aes(x = rho, y = stock_name)) +
  geom_line(aes(group = paired), color = "grey") +
  geom_point(aes(color = method), size = 4) +
  geom_vline(xintercept = 0, color = "black") +
  labs(x = "correlation coefficient", y = "stock identifier", 
       title = "Stocks with primary influence of environment on recruitment") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(here("dcca_sim_figs", "env_case_rho_comp.pdf"), height = 30)

rho_comparison_edge %>% 
  ggplot(aes(x = rho, y = stock_name)) +
  geom_line(aes(group = paired), color = "grey") +
  geom_point(aes(color = method), size = 4) +
  geom_vline(xintercept = 0, color = "black") +
  labs(x = "correlation coefficient", y = "stock identifier", 
       title = "Edge case stocks") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(here("dcca_sim_figs", "edge_case_rho_comp.pdf"), height = 15)

# Committee would like scatter plots as well as dumbell plots
rho_comparison %>% 
  filter(driver == "spawning biomass") %>% 
  ggplot(aes(x = sp_zero_lag, y = dcca_rho)) + geom_point() +
  labs(x = "Spearman's correlation coefficient", y = "DCCA correlation coefficient",
       title = "Spawning biomass stocks") + xlim(0, 1) + ylim(-1, 1) +
  theme_minimal()
ggsave(here("dcca_sim_figs", "sb_case_rho_scatter.pdf"))

rho_comparison %>% 
  filter(driver == "environment") %>% 
  ggplot(aes(x = sp_zero_lag, y = dcca_rho)) + geom_point() +
  labs(x = "Spearman's correlation coefficient", y = "DCCA correlation coefficient",
        title = "Environment stocks") + xlim(-1, 1) + ylim(-1, 1) +
  theme_minimal()
ggsave(here("dcca_sim_figs", "env_case_rho_scatter.pdf"))


rho_comparison %>% 
  filter(driver == "edge case") %>% 
  ggplot(aes(x = sp_zero_lag, y = dcca_rho)) + geom_point() +
  labs(x = "Spearman's correlation coefficient", y = "DCCA correlation coefficient",
       title = "Edge case stocks") + xlim(0, 1) + ylim(-1, 1) +
  theme_minimal()
ggsave(here("dcca_sim_figs", "edge_case_rho_scatter.pdf"))
# PACF in SBiomass ---------------------------------------------------------
# going to calculate the PACF for the stocks included in the anaylsis because
# for the DCCA simulations, I ran some using an AR(1) model with a correlation coefficient of 0.5
# want to make sure that is a reasonable estimate for simulations

stock_sb_pacf <- tibble(stock_name = "test",
                        pacf_val = 1)

for(x in dome_stocks$stock_name){
  row <- which(dome_stocks$stock_name == x)
  row2 <- which(use_stocks$stock_name == x)
  
  stock <- tibble(
    year = takers_rec[,1],
    sb = takers_ssb[,x])
  
  # remove model run in time
  min_year <- pull(use_stocks[row2, "min_year"])
  max_year <- pull(use_stocks[row2, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  sb_pacf <- pacf(stock$sb, plot = FALSE)
  
  stock_sb_pacf <- stock_sb_pacf %>% 
    add_row(stock_name = x,
            pacf_val = sb_pacf$acf[1])
  
}

for(x in monotonic_stocks$stock_name){
  row <- which(monotonic_stocks$stock_name == x)
  row2 <- which(use_stocks$stock_name == x)
  
  stock <- tibble(
    year = takers_rec[,1],
    sb = takers_ssb[,x])
  
  # remove model run in time
  min_year <- pull(use_stocks[row2, "min_year"])
  max_year <- pull(use_stocks[row2, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  sb_pacf <- pacf(stock$sb, plot = FALSE)
  
  stock_sb_pacf <- stock_sb_pacf %>% 
    add_row(stock_name = x,
            pacf_val = sb_pacf$acf[1])
  
}

stock_sb_pacf <- stock_sb_pacf[-1,]

mean(stock_sb_pacf$pacf_val) # 0.723

plot(stock_sb_pacf$pacf_val)
quantile(stock_sb_pacf$pacf_val, probs = c(0.025, 0.975))
quantile(stock_sb_pacf$pacf_val, probs = c(0.25, 0.75))

# should probably up the AR(1) coefficient value when I get the chance