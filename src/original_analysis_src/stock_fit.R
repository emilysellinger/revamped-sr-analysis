# Identify deterministic S-R relationships and remove those stocks from analysis

# RMSE Values -------------------------------------------------------------
# I still have about 200 more stocks than the original analysis, which could be due to the updates to RAM
# in the years following Szuwalski et al 2015, but I just want to make sure that without the model run in times,
# the stocks I currently have are not off of the S-R curve

# Could not fit a ricker curve to these stocks, temporarily removing them
use_stocks <- use_stocks %>% 
  filter(!stock_name %in% c("CSALMJDFS", "PSALMCHINAP", "PSALMHUMPCY", "PSALMOKPH",
                            "PSALMSITUKR", "SSALMFENNELL", "SSALMGATES", "TARAKNZ"))

RMSE_test <- tibble(stock_name = "",
                    rmse = numeric())

for(x in use_stocks$stock_name){
  
  # retrieve spawning biomass and recruitment data
  stock <- retrieve_sr_data(x)
    
  # find starting values for nonlinear regression 
  ricker_starts <- srStarts(recruits~sb, data = stock, type = "Ricker", param = 1)
    
  if(ricker_starts[2] < 0){ricker_starts[2] <- 0.000001} # make sure ricker b param is positive
  ricker_fit <- nlxb(ricker, data = stock, start = ricker_starts)
    
  RMSE <- sqrt(mean((ricker_fit$resid^2)))
    
  RMSE_test <- RMSE_test %>%
    add_row(stock_name = x, rmse = RMSE)
}

# I am also going to remove any stock with an RMSE value less than 0.15 
# the 5th and 95th quantiles are 0.154 and 1.363

stocks <- RMSE_test %>% 
  filter(rmse >= 0.15) %>% 
  select(stock_name)

use_stocks <- use_stocks %>% 
  filter(stock_name %in% stocks$stock_name)

# Compare Ricker and BevHolt fits --------------------------------------------------------------------------------
stock_model_fits <- tibble(stock_name = "",
                          ricker_a = numeric(),
                          ricker_b = numeric(),
                          ricker_sigmaR = numeric(),
                          ricker_AICc = numeric(),
                          bevholt_a = numeric(),
                          bevholt_b = numeric(),
                          bevholt_sigmaR = numeric(),
                          bevholt_AICc = numeric())

for(x in use_stocks$stock_name){
  
  # retrieve spawning biomass and recruitment data for each stock
  stock <- retrieve_sr_data(x)
  
  # Change any 0 recruit values to 1
  stock <- stock %>%
    mutate(logR = replace(logR, logR == -Inf, 1)) %>%
    mutate(recruits = replace(recruits, recruits == 0, 1))
    
  
  # double check to make sure total number of years is at least 20
  if(count(stock) != 0 && count(stock) >= 20 ){
    
    # find starting values for beverton holt and ricker curves
    ricker_starts <- srStarts(recruits~sb, data = stock, type = "Ricker", param = 1)
    bevholt_starts <- srStarts(recruits~sb, data = stock, type = "BevertonHolt", param = 1)
    
    if(ricker_starts[2] < 0){ricker_starts[2] <- 0.000001} # make sure ricker b param is positive
    if(bevholt_starts[1] < 0){bevholt_starts[1] <- 0.000001} # make sure bevholt a param is positive
    if(bevholt_starts[2] < 0){bevholt_starts[2] <- 0.000001} # make sure bevholt b param is positive
    
    # fit ricker and beverton holt models to stock
    ricker_fit <- ricker(stock)
    bevholt_fit <- bevholt(stock)
    
    # calculate AICc for both models
    params <- 3
    n <- nrow(stock)
    ricker_AICc <- AIC(ricker_fit) + ((2*params*(params+1)) /(n - params - 1))
    bevholt_AICc <- AIC(bevholt_fit) + ((2*params*(params+1)) /(n - params - 1))
    
    # add data to filterd stocks tibble
    stock_model_fits <- stock_model_fits %>%
      add_row(stock_name = x, ricker_a = exp(unname(ricker_fit@coef[1])), 
              ricker_b = exp(unname(ricker_fit@coef[2])), ricker_sigmaR = exp(unname(ricker_fit@coef[3])),
              ricker_AICc = ricker_AICc, bevholt_a = exp(unname(bevholt_fit@coef[1])),
              bevholt_b = exp(unname(bevholt_fit@coef[2])), bevholt_sigmaR = exp(unname(bevholt_fit@coef[3])),
              bevholt_AICc = bevholt_AICc)
  }
}

# Plot stock recruit fits -------------------------------------------------------------------------------------
pdf("results/original_analysis/figures/stock_sr_fits.pdf")
for(x in stock_model_fits$stock_name){
  row <- which(stock_model_fits$stock_name == x)
  row2 <- which(use_stocks$stock_name == x)
  
  r_a <- as.numeric(stock_model_fits[row, "ricker_a"])
  r_b <- as.numeric(stock_model_fits[row, "ricker_b"])
  bh_a <- as.numeric(stock_model_fits[row, "bevholt_a"])
  bh_b <- as.numeric(stock_model_fits[row, "bevholt_b"])
  
  stock <- data.frame(takers_rec[,1],
                  takers_rec[, x],
                  takers_ssb[, x])
  colnames(stock) <- c("year", "recruits", "sb")
  # remove model run in time
  min_year <- pull(use_stocks[row2, "min_year"])
  max_year <- pull(use_stocks[row2, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  # simulate ricker and beverton holt data
  max_sb <- max(stock$sb, na.rm = TRUE)
  sb_values <- seq(1, max_sb, length.out = 200)
  rec_pred_r <- r_a*sb_values*exp(-r_b*sb_values)
  rec_pred_bh <- (bh_a*sb_values)/(1 + (bh_b*sb_values))
  
  # create tibble with preditions
  preds <- tibble(sb = sb_values,
                  r_preds = rec_pred_r,
                  bh_preds = rec_pred_bh)
  
  # recruitment time series plot
  rec_ts_plot <- ggplot(data = stock, aes(x = year, y = recruits)) +
    geom_line(size = 1) +
    labs(x = "Year", y = "Recruits", title = x)
  
  # stock recruitment plot
  sr_plot <- ggplot(data = stock, aes(x = sb, y = recruits)) +
    geom_point() + 
    geom_line(data = preds, aes(x = sb, y = r_preds), colour = "#E31A1C", size = 1) +
    geom_line(data = preds, aes(x = sb, y = bh_preds), colour = "#1F78B4", size = 1) +
    labs(x = "Spawning Biomass", y = "Recruits") 
  
  # plot both graphs
  print(grid.arrange(rec_ts_plot, sr_plot, nrow = 2))
}
dev.off()

# Split stocks ----------------------------------------------------------------------------------------
# In Szuwalski et al. 2015, the stocks suitable for analysis were split based on relative likelihood value then
# different procedures were then applied to each. The correlation analysis will be in a different script.

stock_model_fits <- stock_model_fits %>%
  add_column(min_model = "test") %>%
  add_column(rel_likelihood = 1.0)

# determine min AIC model and use it to calculate relative likelihood
for(x in 1:nrow(stock_model_fits)){
  if(stock_model_fits[x,"ricker_AICc"] < stock_model_fits[x, "bevholt_AICc"]){
    stock_model_fits[x, "min_model"] = "ricker"
    stock_model_fits[x,"rel_likelihood"] = 1/(1 + exp((pull(stock_model_fits[x,"ricker_AICc"]) - pull(stock_model_fits[x, "bevholt_AICc"]))/2))
  }else{
    stock_model_fits[x, "min_model"] = "bevholt"
    stock_model_fits[x,"rel_likelihood"] = 1/(1 + exp((pull(stock_model_fits[x,"bevholt_AICc"]) - pull(stock_model_fits[x, "ricker_AICc"]))/2))
  }
}

# dome-shaped stocks are those with a relative likelihood > 0.75
dome_stocks <- stock_model_fits %>%
  filter(min_model == "ricker") %>%
  filter(rel_likelihood > 0.75)

monotonic_stocks <- stock_model_fits %>%
  filter(min_model == "bevholt" | rel_likelihood < 0.75)



# Output to CSV  ----------------------------------------------------------
write.csv(dome_stocks, file = here("results/original_analysis/csv_files", "dome_stocks.csv"))
write.csv(monotonic_stocks, file = here("results/original_analysis/csv_files", "monotonic_stocks.csv"))
write.csv(stock_model_fits, file = here("results/original_analysis/csv_files", "stock_model_fits.csv"))
write.csv(use_stocks, file = here("results/original_analysis/csv_files", "use_stocks.csv"))
write.csv(takers_rec, file = here("results/original_analysis/csv_files", "takers_rec.csv"))
write.csv(takers_ssb, file = here("results//original_analysis/csv_files", "takers_ssb.csv"))



