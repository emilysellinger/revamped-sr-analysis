# Identify deterministic S-R relationships and remove those stocks from analysis

# Define S-R models -------------------------------------------------------------------------------------
bevholt <- logR~log((a*sb)/(1 + b*sb))

ricker <- logR~log(a*sb*exp(-b*sb))

# RMSE Values -------------------------------------------------------------
# I still have about 200 more stocks than the original analysis, which could be due to the updates to RAM
# in the years following Szuwalski et al 2015, but I just want to make sure that without the model run in times,
# the stocks I currently have are not off of the S-R curve

RMSE_test <- tibble(stock_name = "",
                    rmse = numeric())

for(x in use_stocks$stock_name){
  row <- which(use_stocks$stock_name == x)
  
  # create a tibble for each stock's biomass and recruit data
  stock <- tibble(
    year = takers_rec[,1],
    recruits = takers_rec[,x],
    sb = takers_ssb[,x],
    logR = log(takers_rec[,x]))
  
  # remove model run in time
  min_year <- pull(use_stocks[row, "min_year"])
  max_year <- pull(use_stocks[row, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  # Change any 0 recruit values to 1
  stock <- stock %>%
    mutate(logR = replace(logR, logR == -Inf, 1)) %>%
    mutate(recruits = replace(recruits, recruits == 0, 1))

    
    # find starting values for nonlinear regression 
    ricker_starts <- srStarts(recruits~sb, data = stock, type = "Ricker", param = 1)
    
    if(ricker_starts[2] < 0){ricker_starts[2] <- 0.000001} # make sure ricker b param is positive
    ricker_fit <- nlxb(ricker, data = stock, start = ricker_starts)
    
    RMSE <- sqrt(mean((ricker_fit$resid^2)))
    
    RMSE_test <- RMSE_test %>%
      add_row(stock_name = x, rmse = RMSE)
}

# Could not fit a ricker curve to these stocks, temporarily removing them
use_stocks <- use_stocks %>% 
  filter(!stock_name %in% c("CSALMJDFS", "PSALMCHINAP", "PSALMHUMPCY", "PSALMOKPH",
                            "PSALMSITUKR", "SSALMFENNELL", "SSALMGATES", "TARAKNZ"))

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
                          ricker_AICc = numeric(),
                          bevholt_a = numeric(),
                          bevholt_b = numeric(),
                          bevholt_AICc = numeric())

for(x in use_stocks$stock_name){
  row <- which(use_stocks$stock_name == x)
  
  # create a tibble for each stock's biomass and recruit data
  stock <- data.frame(
    takers_rec[,1],
    takers_rec[,x],
    takers_ssb[,x],
    log(takers_rec[,x]))
  
  colnames(stock) <- c("year", "recruits", "sb", "logR")
  
  # remove model run in time
  min_year <- pull(use_stocks[row, "min_year"])
  max_year <- pull(use_stocks[row, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  # Change any 0 recruit values to 1
  stock <- stock %>%
    mutate(logR = replace(logR, logR == -Inf, 1)) %>%
    mutate(recruits = replace(recruits, recruits == 0, 1))
    
  
  # double check to make sure total number of years is at least 20
  if(count(stock) != 0 && count(stock) >= 20 ){
    
    # find starting values for nonlinear regression 
    ricker_starts <- srStarts(recruits~sb, data = stock, type = "Ricker", param = 1)
    bevholt_starts <- srStarts(recruits~sb, data = stock, type = "BevertonHolt", param = 1)
    
    if(ricker_starts[2] < 0){ricker_starts[2] <- 0.000001} # make sure ricker b param is positive
    if(bevholt_starts[1] < 0){bevholt_starts[1] <- 0.000001} # make sure bevholt a param is positive
    if(bevholt_starts[2] < 0){bevholt_starts[2] <- 0.000001} # make sure bevholt b param is positive
    
    # fit ricker and beverton holt models to stock
    ricker_fit <- nlxb(ricker, data = stock, start = ricker_starts)
    bevholt_fit <- nlxb(bevholt, data = stock, start = bevholt_starts)
    
    # calculate AICc for both models
    params <- 3
    n <- nrow(stock)
    ricker_AICc <- n*log(mean(ricker_fit$resid^2)) + ((2*params*(params+1)) /(n - params - 1))
    bevholt_AICc <- n*log(mean(bevholt_fit$resid^2)) + ((2*params*(params+1)) /(n - params - 1))
    
    # add data to filterd stocks tibble
    stock_model_fits <- stock_model_fits %>%
      add_row(stock_name = colnames(takers_rec[x]), ricker_a = ricker_fit$coefficients[1], 
              ricker_b = ricker_fit$coefficients[2], ricker_AICc = ricker_AICc, 
              bevholt_a = bevholt_fit$coefficients[1], bevholt_b = bevholt_fit$coefficients[2],
              bevholt_AICc = bevholt_AICc)
  }
}

# Plot stock recruit fits -------------------------------------------------------------------------------------
pdf("results/stock_sr_fits.pdf")
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
write.csv(dome_stocks, file = here("results", "dome_stocks.csv"))
write.csv(monotonic_stocks, file = here("results", "monotonic_stocks.csv"))
write.csv(stock_model_fits, file = here("results", "stock_model_fits.csv"))
write.csv(use_stocks, file = here("data", "use_stocks.csv"))
write.csv(takers_rec, file = here("data", "takers_rec.csv"))
write.csv(takers_ssb, file = here("data", "takers_ssb.csv"))

# Read in CSV files -------------------------------------------------------
dome_stocks <- read.csv(here("results", "dome_stocks.csv"))
dome_stocks <- as_tibble(dome_stocks[,-1])

monotonic_stocks <- read_csv(here("results", "monotonic_stocks.csv"))
monotonic_stocks <- as_tibble(monotonic_stocks[,-1])

stock_model_fits <- read_csv(here("results", "stock_model_fits.csv"))
stock_model_fits <- as_tibble(stock_model_fits[,-1])

use_stocks <- read_csv(here("data", "use_stocks.csv"))
use_stocks <- use_stocks[,-1]

takers_rec <- read_csv(here("data", "takers_rec.csv"))
takers_rec <- takers_rec[,-1]

takers_ssb <- read_csv(here("data", "takers_ssb.csv"))
takers_ssb <- takers_ssb[,-1]
