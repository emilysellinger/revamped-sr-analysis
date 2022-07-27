# going to calculate the contrast of each of the stock included in the analysis
stock_ssb_contrast <- tibble()
stock_ssb_contrast <- stock_ssb_contrast %>% 
  add_column(stock_name = "test",
             contrast = 1,
             driver = "test")


for(x in dome_stocks$stock_name){
  # pull data
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
  
  # find lower 5th and upper 5th quantile sb
  quants <- unname(quantile(pull(stock$sb), probs = c(0.05, 0.95)))
  
  stock_ssb_contrast <- stock_ssb_contrast %>% 
    add_row(stock_name = x, contrast = (quants[1]/quants[2]), driver = dome_stocks[row, "driver"][[1]])
  
}


for(x in monotonic_stocks$stock_name){
  # pull data
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
  
  # find lower 5th and upper 5th quantile sb
  quants <- unname(quantile(pull(stock$sb), probs = c(0.05, 0.95)))
  
  stock_ssb_contrast <- stock_ssb_contrast %>% 
    add_row(stock_name = x, contrast = (quants[1]/quants[2]), driver = monotonic_stocks[row, "driver"][[1]])
}


# Recruitment Variation ---------------------------------------------------
# will find recruitment variation for each stock, then print
# also want to look specifically at the edge stocks that were modified, to see if variation has
# gone down

stock_sigmaR <- tibble(stock_name = stock_model_fits$stock_name,
                       sigmaR = rep(NA, nrow(stock_model_fits)))

for(i in 1:nrow(stock_model_fits)){
  if(pull(stock_model_fits[i, "min_model"]) == "ricker" && pull(stock_model_fits[i, "rel_likelihood"]) >= 0.75){
    stock_sigmaR[i, "sigmaR"] <- pull(stock_model_fits[i, "ricker_sigmaR"])
  }else{
    stock_sigmaR[i, "sigmaR"] <- pull(stock_model_fits[i, "bevholt_sigmaR"])
  }
}

stock_sigmaR$driver <- rep(NA, nrow(stock_sigmaR))

for(i in 1:nrow(stock_sigmaR)){
  if(pull(stock_sigmaR[i, "stock_name"]) %in% sb_driven_stocks$stock_name){
    stock_sigmaR[i, "driver"] <- "spawning biomass"
  }else if(pull(stock_sigmaR[i, "stock_name"]) %in% env_driven_stocks$stock_name){
    stock_sigmaR[i, "driver"] <- "environment"
  }else{
    stock_sigmaR[i, "driver"] <- "edge case"
  }
}


# Plot results ------------------------------------------------------------

pdf(here("results/stock_contrast", "contrast_sigmaR_plots.pdf"))
a <- ggplot(data = stock_ssb_contrast) + 
  geom_boxplot(aes(x = driver, y = contrast), fill = "#00A1B7") +
  labs(x = "Primary influence on recruitment", y = "Spawning biomass depletion", subtitle = "(a)") +
  theme_minimal()
b <- ggplot(stock_sigmaR) + geom_boxplot(aes(x = driver, y = sigmaR), fill = "#00A1B7") + 
  xlab("Primary influence on recruitment") + ylab(expression(~ sigma*r)) + labs(subtitle = "(b)") +
  theme_minimal()
print(grid.arrange(a, b, nrow = 2))
dev.off()

