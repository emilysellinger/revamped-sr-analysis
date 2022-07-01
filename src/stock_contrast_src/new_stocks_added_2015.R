cody_stocks <- read_csv(here("data/stock_contrast/cody_stocks.csv"))

added_stocks <- read_csv(here("data/stock_contrast/new_stocks_since_15.csv"))

ggplot(data = added_stocks, aes(x = fishery_type)) + geom_bar() +
  labs(x = "fishery type", title = "Stocks added since Szuwalski (2015)") +
   theme_minimal() + coord_flip() 

ggplot(data = added_stocks, aes(x = region)) + geom_bar() +
  labs(x = "primary stock region", title = "Stocks added since Szuwalski (2015)") +
  theme_minimal() + coord_flip()


# Compare Cody's stocks with current ones (3 stocks are messed up, not going to fix right now)
cody_edge_stocks <- cody_stocks %>%
  filter(original_driver == "edge case") %>% 
  filter(is.na(change)) %>% 
  filter(is.na(note))

cody_edge_stocks$driver <- rep(NA, nrow(cody_edge_stocks))

cody_edge_stocks[c(12, 14, 34), "driver"] <- "spawning biomass"
cody_edge_stocks[c(1, 3, 6, 7, 10, 11, 15, 17, 27, 29, 30, 32, 36, 37), "driver"] <- "environment"

for(i in 1:nrow(cody_edge_stocks)){
  if(is.na(cody_edge_stocks[i,"driver"])){
    cody_edge_stocks[i, "driver"] <- "edge case"
  }
}


# will calculate difference in years
added_time <- rep(NA, nrow(cody_edge_stocks))
for(i in 1:nrow(cody_edge_stocks)){
  
  stock_id <- pull(cody_edge_stocks[i, "stock_name"])
  old_max_yr <- pull(cody_edge_stocks[i, "old_max_year"])
  stock <- retrieve_sr_data(stock_id)
  
  new_max_yr <- tail(stock, n = 1)$year
  
  added_time[i] <- new_max_yr - old_max_yr
}

cody_edge_stocks$added_time <- added_time

ggplot(cody_edge_stocks) + geom_boxplot(aes(x = driver, y = added_time)) 
# no real difference between categories

