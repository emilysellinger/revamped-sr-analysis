added_stocks <- read.csv(here("new_stocks_since_15.csv"))

added_stocks <- as_tibble(added_stocks)

ggplot(data = added_stocks, aes(x = fishery_type)) + geom_bar() +
  labs(x = "fishery type", title = "Stocks added since Szuwalski (2015)") +
   theme_minimal() + coord_flip() 

ggplot(data = added_stocks, aes(x = region)) + geom_bar() +
  labs(x = "primary stock region", title = "Stocks added since Szuwalski (2015)") +
  theme_minimal() + coord_flip()
