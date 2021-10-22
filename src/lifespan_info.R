# Lifespan summary

# Load populated lifespan data ---------------------------------------------------------------------------------
# researched max age data for each of the stocks in the analysis, most from FishBase

lifespan <- read.csv(here("data", "lifespan.csv"))

# Going to switch back to the stock naming conventions that the rest of the data frames have
lifespan$stock_name <- str_replace_all(lifespan$stock_name, "-", "\\.")

# Match max age to stock driver ------------------------------------------------------------------------------------
sb_driven_stocks <- sb_driven_stocks %>%
  add_column(age = 1)
for(x in sb_driven_stocks$stock_name){
  row <- which(sb_driven_stocks$stock_name == x)
  row2 <- which(lifespan$stock_name == x)
  
  sb_driven_stocks[row, "age"] <- lifespan[row2, "age"]
}

env_driven_stocks <- env_driven_stocks %>%
  add_column(age = 1)
for(x in env_driven_stocks$stock_name){
  row <- which(env_driven_stocks$stock_name == x)
  row2 <- which(lifespan$stock_name == x)
  
  env_driven_stocks[row, "age"] <- lifespan[row2, "age"]
}

edge_stocks <- edge_stocks %>%
  add_column(age = 1)
for(x in edge_stocks$stock_name){
  row <- which(edge_stocks$stock_name == x)
  row2 <- which(lifespan$stock_name == x)
  
  edge_stocks[row, "age"] <- lifespan[row2, "age"]
}
# Summarize data ----------------------------------------------------------------------------------------------------
sb_driven_stocks %>%
  group_by(curve_shape) %>%
  summarise(mean = mean(age, na.rm = TRUE), n = n())

env_driven_stocks %>%
  group_by(curve_shape) %>%
  summarise(mean = mean(age, na.rm = TRUE), n = n())

edge_stocks %>%
  summarise(mean = mean(age, na.rm = TRUE))

sb_driven_stocks %>%
  group_by(curve_shape) %>%
  summarise(qs = quantile(age, c(0.25, 0.75), na.rm = TRUE), prob = c(0.25, 0.75))

env_driven_stocks %>%
  group_by(curve_shape) %>%
  summarise(qs = quantile(age, c(0.25, 0.75), na.rm = TRUE), prob = c(0.25, 0.75))

edge_stocks %>%
  summarise(qs = quantile(age, c(0.25, 0.75), na.rm = TRUE), prob = c(0.25, 0.75))

sb_driven_stocks <- sb_driven_stocks %>%
  add_column(driver = "spawning biomass")
env_driven_stocks <- env_driven_stocks %>%
  add_column(driver = "environment")
edge_stocks <- edge_stocks %>%
  add_column(driver = "spbio or env") 

all_drivers <- rbind(sb_driven_stocks, env_driven_stocks, edge_stocks)

# Summarize with recruitment regimes ------------------------------------------------------------------------------
env_change_pt %>%
  summarise(n = unique(stock_name)) # 327 stocks with regime changes

env_drivers <- rbind(env_driven_stocks, edge_stocks)

# Add lifespan data to change point data ----------------------------------------------------------------------------
env_change_pt <- env_change_pt %>%
  left_join(env_drivers)

# calculate average regime length/sd across all ages
env_change_pt %>%
  summarise(mean = mean(regime_length), sd = sd(regime_length))

# want to determine what the mean regime length/sd is for different age categories
# first will determine what seems like an appropriate age breakdown
env_change_pt %>%
  summarise(min_age = min(age, na.rm = TRUE), max_age = max(age, na.rm = TRUE))

# get total number of stocks for each age category
env_change_pt %>%
  filter(age <= 10) %>%
  summarise(stock_name = unique(stock_name))

env_change_pt %>%
  filter(age > 10) %>%
  filter(age <= 20) %>%
  summarise(n = unique(stock_name))

env_change_pt %>%
  filter(age > 20) %>%
  filter(age <= 40) %>%
  summarise(n = unique(stock_name))

env_change_pt %>%
  filter(age > 40) %>%
  summarise(n = unique(stock_name))

# calculate mean regime length/sd for each category
env_change_pt %>%
  filter(age <= 10) %>%
  summarise(mean_length = mean(regime_length), sd = sd(regime_length))

env_change_pt %>%
  filter(age > 10) %>%
  filter(age <= 20) %>%
  summarise(mean_length = mean(regime_length), sd = sd(regime_length))

env_change_pt %>%
  filter(age > 20) %>%
  filter(age <= 40) %>%
  summarise(mean_length = mean(regime_length), sd = sd(regime_length))

env_change_pt %>%
  filter(age > 40) %>%
  summarise(mean_length = mean(regime_length), sd = sd(regime_length))

# want to know how many stocks with regime changes don't have age data
env_change_pt %>%
  filter(is.na(age)) %>%
  summarise(stocks = unique(stock_name))


# want some general information about the stocks in the analysis
lifespan %>%
  summarise(species = unique(scientific_name))

lifespan %>%
  filter(is.na(age)) %>%
  summarise(species = unique(scientific_name))
