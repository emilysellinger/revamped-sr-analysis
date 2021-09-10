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
  summarise(n = unique(stock_name)) # 82 stocks with regime changes

env_drivers <- rbind(env_driven_stocks, edge_stocks)

# add lifespan data to change point data
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


# Data visualization -----------------------------------------------------------------------------------------------


ggplot(sb_driven_stocks, aes(x = age, color = curve_shape,  fill = curve_shape)) +
  geom_histogram(binwidth = 5, alpha = 0.5)

ggplot(env_driven_stocks, aes(x = age, color = curve_shape,  fill = curve_shape)) +
  geom_histogram(binwidth = 5, alpha = 0.5)

ggplot(edge_stocks, aes(x = age)) +
  geom_histogram(binwidth = 5, alpha = 0.5)

# doesn't seem to be much difference in max age distributions among spawning biomass and environmentally driven stocks
# will make another graph with both drivers to confirm

ggplot(all_drivers, aes(x = age, color = driver, fill = driver)) +
  geom_histogram(binwidth = 5, alpha = 0.5)

# will compare age and regime length
ggplot(env_change_pt, aes(x = age, y = regime_length, color = curve_shape)) +
  geom_point() +
  geom_text(aes(label = ifelse(age > 100, as.character(stock_name),"")), hjust = 0, vjust = 0)

ggplot(env_change_pt, aes(x = age, y = regime_length, color = curve_shape)) +
  geom_point() +
  coord_fixed(xlim = c(0,100))


# want some general information about the stocks in the analysis
stock_info <- rename(stock_info, stock_name = stockid)

all_drivers <- all_drivers %>%
  left_join(stock_info)
all_drivers %>%
  summarise(total_reg = unique(region))

all_drivers %>%
  summarise(total_species = unique(scientificname))
