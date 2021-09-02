# Lifespan summary 

# Load data --------------------------------------------------------------------------------------------------
taxonomy <- read.csv(here("data", "taxonomy.csv")) # taxonomic info for each species

stock_info <- read.csv(here("data", "stock_info.csv")) # information for each stock, links to taxonomic data


# Match taxonomic info to current stocks ---------------------------------------------------------------------

# in the taxonomy and stock info datasets, there are some discrepancies in the naming conventions for some stocks
# a - is used instead of . To deal with this, going to replace the . w/ -
filtered_stocks$stock_name <- str_replace_all(filtered_stocks$stock_name, "\\.", "-")

lifespan <- tibble(stock_name = "",
                         scientific_name = "",
                         common_name = "",
                         region = "",
                         tsn = numeric())
# match data
for(x in filtered_stocks$stock_name){
  
  # find row in stock info dataset
  row <- which(stock_info$stockid == x)
  
  lifespan <- lifespan %>%
    add_row(stock_name = x, scientific_name = stock_info[row, "scientificname"], common_name = stock_info[row, "commonname"],
            region = stock_info[row, "region"], tsn = stock_info[row, "tsn"])
}

# Add fishery type data --------------------------------------------------------------------------------------
stock_lifespan <- stock_lifespan %>%
  add_column(fishery_type = "")

for(x in stock_lifespan$tsn){
  row <- which(stock_lifespan$tsn == x)
  row2 <- which(taxonomy$tsn == x)
  
  stock_lifespan[row, "fishery_type"] <- taxonomy[row2,"FisheryType"]
}

# Write lifespan data frame to csv --------------------------------------------------------------------------------
write_csv(lifespan, file.path("data", "lifespan.csv"))

# Re-load populated lifespan data ---------------------------------------------------------------------------------
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
  group_by(stock_name) %>%
  summarise(n = n())

env_drivers <- rbind(env_driven_stocks, edge_stocks)
env_change_pt <- env_change_pt %>%
  left_join(env_drivers)
env_change_pt %>%
  summarise(mean = mean(regime_length), sd = sd(regime_length))
  
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


