# Lifespan summary

# Load populated lifespan data ---------------------------------------------------------------------------------
# researched max age data for each of the stocks in the analysis, most from FishBase

lifespan <- read.csv(here("data/lifespan_analysis", "lifespan.csv"))

# Going to switch back to the stock naming conventions that the rest of the data frames have
lifespan$stock_name <- str_replace_all(lifespan$stock_name, "-", "\\.")

# Match max age to stock driver ------------------------------------------------------------------------------------
sb_driven_stocks <- left_join(sb_driven_stocks, lifespan)
env_driven_stocks <- left_join(env_driven_stocks, lifespan)
edge_stocks <- left_join(edge_stocks, lifespan)

sb_driven_stocks <- sb_driven_stocks %>%
  add_column(driver = "spawning biomass")
env_driven_stocks <- env_driven_stocks %>%
  add_column(driver = "environment")
edge_stocks <- edge_stocks %>%
  add_column(driver = "spbio or env") 

all_drivers <- rbind(sb_driven_stocks, env_driven_stocks, edge_stocks)

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

all_drivers %>% 
  summarise(unique(scientific_name)) # 182 species in analysis

all_drivers %>% 
  filter(!is.na(age)) %>% 
  summarise(unique(scientific_name)) # 157 with age data

all_drivers %>% 
  filter(is.na(age)) %>% 
  group_by(fishery_type) %>% 
  summarise(n = n())

# Summarize with recruitment regimes ------------------------------------------------------------------------------
env_change_pt %>%
  summarise(n = unique(stock_name)) # 172 stocks with regime changes

env_drivers <- rbind(env_driven_stocks, edge_stocks)

# Add lifespan data to change point data ----------------------------------------------------------------------------
env_change_pt <- left_join(env_change_pt, env_drivers)

# calculate average regime length/sd across all ages
env_change_pt %>%
  summarise(mean = mean(regime_length), sd = sd(regime_length))

# find the number of stocks with age data
env_change_pt %>% 
  filter(!is.na(age)) %>% 
  summarise(unique(stock_name))




# create a scatter plot of age versus regime length
fit <- lm(regime_length ~ age, data = env_change_pt)
summary(fit)
pdf(here("results/lifespan_analysis", "max_age_regime_plot.pdf"))
a <- ggplot(env_change_pt) + 
  geom_point(aes(x = age, y = regime_length, color = as.factor(fishery_type))) + 
  geom_abline(slope = 0.04876, intercept = 14.67006) + 
  ylab("Regime length") + xlab("Species maximum age") +
  scale_color_manual(values = natparks.pals("Banff", n = 9, type = "continuous"), name = "Fishery type") +
  theme_minimal()
print(a)
dev.off()


# want to investigate if the number of regime shifts is related to age of the species
counts <- counts %>%
  left_join(lifespan)

pdf(here("results/lifespan_analysis", "nregime_age_boxplot.pdf"))
a <- ggplot(counts) +
  geom_boxplot(aes(y = as.factor(nregimes), x = age), fill = "#00A1B7") + 
  labs(x = "Species maximum age", y = "Number of recruitment regimes") +
  theme_minimal()
print(a)
dev.off()



counts %>% filter(age <= 10) %>% summarise(avg_regimes = mean(n))
counts %>% filter(age > 10) %>% filter(age <= 20) %>% summarise(avg_regimes = mean(n))
counts %>% filter(age > 20) %>% filter(age <= 40) %>% summarise(avg_regimes = mean(n))
counts %>% filter(age > 40) %>% summarise(avg_regimes = mean(n))


# Median regime length ----------------------------------------------------
quantile(env_change_pt$regime_length)



# Stock area IDs ----------------------------------------------------
stock_info <- read_csv(here("ram_stock_info.csv"))
area_info <- read_csv(here("ram_area_ids.csv"))

stock_info <- left_join(stock_info, area_info)
write_csv(stock_info, file = here("stock_area_ids.csv"))
