
# Functions -------------------------------------------------------
regime_change_plot <- function(x){
  row <- which(stock_model_fits$stock_name == x)
  
  # create tibble with s-r data
  stock <- retrieve_sr_data(x)
  
  # Fit regime model
  fitPelt	<-cpt.mean(log(stock$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                     pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
  
  # save change point locations for regimes
  changes	<- fitPelt@cpts
  
  # create tibble for recruitment regime plots
  rectangle_data <- tibble(xmin = numeric(),
                           xmax = numeric(),
                           ymin = numeric(),
                           ymax = numeric(),
                           color_factor = numeric())
  z <- 1
  regime_colors <- rep(0, nrow(stock))
  ymin <- min(stock$recruits)
  ymax <- max(stock$recruits)
  
  # save regime mean data
  for(y in 1:length(changes))
  {
    if(y==1)
      ind1	<-1
    if(y>1)
      ind1	<-changes[y-1]+1
    ind2	<-changes[y]
    data	<-stock$recruits[ind1:ind2] # retrieve recruit data from each regime
    
    period	<-seq(ind1,ind2)
    regime_colors[period] <- z # number each regime, for plotting
    
    xmin <- stock$year[ind1]
    xmax <- stock$year[ind2]
    
    rectangle_data <- rectangle_data %>%
      add_row(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color_factor = z)
    z <- z + 1
  }
  # add regime factor to spawning biomass recruitment data
  stock <- stock %>%
    add_column(color_factor = as.factor(regime_colors))
  
  # Make string with common name and stock location
  row3 <- which(lifespan$stock_name == x)
  species_name <- as.character(lifespan[row3, "scientific_name"])
  region <- paste("Region:", as.character(lifespan[row3, "region"]))
  stock_id <- paste("Stock ID:", as.character(lifespan[row3, "stock_name"]))
  
  # recruitment time series plot
  rec_ts_plot <- ggplot(data = stock, aes(x = year, y = recruits)) +
    geom_line(size = 1) +
    geom_rect(data = rectangle_data, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin,
                                                              ymax = ymax, fill = as.factor(color_factor)), alpha = 0.5) +
    guides(fill = "none") +
    labs(x = "Year", y = "Recruits", title = stock_id, subtitle = paste(species_name, region, "(a)", sep = "\n")) +
    scale_fill_manual(values = natparks.pals("Banff")) + 
    theme_minimal()
  
  # MARSS stochastic level model
  mod <- list(
    Z = matrix(1), A = matrix(0), R = matrix("r"),
    B = matrix(1), U = matrix(0), Q = matrix("q"),
    x0 = matrix("pi")
  )
  
  # fit model
  dat <- t(as.matrix(log(stock$recruits)))
  rownames(dat) <- "logrecruits"
  kem.2 <- MARSS(dat, model = mod, silent = TRUE, method = "BFGS")

  # get residuals
  resids <- MARSSresiduals(kem.2, type = "tT")$mar.residuals
  
  # plot test for level changes
  years <- seq(head(stock$year, 1), tail(stock$year, 1), 1)
  
  # create data frame for ggplot
  level_changes <- tibble(year = years,
                          std_resids = resids[2,])
  
  
  level_change_plot <- ggplot(data = level_changes, aes(x = year, y = std_resids)) + geom_line() +
    geom_hline(yintercept = -2, linetype = "dashed") + geom_hline(yintercept = 2, linetype = "dashed") +
    ylim(-4,4) + labs(y = "Standardized residuals", x = "Year", subtitle = "(b)") + theme_minimal()
  
  
  # BCP analysis of log recruitment change points
  bcp_fit <- bcp(log(stock$recruits))
  bcp_dat <- tibble(year = years,
                    pt_prob = bcp_fit$posterior.prob)
  
  bcp_plot <- ggplot(data = bcp_dat, aes(x = year, y = pt_prob)) + geom_line() +
    ylim(0, 1) + labs(x = "Year", y = "Posterior prob of change point", subtitle = "(c)") + 
    geom_hline(yintercept = 0.75, linetype = 2) + theme_minimal()
  
  # print both graphs
  print(grid.arrange(rec_ts_plot, level_change_plot, bcp_plot, nrow = 3))
}



# Method Comparison -------------------------------------------------------

# plot the recruitment regimes identified by each of the three methods
pdf("results/changepoint_comparison/regime_detection_comparison_AICC.pdf")
for(i in unique(env_change_pt$stock_name)){
  regime_change_plot(i)
}
dev.off()

# Also want to determine the difference in the number of regimes for each of the stocks
# create tibble with s-r data
shift_comp <- counts2 %>% 
  filter(nshifts > 0) %>% 
  select(stock_name, nshifts) %>% 
  rename(nshifts_PELT = nshifts) %>% 
  add_column(nshifts_MARSS = rep(NA, 163),
             nshifts_BCP = rep(NA, 163))

# Looking at the graphs from above, I already know some have degenerate solutions for MARSS
# will remove those stocks from the analysis below
degen_stocks <- c("GHALNEAR", "SNAPSAUSSSG", "SOLENS", "ARFLOUNDPCOAST", "ATLCROAKMATLC", "CHILISPCOAST", 
                  "GRSNAPGM", "HERR4RSP", "PHAKEPCOAST", "SABLEFEBSAIGA", "SEELNSSA4", "COD3M", "CSALMSSR", 
                  "PSALMANADYRPW", "PSALMMONTD", "PSALMPDICK", "PSALMPRIMPW", "PSALMROCKYROD", "PSALMWINDYC",
                  "SSALMSEYMOUR")
non_degen_stocks <- shift_comp %>% 
  select(stock_name) %>% 
  filter(!(stock_name %in% degen_stocks))

for(x in non_degen_stocks$stock_name){
  row <- which(shift_comp$stock_name == x)
  stock <- retrieve_sr_data(x)
    
  # MARSS stochastic level model
  mod <- list(
    Z = matrix(1), A = matrix(0), R = matrix("r"),
    B = matrix(1), U = matrix(0), Q = matrix("q"),
    x0 = matrix("pi")
  )
    
  # fit model
  dat <- t(as.matrix(log(stock$recruits)))
  rownames(dat) <- "logrecruits"
  kem.2 <- MARSS(dat, model = mod, silent = TRUE, method = "BFGS")
    
  # get residuals
  resids <- MARSSresiduals(kem.2, type = "tT")$mar.residuals
  state_resids <- resids[2,]
  shift_comp[row, "nshifts_MARSS"] <- sum(state_resids > 2 | state_resids < -2, na.rm = T)
    
  # BCP method
  bcp_fit <- bcp(log(stock$recruits))
  posterior_probs <- bcp_fit$posterior.prob
  shift_comp[row, "nshifts_BCP"] <- sum(posterior_probs >= 0.75, na.rm = T)
  
}

for(x in degen_stocks){
  row <- which(shift_comp$stock_name == x)
  stock <- retrieve_sr_data(x)
  
  # BCP method
  bcp_fit <- bcp(log(stock$recruits))
  posterior_probs <- bcp_fit$posterior.prob
  shift_comp[row, "nshifts_BCP"] <- sum(posterior_probs >= 0.75, na.rm = T)
}

shift_comp2 <- shift_comp %>% 
  pivot_longer(!stock_name, names_to = "method", values_to = "nshifts")

shift_comp2$method <- replace(shift_comp2$method, shift_comp2$method == "nshifts_PELT", "PELT")
shift_comp2$method <- replace(shift_comp2$method, shift_comp2$method == "nshifts_MARSS", "MARSS")
shift_comp2$method <- replace(shift_comp2$method, shift_comp2$method == "nshifts_BCP", "BCP")

pdf(here("results/changepoint_comparison", "nshift_method_boxplot.pdf"))
a <- ggplot(shift_comp2) + 
  geom_boxplot(aes(x = method, y = nshifts), fill = "#00A1B7") + 
  labs(y = "Number of regime shifts", x = "Method") + theme_minimal()
print(a)
dev.off()


# Comparison of regimes to Szuwalski et al --------------------------------
# will determine how many stocks in original analysis had at least 1 regime
# shift and if it has changed since adding

cody_stocks <- read_csv(here("data/stock_contrast/cody_stocks.csv"))
View(cody_stocks)

# extract the stocks that are environmentally influenced and not modified
cody_stocks2 <- cody_stocks %>% 
  filter(original_driver != "spawning biomass") %>% 
  filter(is.na(change))

# Going to just compare the regimes over the original years of Cody's analysis
cody_env_change_pt <- tibble(
  stock_name = "",
  regime_length = numeric())

for(x in cody_stocks2$stock_name){
  # get stock and recruitment data
  stock <- retrieve_old_sr_data(x)
  
  stock <- stock[complete.cases(stock), ]
  # Fit regime model
  fitPelt	<-cpt.mean(log(stock$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                     pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
  changes	<- fitPelt@cpts
  #print(changes)
  # calculate regime length
  for(y in 1:length(changes))
  {
    if(y==1)
      ind1	<- 1
    if(y>1)
      ind1	<-changes[y-1]+1
    ind2	<-changes[y]
    regime_length	<-length(stock$recruits[ind1:ind2])
    
    cody_env_change_pt <- cody_env_change_pt %>%
      add_row(stock_name = x,
              regime_length = regime_length)
  }
}


cody_counts <- cody_env_change_pt %>%
  count(stock_name) %>% 
  rename(nregimes = n) %>%  # number of regimes in time series
  mutate(nshifts = nregimes - 1) # number of times regime shift (1 regime = 0 shifts)


cody_counts %>% filter(nshifts > 0) # 60 have regime shifts

# combine with regime count data
cody_stocks2 <- cody_stocks2 %>% 
  left_join(cody_counts, by = "stock_name")

# remove stocks that were reclassified
cody_stocks2 <- cody_stocks2 %>% 
  filter(!is.na(nregimes))

regime_comp <- cody_stocks2 %>% 
  select(stock_name, nshifts, regime_changes) %>% 
  mutate(diffs = nshifts - regime_changes)

plot_caption <- str_wrap("Histogram of the difference in the number of indentified regimes shifts for PELT and STARS. Recruitment regimes were identified for each of the 371 stocks with some influence of environment on recruitment. The dotted line represents the mean difference in the number of recruitment regimes. On average, PELT identified one fewer regime than STARS.", 110)

pdf(here("results/changepoint_comparison", "peltVstars_AICC.pdf"))
a <- ggplot(regime_comp) + 
  geom_histogram(aes(diffs), color = "#006475", fill = "#00A1B7", alpha = 0.8, binwidth = 1) + 
  xlab("Difference in number of identified regime shifts") + ylab("Count") +
  labs(caption = plot_caption) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0)) +
  geom_vline(xintercept = -1.3, linetype = 2) 
print(a)
dev.off()


# checking
cody_stocks3 <- cody_stocks %>% 
  filter(regime_changes > 0)

cody_stocks3 <- left_join(cody_stocks3, all_drivers)

cody_stocks3 <- cody_stocks3 %>% filter(original_driver != "spawning biomass") %>% 
  filter(driver != "spawning biomass")

cody_env_change_pt2 <- tibble(
  stock_name = "",
  regime_length = numeric())

for(x in cody_stocks3$stock_name){
  # get stock and recruitment data
  stock <- retrieve_old_sr_data(x)
  
  stock <- stock[complete.cases(stock), ]
  # Fit regime model
  fitPelt	<-cpt.mean(log(stock$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                     pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
  changes	<- fitPelt@cpts
  #print(changes)
  # calculate regime length
  for(y in 1:length(changes))
  {
    if(y==1)
      ind1	<- 1
    if(y>1)
      ind1	<-changes[y-1]+1
    ind2	<-changes[y]
    regime_length	<-length(stock$recruits[ind1:ind2])
    
    cody_env_change_pt <- cody_env_change_pt %>%
      add_row(stock_name = x,
              regime_length = regime_length)
  }
}


cody_counts2 <- cody_env_change_pt2 %>%
  count(stock_name) %>% 
  rename(nregimes = n) %>%  # number of regimes in time series
  mutate(nshifts = nregimes - 1) # number of times regime shift (1 regime = 0 shifts)


cody_counts %>% filter(nshifts > 0) # 60 have regime shifts

# combine with regime count data
cody_stocks3 <- cody_stocks3 %>% 
  left_join(cody_counts, by = "stock_name")

regime_comp2 <- cody_stocks3 %>% 
  select(stock_name, nshifts, regime_changes) %>% 
  mutate(diffs = nshifts - regime_changes)
View(regime_comp2)
