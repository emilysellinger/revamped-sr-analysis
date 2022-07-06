library(MARSS)
library(tidyverse)
library(here)
library(onlineBcp)
library(bcp)
library(gridExtra)
RShowDoc("Chapter_StructuralBreaks.R", package = "MARSS")

# MARSS Structural Break example
# will use some of the data with identified regime changes (as indicated by PELT) 

# Plotting function -------------------------------------------------------
regime_change_plot <- function(x){
  row <- which(stock_model_fits$stock_name == x)
  
  # create tibble with s-r data
  stock <- retrieve_sr_data(x)
  
  # Fit regime model
  fitPelt	<-cpt.meanvar(log(stock$recruits),method="PELT",test.stat="Normal",penalty="AIC",minseglen=6)
  
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
  common_name <- as.character(lifespan[row3, "common_name"])
  region <- paste("Region:", as.character(lifespan[row3, "region"]))
  stock_id <- paste("Stock ID:", as.character(lifespan[row3, "stock_name"]))
  
  # recruitment time series plot
  rec_ts_plot <- ggplot(data = stock, aes(x = year, y = recruits)) +
    geom_line(size = 1) +
    geom_rect(data = rectangle_data, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin,
                                                              ymax = ymax, fill = as.factor(color_factor)), alpha = 0.3) +
    guides(fill = "none") +
    labs(x = "Year", y = "Recruits", title = common_name, subtitle = paste(region, stock_id, sep = "\n")) +
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
    ylim(-4,4) + labs(y = "standardized residuals", x = "year") + theme_minimal()
  
  
  # BCP analysis of log recruitment change points
  bcp_fit <- bcp(log(stock$recruits))
  bcp_dat <- tibble(year = years,
                    pt_prob = bcp_fit$posterior.prob)
  
  bcp_plot <- ggplot(data = bcp_dat, aes(x = year, y = pt_prob)) + geom_line() +
    ylim(0, 1) + labs(x = "year", y = "posterior prob of change point") + 
    geom_hline(yintercept = 0.75, linetype = 2) + theme_minimal()
  
  # print both graphs
  print(grid.arrange(rec_ts_plot, level_change_plot, bcp_plot, nrow = 3))
}


# want to do this with stocks I did change point detection on
pdf("results/changepoint_comparison/regime_detection_comparison.pdf")
for(i in unique(env_change_pt$stock_name)){
  regime_change_plot(i)
}
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

# combine with regime count data
cody_stocks2 <- cody_stocks2 %>% 
  left_join(counts2, by = "stock_name")

# remove stocks that were reclassified
cody_stocks2 <- cody_stocks2 %>% 
  filter(!is.na(nregimes))

a <- cody_stocks2 %>% 
  filter(regime_changes == nshifts)
b <- cody_stocks2 %>% 
  filter(regime_changes < nshifts)
c <- cody_stocks2 %>% 
  filter(regime_changes > nshifts)



# STARS Algorithm ---------------------------------------
# Note: is not working. I don't know why
library(rshift)

env_influenced <- rbind(env_driven_stocks, edge_stocks)
stars_cpt <- tibble()

stock <- retrieve_sr_data("CHTRACCH")
fit <- Rodionov(stock, "resids", "year", 32, prob = 0.1, merge = TRUE)
RSI_graph(fit, "resids", "year", "RSI")


fit2 <- cpt.mean(stock$logR, method = "PELT", minseglen = 6, penalty = "BIC")

stock$resids <- (stock$logR - mean(stock$logR))/sd(stock$logR)

# Going to just compare the regimes over the original years of Cody's analysis
cody_env_change_pt <- tibble(
  stock_name = "",
  regime_length = numeric())

for(x in cody_stocks2$stock_name){
  # get stock and recruitment data
  stock <- retrieve_old_sr_data(x)
  
  stock <- stock[complete.cases(stock), ]
  # Fit regime model
  fitPelt	<-cpt.mean(log(stock$recruits),method="PELT",test.stat="Normal",penalty="AIC",minseglen=6)
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


counts <- cody_env_change_pt %>%
  count(stock_name) %>% 
  rename(nregimes = n) %>%  # number of regimes in time series
  mutate(nshifts = nregimes - 1) # number of times regime shift (1 regime = 0 shifts)


counts %>% filter(nshifts > 0) # 62 have regime shifts

# combine with regime count data
cody_stocks2 <- cody_stocks2 %>% 
  left_join(counts, by = "stock_name")

# remove stocks that were reclassified
cody_stocks2 <- cody_stocks2 %>% 
  filter(!is.na(nregimes))

regime_comp <- cody_stocks2 %>% 
  select(stock_name, nshifts, regime_changes) %>% 
  mutate(diffs = nshifts - regime_changes)

pdf(here("results", "peltVstars.pdf"))
a <- ggplot(regime_comp, aes(diffs)) + 
  geom_histogram(color = "black", fill = "gray", binwidth = 1) + 
  xlab("difference in number of identified regime shifts") +
  geom_vline(xintercept = -1.2, linetype = 2) +
  theme_minimal()
print(a)
dev.off()
