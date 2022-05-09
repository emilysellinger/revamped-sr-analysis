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
  stock <- tibble(year = takers_rec[,1],
                  recruits = takers_rec[, x],
                  sb = takers_ssb[, x])
  
  # remove model run in time
  row2 <- which(use_stocks$stock_name == x)
  min_year <- pull(use_stocks[row2, "min_year"])
  max_year <- pull(use_stocks[row2, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year) %>%
    mutate(recruits = replace(recruits, recruits == 0, 1))
  
  # Fit regime model
  fitPelt	<-cpt.mean(log(stock$recruits),method="PELT",test.stat="Normal",penalty="AIC",minseglen=6,pen.value=0.05)
  
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
  years <- seq(min_year, max_year, 1)
  
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
    ylim(0, 1) + labs(x = "year", y = "posterior prob of change point") + theme_minimal()
  
  # print both graphs
  print(grid.arrange(rec_ts_plot, level_change_plot, bcp_plot, nrow = 3))
}


# want to do this with stocks I did change point detection on
pdf("results/MARSS_test.pdf")
for(i in unique(env_change_pt$stock_name)){
  regime_change_plot(i)
}
dev.off()



# MARSS issues ------------------------------------------------------------
x <- "BSBASSMATLC"
row <- which(stock_model_fits$stock_name == x)

# create tibble with s-r data
stock <- tibble(year = takers_rec[,1],
                recruits = takers_rec[, x],
                sb = takers_ssb[, x])

# remove model run in time
row2 <- which(use_stocks$stock_name == x)
min_year <- pull(use_stocks[row2, "min_year"])
max_year <- pull(use_stocks[row2, "max_year"])

stock <- stock %>%
  filter(year >= min_year) %>%
  filter(year <= max_year) %>%
  mutate(recruits = replace(recruits, recruits == 0, 1))


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




# BCP detection -----------------------------------------------------------

x <- "SNROCKPCOAST"
row <- which(stock_model_fits$stock_name == x)

# create tibble with s-r data
stock <- tibble(year = takers_rec[,1],
                recruits = takers_rec[, x],
                sb = takers_ssb[, x])

# remove model run in time
row2 <- which(use_stocks$stock_name == x)
min_year <- pull(use_stocks[row2, "min_year"])
max_year <- pull(use_stocks[row2, "max_year"])

stock <- stock %>%
  filter(year >= min_year) %>%
  filter(year <= max_year) %>%
  mutate(recruits = replace(recruits, recruits == 0, 1))

bcp_fit <- bcp(log(stock$recruits), burnin = 100, mcmc = 1000)
