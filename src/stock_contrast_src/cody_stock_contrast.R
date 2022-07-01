# going to calculate contrast for Cody's stocks
# got these from his supplementary information

cody_stocks <- read_csv(here("data/stock_contrast/cody_stocks.csv"))

# i'm going to first just calculate contrast for stocks that haven't been modified
# beyond having more data added
cody_stocks <- cody_stocks %>%
  filter(is.na(change)) %>% 
  filter(is.na(note))
# need to remove CODCOASTNOR (why??)
cody_stocks2 <- cody_stocks2[-37,]

cody_stocks2$Odepletion <- rep(NA, 178)
cody_stocks2$Cdepletion <- rep(NA, 178)


for(x in cody_stocks2$stock_name){
  row <- which(cody_stocks2$stock_name == x)
  row2 <- which(use_stocks$stock_name == x)
  
  # Cody's data
  stock <- tibble(
    year = takers_rec[,1],
    sb = takers_ssb[,x])
  
  # remove model run in time
  min_year <- cody_stocks2[row, "old_min_year"]
  max_year <- cody_stocks2[row, "old_max_year"]
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  # Current data
  stock2 <- tibble(
    year = takers_rec[,1],
    sb = takers_ssb[,x])
  
  # remove model run in time
  min_year2 <- pull(use_stocks[row2, "min_year"])
  max_year2 <- pull(use_stocks[row2, "max_year"])
  
  stock2 <- stock2 %>%
    filter(year >= min_year2) %>%
    filter(year <= max_year2)
  
  # find lower 5th and upper 5th quantile sb
  quants1 <- unname(quantile(pull(stock$sb), probs = c(0.05, 0.95), na.rm = TRUE))
  quants2 <- unname(quantile(pull(stock2$sb), probs = c(0.05, 0.95), na.rm = TRUE))
  
  # save depletion levels
  cody_stocks2[row, "Odepletion"] <- quants1[1]/quants1[2]
  cody_stocks2[row, "Cdepletion"] <- quants2[1]/quants2[2]
}

# print results
cody_stocks3 <- pivot_longer(cody_stocks2, 
                             !c(stock_name, old_min_year, old_max_year, original._driver, change, note),
                             names_to = "analysis", values_to = "depletion")
cody_stocks3$analysis <- sub("Odepletion", "Szuwalski et al", cody_stocks3$analysis)
cody_stocks3$analysis <- sub("Cdepletion", "Current", cody_stocks3$analysis)

pdf(here("sb_contrast", "cody_stocks_contrast_boxplot.pdf"))
a <- ggplot(data = cody_stocks3) + geom_boxplot(aes(x = original._driver, y = depletion, fill = analysis)) + 
  labs(x = "original recruitment driver classification", y = "depletion", 
       title = "Historical spawning biomass depletion",
       subtitle = "calculated for 178 of the 224 stocks in Szuwalski et al. (2015)")
print(a)
dev.off()


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

pdf(here("results/stock_contrast", "stock_rec_simga.pdf"))
a <- ggplot(stock_sigmaR) + geom_boxplot(aes(x = driver, y = sigmaR)) + 
  xlab("primary influence on recruitment")
print(a)
dev.off()
