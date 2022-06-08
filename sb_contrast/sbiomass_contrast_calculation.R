# going to calculate the contrast of each of the stock included in the analysis
# two measures of contrast - current SSB/max SSB and min SSB/max SSB
stock_ssb_contrast <- tibble()
stock_ssb_contrast <- stock_ssb_contrast %>% 
  add_column(stock_name = "test",
             contrast = 1,
             driver = "test")


for(x in dome_stocks$stock_name){
  # pull data
  row <- which(dome_stocks$stock_name == x)
  row2 <- which(use_stocks$stock_name == x)
  
  stock <- tibble(
    year = takers_rec[,1],
    sb = takers_ssb[,x])
  
  # remove model run in time
  min_year <- pull(use_stocks[row2, "min_year"])
  max_year <- pull(use_stocks[row2, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  # find lower 5th and upper 5th quantile sb
  quants <- unname(quantile(pull(stock$sb), probs = c(0.05, 0.95)))
  
  stock_ssb_contrast <- stock_ssb_contrast %>% 
    add_row(stock_name = x, contrast = (quants[1]/quants[2]), driver = dome_stocks[row, "driver"][[1]])
  
}


for(x in monotonic_stocks$stock_name){
  # pull data
  row <- which(monotonic_stocks$stock_name == x)
  row2 <- which(use_stocks$stock_name == x)
  
  stock <- tibble(
    year = takers_rec[,1],
    sb = takers_ssb[,x])
  
  # remove model run in time
  min_year <- pull(use_stocks[row2, "min_year"])
  max_year <- pull(use_stocks[row2, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  # find lower 5th and upper 5th quantile sb
  quants <- unname(quantile(pull(stock$sb), probs = c(0.05, 0.95)))
  
  stock_ssb_contrast <- stock_ssb_contrast %>% 
    add_row(stock_name = x, contrast = (quants[1]/quants[2]), driver = monotonic_stocks[row, "driver"][[1]])
}

pdf(here("sb_contrast", "all_stocks_contrast_boxplot.pdf"))
a <- ggplot(data = stock_ssb_contrast) + geom_boxplot(aes(x = type, y = contrast)) + 
  labs(x = "contrast type", y = "spawning biomass contrast", 
       title = "Comparision of current and historical spawning biomass contrast",
       subtitle = "calculated for each of the 405 stocks in the analysis")
print(a)
dev.off()

# will do this again with stocks by primary influence to see if there are trends
pdf(here("sb_contrast", "primary_influence_contrast_boxplot.pdf"))
a <- ggplot(data = stock_ssb_contrast) + 
  geom_boxplot(aes(x = driver, y = contrast)) +
  labs(title = "Historical spawning biomass depletion",
       subtitle = "depletion = 0.05 quantile/0.95 quantile ",
       x = "primary influence on recruitment", y = "spawning biomass depletion")
print(a)
dev.off()
