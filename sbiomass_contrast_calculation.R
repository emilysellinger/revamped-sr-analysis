# going to calculate the contrast of each of the stock included in the analysis
# two measures of contrast - current SSB/max SSB and min SSB/max SSB
stock_ssb_contrast <- tibble()
stock_ssb_contrast <- stock_ssb_contrast %>% 
  add_column(stock_name = "test",
             contrast = 1,
             type = "test",
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
  
  #  find current sb
  c_sb <- pull(tail(stock$sb, n = 1))[[1]]
  # find min sb
  min_sb <- min(stock$sb)
  # find max sb
  max_sb <- max(stock$sb)
  
  stock_ssb_contrast <- stock_ssb_contrast %>% 
    add_row(stock_name = x, contrast = (c_sb/max_sb), type = "current", driver = dome_stocks[row, "driver"][[1]]) %>% 
    add_row(stock_name = x, contrast = (min_sb/max_sb), type = "historical", driver = dome_stocks[row, "driver"][[1]])
  
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
  
  #  find current sb
  c_sb <- pull(tail(stock$sb, n = 1))[[1]]
  # find min sb
  min_sb <- min(stock$sb)
  # find max sb
  max_sb <- max(stock$sb)
  
  stock_ssb_contrast <- stock_ssb_contrast %>% 
    add_row(stock_name = x, contrast = (c_sb/max_sb), type = "current", driver = monotonic_stocks[row, "driver"][[1]]) %>% 
    add_row(stock_name = x, contrast = (min_sb/max_sb), type = "historical", driver  = monotonic_stocks[row, "driver"][[1]])
  
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
  geom_boxplot(aes(x = driver, y = contrast, fill = type)) +
  labs(title = "Comparision of current and historical spawning biomass contrast",
       subtitle = "stocks divided by primary influence on recruitment",
       x = "primary influence on recruitment", y = "spawning biomass contrast") +
  scale_fill_discrete(name = "contrast type")
print(a)
dev.off()
