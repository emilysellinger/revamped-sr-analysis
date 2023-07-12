# salmon sensitivity to primary influence on recruitment

salmon_stocks <- all_drivers %>% 
  filter(str_detect(common_name, "salmon"))
salmon_stocks$salmon <- rep('salmon', nrow(salmon_stocks))

non_salmon_stocks <- all_drivers %>% 
  filter(!str_detect(common_name, "salmon"))
non_salmon_stocks$salmon <- rep('non_salmon', nrow(non_salmon_stocks))


salmon_n <- salmon_stocks %>% 
  group_by(driver) %>% 
  summarize(n = n()) %>% 
  mutate(freq = n/sum(n))

non_salmon_n <- non_salmon_stocks %>% 
  group_by(driver) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n))


totals <- tibble(type = c(rep("salmon", 3), rep("non_salmon", 3)),
                 driver = c(salmon_n$driver, non_salmon_n$driver),
                 n = c(salmon_n$n, non_salmon_n$n),
                 freq = c(salmon_n$freq, non_salmon_n$freq))

a <- totals %>% 
  ggplot(aes(x = driver, y = freq, fill = type)) + geom_col(position = "dodge") +
  scale_fill_manual(values = c("#00A1B7", "#898928"), labels = c('Non-salmon stocks', 'Salmon stocks')) + 
  labs(y = 'Frequency', x = 'Primary influence on recruitment', fill = element_blank()) +
  theme_minimal() + ylim(0, 1)

# salmon sensitivity to number of regime changes
salmon_stocks <- left_join(salmon_stocks, counts2)
non_salmon_stocks <- left_join(non_salmon_stocks, counts2)

all_drivers2 <- rbind(salmon_stocks, non_salmon_stocks)

b <- all_drivers2 %>% 
  ggplot() + geom_boxplot(aes(x = salmon, y = nshifts),  fill = "#00A1B7", alpha = 0.7) +
  scale_x_discrete(labels = c('Non-salmon stocks', 'Salmon stocks')) +
  labs(y = 'Number of regime shifts', x = 'Stock type') +
  theme_minimal()
  
