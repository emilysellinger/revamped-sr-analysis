# going to calculate contrast for Cody's stocks
library(devtools)
install_github("ropensci/ramlegacy")
library(ramlegacy)

takers_rec_stocks <- as.data.frame(colnames(takers_rec))

use_stocks <- read_csv(here("data", "use_stocks.csv"))
use_stocks <- use_stocks[,-1]

old_stocks <- read_csv(here("data", "cody_stocks.csv"))
old_stocks <- old_stocks %>% left_join(use_stocks)
