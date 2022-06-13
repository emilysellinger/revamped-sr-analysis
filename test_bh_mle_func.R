library(stats4)

row <- which(use_stocks$stock_name == "ACADREDGOMGB")

# create a tibble for each stock's biomass and recruit data
stock <- tibble(
  year = takers_rec[,1],
  recruits = takers_rec[,"ACADREDGOMGB"],
  sb = takers_ssb[,"ACADREDGOMGB"],
  logR = log(takers_rec[,"ACADREDGOMGB"]))

# remove model run in time
min_year <- pull(use_stocks[row, "min_year"])
max_year <- pull(use_stocks[row, "max_year"])

stock <- stock %>%
  filter(year >= min_year) %>%
  filter(year <= max_year)


dats <- pull(stock$sb)
datr <- pull(stock$recruits)

BHminusLL <- function(loga, logb, logsigmaR){
  # extract parameters
  a <- exp(loga); b <- exp(logb); sigmaR <- exp(logsigmaR)
  
  # make predictions
  pred <- log(a*dats/(1 + b*dats))
  
  #calculate negative log like
  NegLogL <- (-1)*sum(dnorm(log(datr), pred, sigmaR, log = TRUE))
  return(NegLogL)
}


starts <- list(loga = log(640), logb = log(5.0e-06), logsigmaR = 5)
mle_out <- mle(BHminusLL, start = starts)

# extract parameters
a <- exp(coef(mle_out)[1])
b <- exp(coef(mle_out)[2])
sigmaR <- exp(coef(mle_out)[3])
