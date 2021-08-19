# Inital Data filtering - want all stocks with at least 20 years of S-R data
library(tidyverse)
library(here)
library(changepoint)
library(bcp)
library(data.table)
# Load data ------------------------------------------------------------------
rec_data <- read.csv(here("data", "R.csv")) # recruitment data

ssb_data <- read.csv(here("data", "SSB.csv")) # spawning stock biomass data

# Find stocks w/ 20 years -----------------------------------------------------------------------------
takers_rec <- rec_data %>%
  keep(~sum(!is.na(.x)) > 20)

takers_ssb <- ssb_data %>%
  select(one_of(colnames(takers_rec)))

# Identify stocks with 0 recruitment -------------------------------------------------------------
# inital data exploration found some stocks have 0 recruits in a given year, want to find a list of those stocks

zero_r_stocks <- tibble()
zero_r_stocks <- zero_r_stocks %>%
  add_column(stock_name = "test")

for(x in 2:ncol(takers_rec)){
  if(0 %in% takers_rec[,x]){
     zero_r_stocks <- zero_r_stocks %>%
       add_row(stock_name = colnames(takers_rec[x]))
   }
}

# have to decide if we want to toss those 3 stocks or alter the 0 value to 1, as it will affect the S-R fits

# Identify problematic recruitment time series ------------------------------------------------------------
# inital data exploration found some recruitment time series that were extrapolated every few years
# we want to remove those prior to S-R function fits (more filtering will be done following that)

pdf("recruitment_time_series.pdf")
for(x in 2:ncol(takers_rec)){
  
  # plot recruitment time series
  print(ggplot(data = takers_rec, aes(year, takers_rec[,x])) +
    geom_line() + ylab("recruitment") + ggtitle(colnames(takers_rec[x])))
  
}
dev.off()

# upon visual inspection, I've identified some recruitment time series that appear a bit suspect. I will remove those from
# our main analysis for the time being. A list of the stocks I found strange are listed in a text file, I will also 
# include code to plot those specific recruitment time series, so they can be reviewed more easily later

takers_rec <- takers_rec %>%
  select(!c(BGROCKPCOAST, BKCDLFENI, BLACKOREOPR, BLACKOREOWECR, BLACKROCKORECOAST, BNSNZ, CHROCKCPCOAST,
            CHROCKNPCOAST, CHROCKSPCOAST, COD3M, CRLOBSTERSA12, CRLOBSTERSA34, CRLOBSTERSA56,
            CRLOBSTERSA7, CRLOBSTERSA8, GRSPROCKNCAL, GRSPROCKSCAL, OROUGHYNZMEC, OROUGHYSE, PANDALI.II,
            PILCHTSST, PORSHARATL, SAABALONESA, SDOGBLKGSA29, SMOOTHOREOBP, SMOOTHOREOEPR, SMOOTHOREOSLD,SMOOTHOREOWECR,
            SPSDOGPCOAST, YNOSESKACSCH, YNOSESKASCH))

takers_ssb <- takers_ssb %>%
  select(one_of(colnames(takers_rec)))
