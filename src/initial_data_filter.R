# Inital Data filtering - want all stocks with at least 20 years of S-R data
library(tidyverse)
library(here)
library(changepoint)
library(bcp)
library(data.table)
library(nlmrt)
library(FSA)
library(gridExtra)

# Load data ------------------------------------------------------------------
rec_data <- read.csv(here("data", "rec_dat_correct.csv")) # recruitment data

ssb_data <- read.csv(here("data", "ssb_dat_correct.csv")) # spawning stock biomass data

# Find stocks w/ 20 years -----------------------------------------------------------------------------
takers_rec <- rec_data %>%
  keep(~sum(!is.na(.x)) > 20)

takers_ssb <- ssb_data %>%
  select(one_of(colnames(takers_rec)))

# going to remove the stocks with only recruitment data - don't know why this has increased

takers_rec <- takers_rec %>%
  select(one_of(colnames(takers_ssb)))

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

# have to decide if we want to toss those 4 stocks or alter the 0 value to 1, as it will affect the S-R fits

# Identify problematic recruitment time series ------------------------------------------------------------
# inital data exploration found some recruitment time series that were extrapolated every few years
# we want to remove those prior to S-R function fits (more filtering will be done following that)

pdf("results/initial_recruitment_time_series.pdf")
for(x in 2:ncol(takers_rec)){
  
  # plot recruitment time series
  rec_plot <- ggplot() +
    geom_line(data = takers_rec, aes(year, takers_rec[,x])) + ylab("recruitment")
      
  spbio_plot <- ggplot() + geom_line(data = takers_ssb, aes(year, takers_ssb[,x]), linetype = "dashed") +
      ylab("spawning biomass") + ggtitle(colnames(takers_ssb[x]))
  
  print(grid.arrange(spbio_plot, rec_plot, nrow = 2))
}
dev.off()

# upon visual inspection of recruitment time series, I've identified some recruitment time series that appear a bit suspect. 
# I'm going to look at each of the weird time series along with the ssb-recruitment curve


# After looking at these stock -recruitment curves, it looks like most of the weird time series are straight off the
# S-R curve
# the COD3M, PILCHTSST, SDOGBLKGSA29 stocks don't seem to be off the recruitment curve, will have to decide
# what to do with them


# Remove problematic stocks --------------------------------------------------------------------------------
takers_rec <- takers_rec %>%
  select(!c(AMPL5YZ, BGROCKPCOAST, BKCDLFENI, BLACKOREOPR, BLACKOREOWECR, BLACKROCKORECOAST, BNSNZ, BSQLOBSTERCSCH,
            CAPEIIa.V.XIV, CHROCKCPCOAST,CHROCKNPCOAST, CHROCKSPCOAST, CRLOBSTERSA12, CRLOBSTERSA34, CRLOBSTERSA56,
            CRLOBSTERSA7, CRLOBSTERSA8, CSALMGRAYH, GRSPROCKNCAL, GRSPROCKSCAL, LNOSESKAPCOAST, OROUGHYNZMEC, OROUGHYSE,
            PORSHARATL, PSALMAKPNWD, PSALMAMAKC, PSALMANADYRPW, PSALMBARABARA, PSALMBRUINR, PSALMCHIGNIKBD, 
            PSALMDOGFISH, PSALMESAKPH, PSALMGOLOVIN, PSALMOKPH, PSALMPCHATHAM, PSALMRESB, PSALMSBPCKD, 
            PSALMSITUKR, PSALMUNALAKLEETNS, PSALMWKAMPH, PSALMWKAMPW, REDDEEPI.II,
            SAABALONESA, SARDSAW, SDOGATLC, SMOOTHOREOBP, SMOOTHOREOEPR, SMOOTHOREOSLD,SMOOTHOREOWECR,
            SPSDOGPCOAST, SSALMADAMS, SSALMFENNELL, SSALMHORSEFLY, SSALMLSTUART, SSALMMITCHELL,
            SSALMPORTAGE, SSALMSCOTCH, SSALMSHUSWAP, SSTHORNHPCOAST, THRSHARNPAC, TURBLKGSA29,
            YNOSESKACSCH, YNOSESKASCH))

takers_ssb <- takers_ssb %>%
  select(one_of(colnames(takers_rec)))

# Remove extra years from spawning biomass data ---------------------------------------------------
takers_ssb <- takers_ssb %>%
  filter(year < 2021)

