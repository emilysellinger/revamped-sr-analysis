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

pdf("results/initial_recruitment_time_series.pdf")
for(x in 2:ncol(takers_rec)){
  
  # plot recruitment time series
  print(ggplot(data = takers_rec, aes(year, takers_rec[,x])) +
    geom_line() + ylab("recruitment") + ggtitle(colnames(takers_rec[x])))
  
}
dev.off()

# upon visual inspection of recruitment time series, I've identified some recruitment time series that appear a bit suspect. 
# I'm going to look at each of the weird time series along with the ssb-recruitment curve

sus_stocks <- c("AMPL5YZ", "BGROCKPCOAST", "BKCDLFENI", "BLACKOREOPR", "BLACKOREOWECR", "BLACKROCKORECOAST", "BNSNZ", "CHROCKCPCOAST",
                "CHROCKNPCOAST", "CHROCKSPCOAST", "COD3M", "CRLOBSTERSA12", "CRLOBSTERSA34", "CRLOBSTERSA56",
                "CRLOBSTERSA7", "CRLOBSTERSA8", "GRSPROCKNCAL", "GRSPROCKSCAL", "OROUGHYNZMEC", "OROUGHYSE",
                "PILCHTSST", "PORSHARATL", "SAABALONESA", "SDOGBLKGSA29", "SMOOTHOREOBP", "SMOOTHOREOEPR", "SMOOTHOREOSLD","SMOOTHOREOWECR",
                "SPSDOGPCOAST", "YNOSESKACSCH", "YNOSESKASCH")

pdf("results/suspect_stock_SR.pdf")
for(x in sus_stocks){
  # set margins
  par(mfrow=c(2,1),mar=c(.1,.1,.1,.1),oma=c(4,7,4,1))
  
  # plot stock-recruitment curve w/ observations
  plot(takers_rec[,x]~takers_ssb[,x],ylim=c(0,max(takers_rec[,x],na.rm=T)),xlim=c(0,max(takers_ssb[,x],na.rm=T)),
       las=1,xaxt='n')
  
  # axis argument
  axis(side=3)
  
  # plot recruitment time series
  plot(takers_rec[,x]~takers_rec[,1],type='l',
       ylim=c(0,max(takers_rec[,x],na.rm=T)),las=1)
  
  # axis labels
  mtext(outer=T,side=2,"Recruitment",line=4.2)
  mtext(outer = T, side = 3, x, line = 2.5)
  mtext(outer=T,side=3,"Spawning biomass",line=1.5)
  mtext(outer=T,side=1,"Year",line=2.5)
}
dev.off()

# After looking at these stock -recruitment curves, it looks like most of the weird time series are straight off the
# S-R curve
# the COD3M, PILCHTSST, SDOGBLKGSA29 stocks don't seem to be off the recruitment curve, will have to decide
# what to do with them


# Remove problematic stocks --------------------------------------------------------------------------------
takers_rec <- takers_rec %>%
  select(!c(AMPL3Ps, AMPL5YZ, BGROCKPCOAST, BKCDLFENI, BLACKOREOPR, BLACKOREOWECR, BLACKROCKORECOAST, BNSNZ, CHROCKCPCOAST,
            CHROCKNPCOAST, CHROCKSPCOAST, CRLOBSTERSA12, CRLOBSTERSA34, CRLOBSTERSA56,
            CRLOBSTERSA7, CRLOBSTERSA8, GRSPROCKNCAL, GRSPROCKSCAL, OROUGHYNZMEC, OROUGHYSE, PANDALI.II,
            PORSHARATL, SAABALONESA, SMOOTHOREOBP, SMOOTHOREOEPR, SMOOTHOREOSLD,SMOOTHOREOWECR,
            SPSDOGPCOAST, YNOSESKACSCH, YNOSESKASCH))

takers_ssb <- takers_ssb %>%
  select(one_of(colnames(takers_rec)))

# note: AMPL5YZ has a gap in recruitment data, AMPL3Ps and PANDALI.II both don't have spawning biomass data
