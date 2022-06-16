# Inital Data filtering - want all stocks with at least 20 years of S-R data


# Load data ------------------------------------------------------------------
rec_data <- read.csv(here("data/original_analysis", "rec_dat_correct.csv")) # recruitment data

ssb_data <- read.csv(here("data/original_analysis", "ssb_dat_correct.csv")) # spawning stock biomass data

# Find stocks w/ 20 years -----------------------------------------------------------------------------
takers_rec <- rec_data %>%
  keep(~sum(!is.na(.x)) > 20)

takers_ssb <- ssb_data %>%
  select(one_of(colnames(takers_rec)))

# going to remove the stocks with only recruitment data - don't know why this has increased

takers_rec <- takers_rec %>%
  select(one_of(colnames(takers_ssb)))
# Remove extra years from spawning biomass data ---------------------------------------------------
takers_ssb <- takers_ssb %>%
  filter(year < 2021)

# Identify problematic recruitment time series ------------------------------------------------------------
# inital data exploration found some recruitment time series that were extrapolated every few years
# we want to remove those prior to S-R function fits (more filtering will be done following that)

pdf("results/original_analysis/figures/initial_recruitment_time_series.pdf")
for(x in 2:ncol(takers_rec)){
  
  stock <- data.frame(year = takers_rec[,1],
                      spbio = takers_ssb[,x],
                      rec = takers_rec[,x])
  # plot recruitment time series
  rec_plot <- ggplot() +
    geom_line(data = stock, aes(year, rec)) + ylab("recruitment")
      
  spbio_plot <- ggplot() + 
    geom_line(data = stock, aes(year, spbio), linetype = "dashed") +
      ylab("spawning biomass") + ggtitle(colnames(takers_ssb[x]))
  
  spr_plot <- ggplot() + geom_point(data = stock, aes(x = spbio, y = rec)) + 
    labs(x = "spawning biomass", y = "recruitment")
  
  print(grid.arrange(spbio_plot, rec_plot, spr_plot, nrow = 3))
}
dev.off()


# Remove problematic stocks --------------------------------------------------------------------------------
takers_rec <- takers_rec %>%
  select(!c(AMPL5YZ, BGROCKPCOAST, BKCDLFENI, BLACKOREOPR, BLACKOREOWECR, BLACKROCKORECOAST, BNSNZ,
            CAPEIIa.V.XIV, CHROCKCPCOAST,CHROCKNPCOAST, CHROCKSPCOAST, CRLOBSTERSA12, CRLOBSTERSA34, CRLOBSTERSA56,
            CRLOBSTERSA7, CRLOBSTERSA8, CSALMGRAYH, GRSPROCKNCAL, GRSPROCKSCAL, LNOSESKAPCOAST, OROUGHYNZMEC, OROUGHYSE,
            PORSHARATL, PSALMAMAKC, PSALMBARABARA, PSALMCHIGNIKBD, PSALMDOGFISH, PSALMGOLOVIN, 
            PSALMUNALAKLEETNS, PSALMWKAMPW, SAABALONESA, SDOGATLC, SMOOTHOREOBP, SMOOTHOREOEPR, SMOOTHOREOSLD,SMOOTHOREOWECR,
            SPSDOGPCOAST, SSALMADAMS, SSALMHORSEFLY, SSALMLSTUART, SSALMMITCHELL,
            SSALMPORTAGE, SSALMSCOTCH, SSALMSHUSWAP, THRSHARNPAC, YNOSESKACSCH, YNOSESKASCH))

# stocks AMPL5YZ, CAPEIIa.V.XIV, CSALMGRAYH, PSALMAMAKC, PSALMBARABARA, PSALMCHIGNIKBD
# PSALMDOGFISH, PSALMGOLOVIN, PSALMUNALAKLEETNS, SDOGATLC, SSALMMITCHELL, SSALMSCOTCH
# do not have enough data upon visual inspection

# PSALMWKAMPW, SSALMADAMS, SSALMHORSEFLY, SSALMLSTUART, SSALMPORTAGE,
# SSALMSHUSWAP, THRSHARNPAC, seem suspicious - lots of zero recruitment

# all of the rest have recruitment derived from the S-R curve

takers_ssb <- takers_ssb %>%
  select(one_of(colnames(takers_rec)))

# Create usuable stock lists ----------------------------------------------------------------------------
use_stocks <- tibble(stock_name = colnames(takers_rec)[-1],
                     min_year = rep(NA, 1, 473),
                     max_year = rep(NA, 1, 473))

# Eliminate other stocks and find min/max years --------------------------------------------------------------------------------
for(x in use_stocks$stock_name){
  row <- which(use_stocks$stock_name == x)
  # combine stock data into one data frame
  stock <- data.frame(
    year = takers_rec[,1],
    recruits = takers_rec[,x],
    sb = takers_ssb[,x])
  
  # remove NAs
  stock <- stock[complete.cases(stock),]
  
  # make sure there are at least 20 years of data remaining, once removing NAs and model run in time
  if(count(stock) != 0 && count(stock) >= 20 ){
      use_stocks[row, 2] <- min(stock$year)
      use_stocks[row, 3] <- max(stock$year)
    }
}

use_stocks <- use_stocks[complete.cases(use_stocks),]


# Manually trim recruit ts ----------------------------------------------------------------------------
# After visual inspection, I've adjusted the beginning and end time of recruitment time series to 
# account for model burn in times and averages at the end of time series

row <- which(use_stocks$stock_name == "ACADREDGOMGB")
use_stocks[row, "min_year"] <- 1976

row <- which(use_stocks$stock_name == "ALSKABSAI")
use_stocks[row, "min_year"] <- 1980

row <- which(use_stocks$stock_name == "APOLLPJPN")
use_stocks[row, "min_year"] <- 2011

row <- which(use_stocks$stock_name == "ARFLOUNDBSAI")
use_stocks[row, "max_year"] <- 2016

row <- which(use_stocks$stock_name == "ARFLOUNDGA")
use_stocks[row, "min_year"] <- 1971

row <- which(use_stocks$stock_name == "ARFLOUNDPCOAST")
use_stocks[row, "min_year"] <- 1971

row <- which(use_stocks$stock_name == "ATKABSAI")
use_stocks[row, "min_year"] <- 1981

row <- which(use_stocks$stock_name == "ALSKABSAI")
use_stocks[row, "min_year"] <- 1982

row <- which(use_stocks$stock_name == "AUROCKPCOAST")
use_stocks[row, "min_year"] <- 1990

row <- which(use_stocks$stock_name == "AUSSALMONNZ")
use_stocks[row, "min_year"] <- 1985

row <- which(use_stocks$stock_name == "AWOLF5YZ")
use_stocks[row, "max_year"] <- 2011

row <- which(use_stocks$stock_name == "BGRDRNSWWA")
use_stocks[row, "min_year"] <- 1975

row <- which(use_stocks$stock_name == "BIGHTREDSE")
use_stocks[row, "max_year"] <- 2005


row <- which(use_stocks$stock_name == "BLACKROCKCAL")
use_stocks[row, "min_year"] <- 1971

row <- which(use_stocks$stock_name == "BLACKROCKWASH")
use_stocks[row, "min_year"] <- 1961

row <- which(use_stocks$stock_name == "BLSHARNPAC")
use_stocks[row, "min_year"] <- 1984

row <- which(use_stocks$stock_name == "BLUEROCKCAL")
use_stocks[row, "min_year"] <- 1960

row <- which(use_stocks$stock_name == "BOCACCSPCOAST")
use_stocks[row, "min_year"] <- 1961

row <- which(use_stocks$stock_name == "BSQLOBSTERNCH")
use_stocks[row, "min_year"] <- 1987


row <- which(use_stocks$stock_name == "BWHITNEA")
use_stocks[row, "min_year"] <- 1985

row <- which(use_stocks$stock_name == "CABEZNCAL")
use_stocks[row, "min_year"] <- 1970

row <- which(use_stocks$stock_name == "CABEZORECOAST")
use_stocks[row, "min_year"] <- 1980

row <- which(use_stocks$stock_name == "CABEZSCAL")
use_stocks[row, "max_year"] <- 2006
use_stocks[row, "min_year"] <- 1970

row <- which(use_stocks$stock_name == "CALSCORPSCAL")
use_stocks[row, "min_year"] <- 1965

row <- which(use_stocks$stock_name == "CAPENOR")
use_stocks[row, "min_year"] <- 1983

row <- which(use_stocks$stock_name == "CHAKESA")
use_stocks[row, "min_year"] <- 1980

row <- which(use_stocks$stock_name == "CHILISPCOAST")
use_stocks[row, "min_year"] <- 1965

row <- which(use_stocks$stock_name == "CMACKPJPN")
use_stocks[row, "max_year"] <- 2018


row <- which(use_stocks$stock_name == "COBGM")
use_stocks[row, "min_year"] <- 1980

row <- which(use_stocks$stock_name == "CODBA2224")
use_stocks[row, "min_year"] <- 1985

row <- which(use_stocks$stock_name == "CODNEAR")
use_stocks[row, "min_year"] <- 1951

row <- which(use_stocks$stock_name == "CROCKPCOAST")
use_stocks[row, "min_year"] <- 1960

row <- which(use_stocks$stock_name == "CSALMJDFS")
use_stocks[row, "min_year"] <- 1973
use_stocks[row, "max_year"] <- 1993

row <- which(use_stocks$stock_name == "CSALMWILLB")
use_stocks[row, "min_year"] <- 1970
use_stocks[row, "max_year"] <- 1990

row <- which(use_stocks$stock_name == "CTRACSA")
use_stocks[row, "min_year"] <- 1985

row <- which(use_stocks$stock_name == "DEEPCHAKESA")
use_stocks[row, "min_year"] <- 1985

row <- which(use_stocks$stock_name == "DEEPFLATHEADSE")
use_stocks[row, "max_year"] <- 2012

row <- which(use_stocks$stock_name == "DKROCKPCOAST")
use_stocks[row, "min_year"] <- 1970
use_stocks[row, "max_year"] <- 2005

row <- which(use_stocks$stock_name == "DOYSFS")
use_stocks[row, "min_year"] <- 1980

row <- which(use_stocks$stock_name == "DSOLEGA")
use_stocks[row, "min_year"] <- 1982
use_stocks[row, "max_year"] <- 2013

row <- which(use_stocks$stock_name == "DSOLEPCOAST")
use_stocks[row, "min_year"] <- 1973
use_stocks[row, "max_year"] <- 2010

row <- which(use_stocks$stock_name == "DUSROCKGA")
use_stocks[row, "max_year"] <- 2011

row <- which(use_stocks$stock_name == "EBASSVIIIab")
use_stocks[row, "min_year"] <- 1990
use_stocks[row, "max_year"] <- 2015

row <- which(use_stocks$stock_name == "EBASSIVbc.VII")
use_stocks[row, "max_year"] <- 2016

row <- which(use_stocks$stock_name == "ESOLEPCOAST")
use_stocks[row, "min_year"] <- 1940

row <- which(use_stocks$stock_name == "FLSOLEBSAI")
use_stocks[row, "min_year"] <- 1970
use_stocks[row, "max_year"] <- 2011

row <- which(use_stocks$stock_name == "FLSOLEGA")
use_stocks[row, "max_year"] <- 2012
use_stocks[row, "min_year"] <- 1978

row <- which(use_stocks$stock_name == "FMEG8c9a")
use_stocks[row, "max_year"] <- 2015

row <- which(use_stocks$stock_name == "GAGSATLC")
use_stocks[row, "min_year"] <- 1970

row <- which(use_stocks$stock_name == "GEMFISHNZ")
use_stocks[row, "min_year"] <- 1980
use_stocks[row, "max_year"] <- 2000

row <- which(use_stocks$stock_name == "GHALBSAI")
use_stocks[row, "min_year"] <- 1960

row <- which(use_stocks$stock_name == "GKCRABAIES")
use_stocks[row, "min_year"] <- 1995

row <- which(use_stocks$stock_name == "GOPHERSPCOAST")
use_stocks[row, "min_year"] <- 1977
use_stocks[row, "max_year"] <- 2001

row <- which(use_stocks$stock_name == "GRAMBERGM")
use_stocks[row, "min_year"] <- 1977

row <- which(use_stocks$stock_name == "GRAMBERSATLC")
use_stocks[row, "min_year"] <- 1977

row <- which(use_stocks$stock_name == "GRNSTROCKPCOAST")
use_stocks[row, "min_year"] <- 1970

row <- which(use_stocks$stock_name == "GRSNAPGM")
use_stocks[row, "min_year"] <- 1976

row <- which(use_stocks$stock_name == "GOLDREDV.VI.XII.XIV")
use_stocks[row, "max_year"] <- 2009

row <- which(use_stocks$stock_name == "GSTRGZRSTA7")
use_stocks[row, "min_year"] <- 1977
use_stocks[row, "max_year"] <- 2005

row <- which(use_stocks$stock_name == "GTRIGGM")
use_stocks[row, "min_year"] <- 1977

row <- which(use_stocks$stock_name == "HAD4X5Y")
use_stocks[row, "max_year"] <- 2013

row <- which(use_stocks$stock_name == "HADIS")
use_stocks[row, "max_year"] <- 2015

row <- which(use_stocks$stock_name == "HAKENRTN")
use_stocks[row, "max_year"] <- 2017

row <- which(use_stocks$stock_name == "HERRNWATLC")
use_stocks[row, "min_year"] <- 1970

row <- which(use_stocks$stock_name == "HMACKIIa.IVa.Vb.VIa.VII.VIII")
use_stocks[row, "min_year"] <- 1983

row <- which(use_stocks$stock_name == "HMACKIXa")
use_stocks[row, "max_year"] <- 2017

row <- which(use_stocks$stock_name == "HOKIENZ")
use_stocks[row, "min_year"] <- 1975

row <- which(use_stocks$stock_name == "HOKWIWNZ")
use_stocks[row, "min_year"] <- 1977

row <- which(use_stocks$stock_name == "KELPGREENLINGORECOAST")
use_stocks[row, "min_year"] <- 1980

row <- which(use_stocks$stock_name == "KMACKGM")
use_stocks[row, "min_year"] <- 1971

row <- which(use_stocks$stock_name == "KMACKSATLC")
use_stocks[row, "min_year"] <- 1980

row <- which(use_stocks$stock_name == "LINGCODNPCOAST")
use_stocks[row, "min_year"] <- 1962

row <- which(use_stocks$stock_name == "LINGCODSPCOAST")
use_stocks[row, "min_year"] <- 1957

row <- which(use_stocks$stock_name == "LSTHORNHPCOAST")
use_stocks[row, "min_year"] <- 1975


row <- which(use_stocks$stock_name == "MENATLAN")
use_stocks[row, "min_year"] <- 1960

row <- which(use_stocks$stock_name == "MORWONGESE")
use_stocks[row, "min_year"] <- 1948

row <- which(use_stocks$stock_name == "MORWONGWSE")
use_stocks[row, "min_year"] <- 1987
use_stocks[row, "max_year"] <- 2013

row <- which(use_stocks$stock_name == "NROCKGA")
use_stocks[row, "min_year"] <- 1965
use_stocks[row, "max_year"] <- 2010

row <- which(use_stocks$stock_name == "NROCKBSAI")
use_stocks[row, "max_year"] <- 2010

row <- which(use_stocks$stock_name == "NROCKGA")
use_stocks[row, "min_year"] <- 1967
use_stocks[row, "max_year"] <- 2010

row <- which(use_stocks$stock_name == "NSHRIMPCSCH")
use_stocks[row, "min_year"] <- 1960

row <- which(use_stocks$stock_name == "NZLINGESE")
use_stocks[row, "min_year"] <- 1983
use_stocks[row, "max_year"] <- 2005

row <- which(use_stocks$stock_name == "NZLINGLIN3.4")
use_stocks[row, "min_year"] <- 1973

row <- which(use_stocks$stock_name == "NZLINGLIN5.6")
use_stocks[row, "min_year"] <- 1973
use_stocks[row, "max_year"] <- 1997

row <- which(use_stocks$stock_name == "NZLINGLIN6b")
use_stocks[row, "max_year"] <- 1998

row <- which(use_stocks$stock_name == "NZLINGWSE")
use_stocks[row, "min_year"] <- 1983
use_stocks[row, "max_year"] <- 2005

row <- which(use_stocks$stock_name == "NZLINGLIN72")
use_stocks[row, "min_year"] <- 1983

row <- which(use_stocks$stock_name == "NZLINGLIN7WC")
use_stocks[row, "min_year"] <- 1975

row <- which(use_stocks$stock_name == "NZSNAPNZ1BOP.HAGU")
use_stocks[row, "min_year"] <- 1970

row <- which(use_stocks$stock_name == "NZSNAPNZ1ENLD")
use_stocks[row, "min_year"] <- 1965
use_stocks[row, "max_year"] <- 2009

row <- which(use_stocks$stock_name == "NZSNAPNZ7")
use_stocks[row, "min_year"] <- 1965

row <- which(use_stocks$stock_name == "PANCHCSCH")
use_stocks[row, "max_year"] <- 2010

row <- which(use_stocks$stock_name == "PATGRENADIERSARG")
use_stocks[row, "min_year"] <- 1990

row <- which(use_stocks$stock_name == "PAUAPAU5A")
use_stocks[row, "min_year"] <- 1986

row <- which(use_stocks$stock_name == "PAUAPAU5B")
use_stocks[row, "min_year"] <- 1980

row <- which(use_stocks$stock_name == "PAUAPAU5D")
use_stocks[row, "min_year"] <- 1988

row <- which(use_stocks$stock_name == "PAUAPAU7")
use_stocks[row, "min_year"] <- 1985

row <- which(use_stocks$stock_name == "PAUAsPAU5A")
use_stocks[row, "min_year"] <- 1987


row <- which(use_stocks$stock_name == "PCODBS")
use_stocks[row, "min_year"] <- 1977

row <- which(use_stocks$stock_name == "PCODGA")
use_stocks[row, "min_year"] <- 1978

row <- which(use_stocks$stock_name == "PERCHEBSAI")
use_stocks[row, "min_year"] <- 1965

row <- which(use_stocks$stock_name == "PERCHQCI")
use_stocks[row, "max_year"] <- 2004

row <- which(use_stocks$stock_name == "PERCHWCVANI")
use_stocks[row, "min_year"] <- 1977
use_stocks[row, "max_year"] <- 2005

row <- which(use_stocks$stock_name == "POLLFAPL")
use_stocks[row, "max_year"] <- 2002

row <- which(use_stocks$stock_name == "POPERCHGA")
use_stocks[row, "min_year"] <- 1970

row <- which(use_stocks$stock_name == "POPERCHPCOAST")
use_stocks[row, "min_year"] <- 1970

row <- which(use_stocks$stock_name == "PSALMAKPSWUD")
use_stocks[row, "min_year"] <- 1975

row <- which(use_stocks$stock_name == "PSALMATNARKO")
use_stocks[row, "min_year"] <- 1993

row <- which(use_stocks$stock_name == "PSALMCHINAP")
use_stocks[row, "min_year"] <- 1967

row <- which(use_stocks$stock_name == "PSALMSITUKR")
use_stocks[row, "min_year"] <- 1970

row <- which(use_stocks$stock_name == "PSOLEPCOAST")
use_stocks[row, "min_year"] <- 1960

row <- which(use_stocks$stock_name == "PTOOTHFISHMI")
use_stocks[row, "max_year"] <- 2002

row <- which(use_stocks$stock_name == "PTOOTHFISHPEI")
use_stocks[row, "min_year"] <- 1980
use_stocks[row, "max_year"] <- 2005

row <- which(use_stocks$stock_name == "REXSOLEGA")
use_stocks[row, "max_year"] <- 2011

row <- which(use_stocks$stock_name == "REYEROCKBSAI")
use_stocks[row, "min_year"] <- 1995

row <- which(use_stocks$stock_name == "REYEROCKGA")
use_stocks[row, "max_year"] <- 2002

row <- which(use_stocks$stock_name == "RSNAPGM")
use_stocks[row, "min_year"] <- 1970

row <- which(use_stocks$stock_name == "RSNAPSATLC")
use_stocks[row, "min_year"] <- 1987

row <- which(use_stocks$stock_name == "RSROCKBCWN")
use_stocks[row, "min_year"] <- 1980
use_stocks[row, "max_year"] <- 2005

row <- which(use_stocks$stock_name == "RSROCKBCWS")
use_stocks[row, "min_year"] <- 1962

row <- which(use_stocks$stock_name == "SABLEFPCOAST")
use_stocks[row, "min_year"] <- 1965

row <- which(use_stocks$stock_name == "SARDSAS")
use_stocks[row, "max_year"] <- 2015

row <- which(use_stocks$stock_name == "SBELLYROCKPCOAST")
use_stocks[row, "min_year"] <- 1975

row <- which(use_stocks$stock_name == "SBWHITACIR")
use_stocks[row, "min_year"] <- 1980

row <- which(use_stocks$stock_name == "SCMPBP")
use_stocks[row, "min_year"] <- 1987
use_stocks[row, "max_year"] <- 2008

row <- which(use_stocks$stock_name == "SFLOUNMATCL")
use_stocks[row, "min_year"] <- 1990

row <- which(use_stocks$stock_name == "SMOOTHOREOCR")
use_stocks[row, "max_year"] <- 2002

row <- which(use_stocks$stock_name == "SNAPSAUSSGSV")
use_stocks[row, "min_year"] <- 1987

row <- which(use_stocks$stock_name == "SNROCKPCOAST")
use_stocks[row, "min_year"] <- 1977

row <- which(use_stocks$stock_name == "SOUTHHAKECR")
use_stocks[row, "min_year"] <- 1985

row <- which(use_stocks$stock_name == "SOUTHHAKESA")
use_stocks[row, "min_year"] <- 1975
use_stocks[row, "max_year"] <- 1990

row <- which(use_stocks$stock_name == "SPANMACKGM")
use_stocks[row, "min_year"] <- 1985

row <- which(use_stocks$stock_name == "SPHAKECH")
use_stocks[row, "min_year"] <- 1972

row <- which(use_stocks$stock_name == "SPRATIIIa.IV")
use_stocks[row, "min_year"] <- 1982

row <- which(use_stocks$stock_name == "SSALMFENNELL")
use_stocks[row, "min_year"] <- 1965

row <- which(use_stocks$stock_name == "SSALMNADINA")
use_stocks[row, "max_year"] <- 1995

row <- which(use_stocks$stock_name == "SSLOBSTERSASC")
use_stocks[row, "max_year"] <- 2006

row <- which(use_stocks$stock_name == "SSTHORNHPCOAST")
use_stocks[row, "min_year"] <- 1975

row <- which(use_stocks$stock_name == "STFLOUNNPCOAST")
use_stocks[row, "min_year"] <- 1984
use_stocks[row, "max_year"] <- 2002

row <- which(use_stocks$stock_name == "STFLOUNSPCOAST")
use_stocks[row, "min_year"] <- 1978

row <- which(use_stocks$stock_name == "STMARLINNEPAC")
use_stocks[row, "max_year"] <- 2006

row <- which(use_stocks$stock_name == "SDOGATLC")
use_stocks[row, "max_year"] <- 2013

row <- which(use_stocks$stock_name == "STMARLINNEPAC")
use_stocks[row, "max_year"] <- 2007

row <- which(use_stocks$stock_name == "SWHITSE")
use_stocks[row, "min_year"] <- 1980

row <- which(use_stocks$stock_name == "TANNERCRABBSAI")
use_stocks[row, "min_year"] <- 1980

row <- which(use_stocks$stock_name == "TARAKNZ")
use_stocks[row, "min_year"] <- 1975
use_stocks[row, "max_year"] <- 2014

row <- which(use_stocks$stock_name == "THRSHARNPAC")
use_stocks[row, "min_year"] <- 1975

row <- which(use_stocks$stock_name == "TIGERFLATSE")
use_stocks[row, "min_year"] <- 1940

row <- which(use_stocks$stock_name == "TILESATLC")
use_stocks[row, "min_year"] <- 1990
use_stocks[row, "max_year"] <- 2007

row <- which(use_stocks$stock_name == "TREVALLYTRE7")
use_stocks[row, "min_year"] <- 1970
use_stocks[row, "max_year"] <- 2010

row <- which(use_stocks$stock_name == "WHITNS.VIId")
use_stocks[row, "min_year"] <- 1980


row <- which(use_stocks$stock_name == "VSNAPSATLC")
use_stocks[row, "min_year"] <- 1975

row <- which(use_stocks$stock_name == "VSNAPGM")
use_stocks[row, "min_year"] <- 1993

row <- which(use_stocks$stock_name == "WMARLINATL")
use_stocks[row, "min_year"] <- 1976

row <- which(use_stocks$stock_name == "WROCKBCW")
use_stocks[row, "min_year"] <- 1960
use_stocks[row, "max_year"] <- 2005

row <- which(use_stocks$stock_name == "WROCKPCOAST")
use_stocks[row, "min_year"] <- 1969

row <- which(use_stocks$stock_name == "YEGROUPGM")
use_stocks[row, "min_year"] <- 1975

row <- which(use_stocks$stock_name == "YEYEROCKPCOAST")
use_stocks[row, "max_year"] <- 2001

row <- which(use_stocks$stock_name == "YTROCKNPCOAST")
use_stocks[row, "min_year"] <- 1960

# Will plot this just to make sure that I've removed all of the run in times

pdf(here("results/original_analysis/figures", "filtered_recruitment_time_series.pdf"))
for(x in use_stocks$stock_name){
  
  # retrieve spawning biomass and recruitment data for each stock
  stock <- retrieve_sr_data(x)
  
  # graph recruitment time series
  rec_plot <- ggplot() + 
    geom_line(data = stock, aes(year, recruits)) +
    ylab("recruitment") + ggtitle(x)
  
  print(rec_plot)
  
}
dev.off()

# need to remove APOLLPJPN, PSALMAKPNWD, PSALMANDYRPW, PSALMATNARKO, PSALMBRUINR, PSALMKUSKOKWIM,
# PSALMMPNBNS, PSALMPCHATHAM, PSALMRESB, PSALMSBPCKD, PSALMWKAMPH, REDDEEPDP.1.2.V.X.II.XIV,
# REDDEEPI.II, SSALMLLAKE, WHITVIIbce.k
# these stocks either had missing data or lots of zero recruitment

use_stocks <- use_stocks %>% 
  filter(!stock_name %in% c("APOLLPJPN", "PSALMAKPNWD", "PSALMANDYRPW", "PSALMATNARKO", "PSALMBRUINR", 
                            "PSALMKUSKOKWIM", "PSALMMPNBNS", "PSALMPCHATHAM", "PSALMRESB", "PSALMSBPCKD",
                            "PSALMWKAMPH","REDDEEPDP.1.2.V.X.II.XIV", "REDDEEPI.II", "SSALMLLAKE", "WHITVIIbce.k"))
