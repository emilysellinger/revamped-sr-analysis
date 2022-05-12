# Identify deterministic S-R relationships and remove those stocks from analysis

# Define S-R models -------------------------------------------------------------------------------------
bevholt <- logR~log((a*sb)/(1 + b*sb))

ricker <- logR~log(a*sb*exp(-b*sb))

# Create usuable stock lists ----------------------------------------------------------------------------
use_stocks <- tibble()
use_stocks <- use_stocks %>%
  add_column(stock_name = "test",
             min_year = 1,
             max_year = 1)

# Find usuable stocks --------------------------------------------------------------------------------
for(x in 2:ncol(takers_rec)){
  # combine stock data into one data frame
  stock <- data.frame(
    stock_name = colnames(takers_rec[x]),
    year = takers_rec[,1],
    recruits = takers_rec[,x],
    logR = log(takers_rec[,x]),
    sb = takers_ssb[,x])
  
  # remove NAs
  stock <- stock[complete.cases(stock),]
  
  #right now there are some stocks with 0 recruit estimates per year, that needs to be fixed in the initial filtering
  # but for the moment, I'm going to change the inf values that result from taking the log to 0s
  stock <- stock %>%
     mutate(logR = replace(logR, logR == -Inf, 1)) %>%
     mutate(recruits = replace(recruits, recruits == 0, 1))
  
  # make sure there are at least 20 years of data remaining, once removing NAs and model run in time
  if(count(stock) != 0 && count(stock) >= 20 ){
    
    # find starting values for nonlinear regression 
    ricker_starts <- srStarts(recruits~sb, data = stock, type = "Ricker", param = 1)
    
    if(ricker_starts[2] < 0){ricker_starts[2] <- 0.000001} # make sure ricker b param is positive
    
    # fit ricker model to stock
    ricker_fit <- nlxb(ricker, data = stock, start = ricker_starts)
    
    # calculate RMSE using ricker model (can change to an average of BH and Ricker if needed)
    RMSE <- sqrt(mean((ricker_fit$resid^2)))
    
    # If RMSE is less than 0.3, the S-R relationship is a deterministic estimate (RMSE cutoff can be adjusted)
    if(RMSE > 0.2){
      use_stocks <- use_stocks %>%
        add_row(stock_name = colnames(takers_rec[x]), min_year = min(stock$year), max_year = max(stock$year))
    }
  }
}

# Plot filtered recruit data -------------------------------------------------------------------
# Goal is to plot filtered recruit data to further eliminate model run in times
stock_names <- c()
for(i in 1:nrow(use_stocks)){
  stock_names <- c(stock_names, use_stocks[i,1])
}

pdf("results/filtered_recruitment_time_series.pdf")
for(x in stock_names){
  row <- which(use_stocks$stock_name == x)
  
  stock <- tibble(year = takers_rec[,1],
                  recruits = takers_rec[, x],
                  sb = takers_ssb[, x])
  
  print(ggplot(data = stock, aes(x = year, y = recruits)) +
          geom_line(size = 1) +
          xlim(as.numeric(use_stocks[row, "min_year"]), as.numeric(use_stocks[row, "max_year"])) +
          labs(title = x))
  
}
dev.off()

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

row <- which(use_stocks$stock_name == "CALSCORPSCAL")
use_stocks[row, "min_year"] <- 1965

row <- which(use_stocks$stock_name == "CAPENOR")
use_stocks[row, "min_year"] <- 1983

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
use_stocks[row, "max_year"] <- 1994

row <- which(use_stocks$stock_name == "CSALMWILLB")
use_stocks[row, "min_year"] <- 1970
use_stocks[row, "max_year"] <- 1990

row <- which(use_stocks$stock_name == "DEEPCHAKESA")
use_stocks[row, "min_year"] <- 1985

row <- which(use_stocks$stock_name == "DEEPFLATHEADSE")
use_stocks[row, "max_year"] <- 2012

row <- which(use_stocks$stock_name == "DKROCKPCOAST")
use_stocks[row, "min_year"] <- 1970
use_stocks[row, "max_year"] <- 2005

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
use_stocks[row, "max_year"] <- 2005

row <- which(use_stocks$stock_name == "NZLINGLIN3.4")
use_stocks[row, "min_year"] <- 1973

row <- which(use_stocks$stock_name == "NZLINGLIN5.6")
use_stocks[row, "min_year"] <- 1973
use_stocks[row, "max_year"] <- 1997

row <- which(use_stocks$stock_name == "NZLINGLIN6b")
use_stocks[row, "max_year"] <- 1998

row <- which(use_stocks$stock_name == "NZLINGWSE")
use_stocks[row, "max_year"] <- 2005

row <- which(use_stocks$stock_name == "NZLINGLIN72")
use_stocks[row, "min_year"] <- 1983

row <- which(use_stocks$stock_name == "NZLINGLIN7WC")
use_stocks[row, "min_year"] <- 1975

row <- which(use_stocks$stock_name == "NZSNAPNZ1ENLD")
use_stocks[row, "min_year"] <- 1965
use_stocks[row, "max_year"] <- 2009

row <- which(use_stocks$stock_name == "NZSNAPNZ7")
use_stocks[row, "min_year"] <- 1965

row <- which(use_stocks$stock_name == "PANCHCSCH")
use_stocks[row, "max_year"] <- 2010

row <- which(use_stocks$stock_name == "PATGRENADIERSARG")
use_stocks[row, "min_year"] <- 1990

row <- which(use_stocks$stock_name == "PAUAPAU5D")
use_stocks[row, "min_year"] <- 1988

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

row <- which(use_stocks$stock_name == "SSALMNADINA")
use_stocks[row, "max_year"] <- 1995

row <- which(use_stocks$stock_name == "SSLOBSTERSASC")
use_stocks[row, "max_year"] <- 2006

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

row <- which(use_stocks$stock_name == "WMARLINATL")
use_stocks[row, "min_year"] <- 1976

row <- which(use_stocks$stock_name == "WROCKBCW")
use_stocks[row, "min_year"] <- 1960
use_stocks[row, "max_year"] <- 2005

row <- which(use_stocks$stock_name == "WROCKPCOAST")
use_stocks[row, "min_year"] <- 1969

row <- which(use_stocks$stock_name == "YEYEROCKPCOAST")
use_stocks[row, "min_year"] <- 1960

row <- which(use_stocks$stock_name == "YTROCKNPCOAST")
use_stocks[row, "min_year"] <- 1960


# these stocks were removed after they were off the stock-recruitment curve for the bev-holt model
# see code below
use_stocks <- use_stocks %>%
  filter(!(stock_name %in% c("CSALMJDFS", "PSALMCHINAP", "PSALMHUMPCY","PSALMKUSKOKWIM","PSALMMPNBNS", 
                             "SSALMGATES", "SSALMLLAKE", "TARAKNZ")))


# RMSE Values -------------------------------------------------------------
# I still have about 200 more stocks than the original analysis, which could be due to the updates to RAM
# in the years following Szuwalski et al 2015, but I just want to make sure that without the model run in times,
# the stocks I currently have are not off of the S-R curve

RMSE_test <- tibble(stock_name = "",
                    rmse = numeric())

for(x in use_stocks$stock_name){
  row <- which(use_stocks$stock_name == x)
  
  # create a tibble for each stock's biomass and recruit data
  stock <- tibble(
    year = takers_rec[,1],
    recruits = takers_rec[,x],
    sb = takers_ssb[,x],
    logR = log(takers_rec[,x]))
  
  # remove model run in time
  min_year <- pull(use_stocks[row, "min_year"])
  max_year <- pull(use_stocks[row, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  # Change any 0 recruit values to 1
  stock <- stock %>%
    mutate(logR = replace(logR, logR == -Inf, 1)) %>%
    mutate(recruits = replace(recruits, recruits == 0, 1))
  
  # double check to make sure total number of years is at least 20
  if(count(stock) != 0 && count(stock) >= 20 ){
    
    # find starting values for nonlinear regression 
    ricker_starts <- srStarts(recruits~sb, data = stock, type = "Ricker", param = 1)
    
    if(ricker_starts[2] < 0){ricker_starts[2] <- 0.000001} # make sure ricker b param is positive
    ricker_fit <- nlxb(ricker, data = stock, start = ricker_starts)
    
    RMSE <- sqrt(mean((ricker_fit$resid^2)))
    
    RMSE_test <- RMSE_test %>%
      add_row(stock_name = x, rmse = RMSE)
  }
}

# Compare Ricker and BevHolt fits --------------------------------------------------------------------------------
stock_model_fits <- tibble(stock_name = "",
                          ricker_a = numeric(),
                          ricker_b = numeric(),
                          ricker_AICc = numeric(),
                          bevholt_a = numeric(),
                          bevholt_b = numeric(),
                          bevholt_AICc = numeric())

for(x in use_stocks$stock_name){
  row <- which(use_stocks$stock_name == x)
  
  # create a tibble for each stock's biomass and recruit data
  stock <- tibble(
    year = takers_rec[,1],
    recruits = takers_rec[,x],
    sb = takers_ssb[,x],
    logR = log(takers_rec[,x]))
  
  # remove model run in time
  min_year <- pull(use_stocks[row, "min_year"])
  max_year <- pull(use_stocks[row, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  # Change any 0 recruit values to 1
  stock <- stock %>%
    mutate(logR = replace(logR, logR == -Inf, 1)) %>%
    mutate(recruits = replace(recruits, recruits == 0, 1))
  
  # double check to make sure total number of years is at least 20
  if(count(stock) != 0 && count(stock) >= 20 ){
    
    # find starting values for nonlinear regression 
    ricker_starts <- srStarts(recruits~sb, data = stock, type = "Ricker", param = 1)
    bevholt_starts <- srStarts(recruits~sb, data = stock, type = "BevertonHolt", param = 1)
    
    if(ricker_starts[2] < 0){ricker_starts[2] <- 0.000001} # make sure ricker b param is positive
    if(bevholt_starts[1] < 0){bevholt_starts[1] <- 0.000001} # make sure bevholt a param is positive
    if(bevholt_starts[2] < 0){bevholt_starts[2] <- 0.000001} # make sure bevholt b param is positive
    
    # fit ricker and beverton holt models to stock
    ricker_fit <- nlxb(ricker, data = stock, start = ricker_starts)
    bevholt_fit <- nlxb(bevholt, data = stock, start = bevholt_starts)
    
    # calculate AICc for both models
    params <- 3
    n <- nrow(stock)
    ricker_AICc <- n*log(mean(ricker_fit$resid^2)) + ((2*params*(params+1)) /(n - params - 1))
    bevholt_AICc <- n*log(mean(bevholt_fit$resid^2)) + ((2*params*(params+1)) /(n - params - 1))
    
    # add data to filterd stocks tibble
    stock_model_fits <- stock_model_fits %>%
      add_row(stock_name = colnames(takers_rec[x]), ricker_a = ricker_fit$coefficients[1], 
              ricker_b = ricker_fit$coefficients[2], ricker_AICc = ricker_AICc, 
              bevholt_a = bevholt_fit$coefficients[1], bevholt_b = bevholt_fit$coefficients[2],
              bevholt_AICc = bevholt_AICc)
  }
}

# Plot stock recruit fits -------------------------------------------------------------------------------------
pdf("results/stock_sr_fits.pdf")
for(x in stock_model_fits$stock_name){
  row <- which(stock_model_fits$stock_name == x)
  row2 <- which(use_stocks$stock_name == x)
  
  r_a <- as.numeric(stock_model_fits[row, "ricker_a"])
  r_b <- as.numeric(stock_model_fits[row, "ricker_b"])
  bh_a <- as.numeric(stock_model_fits[row, "bevholt_a"])
  bh_b <- as.numeric(stock_model_fits[row, "bevholt_b"])
  
  stock <- tibble(year = takers_rec[,1],
                  recruits = takers_rec[, x],
                  sb = takers_ssb[, x])
  # remove model run in time
  min_year <- pull(use_stocks[row2, "min_year"])
  max_year <- pull(use_stocks[row2, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  # simulate ricker and beverton holt data
  max_sb <- max(stock$sb, na.rm = TRUE)
  sb_values <- seq(1, max_sb, length.out = 200)
  rec_pred_r <- r_a*sb_values*exp(-r_b*sb_values)
  rec_pred_bh <- (bh_a*sb_values)/(1 + (bh_b*sb_values))
  
  # create tibble with preditions
  preds <- tibble(sb = sb_values,
                  r_preds = rec_pred_r,
                  bh_preds = rec_pred_bh)
  
  # recruitment time series plot
  rec_ts_plot <- ggplot(data = stock, aes(x = year, y = recruits)) +
    geom_line(size = 1) +
    labs(x = "Year", y = "Recruits", title = x)
  
  # stock recruitment plot
  sr_plot <- ggplot(data = stock, aes(x = sb, y = recruits)) +
    geom_point() + 
    geom_line(data = preds, aes(x = sb, y = r_preds), colour = "#E31A1C", size = 1) +
    geom_line(data = preds, aes(x = sb, y = bh_preds), colour = "#1F78B4", size = 1) +
    labs(x = "Spawning Biomass", y = "Recruits") 
  
  # plot both graphs
  print(grid.arrange(rec_ts_plot, sr_plot, nrow = 2))
}
dev.off()

# Split stocks ----------------------------------------------------------------------------------------
# In Szuwalski et al. 2015, the stocks suitable for analysis were split based on relative likelihood value then
# different procedures were then applied to each. The correlation analysis will be in a different script.

stock_model_fits <- stock_model_fits %>%
  add_column(min_model = "test") %>%
  add_column(rel_likelihood = 1.0)

# determine min AIC model and use it to calculate relative likelihood
for(x in 1:nrow(stock_model_fits)){
  if(stock_model_fits[x,"ricker_AICc"] < stock_model_fits[x, "bevholt_AICc"]){
    stock_model_fits[x, "min_model"] = "ricker"
    stock_model_fits[x,"rel_likelihood"] = 1/(1 + exp((pull(stock_model_fits[x,"ricker_AICc"]) - pull(stock_model_fits[x, "bevholt_AICc"]))/2))
  }else{
    stock_model_fits[x, "min_model"] = "bevholt"
    stock_model_fits[x,"rel_likelihood"] = 1/(1 + exp((pull(stock_model_fits[x,"bevholt_AICc"]) - pull(stock_model_fits[x, "ricker_AICc"]))/2))
  }
}

# dome-shaped stocks are those with a relative likelihood > 0.75
dome_stocks <- stock_model_fits %>%
  filter(min_model == "ricker") %>%
  filter(rel_likelihood > 0.75)

monotonic_stocks <- stock_model_fits %>%
  filter(min_model == "bevholt" | rel_likelihood < 0.75)



# Output to CSV  ----------------------------------------------------------
write.csv(dome_stocks, file = here("results", "dome_stocks.csv"))
write.csv(monotonic_stocks, file = here("results", "monotonic_stocks.csv"))

