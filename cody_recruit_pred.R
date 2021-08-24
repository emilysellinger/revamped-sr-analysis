#==predicting recruitment with time series methods
library(rEDM)
library(quantmod)
library(ggplot2)
library(reshape2)
library(forecast)
library(tseries)

# at some point it would make more sense to fit to the residuals to the fits of
# SR curves, but you get the general idea

pdf("ensemble_test_rec.pdf")
for(s in 2:ncol(use_rec_dat))
{
  recr<-scale(use_rec_dat[,s]) + 10 # this is dumb--scaled and added 10 because changepoint blows up with large numbers. fix later
  rec_yr<-use_rec_dat[!is.na(recr),1]
  recr<-recr[!is.na(recr)]
  auscafe = ts(recr, start = rec_yr[1], frequency = 1)
  
  end_train<-end(auscafe)
  end_train[1]<-end_train[1]-5 # adjust '5' (years) to change the length of test set
  train <- window(auscafe, end=end_train)
  h <- length(auscafe) - length(train)
  ETS <- forecast(ets(train), h=h)
  ARIMA <- forecast(auto.arima(train, lambda=0, biasadj=TRUE),
                    h=h)
  #STL <- stlf(train, lambda=0, h=h, biasadj=TRUE)
  NNAR <- forecast(nnetar(train), h=h)
  TBATS <- forecast(tbats(train, biasadj=TRUE), h=h)
  # Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
  #                   STL[["mean"]] + NNAR[["mean"]] + TBATS[["mean"]])/5
  
  Combination <- (ETS[["mean"]] + ARIMA[["mean"]] +
                    NNAR[["mean"]] + TBATS[["mean"]])/4
  
  p<-autoplot(auscafe) +
    autolayer(ETS, series="ETS", PI=FALSE) +
    # autolayer(STL, series="STL", PI=FALSE) +
    autolayer(ARIMA, series="ARIMA", PI=FALSE) +
    autolayer(NNAR, series="NNAR", PI=FALSE) +
    autolayer(TBATS, series="TBATS", PI=FALSE) +
    autolayer(Combination, series="Ensemble") +
    xlab("Year") + ylab("Scaled recruitment") +
    ggtitle(colnames(use_rec_dat)[s])
  
  print(p)
}
dev.off()