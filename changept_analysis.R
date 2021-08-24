
# need to save breakpoint information to a new dataframe for analysis later

# Breakpoint fit ---------------------------------------------

pdf("change_point_graphs.pdf") 
for(x in use_stocks$stock_name)
{
  recr<-scale(takers_rec[,x]) + 10 # this is dumb--scaled and added 10 because changepoint blows up with large numbers. fix later 
  rec_yr<-takers_rec[!is.na(recr),1]
  recr<-recr[!is.na(recr)]
  
  #==Fit regime model
  fitPelt	<-cpt.meanvar(recr,method="PELT",test.stat="Normal",penalty="AIC",minseglen=6,pen.value=0.05)
  
  #==look at the different functions in the changepoint library and compare the differences in the output
  #==when you use 'cpt.mean' instead of 'cpt.meanvar' or change minseglen from 6 to 10 (for example)--just play with the knobs!
  
  changes	<- fitPelt@cpts
  totMean	<-rep(0,length(recr))
  totSD	<-rep(0,length(recr))
  for(y in 1:length(changes))
  {
    if(y==1)
      ind1	<-1
    if(y>1)
      ind1	<-changes[y-1]+1
    ind2	<-changes[y]
    data	<-recr[ind1:ind2]
    
    regMean	<-mean(log(data))
    regSD	<-sd(log(data))
    period	<-seq(ind1,ind2)
    
    totMean[period]	<-regMean
    totSD[period]		<-regSD
  }
  par(mfrow=c(1,1),mar=c(4,4,1,1),oma=c(1,1,1,1))
  plot(recr~rec_yr,type='l',las=1,ylab='',xlab='Year')
  lines(exp(totMean)~rec_yr,lty=2,col=2)
  mtext(side=3, x)
}
dev.off()

