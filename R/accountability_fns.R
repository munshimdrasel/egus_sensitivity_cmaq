################################################################################################
#
#
#				Script to detrend met data (input is daily values e.g.from MetVeraging.R)
#				as in the Kuebler et al paper (2002).
#
#				Make sure to edit fd and Y
#				year1 = starting year
#				fd = first day of signal (e.g. January 1st is monday, enter "2")
#				met = TRUE/FALSE meteorological or not
#				log = TRUE/FALSE use the log of the signal or not
#				raw = TRUE/FALSE returns raw signal, minus leap days (and filling missing days)
#					-both fns return same value for this
#
#
#				Lucas Henneman
#				December 2012
#
#
#
#################################################################################################



detrending1 <- function(signal, year1, fd, log, met, raw = FALSE, fill = TRUE){

  library(kza)
  library(reshape2)
  library(zoo)
  library(e1071)


  C <- signal
  N = floor(length(C)/365) #number of years of data
  Y <- year1 #starting year
  fd <- fd ##first numbered day of first year (e.g. January 1st is monday, enter "2")

  days <- 1:365 #sequence of days
  firstweekdays <- fd:7
  weekdays <- append(firstweekdays,rep(1:7, times = (floor(length(C)/7))))

  yrs <- seq(from=1,to=N,by=1)
  years <- rep(yrs,each=365) #assign a year to each day

  #remove leap days
  for (n in 1:N)
  {
    Y1 = Y + n - 1
    D <- as.double(as.Date(as.yearmon(Y1)+1) - as.Date(as.yearmon(Y1)), units = "days")#number of days in the year
    if(D==366)
    {
      C <- C[-(365*(n-1)+31+29)]
      weekdays <- weekdays[-(365*(n-1)+31+29)]
    }
  }


  #replace NA's with predicted mean values (using avg of that day of year)
  if (fill == TRUE){
    Cfix <- C

    for (n in 0:(N-1))
    {
      for (i in 1:365)
      {
        C[(n*(365)+i)] <- ifelse(is.na(Cfix[(n*(365)+i)])==TRUE,mean(Cfix[(0:(N-1))*365+i],na.rm=TRUE),Cfix[(n*(365)+i)])
      }
    }
  }

  if (log == TRUE){C = log(C)} else {C = C}

  if (raw == TRUE){return(C)} ##raw gives back the log of the original time series, minus the leap days and with averaged days

  if (met == FALSE){
    Clt <- kz(C,365,k = 3)
    deltCn <- (C-Clt)/Clt

    deltCnmconc <- c() #mean year of species
    for (ts in 1:365)
    {
      sumconc = 0
      c = 0
      for (i in 1:N)
      {
        day <- ts + (52*7)*(i-1)#species conc depends on weeks
        sumconc = sumconc + ifelse(is.na(deltCn[day])==TRUE,0,deltCn[day])
        c = ifelse(is.na(deltCn[day])==F,c+1,c)
      }
      deltCnmconc[ts] = sumconc/c
    }

    day2mem <- c()

    for (t in 1:length(C))
    {
      day2mem[t] = t - (52*7)*(years[t]-1)
    }

    day2mem <- ifelse(day2mem >= 366, day2mem - 365, day2mem)

    Cbar = Clt*(1+deltCnmconc[day2mem])##This step is what lines up the day-of-year
    r <- C - Cbar
    detrend1 <- r
    delt1 <- exp(C)-exp(Cbar)

    out <- list(delt = r, MY = Cbar)

  } else {

    #calculate mean year=> sum, then divide by total years
    deltCnmmet <- c() #mean year of species

    meanyear <- c()

    ##Create mean year of met vars
    for (ts in 1:365)
    {
      summet = 0
      c = 0
      for (i in 1:N)
      {
        day <- ts + (52*7)*(i-1)#species conc depends on weeks
        day1 <- ts + (365)*(i-1)#met vars on years cycle
        summet = ifelse(is.na(C[day1])==T,summet,summet + C[day1])
        c = ifelse(is.na(C[day1])==F,c+1,c)
      }
      deltCnmmet[ts] = summet/c
    }

    ##Smooth met vars with loess splines, removes short-term stochastic component in mean year
    mets <- predict(loess(deltCnmmet~days,span=0.5,degree=2,family="gaussian"))

    #create time seires matching which mean year value
    metsE <- c()

    yrs <- seq(from=1,to=N,by=1)
    years <- rep(yrs,each=365) #assign a year to each day


    for (t in 1:length(C))
    {
      day3mem = t - 365*(years[t] - 1)
      metsE[t] <- C[t] - mets[day3mem]
    }

    out <- list(delt = metsE, MY = mets)
  } ##for non met

  return(out)

}##End here!

################################################################################################
#
#
#				Detrending3 using fft lowpass filter technique to remove longterm trends
#				Run MetAveraging.R first
#
#				Make sure to edit fd and Y
#				Year1 = starting year
#				fd = first day of signal (e.g. January 1st is monday, enter "2")
#				##met = TRUE/FALSE meteorological or not##unused
#				log = TRUE/FALSE use the log of the signal or not
#				Uf = upper frequency to create band-pass filter, e.g. use 1/30 for 30 day period
#				Lf = lower frequency of bandpass filter
#
#
#
#
#				Lucas Henneman
#				May 2013 (It's almost June though!)
#
#
#
#################################################################################################

detrending3 <- function(signal, year1, fd, log, Uf, Lf=1/(length(signal)/2), raw = FALSE){

  source("/Users/nhenneman/Dropbox/GTResearch/Detrending/fftfilt1.R") #contains filtering functions

  C <- leapyear(signal = signal, year1 = year1, log = log, fill = TRUE)

  if (raw == TRUE){return(C)} ##raw gives back the log of the original time series, minus the leap days and with averaged days

  #Use fft bandpass (lowpass) filter to remove frequencies below Uf days, default: Lf is Nyquest frequency
  Uf = Uf
  if (missing(Lf)){Cfft <- fftband(C,Uf = Uf)} else {Cfft <- fftband(C,Lf = Lf,Uf = Uf)}

  #get back the other portion of the signal
  if (missing(Lf)){Cffti <- fftband(C,Uf=Uf,reverse=TRUE)} else {Cffti <- fftband(C,Lf=Lf,Uf=Uf,reverse=TRUE)}

  Cdecomp <- list(Cfft = Cfft,Cffti = Cffti)

  return(Cdecomp)

}


################################################################################################
#
#
#				fn to calculate the variance at different periods using fft
#
#				Took the cheap way out and just hard coded in periods
#				Make sure 3 signals are the same length
#
#
#
#
#				Lucas Henneman
#				May 2013 (It's almost June though!)
#
#
#
#################################################################################################
comp.var <- function(raw,signal1,signal2){

  library(scales)

  dat <- raw  #make sure no leap days

  sf=1          # sampling frequency per day
  N=length(dat)  # length of the data record
  #Nend=15*60*sf  # Just use 15min of the data record for this example

  # 15 minute sample of dat
  N15=length(dat)      # Number of data points
  Nf=N15/2           # nyquist frequency

  # compute frequencies for spectra
  # frequency to plot spectral intensity (resolved only to nyquist frequency)
  f=(sf/N)*(1:(Nf-1))
  df=sf/N

  # Use Fast Fourier Transorm funciton in Matlab (fft) to calcuate the
  # Fourier transform (Fa) and then the variance
  # See handout, Stull 1988 CH 8, section 8.6, page 312
  Fan2r = (Re(fft(dat)/N15))^2+(Im(fft(dat)/N15))^2
  Fan2.1 = (Re(fft(signal1)/N15))^2+(Im(fft(signal1)/N15))^2
  Fan2.2 = (Re(fft(signal2)/N15))^2+(Im(fft(signal2)/N15))^2

  freq.var <- cbind(1/f[1:(length(f))],Fan2r[2:(length(f)+1)])
  freq.var.1 <- cbind(1/f[1:(length(f))],Fan2.1[2:(length(f)+1)])
  freq.var.2 <- cbind(1/f[1:(length(f))],Fan2.2[2:(length(f)+1)])

  tot.var <- sum(freq.var[,2])

  x1 <- sum(freq.var[(freq.var[,1] > 365),2])#/tot.var
  x2 <- sum(freq.var[(freq.var[,1] == 365),2])#/tot.var
  x3 <- sum(freq.var[(freq.var[,1] >= 90 & freq.var[,1] < 364),2])#/tot.var
  x4 <- sum(freq.var[(freq.var[,1] >= 30 & freq.var[,1] < 90),2])#/tot.var
  x5 <- sum(freq.var[(freq.var[,1] >= 8 & freq.var[,1] < 30),2])#/tot.var
  x6 <- sum(freq.var[(freq.var[,1] >= 6 & freq.var[,1] < 8),2])#/tot.var
  x7 <- sum(freq.var[(freq.var[,1] >= 3 & freq.var[,1] < 6),2])#/tot.var
  x8 <- sum(freq.var[(freq.var[,1] < 3),2])#/tot.var

  X <- c(x1,x2,x3,x4,x5,x6,x7,x8)#percent(c(x1,x2,x3,x4,x5,x6,x7,x8))

  ##Fraction of variance in raw signal in each section remaining after detrending -
  x1.1 <- sum(freq.var.1[(freq.var.1[,1] > 365),2])#/sum(freq.var[(freq.var[,1] > 365),2])
  x2.1 <- sum(freq.var.1[(freq.var.1[,1] == 365),2])#/sum(freq.var[(freq.var[,1] == 365),2])
  x3.1 <- sum(freq.var.1[(freq.var.1[,1] >= 90 & freq.var.1[,1] < 364),2])#/sum(freq.var[(freq.var[,1] >= 90 & freq.var[,1] < 364),2])
  x4.1 <- sum(freq.var.1[(freq.var.1[,1] >= 30 & freq.var.1[,1] < 90),2])#/sum(freq.var[(freq.var[,1] >= 30 & freq.var[,1] < 90),2])
  x5.1 <- sum(freq.var.1[(freq.var.1[,1] >= 8 & freq.var.1[,1] < 30),2])#/sum(freq.var[(freq.var[,1] >= 8 & freq.var[,1] < 30),2])
  x6.1 <- sum(freq.var.1[(freq.var.1[,1] >= 6 & freq.var.1[,1] < 8),2])#/sum(freq.var[(freq.var[,1] >= 6 & freq.var[,1] < 8),2])
  x7.1 <- sum(freq.var.1[(freq.var.1[,1] >= 3 & freq.var.1[,1] < 6),2])#/sum(freq.var[(freq.var[,1] >= 3 & freq.var[,1] < 6),2])
  x8.1 <- sum(freq.var.1[(freq.var.1[,1] < 3),2])#/sum(freq.var[(freq.var[,1] < 3),2])

  X.1 <- c(x1.1,x2.1,x3.1,x4.1,x5.1,x6.1,x7.1,x8.1)#percent(c(x1.1,x2.1,x3.1,x4.1,x5.1,x6.1,x7.1,x8.1))

  ##Fraction of variance in raw signal in each section remaining after detrending -
  x1.2 <- sum(freq.var.2[(freq.var.2[,1] > 365),2])#/sum(freq.var[(freq.var[,1] > 365),2])
  x2.2 <- sum(freq.var.2[(freq.var.2[,1] == 365),2])#/sum(freq.var[(freq.var[,1] == 365),2])
  x3.2 <- sum(freq.var.2[(freq.var.2[,1] >= 90 & freq.var.2[,1] < 364),2])#/sum(freq.var[(freq.var[,1] >= 90 & freq.var[,1] < 364),2])
  x4.2 <- sum(freq.var.2[(freq.var.2[,1] >= 30 & freq.var.2[,1] < 90),2])#/sum(freq.var[(freq.var[,1] >= 30 & freq.var[,1] < 90),2])
  x5.2 <- sum(freq.var.2[(freq.var.2[,1] >= 8 & freq.var.2[,1] < 30),2])#/sum(freq.var[(freq.var[,1] >= 8 & freq.var[,1] < 30),2])
  x6.2 <- sum(freq.var.2[(freq.var.2[,1] >= 6 & freq.var.2[,1] < 8),2])#/sum(freq.var[(freq.var[,1] >= 6 & freq.var[,1] < 8),2])
  x7.2 <- sum(freq.var.2[(freq.var.2[,1] >= 3 & freq.var.2[,1] < 6),2])#/sum(freq.var[(freq.var[,1] >= 3 & freq.var[,1] < 6),2])
  x8.2 <- sum(freq.var.2[(freq.var.2[,1] < 3),2])#/sum(freq.var[(freq.var[,1] < 3),2])

  X.2 <- c(x1.2,x2.2,x3.2,x4.2,x5.2,x6.2,x7.2,x8.2)#percent(c(x1.2,x2.2,x3.2,x4.2,x5.2,x6.2,x7.2,x8.2))


  tab1 <-c("Greater than 365","365","Between 90 and 365","Between 30 and 90","Between 8 and 30","Between 6 and 8","Between 3 and 6","Less than 3")
  tab <- cbind(tab1,X,X.1,X.2)
  colnames(tab) <- c("Period (Days)","Variance of Raw Signal","Detrend1","Detrend2")

  return(tab)


}

################################################################################################
#
#
#				fn to plot daily time series
#
#				Make sure to edit fd and Y
#				Year1 = starting year
#				fd = first day of signal (e.g. January 1st is monday, enter "2")
#				##met = TRUE/FALSE meteorological or not##unused
#				log = TRUE/FALSE use the log of the signal or not
#				Uf = upper frequency to create band-pass filter, e.g. use 1/30 for 30 day period
#				Lf = lower frequency of bandpass filter
#
#
#
#
#				Lucas Henneman
#				May 2013 (It's almost June though!)
#
#
#
#################################################################################################

tsplot <- function(s1,s2,sd,sm,sy,ed,em,ey,title='Time Series',ylab='value',n1='raw',n2='adjusted'){
  require(ggplot2)
  library(zoo)
  library(reshape2)
  library(scales)

  start = paste(sy,sm,sd,sep="/")#"2011/12/31"
  end = paste(ey,em,ed,sep="/")

  dates <- leapyear(seq.Date(as.Date(start),as.Date(end),by="1 day"),sy,log=FALSE,fill=FALSE)

  if(is.na(length(s2))==FALSE){
    df1 <- data.frame(	dates
                       ,s1 #raw
                       ,s2
    )

    dfm <- melt(df1,id.vars=c('dates'))

  } else{
    df1 <- data.frame(	dates
                       ,s1
    )
    dfm <- melt(df1,id.vars=c('dates'))
  }

  if(is.na(length(s2))==TRUE){
    ggplot(dfm,aes(dates,value,colour=variable)) + geom_line(size=.4) + scale_colour_manual(values=c("blue"),labels = c(n1))	+ ylab(ylab) + 	xlab("Date") + labs(title = title) + scale_x_date(labels = date_format("%b %Y")) + theme(legend.position=c(.9,.9),legend.title=element_blank())
  } else{
    ggplot(dfm,
           aes(x=dates,y=value,colour=variable)) +
      geom_line(size=.4) +
      scale_colour_manual(values=c("blue","forestgreen"),labels = c(n1,n2))	#+
    #		ylab(ylab) +
    #		xlab("Date") +
    #		labs(title = title) +
    #		scale_x_date(labels = date_format("%b %Y")) +
    #		theme(legend.position=c(.9,.9),legend.title=element_blank())
  }
}


#
# for visualizing the Fourier transform with  the zero-frequency component in the middle of the spectrum.
#
fftshift<-function(x)
{
  m=length(x);
  p = ceiling(m/2);
  new_index = c(((p+1):m),(1:p));
  old_index = c((1:p),(p+1):m);
  y=x[new_index];
}

fftband <- function(signal,Lf=1/(length(signal)/2),Uf,reverse = FALSE)
{
  h <- hanning.window(length(signal))
  signal=signal*h
  dt = 1 # sampling frequency per day
  Fs = 1/dt #samples per day
  N = length(signal) #number of data points
  Nf = N/2 # Nyquist frequency
  Lf = Lf
  dF = Fs/N
  #f = seq(from = -Fs/2,to = Fs/2 - dF, by = dF) ##from the website way
  f = (dt/N)*(-Nf:(Nf-1)) ##Heather's way
  lower_freq <- Lf
  upper_freq <- Uf
  # Band-Pass Filter:
  if (Lf == 1/(length(signal)/2)){
    BPF = ifelse((abs(f) <= upper_freq) ,0,1) # | is the OR operator

  } else{
    BPF = ifelse((abs(f) >= lower_freq) & (abs(f) <= upper_freq) ,0,1) # | is the OR operator
  }
  BPFi = ifelse(BPF==1,0,1)

  #	  signal=signal-mean(signal)

  spektrum = fftshift(fft(signal))/N
  spektrumfix = BPF*spektrum
  removedspektrum <- BPFi*spektrum

  if(reverse==FALSE){
    y=Re(fft(fftshift(spektrumfix),inverse=TRUE)) / h #inverse ifft
    y = -Re(fft(fftshift(spektrumfix),inverse=TRUE))
  }
  if(reverse==TRUE){
    y=Re(fft(fftshift(removedspektrum),inverse=TRUE)) / h
  }
  return(y)
}

##http://www.ncl.ucar.edu/Applications/filter.shtml
##http://www.johnny-lin.com/idl_code/lanczos_bandpass.pro
##https://mail.google.com/mail/u/0/?shva=1#inbox


fftsignal <- function(signal,varname) #varname is just the name for your signal, e.g."sulfate"
{
  dat <- signal  #make sure no leap days

  sf=1          # sampling frequency per day
  N=length(dat)  # length of the data record

  # 15 minute sample of dat
  N15=length(dat)      # Number of data points
  Nf=N15/2           # nyquist frequency

  # compute frequencies for spectra
  # frequency to plot spectral intensity (resolved only to nyquist frequency)
  #f=(sf/N)*(0:(Nf-2))
  f=(sf/N)*(1:(Nf-1)) ##This might be wrong??
  df=sf/N ##maybe should use Nf here?

  # Use Fast Fourier Transorm funciton in Matlab (fft) to calcuate the
  # Fourier transform (Fa) and then the discrete spectral intensity (Ea)
  # See handout, Stull 1988 CH 8, section 8.6, page 312
  Fan2=(Re(fft(dat)/N15))^2+(Im(fft(dat)/N15))^2
  Ea_n=2*Fan2

  # Approximate the Spectral Energy Density per unit frequency (eq 8.6.2)
  Sa_f=Ea_n/df

  # Plot Spectrum
  # This example is the premultiplied semi-log plot, 8.9d in Stull 1988
  Pdat = Sa_f[2:length(f)]*f[2:length(f)]  # premultiply spectra by freq

  #plot(x=f[2:length(f)],y=Pdat,log='x',xlab='f',ylab='f*S(f)',type='l')
  plot(x=1/f[2:length(f)],y=Pdat,log='xy',xlab='1/f',ylab='f*S(f)',type='l',main=paste("Spectral Intensity of ",varname,sep=""))
  P = 1/f[2:length(f)]
  y = cbind(P,Pdat)

}



################################################################################################
#
#
#				Script to detrend met data (input is daily values e.g.from MetVeraging.R)
#				as in the Rao et al. paper from 1997
#
#				Uses the KZ(365,3) and KZ(15,5) filters
#
#				Make sure to edit fd and Y
#				year1 = starting year
#				fd = first day of signal (e.g. January 1st is monday, enter "2")
#				met = TRUE/FALSE meteorological or not
#				log = TRUE/FALSE use the log of the signal or not
#				raw = TRUE/FALSE returns raw signal, minus leap days (and filling missing days)
#					-both fns return same value for this
#
#
#				Lucas Henneman
#				October 2013
#
#
#
#################################################################################################




detrending4 <- function(signal, year1, log, raw = FALSE){

  library(kza)
  library(reshape2)
  library(zoo)
  library(e1071)


  C <- leapyear(signal = signal, year1 = year1, log = log, fill = TRUE)

  if (raw == TRUE){return(C)} else { ##raw gives back the log of the original time series, minus the leap days and with averaged days

    Clt <- kz(C,365,k = 3)
    Cprime <- C - Clt
    CS <- kz(Cprime,15,k=5)

    Cdelt <- C - Clt - CS
    #Cdelt <- C - kz(C,15,k=5)

    out <- list(delt = Cdelt, S = CS)

    return(out)
  }
}

################################################################################################
#
#
#				Script to detrend met data (input is daily values e.g.from MetVeraging.R)
#				with KZ filter, then average output over 365 days
#
#				Uses the KZ(365,3) and KZ(15,5) filters
#
#				Make sure to edit fd and Y
#				year1 = starting year
#				fd = first day of signal (e.g. January 1st is monday, enter "2")
#				met = TRUE/FALSE meteorological or not
#				log = TRUE/FALSE use the log of the signal or not
#				raw = TRUE/FALSE returns raw signal, minus leap days (and filling missing days)
#					-both fns return same value for this
#				is.leap.year = use FALSE if signal contains leap days
#
#
#				Lucas Henneman
#				October 2013
#
#
#
#################################################################################################




detrending5 <- function(signal, year1, log, raw = FALSE, fill = TRUE, is.leap.year = TRUE, start.date = c()){

  library(kza)
  library(reshape2)
  library(zoo)
  library(e1071)
  library('timeDate')


  if (is.leap.year == TRUE){
    C <- leapyear(signal = signal, year1 = year1, log = log, fill = fill, start.date = start.date)
  } else {C <- signal}

  if (raw == TRUE){return(C)} else { ##raw gives back the log of the original time series, minus the leap days and with averaged days

    Clt <- kz(C,365,k = 3)
    Cprime <- C - Clt
    CS <- kz(Cprime,15,k=5)

    N = floor(length(C)/365) #number of years of data
    CSme = c()


    #create S (seasonal) trend using second KZ filter.
    for (d in 1:365){
      ref <- (0:(N-1))*365+d
      CSme[d] <- mean(CS[ref],na.rm=TRUE)
    }
    #account for signals that do not start on january 1st
    CSme.shift = c()
    if (length(start.date)==0) start.date = as.Date(USNewYearsDay(as.numeric(year1)))
    if (start.date != as.Date(USNewYearsDay(as.numeric(year1)))){
      shift = as.numeric(as.Date(paste(year1,'12-31',sep='-')) - start.date)
      CSme.shift[1:(365-shift)] = CSme[(shift+1):length(CSme)]
      CSme.shift[(365-shift+1):365] = CSme[1:shift]
      CSme.complete = append(CSme[1:(shift+1)],rep(CSme.shift,times=N))
    } else {CSme.shift = CSme; CSme.complete = rep(CSme.shift,times=N)}

    CSmen <- CSme.complete + Clt

    Cdelt <- C - CSmen
    #Cdelt <- C - kz(C,15,k=5)

    out <- list(delt = Cdelt, S = CSme.shift, SLT = CSmen, LT = Clt, Scomplete = CSme.complete)

    return(out)
  }
}

################################################################################################
#
#
#				Function to remove the leap days from a signal
#
#				year1 = starting year
#				fill = TRUE/FALSE to fill NA values with mean of that day of year
#				met = TRUE/FALSE meteorological or not
#				log = TRUE/FALSE use the log of the signal or not
#
#				Lucas Henneman
#				December 2012
#
#
#
#################################################################################################

leapyear <- function(signal, year1, log, fill = TRUE, start.date = c()){


  #library(reshape2)
  #library(zoo)
  #library(e1071)

  C <- signal
  N = floor(length(C)/365) #number of years of data
  Y <- year1 #starting year

  if (length(C) >= (365*N+ceiling(N/4))) {N=N+1}

  #if start.date exists, use it for the leapyear signal
  if (length(start.date) == 1){
    start.date.use = as.Date(start.date)
  } else if(length(start.date) == 0){
    start.date.use = as.Date(paste(year1,'01-01',sep='-'))
  } else stop("start.date is not appropriate format (YYYY-MM-DD)")

  date.vec <- seq.Date(start.date.use,by='1 day',length.out=length(signal))
  which.leapdays <- grep('-02-29',date.vec)

  C <- C[-which.leapdays]

  #remove leap days
  #for (n in 1:N){
  #	Y1 = Y + n - 1
  #	D <- as.double(as.Date(as.yearmon(Y1)+1) - as.Date(as.yearmon(Y1)), units = "days")#number of days in the year
  #	if(D==366){
  #		C <- C[-(365*(n-1)+31+29)]
  #	}
  #}#

  #replace NA's with predicted mean values (using avg of that day of year)
  if(fill == TRUE){
    Cfix <- C
    for (n in 0:(N-1)){
      for (i in 1:365){
        C[(n*(365)+i)] <- ifelse(is.na(Cfix[(n*(365)+i)])==TRUE,mean(Cfix[(0:(N-1))*365+i],na.rm=TRUE),Cfix[(n*(365)+i)])
      }
    }
  }

  if (log == TRUE){C = log(C)}

  C
}

#==================================================================================================
#
#					Functions to compute best fit of subset of a linear model
#
#					Order of covars should match coefs
#
#==================================================================================================

subfit <- function(coefs,covars,na=TRUE){

  if (length(covars) > 0){
    fit = matrix(data=NA,nrow = dim(covars)[1],ncol = dim(covars)[2])
    for (i in 1:length(coefs)){
      fit[,i] = coefs[i]*covars[,i]
    }
    fit = rowSums(fit,na.rm=na)
    fit
  } else {c(0)}
}

##more flexible version that matches coefs name to covars
subfit1 <- function(coeffs,covars,na=TRUE){

  if (is.null(dim(covars)) == T){
    fit.tot = coeffs * covars
  } else {
    coeffs[is.na(coeffs)==T] <- 0
    names.coefs <- names(coeffs) #[names(coeffs) %in% colnames(covars)]
    names.covars <- colnames(covars)

    if('(Intercept)' %in% names.coefs){
      covars.fit <- as.matrix(covars[,names.coefs[-which(names.coefs=='(Intercept)')]])
      fit.tot <- covars.fit %*% coeffs[-which(names.coefs=='(Intercept)')] + coeffs['(Intercept)']
    } else {
      covars.fit <- as.matrix(covars[,names.coefs])
      fit.tot <- covars.fit %*% coeffs
    }
  }


  fit.tot
}

#==================================================#
#	Observations - create sensitivities
#		sensitivities units: concentration
#		for log concentrations, fit model without variable, subtract
#			for the concentration sensitivity
#==================================================#

subfit.sens <- function(model,covars.in,source,log=F){
  coefs  <- coef(model)[grep(paste(source,collapse="|"),names(coef(model)))]
  covars <- as.data.frame(covars.in[,names(coefs)]); colnames(covars) = names(coefs)
  if (log == F){
    sens   <- subfit(coefs,covars)
    if(length(coefs) == 0) sens <- rep(NA,dim(covars)[1])
  } else {
    subfit.b = matrix(NA,nrow = dim(covars)[1],ncol=dim(covars)[2])
    for (b in 1:length(coefs)){
      ref.cov = grep(names(coefs)[b],colnames(covars.in))
      covars.zeroout <- covars.in;

      if (coefs[b]=='(Intercept)') covars.zeroout[,1:dim(covars)[2]] <- 0 else covars.zeroout[,ref.cov] <- 0

      subfit.b[,b] = ld(exp(predict(model)),fill.na=T) - exp(predict(model,newdata = covars.zeroout))
    }
    sens = rowSums(subfit.b)
  }

  sens
}



#==================================================================================================
#
#					Standard error of the mean
#
#==================================================================================================

ste <- function(x) sqrt(var(x,na.rm=T)/length(which(is.na(x)==F)))

#==================================================================================================
#
#					ggplot functions for faceting
#
#==================================================================================================

scientific_10 <- function(x) {
  library('scales')
  parse(text=gsub("e", " %*% 10^", scientific_format(digits=4)(x)))
}

facet_wrap_labeller <- function(gg.plot,labels=NULL) {
  #works with R 3.0.1 and ggplot2 0.9.3.1
  require(gridExtra)
  g <- ggplotGrob(gg.plot)
  gg <- g$grobs
  strips <- grep("strip_t", names(gg))

  for(ii in seq_along(labels))  {
    modgrob <- getGrob(gg[[strips[ii]]], "strip.text",
                       grep=TRUE, global=TRUE)
    gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
  }
  g$grobs <- gg
  class(g) = c("arrange", "ggplot",class(g))
  g
}

#==================================================================================================
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
#==================================================================================================

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    if(is.null(title) == F){
      layout <- matrix(seq(2, cols * ceiling(numPlots/cols) + 1),
                       ncol = cols, nrow = ceiling(numPlots/cols))
      layout <- rbind(c(1,1),(layout))
    } else {
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    if (is.null(title)) {
      heights = unit(rep_len(1, ceiling(numPlots/cols)), "null")
    } else {
      heights = unit(c(.5, rep_len(4, ceiling(numPlots/cols))), "null")
    }
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout),heights= heights)))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      if(is.null(title)){
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      } else {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i+1, arr.ind = TRUE))
        grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = NULL,height=unit(1,'cm')))
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
}

#==================================================================================================
#
#		Function to calculate MDA8h ozone for vector of hourly values
#
#			mean = TRUE will calculate 24-hr mean
#			mean = 'afternoon' will calculate afternoon mean (11a-7p)
#			define S vector to calculate averages for same hours as x (e.g. for sensntivities)
#			if df = F, return vector
#==================================================================================================

mda8 <- function(x,s,mean=FALSE,df=T){
  max.fun <- function(z){
    h.vec <- rep(NA,24)
    for ( h in 8:31 ){
      h.vec[h] <- mean(z[h:(h+7)])
    }
    h.max <- which.max(h.vec)
    if (length(h.max) == 0) h.max <- NA
    h.max
  }

  mean.which8h <- function(Z,Z.which){
    Z.8h <- rep(NA,length(Z.which))
    for (d in 1:length(Z.which)){
      if (is.na(Z.which[d]) == T) next
      h.which <- (Z.which[d]):(Z.which[d] + 7)
      Z.8h[d] <- mean(Z[h.which,d])
    }
    Z.8h
  }

  #Z defines which hour is associated with mda8h
  length.x = length(x)
  nday = length.x / 24
  try(if(nday < 3) stop("too few days"))

  ## Mean takes the 24-hr average of each day
  if (mean != TRUE) {## MDA8 of each day
    #z's much be from hour 17 the day before
    Z = matrix(NA,31,nday);
    if (!missing(s)){
      s <- as.matrix(s)
      sec = dim(s)[2]
      for (t in 1:sec){
        assign(paste('S',t,sep=''),matrix(NA,31,nday))
      }
    }
    for (d in 1:(nday-1)){
      h.mark = (d-1)*24
      Z[,d] = as.double(x[(h.mark+1):(h.mark+24+7)])

      if(!missing(s)) {
        S1[,d] = s[(h.mark+1):(h.mark+24+7),1]
        if (exists('S2') == T) S2[,d] = as.double(s[(h.mark+1):(h.mark+24+7),2])
        if (exists('S3') == T) S3[,d] = as.double(s[(h.mark+1):(h.mark+24+7),3])
        if (exists('S4') == T) S4[,d] = as.double(s[(h.mark+1):(h.mark+24+7),4])
        if (exists('S5') == T) S5[,d] = as.double(s[(h.mark+1):(h.mark+24+7),5])
        if (exists('S6') == T) S6[,d] = as.double(s[(h.mark+1):(h.mark+24+7),6])
        if (exists('S7') == T) S7[,d] = as.double(s[(h.mark+1):(h.mark+24+7),7])
        if (exists('S8') == T) S8[,d] = as.double(s[(h.mark+1):(h.mark+24+7),8])
      }
    }
    Z.which = unlist(apply(Z,2,max.fun))

    Z.8h <- mean.which8h(Z,Z.which)
    if (missing(s) == F ){

      S1.8h <- mean.which8h(S1,Z.which); out <- data.frame(Z.8h,S1.8h)
      if (exists('S2') == T) { S2.8h <- mean.which8h(S2,Z.which)
      out <- data.frame(Z.8h,S1.8h,S2.8h)}
      if (exists('S3') == T) { S3.8h <- mean.which8h(S3,Z.which)
      out <- data.frame(Z.8h,S1.8h,S2.8h,S3.8h)}
      if (exists('S4') == T) { S4.8h <- mean.which8h(S4,Z.which)
      out <- data.frame(Z.8h,S1.8h,S2.8h,S3.8h,S4.8h)}
      if (exists('S5') == T) { S5.8h <- mean.which8h(S5,Z.which)
      out <- data.frame(Z.8h,S1.8h,S2.8h,S3.8h,S4.8h,S5.8h)}
      if (exists('S6') == T) { S6.8h <- mean.which8h(S6,Z.which)
      out <- data.frame(Z.8h,S1.8h,S2.8h,S3.8h,S4.8h,S5.8h,S6.8h)}
      if (exists('S7') == T) { S7.8h <- mean.which8h(S7,Z.which)
      out <- data.frame(Z.8h,S1.8h,S2.8h,S3.8h,S4.8h,S5.8h,S6.8h,S7.8h)}
      if (exists('S8') == T) { S8.8h <- mean.which8h(S8,Z.which)
      out <- data.frame(Z.8h,S1.8h,S2.8h,S3.8h,S4.8h,S5.8h,S6.8h,S7.8h,SS.8h)}
    } else out = data.frame(Z.8h)

  } else {
    Z.mean <- rep(NA,nday);
    if (!missing(s)){
      s <- as.matrix(s)
      sec = dim(s)[2]
      S.mean <- matrix(NA,nday,sec)

      for (d in 1:nday){
        if (mean=='afternoon') {h = ((d - 1) * 24 + 12):(d * 24 - 4)
        } else h = ((d - 1) * 24 + 1):(d * 24)

        Z.mean[d] <- mean(x[h],na.rm=T)
        for (t in 1:sec){
          S.mean[d,t] <- mean(s[h,t],na.rm=T)
        }
      }
      out <- data.frame(Z.mean,S.mean)

    } else {
      for (d in 1:nday){
        if (mean=='afternoon') {h = ((d - 1) * 24 + 12):(d * 24 - 4)
        } else h = ((d - 1) * 24 + 1):(d * 24)

        Z.mean[d] <- mean(x[h],na.rm=T)
      }
      out <- data.frame(Z.mean)
    }
  }
  if (df == F) out = unlist(out)
  out
}

#==================================================================================================
#
#		Function to calculate daily metrics from hourly inputs
#
#		start.hour defines which hour data starts on, default is zero
#			values denote how many hours before midnight data starts
#
#			metrics:
#				mda8 - mean daily 8 hr average
#				afternoon - afternoon average ()
#				mean
#				sum
#				midday
#				morning
#				max
#				2p
#==================================================================================================


#define function with inputs of hourly measurements and metric of interest
#output is a daily vector
metrics.worker <- function(x,metric,start.hour = 0){
  try(if(metric %ni% c('mda8','afternoon','mean','sum','midday','morning','max','2p'))
    stop('choose a real metric'))

  #trim data from hours before/after 0
  if (start.hour != 0){
    length.x = length(x)
    x <- x[-c(1:start.hour)]
    x[(length(x)+1):length.x] <- NA
  }

  #choose which maximum for md8a ozone
  max.fun <- function(z){
    h.vec <- rep(NA,24)
    for ( h in 1:24 ){
      h.vec[h] <- mean(z[h:(h+7)])
    }
    h.max <- which.max(h.vec)
    if (length(h.max) == 0) h.max <- NA
    h.max
  }

  #get the mda8
  mean.which8h <- function(Z,Z.which){
    Z.8h <- rep(NA,length(Z.which))
    for (d in 2:length(Z.which)){
      if (is.na(Z.which[d]) == T) next
      h.which <- (Z.which[d]):(Z.which[d] + 7)
      Z.8h[d] <- mean(Z[h.which,d])
    }
    Z.8h
  }

  #Z defines which hour is associated with mda8h
  length.x = length(x)
  nday = length.x / 24
  try(if(nday < 3) stop("too few days"))

  ## Mean takes the 24-hr average of each day
  if (metric == 'mda8') {## MDA8 of each day
    #z's much be from hour 17 the day before
    Z = matrix(NA,31,nday);
    for (d in 1:nday){
      h.mark = (d-1)*24
      Z[,d] = as.double(x[(h.mark+1):(h.mark+24+7)])
    }
    Z.which = unlist(apply(Z,2,max.fun))

    Z.8h <- mean.which8h(Z,Z.which)
    out = unlist(Z.8h)

  } else {
    Z.mean <- rep(NA,nday)
    for (d in 1:nday){
      if (metric =='afternoon')    {
        h = ((d - 1) * 24 + 12): (d * 24 - 4)
        na.crit = 5;
        if (length(which(is.na(x[h])==T)) >= na.crit) next
        Z.mean[d] <- mean(x[h],na.rm=T)
      } else if (metric =='mean')  {
        h = ((d - 1) * 24 + 1) : (d * 24)
        na.crit = 12
        if (length(which(is.na(x[h])==T)) >= na.crit) next
        Z.mean[d] <- mean(x[h],na.rm=T)
      } else if (metric =='sum')   {
        h = ((d - 1) * 24 + 1) : (d * 24)
        na.crit = 1
        if (length(which(is.na(x[h])==T)) >= na.crit) next
        Z.mean[d] <- sum(x[h],na.rm=T)
      } else if (metric =='midday'){
        h = ((d - 1) * 24 + 12): (d * 24 - 8)
        na.crit = 3
        if (length(which(is.na(x[h])==T)) >= na.crit) next
        Z.mean[d] <- mean(x[h],na.rm=T)
      } else if (metric =='morning'){
        h = ((d - 1) * 24 + 8) : (d * 24 - 13)
        na.crit = 3
        if (length(which(is.na(x[h])==T)) >= na.crit) next
        Z.mean[d] <- mean(x[h],na.rm=T)
      } else if (metric =='max'){
        h = ((d - 1) * 24 + 1) : (d * 24)
        na.crit = 24
        if (length(which(is.na(x[h])==T)) >= na.crit) next
        Z.mean[d] <- max(x[h],na.rm=T)
      } else if (metric =='2p'){
        h = ((d - 1) * 24 + 15) : (d * 24 - 8)
        na.crit = 1
        if (length(which(is.na(x[h])==T)) >= na.crit) next
        Z.mean[d] <- mean(x[h],na.rm=T)
      }
    }
    out <- unlist(Z.mean)
  }
  out
}



#==================================================================================================
#
#		operator that negates the '%in%' operator
#==================================================================================================

`%ni%` <- Negate(`%in%`)
mean.na <- function(x) mean(x,na.rm = T)



#==================================================================================================
#	Function to calculate annual averages or sums
#==================================================================================================

agg.an <- function(x,fn='mean',na.omit=T,trans='eval',days=c(),...){

  if (length(days) > 0){
    nyear = round(length(x)/365)
    ndays = length(days)
    x = x[days + rep(365*(0:(nyear-1)),each=ndays)]
  } else ndays = 365

  N = round(length(x)/ndays)
  x.mat = apply(matrix(x,ncol=N),2,trans)
  X = apply(x.mat,2,eval(fn),na.rm = na.omit,...)
  X
}

#==================================================================================================
#	Function to calculate monthly averages or sums
#==================================================================================================

agg.mo <- function(x,dates,metric='mean'){
  library('zoo')

  dates.mo <- as.yearmon(dates, "%Y-%m")
  df.agg <- data.frame(x = x)

  if (metric == 'mean'){
    monthly.means = c(by(df.agg, dates.mo,function(x) mean.na(unlist(x))))
    dates.out <- names(monthly.means)
    out <- data.frame(dates = dates.out, monthly.means = monthly.means)
  } else if (metric == 'count'){
    monthly.counts = c(by(df.agg, dates.mo,function(x) length(unlist(na.omit(x)))))
    dates.out <- names(monthly.counts)
    out <- data.frame(dates = dates.out, monthly.counts = monthly.counts)
  }

  out
}
#==================================================================================================
#	Function to calculate annual averages or sums
#==================================================================================================

agg.yr <- function(x,dates){
  library('zoo')

  dates.yr <- format(dates, "%Y")
  df.agg <- data.frame(x = x)

  annual.means = c(by(df.agg, dates.yr,function(x) mean.na(unlist(x))))

  dates.out <- unique(as.Date(paste(dates.yr,'01','01',sep='-')))

  out <- data.frame(dates = dates.out, annual.means = annual.means)
  out
}

#==================================================================================================
#	Functions to save file with today's date added onto the end
#		defaults to save without environemnts
#==================================================================================================

save.today <- function(file.name,env = F){
  today = format(Sys.Date(),'%Y%m%d')
  print(paste('Saving ',file.name,today,'.RData',sep=''))

  list.env = ls(envir = .GlobalEnv)
  filename.full = paste(file.name,today,'.RData',sep='')

  if (env == F){
    object.class = sapply(sapply(list.env,get),typeof)
    object.env = grep('environment',object.class,invert = T)
  } else object.env = 1:length(list.env)

  #	print(list.env[object.env])

  save(list = list.env[object.env], file = filename.full,envir=.GlobalEnv)

  #	save.image(paste(file.name,today,'.RData',sep=''))
}



#==================================================================================================
#	This functions searches for the most recent run file in each loc directory
#==================================================================================================

source.which.file <- function(loc,name,env=.GlobalEnv,old.date=F){

  if (old.date != F) today = as.Date(old.date)
  #today's date
  today <- Sys.Date()

  #list of files in the loc directory, and select ones with dates
  file.list <- grep(paste('^',name,sep=''),list.files(loc),value=T)
  files.date.iso <- substr(file.list,nchar(name)+1,nchar(name)+8)
  files.date <- c()

  for(l in length(files.date.iso)){
    if(is.numeric(type.convert(files.date.iso[l]))==T){
      files.date[l] <- as.Date(files.date.iso[l],format='%Y%m%d')
    }
  }

  #compare the date to today's date, and select most recent file
  which.file <- which.min(as.numeric(today) - files.date)

  file <- paste(loc,file.list[which.file],sep='/')
  print(paste('Sourcing ',file))

  if(length(grep('RData',file))==0) source(file) else load(file,env)
}



#==================================================#
#	leap day interpolation function			   #
#		fill.na will fill leap days with NA
#==================================================#

ld <- function(x,date.ly=date,fill.na=F){
  #create date vectors of which days are leap days (missing)
  from = date.ly[1]
  to = date.ly[length(date.ly)]
  date.ld <- seq.Date(from, to,'1 day')
  dates.not <- which(date.ld %ni% as.Date(date.ly))
  dates.yes <- which(date.ld %in% as.Date(date.ly))

  x.ly <- c()
  x.ly[dates.yes] <- x

  if (fill.na == T) {
    x.ly[dates.not] <- NA
  } else {
    z = 1
    for (a in dates.not){
      ##add ec stm, oc stm, no3, ...
      x.ly[a] <- ifelse(length(which(is.na(x.ly[a:(a+z-1)])==TRUE))==z,
                        x.ly[a-1]+(x.ly[a+z]-x.ly[a-1])*(1/(z+1)),x.ly[a])
    }
  }

  x.ly
}



#===============================================================#
# Counterfactuals function
#		log = T assumes model of form log(y) = ßx + e
#================================================================#
#predicts filled values for each species
counterfactuals <- function(species,model,covars.new.ld,
                            covars.fit.ld,coefs,raw,stm,date.emp,name,log = F){

  if (missing('name') == T) name = ''
  detectlim = min(na.omit(raw))

  #If missing coefficients, base them on the model
  if (length(coefs)==0) {
    coefs = model$coef
  } else coefs = coefs

  covars.fit = covars.fit.ld[,names(coefs[-1])]

  #develop counterfactuals using sampled parameters and counterfactual emissions
  if (log == F) modeled.cf <- subfit1(coefs,covars.new.ld) else modeled.cf <- exp(subfit1(coefs,covars.new.ld))
  if (log == F) modeled.ac <- subfit1(coefs, covars.fit) else modeled.ac <- exp(subfit1(coefs, covars.fit))

  residuals <- raw - modeled.ac

  all_nas <- modeled.cf + residuals
  na.which = which(is.na(all_nas)==T);

  #first fill option: use average difference between observed and counterfactual for
  #	ten days surrounding day of interest
  for (t in 1:length(na.which)){
    t1 = na.which[t]
    range.avg = (t1 - 15):(t1 + 15);
    range.avg = as.numeric(range.avg[which(range.avg>0 & range.avg <= length(residuals))])
    all_nas[t1] <- raw[t1] + mean.na(all_nas[range.avg] - raw[range.avg])
    all_nas[t1] <- ifelse(is.na(all_nas[t1])==T,NA,all_nas[t1])
  }

  #second fill option - use model prediction and stm
  all_nas[is.na(all_nas)] <- modeled.cf[is.na(all_nas)] + stm[is.na(all_nas)]

  #correct potential negative values
  all_nas[which(all_nas<detectlim)] <- detectlim

  all_flags <- c()
  all_flags[is.na(residuals)] <- 1; all_flags[is.na(raw)] <- 1
  all_flags[!is.na(residuals)] <- 0

  #create date vectors of which days are leap days (missing)
  from = date.emp[1]
  to = date.emp[length(date)]
  date.ly <- seq.Date(from, to,'1 day')
  dates.not <- which(date.ly %ni% as.Date(date.emp))
  all_flags[dates.not] <- 1

  out <- cbind(all_nas,all_flags)
  colnames(out) <- c(paste(species,'.',name,sep=''),paste(species,'.',name,'_flags',sep=''))

  out
}


#===============================================================#
# Return season for given dates
#================================================================#


getSeason <- function(DATES) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-20",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-22",  format = "%Y-%m-%d") # Fall Equinox

  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))

  ifelse (d >= WS | d < SE, "win",
          ifelse (d >= SE & d < SS, "spr",
                  ifelse (d >= SS & d < FE, "sum", "fal")))
}












