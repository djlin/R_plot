##
## Tachun Lin
## Last modified: Feb 1, 2016
## For my CS 330 class
## 
## Usage: 
## sin_freq(f), f=frequency
## sin_freq(f, A, phase), A=ampliture, phase=phase shift
## sin_comp(), to show the comparisons of sine waves with different A, f, phase
##
## fourier(freq, items), freq=frequency, items=number of terms used
## harmonic(items), items=number of terms used
##

sin_freq<-function(f) {
  x<- seq(0, 2, length=1000)
  y<- sin(2*pi*f*x)
  plot(x,y)  
}

sin_freq<-function(f, A=1, phase=0, ylabel="y") {
  x<- seq(0, 2, length=1000)
  y<- A * sin(2*pi*f*x + phase)
  plot(x,y, ylab=ylabel)  
}

# plot the comparison of sin waves
sin_comp<-function() {
  par(mar=c(4,4,1,1))
  par(mfrow = c(2,2))
  sin_freq(1, ylabel="y=sin(x)")
  sin_freq(2, ylabel="y=sin(2x)")
  sin_freq(1, 2, ylabel="y=2sin(x)")
  sin_freq(1, 1, 0.5*pi, ylabel="y=sin(x+pi/2)")
}

# plot the fourier series
fourier<-function(freq=1, items=1) {
  # set the margin (bottom, left, top, right) to remove
  # Error in plot.new() : figure margins too large
  par(mar=c(4,4,1,1))
  x<- seq(0, 4, length=1000)
  y <- 0
  idx<-seq(0,items, by=1)
  # fourier series
  #y<- 4/pi* cos(2*pi*f*x) - 4/(3*pi)* cos(3*2*pi*f*x) + 4/(5*pi)* cos(5*2*pi*f*x) - 4/(7*pi)* cos(7*2*pi*f*x)
  #+ 4/(9*pi)* cos(9*2*pi*f*x) - ......  
  for(i in idx) {
    y<- y - (-1)^i*4/((2*i+1)*pi)*cos((2*i+1)*pi*freq*x)
  }
  plot(x,y, ylim = c(2,-2))
}

harmonic <- function(items=1) {
  x <- seq(0,2,length=1000)
  y <- 0
  for (i in 1:items) {
    # for the sum of odd harmonics
    y <- y + sin(pi*(2*i -1)*x)
    # for the sum of all harmonics
    #y <- y + sin(pi*i*x)
  }
  plot(x, y)
}
