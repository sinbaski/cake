rm(list=ls());
graphics.off();
library("TTR");
library("quantmod");
source("common/libxxie.r");

getSymbols("DAX");
n <- dim(DAX)[1];
data <- DAX[(n-260):n, ];
## plot(data[, "DAX.Close"]);



chartSeries(data,
            theme=chartTheme('white'),
            TA=c(addVo(),addBBands(),
                 addMACD(), addCCI(),
                 addRSI(), addOBV()
                 ))  #add volume and Bollinger Bands from TTR

X <- EMA(data[, "DAX.Volume"], n=12) - EMA(data[, "DAX.Volume"], n=26);
Y <- EMA(X, n=9);
## vo.macd <- xts(list(fast=X, slow=Y), order=time(X));

addTA(X, on=NA,
      legend="VoMACD",
      col="#0000FF", type="l");
addTA(Y, on=7,
      col="#000000", type="l");

## X11();
## chartSeries(data,
##             theme=chartTheme('white'),
##             TA=c(addBBands(), addDPO(), addOBV()))

## stol <- stoch(data[, c("DAX.High", "DAX.Low", "DAX.Close")]);

## X11();
## plot(stol[, "fastD"]);
## lines(stol[, "slowD"], col="#FF0000")

## I like the following
## Stochastic Oscillator
## Donchian channel

## Have looked at the following
## SMI stochastic momentum index
## CMO (up - down)/(up + down)
## RSI up/(up + down)

## SAR Parabolic SAR

