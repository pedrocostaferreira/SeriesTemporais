
 #################################################################
 ######## A drunk and her dog ###################################
 #################################################################

## ----- Two Randon walk process ------
rm(list=ls())
set.seed(seed = 1)
ut <- rnorm(2000,mean = 0, sd = 1)
wt <- rnorm(2000,mean = 0, sd = 1)

xt<-NULL
yt <-NULL
xt[1] <- ut[1]
yt[1] <- wt[1]

for (i in 2:2000) {
  xt[i] <- xt[i-1] + ut[i]
  yt[i] <- yt[i-1] + wt[i]
}
xt <- ts(xt,start = c(1900,1),frequency = 12)
yt <- ts(yt,start = c(1900,1),frequency = 12)

ts.plot(xt,yt,type = "l",col = c(1,2))


## ---- Two cointegrated Randon walk process -----

xt_cointegrated <-NULL
yt_cointegrated <- NULL
xt_cointegrated[i] <- ut[1]
yt_cointegrated[i] <- wt[1]

c <- 0.4
d <- 0.4 

for (i in 2:2000) {
  xt_cointegrated[i] <- xt[i-1] + ut[i] + c*(yt[i-1]-xt[i-1])
  yt_cointegrated[i] <- yt[i-1] + wt[i] + d*(xt[i-1]-yt[i-1])
}
  
xt_cointegrated <- ts(xt_cointegrated,start = c(1900,1),frequency = 12)
yt_cointegrated <- ts(yt_cointegrated,start = c(1900,1),frequency = 12)

ts.plot(yt_cointegrated, xt_cointegrated, col = c(1,2))



