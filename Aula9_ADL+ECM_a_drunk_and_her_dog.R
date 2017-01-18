
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
xt_cointegrated[1] <- ut[1]
yt_cointegrated[1] <- wt[1]

####
# Observe that the values of c ad d are too small...
####

c <- 0.004
d <- 0.006


for (i in 2:2000) {
  xt_cointegrated[i] <- xt_cointegrated[i-1] + ut[i] + c*(yt_cointegrated[i-1]-xt_cointegrated[i-1])
  yt_cointegrated[i] <- yt_cointegrated[i-1] + wt[i] + d*(xt_cointegrated[i-1]-yt_cointegrated[i-1])
}
  
xt_cointegrated <- ts(xt_cointegrated,start = c(1900,1),frequency = 12)
yt_cointegrated <- ts(yt_cointegrated,start = c(1900,1),frequency = 12)

ts.plot(yt_cointegrated, xt_cointegrated, col = c(1,2))


####
# but, If c and d are equals zero
####

c <- 0.00
d <- 0.00

for (i in 2:2000) {
  xt_cointegrated[i] <- xt_cointegrated[i-1] + ut[i] + c*(yt_cointegrated[i-1]-xt_cointegrated[i-1])
  yt_cointegrated[i] <- yt_cointegrated[i-1] + wt[i] + d*(xt_cointegrated[i-1]-yt_cointegrated[i-1])
}

ts.plot(yt_cointegrated, xt_cointegrated, col = c(1,2))

