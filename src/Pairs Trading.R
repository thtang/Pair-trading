library(quantmod)
library(egcm)  #install.packages("egcm")
library(PerformanceAnalytics)
library(KFAS)
library(TTR)
source(file = "./GitHub/Pair-trading/src/utils.R")

AUD<-read.csv("C:/Users/thtang/Documents/GitHub/Pair-trading/data/AUD_mod.csv", header=T, sep=",")
EUR<-read.csv("C:/Users/thtang/Documents/GitHub/Pair-trading/data/EUR_mod.csv", header=T, sep=",")
GBP<-read.csv("C:/Users/thtang/Documents/GitHub/Pair-trading/data/GBP_mod.csv", header=T, sep=",")
NZD<-read.csv("C:/Users/thtang/Documents/GitHub/Pair-trading/data/NZD_mod.csv", header=T, sep=",")

AUD_xts <- xts(AUD$Price, order.by=as.Date(AUD$Date,"%Y/%m/%d"), )
NZD_xts <- xts(NZD$Price, order.by=as.Date(NZD$Date,"%Y/%m/%d"), )
AUD_NZD <- cbind(AUD_xts, NZD_xts)

AUD_NZD$NZD_xts[(is.na(AUD_NZD$NZD_xts))] <- mean(AUD_NZD$NZD_xts, na.rm = TRUE)
plot(AUD_NZD, legend.loc = "topleft", main = "ETF prices")

AUD_NZD_sub <- AUD_NZD["2009-01-01/"]

res <- egcm(AUD_NZD_sub$AUD_xts, AUD_NZD_sub$NZD_xts)
summary(res)
plot(res)

logprices <- (AUD_NZD)
plot(logprices, legend.loc = "topleft", main = "Stock log-prices")

# regression
T <- nrow(logprices)
T_trn <- round(0.7*T)  # define the training set
T_tst <- T - T_trn
y1 <- logprices[, 1]
y2 <- logprices[, 2]

# do LS regression
ls_coeffs <- coef(lm(y1[1:T_trn] ~ y2[1:T_trn]))
ls_coeffs
#> (Intercept) y2[1:T_trn] 
#>    2.435953    0.864618
mu <- ls_coeffs[1]
gamma <- ls_coeffs[2]

tmp <- cbind(y1, mu + gamma*y2)
colnames(tmp) <- c(colnames(y1), paste("mu + gamma x", colnames(y2)))
{ plot(tmp, legend.loc = "topleft", main = "Regression of y1 from y2")
  addEventLines(xts("training", index(y1[T_trn])), srt = 90, pos = 2, lwd = 2, col = "blue") }

spread <- y1 - gamma*y2
{ plot(spread, main = "Spread")
  addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }

coint_result <- egcm(y1, y2, p.value = 0.05)
print(coint_result)
plot(coint_result)


# normalized portfolio
w_ref <- c(1, -gamma)/(1+gamma)
sum(abs(w_ref))
#> [1] 1
w_spread <- matrix(w_ref, T, 2, byrow = TRUE)
w_spread <- xts(w_spread, index(y1))
colnames(w_spread) <- c("w1", "w2")

# resulting normalized spread
spread <- rowSums(cbind(y1, y2) * w_spread)
spread <- xts(spread, index(y1))
colnames(spread) <- "spread"
{ plot(spread, main = "Spread (from normalized portfolio)")
  addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }

spread_mean <- mean(spread[1:T_trn], na.rm = TRUE)
spread_var <- as.numeric(var(spread[1:T_trn], na.rm = TRUE))
Z_score <- (spread-spread_mean)/sqrt(spread_var)
colnames(Z_score) <- "Z-score"
threshold_long <- threshold_short <- Z_score
threshold_short[] <- .7
threshold_long[] <- -.7

{ plot(Z_score, main = "Z-score")
  lines(threshold_short, lty = 2)
  lines(threshold_long, lty = 2) 
  addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }



# now just invoke the function
signal <- generate_signal(Z_score, threshold_long, threshold_short)

{ plot(cbind(Z_score, signal), main = "Z-score and trading signal", legend.loc = "topleft")
  addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }

# let's compute the PnL directly from the signal and spread
spread_return <- diff(spread)
traded_return <- spread_return * lag(signal)   # NOTE THE LAG!!
traded_return[is.na(traded_return)] <- 0
colnames(traded_return) <- "traded spread"

{ plot(traded_return, main = "Return of traded spread")
  addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }

{ plot(1 + cumsum(traded_return), main = "Cum P&L of traded spread (no reinvestment)")
  addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }

{ plot(cumprod(1 + traded_return), main = "Cum P&L of traded spread (w/ reinvestment)")
  addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }

{ chart.Drawdown(traded_return, main = "Drawdown", lwd = 1)
  addEventLines(xts("", index(y1[T_trn])), lwd = 2, col = "blue") }







## Experiment

Y_ <- log(AUD_NZD)
LS <- estimate_mu_gamma_LS(Y_, pct_training = 0.8)
rolling_LS <- estimate_mu_gamma_rolling_LS(Y_, pct_training = 0.8)
Kalman <- estimate_mu_gamma_Kalman(Y_)

# plots
par(mfrow = c(2, 1))
{ plot(cbind(LS$mu, rolling_LS$mu, Kalman$mu), 
       legend.loc = "left", main = "Tracking of mu")
  addEventLines(xts("", index(Y_[round(0.3*nrow(Y_))])), lwd = 2, col = "blue") }
{ plot(cbind(LS$gamma, rolling_LS$gamma, Kalman$gamma), 
       legend.loc = "left", main = "Tracking of gamma")
  addEventLines(xts("", index(Y_[round(0.3*nrow(Y_))])), lwd = 2, col = "blue") }

spread_LS <- compute_spread(Y_, LS$gamma, LS$mu, "LS")

spread_rolling_LS <- compute_spread(Y_, rolling_LS$gamma, rolling_LS$mu, "rolling-LS")
spread_Kalman <- compute_spread(Y_, Kalman$gamma, Kalman$mu, "Kalman")

# plots
plot(cbind(spread_LS, spread_rolling_LS, spread_Kalman), legend.loc = "topleft", main = "Spreads")

return_LS <- pairs_trading(Y_, LS$gamma, LS$mu,  "LS", plot = TRUE)

return_rolling_LS <- pairs_trading(Y_, rolling_LS$gamma, rolling_LS$mu, "rolling-LS", plot = TRUE)

return_Kalman <- pairs_trading(Y_, Kalman$gamma, Kalman$mu, "Kalman", plot = TRUE)

# plot
plot(cumprod(1 + cbind(return_LS, return_rolling_LS, return_Kalman)), 
     main = "Cum P&L", legend.loc = "topleft")
