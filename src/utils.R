library(quantmod)
library(xts)
library(MASS)
library(MTS)
library(egcm)
library(TTR)
library(KFAS)
library(PerformanceAnalytics)
egcm.set.default.pvalue(0.05)

generate_signal <- function(Z_score, threshold_long, threshold_short) {
  signal <- Z_score
  colnames(signal) <- "signal"
  signal[] <- NA
  
  #initial position
  signal[1] <- 0
  if (Z_score[1] <= threshold_long[1]) {
    signal[1] <- 1
  } else if (Z_score[1] >= threshold_short[1])
    signal[1] <- -1
  
  # loop
  for (t in 2:nrow(Z_score)) {
    if (signal[t-1] == 0) {  #if we were in no position
      if (Z_score[t] <= threshold_long[t]) {
        signal[t] <- 1
      } else if(Z_score[t] >= threshold_short[t]) {
        signal[t] <- -1
      } else signal[t] <- 0
    } else if (signal[t-1] == 1) {  #if we were in a long position
      if (Z_score[t] >= 0) signal[t] <- 0
      else signal[t] <- signal[t-1]
    } else {  #if we were in a short position
      if (Z_score[t] <= 0) signal[t] <- 0
      else signal[t] <- signal[t-1]
    }
  }
  return(signal)
}

estimate_mu_gamma_LS <- function(Y, pct_training = 0.7) {
  T <- nrow(Y)
  T_trn <- round(pct_training*T) # set training period
  # LS regression
  ls_coeffs <- coef(lm(Y[1:T_trn, 1] ~ Y[1:T_trn, 2]))
  mu <- xts(rep(ls_coeffs[1], T), index(Y))
  colnames(mu) <- "mu-LS"
  gamma <- xts(rep(ls_coeffs[2], T), index(Y))
  colnames(gamma) <- "gamma-LS"
  return(list(mu = mu, gamma = gamma))
}

estimate_mu_gamma_rolling_LS <- function(Y, pct_training = 0.7) {
  T <- nrow(Y)
  T_start <- round(pct_training*T)
  T_lookback <- 300  # lookback window length
  T_shift <- 10  # how often is refreshed
  # init empty variables
  gamma_rolling_LS <- mu_rolling_LS <- xts(rep(NA, T), index(Y))
  colnames(mu_rolling_LS) <- "mu-rolling-LS"
  colnames(gamma_rolling_LS) <- "gamma-rolling-LS"
  # loop
  t0_update <- seq(from = min(T_start, T_lookback), to = T-T_shift, by = T_shift)
  for (t0 in t0_update) {
    T_lookback_ <- ifelse(t0-T_lookback+1 >= 1, T_lookback, T_start)
    ls_coeffs <- coef(lm(Y[(t0-T_lookback_+1):t0, 1] ~ Y[(t0-T_lookback_+1):t0, 2],
                         weights = last(1:T_lookback, T_lookback_)))
    mu_rolling_LS[t0+1] <- ls_coeffs[1]
    gamma_rolling_LS[t0+1] <- ls_coeffs[2]
  }
  # fill na value
  mu_rolling_LS <- na.locf(mu_rolling_LS)  
  mu_rolling_LS <- na.locf(mu_rolling_LS, fromLast = TRUE)
  gamma_rolling_LS <- na.locf(gamma_rolling_LS)
  gamma_rolling_LS <- na.locf(gamma_rolling_LS, fromLast = TRUE)
  # smoothing
  L <- 15
  mu_rolling_LS[] <- filter(mu_rolling_LS, rep(1, L)/L, sides = 1)
  mu_rolling_LS <- na.locf(mu_rolling_LS, fromLast = TRUE)
  gamma_rolling_LS[] <- filter(gamma_rolling_LS, rep(1, L)/L, sides = 1)
  gamma_rolling_LS <- na.locf(gamma_rolling_LS, fromLast = TRUE)
  return(list(mu = mu_rolling_LS, gamma = gamma_rolling_LS))
}

estimate_mu_gamma_Kalman <- function(Y, pct_training=0.7) {
  T <- nrow(Y)
  # init empty variables
  gamma_Kalman_filtering <- mu_Kalman_filtering <- xts(rep(NA, T), index(Y))
  colnames(mu_Kalman_filtering) <- "mu-Kalman"
  colnames(gamma_Kalman_filtering) <- "gamma-Kalman"
  # Kalman parameters
  Tt <- diag(2)
  Rt <- diag(2)
  Qt <- 1e-5*diag(2)  # state transition variance very small
  Zt <- array(as.vector(t(cbind(1, as.matrix(Y[, 2])))), dim = c(1, 2, T))  # time-varying
  Ht <- matrix(1e-3)  # observation variance
  # the prior in the code: P1cov = kappa*P1Inf + P1, kappa = 1e7
  init <- estimate_mu_gamma_LS(Y, pct_training=pct_training)
  a1 <- matrix(c(init$mu[1], init$gamma[1]), 2, 1)
  P1 <- 1e-5*diag(2)  # variance of initial point
  P1inf <- 0*diag(2)
  # create Kalman model
  model <- SSModel(as.matrix(Y[, 1]) ~ 0 + SSMcustom(Z=Zt, T=Tt, R=Rt, Q=Qt, a1=a1, P1=P1, P1inf=P1inf), H=Ht)
  # run Kalman filtering
  out <- KFS(model)
  mu_Kalman_filtering[] <- out$a[-1, 1]  # a is Kalman filtering (alphahat is Kalman smoothing) (a(T+1)=alphahat(T))
  gamma_Kalman_filtering[] <- out$a[-1, 2]
  # smoothing
  L <- 30
  mu_Kalman_filtering[] <- filter(mu_Kalman_filtering, rep(1, L)/L, sides = 1)
  mu_Kalman_filtering <- na.locf(mu_Kalman_filtering, fromLast = TRUE)
  gamma_Kalman_filtering[] <- filter(gamma_Kalman_filtering, rep(1, L)/L, sides = 1)
  gamma_Kalman_filtering <- na.locf(gamma_Kalman_filtering, fromLast = TRUE)
  return(list(mu = mu_Kalman_filtering, gamma = gamma_Kalman_filtering))
}

compute_spread <- function(Y, gamma, mu, name = NULL) {
  w_spread <- cbind(1, -gamma)/cbind(1+gamma, 1+gamma) # normalize the weight
  spread <- rowSums(Y * w_spread) - mu/(1+gamma) # construct profolio
  colnames(spread) <- name
  return(spread)
}

generate_Z_score <- function(spread, n = 512, EMA_flag=TRUE) {
  ## traditional rolling windowed mean and variance
  # first, the mean
  if (EMA_flag){
    print("smoothing")
    spread.mean <- EMA(spread, n)
    spread.mean <- na.locf(spread.mean, fromLast = TRUE)
    spread.demeaned <- spread - spread.mean
    # second, the variance
    spread.var <- EMA(spread.demeaned^2, n)
    spread.var <- na.locf(spread.var, fromLast = TRUE)
    # finally compute Z-score
    Z.score <- spread.demeaned/sqrt(spread.var)
  }
  else{Z.score <- (spread-mean(spread["::2017"]))/sd(spread["::2017"])}

  return(Z.score)
}

pairs_trading <- function(Y, gamma, mu, name = NULL, threshold = 0.72, plot = FALSE) {
  # spread and spread portfolio
  w_spread <- cbind(1, -gamma)/cbind(1+gamma, 1+gamma)
  spread <- rowSums(Y * w_spread) - mu/(1+gamma)
  
  # Z-score
  Z_score <- generate_Z_score(spread)
  threshold_long <- threshold_short <- Z_score
  threshold_short[] <- threshold
  threshold_long[] <- -threshold
  
  # trading signal
  signal <- generate_signal(Z_score, threshold_long, threshold_short)
  
  # combine the ref portfolio with trading signal
  w_portf <- w_spread * lag(cbind(signal, signal))
  
  # now compute the PnL from the log-prices and the portfolio
  X <- diff(Y)  #compute log-returns from log-prices
  portf_return <- xts(rowSums(X * w_portf), index(X))
  portf_return[is.na(portf_return)] <- 0
  colnames(portf_return) <- name
  
  # plots
  if (plot) {
    tmp <- cbind(Z_score, signal)
    colnames(tmp) <- c("Z-score", "signal")
    par(mfrow = c(2, 1))
    { plot(tmp, legend.loc = "topleft",
           main = paste("Z-score and trading on spread based on", name))
      lines(threshold_short, lty = 2)
      print(lines(threshold_long, lty = 2)) }
    print(plot(1 + cumsum(portf_return), main = paste("Cum P&L for spread based on", name)))
  }
  
  return(list("return"=portf_return, "position"=signal))
}
