# Test 2 - Periodicity and zero inflation
.AnalyzeBias <- function(var, nam, var_latlong = 1, rounding_digits = 5, plot_bias = T, 
                         ratio_threshold_0 = 15, ratio_threshold_12 = 3.5) {
  x <- unlist(var[, var_latlong])
  x <- round(x, rounding_digits)
  mle <- .RunGreedyOptim(x)  # run greedy ML optimization (rates, time frames)
  res <- .PoiRateMLE(x, mle$max_par, F)  # get ML rates
  
  rate12_ratio <- max(res$lambdas[2:3]) / min(res$lambdas[2:3])
  rate0_ratio <- res$lambdas[1] / max(unique(res$lambdas[2:3]))
  
  mlp <- log(mle$rate_changes[mle$lik_vec >= (mle$max_lik - 2)])
  
  # condition for periodic bias
  PB <- min(log(mle$rate_changes[mle$lik_vec >= (mle$max_lik - 2)])) > log(7) & rate12_ratio > 
    ratio_threshold_12
  
  # condition for 0-rounding bias
  PCP <- max(mle$s0_size[mle$lik_vec >= (mle$max_lik - 2)]) < 0.1 & rate0_ratio > ratio_threshold_0
  
  out <- data.frame(min(log(mle$rate_changes[mle$lik_vec >= (mle$max_lik - 2)])), rate12_ratio, 
                    !PB, max(mle$s0_size[mle$lik_vec >= (mle$max_lik - 2)]), rate0_ratio, !PCP)
  
  if (plot_bias) {
    par(mfrow = c(2, 2))
    hist(x, nclass = 50)
    .PeriodicityRoundingLik(mle$max_par, x, T)
    
    plot(mle$lik_vec ~ log(mle$rate_changes), col = rgb(0,0,0, 0.1), pch = 16, 
         xlab = "Number of presumable sampling sites (log)", ylab = "log likelihood", 
         main = "Periodicity in the data (if any)")
    points(mle$lik_vec[mle$lik_vec >= (mle$max_lik - 2)] ~ 
             log(mle$rate_changes[mle$lik_vec >=(mle$max_lik - 2)]), col = "red", pch = 19)
    # excluding the first bin, 5 = 4 repeats (a,b,a,b); 7 = 6 repeats (a,b,a,b,a,b);
    abline(v = log(c(5, 7, 11)))
    
    plot(mle$lik_vec ~ mle$s0_size, col = rgb(0,0,0, 0.02), pch = 16, xlab = "Size of s0", 
         ylab = "log likelihood", main = "Rounding bias (if any)")
    points(mle$lik_vec[mle$lik_vec >= (mle$max_lik - 2)] ~ 
             mle$s0_size[mle$lik_vec >= (mle$max_lik - 2)], col = "red", pch = 19)
    # excluding the first bin, 5 = 4 repeats (a,b,a,b); 7 = 6 repeats (a,b,a,b,a,b);
    abline(v = c(0.015))

    if (!PCP & !PB) {
      title(gsub("input/", "", nam), outer = TRUE, line = -1)
    }
    if (PB & PCP) {
      title(paste("Period + Zero", gsub("input/", "", nam), sep = " - "), outer = TRUE, 
            line = -1)
    } else {
      if (PB) {
        title(paste("Period", gsub("input/", "", nam), sep = " - "), outer = TRUE, 
              line = -1)
      }
      if (PCP) {
        title(paste("Zero", gsub("input/", "", nam), sep = " - "), outer = TRUE, 
              line = -1)
      }
    }
    par(mfrow = c(1, 1))
  }
  return(out)
}

.PoissonProcessLik <- function(lambda = 1, x = data, S = 1) {
  # log PDF of Poisson distribution
  lambda <- lambda + 1e-10
  lik <- sum(x * log(lambda) - lambda * S)
  return(lik)
}

.PoiRateMLE <- function(xd, s, plt = F) {
  s0 <- s[1]
  s1 <- s[2]
  s2 <- s[3]
  S <- s1 + s2
  nS <- 1/S + 1
  bins <- cumsum(c(s0, rep(c(s1, s2), round(nS))))
  bins <- c(0, bins[bins < 1], 1)
  
  if (plt == T) {
    h <- hist(xd, breaks = bins, col = "gray")
  } else {
    h <- hist(xd, breaks = bins, plot = plt)
  }
  
  count <- h$counts
  
  l_indx <- rep(c(2, 3), nS)
  l_indx <- c(1, l_indx)
  l_indx <- l_indx[1:length(count)]
  
  dT <- diff(h$breaks) * 1000
  # rate MLE
  l0 <- mean(count[l_indx == 1] / dT[1])
  l1 <- mean(count[l_indx == 2] / dT[2])
  l2 <- mean(count[l_indx == 3] / dT[3])
  l_all <- c(l0, l1, l2)
  
  lambdas <- l_all[l_indx]
  mle <- NULL
  mle$lambdas <- lambdas
  mle$dT <- dT
  mle$count <- count
  return(mle)
}

.PeriodicityRoundingLik <- function(args, x, plt = F) {
  s0 <- abs(args[1])
  s1 <- abs(args[2])
  s2 <- abs(args[3])
  
  if ((s0 + s1 + s2) > 1) {
    return(-1e+05)
  }
  
  mle <- .PoiRateMLE(x, c(s0, s1, s2), plt)
  lambdas <- mle$lambdas
  count <- mle$count
  dT <- mle$dT
  lik <- .PoissonProcessLik(lambdas, count, dT)
  # print(c(l_all, -lik))
  return(lik)
}

.RunGreedyOptim <- function(xd) {
  # Greedy optimization possible_set_0 = (1:9)*10^(-2)
  possible_set <- (1:33) * 10^(-2)
  max_lik <- -1e+05
  max_par <- NULL
  lik_vec <- c()
  rate_changes <- c()
  s0_size <- c()
  for (s0 in possible_set) {
    print(c(s0, max_lik))
    for (s1 in possible_set) {
      for (s2 in possible_set) {
        args <- c(s0, s1, s2)
        lik <- .PeriodicityRoundingLik(args, x = xd)
        lik_vec <- c(lik_vec, lik)
        # rate_changes is the number of bins that are being used
        rate_changes <- c(rate_changes, (1 + (1 - args[1])/(args[2] + args[3]) * 2))
        s0_size <- c(s0_size, s0)
        if (lik > max_lik) {
          max_lik <- lik
          max_par <- args
        }
      }
    }
  }
  res <- NULL
  res$max_lik <- max_lik
  res$max_par <- max_par
  res$lik_vec <- lik_vec
  res$rate_changes <- rate_changes
  print(res$max_lik)
  print(res$max_par)
  res$s0_size <- s0_size
  return(res)
}