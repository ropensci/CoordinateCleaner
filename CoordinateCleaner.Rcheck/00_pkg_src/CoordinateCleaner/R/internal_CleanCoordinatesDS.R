# 1. The autocorrelation function returning a gamma vector, paramters = nbins and roudning, the latter = 0 for now

.CalcACT <- function(data, digit.round = 0, nc = 3000, graphs = T, graph.title = "Title", rarefy = F){
  
  if(rarefy){data <- unique(data)}
  
  data.units <- sort(abs(data))
  
  if (digit.round > 0){ # if set to 10 takes only units into account
    data_units = data_units - round(floor(data.units / digit.round) * digit.round)
  }
  
  if(graphs){
    h <- hist(data.units, nclass = nc, main = graph.title)
  }else{
    h <- hist(data.units, nclass = nc, plot = F)
  }
  
  f <- h$counts
  max.range <- round(length(f) * 0.9)
  gamma.0 <- stats::cov(f[1:max.range], f[1:max.range])
  
  gamma.vec <- c()
  
  for (k in 1:max.range){
    f.0 <- f[-(1:k)]
    f.k <- f[-((length(f) - k + 1):(length(f)))]
    gamma.vec <- c(gamma.vec, stats::cov(f.0, f.k) / gamma.0)
  }
  
  #add coordinates
  coords <- h$mids[1:max.range]
  
  #plot outlier bins vs coordinates
  if(graphs){plot(coords, gamma.vec)}
  
  #create output data.frame, with the gamma vector and the respective coordinates
  
  out <- data.frame(gamma = gamma.vec,
                    coords = coords)
  
  return(out)
}

# 2.function to run a sliding window over the gamma vector, identifying outliers, using the interquantile range. Two parameters: window size (fixed at 10 points for now) and the outlier threshold (T1, this is the most important paramter for the function). The function returns the number of non-consecutive 1 (= outlier found) 
.OutDetect <- function(x, T1 = 7, window.size = 10, 
                       detection.rounding = 2, detection.threshold = 6, 
                       graphs = T){
  max.range <- nrow(x) - window.size #The maximum range end for the sliding window
  
  out <- matrix(ncol = 2)
  
  for (k in 1:max.range){
    #sliding window
    sub <- x[k:(k + window.size),] #sliding window
    
    #interquantile range outlier detection
    quo <- stats::quantile(sub$gamma, c(0.25, 0.75), na.rm = T)
    outl <- matrix(nrow = nrow(sub), ncol = 2)
    outl[, 1] <- sub$gamma > (quo[2] + stats::IQR(sub$gamma) * T1) #what if more than one outlier are found?
    outl[, 2] <- sub$coord
    out <- rbind(out,outl)
  }
  
  out <- stats::aggregate(out[,1] ~ out[,2] , FUN = "sum")
  names(out) <- c("V1", "V2")
  out <- out[,c(2,1)]
  out[,1] <- as.numeric(out[,1] >= detection.threshold) # only "outliers" that have at least been found by at least 6 sliding windows
  
  #A distance matrix between the outliers
  #we use euclidean space, because 1) we are interest  in regularity, not so much the absolute distance, 2) grids might often be in  lat/lon and 3) most grids wil only spann small spatial extent
  outl <- out[out[,1] == 1,]
  
  if(nrow(outl) == 0){
    out <- data.frame(n.outliers = 0,
                      n.regular.outliers = 0,
                      regular.distance = NA)
  }else if(nrow(outl) == 1){
    if(graphs){abline(v = outl[,2], col = "green")}
    
    out <- data.frame(n.outliers = 1,
                      n.regular.outliers = 0,
                      regular.distance = NA)
  }else{
    #add the identified outliers to the plot
    if(graphs){abline(v = outl[,2], col = "green")}
    
    #calculate the distance between outliers
    dist.m <- round(stats::dist(round(outl[,2, drop = F], detection.rounding), diag = F), detection.rounding)
    dist.m[dist.m > 2] <- NA
    
    if(length(dist.m) == sum(is.na(dist.m))){
      out <- data.frame(n.outliers = nrow(outl),
                        n.regular.outliers = 0,
                        regular.distance = NA)
    }else{
      #process distance matrix
      dist.m[dist.m < 10^(-detection.rounding)] <- NA
      dists <- c(dist.m)
      dists <- sort(table(dists))
      dist.m <- as.matrix(dist.m)
      dist.m[row(dist.m) <= col(dist.m)] <- NA
      
      #select those with the most common distance
      com.dist <-  as.numeric(names(which(dists == max(dists)))) #find the most common distance between points
      
      sel <- which(dist.m %in% com.dist[1]) #if there is more than one probably no bias
      sel <- unique(arrayInd(sel, dim(dist.m))) #identify rows with at least one time the most common distance
      
      #sel <- unique(as.numeric(colnames(dist.m)[sel[,1]]))
      
      reg.outl <- cbind(outl[sel[,1], 2], outl[sel[,2], 2])
      
      if(graphs){
        # find the right y to plot the segments
        y0 <- max(x$gamma)
        
        if(y0 < 0.3 | y0 > 2){ 
          y1 <- max(x$gamma) - max(x$gamma)/nrow(reg.outl)
        }else{
          y1 <- max(x$gamma) - 0.1
        }
        ys <- seq(y0, y1, by = -((y0 - y1)/(nrow(reg.outl) - 1)))
        
        segments(x0 = reg.outl[,1], x1 = reg.outl[, 2], y0 = ys, y1 = ys, col = "red")
      }
      
      
      
      #output: number of outliers, number of outliers with the most common distance, the most common distance
      out <- data.frame(n.outliers = nrow(outl),
                        n.regular.outliers = nrow(reg.outl),
                        regular.distance = com.dist[1])
    }
  }
  return(out)
}






# # Test 2 - Periodicity and zero inflation
# .AnalyzeBias <- function(var, nam, var_latlong = 1, rounding_digits = 5, plot_bias = T, 
#                          ratio_threshold_0 = 15, ratio_threshold_12 = 3.5) {
#   x <- unlist(var[, var_latlong])
#   x <- round(x, rounding_digits)
#   mle <- .RunGreedyOptim(x)  # run greedy ML optimization (rates, time frames)
#   res <- .PoiRateMLE(x, mle$max_par, F)  # get ML rates
#   
#   rate12_ratio <- max(res$lambdas[2:3]) / min(res$lambdas[2:3])
#   rate0_ratio <- res$lambdas[1] / max(unique(res$lambdas[2:3]))
#   
#   mlp <- log(mle$rate_changes[mle$lik_vec >= (mle$max_lik - 2)])
#   
#   # condition for periodic bias
#   PB <- min(log(mle$rate_changes[mle$lik_vec >= (mle$max_lik - 2)])) > log(7) & rate12_ratio > 
#     ratio_threshold_12
#   
#   # condition for 0-rounding bias
#   PCP <- max(mle$s0_size[mle$lik_vec >= (mle$max_lik - 2)]) < 0.1 & rate0_ratio > ratio_threshold_0
#   
#   out <- data.frame(min(log(mle$rate_changes[mle$lik_vec >= (mle$max_lik - 2)])), rate12_ratio, 
#                     !PB, max(mle$s0_size[mle$lik_vec >= (mle$max_lik - 2)]), rate0_ratio, !PCP)
#   
#   if (plot_bias) {
#     par(mfrow = c(2, 2))
#     hist(x, nclass = 50)
#     .PeriodicityRoundingLik(mle$max_par, x, T)
#     
#     plot(mle$lik_vec ~ log(mle$rate_changes), col = rgb(0,0,0, 0.1), pch = 16, 
#          xlab = "Number of presumable sampling sites (log)", ylab = "log likelihood", 
#          main = "Periodicity in the data (if any)")
#     points(mle$lik_vec[mle$lik_vec >= (mle$max_lik - 2)] ~ 
#              log(mle$rate_changes[mle$lik_vec >=(mle$max_lik - 2)]), col = "red", pch = 19)
#     # excluding the first bin, 5 = 4 repeats (a,b,a,b); 7 = 6 repeats (a,b,a,b,a,b);
#     abline(v = log(c(5, 7, 11)))
#     
#     plot(mle$lik_vec ~ mle$s0_size, col = rgb(0,0,0, 0.02), pch = 16, xlab = "Size of s0", 
#          ylab = "log likelihood", main = "Rounding bias (if any)")
#     points(mle$lik_vec[mle$lik_vec >= (mle$max_lik - 2)] ~ 
#              mle$s0_size[mle$lik_vec >= (mle$max_lik - 2)], col = "red", pch = 19)
#     # excluding the first bin, 5 = 4 repeats (a,b,a,b); 7 = 6 repeats (a,b,a,b,a,b);
#     abline(v = c(0.015))
# 
#     if (!PCP & !PB) {
#       title(gsub("input/", "", nam), outer = TRUE, line = -1)
#     }
#     if (PB & PCP) {
#       title(paste("Period + Zero", gsub("input/", "", nam), sep = " - "), outer = TRUE, 
#             line = -1)
#     } else {
#       if (PB) {
#         title(paste("Period", gsub("input/", "", nam), sep = " - "), outer = TRUE, 
#               line = -1)
#       }
#       if (PCP) {
#         title(paste("Zero", gsub("input/", "", nam), sep = " - "), outer = TRUE, 
#               line = -1)
#       }
#     }
#     par(mfrow = c(1, 1))
#   }
#   return(out)
# }
# 
# .PoissonProcessLik <- function(lambda = 1, x = data, S = 1) {
#   # log PDF of Poisson distribution
#   lambda <- lambda + 1e-10
#   lik <- sum(x * log(lambda) - lambda * S)
#   return(lik)
# }
# 
# .PoiRateMLE <- function(xd, s, plt = F) {
#   s0 <- s[1]
#   s1 <- s[2]
#   s2 <- s[3]
#   S <- s1 + s2
#   nS <- 1/S + 1
#   bins <- cumsum(c(s0, rep(c(s1, s2), round(nS))))
#   bins <- c(0, bins[bins < 1], 1)
#   
#   if (plt == T) {
#     h <- hist(xd, breaks = bins, col = "gray")
#   } else {
#     h <- hist(xd, breaks = bins, plot = plt)
#   }
#   
#   count <- h$counts
#   
#   l_indx <- rep(c(2, 3), nS)
#   l_indx <- c(1, l_indx)
#   l_indx <- l_indx[1:length(count)]
#   
#   dT <- diff(h$breaks) * 1000
#   # rate MLE
#   l0 <- mean(count[l_indx == 1] / dT[1])
#   l1 <- mean(count[l_indx == 2] / dT[2])
#   l2 <- mean(count[l_indx == 3] / dT[3])
#   l_all <- c(l0, l1, l2)
#   
#   lambdas <- l_all[l_indx]
#   mle <- NULL
#   mle$lambdas <- lambdas
#   mle$dT <- dT
#   mle$count <- count
#   return(mle)
# }
# 
# .PeriodicityRoundingLik <- function(args, x, plt = F) {
#   s0 <- abs(args[1])
#   s1 <- abs(args[2])
#   s2 <- abs(args[3])
#   
#   if ((s0 + s1 + s2) > 1) {
#     return(-1e+05)
#   }
#   
#   mle <- .PoiRateMLE(x, c(s0, s1, s2), plt)
#   lambdas <- mle$lambdas
#   count <- mle$count
#   dT <- mle$dT
#   lik <- .PoissonProcessLik(lambdas, count, dT)
#   # print(c(l_all, -lik))
#   return(lik)
# }
# 
# .RunGreedyOptim <- function(xd) {
#   # Greedy optimization possible_set_0 = (1:9)*10^(-2)
#   possible_set <- (1:33) * 10^(-2)
#   max_lik <- -1e+05
#   max_par <- NULL
#   lik_vec <- c()
#   rate_changes <- c()
#   s0_size <- c()
#   for (s0 in possible_set) {
#     print(c(s0, max_lik))
#     for (s1 in possible_set) {
#       for (s2 in possible_set) {
#         args <- c(s0, s1, s2)
#         lik <- .PeriodicityRoundingLik(args, x = xd)
#         lik_vec <- c(lik_vec, lik)
#         # rate_changes is the number of bins that are being used
#         rate_changes <- c(rate_changes, (1 + (1 - args[1])/(args[2] + args[3]) * 2))
#         s0_size <- c(s0_size, s0)
#         if (lik > max_lik) {
#           max_lik <- lik
#           max_par <- args
#         }
#       }
#     }
#   }
#   res <- NULL
#   res$max_lik <- max_lik
#   res$max_par <- max_par
#   res$lik_vec <- lik_vec
#   res$rate_changes <- rate_changes
#   print(res$max_lik)
#   print(res$max_par)
#   res$s0_size <- s0_size
#   return(res)
# }