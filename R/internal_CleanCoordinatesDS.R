# 1. The autocorrelation function returning a gamma vector, paramters = nbins and roudning, the latter = 0 for now
.CalcACT <- function(data, digit.round = 0, nc = 3000, 
                     graphs = T, graph.title = "Title", rarefy = FALSE){
  
  if(rarefy){data <- unique(data)}
  
  data.units <- sort(abs(data))
  
  if (digit.round > 0){ # if set to 10 takes only units into account
    data_units <- data_units - round(floor(data.units / digit.round) * digit.round)
  }
  
  if(graphs){
    h <- hist(data.units, nclass = nc, main = graph.title)
  }else{
    h <- hist(data.units, nclass = nc, plot = FALSE)
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
                       graphs = TRUE){
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
    dist.m <- round(stats::dist(round(outl[,2, drop = FALSE], detection.rounding), diag = FALSE), detection.rounding)
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
