WritePyRate <- function(x, taxon = "accepted_name", min.age = "min_ma", max.age = "max_ma", 
                        status = NULL, trait = NULL, 
                        fname = NULL, path = getwd(), 
                        replicates = 1, cutoff = NULL, random = TRUE){
  
  if (is.null(fname)) 
    stop("'fname' missing with no default")
  
  if (is.null(status)){
    stop("'status' missing with no default.")
  }
  
  #adapt input data
  dat1 <- data.frame(species = x[, taxon], Status = status,
                     min_age = x[, min.age], max_age = x[, max.age])
  
  if(length(trait) > 0){
    dat1 <- data.frame(dat1, trait)
  }

  outfile <- paste(path, "/", fname, "_PyRate.py", sep="")
  
  dat1[,1] <- gsub("[[:blank:]]{1,}","_", dat1[,1])
  
  if (replicates > 1){
    random <- TRUE
  }
  
  if (any(is.na(dat1[,1:4]))){
    print(c(which(is.na(dat1[,1])), which(is.na(dat1[,2])),
            which(is.na(dat1[,3])), which(is.na(dat1[,4]))))
    stop("the input file contains missing data in species names, status or ages)\n")
  }
  
  if (!is.null(cutoff)){
    dat <- dat1[!(dat1[,4] - dat1[,3] >= cutoff),]
    cat("\n\nExcluded ", 100 - round(100*dim(dat)[1]/dim(dat1)[1]), "% occurrences")
    hist(dat1[,4] - dat1[,3])
  } else { 
    dat <- dat1 
  }
  
  if (length(dat) == 5){
    colnames(dat) <- c("Species", "Status", "min_age", "max_age", "trait")	
  } else {
    colnames(dat) <- c("Species", "Status", "min_age", "max_age")
  }
  
  dat$new_age <- "NA"
  splist <- unique(dat[, c(1, 2)])[order(unique(dat[, c(1, 2)][, 1])),]
  
  
  if (any(is.element(splist$Species[splist$Status == "extant"], splist$Species[splist$Status == "extinct"]))){
    print(intersect(splist$Species[splist$Status == "extant"], splist$Species[splist$Status == "extinct"]))
    stop("at least one species is listed as both extinct and extant\n")
  }
  
  cat("#!/usr/bin/env python", "from numpy import * ", "",  file=outfile, sep="\n")
  
  for (j in 1:replicates){
    times <- list()
    cat ("\nreplicate", j)
    
    dat[dat$min_age == 0,3] <- 0.001

    if (isTRUE(random)){
      dat$new_age <- round(stats::runif(length(dat[,1]), min=apply(dat[,3:4],FUN=min,1), max=apply(dat[,3:4],FUN=max,1)), digits=6)
    } else {
      for (i in 1:length(dat[,1])){
        dat$new_age[i] <- mean(c(dat[i,3], dat[i,4]))
      }				
    }
    
    dat2 <- subset(dat, select=c("Species","new_age"))
    taxa <- sort(unique(dat2$Species))
    
    for (n in 1:length(taxa)){
      times[[n]] <- dat2$new_age[dat2$Species == taxa[n]]
      if (toupper(splist$Status[splist$Species == taxa[n]]) == toupper("extant")){
        times[[n]] <- append(times[[n]], "0", after=length(times[[n]]))
      }
    }
    
    dat3 <- matrix(data=NA, nrow=length(times), ncol=max(sapply(times, length)))
    rownames(dat3) <- taxa
    
    for (p in 1:length(times)){
      dat3[p,1:length(times[[p]])] <- times[[p]]
    }
    
    cat(noquote(sprintf("\ndata_%s=[", j)), file=outfile, append=TRUE)
    
    for (n in 1:(length(taxa)-1)){
      rec <- paste(dat3[n,!is.na(dat3[n,])], collapse=",")
      cat(noquote(sprintf("array([%s]),", rec)), file=outfile, append=TRUE, sep="\n")
    }
    
    n <- n+1
    rec <- paste(dat3[n,!is.na(dat3[n,])], collapse=",")
    cat(noquote(sprintf("array([%s])", rec)), file=outfile, append=TRUE, sep="\n")
    
    cat("]", "", file=outfile, append=TRUE, sep="\n")
  }
  
  
  data_sets <- ""
  names <- ""
  
  if (replicates > 1){
    for (j in 1:(replicates-1)) {
      data_sets <- paste(data_sets, noquote(sprintf("data_%s,", j)))
      names <- paste(names, noquote(sprintf(" '%s_%s',", fname,j)))
    }
    
    data_sets <- paste(data_sets, noquote(sprintf("data_%s", j+1)))
    names <- paste(names, noquote(sprintf(" '%s_%s',", fname,j+1)))
  } else {
    data_sets <- "data_1"
    names <- noquote(sprintf(" '%s_1'", fname))	
  }
  
  cat(noquote(sprintf("d=[%s]", data_sets)), noquote(sprintf("names=[%s]", names)), "def get_data(i): return d[i]", "def get_out_name(i): return  names[i]", file=outfile, append=TRUE, sep="\n")
  
  
  tax_names <- paste(taxa, collapse="','")
  cat(noquote(sprintf("taxa_names=['%s']", tax_names)), "def get_taxa_names(): return taxa_names", file=outfile, append=TRUE, sep="\n")
  
  
  if ("trait" %in% colnames(dat)){
    datBM <- dat[,1]
    splist$Trait <- NA
    for (n in 1:length(splist[,1])){
      splist$Trait[n] <- mean(dat$trait[datBM == splist[n,1]], na.rm=T)
    }
    s1 <- "\ntrait1=array(["
    BM <- gsub("NaN|NA", "nan", toString(splist$Trait))
    s2 <- "])\ntraits=[trait1]\ndef get_continuous(i): return traits[i]"
    STR <- paste(s1,BM,s2)
    cat(STR, file=outfile, append=TRUE, sep="\n")
  }
  
  splistout <- paste(path, "/", fname, "_TaxonList.txt", sep="")
  lookup <- as.data.frame(taxa)
  lookup$status  <- "extinct"
  
  write.table(splist, file=splistout, sep="\t", row.names=F, quote=F)
  cat("\n\nPyRate input file was saved in: ", sprintf("%s", outfile), "\n\n")
  
}