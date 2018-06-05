#' Create Input Files for PyRate
#' 
#' Creates the input necessary to run Pyrate, based on a data.frame with fossil
#' ages (as derived e.g. from CleanCoordinatesFOS) and a vector of the
#' extinction status for each sample. Creates files in the working directory!
#' 
#' The replicate option allows the user to generate several replicates of the
#' data set in a single input file, each time re-drawing the ages of the
#' occurrences at random from uniform distributions with boundaries MinT and
#' MaxT. The replicates can be analyzed in different runs (see PyRate command
#' -j) and combining the results of these replicates is a way to account for
#' the uncertainty of the true ages of the fossil occurrences. Examples:
#' replicates=1 (default, generates 1 data set), replicates=10 (generates 10
#' random replicates of the data set).
#' 
#' @param x a data.frame. Containing geographical coordinates and species
#' names.
#' @param taxon a character string. The column with the taxon name. If
#' \dQuote{}, searches for outliers over the entire dataset, otherwise per
#' specified taxon. Default = \dQuote{identified_name}.
#' @param min.age a character string. The column with the minimum age. Default
#' = \dQuote{min_ma}.
#' @param max.age a character string. The column with the maximum age. Default
#' = \dQuote{max_ma}.
#' @param status a vector of character strings of length \code{nrow(x)}.
#' Indicating for each record \dQuote{extinct} or \dQuote{extant}.
#' @param trait a numeric vector of length \code{nrow(x)}. Indicating trait
#' values for each record. Optional.  Default = NULL.
#' @param fname a character string. The prefix to use for the output files.
#' @param path a character string. giving the absolute path to write the output
#' files. Default is the working directory.
#' @param replicates a numerical. The number of replicates for the randomized
#' age generation. See details. Default = 1.
#' @param cutoff a numerical. Specify a threshold to exclude fossil occurrences
#' with a high temporal uncertainty, i.e. with a wide temporal range between
#' min.age and max.age. Examples: cutoff=NULL (default; all occurrences are
#' kept in the data set) cutoff=5 (all occurrences with a temporal range of 5
#' Myr or higher are excluded from the data set)
#' @param random logical. Specify whether to take a random age (between MinT
#' and MaxT) for each occurrence or the midpoint age. Note that this option
#' defaults to TRUE if several replicates are generated (i.e. replicates > 1).
#' Examples: random = TRUE (default) random = FALSE (use midpoint ages)
#' @return PyRate input files in the working directory.
#' @note See \url{https://github.com/dsilvestro/PyRate/wiki} for more details
#' and tutorials on PyRate and PyRate input.
#' @author Daniele Silvestro
#' @keywords Fossil
#' @examples
#' 
#' minages <- runif(250, 0, 65)
#' exmpl <- data.frame(identified_name = sample(letters, size = 250, replace = TRUE),
#'                     lng = runif(250, min = 42, max = 51),
#'                     lat = runif(250, min = -26, max = -11),
#'                     min_ma = minages,
#'                     max_ma = minages + runif(250, 0.1, 65))
#' 
#' #a vector with the status for each record, 
#' #make sure species are only classified as either extinct or extant, 
#' #otherwise the function will drop an error
#' 
#' status <- sample(c("extinct", "extant"), size = nrow(exmpl), replace = TRUE)
#' 
#' #or from a list of species
#' status <- sample(c("extinct", "extant"), size = length(letters), replace = TRUE)
#' names(status) <- letters
#' status <- status[exmpl$identified_name]
#' 
#' \dontrun{
#' WritePyRate(x = exmpl,fname = "test", status = status)
#' }
#' 
#' @export
#' @importFrom stats runif 
#' @importFrom utils write.table
WritePyRate <- function(x,
                        status,
                        fname, 
                        taxon = "accepted_name", 
                        min.age = "min_ma", 
                        max.age = "max_ma",
                        trait = NULL, 
                        path = getwd(), 
                        replicates = 1,
                        cutoff = NULL, 
                        random = TRUE) {
  if (missing(fname)) {
    stop("'fname' missing with no default")
  }

  if (missing(status)) {
    stop("'status' missing with no default.")
  }

  # adapt input data
  dat1 <- data.frame(species = x[, taxon], 
                     Status = status, 
                     min_age = x[, min.age], 
                     max_age = x[, max.age])

  if (length(trait) > 0) {
    dat1 <- data.frame(dat1, trait)
  }

  outfile <- paste(path, "/", fname, "_PyRate.py", sep = "")

  dat1[, 1] <- gsub("[[:blank:]]{1,}", "_", dat1[, 1])

  if (replicates > 1) {
    random <- TRUE
  }

  if (any(is.na(dat1[, 1:4]))) {
    print(c(which(is.na(dat1[, 1])), which(is.na(dat1[, 2])), 
            which(is.na(dat1[, 3])), which(is.na(dat1[, 4]))))
    stop(paste("the input file contains missing data",
               "in species names, status or ages\n"))
  }

  if (!is.null(cutoff)) {
    dat <- dat1[!(dat1[, 4] - dat1[, 3] >= cutoff), ]
    cat("\n\nExcluded ", 
        100 - round(100 * dim(dat)[1] / dim(dat1)[1]), 
        "% occurrences")
    hist(dat1[, 4] - dat1[, 3])
  } else {
    dat <- dat1
  }

  if (length(dat) == 5) {
    colnames(dat) <- c("Species", "Status", "min_age", "max_age", "trait")
  } else {
    colnames(dat) <- c("Species", "Status", "min_age", "max_age")
  }

  dat$new_age <- "NA"
  splist <- unique(dat[, c(1, 2)])[order(unique(dat[, c(1, 2)][, 1])), ]


  if (any(is.element(splist$Species[splist$Status == "extant"], 
                     splist$Species[splist$Status == "extinct"]))) {
    print(intersect(splist$Species[splist$Status == "extant"], 
                    splist$Species[splist$Status == "extinct"]))
    stop("at least one species is listed as both extinct and extant\n")
  }

  cat("#!/usr/bin/env python", "from numpy import * ", "",
    file = outfile,
    sep = "\n"
  )

  for (j in 1:replicates) {
    times <- list()
    cat("\nreplicate", j)

    dat[dat$min_age == 0, 3] <- 0.001

    if (isTRUE(random)) {
      dat$new_age <- round(stats::runif(length(dat[, 1]), 
                                        min = apply(dat[,3:4], FUN = min, 1), 
                                        max = apply(dat[, 3:4], FUN = max, 1)),
      digits = 6
      )
    } else {
      for (i in seq_along(dat[, 1])) {
        dat$new_age[i] <- mean(c(dat[i, 3], dat[i, 4]))
      }
    }

    dat2 <- subset(dat, select = c("Species", "new_age"))
    taxa <- sort(unique(dat2$Species))

    for (n in seq_along(taxa)) {
      times[[n]] <- dat2$new_age[dat2$Species == taxa[n]]
      if (toupper(splist$Status[splist$Species == taxa[n]]) == 
          toupper("extant")) {
        times[[n]] <- append(times[[n]], "0", after = length(times[[n]]))
      }
    }

    dat3 <- matrix(data = NA, 
                   nrow = length(times), 
                   ncol = max(vapply(times, length, FUN.VALUE = numeric(1))))
    rownames(dat3) <- taxa

    for (p in seq_along(times)) {
      dat3[p, seq_along(times[[p]])] <- times[[p]]
    }

    cat(noquote(sprintf("\ndata_%s=[", j)), file = outfile, append = TRUE)

    for (n in seq_len(length(taxa) - 1)) {
      rec <- paste(dat3[n, !is.na(dat3[n, ])], collapse = ",")
      cat(noquote(sprintf("array([%s]),", rec)),
        file = outfile, append = TRUE,
        sep = "\n"
      )
    }

    n <- n + 1
    rec <- paste(dat3[n, !is.na(dat3[n, ])], collapse = ",")
    cat(noquote(sprintf("array([%s])", rec)),
      file = outfile, append = TRUE,
      sep = "\n"
    )

    cat("]", "", file = outfile, append = TRUE, sep = "\n")
  }


  data_sets <- ""
  names <- ""

  if (replicates > 1) {
    for (j in 1:(replicates - 1)) {
      data_sets <- paste(data_sets, noquote(sprintf("data_%s,", j)))
      names <- paste(names, noquote(sprintf(" '%s_%s',", fname, j)))
    }

    data_sets <- paste(data_sets, noquote(sprintf("data_%s", j + 1)))
    names <- paste(names, noquote(sprintf(" '%s_%s',", fname, j + 1)))
  } else {
    data_sets <- "data_1"
    names <- noquote(sprintf(" '%s_1'", fname))
  }

  cat(noquote(sprintf("d=[%s]", data_sets)), noquote(sprintf(
    "names=[%s]",
    names
  )), "def get_data(i): return d[i]", "def get_out_name(i): return  names[i]",
  file = outfile, append = TRUE, sep = "\n"
  )


  tax_names <- paste(taxa, collapse = "','")
  cat(noquote(sprintf("taxa_names=['%s']", tax_names)), 
      "def get_taxa_names(): return taxa_names",
    file = outfile, append = TRUE, sep = "\n"
  )


  if ("trait" %in% colnames(dat)) {
    datBM <- dat[, 1]
    splist$Trait <- NA
    for (n in seq_along(splist[, 1])) {
      splist$Trait[n] <- mean(dat$trait[datBM == splist[n, 1]], na.rm = TRUE)
    }
    s1 <- "\ntrait1=array(["
    BM <- gsub("NaN|NA", "nan", toString(splist$Trait))
    s2 <- "])\ntraits=[trait1]\ndef get_continuous(i): return traits[i]"
    STR <- paste(s1, BM, s2)
    cat(STR, file = outfile, append = TRUE, sep = "\n")
  }

  splistout <- paste(path, "/", fname, "_TaxonList.txt", sep = "")
  lookup <- as.data.frame(taxa)
  lookup$status <- "extinct"

  write.table(splist, 
              file = splistout, 
              sep = "\t", 
              row.names = FALSE, 
              quote = FALSE)
  cat("\n\nPyRate input file was saved in: ", sprintf("%s", outfile), "\n\n")
}
