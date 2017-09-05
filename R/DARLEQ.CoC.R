######################################################################################
#                                                                                    #
# Version:  1.0                                                                      #
# Revision: 0 - 10/02/2015. Published version                                        #
#                                                                                    #
######################################################################################
# Parameters:
# x:      EQR mean within a site
# n:      Number of samples used to calculate the mean
# metric: Either "rivers" or "lakes"
# class:  Status class of the EQR mean. "Bad", "Poor", "Moderate", "Good" or "High"
# b:      Matrix of EQR status class boundaries as defined in DARLEQ.R / DARLEQ.L
.DARLEQ.CoC <- function(x, n, metric, class, b) {

  # Input handling
  if (is.null(x))
    stop("No value of EQR mean has been specified")

  if (is.null(n))
    stop("The number of samples used to compute the mean needs to be specified")

  if (is.null(metric))
    stop("The metric to compute has not been specified (rivers or lakes)")

  if (is.null(class))
    stop("A class must be specified: Bad, Poor, Moderate, Good or High")

  .class <- tolower(class)
  if (any(!.class %in% c("high", "good", "moderate", "poor", "bad")))
    stop("Incorrect class. Please verify spelling.")

  if (is.null(b))
    stop("Boundary classes must be specified in vector parameter 'b'")
  
  # Make boundaries readable
  b.bad <- b[1]
  b.poor <- b[2]
  b.mod <- b[3]
  b.good <- b[4]

  # Definition of the fitting function
  if (metric == "rivers") {

    r <- function (m) { y <- 0.03 + 0.177 * m + (-0.157) * m ^ 5.73 } 

  } else {

    r <- function(m) { y <- 0.03 + 0.273 * m + (-0.253) * m ^ 1.96 }

  }

  if (x == 0L) { # mean = 0L, then it is Bad (-)

    bad <- 100L
    poor <- 0L
    mod <- 0L
    good <- 0L
    high <- 0L
    rom <- 0L
    hg <- 0L
    mpb <- 100L
    rom.gm <- 0L

  } else if (x == 1L) { # mean = 1L, then it is High

    bad <- 0L
    poor <- 0L
    mod <- 0L
    good <- 0L
    high <- 100L
    rom <- 0L
    hg <- 100L
    mpb <- 0L
    rom.gm <- 0L

  } else {

    # Standard Error and (logit) transformation of variables
    SE <- r(x) / sqrt(n)
    trns.x <- log(x / (1 - x))
    #trns.err <- SE / (x * (1 - x))

    # Transformed standard error for each boundary
    trns.err.b <- (r(b.bad) / sqrt(n))/(b.bad * (1 - b.bad))
    trns.err.p <- (r(b.poor) / sqrt(n))/(b.poor * (1 - b.poor))
    trns.err.g <- (r(b.mod) / sqrt(n))/(b.mod * (1 - b.mod))
    trns.err.h <- (r(b.good) / sqrt(n))/(b.good * (1 - b.good))

    # Confidence of status, based on standard cumulative normal distribution.
    p.bad <- 1 - pnorm((trns.x - log(b.bad / (1 - b.bad))) / trns.err.b)
    p.poor <- 1- pnorm((trns.x - log(b.poor / (1 - b.poor))) / trns.err.p)
    p.mod <- 1- pnorm((trns.x - log(b.mod / (1 - b.mod))) / trns.err.g)
    p.good <- 1 - pnorm((trns.x - log(b.good / (1 - b.good))) / trns.err.h)

    bad <- 100 * p.bad
    poor <- 100 * (p.poor - p.bad)
    mod <- 100 * (p.mod - p.poor)
    good <- 100 * (p.good - p.mod)
    high <- 100 * (1 - p.good)

    # Risk of misclassification for predicted class
    rom <- switch(.class,
                  bad = { 100 - bad },
                  poor = { 100 - poor },
                  moderate = { 100 - mod },
                  good = { 100 - good },
                  high = { 100 - high },
           )

    # Confidence the site is either better than moderate (hg) or worse than good (mpb)
    hg <- good + high
    mpb <- mod + poor + bad

    # Risk of misclassification above/below good/moderate boundary
    rom.gm <- switch(.class,
                      bad =,
                      poor =,
                      moderate = { hg },
                      good =,
                      high = { mpb }

              )

  }

  # Return vector of values
  ret <- data.frame(n, x, class, high, good, mod, poor, bad, rom, hg, mpb, rom.gm)
  names(ret) <- c("n", "Mean EQR", "Class","CoC High", "CoC Good", "CoC Mod",
                  "CoC Poor", "CoC Bad", "RoM", "CoC HG", "CoC MPB", "RoM-G/M")
  ret

}