######################################################################################
#                                                                                    #
# Version:  1.0                                                                      #
# Revision: 0 - 10/02/2015. Published version                                        #
#                                                                                    #
######################################################################################
CalcDARLEQ.R <- function(data = NULL, version="latest") {

  # Input columns:
  #(Type), SampleID, siteID,  Date (date object), Code, Taxa, Abundance, Alk
  
  # Input handling
  if (is.null(data))
    stop("No dataframe has been specified as 'data'")
  
  if (ncol(data) < 7)
    stop("It seems the input data.frame does not have the required columns")
  
  if(any(!version %in% c("latest","previous", "all")))
    stop("Input version is not valid, please verify its value(s)")
  
  # Define constants matching input parameters
  METRICR <- "rivers"
  VERSL <- "latest"
  VERSP <- "previous"
  EQR.CAP <- 1.00
  
  # Matrices. EQR status class boundaries for lakes and rivers
  dim.names <- list(c("Bad", "Poor", "Moderate", "Good", "High"),
                    "upperlimit")
  
  TDI3class <- matrix(c(0.26, 0.52, 0.78, 0.93, 1.00), nrow=5, dimnames = dim.names)
  
  TDI4class <- matrix(c(0.20, 0.40, 0.60, 0.80, 1.00), nrow=5, dimnames = dim.names)
  
  if ("all" %in% version) {
    versions <- c(VERSL, VERSP)
  } else {
    versions <- version
  }
  
  input.names <- names(data)
  if (ncol(data) == 7) {
    names(data) <- c("SampleID", "SiteID", "Date", "Code", "Taxa", 
                     "Abundance", "Alk")
  } else {
    names(data) <- c("Type", "SampleID", "SiteID",  "Date", "Code", 
                     "Taxa", "Abundance", "Alk")
  }

  # Find out which taxa has no presence in the scoring list (TDI4 or TDI3)
  not.in.list <- !(data$Code %in% LookUpDARLEQ2$TaxonId)
  na.tdi4 <- LookUpDARLEQ2[is.na(LookUpDARLEQ2$TDI4) & LookUpDARLEQ2$TaxonId %in% unique(data$Code),"TaxonId"]
  na.tdi3 <- LookUpDARLEQ2[is.na(LookUpDARLEQ2$TDI3) & LookUpDARLEQ2$TaxonId %in% unique(data$Code),"TaxonId"]

  lacking.tdi4 <- unique(data[not.in.list | data$Code %in% na.tdi4, c("Code", "Taxa")])
  lacking.tdi4 <- lacking.tdi4[order(lacking.tdi4$Code), ]
  rownames(lacking.tdi4) <- NULL

  lacking.tdi3 <- unique(data[not.in.list | data$Code %in% na.tdi3, c("Code", "Taxa")])
  lacking.tdi3 <- lacking.tdi3[order(lacking.tdi3$Code), ]
  rownames(lacking.tdi3) <- NULL

  # Merge
  data <- merge(data, LookUpDARLEQ2, by.x="Code", by.y="TaxonId")

  # Initialisation
  river.samples <- NA
  CoC.rivers <- NA

  # Alkalinity
  with (data, {

    if (VERSL %in% versions) {
      data$`TDI4 Alk` <<- ifelse(Alk < 5, 5, ifelse(Alk > 250, 250, Alk))
    } 

    if (VERSP %in% versions) {
      data$`TDI3 Alk` <<- ifelse(Alk < 6, 6, ifelse(Alk > 150, 150, Alk))
    }

  })

  data$s <- ifelse(as.POSIXlt(data$Date)$mon + 1 > 6, 1, 0)

  agg.data <- unique(data[ , c("SampleID", "SiteID", "Date", "Alk")])


  # Compute metrics
  river.samples <- as.data.frame(
    do.call(rbind, by(data, data$SampleID, FUN=function(df) {

    out <- data.frame(NA, stringsAsFactors=FALSE)
                                                       
    if (VERSL %in% versions) {
                                                         
      out$oTDI4 <- (sum(df$Abundance * df$TDI4, na.rm=TRUE) / 
                      sum(df[!is.na(df$TDI4), "Abundance"], na.rm=TRUE) * 25) - 25

      out$eTDI4 <- 9.933 * exp(log10(df$`TDI4 Alk`[1]) * 0.81)

      out$`EQR TDI4` <- ((100 - out$oTDI4) / (100 - out$eTDI4)) * 0.80

      out[out$`EQR TDI4` > EQR.CAP & !is.na(out$`EQR TDI4`), "EQR TDI4"] <- EQR.CAP

      intervals <- cut(out$`EQR TDI4`,
                        breaks=c(0, TDI4class[, "upperlimit"]), include.lowest=TRUE)
      key <- data.frame(range=levels(intervals), rc=rownames(TDI4class))
      out$`Class TDI4` <- key[match(intervals, key$range), 2]

    }

    if (VERSP %in% versions) { #older version

      out$oTDI3 <- (sum(df$Abundance * df$TDI3, na.rm=TRUE) /
                      sum(df[!is.na(df$TDI3), "Abundance"], na.rm=TRUE) * 25) - 25

      out$eTDI3 <- -25.36 + 56.83 * log10(df$`TDI3 Alk`[1]) - 12.96 * 
        (log10(df$`TDI3 Alk`[1]))^2 + 3.21 * df$s[1]

      out$`EQR TDI3` <- (100 - out$oTDI3) / (100 - out$eTDI3)

      out[out$`EQR TDI3` > EQR.CAP & !is.na(out$`EQR TDI3`), "EQR TDI3"] <- EQR.CAP

      intervals <- cut(out$`EQR TDI3`,
                         breaks=c(0, TDI3class[, "upperlimit"]), include.lowest=TRUE)
      key <- data.frame(range=levels(intervals), rc=rownames(TDI3class))
      out$`Class TDI3` <- key[match(intervals, key$range), 2]

    }

    out$`% Planktic` <- sum(df$Abundance[df$Planktic == TRUE], na.rm=TRUE) /
      sum(df$Abundance, na.rm=TRUE) * 100
    out$`% Motile` <- sum(df$Abundance[df$Motile == TRUE], na.rm=TRUE) /
      sum(df$Abundance, na.rm=TRUE) * 100
    out$`% Organic tolerant` <- sum(df$Abundance[df$OrganicTolerant == TRUE],
      na.rm=TRUE) / sum(df$Abundance, na.rm=TRUE) * 100
    out$`% Saline` <- sum(df$Abundance[df$Saline == TRUE], na.rm=TRUE) /
      sum(df$Abundance, na.rm=TRUE) * 100

    out[, -1]
                                                       
  }))) # end of river.samples

  river.samples$SampleID <- rownames(river.samples)
  rownames(river.samples) <- NULL

  river.samples <- merge(agg.data, river.samples)
  
  if (VERSL %in% versions) {

    lat.CoC.rivers <- as.data.frame(
      do.call(rbind, by(river.samples, river.samples$SiteID, FUN=function(df) {

        # Mean and number of samples
        m <- mean(df$`EQR TDI4`)
        n <- length(df$`EQR TDI4`)

        # Class
        intervals <- cut(m, breaks=c(0, TDI4class[, "upperlimit"]), 
                         include.lowest=TRUE)
        key <- data.frame(range=levels(intervals), rc=rownames(TDI4class))
        class <- key[match(intervals, key$range), 2]

        latest <- .DARLEQ.CoC(m, n, METRICR, class, TDI4class)

    })))

    lat.CoC.rivers$SiteID <- rownames(lat.CoC.rivers)
    rownames(lat.CoC.rivers) <- NULL
    lat.CoC.rivers <- lat.CoC.rivers[c(ncol(lat.CoC.rivers), 1:ncol(lat.CoC.rivers)-1)]

  }
  
  if (VERSP %in% versions) {

    prev.CoC.rivers <- as.data.frame(
      do.call(rbind, by(river.samples, river.samples$SiteID, FUN=function(df) {

        # Mean and number of samples
        m <- mean(df$`EQR TDI3`)
        n <- length(df$`EQR TDI3`)

        # Class
        intervals <- cut(m, breaks=c(0, TDI3class[, "upperlimit"]),
                         include.lowest=TRUE)
        key <- data.frame(range=levels(intervals), rc=rownames(TDI3class))
        class <- key[match(intervals, key$range), 2]

        previous <- .DARLEQ.CoC(m, n, METRICR, class, TDI3class)

    })))

    prev.CoC.rivers$SiteID <- rownames(prev.CoC.rivers)
    rownames(prev.CoC.rivers) <- NULL
    prev.CoC.rivers <- prev.CoC.rivers[c(ncol(prev.CoC.rivers), 1:ncol(prev.CoC.rivers)-1)]

  }
  
  DARLEQ.R <- switch(version,
                     all= { list(rivers=river.samples,
                                 tdi4.CoC=lat.CoC.rivers, 
                                 tdi3.CoC=prev.CoC.rivers,
                                 taxa.lacking.tdi4=lacking.tdi4,
                                 taxa.lacking.tdi3=lacking.tdi3)
                     },
                     latest= { list(rivers=river.samples,
                                    tdi4.CoC=lat.CoC.rivers,
                                    taxa.lacking.tdi4=lacking.tdi4)
                     },
                     previous= { list(rivers=river.samples,
                                      tdi3.CoC=prev.CoC.rivers,
                                      taxa.lacking.tdi3=lacking.tdi3)
                     }
  )

} # end CalcDARLEQ.R