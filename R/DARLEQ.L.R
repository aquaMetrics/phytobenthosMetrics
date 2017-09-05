######################################################################################
#                                                                                    #
# Version:  1.0                                                                      #
# Revision: 0 - 10/02/2015. Published version                                        #
#                                                                                    #
######################################################################################
CalcDARLEQ.L <- function(data = NULL, version="latest") {
  
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
  METRICL <- "lakes"
  VERSL <- "latest"
  VERSP <- "previous"
  EQR.CAP <- 1.00
  
  # Matrices. EQR status class boundaries for lakes and rivers
  dim.names <- list(c("Bad", "Poor", "Moderate", "Good", "High"),
                    "upperlimit")
  
  LTDI1class.HA <- matrix(c(0.22, 0.44, 0.66, 0.90, 1.00), nrow=5,
                          dimnames = dim.names)
  
  LTDI1class.MA <- matrix(c(0.22, 0.44, 0.66, 0.90, 1.00), nrow=5,
                          dimnames = dim.names)
  
  LTDI1class.LA <- matrix(c(0.22, 0.44, 0.63, 0.90, 1.00), nrow=5,
                          dimnames = dim.names)
  
  LTDI2class.HA <- matrix(c(0.23, 0.46, 0.70, 0.92, 1.00), nrow=5,
                          dimnames = dim.names)
  
  LTDI2class.MA <- matrix(c(0.23, 0.46, 0.66, 0.93, 1.00), nrow=5,
                          dimnames = dim.names)
  
  LTDI2class.LA <- matrix(c(0.23, 0.46, 0.70, 0.92, 1.00), nrow=5,
                          dimnames = dim.names)
  
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

  # Find out which taxa has no presence in the scoring list (LTDI2 or LTDI1)
  not.in.list <- !(data$Code %in% LookUpDARLEQ2$TaxonId)
  na.ltdi2 <- LookUpDARLEQ2[is.na(LookUpDARLEQ2$LTDI2) & LookUpDARLEQ2$TaxonId %in% unique(data$Code),"TaxonId"]
  na.ltdi1 <- LookUpDARLEQ2[is.na(LookUpDARLEQ2$LTDI1) & LookUpDARLEQ2$TaxonId %in% unique(data$Code),"TaxonId"]
  
  lacking.ltdi2 <- unique(data[not.in.list | data$Code %in% na.ltdi2, c("Code", "Taxa")])
  lacking.ltdi2 <- lacking.ltdi2[order(lacking.ltdi2$Code), ]
  rownames(lacking.ltdi2) <- NULL
  
  lacking.ltdi1 <- unique(data[not.in.list | data$Code %in% na.ltdi1, c("Code", "Taxa")])
  lacking.ltdi1 <- lacking.ltdi1[order(lacking.ltdi1$Code), ]
  rownames(lacking.ltdi1) <- NULL

  # Merge
  data <- merge(data, LookUpDARLEQ2, by.x="Code", by.y="TaxonId")
  
  # Initialisation
  lake.samples <- NA
  CoC.lakes <- NA
  
  # Alkalinity
  with (data, {

    data$`Lake Alk class` <<- ifelse(Alk < 10, "LA", ifelse(Alk > 50, "HA", "MA"))

    if (VERSL %in% versions) {
      data$`eLTDI2` <<- ifelse(Alk < 10, 22, ifelse(Alk > 50, 42, 35))
    }

    if (VERSP %in% versions) {
      data$`eLTDI1` <<- ifelse(Alk < 10, 20, 25)
    }

  })

  agg.data <- unique(data[ , c("SampleID", "SiteID", "Date", "Alk")])


  # Compute metrics
  lake.samples <- as.data.frame(
    do.call(rbind, by(data, data$SampleID, FUN=function(df) {

    out <- data.frame(NA, stringsAsFactors=FALSE)

    if (VERSL %in% versions) {

      out$oLTDI2 <- (sum(df$Abundance * df$LTDI2, na.rm=TRUE) /
                       sum(df[!is.na(df$LTDI2), "Abundance"], na.rm=TRUE) * 25) - 25

      out$`Lake Alk class` <- df$`Lake Alk class`[1]
      out$eLTDI2 <- df$eLTDI2[1]

      out$`EQR LTDI2` <- (100 - out$oLTDI2) / (100 - out$eLTDI2)

      out[out$`EQR LTDI2` > EQR.CAP & !is.na(out$`EQR LTDI2`), "EQR LTDI2"] <- EQR.CAP

      # LTDI2class.LA intervals
      intervals <- cut(out[out$`Lake Alk class` == "LA", "EQR LTDI2"], 
                       breaks=c(0, LTDI2class.LA[, "upperlimit"]), include.lowest=TRUE)
      key <- data.frame(range=levels(intervals), rc=rownames(LTDI2class.LA))
      out[out$`Lake Alk class` == "LA", "Class LTDI2"] <- 
        key[match(intervals, key$range), 2]

      # LTDI2class.MA intervals
      intervals <- cut(out[out$`Lake Alk class` == "MA", "EQR LTDI2"],
                       breaks=c(0, LTDI2class.MA[, "upperlimit"]), include.lowest=TRUE)
      key <- data.frame(range=levels(intervals), rc=rownames(LTDI2class.MA))
      out[out$`Lake Alk class` == "MA", "Class LTDI2"] <- 
        key[match(intervals, key$range), 2]
                                                        
      # LTDI2class.HA intervals
      intervals <- cut(out[out$`Lake Alk class` == "HA", "EQR LTDI2"],
                       breaks=c(0, LTDI2class.HA[, "upperlimit"]), include.lowest=TRUE)
      key <- data.frame(range=levels(intervals), rc=rownames(LTDI2class.HA))
      out[out$`Lake Alk class` == "HA", "Class LTDI2"] <- 
        key[match(intervals, key$range), 2]
                                                        
    }

    if (VERSP %in% versions) { # older version

      out$oLTDI1 <- (sum(df$Abundance * df$LTDI1, na.rm=TRUE) /
                       sum(df[!is.na(df$LTDI1), "Abundance"], na.rm=TRUE) * 25) - 25

      out$`Lake Alk class` <- df$`Lake Alk class`[1]
      out$eLTDI1 <- df$eLTDI1[1]

      out$`EQR LTDI1` <- (100 - out$oLTDI1) / (100 - out$eLTDI1)

      out[out$`EQR LTDI1` > EQR.CAP & !is.na(out$`EQR LTDI1`), "EQR LTDI1"] <- EQR.CAP
                                                        
      # LTDI1class.LA intervals
      intervals <- cut(out[out$`Lake Alk class` == "LA", "EQR LTDI1"],
                       breaks=c(0, LTDI1class.LA[, "upperlimit"]), include.lowest=TRUE)
      key <- data.frame(range=levels(intervals), rc=rownames(LTDI1class.LA))
      out[out$`Lake Alk class` == "LA", "Class LTDI1"] <- 
        key[match(intervals, key$range), 2]

      # LTDI1class.MA intervals
      intervals <- cut(out[out$`Lake Alk class` == "MA", "EQR LTDI1"],
                       breaks=c(0, LTDI1class.MA[, "upperlimit"]), include.lowest=TRUE)
      key <- data.frame(range=levels(intervals), rc=rownames(LTDI1class.MA))
      out[out$`Lake Alk class` == "MA", "Class LTDI1"] <-
        key[match(intervals, key$range), 2]

      # LTDI1class.HA intervals
      intervals <- cut(out[out$`Lake Alk class` == "HA" & !is.na(out$`Lake Alk class`),
          "EQR LTDI1"], breaks=c(0, LTDI1class.HA[, "upperlimit"]),
          include.lowest=TRUE)
      key <- data.frame(range=levels(intervals), rc=rownames(LTDI1class.HA))
      out[out$`Lake Alk class` == "HA", "Class LTDI1"] <- 
        key[match(intervals, key$range), 2]

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
                                                      
  }))) # End of lake.samples
    
  lake.samples$SampleID <- rownames(lake.samples)
  rownames(lake.samples) <- NULL

  lake.samples <- merge(agg.data, lake.samples)
  
  if (VERSL %in% versions) {
  
    lat.CoC.lakes <- as.data.frame(
      do.call(rbind, by(lake.samples, lake.samples$SiteID, FUN=function(df) {

      # Mean and number of samples
      m <- mean(df$`EQR LTDI2`)
      n <- length(df$`EQR LTDI2`)

      # Class
      if (df$`Lake Alk class`[1] == "LA") {

        intervals <- cut(m, breaks=c(0, LTDI2class.LA[, "upperlimit"]),
                         include.lowest=TRUE)
        key <- data.frame(range=levels(intervals), rc=rownames(LTDI2class.LA))
        b <- LTDI2class.LA
                                                       
      } else if (df$`Lake Alk class`[1] == "MA") {

        intervals <- cut(m, breaks=c(0, LTDI2class.MA[, "upperlimit"]),
                         include.lowest=TRUE)
        key <- data.frame(range=levels(intervals), rc=rownames(LTDI2class.MA))
        b <- LTDI2class.MA

      } else {

        intervals <- cut(m, breaks=c(0, LTDI2class.HA[, "upperlimit"]),
                         include.lowest=TRUE)
        key <- data.frame(range=levels(intervals), rc=rownames(LTDI2class.HA))
        b <- LTDI2class.HA

      }

      class <- key[match(intervals, key$range), 2]

      latest <- .DARLEQ.CoC(m, n, METRICL, class, b)

    })))
    
    lat.CoC.lakes$SiteID <- rownames(lat.CoC.lakes)
    rownames(lat.CoC.lakes) <- NULL
    lat.CoC.lakes <- lat.CoC.lakes[c(ncol(lat.CoC.lakes), 1:ncol(lat.CoC.lakes)-1)]

  }                                              

  if (VERSP %in% versions) {

    prev.CoC.lakes <- as.data.frame(
      do.call(rbind, by(lake.samples, lake.samples$SiteID, FUN=function(df) {

      # Mean and number of samples
      m <- mean(df$`EQR LTDI1`)
      n <- length(df$`EQR LTDI1`)

      # Class
      if (df$`Lake Alk class`[1] == "LA") {

        intervals <- cut(m, breaks=c(0, LTDI1class.LA[, "upperlimit"]),
                         include.lowest=TRUE)
        key <- data.frame(range=levels(intervals), rc=rownames(LTDI1class.LA))
        b <- LTDI1class.LA

      } else if (df$`Lake Alk class`[1] == "MA") {

        intervals <- cut(m, breaks=c(0, LTDI1class.MA[, "upperlimit"]),
                         include.lowest=TRUE)
        key <- data.frame(range=levels(intervals), rc=rownames(LTDI1class.MA))
        b <- LTDI1class.MA

      } else {

         intervals <- cut(m, breaks=c(0, LTDI1class.HA[, "upperlimit"]),
                          include.lowest=TRUE)
         key <- data.frame(range=levels(intervals), rc=rownames(LTDI1class.HA))
         b <- LTDI1class.HA

      }

      class <- key[match(intervals, key$range), 2]

      previous <- .DARLEQ.CoC(m, n, METRICL, class, b)

    })))

    prev.CoC.lakes$SiteID <- rownames(prev.CoC.lakes)
    rownames(prev.CoC.lakes) <- NULL
    prev.CoC.lakes <- prev.CoC.lakes[c(ncol(prev.CoC.lakes), 1:ncol(prev.CoC.lakes)-1)]

  }

  DARLEQ.L <- switch(version,
                     all= { list(lakes=lake.samples, 
                                 ltdi2.CoC=lat.CoC.lakes, 
                                 ltdi1.CoC=prev.CoC.lakes,
                                 taxa.lacking.ltdi2=lacking.ltdi2,
                                 taxa.lacking.ltdi1=lacking.ltdi1)
                     },
                     latest= { list(lakes=lake.samples,
                                    ltdi2.CoC=lat.CoC.lakes,
                                    taxa.lacking.ltdi2=lacking.ltdi2)
                     },
                     previous= { list(lakes=lake.samples,
                                      ltdi1.CoC=prev.CoC.lakes,
                                      taxa.lacking.ltdi1=lacking.ltdi1)
                     }
  )

} # end CalcDARLEQ.L