#' Diatom Assessment of River and Lake Ecological Quality
#'
#' DARLEQ is one of the methods used to evaluate phytobentos (algae) in rivers
#' and lakes. Assessment is focused on the diatoms, a large and diverse group of
#' algae using a metric called the Trophic Diatom Index (TDI), based on the
#' principle that different diatoms have different environmental preferences.
#'
#' @usage CalcDARLEQ(data = NULL, metric = NULL, version = "latest")
#' @param data A dataframe containing diatoms taxa with
#' \emph{eight columns in the specified order} (naming of the columns is not
#' important):
#' \code{Type} (one character, either \samp{"L"} -lake- or \samp{"R"} -river-),
#' \code{SampleID}, \code{SiteID}, \code{Date} (Date object), \code{Taxon Code},
#' \code{Taxon name}, \code{Abundance}, \code{Alkalinity}.
#' @param metric Specifies which metric to compute. It can take any of the
#'   following values: \code{"rivers"}, \code{"lakes"} or \code{"all"}.
#' @param version Specifies which version of the metric to compute. It can take
#'   any of the following values: \code{"latest"} (TDI4/LTDI2),
#'   \code{"previous"} (TDI3/LTDI1) or \code{"all"}.
#' @details This routine does also compute the uncertainty associated to the
#' ecological status assessments of lakes and rivers. Calculation of the
#' confidence of class (CoC) and risk of misclassification is based in a
#' polynomial fitting of the SD of EQR as a function of mean EQR. The parameters
#' of those polynomial curves are the same found in DARLEQ2 software (Kelly et
#' al, 2014). This polynomial function makes it possible to compute an expected
#' SD for any given EQR and by relating this SD to the position of the status
#' class boundaries enables the confidence to be calculated.
#' @return A list containing up to 10 dataframes, depending on the \emph{metric}
#' and \emph{version} parameters. \itemize{ \item\code{rivers}: Dataframe
#' containing river results for every metric computed (TDI3/TDI4).
#'
#' \item\code{tdi4.CoC}: Dataframe containing the confidence of classification
#' for the metric TDI4.
#'
#' \item\code{tdi3.CoC}: Dataframe containing the confidence of classification
#' for the metric TDI3.
#'
#' \item\code{taxa.lacking.tdi4}: Dataframe containing all those taxa which lack
#' TDI4 scores in the database. It will be empty if all taxa find a match.
#'
#' \item\code{taxa.lacking.tdi3}: Dataframe containing all those taxa which lack
#' TDI3 scores in the database. It will be empty if all taxa find a match.
#'
#' \item\code{lakes}: Dataframe containing lake results for every metric
#' computed (LTDI1/LTDI2).
#'
#' \item\code{ltdi2.CoC}: Dataframe containing the confidence of classification
#' for the metric LTDI2.
#'
#' \item\code{ltdi1.CoC}: Dataframe containing the confidence of classification
#' for the metric LTDI1.
#'
#' \item\code{taxa.lacking.ltdi2}: Dataframe containing all those taxa which
#' lack LTDI2 scores in the database. It will be empty if all taxa find a match.
#'
#' \item\code{taxa.lacking.ltdi1}: Dataframe containing all those taxa which
#' lack LTDI1 scores in the database. It will be empty if all taxa find a match.
#' }
#' @references Bennion H., Kelly M.G., Juggins S., Yallop M., Burgess A.,
#' Jamieson J., Krokowski J. 2014. \emph{Assessment of ecological status in UK
#' lakes using benthic diatoms}. Freshwater Science. 33(2): 639-654.
#'
#' Ellis J., Adriaenssens V. 2006. \emph{Uncertainty estimation for monitoring
#' results by the WFD biological classification tools}. WFD REport:
#' GEHO1006BLOR-E-P. Environment Agency (UK)
#'
#' Kelly M.G., Juggins S., Bennion, H., Burgess A., Yallop M., Hirst H., King
#' L., Jamieson B.J., Guthrie R., Rippey B. 2008. \emph{Use of diatoms for
#' evaluating ecological status in UK freshwaters}. Science Report:
#' SC030103/SR4. Environment Agency (UK)
#'
#' Kelly M., Juggins S., Guthrie R., Pritchard S., Jamieson J., Rippey B., Hirst
#' H., Yallop M. 2008. \emph{Assessment of ecological status in UK rivers using
#' diatoms}. Freshwater Biology. 56: 403-422
#'
#' Kelly M., Bennion H., Burgess A., Ellis J., Juggins S., Guthrie R., Jamieson
#' J., Adrianssens V., Yallop M. 2009. \emph{Uncertainty in ecological status
#' assessments of lakes and rivers using diatoms}. Hydrobiologia. 633: 5-15.
#'
#' Kelly M., Juggins S., Bennion H., Burgess A., Yallop M., Hirst H., Jamieson
#' J., Guthrie R., Rippey B. 2014. \emph{Software for Freshwater Status
#' Classification using benthic diatoms (Version 2.0.1)} Available from
#' http://www.wfduk.org/sites/default/files/Media/Characterisation of the water
#' environment/Biological Method Statements/DARLEQ2Setup.zip
#'
#' WFD - UK Technical Advisory Group. 2014. \emph{Phytobentos - Diatoms for
#' Assessing River and Lake Ecological Quality (Lake DARLEQ2)}. WFD - UKTAG.
#'
#' WFD - UK Technical Advisory Group. 2014. \emph{Phytobentos - Diatoms for
#' Assessing River and Lake Ecological Quality (River DARLEQ2)}. WFD - UKTAG.
#' @examples
#' \dontrun{
#' DARLEQoutputs <- CalcDARLEQ(demoDiatomRiverData, metric = 'rivers')
#' }
#' @export


# As this code was written before linting enforced - skip checks
# nolint start
CalcDARLEQ <- function(data = NULL, metric = NULL, version = "latest") {


  ######################################################################################
  ## Version:  1.0
  ## Revision: 0 - 10/02/2015. Published version
  ## #
  ######################################################################################

  # Input handling
  if (is.null(data))
    stop("No dataframe has been specified as 'data'")

  if (is.null(metric))
    stop("The metric to compute has not been specified")

  if (ncol(data) < 8)
    stop("It seems the input data.frame does not have the required columns")

  if(class(metric) != "character")
    stop("Input metric must be a string")

  if(class(version) != "character")
    stop("Input version must be a string")

  if (any(!metric %in% c("rivers","lakes", "all")))
    stop("Input metric is not valid, please verify its value(s)")

  if(any(!version %in% c("latest","previous", "all")))
    stop("Input version is not valid, please verify its value(s)")

  # Define constants matching input parameters
  METRICR <- "rivers"
  METRICL <- "lakes"
  TYPER <- "R"
  TYPEL <- "L"

  if (metric == "all") {

    # even if the user specified "all" maybe there is only one type in data
    types <- unique(data[, 1])
    if (length(types) > 1) {

      metrics <- c(METRICR, METRICL)

    } else if (types == TYPER){

      metrics <- METRICR
      metric <- METRICR

    } else {

      metrics <- METRICL
      metric <- METRICL
    }

  } else {

    metrics <- metric

  }

  if (METRICR %in% metrics) {

    #list of 2 dataframes
    rivers <- CalcDARLEQ.R(data[data$Type==TYPER, ], version)

  }

  if (METRICL %in% metrics) {

    #list of 2 dataframes
    lakes <- CalcDARLEQ.L(data[data$Type==TYPEL, ], version)

  }

  DARLEQ <- switch(metric,
                   all= { c(rivers, lakes) },
                   rivers= { rivers },
                   lakes= { lakes }
  )

} # end CalcDARLEQ
# nolint end
