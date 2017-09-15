#' @title Phytobenthos Biotic indices
#'
#' @description
#' Metrics for assessing ecological indicators of biodiversity in rivers and lakes.
#' Specifically coded to be used with TIBCO Enterprise Runtime for R (TERR).
#'
#' @name phytobenthosMetrics
#' @aliases phytobenthosMetrics
#' @docType package
#' @details
#' \tabular{ll}{
#'  Package: \tab phytobenthosMetrics\cr
#'  Type: \tab Package\cr
#'  Version: \tab 0.98-2\cr
#'  Date: \tab 2017-03-30\cr
#'  License: \tab MIT\cr
#' }
#' This package contains several functions which compute multiple ecological
#' metrics using multiple season combinations.
#' \itemize{
#'  \item DARLEQ (\code{\link{CalcDARLEQ}})\cr Diatoms for Assessing River and Lake Ecological Quality
#' }
#'
#' @keywords package
#' @author Scottish Environment Protection Agency
#' Maintainer: Carlos Ruiz \email{carlos.ruiz@@sepa.org.uk}
NULL

#' DARLEQ. TDIs associated with the presence of diatoms.
#' TDI (Trophic Diatom Index) associated with diatoms in rivers and lakes,
#' providing a information on the nutrient status of rivers, as appear in DARLEQ2
#'
#' The columns are as follows:
#'
#' \itemize{
#'  \item TaxonId - Code added by tool developer. This column should have unique codes
#'  and all rows in the column should have an entry.
#'  \item NBSCode - \href{https://api.nbnatlas.org/;jsessionid=821F3DBADB0ABD14F10EA7B2354EE741#ws80}{NBN Atlas identifier code}
#'  more specifically the \href{http://nbn-sd-dev.nhm.ac.uk/glossary.php}{'TVK'}
#'  code created by the Natural History Museum. This is not necessarily the
#'  preferred NBN Atlas code. These are GUIDs, each taxon concept should have a
#'  unique GUID/code, therefore each row in this table has a unique
#'  NBSCode. However, not all taxa have in the table have been added to the
#'  NHM system. Therefore this column is either Null or has a unique value.
#'  \item AccCode
#'  \item Class
#'  \item Family
#'  \item Authority
#'  \item Genus
#'  \item TaxonName
#'  \item DARESTaxonName - Name used in DARES tool?
#'  \item DARESAuthority - Authority used in DARES tool?
#'  \item Aggregate - ?
#'  \item Status - ?
#'  \item TDIo - First diatom metric? Reference needed
#'  \item TDI3 - DARES river metric (previous)
#'  \item TDI4 - DARLEQ river metric (latest)
#'  \item LTDI1 - Previous Lake metric
#'  \item LTDI3 - DARLEQ lake metric (latest)
#'  \item DAM - Diatom Acid Metric
#'  \item Motile - Taxa classed as motile
#'  \item OrganicTolerant - Taxa classed as organic tolerant
#'  \item Planktic - Taxa classed as planktic
#'  \item Saline- Taxa classed as saline tolerant
#'  \item Notes - For example if typo or name change
#' }
#'
#' @docType data
#' @keywords datasets
#' @name LookUpDARLEQ2
#' @format A data frame with 1275 rows and 24 variables
NULL
