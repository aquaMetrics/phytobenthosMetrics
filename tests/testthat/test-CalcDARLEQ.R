context("CalcDARLEQ")

test_that("CalcDARLEQ returns correct error measures", {
  # nolint start
   # skip_on_cran()
  # nolint end
    expect_silent(CalcDARLEQ(demoDiatomRiverData,
                             metric = "rivers",
                             version = "latest"))

    badData <- data.frame(sampleID = c("1", "2", "3"),
                           date = c("01/01/2002", "01/01/2002", "01/01/2002"))
    expect_error(CalcDARLEQ(badData, metric = "rivers", version = "latest"),
          "It seems the input data.frame does not have the required columns")
    expect_error(CalcDARLEQ(metric = "rivers", version = "latest"),
                 "No dataframe has been specified as 'data'")


})


test_that("Check DARLEQ-TAXON-DICTIONARY is same as DALREQlookup", {

  # These two files should be the same, there are two files for ease of use:

  # DARLEQ-TAXON-DICTIONARY is in csv file for easy reading and comparison on
  # github

  darleqTaxonDictionary  <-  read.csv(system.file("extdata",
                               "DARLEQ2_TAXON_DICTIONARY.csv",
                               package = "phytobenthosMetrics"),
                               stringsAsFactors = F)

 # LookUpDARLEQ2 table is held in sys.data file as a binary for fast access in
 # the package
  lookUpDARLEQ2 <- phytobenthosMetrics:::LookUpDARLEQ2

# check that the binary file matches the plain text csv file

  mismatches <- all.equal(darleqTaxonDictionary,
                          lookUpDARLEQ2)

  expect_true(mismatches == T)

  # If this test fails, check if either file has been changed. Changes in
  # taxonomy / scores should flow from DARLEQ2_TAXON_DICTIONARY to LookUpDARLEQ2
  # - if changes agreed by all
  # Follow guidance for updating the LookUpDARLEQ2/sysdata:
  # http://r-pkgs.had.co.nz/data.html#data-sysdata

})


test_that("CalcDARLEQ returns same results as DARLEQ2 tool outputs", {

    # load example data
    riverAquaMetrics  <-  read.csv(system.file("extdata",
                      "diatomRivers2012-aquametrics-format.csv",
                                package = "phytobenthosMetrics"))

    lakesAquaMetrics  <-  read.csv(system.file("extdata",
                         "diatomLakes2012-aquametrics-format.csv",
                                    package = "phytobenthosMetrics"))

    # load matching outputs from DARLEQ2 - 'here's one I prepared earlier...'
    riverDARLEQRResults  <-  read.csv(system.file("extdata",
                                     "ResultsDARLEQRiver2012-tdi4.csv",
                                        package = "phytobenthosMetrics"))

    lakesDARLEQRResults  <-  read.csv(system.file("extdata",
                               "ResultsDARLEQLakes2012-ltdi2.csv",
                                    package = "phytobenthosMetrics"))

    # run example data through CalcDARLEQ function
    riverAquaMetricsResults <- CalcDARLEQ(riverAquaMetrics, metric = "rivers")
    riverAquaMetricsResults <- data.frame(riverAquaMetricsResults[1])

    lakeAquaMetricsResults <- CalcDARLEQ(lakesAquaMetrics, metric = "lakes")
    lakeAquaMetricsResults <- data.frame(lakeAquaMetricsResults[1])

    # need to round result like Excel
    roundExcel <- function(x, n) {
      posneg <- sign(x)
      z <- abs(x) * 10 ^ n
      z <- z + 0.5
      z <- trunc(z)
      z <- z / 10 ^ n
      z * posneg
    }

    # hack to get rid of decimal place issues: '...since e.g. 0.15 is not
    # represented exactly, the rounding rule applies to the represented number
    # and not to the printed number, and so round(0.15, 1) could be either 0.1
    # or 0.2).'
    riverAquaMetricsResults$rivers.oTDI4 <- as.character(riverAquaMetricsResults$rivers.oTDI4) # nolint
    riverAquaMetricsResults$rivers.oTDI4 <- as.numeric(riverAquaMetricsResults$rivers.oTDI4) # nolint

    # check CalcDARLEQ outputs match DARLEQ2 tool outputs
    expect_equal(roundExcel(riverAquaMetricsResults$rivers.oTDI4, 2),
                 roundExcel(riverDARLEQRResults$TDI4, 2))

    expect_equal(roundExcel(riverAquaMetricsResults$rivers.EQR.TDI4, 2),
                 roundExcel(riverDARLEQRResults$EQR.TDI4, 2))

    expect_equal(roundExcel(lakeAquaMetricsResults$lakes.oLTDI2, 2),
                 roundExcel(lakesDARLEQRResults$LTDI2, 2))

    expect_equal(roundExcel(lakeAquaMetricsResults$lakes.EQR.LTDI2, 2),
                 roundExcel(lakesDARLEQRResults$EQR.LTDI2, 2))


})

devtools::install_github("jimhester/lintr")

if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}
