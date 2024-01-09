#' Read in summary data
#'
#' @description Read in summary data provided by the Geospatial Centre
#'
#' @param summary.path Path to summary file (can be .csv or .sqlite)
#' @param table Name of table of interest for SQLite database (ignored for csv files)
#' @param col.subset Optional; column name to be used for subsetting
#' @param val.subset Values in \code{col.subset} to keep. Can be single value or character vector (e.g., c("NE", "NW", "SE", "SW"))
#'
#' @import DBI
#' @importFrom RSQLite SQLite
#' @importFrom utils read.csv
#'
#' @export
#'
read_summary <- function(summary.path, table = NULL, col.subset = NULL, val.subset = NULL) {

  # Read in data
  if (endsWith(tolower(summary.path), ".csv")) {

    cat("Reading CSV file:\n", summary.path, "... ")
    landcover.data <- utils::read.csv(summary.path)

  } else {

    cat("Connecting to SQLite database:\n", summary.path)
    data.base <- DBI::dbConnect(RSQLite::SQLite(), summary.path)
    cat("\n\nFound the following tables:\n")
    cat(paste(DBI::dbListTables(data.base), collapse="\n"))
    cat("\n\nLoading table:\n", table)
    landcover.data <- DBI::dbReadTable(data.base, table)
    cat("\n\nDisconnecting ... ")
    DBI::dbDisconnect(data.base)
    landcover.data <- .make_char2fact(landcover.data)

  }

  # Take subset if needed
  if (!is.null(col.subset)) {

    cat("OK\n\nTaking subset ... ")

    if (is.null(val.subset)) {

      stop("Error: No subset value was provided\n")

    } else {

      landcover.data <- landcover.data[landcover.data[[col.subset]] %in% val.subset,]

    }

  }

  return(landcover.data)

}
