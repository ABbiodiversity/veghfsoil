#' Read in summary data
#'
#' @description Read in summary data provided by the Geospatial Centre
#'
#' @param summary_path Path to summary file (can be .csv or .sqlite)
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
read_summary <- function(summary_path, table = NULL, col.subset = NULL, val.subset) {

  # Read in data
  if (endsWith(tolower(summary_path), ".csv")) {
    cat("Reading CSV file:\n", summary_path, "... ")
    d <- utils::read.csv(summary_path)
  } else {
    cat("Connecting to SQLite database:\n", summary_path)
    db <- DBI::dbConnect(RSQLite::SQLite(), summary_path)
    cat("\n\nFound the following tables:\n")
    cat(paste(DBI::dbListTables(db), collapse="\n"))
    cat("\n\nLoading table:\n", table)
    d <- DBI::dbReadTable(db, table)
    cat("\n\nDisconnecting ... ")
    DBI::dbDisconnect(db)
    d <- .make_char2fact(d)
  }

  # Take subset if needed
  if (!is.null(col.subset)) {
    cat("OK\n\nTaking subset ... ")
    d <- d[d[[col.subset]] %in% val.subset,]
  }

  return(d)

}
