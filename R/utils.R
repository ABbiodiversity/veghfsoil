#' Internal functions
#'
#' Character to Factor
#'
#' @description Make factors from characters in SQL tables
#'
#' @param x Input
#'
.make_char2fact <- function(x) {

  if (is.null(dim(x))) {

    if (is.character(x)) {

      return(as.factor(x))

    }

  }

  for (i in seq_len(ncol(x))) {

    if (is.character(x[,i])) {

      x[,i] <- as.factor(x[,i])

    }

  }

  return(x)

}

#' Origin year rounding
#'
#' @description Function to address origin year rounding in a reproducible manner
#'
#' @param y Input
#'
.age_unround <- function(y) {

  j <- c(-4L, 3L, 4L, 2L, -3L, 5L, 0L, -1L, 1L, -2L)
  s <- y %% 10 == 0
  yy <- y[s]
  i <- as.integer(round(10*(yy/100 - floor(yy/100))))
  i[i == 0L] <- 10L
  y[s] <- y[s] + j[i]
  y

}

#' Fill in 0 ages
#'
#' @description This function redistributes unknown ages in wide form summaries
#'
#' @param x Input
#' @param NSR NSR column
#' @param ages.list Ages list object
#' @param age.data Default; Maltman.old. Defines which age information will be used for unknown stands (ABMI.Young; Maltman.Young; ABMI.Old; Maltman.Old)
#'
#' @importFrom utils data flush.console
#' @importFrom methods as
#'
#'
.fill_in_0ages <- function(x,
                           NSR,
                           ages.list,
                           age.data = "Maltman.Old") {

  # Determine stand names, ages, and natural subregions
  Target <- names(ages.list)
  Ages <- c("0", "R", as.character(1:9))
  NSR <- droplevels(as.factor(NSR))
  NSRs <- levels(NSR)

  # Determine which age information to use
  if(age.data == "ABMI.Young") {

    age.proportions.nsr <- age.nsr.abmi
    age.proportions.all <- age.all.abmi
  }

  if(age.data == "ABMI.Old") {

    age.proportions.nsr <- age.old.nsr.abmi
    age.proportions.all <- age.old.all.abmi
  }

  if(age.data == "Maltman.Young") {

    age.proportions.nsr <- age.nsr.maltman
    age.proportions.all <- age.all.maltman
  }

  if(age.data == "Maltman.Old") {

    age.proportions.nsr <- age.old.nsr.maltman
    age.proportions.all <- age.old.all.maltman
  }

  for (current in c(TRUE, FALSE)) {
    xx <- if (current)
      x$veg.current else x$veg.reference
    xx <- as.matrix(xx)
    ag <- if (current)
      age.proportions.nsr$current else age.proportions.nsr$reference
    ag2 <- if (current)
      age.proportions.all$current else age.proportions.all$reference
    for (nsr in NSRs) {
      cat(ifelse(current, "current:", "reference:"), nsr)
      flush.console()
      for (i in Target) {
        Cols <- paste0(i, Ages)
        j <- NSR == nsr
        if (any(j)) {
          p0 <- ag[[i]][nsr,]
          if (sum(p0) == 0) {p0 <- ag2[[i]]}

          Mat <- xx[j, Cols, drop=FALSE]
          Mat0 <- Mat
          ## multiply Mat[,1] (unknown age) with this matrix
          Unk <- Mat[,1] * t(matrix(p0, length(Ages), sum(j)))
          Mat[,1] <- 0 # will be 0 and redistributed from Unk
          Mat <- Mat + Unk
          xx[j, Cols] <- Mat # ridiculously slow as sparse matrix
          if (sum(Mat0)-sum(Mat) > 10^-6)
            cat("\n\ttype:", i, "| diff =", round((sum(Mat0)-sum(Mat))/10^6))
        }
      }
      cat(" ... OK\n")
    }
    if (current) {
      x$veg.current <- as(xx, "dgCMatrix")
    } else {
      x$veg.reference <- as(xx, "dgCMatrix")
    }
  }

  return(x)
}
