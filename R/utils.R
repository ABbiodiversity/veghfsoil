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

#'
#' impute_unknown_ages
#'
#' @description This function imputes unknown ages into the long form. This is used for tracking changes over time
#'
#' @param landcover Input
#' @param ages.list Ages list object
#' @param col.baseyear Base year for surveys or HF inventory. Used to patch disturbances
#' @param decadal.fix Logical; Determines if decades prior to 1960 should be smoothed
#' @param origin.fix Logical; Determines if origin years should be assigned based off Maltman et al when possible
#' @param unknown.fix Logical; Determines if we assign origin years to remaining unknown stands. Use TRUE for reporting, FALSE for site summaries
#' @param age.data Default; Maltman.old. Defines which age information will be used for unknown stands (ABMI.Old; Maltman.Old)
#' @param ver.id Character; defines which version of the backfill is being processed. Supports V6.1 and V7.0
#' @importFrom utils data flush.console
#' @importFrom methods as
#'
#'
.impute_unknown_ages <- function(landcover,
                                 ages.list,
                                 col.baseyear,
                                 decadal.fix = TRUE,
                                 origin.fix = TRUE,
                                 unknown.fix = FALSE,
                                 age.data = "Maltman.Old",
                                 ver.id = "V7.0") {

  # If the natural disturbance occurs after the focal year, correct to the origin year
  landcover$Origin_Year_NatDist[landcover$Origin_Year_NatDist > col.baseyear & landcover$Origin_Year_NatDist != 9999] <- landcover$Origin_Year[landcover$Origin_Year_NatDist > col.baseyear & landcover$Origin_Year_NatDist != 9999]

  # If the origin year is still great than the focal year, fix both to unknown
  landcover$Origin_Year_NatDist[landcover$Origin_Year_NatDist > col.baseyear] <- 9999
  landcover$Origin_Year[landcover$Origin_Year > col.baseyear] <- 9999

  # If for some reason, we have a viable natural disturbance, but the origin year is unknown, update the origin year.
  landcover$Origin_Year[landcover$Origin_Year == 9999 & landcover$Origin_Year_NatDist != 9999] <- landcover$Origin_Year_NatDist[landcover$Origin_Year == 9999 & landcover$Origin_Year_NatDist != 9999]

  # Smooths the decades prior to 1960 to prevent large changes in age classes due
  # to origin years being grouped by the nearest decade.
  if(decadal.fix == TRUE) {

    # Identify the valid decades
    decade.logical <- landcover$Origin_Year_NatDist %% 10 == 0
    valid.decades <- landcover$Origin_Year_NatDist[decade.logical]
    valid.decades <- unique(valid.decades[valid.decades <= 1960])

    # Apply the decadal fix if origin and natural disturbance are decadal
    valid.pixels <- landcover$Origin_Year %in% valid.decades & landcover$Origin_Year_NatDist %in% valid.decades
    landcover$Age_Correction <- 9999
    landcover$Age_Correction[valid.pixels] <- kgrid$AgeOffset[match(landcover$GRID_LABEL, kgrid$LinkID)][valid.pixels]

  }

  # When possible, impute the origin year directly from the Maltman et al., 2023 lookup table.
  if(origin.fix == TRUE) {

    # Apply the Origin year from Maltman et al., 2023 to forested stands (Reference)
    treed.classes <- as.character(unique(vegetation.lookup[vegetation.lookup$Version == ver.id & vegetation.lookup$TreedClasses, "Landcover"]))
    valid.pixels <- landcover$Origin_Year == 9999 & landcover$Combined_ChgByCWCS %in% treed.classes
    landcover$Origin_Year[valid.pixels] <- kgrid$UnknownOrigin[match(landcover$GRID_LABEL, kgrid$LinkID, nomatch = 0)][valid.pixels]

    # And current
    valid.pixels <- landcover$Origin_Year_NatDist == 9999 & landcover$Combined_ChgByCWCS %in% treed.classes
    landcover$Origin_Year_NatDist[valid.pixels] <- kgrid$UnknownOrigin[match(landcover$GRID_LABEL, kgrid$LinkID, nomatch = 0)][valid.pixels]

  }

  # In instances where we can't impute the origin directly, apply the natural subregion distribution curves
  # This process assigns an origin year, then offsets (-9 to +10) by linkID.
  # Origin year is calculated as the assigned age minus 2019 (year of capture in Maltman et al).

  if(unknown.fix == TRUE) {

    # Determine stand names, ages, and natural subregions
    treed.classes <- as.character(unique(vegetation.lookup[vegetation.lookup$Version == ver.id & vegetation.lookup$TreedClasses, "Landcover"]))
    Target <- names(ages.list)
    Ages <- data.frame(Class = as.character(4:9),
                       Age = c(70, 90, 110, 130, 150, 170)) # Only viable for the old age distributions
    Ages$Age <- 2019 - Ages$Age

    # Determine which age information to use
    if(age.data == "ABMI.Old") {

      age.proportions.nsr <- age.old.nsr.abmi
      age.proportions.all <- age.old.all.abmi
    }

    if(age.data == "Maltman.Old") {

      age.proportions.nsr <- age.old.nsr.maltman
      age.proportions.all <- age.old.all.maltman
    }

    # Identify the target polygons
    valid.pixels <- landcover$Origin_Year == 9999 & landcover$Combined_ChgByCWCS %in% treed.classes
    unknown.landcover <- landcover[valid.pixels, ]

    # Create a blank object for storing the imputed ages
    imputed.veg <- matrix(ncol = ncol(unknown.landcover))[-1, ]
    colnames(imputed.veg) <- colnames(unknown.landcover)

    # Loop through each vegetation type
    for(veg in unique(unknown.landcover$Combined_ChgByCWCS)) {

      unknown.veg <- unknown.landcover[unknown.landcover$Combined_ChgByCWCS == veg, ]
      matching.class <- as.character(vegetation.lookup[vegetation.lookup$Version == ver.id & as.character(vegetation.lookup$Landcover) == as.character(veg), "Combined"])

      for (nsr in unique(unknown.veg$NSRNAME)) {

        for(age in Ages$Class) {

          # Determine which natural subregions are present
          # This resents the matrix each time for the imputation process
          unknown.veg.nsr <- unknown.veg[unknown.veg$NSRNAME == nsr, ]

          # Assign the year
          unknown.veg.nsr$Origin_Year <- Ages$Age[Ages$Class == age]

          # Add the age offset
          unknown.veg.nsr$Origin_Year <- unknown.veg.nsr$Origin_Year + kgrid$AgeUnknownOffset[match(unknown.veg.nsr$GRID_LABEL, kgrid$LinkID, nomatch = 0)]

          # Adjust the natural disturbance year to match
          unknown.veg.nsr$Origin_Year_NatDist <- unknown.veg.nsr$Origin_Year

          # Multiply the shape area by the appropriate proportions
          unknown.veg.nsr$Shape_Area <- unknown.veg.nsr$Shape_Area * age.proportions.nsr$current[[matching.class]][as.character(nsr), paste0(matching.class, age)]

          # Store as imputed veg
          imputed.veg <- rbind(imputed.veg, unknown.veg.nsr)

        }

      }

      # Remove any new polygons with 0 area
      imputed.veg <- imputed.veg[imputed.veg$Shape_Area != 0, ]

    }

    # Remove the previous unknown values and replace with the new information
    landcover <- landcover[!valid.pixels, ]
    landcover <- rbind(landcover, imputed.veg)
  }

  return(landcover)

}
