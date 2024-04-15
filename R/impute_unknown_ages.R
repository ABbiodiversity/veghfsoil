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
#' @export
#'
#'
impute_unknown_ages <- function(landcover,
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
