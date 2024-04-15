#' Process input data into long summary
#'
#' @description Create veg, hf, and soil summaries from GIS back-filled data in long format
#'
#' @param landcover Data frame of summary read in using \code{read_summary()}
#' @param col.label Unique ID (can all be same value); e.g., "Site_ID"
#' @param col.baseyear Base year for surveys or HF inventory. Used to calculate years since last disturbance. Use numeric when it is the same for each record, character to indicate field when it varies by record.
#' @param col.hfyear HF year column; default is "YEAR"
#' @param col.veg Vegetation column; default is "Combined_ChgByCWCS"
#' @param col.soil Soil column; default is "Soil_Type_1"
#' @param burn.cc Logical; defaults to TRUE. Decides if burned harvested areas become natural burned stands or maintain cutblock status with harvest year age.
#' @param hf.fine Logical; defaults to TRUE. Decides if coarse of fine footprint levels should be used.
#' @param unround Logical; whether to unround rounded origin year values pre-2000
#' @param age.correction Age correction column; applies built in age correction old forests. Default of FALSE means no age correction occurs.
#' @param ver.id Character; defines which version of the backfill is being processed. Supports V6.1 and V7.0
#'
#' @importFrom mefa4 reclass
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d.long <- make_veghf_long(landcover = df,
#'                           col.label = "Region",
#'                           col.baseyear = 2017,
#'                           col.hfyear = "YEAR",
#'                           col.veg = "Combined_ChgByCWCS",
#'                           col.soil = "Soil_Type_1",
#'                           hf_fine = TRUE,
#'                           burn.cc = TRUE,
#'                           unround = FALSE,
#'                           age.correction = FALSE,
#'                           ver.id = "V7.0")
#' }
#'
make_landcover_long <- function(landcover,
                                col.label,
                                col.baseyear = NULL, # Numeric.
                                col.hfyear = "YEAR",
                                col.veg = "Combined_ChgByCWCS",
                                col.soil = "Soil_Type_1",
                                hf.fine = TRUE,
                                burn.cc = TRUE,
                                unround = FALSE,
                                age.correction = FALSE,
                                ver.id = "V7.0") {

#   # Load appropriate lookup tables
#   data("footprint.lookup", envir = environment())
#   data("harvest.lookup", envir = environment())
#   data("vegetation.lookup", envir = environment())
#   data("soil.lookup", envir = environment())
#   data("kgrid", envir = environment())

  # Make sure NA values area defined as ""
  levels(landcover$FEATURE_TY) <- c(levels(landcover$FEATURE_TY), "")
  landcover$FEATURE_TY[is.na(landcover$FEATURE_TY)] <- ""

  # Correct potentially suitable landcover types that fall in HARVEST-AREAS
  for (veg in harvest.lookup$Landcover[harvest.lookup$Combined != ""]) {

    landcover[landcover[, col.veg] == veg & landcover[, "FEATURE_TY"] == "HARVEST-AREA", col.veg] <- harvest.lookup[harvest.lookup$Landcover == veg, "Combined"]

  }

  # Correct HARVEST-AREAS that fall in unsuitable landcover types
  for (veg in harvest.lookup$Landcover[harvest.lookup$Combined == ""]) {

    landcover[landcover[, col.veg] == veg & landcover[, "FEATURE_TY"] == "HARVEST-AREA", "FEATURE_TY"] <- harvest.lookup[harvest.lookup$Landcover == veg, "Combined"]

  }

  # If a natural disturbance (burn) occurs after a HARVEST-AREA event, remove the HARVEST-AREA
  if (burn.cc) {

    landcover[landcover[, "FEATURE_TY"] == "HARVEST-AREA" & landcover[, "Origin_Year_NatDist"] >= landcover[, "YEAR"], "FEATURE_TY"] <- ""
    landcover[landcover[, "FEATURE_TY"] == "HARVEST-AREA" & landcover[, "Origin_Year_NatDist"] >= landcover[, "YEAR"], "YEAR"] <- 0

  }

  # Define the appropriate footprint labels
  if (hf.fine) {

    HFLab <- unique(footprint.lookup$HF_GROUP_FINE)

  } else {

    HFLab <- unique(footprint.lookup$HF_GROUP_COARSE)

  }

  # Define the landcover classes that require ages
  treed.classes.cc <- as.character(unique(vegetation.lookup[vegetation.lookup$Version == ver.id & vegetation.lookup$TreedClassesCC, "Combined"]))
  treed.classes <- as.character(unique(vegetation.lookup[vegetation.lookup$Version == ver.id & vegetation.lookup$TreedClasses, "Combined"]))
  non.treed.classes <- as.character(unique(vegetation.lookup[vegetation.lookup$Version == ver.id & vegetation.lookup$NonTreedClasses, "Combined"]))

  RfLab <- c(paste0(rep(treed.classes, each=11),
                    c("0","R","1","2","3","4","5","6","7","8","9")),
             non.treed.classes)

  # Use necessary CC-forest classes once evaluated
  CCOnlyLab <- paste0("CC", paste0(rep(treed.classes.cc, each=5),
                                   c("R","1","2","3","4")))

  CrOnlyLab <- c(HFLab, CCOnlyLab)
  HLEVS <- c(treed.classes, non.treed.classes)
  AllLabels <- c(RfLab, CrOnlyLab)

  # Designate a label column (there are different column names in use)
  landcover$LABEL <- landcover[,col.label]

  if (any(landcover$LABEL == "")) {

    stop("Missing col.label values")

  }

  ###################
  # Vegetation data #
  ###################

  # Identify if vegetation information is missing
  if (is.null(landcover[,col.veg])) {

    stop("No suitable vegetation found. Check for missing classifications")

  }

  # Drop levels and remove any missing values
  landcover <- droplevels(landcover[!is.na(landcover[,col.veg]) & landcover[,col.veg] != "",])

  # Reclassify the landcover based on vegetation.lookup
  landcover$veg <- mefa4::reclass(landcover[, col.veg], as.matrix(vegetation.lookup[vegetation.lookup$Version == ver.id, c("Landcover", "Combined")]), all = TRUE)

  # Define the year footprint was created
  landcover$HF_Year <- landcover[, col.hfyear]

  # Designate a base year column
  if (is.null(col.baseyear)) {

    # If null, assumes current year
    landcover$SampleYear <- as.POSIXlt(Sys.Date())$year + 1900

  } else {

    if (is.numeric(col.baseyear)) {

      if (length(col.baseyear) > 1) {stop("length of col.baseyear > 1")}
      landcover$SampleYear <- col.baseyear

    } else {

      landcover$SampleYear <- landcover[,col.baseyear]

    }

  }

  # Forces upper-case labels for FEATURE_TY incase there are typos
  levels(landcover$FEATURE_TY) <- toupper(levels(landcover$FEATURE_TY))

  # Since we keep track of Origin_Year and Natural Disturbance in version 7, we need to properly track
  # the current and reference years

  # If we have a natural disturbance, replace the origin year
  if(!is.null(landcover$Origin_Year_NatDist)) {

    landcover$ORIGIN_YEAR_REFERENCE <- landcover$Origin_Year
    landcover$ORIGIN_YEAR_REFERENCE[landcover$Origin_Year_NatDist != 9999] <- landcover$Origin_Year_NatDist[landcover$Origin_Year_NatDist != 9999]
    landcover$ORIGIN_YEAR_CURRENT <- landcover$ORIGIN_YEAR_REFERENCE

  } else {

    landcover$ORIGIN_YEAR_REFERENCE <- landcover$Origin_Year
    landcover$ORIGIN_YEAR_CURRENT <- landcover$ORIGIN_YEAR_REFERENCE

  }

  # If we have age correction information, add it to the current and reference origin years
  # Age correction is stored in the backfill layer, but can be reproduced using the unround function.
  # This would require additional filtering based on the source of the age information.
  # It is highly recommended to only use the age correction information in the backfill layer.

  if(age.correction != FALSE) {

    landcover$ORIGIN_YEAR_REFERENCE[landcover[, age.correction] != 9999] <- landcover$ORIGIN_YEAR_REFERENCE[landcover[, age.correction] != 9999] + landcover[, age.correction][landcover[, age.correction] != 9999]
    landcover$ORIGIN_YEAR_CURRENT[landcover[, age.correction] != 9999] <- landcover$ORIGIN_YEAR_CURRENT[landcover[, age.correction] != 9999] + landcover[, age.correction][landcover[, age.correction] != 9999]

  }

  # Footprint classes:
  # Check if we have all the feature types in the lookup table
  ## "" blank is for non-HF classes in current veg
  levels(landcover$FEATURE_TY)[levels(landcover$FEATURE_TY) == "''"] <- ""
  levels(landcover$FEATURE_TY)[levels(landcover$FEATURE_TY) == " "] <- ""

  if (!all(setdiff(levels(landcover$FEATURE_TY), rownames(footprint.lookup)) == "")) {

    print(setdiff(levels(landcover$FEATURE_TY), c("", rownames(footprint.lookup))))

    stop("Difference in human footprint feature types. Check lookup table or input data.")

  }

  # Classify feature types according to the chosen level of HF designation
  # which comes from hf.level column of hflt (HF lookup table)
  if (hf.fine) {

    landcover$HFclass <- as.factor(footprint.lookup$HF_GROUP_FINE[match(landcover$FEATURE_TY, rownames(footprint.lookup))])

  } else {

    landcover$HFclass <- as.factor(footprint.lookup$HF_GROUP_COARSE[match(landcover$FEATURE_TY, rownames(footprint.lookup))])

  }

  # HFclass inherits all levels from hflt[,hf.level]
  # Need to add in the blank for further parsing
  levels(landcover$HFclass) <- c(levels(landcover$HFclass), "")
  landcover$HFclass[is.na(landcover$HFclass)] <- ""

  # HABIT/EC classes:
  # Follow HABIT/EC classes, but there are few oddities when outside of AVI
  landcover$VEGclass <- landcover$veg
  if (length(setdiff(landcover$VEGclass, HLEVS)) > 0) {

    stop(paste("check vegetation classes", setdiff(landcover$VEGclass, HLEVS)))

  }

  # Age info for backfilled (Rf) and current (Cr)
  # reference age class 0=no age (either not forest or no info)
  # 1=0-19, 2=20-39, etc.
  landcover$ORIGIN_YEAR_REFERENCE[!is.na(landcover$ORIGIN_YEAR_REFERENCE) & landcover$ORIGIN_YEAR_REFERENCE == 9999] <- NA
  landcover$ORIGIN_YEAR_REFERENCE[!is.na(landcover$ORIGIN_YEAR_REFERENCE) & landcover$ORIGIN_YEAR_REFERENCE > landcover$SampleYear] <- NA
  landcover$AGE_RF <- as.integer(sign(landcover$ORIGIN_YEAR_REFERENCE) * (1 + floor((landcover$SampleYear - landcover$ORIGIN_YEAR_REFERENCE) / 20)))
  # truncate reference age classes at 9 = 160+
  landcover$AGE_RF[landcover$AGE_RF > 9L] <- 9L
  # catching origin_year > sample_year instances: this defaults to old
  landcover$AGE_RF[landcover$AGE_RF < 1] <- 9L
  # placeholder for recent burn (0-9 years)
  tmp <- as.integer(sign(landcover$ORIGIN_YEAR_REFERENCE) * (1 + floor((landcover$SampleYear - landcover$ORIGIN_YEAR_REFERENCE) / 10)))
  landcover$AGE_RF[tmp == 1L] <- 999L
  landcover$AGE_RF[is.na(landcover$AGE_RF)] <- 0L

  # turning age values into factor:
  # 0=no age info,
  # 1:9=valid age classes for treed veg types,
  # ""=non-treed
  # 999=placeholder for _R_ecent burn "R"
  landcover$AGE_RF <- factor(landcover$AGE_RF, levels=c(as.character(c(0:9, 999)), ""))

  # NA --> "0" as unknown age class
  landcover$AGE_RF[is.na(landcover$AGE_RF)] <- "0"

  # age is not relevant in non-treed veg types
  landcover$AGE_RF[!(landcover$VEGclass %in% treed.classes)] <- ""

  # burn
  levels(landcover$AGE_RF)[levels(landcover$AGE_RF) == "999"] <- "R"

  # incorporate HF year for cutblocks
  # Remember, if origin year is after the summary year, default to NA
  landcover$ORIGIN_YEAR_CURRENT[!is.na(landcover$ORIGIN_YEAR_CURRENT) & landcover$ORIGIN_YEAR_CURRENT > landcover$SampleYear] <- NA
  landcover$CC_ORIGIN_YEAR <- landcover$ORIGIN_YEAR_CURRENT
  ii <- landcover$HFclass == "CutBlocks"
  ii[ii & !is.na(landcover$ORIGIN_YEAR_CURRENT) & landcover$HF_Year >= landcover$ORIGIN_YEAR_CURRENT] <- TRUE
  ii[ii & is.na(landcover$ORIGIN_YEAR_CURRENT)] <- TRUE
  landcover$CC_ORIGIN_YEAR[ii] <- landcover$HF_Year[ii]
  landcover$CC_ORIGIN_YEAR[!is.na(landcover$CC_ORIGIN_YEAR) & landcover$CC_ORIGIN_YEAR == 9999] <- NA

  # age for current with cutblock ages
  landcover$AGE_CR <- as.integer(sign(landcover$CC_ORIGIN_YEAR) * (1 + floor((landcover$SampleYear - landcover$CC_ORIGIN_YEAR) / 20)))

  # truncate current age classes at 9
  landcover$AGE_CR[landcover$AGE_CR > 9L] <- 9L

  # catching origin_year > sample_year instances: this defaults to old
  landcover$AGE_CR[landcover$AGE_CR < 1] <- 9L

  # placeholder for recent CC (0-9 years)
  tmp <- as.integer(sign(landcover$CC_ORIGIN_YEAR) * (1 + floor((landcover$SampleYear - landcover$CC_ORIGIN_YEAR) / 10)))
  landcover$AGE_CR[tmp == 1L] <- 999L

  # making current age as factor
  landcover$AGE_CR <- factor(landcover$AGE_CR, levels=c(as.character(c(0:9, 999)), ""))

  # NA --> "0" as unknown age class
  landcover$AGE_CR[is.na(landcover$AGE_CR)] <- "0"

  # age is not relevant in non-treed veg types (no HF)
  landcover$AGE_CR[landcover$VEGclass %in% non.treed.classes & landcover$HFclass == ""] <- ""

  # age is not relevant outside of cutblocks
  landcover$AGE_CR[!(landcover$HFclass %in% c("", "CutBlocks"))] <- ""

  # recent CC
  levels(landcover$AGE_CR)[levels(landcover$AGE_CR)=="999"] <- "R"

  ##############################
  # Combining VEG, HF and Age: #
  ##############################

  # reference VEG + Age labels:
  landcover$VEGAGEclass <- interaction(landcover$VEGclass, landcover$AGE_RF, drop=TRUE, sep="", lex.order=TRUE)
  levels(landcover$VEGAGEclass) <- c(levels(landcover$VEGAGEclass),
                                     setdiff(RfLab, levels(landcover$VEGAGEclass)))

  # manage CC labels and current veg+hf
  landcover$VEGHFclass <- landcover$VEGclass
  CClabels <- paste0("CC", levels(landcover$VEGclass))
  tmp <- setdiff(levels(landcover$HFclass), levels(landcover$VEGclass))
  tmp <- tmp[!(tmp %in% c("", "CutBlocks"))]
  levels(landcover$VEGHFclass) <- c(levels(landcover$VEGHFclass), tmp, CClabels)
  # add non-CC HF types
  landcover$VEGHFclass[!(landcover$HFclass %in% c("", "CutBlocks"))] <- landcover$HFclass[!(landcover$HFclass %in% c("", "CutBlocks"))]

  # should later the non-merchendisable forests with CC should be redistributed?
  # e.g. after producing the wide format
  # update CC labels only for <= 80 yr CC (usually this does not happen
  # just to make sure labels are OK)
  # anything above age class >4 is turned into 4 to avoid labeling issues (shrubland)

  landcover$AGE_CR[landcover$HFclass == "CutBlocks" & landcover$AGE_CR %in% c("5","6","7","8","9")] <- "4"
  ii <- landcover$HFclass == "CutBlocks" & landcover$AGE_CR %in% c("0","R","1","2","3","4")
  if (sum(ii) > 0) {

    landcover$VEGHFclass[ii] <- paste0("CC", as.character(landcover$VEGclass[ii]))

  }

  # current VEG + HF + Age labels:
  landcover$VEGHFAGEclass <- interaction(landcover$VEGHFclass, landcover$AGE_CR, drop=TRUE, sep="", lex.order=TRUE)
  # Labels for output columns
  levels(landcover$VEGHFAGEclass) <- c(levels(landcover$VEGHFAGEclass), setdiff(AllLabels, levels(landcover$VEGHFAGEclass)))

  #############
  # Soil data #
  #############

  if (is.null(landcover[,col.soil])) {

    stop("No suitable soil found. Check for missing classifications")

  }

  landcover$SOILclass <- landcover[,col.soil]

  ## need to have the UNKnown class to be able to deal with NAs
  if (!is.factor(landcover$SOILclass)) {

    landcover$SOILclass <- as.factor(landcover$SOILclass)

  }

  if (!any(levels(landcover$SOILclass) == "")) {

    levels(landcover$SOILclass) <- c(levels(landcover$SOILclass), "")

  }

  # Handling of NA values
  landcover$SOILclass[is.na(landcover$SOILclass)] <- ""

  # Define as unknown soil type outside of GVI and Dry Mixedwood
  levels(landcover$SOILclass)[levels(landcover$SOILclass) == ""] <- "UNK"
  levels(landcover$SOILclass)[levels(landcover$SOILclass) == " "] <- "UNK"

  # Get rid of modifiers
  levels(landcover$SOILclass) <- sapply(strsplit(levels(landcover$SOILclass), "-"), function(z) z[1L])

  # Add in Water label
  levels(landcover$SOILclass) <- c(levels(landcover$SOILclass), "Water")

  # Define these wet habitat types as water
  levels(landcover$SOILclass)[levels(landcover$SOILclass) %in% soil.lookup$Soil[soil.lookup$Water]] <- "Water"

  # If defined as water in the veg class, define the associated soil as water. DEM/EC based Water class overrides soil
  landcover$SOILclass[landcover$VEGclass == "Water"] <- "Water"

  levels(landcover$SOILclass) <- c(levels(landcover$SOILclass), setdiff(soil.lookup$Soil, levels(landcover$SOILclass)))
  landcover$SOILHFclass <- landcover$SOILclass
  levels(landcover$SOILHFclass) <- c(levels(landcover$SOILHFclass), levels(landcover$HFclass)[levels(landcover$HFclass) != ""])
  landcover$SOILHFclass[landcover$HFclass != ""] <- landcover$HFclass[landcover$HFclass != ""]

  return(landcover)

}
