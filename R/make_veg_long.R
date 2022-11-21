#' Process input data into long summary
#'
#' @description Create veg, hf, and soil summaries from GIS back-filled data in long format
#'
#' @param d Data frame of summary read in using \code{read_summary()}
#' @param col.label Unique ID (can all be same value); e.g., "Site_ID"
#' @param col.baseyear Base year for surveys or HF inventory. Used to calculate years since last disturbance. Use numeric when it is the same for each record, character to indicate field when it varies by record.
#' @param col.hfyear HF year column; default is "YEAR"
#' @param col.veg Vegetation column; default is "Combined_ChgByCWCS"
#' @param col.soil Soil column; default is "Soil_Type_1"
#' @param hf_fine Logical; defaults to TRUE
#' @param unround Logical; whether to unround rounded origin year values pre-2000
#'
#' @importFrom mefa4 reclass
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d_long <- make_veghf_long(d = df,
#'                           col.label = "Region",
#'                           col.baseyear = 2017,
#'                           col.hfyear = "YEAR",
#'                           col.veg = "Combined_ChgByCWCS",
#'                           col.soil = "Soil_Type_1",
#'                           hf_fine = TRUE,
#'                           unround = FALSE)
#' }
#'
make_veghf_long <- function(d,
                            col.label,
                            col.baseyear = NULL, # Numeric.
                            col.hfyear = "YEAR",
                            col.veg = "Combined_ChgByCWCS",
                            col.soil = "Soil_Type_1",
                            hf_fine = TRUE,
                            unround = FALSE) {

  TreedClassesCC <- c("Decid", "Mixedwood", "Pine", "Spruce")
  TreedClasses   <- c(TreedClassesCC, "TreedBog", "TreedFen", "TreedSwamp")
  NontreedClasses <- c("GrassHerb", "Shrub",
                       "GraminoidFen", "Marsh",
                       "ShrubbyBog", "ShrubbyFen", "ShrubbySwamp",
                       "Bare", "SnowIce", "Water")

  if (!hf_fine) {
    HFLab <- c("BorrowpitsDugoutsSumps", "Canals", "CultivationCropPastureBareground",
               "CutBlocks", "HighDensityLivestockOperation", "IndustrialSiteRural",
               "MineSite", "MunicipalWaterSewage", "OtherDisturbedVegetation",
               "PeatMine", "Pipeline", "RailHardSurface", "RailVegetatedVerge",
               "Reservoirs", "RoadHardSurface", "RoadTrailVegetated", "RoadVegetatedVerge",
               "RuralResidentialIndustrial", "SeismicLine", "TransmissionLine",
               "Urban", "WellSite", "WindGenerationFacility")
  } else {
    HFLab <- c("UrbanIndustrial", "UrbanResidence", "RuralResidentialIndustrial",
               "IndustrialSiteRural", "WindGenerationFacility", "OtherDisturbedVegetation",
               "MineSite", "PeatMine", "WellSite", "Pipeline", "TransmissionLine",
               "SeismicLineNarrow", "SeismicLineWide", "RoadHardSurface", "RailHardSurface",
               "RoadTrailVegetated", "RoadVegetatedVerge", "RailVegetatedVerge",
               "CultivationCrop", "CultivationAbandoned", "CultivationRoughPasture",
               "CultivationTamePasture", "HighDensityLivestockOperation",
               "CutBlocks", "BorrowpitsDugoutsSumps", "MunicipalWaterSewage",
               "Reservoirs", "Canals")
  }

  SoilLab <- c("UNK", "Water", #"Wetland",
               "BdL", "BlO", "CS", "Cy", "Gr", "LenA", "LenSP",
               "LenT", "LenS", "Li", "Lo", "LtcC", "LtcD", "LtcH", "LtcS", "Ov",
               "Sa", "Sb", "SL", "SwG", "Sy", "TB")

  RfLab <- c(paste0(rep(TreedClasses, each=11),
                    c("0","R","1","2","3","4","5","6","7","8","9")),
             NontreedClasses)

  # Use necessary CC-forest classes once evaluated
  CCOnlyLab <- paste0("CC", paste0(rep(TreedClassesCC, each=5),
                                   c("R","1","2","3","4")))

  CrOnlyLab <- c(HFLab, CCOnlyLab)
  HLEVS <- c(TreedClasses, NontreedClasses)
  AllLabels <- c(RfLab, CrOnlyLab)

  # Designate a label column (there are different column names in use)
  d$LABEL <- d[,col.label]

  if (is.null(d[,col.veg]))
    stop("Shoot -- check the vegetation column...")

  d <- droplevels(d[!is.na(d[,col.veg]) & d[,col.veg] != "",])

  # Load stored HF type/class lookup
  data("recl", envir = environment())

  d$veg <- mefa4::reclass(d[, col.veg], as.matrix(recl), all = TRUE)

  d$HF_Year <- d[,col.hfyear]

  if (any(is.na(d$LABEL)))
    stop("missing LABEL")

  # Designate a base year column
  if (is.null(col.baseyear)) {
    THIS_YEAR <- as.POSIXlt(Sys.Date())$year + 1900
    d$SampleYear <- THIS_YEAR
  } else {
    if (is.numeric(col.baseyear)) {
      if (length(col.baseyear) > 1)
        stop("length of col.year > 1")
      THIS_YEAR <- col.baseyear
      d$SampleYear <- THIS_YEAR
    } else {
      THIS_YEAR <- NA
      d$SampleYear <- d[,col.baseyear]
    }
  }

  # Use upper-case labels for FEATURE_TY
  levels(d$FEATURE_TY) <- toupper(levels(d$FEATURE_TY))

  d$ORIGIN_YEAR <- d$Origin_Year
  if (unround)
    d$ORIGIN_YEAR[d$ORIGIN_YEAR < 2000] <- .age_unround(d$ORIGIN_YEAR[d$ORIGIN_YEAR < 2000])

  # Load stored HF type/class lookup
  data("hflt", envir = environment())

  # Footprint classes:
  # Check if we have all the feature types in the lookup table
  ## "" blank is for non-HF classes in current veg
  levels(d$FEATURE_TY)[levels(d$FEATURE_TY) == "''"] <- ""
  levels(d$FEATURE_TY)[levels(d$FEATURE_TY) == " "] <- ""
  if (!all(setdiff(levels(d$FEATURE_TY), rownames(hflt)) == "")) {
    print(setdiff(levels(d$FEATURE_TY), c("", rownames(hflt))))
    stop("HF diff found, see above")
  }
  # Classify feature types according to the chosen level of HF designation
  # which comes from hf.level column of hflt (HF lookup table)
  if (hf_fine) {
    d$HFclass <- hflt$HF_GROUP_COMB[match(d$FEATURE_TY, rownames(hflt))]
  } else {
    d$HFclass <- hflt$HF_GROUP[match(d$FEATURE_TY, rownames(hflt))]
  }
  d$HFclass <- as.factor(d$HFclass)
  # HFclass inherits all levels from hflt[,hf.level]
  # Need to add in the blank for further parsing
  levels(d$HFclass) <- c(levels(d$HFclass), "")
  d$HFclass[is.na(d$HFclass)] <- ""

  # Slivers (tiny polygons with no veg info):
  #stopifnot(max(d$Shape_Area[d$VEGclass == ""]) < 1)
  if (any(d$veg == ""))
    warning(paste("blank HABIT:", sum(d$Shape_Area[d$HABIT == ""]), "m^2"))
  #d <- d[d$HABIT != "",]
  d$veg <- droplevels(d$veg)

  # HABIT/EC classes:
  # Follow HABIT/EC classes, but there are few oddities when outside of AVI
  d$VEGclass <- d$veg
  if (length(setdiff(d$VEGclass, HLEVS)) > 0)
    stop(paste("check vegetation classes", setdiff(d$VEGclass, HLEVS)))

  # Age info for backfilled (Rf) and current (Cr)
  # Reference age class 0=no age (either not forest or no info)
  # 1=0-19, 2=20-39, etc.
  d$ORIGIN_YEAR[!is.na(d$ORIGIN_YEAR) & d$ORIGIN_YEAR == 9999] <- NA
  d$ORIGIN_YEAR[!is.na(d$ORIGIN_YEAR) & d$ORIGIN_YEAR > d$SampleYear] <- NA
  d$AgeRf <- as.integer(sign(d$ORIGIN_YEAR) * (1 + floor((d$SampleYear - d$ORIGIN_YEAR) / 20)))
  ## truncate reference age classes at 9 = 160+
  d$AgeRf[d$AgeRf > 9L] <- 9L
  ## catching origin_year > sample_year instances: this defaults to old
  d$AgeRf[d$AgeRf < 1] <- 9L
  ## placeholder for recent burn (0-9 years)
  tmp <- as.integer(sign(d$ORIGIN_YEAR) * (1 + floor((d$SampleYear - d$ORIGIN_YEAR) / 10)))
  d$AgeRf[tmp == 1L] <- 999L

  # Incorporate HF year for cutblocks
  d$CC_ORIGIN_YEAR <- d$ORIGIN_YEAR
  ii <- d$HFclass == "CutBlocks"
  ii[ii & !is.na(d$ORIGIN_YEAR) & d$HF_Year >= d$ORIGIN_YEAR] <- TRUE
  ii[ii & is.na(d$ORIGIN_YEAR)] <- TRUE
  d$CC_ORIGIN_YEAR[ii] <- d$HF_Year[ii]
  d$CC_ORIGIN_YEAR[!is.na(d$CC_ORIGIN_YEAR) & d$CC_ORIGIN_YEAR == 9999] <- NA
  ## age for current with cutblock ages
  d$AgeCr <- as.integer(sign(d$CC_ORIGIN_YEAR) * (1 + floor((d$SampleYear - d$CC_ORIGIN_YEAR) / 20)))
  ## truncate current age classes at 9
  d$AgeCr[d$AgeCr > 9L] <- 9L
  ## catching origin_year > sample_year instances: this defaults to old
  d$AgeCr[d$AgeCr < 1] <- 9L
  ## placeholder for recent CC (0-9 years)
  tmp <- as.integer(sign(d$CC_ORIGIN_YEAR) * (1 + floor((d$SampleYear - d$CC_ORIGIN_YEAR) / 10)))
  d$AgeCr[tmp == 1L] <- 999L
  ## unknown age is set to 0
  #table(d$AgeCr, d$VEGclass, useNA="a") # check NA ORIGIN_YEAR values
  #d$AgeCr[is.na(d$AgeCr)] <- 0L
  #table(d$AgeCr,useNA="a")

  d$AgeRf[is.na(d$AgeRf)] <- 0L
  #table(rf=d$AgeRf,cr=d$AgeCr,useNA="a")
  ## turning age values into factor:
  ## 0=no age info,
  ## 1:9=valid age classes for treed veg types,
  ## ""=non-treed
  ## 999=placeholder for _R_ecent burn "R"
  d$AgeRf <- factor(d$AgeRf, levels=c(as.character(c(0:9, 999)), ""))
  ## NA --> "0" as unknown age class
  d$AgeRf[is.na(d$AgeRf)] <- "0"
  ## age is not relevant in non-treed veg types
  d$AgeRf[!(d$VEGclass %in% TreedClasses)] <- ""
  ## burn
  levels(d$AgeRf)[levels(d$AgeRf)=="999"] <- "R"

  ## making current age as factor
  d$AgeCr <- factor(d$AgeCr, levels=c(as.character(c(0:9, 999)), ""))
  ## NA --> "0" as unknown age class
  d$AgeCr[is.na(d$AgeCr)] <- "0"
  ## age is not relevant in non-treed veg types (no HF)
  d$AgeCr[d$VEGclass %in% NontreedClasses & d$HFclass == ""] <- ""
  ## age is not relevant outside of cutblocks
  d$AgeCr[!(d$HFclass %in% c("", "CutBlocks"))] <- ""
  ## recent CC
  levels(d$AgeCr)[levels(d$AgeCr)=="999"] <- "R"
  #table(current=d$AgeCr, reference=d$AgeRf)

  #### Combining VEG, HF and Age:
  ## reference VEG + Age labels:
  d$VEGAGEclass <- interaction(d$VEGclass, d$AgeRf, drop=TRUE, sep="", lex.order=TRUE)
  levels(d$VEGAGEclass) <- c(levels(d$VEGAGEclass),
                             setdiff(RfLab, levels(d$VEGAGEclass)))

  ## manage CC labels
  ## current veg+hf
  d$VEGHFclass <- d$VEGclass
  #CClabels <- paste0("CC", levels(d$VEGclass)[levels(d$VEGclass) != ""])
  CClabels <- paste0("CC", levels(d$VEGclass))
  tmp <- setdiff(levels(d$HFclass), levels(d$VEGclass))
  tmp <- tmp[!(tmp %in% c("", "CutBlocks"))]
  levels(d$VEGHFclass) <- c(levels(d$VEGHFclass), tmp, CClabels)
  ## add non-CC HF types
  d$VEGHFclass[!(d$HFclass %in% c("", "CutBlocks"))] <- d$HFclass[!(d$HFclass %in% c("", "CutBlocks"))]
  ## should later the non-merchendisable forests with CC should be redistributed?
  ## e.g. after producing the wide format
  ## update CC labels obly for <= 80 yr CC (usually this does not happen
  ## just to make sure labels are OK)
  ## anything above age class >4 is turned into 4 to avoid labeling issues (shrubland)
  d$AgeCr[d$HFclass == "CutBlocks" & d$AgeCr %in% c("5","6","7","8","9")] <- "4"
  ii <- d$HFclass == "CutBlocks" & d$AgeCr %in% c("0","R","1","2","3","4")
  if (sum(ii) > 0)
    d$VEGHFclass[ii] <- paste0("CC", as.character(d$VEGclass[ii]))

  ## current VEG + HF + Age labels:
  d$VEGHFAGEclass <- interaction(d$VEGHFclass, d$AgeCr, drop=TRUE, sep="", lex.order=TRUE)
  ## Labels for output columns
  levels(d$VEGHFAGEclass) <- c(levels(d$VEGHFAGEclass), setdiff(AllLabels, levels(d$VEGHFAGEclass)))

  #### soils:
  if (is.null(d[,col.soil]))
    stop("Shoot -- check the damn SOIL column...")
  d$SOILclass <- d[,col.soil]
  ## need to have the UNKnown class to be able to deal with NAs
  if (!is.factor(d$SOILclass))
    d$SOILclass <- as.factor(d$SOILclass)
  if (!any(levels(d$SOILclass) == ""))
    levels(d$SOILclass) <- c(levels(d$SOILclass), "")
  ## dealing with NAs
  d$SOILclass[is.na(d$SOILclass)] <- ""
  ## unknown soil type outside of GVI and Dry Mixedwood
  levels(d$SOILclass)[levels(d$SOILclass) == ""] <- "UNK"
  levels(d$SOILclass)[levels(d$SOILclass) == " "] <- "UNK"
  ## get rid of modifiers
  levels(d$SOILclass) <- sapply(strsplit(levels(d$SOILclass), "-"), function(z) z[1L])
  ## add in Water label
  levels(d$SOILclass) <- c(levels(d$SOILclass), "Water")

  ## treat these as Water or Wetland?
  levels(d$SOILclass)[levels(d$SOILclass) %in% c("Len","LenW","Ltc","LtcR")] <- "Water"
  #    levels(d$SOILclass)[levels(d$SOILclass) %in% c("Len","LenW","Ltc","LtcR")] <- "Wetland"
  ## DEM/EC based Water class overrides soil
  d$SOILclass[d$VEGclass == "Water"] <- "Water"

  levels(d$SOILclass) <- c(levels(d$SOILclass), setdiff(SoilLab, levels(d$SOILclass)))
  d$SOILHFclass <- d$SOILclass
  levels(d$SOILHFclass) <- c(levels(d$SOILHFclass), levels(d$HFclass)[levels(d$HFclass) != ""])
  d$SOILHFclass[d$HFclass != ""] <- d$HFclass[d$HFclass != ""]
  ## NOTE: current UNK can be smaller than reference UNK, it can be turned into HF
  ## currently this is not tracked

  return(d)

}
