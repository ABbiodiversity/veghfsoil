#' Process long summary into wide summary
#'
#' @description Create veg, hf, and soil summaries from GIS back-filled data in wide format
#'
#' @param d Data frame of summary read in using \code{read_summary()}
#' @param long_output Output from \code{make_veghf_long()}
#' @param col.label Unique ID (can all be same value); e.g., "Site_ID"
#' @param col.area Column of polygon area; defaults to "Shape_Area"
#' @param hf_fine Logical; defaults to TRUE
#' @param tol Tolerance level for excluding unknown aged harvest areas
#' @param sparse Logical; defaults to TRUE
#'
#' @importFrom mefa4 Xtab nonDuplicated
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d_wide <- make_veghf_wide(d = df, long_output = d_long, col.label = "Site_ID")
#' }
#'
make_veghf_wide <- function(d,
                            long_output,
                            col.label,
                            col.area = "Shape_Area",
                            hf_fine = TRUE,
                            tol = 0,
                            sparse = TRUE
                            ) {

  if (col.area != "Shape_Area") {
    long_out[["Shape_Area"]] <- long_output[[col.area]]
    long_output[[col.area]] <- NULL
  }

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

  RfLab <- c(paste0(rep(TreedClasses, each=11),
                    c("0","R","1","2","3","4","5","6","7","8","9")),
             NontreedClasses)

  # Use necessary CC-forest classes once evaluated
  CCOnlyLab <- paste0("CC", paste0(rep(TreedClassesCC, each=5),
                                   c("R","1","2","3","4")))

  CrOnlyLab <- c(HFLab, CCOnlyLab)
  HLEVS <- c(TreedClasses, NontreedClasses)
  AllLabels <- c(RfLab, CrOnlyLab)

  SoilLab <- c("UNK", "Water", #"Wetland",
               "BdL", "BlO", "CS", "Cy", "Gr", "LenA", "LenSP",
               "LenT", "LenS", "Li", "Lo", "LtcC", "LtcD", "LtcH", "LtcS", "Ov",
               "Sa", "Sb", "SL", "SwG", "Sy", "TB")

  # Labels for output columns
  #    SoilHFLab <- levels(d$SOILHFclass)
  SoilHFLab <- c(HFLab, SoilLab)
  levels(long_output$SOILHFclass) <- c(levels(long_output$SOILHFclass), setdiff(SoilHFLab, levels(long_output$SOILHFclass)))

  # Crosstabs
  ## veg reference
  VegRf <- mefa4::Xtab(Shape_Area ~ LABEL + VEGAGEclass, long_output)
  ## veg + HF current
  VegCr <- mefa4::Xtab(Shape_Area ~ LABEL + VEGHFAGEclass, long_output)
  ## soils (`reference`)
  SoilRf <- mefa4::Xtab(Shape_Area ~ LABEL + SOILclass, long_output)
  ## soils (`current`, soil + HF)
  SoilCr <- mefa4::Xtab(Shape_Area ~ LABEL + SOILHFclass, long_output)

  # Removing unknown aged forest harvest within tolerance level
  if (tol > 0) {
    #CC0 <- sum(VegCr[,startsWith(colnames(VegCr), "CC") & endsWith(colnames(VegCr), "0")])
    ISSUECOL <- startsWith(colnames(VegCr), "CC") & !(colnames(VegCr) %in% CCOnlyLab)
    CC0 <- sum(VegCr[,ISSUECOL])
    CC0 <- CC0 / sum(VegCr)
    if (CC0 > 0)
      cat("Unknown age or non merchentable CC found: ", round(100*CC0,4), "%\n", sep="")
    if (CC0 <= tol)
      VegCr <- VegCr[,!ISSUECOL]
  }

  if (!all(colnames(VegRf) %in% RfLab)) {
    cat(colnames(VegRf)[!(colnames(VegRf) %in% RfLab)], sep="\n")
    stop("Unexpected VegRf label")
  }
  if (!all(colnames(VegCr) %in% AllLabels)) {
    cat(colnames(VegCr)[!(colnames(VegCr) %in% AllLabels)], sep="\n")
    stop("Unexpected VegCr label")
  }
  if (!all(colnames(SoilRf) %in% SoilLab)) {
    cat(colnames(SoilRf)[!(colnames(SoilRf) %in% SoilLab)], sep="\n")
    stop("Unexpected SoilRf label")
  }
  if (!all(colnames(SoilCr) %in% SoilHFLab)) {
    cat(colnames(SoilCr)[!(colnames(SoilCr) %in% SoilHFLab)], sep="\n")
    stop("Unexpected SoilCr label")
  }

  rn <- rownames(VegRf) # make sure row labels are identical across tables
  VegRf <- VegRf[rn, RfLab, drop=FALSE]
  VegCr <- VegCr[rn, AllLabels, drop=FALSE]
  SoilRf <- SoilRf[rn, SoilLab, drop=FALSE]
  SoilCr <- SoilCr[rn, SoilHFLab, drop=FALSE]

  out <- list(veg_current=VegCr,
              veg_reference=VegRf,
              soil_current=SoilCr,
              soil_reference=SoilRf)

  if (!sparse)
    out <- lapply(out, as.matrix)

  # Year for each row
  tmp <- mefa4::nonDuplicated(long_output, LABEL, TRUE)
  tmp <- tmp[rownames(VegCr),]
  #out$sample_year <- THIS_YEAR
  out$sample_year <- tmp$SampleYear

  dx <- nonDuplicated(d, d[[col.label]], TRUE)[rownames(out[[1]]),]

  # Load ages by NSR
  data("ages-by-nsr", envir = environment())
  # Fill in 0 ages
  d_wide <- .fill_in_0ages(out, dx$NSRNAME, ages_list)

  return(d_wide)

}
