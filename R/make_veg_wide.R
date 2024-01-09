#' Process long summary into wide summary
#'
#' @description Create veg, hf, and soil summaries from GIS back-filled data in wide format
#'
#' @param long.output Output from \code{make_veghf_long()}
#' @param col.label Unique ID (can all be same value); e.g., "Site_ID"
#' @param col.area Column of polygon area; defaults to "Shape_Area"
#' @param hf.fine Logical; defaults to TRUE
#' @param tol Tolerance level for excluding unknown aged harvest areas
#' @param sparse Logical; defaults to TRUE
#' @param assign.unknown.ages Logical; defaults to TRUE, assigns unknown ages based on the proportions in each natural subregion
#' @param age.data Default; Maltman.old. Defines which age information will be used for unknown stands (ABMI.Young; Maltman.Young; ABMI.Old; Maltman.Old)
#' @param ver.id Character; defines which version of the backfill is being processed. Supports V6.1 and V7.0
#' @param rm0 Logical; defines if the original column for unknown ages is removed after age assignment
#'
#' @importFrom mefa4 Xtab nonDuplicated
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d.wide <- make_veghf_wide(long.output = d.long,
#'                           col.label = "Site_ID",
#'                           col.area = "Shape_Area",
#'                           hf.fine = TRUE,
#'                           tol = 0,
#'                           sparse = TRUE,
#'                           assign.unknown.ages = TRUE,
#'                           age.data = "Maltman.Old",
#'                           ver.id = "V7.0",
#'                           rm0 = TRUE)
#' }
#'
make_landcover_wide <- function(long.output,
                                col.label,
                                col.area = "Shape_Area",
                                hf.fine = TRUE,
                                tol = 0,
                                sparse = TRUE,
                                assign.unknown.ages = TRUE,
                                age.data = "Maltman.Old",
                                ver.id = "V7.0",
                                rm0 = TRUE) {

  # Load appropriate lookup tables
  data("footprint.lookup", envir = environment())
  data("harvest.lookup", envir = environment())
  data("vegetation.lookup", envir = environment())
  data("soil.lookup", envir = environment())
  data("ages.by.nsr", envir = environment())

  # Update the shape area if it isn't as expected
  if (col.area != "Shape_Area") {

    long.output[["Shape_Area"]] <- long.output[[col.area]]
    long.output[[col.area]] <- NULL

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

  # Reference labels
  VegLab <- c(paste0(rep(treed.classes, each=11),
                     c("0","R","1","2","3","4","5","6","7","8","9")),
              non.treed.classes)

  # Use necessary CC-forest classes once evaluated
  CCOnlyLab <- paste0("CC", paste0(rep(treed.classes.cc, each=5),
                                   c("R","1","2","3","4")))
  CrOnlyLab <- c(HFLab, CCOnlyLab)
  HLEVS <- c(treed.classes, non.treed.classes)
  VegHFLab <- c(VegLab, CrOnlyLab)

  # Soil labels
  SoilLab <- soil.lookup$Soil
  SoilHFLab <- c(SoilLab, HFLab)

  # Crosstabs to create the matrices
  # veg reference
  veg.ref <- mefa4::Xtab(Shape_Area ~ LABEL + VEGAGEclass, long.output)

  if (!all(colnames(veg.ref) %in% VegLab)) {
    cat(colnames(veg.ref)[!(colnames(veg.ref) %in% VegLab)], sep="\n")
    stop("Unexpected veg.ref label")
  }

  # veg + HF current
  veg.cur <- mefa4::Xtab(Shape_Area ~ LABEL + VEGHFAGEclass, long.output)

  if (!all(colnames(veg.cur) %in% VegHFLab)) {
    cat(colnames(veg.cur)[!(colnames(veg.cur) %in% VegHFLab)], sep="\n")
    stop("Unexpected veg.cur label")
  }

  # soils (`reference`)
  soil.ref <- mefa4::Xtab(Shape_Area ~ LABEL + SOILclass, long.output)

  if (!all(colnames(soil.ref) %in% SoilLab)) {
    cat(colnames(soil.ref)[!(colnames(soil.ref) %in% SoilLab)], sep="\n")
    stop("Unexpected soil.ref label")
  }

  # soils (`current`, soil + HF)
  soil.cur <- mefa4::Xtab(Shape_Area ~ LABEL + SOILHFclass, long.output)

  if (!all(colnames(soil.cur) %in% SoilHFLab)) {
    cat(colnames(soil.cur)[!(colnames(soil.cur) %in% SoilHFLab)], sep="\n")
    stop("Unexpected soil.cur label")
  }

  # Removing unknown aged forest harvest within tolerance level
  if (tol > 0) {
    #CC0 <- sum(VegCr[,startsWith(colnames(VegCr), "CC") & endsWith(colnames(VegCr), "0")])
    ISSUECOL <- startsWith(colnames(veg.cur), "CC") & !(colnames(veg.cur) %in% CCOnlyLab)
    CC0 <- sum(veg.cur[,ISSUECOL])
    CC0 <- CC0 / sum(veg.cur)
    if (CC0 > 0)
      cat("Unknown age or non merchentable CC found: ", round(100*CC0,4), "%\n", sep="")
    if (CC0 <= tol)
      veg.cur <- veg.cur[,!ISSUECOL]
  }

  # Remove the Len, LenW, Ltc, and LtcR that were added to water
  SoilLab <- SoilLab[!(SoilLab %in% soil.lookup$Soil[soil.lookup$Water])]
  SoilHFLab <- SoilHFLab[!(SoilHFLab %in% soil.lookup$Soil[soil.lookup$Water])]

  # In some instances, features types or landcover types aren't present.
  # Therefore, we add any missing labels to the matrices

  if(!all(VegHFLab %in% colnames(veg.cur))) {

    missing.labels <- VegHFLab[!(VegHFLab %in% colnames(veg.cur))]
    fill.matrix <- matrix(data = 0, nrow = nrow(veg.cur), ncol = length(missing.labels),
                          dimnames = list(rownames(veg.cur), missing.labels))
    veg.cur <- cbind(veg.cur, fill.matrix)

  }

  if(!all(VegLab %in% colnames(veg.ref))) {

    missing.labels <- VegLab[!(VegLab %in% colnames(veg.ref))]
    fill.matrix <- matrix(data = 0, nrow = nrow(veg.ref), ncol = length(missing.labels),
                          dimnames = list(rownames(veg.ref), missing.labels))
    veg.ref <- cbind(veg.ref, fill.matrix)

  }

  if(!all(SoilHFLab %in% colnames(soil.cur))) {

    missing.labels <- SoilHFLab[!(SoilHFLab %in% colnames(soil.cur))]
    fill.matrix <- matrix(data = 0, nrow = nrow(soil.cur), ncol = length(missing.labels),
                          dimnames = list(rownames(soil.cur), missing.labels))
    soil.cur <- cbind(soil.cur, fill.matrix)

  }

  if(!all(SoilLab %in% colnames(soil.ref))) {

    missing.labels <- SoilLab[!(SoilLab %in% colnames(soil.ref))]
    fill.matrix <- matrix(data = 0, nrow = nrow(soil.ref), ncol = length(missing.labels),
                          dimnames = list(rownames(soil.ref), missing.labels))
    soil.ref <- cbind(soil.ref, fill.matrix)

  }

  # Make sure row labels are identical across tables
  rn <- rownames(veg.ref)
  veg.ref <- veg.ref[rn, VegLab, drop=FALSE]
  veg.cur <- veg.cur[rn, VegHFLab, drop=FALSE]
  soil.ref <- soil.ref[rn, SoilLab, drop=FALSE]
  soil.cur <- soil.cur[rn, SoilHFLab, drop=FALSE]

  out <- list(veg.current = veg.cur,
              veg.reference = veg.ref,
              soil.current = soil.cur,
              soil.reference = soil.ref)

  if (!sparse) {

    out <- lapply(out, as.matrix)

  }

  # Determine if we want to assign unknown ages.
  if(assign.unknown.ages == TRUE) {

    # Year for each row and prepare identification of natural subregions for filling in ages
    tmp <- mefa4::nonDuplicated(long.output, LABEL, TRUE)
    tmp <- tmp[rownames(veg.cur),]
    out$sample_year <- tmp$SampleYear

    dx <- mefa4::nonDuplicated(long.output, long.output[[col.label]], TRUE)[rownames(out[[1]]),]

    # Fill in 0 ages based on the version id
    d.wide <- .fill_in_0ages(x = out,
                             NSR = dx$NSRNAME,
                             ages.list = ages.list,
                             age.data = age.data)


  } else {

    d.wide <- out

  }

  # Remove the unknown ages
  if (rm0) {
    excl <- c("Decid0", "Mixedwood0", "Pine0", "Spruce0", "TreedBog0", "TreedFen0",
              "TreedSwamp0", "CutBlocks")
    d.wide$veg.current <- d.wide$veg.current[, !(colnames(d.wide$veg.current) %in% excl)]
    d.wide$veg.reference <- d.wide$veg.reference[, !(colnames(d.wide$veg.reference) %in% excl)]
  }

  return(d.wide)

}
